library(shiny)

normalizeSqliteResult <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  data[] <- lapply(data, function(column) {
    if (is.list(column)) {
      vapply(column, function(value) {
        if (length(value) == 0 || is.null(value)) {
          return(NA_character_)
        }

        if (is.raw(value)) {
          return(paste(as.character(value), collapse = " "))
        }

        paste(as.character(value), collapse = ", ")
      }, FUN.VALUE = character(1))
    } else if (is.raw(column)) {
      as.character(column)
    } else {
      column
    }
  })

  data
}

buildSqliteResultsTable <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0) {
    return(tags$p("Query completed, but there were no columns to display."))
  }

  normalizedData <- normalizeSqliteResult(data)
  rows <- split(normalizedData, seq_len(nrow(normalizedData)))

  tags$table(
    class = "table table-striped table-bordered table-hover",
    tags$thead(
      tags$tr(lapply(names(normalizedData), function(columnName) tags$th(columnName)))
    ),
    tags$tbody(
      if (nrow(normalizedData) == 0) {
        tags$tr(
          tags$td(
            colspan = ncol(normalizedData),
            "Query returned 0 rows."
          )
        )
      } else {
        lapply(rows, function(row) {
          tags$tr(lapply(row, function(value) tags$td(as.character(value[[1]]))))
        })
      }
    )
  )
}

server <- function(input, output, session) {
  observe({
    jsonPath <- Sys.getenv("jsonPath")
    results <- DataQualityDashboard::convertJsonResultsFileCase(jsonPath, writeToFile = FALSE, targetCase = "camel")
    
    if (!('severity' %in% names(results$CheckResults))) {
      tryCatch({
        # Read checkDescription to get severity status for each check
        cdmVersion <- results$Metadata$cdmVersion
        checkDescriptionsDf <- readr::read_csv(
          file = system.file(
            "csv",
            sprintf("OMOP_CDMv%s_Check_Descriptions.csv", substr(sub('$v', '', cdmVersion), 1, 3)),  
            package = "DataQualityDashboard"
          ),
          show_col_types = FALSE
        )

        results$CheckResults <- results$CheckResults |>
          dplyr::left_join(
            checkDescriptionsDf,
            dplyr::join_by('checkLevel', 'checkName')
          )
      }, error = function(e) {
        results$CheckResults$severity <- NA
      })
    }
    
    results$appVersion <- as.character(packageVersion('DataQualityDashboard'))
    
    # Fix json formatting
    results <- jsonlite::parse_json(jsonlite::toJSON(results))    

    session$sendCustomMessage("results", results)
  })

  sqliteQueryResult <- shiny::reactiveVal(list(
    data = NULL,
    type = "info",
    message = "Enter a SQLite file path and SQL query, then click Execute."
  ))

  output$concept_coverage_plot <- renderPlot(
    {
      tryCatch(
        {
          DataQualityDashboard::plotConceptCoverage(Sys.getenv("jsonPath"))
        },
        error = function(error) {
          plot.new()
          text(
            0.5,
            0.5,
            labels = paste(
              "Concept coverage plot unavailable.",
              conditionMessage(error)
            ),
            cex = 1
          )
        }
      )
    },
    height = 720,
    res = 96
  )

  output$compare_results_section_ui <- renderUI({
    compareJsonPath <- Sys.getenv("compareJsonPath")

    if (!nzchar(compareJsonPath)) {
      return(NULL)
    }

    sectionContent <- NULL
    if (!file.exists(compareJsonPath)) {
      sectionContent <- tags$div(
        class = "alert alert-warning",
        sprintf("Comparison JSON file was not found: %s", compareJsonPath)
      )
    } else if (!requireNamespace("plotly", quietly = TRUE)) {
      sectionContent <- tags$div(
        class = "alert alert-warning",
        "Install the 'plotly' package to view the DQD comparison plot."
      )
    } else {
      sectionContent <- plotly::plotlyOutput("compare_results_plot", height = "720px")
    }

    tagList(
      tags$hr(class = "m-0"),
      tags$section(
        class = "resume-section p-3 p-lg-5",
        id = "compare-results",
        tags$div(
          class = "w-100",
          tags$h2(class = "mb-4", "Compare Results"),
          tags$p("Interactive comparison of the currently loaded DQD results against the optional second JSON file."),
          sectionContent
        )
      )
    )
  })

  observe({
    compareJsonPath <- Sys.getenv("compareJsonPath")

    if (!nzchar(compareJsonPath) ||
        !file.exists(compareJsonPath) ||
        !requireNamespace("plotly", quietly = TRUE)) {
      return(invisible(NULL))
    }

    output$compare_results_plot <- plotly::renderPlotly({
      tryCatch(
        {
          DataQualityDashboard::plotCompareDqdResults(
            Sys.getenv("jsonPath"),
            compareJsonPath
          )
        },
        error = function(error) {
          plotly::plot_ly() |>
            plotly::layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(list(
                text = paste(
                  "Comparison plot unavailable.",
                  conditionMessage(error)
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
              ))
            )
        }
      )
    })
  })

  observeEvent(input$sqlite_execute, {
    sqlitePath <- if (is.null(input$sqlite_path)) "" else trimws(input$sqlite_path)
    sqlQuery <- if (is.null(input$sqlite_query)) "" else trimws(input$sqlite_query)

    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
      sqliteQueryResult(list(
        data = NULL,
        type = "danger",
        message = "The DBI and RSQLite packages must be installed to execute SQLite queries."
      ))
      return(invisible(NULL))
    }

    if (sqlitePath == "") {
      sqliteQueryResult(list(
        data = NULL,
        type = "warning",
        message = "Enter the path to a SQLite database file."
      ))
      return(invisible(NULL))
    }

    if (!file.exists(sqlitePath)) {
      sqliteQueryResult(list(
        data = NULL,
        type = "warning",
        message = sprintf("The SQLite file was not found: %s", sqlitePath)
      ))
      return(invisible(NULL))
    }

    if (sqlQuery == "") {
      sqliteQueryResult(list(
        data = NULL,
        type = "warning",
        message = "Enter a SQL query to execute."
      ))
      return(invisible(NULL))
    }

    queryOutcome <- tryCatch(
      {
        connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlitePath)
        on.exit(DBI::dbDisconnect(connection), add = TRUE)

        isResultQuery <- grepl("^(SELECT|WITH|PRAGMA|EXPLAIN)\\b", sqlQuery, ignore.case = TRUE)

        if (isResultQuery) {
          queryResult <- DBI::dbGetQuery(connection, sqlQuery)
          list(
            data = queryResult,
            type = "success",
            message = sprintf("Query completed successfully. %s row(s) returned.", nrow(queryResult))
          )
        } else {
          affectedRows <- DBI::dbExecute(connection, sqlQuery)
          list(
            data = data.frame(
              Message = sprintf("Statement executed successfully. %s row(s) affected.", affectedRows),
              stringsAsFactors = FALSE
            ),
            type = "success",
            message = "Statement executed successfully."
          )
        }
      },
      error = function(error) {
        list(
          data = NULL,
          type = "danger",
          message = conditionMessage(error)
        )
      }
    )

    sqliteQueryResult(queryOutcome)
  }, ignoreInit = TRUE)

  output$sqlite_query_status <- shiny::renderUI({
    queryResult <- sqliteQueryResult()

    shiny::HTML(sprintf(
      "<div class=\"alert alert-%s\">%s</div>",
      queryResult$type,
      htmltools::htmlEscape(queryResult$message)
    ))
  })

  output$sqlite_query_results <- shiny::renderUI({
    queryResult <- sqliteQueryResult()
    if (is.null(queryResult) || is.null(queryResult$data)) {
      return(NULL)
    }

    tags$div(
      class = "table-responsive",
      buildSqliteResultsTable(queryResult$data)
    )
  })
}

ui <- fluidPage(
  suppressDependencies("bootstrap"),
  shiny::htmlTemplate(
    filename = "www/index.html",
    compareResultsSectionUi = uiOutput("compare_results_section_ui"),
    conceptCoverageUi = tags$div(
      class = "concept-coverage-panel",
      plotOutput("concept_coverage_plot")
    ),
    sqliteQueryUi = tags$div(
      class = "sqlite-query-panel",
      textInput(
        inputId = "sqlite_path",
        label = "SQLite file path",
        value = Sys.getenv('sqlitePath'),
        placeholder = "/path/to/database.sqlite"
      ),
      textAreaInput(
        inputId = "sqlite_query",
        label = "SQL query",
        value = "SELECT name, type FROM sqlite_master ORDER BY type, name LIMIT 100;",
        width = "100%",
        rows = 12
      ),
      actionButton("sqlite_execute", "Execute", class = "btn-primary"),
      tags$div(style = "margin-top: 1rem;", htmlOutput("sqlite_query_status")),
      uiOutput("sqlite_query_results")
    )
  ),
  tags$head(
    tags$script(src = "js/loadResults.js"),
    tags$script("Shiny.addCustomMessageHandler('results', loadResults);"),
    tags$style(HTML("
      #sqlite_query {
        font-family: monospace;
      }

      .sqlite-query-panel .form-group {
        margin-bottom: 1.5rem;
      }
    "))
  )
)

shiny::shinyApp(ui = ui, server = server)
