#' Plot concept mapping coverage
#' 
#' Finds mapping coverage from a given DQD results file, and creates barplot
#' 
#' @param jsonPath the path to the DQD json results file
#' 
#' @author Elena Garcia Lara
#' @author Maxim Moinat
#' 
#' @return A figure to visualize concept mapping coverage per domain, or NULL when no coverage data is available
#' @export
#' 
#' @examples 
#' \dontrun{
#'   plotConceptCoverage("dqd_results.json")
#' }
plotConceptCoverage <- function(jsonPath, domains = c("VISIT", "PROCEDURE", "DRUG", "CONDITION", "MEASUREMENT", "OBSERVATION", "MEAS-UNIT", "OBS-UNIT")) {
  coverage_results <- .getCoverageResults(jsonPath, domains)

  if (is.null(coverage_results)) {
    return(NULL)
  }
  
  # Barplot styled after EHDEN DoA
  p <- coverage_results %>%
    ggplot(aes(x=coverageType, y = mappingCoverage, fill = coverageType)) +
    geom_col() +
    geom_text(
      aes(label = scales::percent(mappingCoverage, accuracy = 0.01)), 
      position = position_stack(vjust = 0.5), 
      size = 3,
      colour = "gray10",
      fontface = "bold"
    ) +
    theme_minimal() +
    theme(
      axis.text.y=element_text(size = 10),
      strip.placement = "outside",
      strip.text.y = element_text(angle = 0, hjust = 0.5, face = "bold", size = 6)
    ) +
    coord_flip() +
    facet_grid(domainField ~ ., scales = "free_y", space = "free_y", switch = "y") +
    guides(fill='none') +
    ylab("Percentage Coverage (%)") + 
    xlab("") + 
    scale_fill_manual(values=c("cornflowerblue", "skyblue"))
  
  return(p)
}

#' Table concept mapping coverage
#' 
#' Finds mapping coverage from a given DQD results file.
#' 
#' @param jsonPath the path to the DQD json results file
#' 
#' @author Elena Garcia Lara
#' @author Maxim Moinat
#' 
#' @return A dataframe with coverage_results
#' @export
#' 
#' @examples 
#' \dontrun{
#'   tableConceptCoverage("dqd_results.json")
#' }
tableConceptCoverage <- function(jsonPath){
  coverage_results <- .getCoverageResults(jsonPath)

  table <- coverage_results %>% 
    dplyr::mutate(
      percentageMapped = scales::percent(mappingCoverage, accuracy = 0.01),
      nMapped = formatC(NUM_DENOMINATOR_ROWS - NUM_VIOLATED_ROWS, format="d", big.mark=","),
      nTotal = formatC(NUM_DENOMINATOR_ROWS, format="d", big.mark=",")
    ) %>%
    dplyr::select(domainField, coverageType, percentUnmapped, nUnmapped, nTotal) %>% 
    dplyr::arrange(domainField, desc(coverageType))  # by domain, terms first, then records

  return(table)
}

.getCoverageResults <- function(jsonPath, domains) {
  result <- convertJsonResultsFileCase(jsonPath, writeToFile = FALSE, targetCase = 'snake')

  if (!is.list(result) ||
      is.null(result$CheckResults) ||
      !is.data.frame(result$CheckResults) ||
      nrow(result$CheckResults) == 0) {
    return(NULL)
  }

  requiredColumns <- c(
    "CHECK_NAME",
    "CDM_TABLE_NAME",
    "CDM_FIELD_NAME",
    "PCT_VIOLATED_ROWS",
    "NUM_DENOMINATOR_ROWS",
    "NUM_VIOLATED_ROWS"
  )

  if (!all(requiredColumns %in% names(result$CheckResults))) {
    return(NULL)
  }

  coverageResults <- result$CheckResults %>%
    dplyr::filter(CHECK_NAME %in% c("standardConceptRecordCompleteness", "sourceValueCompleteness")) %>%
    # Not interested in eras as these are all derived
    dplyr::filter(!(CDM_TABLE_NAME %in% c("DRUG_ERA", "DOSE_ERA", "CONDITION_ERA"))) %>%
    dplyr::mutate(
      # First check is over all records, second over the unique source terms
      coverageType = dplyr::recode_values(
        CHECK_NAME, 
        "standardConceptRecordCompleteness" ~ "Records",
        "sourceValueCompleteness" ~ "Terms"
      ),
      # Coverage is rows not failing
      mappingCoverage = 1 - PCT_VIOLATED_ROWS,
      # Naming of domains
      domain = gsub("_(OCC\\w+|EXP\\w+|PLAN.+)$", "", CDM_TABLE_NAME),
      variable = ifelse(
        CHECK_NAME == "standardConceptRecordCompleteness", 
        sub('_CONCEPT_ID', '', CDM_FIELD_NAME), 
        sub('_SOURCE_VALUE', '', CDM_FIELD_NAME)
      ),
      domain_abbrev = recode_values(
        domain, 
        "VISIT" ~ "VST",
        "CONDITION" ~ "COND",
        "PROCEDURE" ~ "PROC",
        "OBSERVATION" ~ "OBS",
        "MEASUREMENT" ~ "MEAS",
        "SPECIMEN" ~ "SPEC"
      ),
      domainField = ifelse(
      domain == variable,
        domain,
        paste0(domain_abbrev,"-",variable)
      )
    ) %>%
    dplyr::filter(
      domainField %in% domains,
      NUM_DENOMINATOR_ROWS > 0
    )

  if (nrow(coverageResults) == 0) {
    return(NULL)
  }

  coverageResults
}
