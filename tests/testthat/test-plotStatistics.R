library(testthat)

test_that("plotConceptCoverage handles results without coverage checks", {
  jsonFilePath <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFilePath), add = TRUE)

  jsonlite::write_json(
    list(
      Metadata = data.frame(
        cdmVersion = "5.3",
        stringsAsFactors = FALSE
      ),
      CheckResults = data.frame(
        checkName = "measurePersonCompleteness",
        cdmTableName = "PERSON",
        cdmFieldName = "PERSON_ID",
        pctViolatedRows = 0,
        numDenominatorRows = 1,
        numViolatedRows = 0,
        checkId = 1,
        stringsAsFactors = FALSE
      )
    ),
    path = jsonFilePath,
    auto_unbox = TRUE
  )

  expect_no_error({
    plot <- plotConceptCoverage(jsonFilePath)
  })
  expect_null(plot)
})
