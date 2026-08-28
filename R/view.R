# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of DataQualityDashboard
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' View DQ Dashboard
#'
#' @param jsonPath        The fully-qualified path to the JSON file produced by \code{\link{executeDqChecks}}
#' @param compareJsonPath Optional fully-qualified path to a second JSON file produced by
#'                        \code{\link{executeDqChecks}}. When provided, the Shiny app shows
#'                        an interactive comparison plot generated using
#'                        \code{\link{plotCompareDqdResults}}.
#' @param launch.browser  Passed on to \code{shiny::runApp}
#' @param display.mode    Passed on to \code{shiny::runApp}
#' @param ...             Extra parameters for shiny::runApp() like "port" or "host"
#'
#' @return NULL (launches Shiny application)
#'
#' @importFrom jsonlite toJSON parse_json
#'
#' @export
viewDqDashboard <- function(
    jsonPath,
    sqlitePath = file.path(getwd(), 'data/chimera.sqlite'),
    compareJsonPath = NULL,
    launch.browser = NULL,
    display.mode = NULL,
    ...
) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package must be installed to use this function. Please install it with: install.packages(\"shiny\")", call. = FALSE)
  }

  oldJsonPath <- Sys.getenv("jsonPath", unset = NA_character_)
  oldCompareJsonPath <- Sys.getenv("compareJsonPath", unset = NA_character_)
  on.exit({
    if (is.na(oldJsonPath)) {
      Sys.unsetenv("jsonPath")
    } else {
      Sys.setenv(jsonPath = oldJsonPath)
    }

    if (is.na(oldCompareJsonPath)) {
      Sys.unsetenv("compareJsonPath")
    } else {
      Sys.setenv(compareJsonPath = oldCompareJsonPath)
    }
  }, add = TRUE)

  Sys.setenv(jsonPath = jsonPath)
  Sys.setenv(compareJsonPath = if (is.null(compareJsonPath)) "" else compareJsonPath)
  Sys.setenv(sqlitePath = if (is.null(sqlitePath)) "" else sqlitePath)
  appDir <- system.file("shinyApps", package = "DataQualityDashboard")

  if (is.null(display.mode)) {
    display.mode <- "normal"
  }

  if (is.null(launch.browser)) {
    launch.browser <- TRUE
  }

  shiny::runApp(appDir = appDir, launch.browser = launch.browser, display.mode = display.mode, ...)
}
