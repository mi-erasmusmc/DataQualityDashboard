# @file compareDqdResults.R
#
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


#' Create an interactive scatter plot comparing two DQD results
#' 
#' Matches checks from the two provided DQD results and saves two outputs:
#' \itemize{
#'  \item a table with all checks that are different
#'  \item an interactive plot (html) with old fail percentage versus new fail
#'  percentage
#' } 
#' This makes it easy to identify differences between subsequent runs of DQD.
#' Nothing will be created if there are no differences between the given DQD results.
#' See also: \link[Comparing Data Quality Dashboard results from consecutive ETL iterations: two new visualizations and one utility script]{https://www.ohdsi.org/wp-content/uploads/2021/09/67-abstract-Lara.pdf}
#' 
#' @param jsonFilePathOld the path to the old DQD json results file
#' @param jsonFilePathNew the path to the new DQD json results file
#' @param outputPath the path to write the resulting figure to as html file
#' 
#' @author Elena Garcia Lara
#' @author Maxim Moinat
#' 
#' @return An interactive plotly figure or nothing if no differences are found.
#' @export
#' 
#' @examples 
#' \dontrun{
#'   plotCompareDqdResults("dqd_results_1.json", "dqd_results_2.json", "output")
#' }
plotCompareDqdResults <- function(jsonFilePathOld, jsonFilePathNew, outputPath = NA) {
  p <- .plotCompareDqdResults(jsonFilePathOld, jsonFilePathNew) %>%
    plotly::ggplotly(tooltip="text") %>%
    plotly::style(hoveron="text")

  if (!is.na(outputPath)) {
    savingName <- file.path(outputPath, paste("compare_dqd", Sys.Date(), sep="_"))
    dir.create(file.path(outputPath), showWarnings = FALSE)
    htmlwidgets::saveWidget(p_interactive, file=paste(savingName, ".html", sep=""))
  }
  
  return(p)
}

#' Comparison scatter plot. The base funciton used to create the figure with interaction
.plotCompareDqdResults <- function(jsonFilePathOld, jsonFilePathNew){
  combinedResult <- .joinDqdResults(jsonFilePathOld, jsonFilePathNew, suffixes = c(".old", ".new"))
  
  # Only keep changed
  combinedResult <- combinedResult %>%
    filter(PCT_VIOLATED_ROWS.old != PCT_VIOLATED_ROWS.new)

  # When no difference found, exit function
  if(nrow(combinedResult)==0){
    stop("No differences found.")
  }
  
  # Visualization
  combinedResult %>%
    dplyr::mutate(       
      fail_status = ifelse(
        FAILED.old, 
        ifelse(FAILED.new, "Fail-to-Fail", "Fail-to-Pass"),
        ifelse(FAILED.new, "Pass-to-Fail", "Pass-to-Pass")
      ),
      pct_old = round(PCT_VIOLATED_ROWS.old*100, digits=2),
      pct_new = round(PCT_VIOLATED_ROWS.new*100, digits=2)
    ) %>% 
    ggplot2::ggplot(
      aes(
        x = pct_old,
        y = pct_new,
        colour = fail_status,
        text = paste(
          sprintf('<br><i>Check name: </i>%s', CHECK_NAME),
          sprintf('<br><i>Table: </i>%s', CDM_TABLE_NAME),
          sprintf('<br><i>Field: </i>%s', CDM_FIELD_NAME),
          sprintf('<br><i>Threshold value: </i>%.1f%%', THRESHOLD_VALUE.new),
          sprintf('<br><b><i>old: </i> %.2f%% </b>', pct_old),
          sprintf('<br><b><i>new: </i> %.2f%% </b>', pct_new)
        ), 
        alpha = 0.6
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(colour="gray", linetype = "dashed")+
    ggplot2::scale_colour_manual(
      labels = c("Fail-to-Pass", "Fail-to-Fail", "Pass-to-Pass", "Pass-to-Fail"),
      values = c("Pass-to-Pass" = "lightblue", "Fail-to-Fail" = "chocolate1",
                 "Fail-to-Pass" = "darkblue", "Pass-to-Fail" = "coral")
    )+
    ggplot2::scale_alpha(guide = 'none') +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::lims(y=c(0,100), x=c(0,100)) +
    ggplot2::labs(x="Previous % of row fails", y="Current % of row fails") +
    ggplot2::annotate("text", label="Improved", x = 88.5, y = 12.5, colour="grey") +
    ggplot2::annotate("text", label="Worsened", x = 12.5, y = 88.5, colour="grey")
}


#' Create table comparing two DQD results.
#' TODO: add some additional value here. e.g. combine with the join function. The filter is done in plot as well.
#' 
#' @param jsonFilePathOld the path to the old DQD json results file
#' @param jsonFilePathNew the path to the new DQD json results file
#' 
#' @author Elena Garcia Lara
#' @author Maxim Moinat
#' 
#' @return An overview of all differing checks
#' @export
#' 
#' @examples 
#' \dontrun{
#'   tableCompareDqdResults("dqd_results_1.json", "dqd_results_2.json", "output")
#' }
tableCompareDqdResults <- function(jsonFilePathOld, jsonFilePathNew){
  combinedResult <- .joinDqdResults(jsonFilePathOld, jsonFilePathNew, suffixes = c(".old", ".new"))
  
  # Only keep changed
  combinedResult <- combinedResult %>%
    filter(PCT_VIOLATED_ROWS.old != PCT_VIOLATED_ROWS.new) %>%
    select(
      CHECK_NAME,
      CDM_TABLE_NAME,
      CDM_FIELD_NAME,
      CONCEPT_ID,
      # UNIT_CONCEPT_ID,
      PCT_VIOLATED_ROWS.old,
      NUM_DENOMINATOR_ROWS.old,
      FAILED.old,
      PCT_VIOLATED_ROWS.new,
      NUM_DENOMINATOR_ROWS.new,
      FAILED.new,
      NOTES_VALUE.old,
      NOTES_VALUE.new
    )
  
  return(combinedResult)
}

#' Joins two DQD results
#' 
#' @param jsonPath1 the path to the first DQD json results file
#' @param jsonPath2 the path to the second DQD json results file
#' 
#' @return A tibble with the joined dqd results, one row per DQD check.
.joinDqdResults <- function(jsonPath1, jsonPath2, suffixes = c(".1", ".2")){
  r1 <- convertJsonResultsFileCase(jsonPath1, writeToFile = FALSE, targetCase = 'snake')
  # cr1 <- tibble(r1$CheckResults)
  
  r2 <- convertJsonResultsFileCase(jsonPath2, writeToFile = FALSE, targetCase = 'snake')
  # cr2 <- tibble(r2$CheckResults)

  joinedCheckResults <- r1$CheckResults %>%
    dplyr::left_join(
      r2$CheckResults,
      by = dplyr::join_by(
        CHECK_NAME,
        CDM_TABLE_NAME,
        CDM_FIELD_NAME,
        CONCEPT_ID
        # UNIT_CONCEPT_ID
      ),
      suffix = suffixes
    )

  return(joinedCheckResults)
}