#' Converts the regular wide format thresholds file to a long format
#' Results are by default written in the extras folder.

library(tidyverse)

in_path <- "/Users/maxim/Develop/OHDSI/DataQualityDashboard/inst/csv"
out_path <- "extras/thresholdsLongFormat"
version <- "5.4"
tableLevelThreshold <- sprintf("OMOP_CDMv%s_Table_Level.csv", version)
fieldLevelThreshold <- sprintf("OMOP_CDMv%s_Field_Level.csv", version)
conceptLevelThreshold <- sprintf("OMOP_CDMv%s_Concept_Level.csv", version)

dir.create(out_path, showWarnings = FALSE)

# Table Level ---------------------
tableLevel <- read.csv(
  file.path(in_path, tableLevelThreshold),
  stringsAsFactors = TRUE
)

checkValue <- tableLevel %>%
  select(
    cdmTableName,
    !c(
      conceptPrefix,
      tableDescription,
      validation,
      userGuidance,
      etlConventions,
      ends_with("Threshold"),
      ends_with("Notes")
    )
  ) %>%
  pivot_longer(
    cols = !c(cdmTableName, schema),
    names_to = "checkName",
    values_to = "checkValue",
    cols_vary = "slowest"
  )

checkVars <- tableLevel %>%
  select(
    cdmTableName,
    schema,
    ends_with("Threshold"),
    ends_with("Notes")
  ) %>%
  pivot_longer(
    cols = !c(cdmTableName, schema),
    names_to = c("checkName", ".value"),
    names_pattern = "(.*)(Threshold|Notes)",
    cols_vary = "slowest"
  )

tableLevelChecks <- checkValue %>%
  filter(
    !(checkValue == 'No' | checkValue == '')
  ) %>%
  left_join(
    checkVars,
    by = join_by(cdmTableName, schema, checkName)
  ) %>%
  mutate(
    checkValue = if_else(checkValue == 'Yes', NA, checkValue)
  ) %>%
  select(
    cdmTableName,
    checkName,
    Threshold,
    checkParameter = checkValue,
    Notes
  )

# Filter out the isRequired, which is not a table level check
tableLevelChecks <- tableLevelChecks %>%
  filter(
    checkName != 'isRequired'
  )

# Add cdmTable, which is implicit in wide threshold file
tableLevelChecks <- rbind(
  tableLevel %>%
    mutate(
      cdmTableName,
      checkName = 'cdmTable',
      Threshold = 0,
      checkParameter = NA,
      Notes = NA,
      .keep = 'none'
    ),
  tableLevelChecks
) 

#unique(tableLevelChecks$checkName)
# View(tableLevelChecks)

write_csv(
  tableLevelChecks,
  file.path(out_path, paste0('long_', tableLevelThreshold)),
  na = ""
)

# Field Level ---------------------
fieldLevel <- read.csv(
  file.path(in_path, fieldLevelThreshold),
  # colClasses = "character",
  stringsAsFactors = TRUE
)

fieldLevel <- fieldLevel %>% rename(
  isForeignKeyTableName = fkTableName,
  isForeignKeyFieldName = fkFieldName,
  # The field name used for sourceValueCompleteness is called standardConceptFieldName
  sourceValueCompletenessFieldName = standardConceptFieldName
)

checkValue <- fieldLevel %>%
  select(
    cdmTableName,
    cdmFieldName,
    !c(
      databaseSchema,
      userGuidance,
      etlConventions,
      runForCohort,
      ends_with("Threshold"),
      ends_with("Notes"),
      ends_with("TableName"),
      ends_with("FieldName")
    )
  ) %>%
  pivot_longer(
    cols = !c(cdmTableName, cdmFieldName),
    names_to = "checkName",
    values_to = "checkValue",
    cols_vary = "slowest"
  )

checkVars <- fieldLevel %>%
  select(
    cdmTableName,
    cdmFieldName,
    ends_with("Threshold"),
    ends_with("TableName"),
    ends_with("FieldName"),
    ends_with("Notes")
  ) %>%
  pivot_longer(
    cols = !c(cdmTableName, cdmFieldName),
    names_to = c("checkName", ".value"),
    names_pattern = "(.*)(Threshold|Notes|TableName|FieldName)",
    cols_vary = "slowest"
  )

fieldLevelChecks <- checkValue %>%
  filter(
    !(checkValue == 'No' | checkValue == '')
  ) %>%
  left_join(
    checkVars,
    by = join_by(cdmTableName, cdmFieldName, checkName)
  ) %>%
  mutate(
    checkValue = if_else(checkValue == 'Yes', NA, checkValue)
  ) %>%
  select(
    cdmTableName,
    cdmFieldName,
    checkName,
    Threshold,
    checkParameter = checkValue,
    checkParameter_TableName = TableName,
    checkParameter_FieldName = FieldName,
    Notes
  )

# Remove cdmDatatype checks that are not integer
fieldLevelChecks <- fieldLevelChecks %>%
  filter(
    !(checkName == 'cdmDataType' & checkParameter != 'integer')
  )

# Add cdmField, which is implicit in wide threshold file
fieldLevelChecks <- rbind(
  fieldLevel %>%
    mutate(
      cdmTableName,
      cdmFieldName,
      checkName = 'cdmField',
      Threshold = 0,
      checkParameter = NA,
      checkParameter_TableName = NA,
      checkParameter_FieldName = NA,
      Notes = NA,
      .keep = 'none'
    ),
  fieldLevelChecks
)

# unique(fieldLevelChecks$checkName)
# View(fieldLevelChecks)

write_csv(
  fieldLevelChecks,
  file.path(out_path, paste0('long_', fieldLevelThreshold)),
  na = ""
)

# Concept Level ---------------------
conceptLevel <- read.csv(
  file.path(in_path, conceptLevelThreshold),
  # colClasses = "character",
  stringsAsFactors = F
)

checkValue <- conceptLevel %>%
  select(
    cdmTableName,
    cdmFieldName,
    conceptId,
    conceptName,
    unitConceptId,
    unitConceptName,
    !c(
      ends_with("Threshold"),
      ends_with("Notes")
    )
  ) %>%
  pivot_longer(
    cols = !1:6,
    names_to = "checkName",
    values_to = "checkValue",
    cols_vary = "slowest",
    values_transform = as.character
  ) %>%
  filter(
    !(checkValue == 'No' | checkValue == '')
  )

checkVars <- conceptLevel %>%
  select(
    cdmTableName,
    cdmFieldName,
    conceptId,
    conceptName,
    unitConceptId,
    unitConceptName,
    ends_with("Threshold"),
    ends_with("TableName"),
    ends_with("FieldName"),
    ends_with("Notes")
  ) %>%
  pivot_longer(
    cols = !1:6,
    names_to = c("checkName", ".value"),
    names_pattern = "(.*)(Threshold|Notes)",
    cols_vary = "slowest"
  ) %>%
  mutate(
    Threshold = as.integer(Threshold)
  )

conceptLevelChecks <- checkValue %>%
  left_join(
    checkVars,
    by = join_by(
      cdmTableName,
      cdmFieldName,
      checkName,
      conceptId,
      conceptName,
      unitConceptId,
      unitConceptName,
    )
  ) %>%
  mutate(
    checkValue = if_else(checkValue == 'Yes', NA, checkValue)
  ) %>%
  select(
    cdmTableName,
    cdmFieldName,
    conceptId,
    conceptName,
    unitConceptId,
    unitConceptName,
    checkName,
    Threshold,
    checkParameter = checkValue,
    Notes
  )

# unique(conceptLevelChecks$checkName)
# View(conceptLevelChecks)

write_csv(
  conceptLevelChecks,
  file.path(out_path, paste0('long_', conceptLevelThreshold)),
  na = ""
)

# Summary on all checks
checkThresholdSummary <- bind_rows(tableLevelChecks, fieldLevelChecks, conceptLevelChecks) %>%
  summarise(
    n = n(),
    min_threshold = min(Threshold, na.rm = TRUE),
    q25 = quantile(Threshold, 0.25, na.rm = TRUE),
    median_threshold = median(Threshold, na.rm = TRUE),
    q75 = quantile(Threshold, 0.75, na.rm = TRUE),
    max_threshold = max(Threshold, na.rm = TRUE),
    .by = checkName
  )
# View(checkThresholdSummary)

write_csv(
  checkThresholdSummary,
  file.path(in_path, sprintf("OMOP_CDMv%s_threshold_summary.csv", version)),
  na = ""
)

checkDescriptions <- read.csv(
  file.path(in_path, 'OMOP_CDMv5.4_Check_Descriptions.csv'),
  stringsAsFactors = F
)

df <- bind_rows(
    tableLevelChecks |> mutate(checkLevel = 'TABLE'),
    fieldLevelChecks |> mutate(checkLevel = 'FIELD'),
    conceptLevelChecks |> mutate(checkLevel = 'CONCEPT')
  ) |>
  left_join(
    checkDescriptions,
    by = join_by(checkLevel, checkName)
  ) 

checkCounts <- df |>
  filter(
    !(
      is.na(kahnContext) | # The more obscure isTemporallyConstant, validPrevalenceLow, validPrevalenceHigh
      checkName %in% c('plausibleTemporalAfter', 'plausibleDuringLife', 'plausibleGender') |
      (checkName == 'cdmDatatype' & checkParameter != 'integer') |
      (checkName == 'plausibleValueLow' & checkLevel == 'CONCEPT') |
      (checkName == 'plausibleValueHigh' & checkLevel == 'CONCEPT')
    )
  ) |>
  summarise(
    n_checks = n_distinct(checkName),
    n_resolved = n(),
    .by = c(checkLevel, kahnCategory, severity, checkName)
  ) |>
  arrange(
    desc(checkLevel), kahnCategory, severity
  )

checkCounts |>
 summarise(
    n_checks = sum(n_checks),
    n_resolved = sum(n_resolved),
    .by = c(checkLevel, kahnCategory, severity)
  ) |>
  rbind(c(
    'Total',
    '',
    '',
    sum(checkCounts$n_checks),
    sum(checkCounts$n_resolved)
  ))

# Count of 0,1,5,10,50,95 and 100% thresholds
# unique_thresholds <- bind_rows(tableLevelChecks, fieldLevelChecks, conceptLevelChecks) %>%
#   summarise(
#     n = n(),
#     `0` = sum(Threshold == 0),
#     `1` = sum(Threshold == 1),
#     `5` = sum(Threshold == 5),
#     `10` = sum(Threshold == 10),
#     `50` = sum(Threshold == 50),
#     `95` = sum(Threshold == 95),
#     `100` = sum(Threshold == 100),
#     `na` = sum(is.na(Threshold)),
#     .by = checkName
#   )

# bind_rows(tableLevelChecks, fieldLevelChecks, conceptLevelChecks) %>% filter(checkName == 'isRequired' & Threshold == 100)
