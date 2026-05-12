## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  eval = TRUE,
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE, results="hide"-------------------------
library(nemsqar)

## ----asthma_01_tables, results="hide"-----------------------------------------
data("nemsqar_patient_scene_table")
data("nemsqar_response_table")
data("nemsqar_situation_table")
data("nemsqar_medications_table")

## ----alt_data_load, eval=FALSE, echo=TRUE, results="hide"---------------------
# # Store your file path, preferably in .Renviron files
# path <- file.path("some_path_to_file")
# 
# # Load data from the path using the tidyverse package `readr`
# data <- readr::read_csv(path)
# 
# # Load data from the path using base R
# data <- read.csv(file = path)

## ----glimpse_patient_scene_table----------------------------------------------
# Quick overview of column names and data types
dplyr::glimpse(nemsqar_patient_scene_table)

## ----head_patient_scene_table-------------------------------------------------
# An abbreviated look at the actual data tables
head(nemsqar_patient_scene_table, n = 10)

## ----fix_dates_examples, eval=FALSE, echo=TRUE, results="hide"----------------
# # Example: incident dates stored as character values
# example_data <- data.frame(
#   Incident_Date = c("2023-01-10", "01/12/2023", "20230114"),
#   stringsAsFactors = FALSE
# )
# 
# # Convert using lubridate (recommended)
# example_data$Incident_Date <- lubridate::parse_date_time(
#   example_data$Incident_Date,
#   orders = c("ymd", "mdy", "Ymd")
# )

## ----fix_numeric_examples, eval=FALSE, echo=TRUE, results="hide"--------------
# numeric_example <- data.frame(
#   # note that 45 here has whitespace surrounding the value
#   Patient_Age_raw = c("34", "18", "07", " 45 ")
# )
# 
# # Trim whitespace and convert to numeric
# numeric_example$Patient_Age <- as.numeric(trimws(
#   numeric_example$Patient_Age_raw
# ))

## ----fix_missing_examples, eval=FALSE, echo=TRUE, results="hide"--------------
# missing_example <- data.frame(
#   eSituation_11 = c("", "R41.82", "", "T14.90")
# )
# 
# # Replace empty strings with NA
# missing_example$eSituation_11 <- dplyr::na_if(missing_example$eSituation_11, "")
# 
# missing_example

## ----clean_column_names, echo=TRUE, eval=TRUE---------------------------------
# Define a reusable column-cleaning function
clean_cols <- function(data) {
  data |>
    dplyr::rename_with(
      .cols = tidyselect::everything(),
      ~ . |>
        gsub(pattern = "\\.|\\(|-|\\s", replacement = "_") |>
        gsub(pattern = "_+", replacement = "_") |>
        gsub(pattern = "\\)", replacement = "")
    )
}

# Apply cleaning to each table
nemsqar_patient_scene_data <- nemsqar_patient_scene_table |> clean_cols()
nemsqar_response_data <- nemsqar_response_table |> clean_cols()
nemsqar_situation_data <- nemsqar_situation_table |> clean_cols()
nemsqar_medications_data <- nemsqar_medications_table |> clean_cols()

# Inspect the cleaned patient/scene table
dplyr::glimpse(nemsqar_patient_scene_data)

## ----run_asthma_01, results="show"--------------------------------------------
# Run Asthma‑01 without grouping
asthma_01_all <- asthma_01(
  patient_scene_table = nemsqar_patient_scene_data,
  response_table = nemsqar_response_data,
  situation_table = nemsqar_situation_data,
  medications_table = nemsqar_medications_data,
  erecord_01_col = Incident_Patient_Care_Report_Number_PCR_eRecord_01,
  incident_date_col = Incident_Date,
  patient_DOB_col = Patient_Date_Of_Birth_ePatient_17,
  epatient_15_col = Patient_Age_ePatient_15,
  epatient_16_col = Patient_Age_Units_ePatient_16,
  eresponse_05_col = Response_Type_Of_Service_Requested_With_Code_eResponse_05,
  esituation_11_col = Situation_Provider_Primary_Impression_Code_And_Description_eSituation_11,
  esituation_12_col = Situation_Provider_Secondary_Impression_Description_And_Code_List_eSituation_12,
  emedications_03_col = Patient_Medication_Given_or_Administered_Description_And_RXCUI_Codes_List_eMedications_03,
  confidence_interval = TRUE,
  method = "clopper-pearson",
  conf.level = 0.95
)


# print the results
asthma_01_all

## ----run_asthma_01_age, results="show"----------------------------------------
# Run `asthma_01` for a whole dataset, group by age units.
# All core inputs remain the same. Only the .by argument is added.
asthma_01_age <- asthma_01(
  patient_scene_table = nemsqar_patient_scene_data,
  response_table = nemsqar_response_data,
  situation_table = nemsqar_situation_data,
  medications_table = nemsqar_medications_data,
  erecord_01_col = Incident_Patient_Care_Report_Number_PCR_eRecord_01,
  incident_date_col = Incident_Date,
  patient_DOB_col = Patient_Date_Of_Birth_ePatient_17,
  epatient_15_col = Patient_Age_ePatient_15,
  epatient_16_col = Patient_Age_Units_ePatient_16,
  eresponse_05_col = Response_Type_Of_Service_Requested_With_Code_eResponse_05,
  esituation_11_col = Situation_Provider_Primary_Impression_Code_And_Description_eSituation_11,
  esituation_12_col = Situation_Provider_Secondary_Impression_Description_And_Code_List_eSituation_12,
  emedications_03_col = Patient_Medication_Given_or_Administered_Description_And_RXCUI_Codes_List_eMedications_03,
  confidence_interval = TRUE,
  method = "clopper-pearson",
  conf.level = 0.95,
  # notice here that we use the `.by` argument from `dplyr::summarize` to group
  # our analysis
  .by = Patient_Age_Units_ePatient_16
)

# print the results
asthma_01_age

## ----population_asthma_01, results="show"-------------------------------------
# Run `asthma_01_population` for a whole dataset
# The code is virtually the same as `asthma_01()`, but we do not use the
# confidence interval arguments, nor the tidy dot `...` arguments for grouping
# or other operations via `dplyr::summarize`
populations_asthma_01 <- asthma_01_population(
  patient_scene_table = nemsqar_patient_scene_data,
  response_table = nemsqar_response_data,
  situation_table = nemsqar_situation_data,
  medications_table = nemsqar_medications_data,
  erecord_01_col = Incident_Patient_Care_Report_Number_PCR_eRecord_01,
  incident_date_col = Incident_Date,
  patient_DOB_col = Patient_Date_Of_Birth_ePatient_17,
  epatient_15_col = Patient_Age_ePatient_15,
  epatient_16_col = Patient_Age_Units_ePatient_16,
  eresponse_05_col = Response_Type_Of_Service_Requested_With_Code_eResponse_05,
  esituation_11_col = Situation_Provider_Primary_Impression_Code_And_Description_eSituation_11,
  esituation_12_col = Situation_Provider_Secondary_Impression_Description_And_Code_List_eSituation_12,
  emedications_03_col = Patient_Medication_Given_or_Administered_Description_And_RXCUI_Codes_List_eMedications_03
)

# print structure of the results using `base::summary()`
populations_asthma_01 |> summary()

## ----examine_filter_process---------------------------------------------------
# Display counts for each filtering step
populations_asthma_01$filter_process

