library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)

ls()
measurements <- dataset_18077420_measurement_df
height <- measurements %>% dplyr::filter(standard_concept_name == "Body height")
weight <- measurements %>% dplyr::filter(standard_concept_name == "Body weight")
person <- dataset_18077420_person_df
surveydata <- dataset_18077420_survey_df

colnames(surveydata)

## reduce each data file to relevant columns
surveydata <- surveydata %>% dplyr::select(person_id, question, answer, survey_datetime)
weight <- weight %>% dplyr::select(person_id, standard_concept_name, value_as_number, measurement_datetime)
height <- height %>% dplyr::select(person_id, standard_concept_name, value_as_number, measurement_datetime)
person <- person %>% mutate(question = rep("birthdate")) %>% dplyr::select(person_id, question, date_of_birth)

## rename columns so they are interpretable when merging dataframes
surveydata <- surveydata %>% rename(question = question, answer = answer, date = survey_datetime)
height <- height %>% rename(question = standard_concept_name, answer = value_as_number)
weight <- weight %>% rename(question = standard_concept_name, answer = value_as_number)
person <- person %>% rename(answer = date_of_birth)

dflist <- list(person, surveydata, height, weight)
alldata <- rbindlist(dflist, fill = TRUE)
rm(dflist, height, weight, person, surveydata)

## convert survey dates to R-friendly format
alldata <- alldata %>% mutate(date = ymd_hms(date, tz = "UTC"))
alldata <- alldata %>% group_by(person_id) %>% arrange(date) %>% ungroup() ## arrange by date, so that later PHQ items can be removed
# only keep PHQ items if they were part of PHQ-9, not if part of PHQ-3 (i.e. remove any PHQ items recorded after July 2020)
alldata <- alldata %>% 
  dplyr::filter_out(question == "In the past 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless." &  as_datetime(date) > as_date("2020-07-31")) %>% 
  dplyr::filter_out(question == "In the past 2 weeks, how often have you been bothered by little interest or pleasure in doing things." & as_datetime(date) > as_date("2020-07-31")) %>% 
  dplyr::filter_out(question == "In the past 2 weeks, how often have you been bothered by thoughts that you would be better off dead or of hurting yourself in some way." & as_datetime(date) > as_date("2020-07-31"))

alldata <- alldata %>% mutate(date = year(date))
alldata <- alldata %>% group_by(person_id) %>% tidyr::fill(date, .direction = "downup") %>% ungroup() # fill dates where NA
alldata <- alldata %>% group_by(person_id) %>% mutate(date = min(date)) %>% ungroup() # keep earliest date of engagement with the cohort as the 'reporting date' for birthdate to allow for pivoting without duplicates

## convert to a wide format dataframe, with questions pivoted to individual columns
alldata <- alldata %>% pivot_wider(
  names_from = question,
  values_from = answer,
  values_fn = ~paste0(., collapse = "|") # where people have selected multiple options for a question, concatenate them with "|" separator
)

## convert birth date to R-friendly format
alldata <- alldata %>% mutate(birthdate = ymd_hms(birthdate, tz = "UTC")) %>% mutate(birthdate = year(birthdate))

colnames(alldata)

## rename variables
alldata <- alldata %>% rename(
  
  # outcomes
  diag_psych = "Have you or anyone in your family ever been diagnosed with the following mental health or substance use conditions? Think only of the people you are related to by blood. Select all that apply.",
  diag_dep = "Including yourself, who in your family has had depression? Select all that apply.",
  dep_treat = "Are you still seeing a doctor or health care provider for depression?",
  dep_meds = "Are you currently prescribed medications and/or receiving treatment for depression?",
  dep_onset = "About how old were you when you were first told you had depression?",
  
  phq1 = "In the past 2 weeks, how often have you been bothered by little interest or pleasure in doing things.",
  phq2 = "In the past 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless.",
  phq3 = "In the past 2 weeks, how often have you been bothered by trouble falling or staying asleep, or sleeping too much.",
  phq4 = "In the past 2 weeks, how often have you been bothered by feeling tired or having little energy.",
  phq5 = "In the past 2 weeks, how often have you been bothered by poor appetite or overeating.",
  phq6 = "In the past 2 weeks, how often have you been bothered by feeling bad about yourself or that you are a failure or have let yourself or your family down.",
  phq7 = "In the past 2 weeks, how often have you been bothered by trouble concentrating on things, such as reading the newspaper or watching television.",
  phq8 = "In the past 2 weeks, how often have you been bothered by moving or speaking so slowly that other people could have noticed? or the opposite - being so fidgety or restless that you have been moving around a lot more than usual.",
  phq9 = "In the past 2 weeks, how often have you been bothered by thoughts that you would be better off dead or of hurting yourself in some way.",
  
  ## predictors
  birthcountry = "The Basics: Birthplace",
  ethnicity = "Race: What Race Ethnicity",
  ethn_asian = "Asian: Asian Specific",
  ethn_black = "Black: Black Specific",
  ethn_mena = "MENA: MENA Specific",
  ethn_latino = "Hispanic: Hispanic Specific",
  ethn_white = "White: White Specific",
  
  ## covariates
  sex = "Biological Sex At Birth: Sex At Birth",
  height = "Body height",
  weight = "Body weight",
  income = "Income: Annual Income",
  alcohol = "Alcohol: Drink Frequency Past Year",
  smoking = "Smoking: 100 Cigs Lifetime",
  ucla1 = "How often do you feel lack companionship?",
  ucla2 = "How often do you feel that there is no one you can turn to?",
  ucla3 = "How often do you feel that you are an outgoing person?",
  ucla4 = "How often do you feel left out?",
  ucla5 = "How often do you feel isolated from others?",
  ucla6 = "How often do you fell that you can find companionship when you want it?",
  ucla7 = "How often do you feel that you are unhappy being so withdrawn?",
  ucla8 = "How often do you feel that people are around you but not with you?"
  
)

## reorder variables for final dataframe
alldata <- alldata %>% select(
  person_id,
  date, birthdate, sex, # basic demos
  diag_psych, diag_dep, dep_treat, dep_meds, dep_onset, phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, phq9, # outcomes
  birthcountry, ethnicity, ethn_asian, ethn_black, ethn_latino, ethn_mena, ethn_white, # predictors
  height, weight, income, alcohol, smoking, ucla1, ucla2, ucla3, ucla4, ucla5, ucla6, ucla7, ucla8 # covariates
)

alldata <- data.frame(alldata)


fwrite(alldata, "02-cleandata/mergeddata.csv", row.names = FALSE)
backupcopy <- alldata

#rm(list=setdiff(ls(), c("alldata", "backupcopy")))
