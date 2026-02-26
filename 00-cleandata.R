## GOAL: Process the raw dataset to (1) rename and reclass variables, (2) derive new variables, and (3) produce single cleaned dataframe with all relevant variables

rm(list = ls())
library(dplyr)


# Load datasets
participants <- read.csv("data/participant.csv")
questionnaire <- read.csv("data/questionnaire.csv")
clinic <- read.csv("data/clinic.csv")
#remove 'id' column from questionnaire and clinic dataframes
questionnaire <- questionnaire %>% dplyr::select(-id)
clinic <- clinic %>% dplyr::select(-id)
#now merge datasets
alldata <- merge(participants, questionnaire, by = "pid", all.x = TRUE)
alldata <- merge(alldata, clinic, by = "pid", all.x = TRUE)
alldata[alldata == ""] <- NA

## remove participants who did not answer the diag_2_m question (i.e. those who did not complete v2 of the questionnaire)
alldata <- alldata %>% dplyr::filter(!is.na(diag_2_m))
## now remove unnecessary columns
alldata <- alldata %>% dplyr::select(-c("diag_2_m", "sleep_trouble_1_1", "demog_sex_1_1"))

## Rename columns to more interpretable variable names
alldata <- alldata %>% rename(
  "orig_ethnicity" = "demog_ethnicity_1_1",
  "sex" = "demog_sex_2_1",
  "diag_psych" = "diag_psych_1_m",
  "phq1" = "phq9_item1_interest_1_1",
  "phq2" = "phq9_item2_down_1_1",
  "phq3" = "phq9_item3_sleep_1_1",
  "phq4" = "phq9_item4_energy_1_1",
  "phq5" = "phq9_item5_appetite_1_1",
  "phq6" = "phq9_item6_bad_1_1",
  "phq7" = "phq9_item7_concentr_1_1",
  "phq8" = "phq9_item8_movement_1_1",
  "phq9" = "phq9_item9_harm_1_1",
  "diag_endoc" = "diag_endocr_1_m",
  "birthplace" = "birth_place_1_1",
  "immigrate_year" = "immigrate_uk_yr_1_1",
  "education" = "edu_qual_1_m",
  "income" = "housing_income_1_1",
  "orig_alcohol" = "alcohol_curr_1_1",
  "orig_smoking" = "smoke_tobacco_type_1_m",
  "orig_social" = "lifestyle_social_visits_1_1",
  "orig_fatherpsych" = "father_diag_a_2_m",
  "orig_motherpsych" = "mother_diag_a_2_m"
)

###########################################################
########### CLEAN, RECODE, AND/OR DERIVE OUTCOME VARIABLES: depression diagnosis, diabetes diagnosis, and PHQ-9 score
###########################################################

alldata <- alldata %>% mutate(depression = case_when(
  diag_psych == "Do not know" ~ NA,
  diag_psych == "Prefer not to answer" ~ NA,
  grepl("Depression", diag_psych, ignore.case = T) ~ TRUE,
  TRUE ~ FALSE
))

alldata <- alldata %>% mutate(diabetes = case_when(
  diag_endoc == "Do not know" ~ NA,
  diag_endoc == "Prefer not to answer" ~ NA,
  grepl("Type 2 diabetes", diag_endoc, ignore.case = T) ~ TRUE,
  TRUE ~ FALSE
))

## Create new logical variable for lifetime depression diagnosis
#alldata$depression <- grepl("Depression", alldata$diag_psych, ignore.case = T)

## Create new logical variable for lifetime type 2 diabetes diagnosis
#alldata$diabetes <- grepl("Type 2 diabetes", alldata$diag_endoc, ignore.case = T)

## Recode PHQ-9 variables to numeric
alldata <- alldata %>% mutate(across(.cols = starts_with("phq"), 
                                     .fns = ~case_when(
                                       .x == "Not at all" ~ "0",
                                       .x == "Several days" ~ "1",
                                       .x == "More than half the days" ~ "2",
                                       .x == "Nearly every day" ~ "3"
                                     )))
numcols <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")
alldata[numcols] <- lapply(alldata[numcols], as.numeric)
rm(numcols)
## Create new variable for total PHQ-9 score
alldata$PHQ <- rowSums(alldata[,c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")])
## Create new binary variable for current depressive symptoms (PHQ > 9)
alldata$currdep <- ifelse(alldata$PHQ > 9, TRUE, FALSE)


###########################################################
########### CLEAN, RECODE, AND/OR DERIVE PREDICTOR VARIABLES: ethnicity, immigration status, and immigration year
###########################################################

## Recode ethnicity to appropriate categories
alldata <- alldata %>% mutate(ethnicity = case_when(
  orig_ethnicity == "Arab" ~ "Arab",
  orig_ethnicity == "Black or Black British – African" | orig_ethnicity == "Black or Black British – Caribbean" | orig_ethnicity == "Any other Black / African / Caribbean background" ~ "Black",
  orig_ethnicity == "Asian or Asian British – Indian" | orig_ethnicity == "Asian or Asian British – Bangladeshi" | orig_ethnicity == "Asian or Asian British – Pakistani" ~ "South Asian",
  orig_ethnicity == "Chinese" | orig_ethnicity == "Any other Asian/Asian British background" ~ "Other Asian",
  orig_ethnicity == "Mixed – White and Asian" | orig_ethnicity == "Mixed – White and Black African" | orig_ethnicity == "Mixed – White and Black Caribbean" | orig_ethnicity == "Any other mixed multiple ethnic background" ~ "Mixed or Multiple Heritage",
  orig_ethnicity == "White – English / Welsh / Scottish / Northern Irish / British" | orig_ethnicity == "White – Irish" | orig_ethnicity == "White – Gypsy or Irish Traveller" | orig_ethnicity == "White – Polish" | orig_ethnicity == "Any other white background" ~ "White",
  orig_ethnicity == "Prefer not to answer" | orig_ethnicity == "Other" ~ NA
))

## Recode birthplace to appropriate categories to create immigration status variable
alldata <- alldata %>% mutate(immigration = case_when(
  birthplace == "England" |  birthplace == "Scotland" | birthplace == "Wales" | birthplace == "Northern Ireland" | birthplace == "UK (don't know country)" ~ "Non-Immigrant",
  birthplace == "Do not know" | birthplace == "Prefer not to answer" ~ NA,
  TRUE ~ "Immigrant"
))

## Convert immigration year and consent year variables to R-friendly year format
alldata$immigrate_year <- format(as.Date(strptime(alldata$immigrate_year, format = "%Y")), "%Y")
alldata$immigrate_year <- as.numeric(alldata$immigrate_year)
alldata$consent_year <- format(as.Date(strptime(alldata$consent_year, format = "%Y")), "%Y")
alldata$consent_year <- as.numeric(alldata$consent_year)
## Calculate years since immigration at consent year
alldata$immigrate_duration <- alldata$consent_year - alldata$immigrate_year

## DROP PARTICIPANTS IF THEY HAVE ETHNICITY OR IMMIGRATION STATUS MISSING
alldata <- alldata %>% dplyr::filter(!is.na(ethnicity))
alldata <- alldata %>% dplyr::filter(!is.na(immigration))

###########################################################
########### CLEAN, RECODE, AND/OR DERIVE COVARIATES: age, sex, BMI, income, alc, cig, social, insomnia, parental psych
###########################################################

## Combine sex data from two variables (pilot vs full cohort) into a single sex column
#alldata$sex <- coalesce(alldata$orig_sex2, alldata$orig_sex1)
# recode sexes other than male or female to NA so these cases (~1200 people) can be omitted
alldata$sex[alldata$sex == "Intersex"] <- NA
alldata$sex[alldata$sex == "Prefer not to answer"] <- NA
alldata$sex[alldata$sex == "Other"] <- NA

## Convert birth_year variable to R-friendly year format
alldata$birth_year <- format(as.Date(strptime(alldata$birth_year, format = "%Y")), "%Y")
alldata$birth_year <- as.numeric(alldata$birth_year)
## Calculate age at point of consent from consent year and birth year
alldata$age <- alldata$consent_year - alldata$birth_year
## Calculate age at point of immigration
alldata$immigrate_age <- alldata$immigrate_year - alldata$birth_year
## Calculate life stage (child/adults) at point of immigration
alldata <- alldata %>% mutate(immigrate_stage = case_when(is.na(immigrate_age) ~ NA, immigrate_age < 18 ~ "Child", immigrate_age >= 18 ~ "Adult"))

## DROP PARTICIPANTS IF THEY HAVE AGE OR SEX MISSING
alldata <- alldata %>% dplyr::filter(!is.na(age))
alldata <- alldata %>% dplyr::filter(!is.na(sex))


## Calculate BMI using height and weight
## but first, replace extreme values with NA (use OFH thresholds)
alldata <- alldata %>% mutate(height = case_when(height > 299 ~ NA, height < 90 ~ NA, TRUE ~ height)) %>% mutate(height = height/100)
alldata <- alldata %>% mutate(weight = case_when(weight > 400 ~ NA, weight < 20 ~ NA, TRUE ~ weight))
alldata <- alldata %>% mutate(bmi = (weight / height^2))


# Recode missing income to NA
alldata$income[alldata$income == "Do not know"] <- NA
alldata$income[alldata$income == "Prefer not to answer"] <- NA


## Recode the following categorical variables: alcohol, smoking (logical), social (degrees, not binary), and insomnia
alldata <- alldata %>% mutate(alcohol = case_when(
  is.na(orig_alcohol) ~ NA,
  orig_alcohol == "Never" | orig_alcohol == "Special occasions only" ~ "Rarely or Never",
  orig_alcohol == "Prefer not to answer" ~ NA,
  TRUE ~ "Regularly"
))
alldata <- alldata %>% mutate(smoking = case_when(
  is.na(orig_smoking) ~ NA,
  orig_smoking == "I have not used any of these tobacco products" ~ FALSE,
  orig_smoking == "Prefer not to answer" ~ NA,
  TRUE ~ TRUE
))
alldata <- alldata %>% mutate(social = case_when(
  is.na(orig_social) ~ NA,
  orig_social == "No friends/family outside household" | orig_social == "Never or almost never" ~ "0",
  orig_social == "Once every few month" ~ "1",
  orig_social == "About once a month" ~ "2",
  orig_social == "About once a week" ~ "3",
  orig_social == "2-4 times a week" ~ "4",
  orig_social == "Almost daily" ~ "5",
  TRUE ~ NA
))
#alldata <- alldata %>% mutate(insomnia = case_when(
#  is.na(orig_insomnia) ~ NA,
#  orig_insomnia == "Never/rarely" | orig_insomnia == "Sometimes" ~ "Rarely or Never",
#  orig_insomnia == "Prefer not to answer" ~ NA,
#  TRUE ~ "Regularly"
#))

## Create new columns for family psychiatric history
## TRUE if mother/father diag_psych contains "Depression"
alldata$orig_motherpsych[alldata$orig_motherpsych == "Do not know"] <- NA
alldata$orig_motherpsych[alldata$orig_motherpsych == "Prefer not to answer"] <- NA
alldata$orig_fatherpsych[alldata$orig_fatherpsych == "Do not know"] <- NA
alldata$orig_fatherpsych[alldata$orig_fatherpsych == "Prefer not to answer"] <- NA
alldata$motherpsych <- ifelse(grepl("Depression", alldata$orig_motherpsych, ignore.case = T), TRUE, FALSE)
alldata$fatherpsych <- ifelse(grepl("Depression", alldata$orig_fatherpsych, ignore.case = T), TRUE, FALSE)



## Convert the following variables to factors: pid, ethnicity, immigration, sex, edu, income, alc, insomnia
factorcols <- c("pid", "ethnicity", "immigration", "immigrate_stage", "sex", "education", "income", "alcohol")
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
alldata$sex <- relevel(alldata$sex, ref = "Male")
alldata$income <- relevel(alldata$income, ref = "Less than £18,000")
alldata$alcohol <- relevel(alldata$alcohol, "Rarely or Never")
alldata$immigration <- relevel(alldata$immigration, "Non-Immigrant")
alldata$ethnicity <- relevel(alldata$ethnicity, ref = "White")
rm(factorcols)

## Ensure the following variables are numeric: PHQ, immigrate_year, immigrate_duration, age, immigrate_age, bmi, social
numcols <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9", "PHQ", "immigrate_year", "immigrate_duration", "immigrate_year", "age", "bmi", "social")
alldata <- alldata %>% mutate(across(numcols, ~ as.numeric(.)))
rm(numcols)

## DROP PARTICIPANTS IF THEIR IMMIGRATION AGE OR DURATION IS NEGATIVE
alldata <- alldata %>% dplyr::filter(immigrate_age >= 0 | is.na(immigrate_age))
alldata <- alldata %>% dplyr::filter(immigrate_duration >= 0 | is.na(immigrate_duration))

## Finally, delete unnecessary columns and reorder columns as needed to generate cleaned dataset
alldata <- alldata %>% dplyr::select(-c(
  "orig_ethnicity", "diag_psych", "diag_endoc", "birthplace", "education", "orig_alcohol", "orig_smoking", "orig_social", "orig_fatherpsych", "orig_motherpsych", "consent_year", "birth_year"
))
alldata <- alldata %>% relocate(
  pid, #participant ID
  depression, currdep, PHQ, diabetes, #outcomes
  ethnicity, immigration, immigrate_year, immigrate_duration, immigrate_age, immigrate_stage, #predictors
  age, sex, height, weight, bmi, income, alcohol, smoking, social, fatherpsych, motherpsych, #covariates
  phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, phq9, #phq-9 items
)


summary(alldata)

write.csv(alldata, "cleandata/cleandata.csv", row.names = FALSE)


rm(participants, clinic, questionnaire)
