#####################################
## GOALS: ensure correct variable types, recode as necessary, and produce cleaned dataframe for analysis
#####################################
library(dplyr)
library(stringr)

# Load data
#alldata <- fread("01-cleandata/mergeddata.csv")

###########################################################
########### CLEAN, RECODE, AND/OR DERIVE OUTCOME VARIABLES: depression diagnosis and PHQ-9 score
###########################################################

## Does anyone in family have depression
alldata <- alldata %>% mutate(familydepression = case_when(
  is.na(diag_psych) ~ NA,
  diag_psych == "PMI: Dont Know" ~ NA,
  diag_psych == "PMI: Skip" ~ NA,
  diag_psych == "PMI: None" ~ FALSE,
  grepl("Depression", diag_psych, ignore.case = T) ~ TRUE,
  TRUE ~ FALSE # for people reporting other mental health conditions but not depression
))

## Does participant have depression
alldata <- alldata %>% mutate(depression = case_when(
  is.na(familydepression) ~ NA,
  familydepression == FALSE ~ FALSE,
  diag_dep == "PMI: Skip" ~ NA,
  grepl("Self", diag_dep, ignore.case = FALSE) ~ TRUE,
  TRUE ~ FALSE
))

## Does participant's father have depression
alldata <- alldata %>% mutate(fatherpsych = case_when(
  is.na(familydepression) ~ NA,
  familydepression == FALSE ~ FALSE,
  diag_dep == "PMI: Skip" ~ NA,
  grepl("Father", diag_dep, ignore.case = FALSE) ~ TRUE,
  TRUE ~ FALSE
))

## Does participant's mother have depression
alldata <- alldata %>% mutate(motherpsych = case_when(
  is.na(familydepression) ~ NA,
  familydepression == FALSE ~ FALSE,
  diag_dep == "PMI: Skip" ~ NA,
  grepl("Mother", diag_dep, ignore.case = FALSE) ~ TRUE,
  TRUE ~ FALSE
))

## Still seeing doctor for depression
alldata <- alldata %>% mutate(dep_treat = case_when(
  is.na(dep_treat) ~ NA,
  dep_treat == "PMI: Skip" ~ NA,
  dep_treat == "Are you still seeing a doctor or health care provider for depression? - Yes" ~ TRUE,
  TRUE ~ FALSE
))

## Still receiving medications for depression
alldata <- alldata %>% mutate(dep_meds = case_when(
  is.na(dep_meds) ~ NA,
  dep_meds == "PMI: Skip" ~ NA,
  dep_meds == "Are you currently prescribed medications and/or receiving treatment for depression? - Yes" ~ TRUE,
  TRUE ~ FALSE
))

## Age of onset of depression
alldata <- alldata %>% mutate(dep_onset = case_when(
  is.na(dep_onset) ~ NA,
  dep_onset == "PMI: Skip" ~ NA,
  grepl("12-17", dep_onset, ignore.case = FALSE) ~ "12-17",
  grepl("18-64", dep_onset, ignore.case = FALSE) ~ "18-64",
  grepl("0-11", dep_onset, ignore.case = FALSE) ~ "0-11",
  grepl("75", dep_onset, ignore.case = FALSE) ~ "75+",
  grepl("65-74", dep_onset, ignore.case = FALSE) ~ "65-74"
))
# relevel as factor
alldata$dep_onset <- factor(alldata$dep_onset, levels = c("18-64", "0-11", "12-17", "65-74", "75+"))

alldata <- alldata %>% relocate(depression, fatherpsych, motherpsych, dep_treat, dep_meds, dep_onset, .after = sex) %>%
  dplyr::select(-c(diag_psych, diag_dep, familydepression))

## Recode PHQ-9 variables to numeric
alldata <- alldata %>% mutate(across(.cols = starts_with("phq"), 
                                     .fns = ~str_replace_all(.x, c(
                                       "Not at all" = "0",
                                       "Several days" = "1",
                                       "More than half the days" = "2",
                                       "Nearly every day" = "3",
                                       "Skip" = NA,
                                       "Other" = NA
                                     ))))

## we can now either keep the last available score for each item, or keep all scores
# if keeping last available score
#alldata <- alldata %>% mutate(across(.cols = starts_with("phq"), 
#                                     .fns = ~str_extract(.x, "[^|]*$" 
#                                     )))

# separate out PHQ-9 scores from repeated surveys into different columns, with "T1", "T2", "T3" suffixes
alldata <- alldata %>% separate_wider_delim(starts_with("phq"), delim = "|", names_sep = "_T", too_few = "align_start")
# convert all PHQ-9 scores to numeric
alldata <- alldata %>% mutate(across(.cols = starts_with("phq"), .fns = ~as.numeric(.x)))
# calculate sum of PHQ-9 scores for each of the timepoints separately
alldata$PHQ_T1 <- rowSums(alldata[,c("phq1_T1", "phq2_T1", "phq3_T1", "phq4_T1", "phq5_T1",
                                     "phq6_T1", "phq7_T1", "phq8_T1", "phq9_T1")])
alldata$PHQ_T2 <- rowSums(alldata[,c("phq1_T2", "phq2_T2", "phq3_T2", "phq4_T2", "phq5_T2",
                                     "phq6_T2", "phq7_T2", "phq8_T2", "phq9_T2")])
alldata$PHQ_T3 <- rowSums(alldata[,c("phq1_T3", "phq2_T3", "phq3_T3", "phq4_T3", "phq5_T3",
                                     "phq6_T3", "phq7_T3", "phq8_T3", "phq9_T3")])
## drop individual item score columns
alldata <- alldata %>% dplyr::select(-c(
  "phq1_T1", "phq2_T1", "phq3_T1", "phq4_T1", "phq5_T1", "phq6_T1", "phq7_T1", "phq8_T1", "phq9_T1",
  "phq1_T2", "phq2_T2", "phq3_T2", "phq4_T2", "phq5_T2", "phq6_T2", "phq7_T2", "phq8_T2", "phq9_T2",
  "phq1_T3", "phq2_T3", "phq3_T3", "phq4_T3", "phq5_T3", "phq6_T3", "phq7_T3", "phq8_T3", "phq9_T3"
)) %>% relocate(c(PHQ_T1, PHQ_T2, PHQ_T3), .before = birthcountry)
## create column for current depressive symptoms
alldata <- alldata %>% mutate(currdep = case_when(
  is.na(PHQ_T1) & is.na(PHQ_T2) & is.na(PHQ_T3) ~ NA,
  PHQ_T1 > 9 | PHQ_T2 > 9 | PHQ_T3 > 9 ~ TRUE,
  TRUE ~ FALSE
)) %>% relocate(currdep, .before = PHQ_T1)



###########################################################
########### CLEAN, RECODE, AND/OR DERIVE PREDICTOR VARIABLES: immigration status and ethnicity
###########################################################

## Immigration status from birth country
alldata <- alldata %>% mutate(immigration = case_when(
  is.na(birthcountry) ~ NA,
  birthcountry == "PMI: Skip" ~ NA,
  birthcountry == "Birthplace: USA" ~ "Non-Immigrant",
  birthcountry == "PMI: Other" ~ "Immigrant"
)) %>% 
  relocate(immigration, .before = birthcountry) %>%
  dplyr::select(-birthcountry)
# relevel as factor
alldata$immigration <- factor(alldata$immigration, levels = c("Non-Immigrant", "Immigrant"))


## Ethnicity (broad categories: White, Black, South Asian, Other Asian, MENA, Hispanic/Latinx, Mixed or Multiple Heritage)
alldata <- alldata %>% mutate(ethnicity = case_when(
  is.na(ethnicity) ~ NA,
  ethnicity == "PMI: Skip" ~ NA,
  ethnicity == "What Race Ethnicity: Race Ethnicity None Of These" ~ NA,
  ethnicity == "PMI: Prefer Not To Answer" ~ NA,
  grepl("\\|", ethnicity) ~ "Mixed or Multiple Heritage",
  ethnicity == "What Race Ethnicity: White" ~ "White",
  ethnicity == "What Race Ethnicity: Black" ~ "Black",
  ethnicity == "What Race Ethnicity: Asian" & (ethn_asian == "Asian Specific Indian" | ethn_asian == "Pakistani") ~ "South Asian",
  ethnicity == "What Race Ethnicity: Asian" & (ethn_asian != "Asian Specific Indian" & ethn_asian != "Pakistani" & ethn_asian != "Skip") ~ "Other Asian",
  ethnicity == "What Race Ethnicity: MENA" ~ "MENA",
  ethnicity == "What Race Ethnicity: Hispanic" ~ "Hispanic or Latinx",
  TRUE ~ NA
))
alldata <- alldata %>% dplyr::select(-c(ethn_asian, ethn_black, ethn_latino, ethn_mena, ethn_white))
# relevel as factor
alldata$ethnicity <- factor(alldata$ethnicity, levels = c(
  "White", "Black", "Hispanic or Latinx", "MENA", "Mixed or Multiple Heritage", "Other Asian", "South Asian"
))


###########################################################
########### CLEAN, RECODE, AND/OR DERIVE PREDICTOR VARIABLES: age, sex, BMI, income, alcohol, smoking, loneliness, fatherdep, motherdep
###########################################################

## Derive age at first survey instance
alldata <- alldata %>% mutate(age = date - birthdate) %>% 
  relocate(age, .after = person_id) %>% 
  dplyr::select(-c(date, birthdate)) %>%
  mutate(age = as.numeric(age))

## Recode sex
alldata <- alldata %>% mutate(sex = replace_values(sex,
                                                   "Sex At Birth: Intersex" ~ NA,
                                                   "PMI: Prefer Not To Answer" ~ NA,
                                                   "Sex At Birth None Of These" ~ NA,
                                                   "PMI: Skip" ~ NA,
                                                   "Sex At Birth: Male" ~ "Male",
                                                   "Sex At Birth: Female" ~ "Female"
))
# relevel as factor
alldata$sex <- factor(alldata$sex, levels = c("Male", "Female"))


## Derive BMI
# some people had height and weight measured in a separate "instance", so this needs to be cleaned up
alldata <- alldata %>% separate_wider_delim(c(height, weight), delim = "|", names_sep = "_T", too_few = "align_start")
# coerce to numeric
alldata <- alldata %>% mutate(across(.cols = c(height_T1, height_T2, weight_T1, weight_T2),
                                     .fns = ~as.numeric(.)))
# calculate mean height and weight in case two separate measurements were recorded
alldata <- alldata %>% mutate(height = rowMeans(across(c(height_T1, height_T2)), na.rm = TRUE)) %>% 
  mutate(height = height/100) %>% 
  mutate(height = ifelse(is.nan(height), NA, height))
alldata <- alldata %>% mutate(weight = rowMeans(across(c(weight_T1, weight_T2)), na.rm = TRUE)) %>% 
  mutate(weight = ifelse(is.nan(weight), NA, weight))
# now calculate BMI
alldata <- alldata %>% mutate(bmi = (weight / height^2))
alldata <- alldata %>% dplyr::select(-c(height_T1, height_T2, height, weight_T1, weight_T2, weight)) %>%
  relocate(bmi, .after = ethnicity)


## Recode income
alldata <- alldata %>% mutate(income = replace_values(income,
                                                      "Annual Income: less 10k" ~ "<10k",
                                                      "Annual Income: 10k 25k" ~ "10-25k",
                                                      "Annual Income: 25k 35k" ~ "25-35k",
                                                      "Annual Income: 35k 50k" ~ "35-50k",
                                                      "Annual Income: 50k 75k" ~ "50-75k",
                                                      "Annual Income: 75k 100k" ~ "75-100k",
                                                      "Annual Income: 100k 150k" ~ "100-150k",
                                                      "Annual Income: 150k 200k" ~ "150-200k",
                                                      "Annual Income: more 200k" ~ ">200k",
                                                      "PMI: Prefer Not To Answer" ~ NA,
                                                      "PMI: Skip" ~ NA
))
# relevel as factor
alldata$income <- factor(alldata$income, levels = c(
  "<10k", "10-25k", "25-35k", "35-50k", "50-75k", "75-100k", "100-150k", "150-200k", ">200k"
))


## Recode current (i.e. past year) alcohol use
alldata <- alldata %>% mutate(alcohol = case_when(
  is.na(alcohol) ~ NA,
  alcohol == "Drink Frequency Past Year: Never" | alcohol == "Drink Frequency Past Year: Monthly Or Less" ~ "Drink Frequency Past Year: Rarely or Never",
  alcohol == "PMI: Prefer Not To Answer" ~ NA,
  alcohol == "PMI: Skip" ~ NA,
  TRUE ~ "Regularly"
))
# relevel as factor
alldata$alcohol <- factor(alldata$alcohol, levels = c("Rarely or Never", "Regularly"))

## Recode lifetime smoking (logical variable, TRUE if 100 smokes in lifetime)
alldata <- alldata %>% mutate(smoking = case_when(
  is.na(smoking) ~ NA,
  smoking == "PMI: Prefer Not To Answer" ~ NA,
  smoking == "PMI: Dont Know" ~ NA,
  smoking == "PMI: Skip" ~ NA,
  smoking == "100 Cigs Lifetime: Yes" ~ TRUE,
  smoking == "100 Cigs Lifetime: No" ~ FALSE
))

summary(as.factor(alldata$ucla1))
## Derive UCLA 8-item loneliness scale score
alldata <- alldata %>% mutate(across(.cols = c(ucla1, ucla2, ucla4, ucla5, ucla7, ucla8), 
                                     .fns = ~str_replace_all(.x, c(
                                       "Never" = "1",
                                       "Rarely" = "2",
                                       "Sometimes" = "3",
                                       "Often" = "4",
                                       "PMI: Skip" = NA
                                     ))))
alldata <- alldata %>% mutate(across(.cols = c(ucla3, ucla6), 
                                     .fns = ~str_replace_all(.x, c(
                                       "Never" = "4",
                                       "Rarely" = "3",
                                       "Sometimes" = "2",
                                       "Often" = "1",
                                       "Skip" = NA
                                     ))))
# convert all UCLA items to numeric
alldata <- alldata %>% mutate(across(.cols = starts_with("ucla"), .fns = ~as.numeric(.x)))
# calculate sum of UCLA loneliness scale items
alldata$loneliness <- rowSums(alldata[,c("ucla1", "ucla2", "ucla3", "ucla4",
                                         "ucla5", "ucla6", "ucla7", "ucla8")])
alldata <- alldata %>% dplyr::select(-starts_with("ucla"))

###########################################################
########### drop participants with missing data in core variables
###########################################################
alldata <- alldata %>% drop_na(c(depression, ethnicity, immigration, age, sex))

summary(alldata)

###########################################################
########### export cleaned dataset
###########################################################

#fwrite(alldata, "02-cleandata/cleandata.csv", row.names = FALSE)
