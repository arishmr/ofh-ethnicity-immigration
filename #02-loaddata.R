## GOAL: Load the cleaned dataset and reclass variables as appropriate in preparation for analysis

rm(list = ls())
library(dplyr)
library(tidyr)

# Load dataset
alldata <- read.csv("data/cleandata.csv")

## Convert the following variables to factors: pid, ethnicity, immigration, sex, edu, income, alc, cig, insomnia
factorcols <- c("pid", "ethnicity", "immigration", "immigrate_stage", "sex", "income", "alcohol", "smoking", "insomnia", "depression", "currdep", "diabetes", "fatherpsych", "motherpsych")
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
alldata$sex <- relevel(alldata$sex, ref = "Male")
alldata$income <- relevel(alldata$income, ref = "Less than £18,000")
alldata$alcohol <- relevel(alldata$alcohol, "Rarely or Never")
alldata$smoking <- relevel(alldata$smoking, "Rarely or Never")
alldata$immigration <- relevel(alldata$immigration, "Non-Immigrant")
alldata$immigrate_stage <- relevel(alldata$immigrate_stage, "Adult")
alldata$ethnicity <- relevel(alldata$ethnicity, ref = "White")
rm(factorcols)

## Ensure the following variables are numeric: PHQ, immigrate_year, immigrate_duration, age, bmi, social
numcols <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9", "PHQ", "immigrate_year", "immigrate_duration", "immigrate_age", "age", "bmi", "social")
alldata <- alldata %>% mutate(across(numcols, ~ as.numeric(.)))
rm(numcols)


#summary(alldata)


######### IF USING COMPLETE CASE ANALYSIS
#alldata <- alldata %>% tidyr::drop_na(bmi, income, alcohol, smoking, social, insomnia, fatherpsych, motherpsych)