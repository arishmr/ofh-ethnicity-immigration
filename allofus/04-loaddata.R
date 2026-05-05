rm(list=ls())

## Load data

aou_dep_ethnicity <- read.csv("03-results/aou_logreg_dep_ethnicity.csv")
aou_dep_immigration <- read.csv("03-results/aou_logreg_dep_immigration.csv")
aou_dep_ethnicity <- aou_dep_ethnicity %>% mutate(cohort = rep("All Of Us")) %>% relocate(cohort)
aou_dep_immigration <- aou_dep_immigration %>% mutate(cohort = rep("All Of Us")) %>% relocate(cohort)

ofh_dep_ethnicity <- read.csv("03-results/ofh_logreg_dep_ethnicity.csv")
ofh_dep_immigration <- read.csv("03-results/ofh_logreg_dep_immigration.csv")
ofh_dep_ethnicity <- ofh_dep_ethnicity %>% mutate(cohort = rep("Our Future Health")) %>% relocate(cohort)
ofh_dep_immigration <- ofh_dep_immigration %>% mutate(cohort = rep("Our Future Health")) %>% relocate(cohort)

dep_ethnicity <- bind_rows(aou_dep_ethnicity, ofh_dep_ethnicity)
dep_immigration <- bind_rows(aou_dep_immigration, ofh_dep_immigration)

rm(aou_dep_ethnicity, aou_dep_immigration, ofh_dep_ethnicity, ofh_dep_immigration)

## create a function to transform outcomes and convert relevant variables to factors for logistic regression outputs
logregtransform <- function (df) {
    df <- df %>% mutate(outcome = outcome %>% replace_values(
      "currdep" ~ "Probable Current Depression",
      "depression" ~ "Lifetime Depression")) %>% 
      mutate(outcome = factor(outcome, levels = c("Lifetime Depression", "Probable Current Depression"))) %>%
      mutate(across(.cols = c(ethnicity, immigration), .fns = ~as.factor(.))) %>%
      mutate(model = model %>%
               replace_values(
                 "minimal" ~ "Minimally Adjusted",
                 "full" ~ "Fully Adjusted")) %>%
      mutate(model = factor(model, levels = c("Minimally Adjusted", "Fully Adjusted")))
}

## run this function on all the logistic regression output dataframes
dep_ethnicity <- logregtransform(dep_ethnicity)
dep_immigration <- logregtransform(dep_immigration)

## manually set reference levels for predictor variables
dep_ethnicity$immigration <- relevel(dep_ethnicity$immigration, "Non-Immigrant")
dep_immigration$ethnicity <- relevel(dep_immigration$ethnicity, ref = "Overall")

rm(logregtransform)
