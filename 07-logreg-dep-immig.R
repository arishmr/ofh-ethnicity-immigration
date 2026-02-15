## GOAL: within people who immigrated in each ethnicity subgroup, calculate Odds Ratios for depression (lifetime/current) comparing:
# immigrated >10 years ago compared to <10 years ago
# immigrated as <18 years old compared to immigrated in adulthood

library(dplyr)
library(tidyr)

data <- alldata %>% dplyr::filter(immigration == "Immigrant")

data <- alldata
data <- data %>% mutate(undiagdep = case_when(
  depression == FALSE & currdep == TRUE ~ TRUE,
  TRUE ~ FALSE
)) %>%
  relocate(undiagdep, .after = currdep)

data <- data %>% mutate(immigrate_duration = case_when(
  immigrate_duration >= 5 ~ "5 years or more",
  TRUE ~ "Less than 5 years"
))
data$immigrate_duration <- as.factor(data$immigrate_duration)
data$immigrate_duration <- relevel(data$immigrate_duration, ref = "5 years or more")

##############################
## COMPARING IMMIGRATION DURATION
##############################

## create blank dataframe to populate with outputs of logistic regression models
output <- data.frame(
  ethnicity = character(0),
  immigration = character(0),
  comparison = character(0),
  outcome = character(0),
  predictor = character(0),
  model = character(0),
  n = numeric(0),
  OR = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

## Create a generic function:
# (1) subset the dataset to relevant groups and outcome variables,
# (2) run 3x logistic regression models, and
# (3) add the results from the models to the output dataframe

logreg_immigration <- function (data, subset, outVar, predVar, compGroup) {
  
  if (subset == "Overall") df <- data
  else
    #subset data to desired ethnicity group
    df <- data %>% dplyr::filter(ethnicity == subset)
  
  # Minimally adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_age")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "minimal",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # BMI-adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_age + bmi")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "bmi",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # Fully adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_age + bmi + income + alcohol + smoking + social + insomnia + fatherpsych + motherpsych")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "full",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  return(output)
}  

## Now run this function with immigration duration as the predictor
## for lifetime depression across all ethnicities
output <- logreg_immigration(data, "Overall", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Arab", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Black", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Other Asian", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "South Asian", "depression", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "White", "depression", "immigrate_duration", "vs Higher Duration")
## for current depression across all ethnicities
output <- logreg_immigration(data, "Overall", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Arab", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Black", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Other Asian", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "South Asian", "currdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "White", "currdep", "immigrate_duration", "vs Higher Duration")
## for undiagnosed depression across all ethnicities
output <- logreg_immigration(data, "Overall", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Arab", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Black", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "Other Asian", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "South Asian", "undiagdep", "immigrate_duration", "vs Higher Duration")
output <- logreg_immigration(data, "White", "undiagdep", "immigrate_duration", "vs Higher Duration")

# correct p values for multiple comparisons
#output$p.fdr <- p.adjust(output$p.value, method = "fdr")

# correct p values for multiple comparisons
output$p.fdr <- p.adjust(output$p.value, method = "fdr")

## Export output dataframe
write.csv(output, "results/logreg_dep_immigrate_duration.csv", row.names = FALSE)



##############################
## COMPARING IMMIGRATION LIFE STAGE
##############################

## create blank dataframe to populate with outputs of logistic regression models
output <- data.frame(
  ethnicity = character(0),
  immigration = character(0),
  comparison = character(0),
  outcome = character(0),
  predictor = character(0),
  model = character(0),
  n = numeric(0),
  OR = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

## Create a generic function:
# (1) subset the dataset to relevant groups and outcome variables,
# (2) run 3x logistic regression models, and
# (3) add the results from the models to the output dataframe

logreg_immigration <- function (data, subset, outVar, predVar, compGroup) {
  
  if (subset == "Overall") df <- data
  else
    #subset data to desired ethnicity group
    df <- data %>% dplyr::filter(ethnicity == subset)
  
  # Minimally adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_duration")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "minimal",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # BMI-adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_duration + bmi")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "bmi",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # Fully adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ", predVar, " + age + sex + immigrate_duration + bmi + income + alcohol + smoking + social + insomnia + fatherpsych + motherpsych")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = rep("Immigrant"),
    predictor = rep(predVar),
    comparison = rep(compGroup),
    outcome = rep(outVar),
    model = "full",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2])),
    ci.min = as.numeric(exp(confint.default(fit1))[2,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2,2]),
    p.value = as.numeric(coef(summary(fit1))[2,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  return(output)
}  

## Now run this function again with immigration stage as the predictor
## for lifetime depression across all ethnicities
output <- logreg_immigration(data, "Overall", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Arab", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Black", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Other Asian", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "South Asian", "depression", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "White", "depression", "immigrate_stage", "vs Adulthood")
## for current depression across all ethnicities
output <- logreg_immigration(data, "Overall", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Arab", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Black", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Other Asian", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "South Asian", "currdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "White", "currdep", "immigrate_stage", "vs Adulthood")
## for undiagnosed depression across all ethnicities
output <- logreg_immigration(data, "Overall", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Arab", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Black", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "Other Asian", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "South Asian", "undiagdep", "immigrate_stage", "vs Adulthood")
output <- logreg_immigration(data, "White", "undiagdep", "immigrate_stage", "vs Adulthood")

# correct p values for multiple comparisons
output$p.fdr <- p.adjust(output$p.value, method = "fdr")

## Export output dataframe
write.csv(output, "results/logreg_dep_immigrate_stage.csv", row.names = FALSE)

rm(logreg_immigration, output)


