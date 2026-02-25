## GOAL: within people who self-reported lifetime diagnoses of depression AND within each immigration and ethnicity sub-group, calculate Odds Ratios for self-reported diabetes

library(dplyr)
library(tidyr)

data <- alldata %>% dplyr::filter(depression == TRUE)

##############################
## COMPARING ETHNICITY GROUPS
##############################

## create blank dataframe to populate with outputs of logistic regression models
output <- data.frame(
  ethnicity = character(0),
  immigration = character(0),
  comparison = character(0),
  outcome = character(0),
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

logreg_ethnicity <- function (data, subset, outVar) {
  
  if (subset == "Overall") df <- data
  else
    #subset data to desired immigration status level
    df <- data %>% dplyr::filter(immigration == subset)
  
  # Minimally adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ethnicity + age + sex")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = fit1$xlevels$ethnicity[-1],
    immigration = rep(subset),
    comparison = rep("vs White"),
    outcome = rep(outVar),
    model = "minimal",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2:6])),
    ci.min = as.numeric(exp(confint.default(fit1))[2:6,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2:6,2]),
    p.value = as.numeric(coef(summary(fit1))[2:6,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # BMI-adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ethnicity + age + sex + bmi")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = fit1$xlevels$ethnicity[-1],
    immigration = rep(subset),
    comparison = rep("vs White"),
    outcome = rep(outVar),
    model = "bmi",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2:6])),
    ci.min = as.numeric(exp(confint.default(fit1))[2:6,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2:6,2]),
    p.value = as.numeric(coef(summary(fit1))[2:6,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  
  # Fully adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ ethnicity + age + sex + bmi + income + alcohol + smoking + social + fatherpsych + motherpsych")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = fit1$xlevels$ethnicity[-1],
    immigration = rep(subset),
    comparison = rep("vs White"),
    outcome = rep(outVar),
    model = "full",
    n = rep(nobs(fit1)),
    OR = as.numeric(exp(coef(summary(fit1))[2:6])),
    ci.min = as.numeric(exp(confint.default(fit1))[2:6,1]),
    ci.max = as.numeric(exp(confint.default(fit1))[2:6,2]),
    p.value = as.numeric(coef(summary(fit1))[2:6,4]))
  ## add these to the main outputs dataframe
  output <- bind_rows(output, estimates)
  return(output)
}  

## Now run this function for each subset and outcome
output <- logreg_ethnicity(data, "Overall", "diabetes")
output <- logreg_ethnicity(data, "Non-Immigrant", "diabetes")
output <- logreg_ethnicity(data, "Immigrant", "diabetes")

# correct p values for multiple comparisons
output$p.fdr <- p.adjust(output$p.value, method = "fdr")

## Export output dataframe
write.csv(output, "results/logreg_diab_ethnicity.csv", row.names = FALSE)
rm(logreg_ethnicity, output)






##############################
## COMPARING IMMIGRATION STATUS GROUPS
##############################

## create blank dataframe to populate with outputs of logistic regression models
output <- data.frame(
  ethnicity = character(0),
  immigration = character(0),
  comparison = character(0),
  outcome = character(0),
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

logreg_immigration <- function (data, subset, outVar) {
  
  if (subset == "Overall") df <- data
  else
    #subset data to desired immigration status level
    df <- data %>% dplyr::filter(ethnicity == subset)
  
  # Minimally adjusted model
  fit1 <- glm(as.formula(paste(outVar, "~ immigration + age + sex")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = fit1$xlevels$immigration[-1],
    comparison = rep("vs Non-Immigrant"),
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
  fit1 <- glm(as.formula(paste(outVar, "~ immigration + age + sex + bmi")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = fit1$xlevels$immigration[-1],
    comparison = rep("vs Non-Immigrant"),
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
  fit1 <- glm(as.formula(paste(outVar, "~ immigration + age + sex + bmi + income + alcohol + smoking + social + fatherpsych + motherpsych")),
              family = binomial(link="logit"),
              data = df)
  ## extract relevant estimates from the model
  estimates <- data.frame(
    ethnicity = rep(subset),
    immigration = fit1$xlevels$immigration[-1],
    comparison = rep("vs Non-Immigrant"),
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

## Now run this function for each subset and outcome
output <- logreg_immigration(data, "Overall", "diabetes")
output <- logreg_immigration(data, "Arab", "diabetes")
output <- logreg_immigration(data, "Black", "diabetes")
output <- logreg_immigration(data, "Mixed or Multiple Heritage", "diabetes")
output <- logreg_immigration(data, "Other Asian", "diabetes")
output <- logreg_immigration(data, "South Asian", "diabetes")
output <- logreg_immigration(data, "White", "diabetes")

# correct p values for multiple comparisons
output$p.fdr <- p.adjust(output$p.value, method = "fdr")

## Export output dataframe
write.csv(output, "results/logreg_diab_immigration.csv", row.names = FALSE)
rm(logreg_immigration, output)
