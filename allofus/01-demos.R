#####################################
## GOAL: summarise age, sex, BMI, PHQ-9 score, and % immigrant by - ethnicity and immigration status
#####################################
#install.packages("gtsummary")
#install.packages("flextable")
library(gtsummary)
library(flextable)

## Reduce data to relevant columns for demographics
demodata <- alldata %>% dplyr::select(c(age, sex, bmi, depression, immigration, ethnicity))

## Create gt_summary table by ethnicity

tab1 <- tbl_summary(demodata,
                    by = ethnicity,
                    include = c(
                      age, sex, bmi, depression, immigration
                    ),
                    label = list(
                      age ~ "Age (years)",
                      sex ~ "Sex (Female)",
                      bmi ~ "BMI",
                      depression ~ "Lifetime Depression Diagnosis",
                      immigration ~ "Immigrant"
                    ),
                    type = list(age ~ "continuous",
                                sex ~ "dichotomous",
                                bmi ~ "continuous",
                                depression ~ "dichotomous",
                                immigration ~ "dichotomous"
                    ),
                    value = list(
                      sex ~ "Female",
                      immigration ~ "Immigrant"
                    ),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}—{p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    missing = "ifany",
                    missing_text = "Missing"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "03-results/demographics_ethnicity.docx")

rm(tab1, tab1.print)



## Create gt_summary table by immigration status

tab1 <- tbl_summary(demodata,
                    by = immigration,
                    include = c(
                      age, sex, bmi, depression
                    ),
                    label = list(
                      age ~ "Age (years)",
                      sex ~ "Sex (Female)",
                      bmi ~ "BMI",
                      depression ~ "Lifetime Depression Diagnosis"
                    ),
                    type = list(age ~ "continuous",
                                sex ~ "dichotomous",
                                bmi ~ "continuous",
                                depression ~ "dichotomous"
                    ),
                    value = list(
                      sex ~ "Female"
                    ),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}—{p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    missing = "ifany",
                    missing_text = "Missing"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "03-results/demographics_immigration.docx")

rm(tab1, tab1.print)
rm(demodata)