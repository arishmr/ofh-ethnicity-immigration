## Reduce data to relevant columns for demographics
demodata <- alldata %>% dplyr::select(-c(pid, phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, phq9))

##########################
## Calculate median and IQR for continuous variables
##########################

## Create a longer-form dataframe with continuous variables in one column (age, bmi, PHQ, immigrate_duration)
contdemos <- demodata %>%
  tidyr::gather(key = "variable",
                value = "value",
                age, bmi, PHQ, immigrate_duration) %>%
  tidyr::drop_na(value)

## Split dataset by demographic variables
splitdata <- contdemos %>%
  group_by(variable) %>%
  group_split()

## Create a blank dataframe in which the outputs will be populated

output <- data.frame(
  variable = character(0),
  group = character(0),
  N = numeric(0),
  Median = numeric(0),
  Q25 = numeric(0),
  Q75 = numeric(0),
  stringsAsFactors = TRUE
)
anova <- data.frame(
  variable = character(0),
  predictor = character(0),
  Df = numeric(0),
  Fstat = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = TRUE
)

## Now apply the summarise function over each element of the splitdata
output <- lapply(splitdata, function (x) {
  # grouped by ethnicity
  y <-  x %>%
    group_by(group = ethnicity) %>% 
    summarise(
      N = sum(!is.na(value)),  
      Median = median(value, na.rm = T), 
      Q25 = quantile(value, 0.25, na.rm = T),
      Q75 = quantile(value, 0.75, na.rm = T)
    )
  y <- y %>% mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  y <- dplyr::bind_rows(output, y)
  y
  # grouped by immigration status
  z <-  x %>%
    group_by(group = immigration) %>% 
    summarise(
      N = sum(!is.na(value)),  
      Median = median(value, na.rm = T), 
      Q25 = quantile(value, 0.25, na.rm = T),
      Q75 = quantile(value, 0.75, na.rm = T)
    )
  z <- z %>% mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  z <- dplyr::bind_rows(y, z)
  z
  # overall
  a <-  x %>%
    summarise(
      N = sum(!is.na(value)),  
      Median = median(value, na.rm = T), 
      Q25 = quantile(value, 0.25, na.rm = T),
      Q75 = quantile(value, 0.75, na.rm = T)
    )
  a <- a %>% mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  a <- a %>% mutate(group = rep("Overall")) %>% relocate(.after = variable)
  a <- dplyr::bind_rows(z, a)
  a
})
output <- purrr::reduce(output, full_join)

splitdata[[4]] <- NULL # delete element of list that involves immigrate_duration

## Now apply the anova function over each element of the splitdata
anova <- lapply(splitdata, function (x) {
  y <- summary(aov(value ~ ethnicity, data = x))
  Df <- y[[1]][1,1]
  Fstat <- y[[1]][1,4]
  p.value <- y[[1]][1,5]
  y <- data.frame(variable = unique(x$variable), predictor = "ethnicity", Df, Fstat, p.value)
  y <- dplyr::bind_rows(anova, y)
  y
  z <- summary(aov(value ~ immigration, data = x))
  Df <- z[[1]][1,1]
  Fstat <- z[[1]][1,4]
  p.value <- z[[1]][1,5]
  z <- data.frame(variable = unique(x$variable), predictor = "immigration", Df, Fstat, p.value)
  z <- dplyr::bind_rows(z, y)
  z
})
anova <- purrr::reduce(anova, full_join)

write.csv(output, "results/contdemos.csv", row.names = FALSE)
write.csv(anova, "results/contanova.csv", row.names = FALSE)
rm(contdemos, output, splitdata, anova)





##########################
## Calculate proportions for categorical variables
##########################

## Create a longer-form dataframe with categorical variables in one column (sex, income, alcohol, smoking, insomnia, fatherpsych, motherpsych, immigrate_stage)
catdemos <- demodata %>%
  tidyr::gather(key = "variable",
                value = "value",
                sex, income, alcohol, smoking, fatherpsych, motherpsych, immigrate_stage) %>%
  tidyr::drop_na(value)

## Split dataset by demographic variables
splitdata <- catdemos %>%
  group_by(variable) %>%
  group_split()

## ensure that values are classed as factors
splitdata <- lapply(splitdata, function (x) {
  x$value <- as.factor(x$value)
  x
})

## Create a blank dataframe in which the outputs will be populated

output <- data.frame(
  variable = character(0),
  group = character(0),
  value = character(0),
  N = numeric(0),
  Freq = numeric(0),
  stringsAsFactors = TRUE
)
chisq <- data.frame(
  variable = character(0),
  predictor = character(0),
  Df = numeric(0),
  chi = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = TRUE
)

## Now apply the summarise function over each element of the splitdata

output <- lapply(splitdata, function (x) {
  # grouped by ethnicity
  y <- x %>% group_by(group = ethnicity, value) %>%
    summarise(N = n()) %>%
    mutate(Freq = N / sum(N)*100) %>%
    mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  y <- dplyr::bind_rows(output, y)
  y
  # grouped by immigration status
  z <- x %>% group_by(group = immigration, value) %>%
    summarise(N = n()) %>%
    mutate(Freq = N / sum(N)*100) %>%
    mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  z <- dplyr::bind_rows(y, z)
  z
  # overall
  a <-  x %>% group_by(value) %>%
    summarise(N = n()) %>%
    mutate(Freq = N / sum(N)*100) %>%
    mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  a <- a %>% mutate(variable = rep(unique(x$variable))) %>% relocate(variable)
  a <- a %>% mutate(group = rep("Overall")) %>% relocate(.after = variable)
  a <- dplyr::bind_rows(z, a)
  a
})
output <- purrr::reduce(output, full_join)

splitdata[[3]] <- NULL # delete element of list that involves immigrate_stage

## Now apply the chisq function over each element of the splitdata
chisq <- lapply(splitdata, function (x) {
  y <- chisq.test(x$value, x$ethnicity)
  Df <- as.numeric(y[[2]])
  chi <- as.numeric(y[[1]])
  p.value <- y[[3]]
  y <- data.frame(variable = unique(x$variable), predictor = "ethnicity", Df, chi, p.value)
  y <- dplyr::bind_rows(chisq, y)
  y
  z <- chisq.test(x$value, x$immigration)
  Df <- as.numeric(z[[2]])
  chi <- as.numeric(z[[1]])
  p.value <- z[[3]]
  z <- data.frame(variable = unique(x$variable), predictor = "immigration", Df, chi, p.value)
  z <- dplyr::bind_rows(y, z)
  z
})
chisq <- purrr::reduce(chisq, full_join)

write.csv(output, "results/catdemos.csv", row.names = F)
write.csv(chisq, "results/catchisq.csv", row.names = FALSE)
rm(catdemos, output, splitdata, chisq)
