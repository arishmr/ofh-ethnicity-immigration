## GOAL: create a function to calculate prevalence of an outcome in sub-groups
## then run this to calculate prevalence (with 95% CIs) of depression and diabetes in each of:
## 6 ethnicity sub-groups + 2 immigration status sub-groups

library(epiR)

## create blank dataframe to record outputs
output <- data.frame(
  outcome = character(0),
  ethnicity = character(0),
  immigration = character(0),
  est = numeric(0),
  lower = numeric(0),
  upper = numeric(0),
  Ntotal = numeric(0),
  Ncases = numeric(0),
  stringsAsFactors = TRUE
)


####################
## SEPARATELY FOR ALL SUBGROUPS
####################

data <- alldata
data <- data %>% mutate(undiagdep = case_when(
  depression == FALSE & currdep == TRUE ~ TRUE,
  TRUE ~ FALSE
)) %>%
  relocate(undiagdep, .after = currdep)

## convert outcomes to long form
data <- data %>% tidyr::gather(key = "outcome", value = "value", depression, currdep, undiagdep, diabetes)

## create splitdata with each list element corresponding to one ethnicity/immigration sub-group
data <- data %>% group_by(ethnicity, immigration, outcome) %>% group_split()

## apply a general function to calculate prevalence and produce a dataframe with the results to each element of splitdata

output <- lapply(data, function (x) {
  npop <- as.numeric(sum(!is.na(x$value))) # total number of observations in the group
  ncase <- as.numeric(sum(!is.na(x$value[x$value == TRUE]))) # total number of people with outcome
  tmp <- as.matrix(cbind(ncase, npop))
  prev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100 # epi.conf calculates prevalence
  # add relevant labels to the output to correctly identify the prevalence estimates
  prev$ethnicity <- unique(x$ethnicity)
  prev$immigration <- unique(x$immigration)
  prev$outcome <- unique(x$outcome)
  # add total number of observations to the output (just for reference)
  prev$Ntotal <- npop
  prev$Ncases <- ncase
  # reorder columns
  prev <- prev %>% relocate(outcome, ethnicity, immigration, est, lower, upper, Ntotal, Ncases)
  output <- dplyr::bind_rows(output, prev)
  output
})
output <- purrr::reduce(output, full_join) %>% arrange(outcome)


####################
## FOR ETHNICITY GROUPS ONLY (not split by immigration status)
####################

data <- alldata
data <- alldata
data <- data %>% mutate(undiagdep = case_when(
  depression == FALSE & currdep == TRUE ~ TRUE,
  TRUE ~ FALSE
)) %>%
  relocate(undiagdep, .after = currdep)

## convert outcomes to long form
data <- data %>% tidyr::gather(key = "outcome", value = "value", depression, currdep, undiagdep, diabetes)
## create splitdata with each list element corresponding to one ethnicity/immigration sub-group
data <- data %>% group_by(ethnicity, outcome) %>% group_split()

## apply a general function to calculate prevalence and produce a dataframe with the results to each element of splitdata

output <- lapply(data, function (x) {
  npop <- as.numeric(sum(!is.na(x$value))) # total number of observations in the group
  ncase <- as.numeric(sum(!is.na(x$value[x$value == TRUE]))) # total number of people with outcome
  tmp <- as.matrix(cbind(ncase, npop))
  prev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100 # epi.conf calculates prevalence
  # add relevant labels to the output to correctly identify the prevalence estimates
  prev$ethnicity <- unique(x$ethnicity)
  prev$immigration <- rep("Overall")
  prev$outcome <- unique(x$outcome)
  # add total number of observations to the output (just for reference)
  prev$Ntotal <- npop
  prev$Ncases <- ncase
  # reorder columns
  prev <- prev %>% relocate(outcome, ethnicity, immigration, est, lower, upper, Ntotal, Ncases)
  output <- dplyr::bind_rows(output, prev)
  output
})
output <- purrr::reduce(output, full_join) %>% arrange(outcome)


####################
## FOR IMMIGRATION STATUS ONLY (not split by ethnicity)
####################

data <- alldata
data <- alldata
data <- data %>% mutate(undiagdep = case_when(
  depression == FALSE & currdep == TRUE ~ TRUE,
  TRUE ~ FALSE
)) %>%
  relocate(undiagdep, .after = currdep)

## convert outcomes to long form
data <- data %>% tidyr::gather(key = "outcome", value = "value", depression, currdep, undiagdep, diabetes)
## create splitdata with each list element corresponding to one ethnicity/immigration sub-group
data <- data %>% group_by(immigration, outcome) %>% group_split()

## apply a general function to calculate prevalence and produce a dataframe with the results to each element of splitdata

output <- lapply(data, function (x) {
  npop <- as.numeric(sum(!is.na(x$value))) # total number of observations in the group
  ncase <- as.numeric(sum(!is.na(x$value[x$value == TRUE]))) # total number of people with outcome
  tmp <- as.matrix(cbind(ncase, npop))
  prev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100 # epi.conf calculates prevalence
  # add relevant labels to the output to correctly identify the prevalence estimates
  prev$ethnicity <- rep("Overall")
  prev$immigration <- unique(x$immigration)
  prev$outcome <- unique(x$outcome)
  # add total number of observations to the output (just for reference)
  prev$Ntotal <- npop
  prev$Ncases <- ncase
  # reorder columns
  prev <- prev %>% relocate(outcome, ethnicity, immigration, est, lower, upper, Ntotal, Ncases)
  output <- dplyr::bind_rows(output, prev)
  output
})
output <- purrr::reduce(output, full_join) %>% arrange(outcome)
output


write.csv(output, "results/prevalence.csv", row.names = FALSE)

rm(data, output)