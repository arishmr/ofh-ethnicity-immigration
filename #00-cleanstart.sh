#######################################
## BASH STARTUP FOR DATA CLEANING
#######################################

mkdir data
mkdir cleandata

# download raw data files from the platform storage
dx download "02-data/data_participant.csv" -o "data/participant.csv"
dx download "02-data/data_questionnaire.csv" -o "data/questionnaire.csv"
dx download "02-data/data_clinic_measurements.csv" -o "data/clinic.csv"



#######################################
## BASH ENDING FOR DATA CLEANING
#######################################

dx upload cleandata/* --path /03-cleandata/ --brief