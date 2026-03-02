#######################################
## BASH STARTUP FOR ANALYSIS
#######################################

mkdir data
mkdir results

dx download "03-cleandata/cleandata.csv" -o "data/cleandata.csv"



#######################################
## BASH ENDING FOR ANALYSIS
#######################################

dx upload results/* --path /04-results/ --brief