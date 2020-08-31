#########################
# Assignment No. 1
# Group 5
#########################
library(tidyverse)

## load insolvency dataset
#setwd("C:/Users/Simone/git/vhb_qear20/raw_data")

insolvency_data <- read.csv("insolvency_filings_de_julaug2020_incomplete.csv", header = TRUE, sep = ",")
str(insolvency_data)
# 6 variables
# 10.035 observations
# unique identifier: court_file_number

# data wrangeling issues:
#  - date: need to be YYYY/MM/DD
#  - insolvency court: might be factor
#  - court_file_numer: ok
#  - subject: need to be factor
#  - name-debtor: 
#  - domicile_debtor: sometimes you have Zip-codes and sometimes not

# first observations:
#  - one court_file_number can have multiple subjects at the same date
#  - there are also duplicates
#  - need to learn more about insolvency process: what does the subjects represent
#  - get map of Germany that show "hot spots" of insolvency
#  - no "Berlin" but "Charlottenburg"
#  - insolvency development over time

table(insolvency_data$date) #data from 01/07/2020 to 18/08/2020
table(insolvency_data$subject) # 10 different factors
table(insolvency_data$insolvency_court)
dplyr::count(insolvency_data, insolvency_court, sort = TRUE) #176 insolvency courts

###Eröffnung
###Sicherheitsmaßnahme
###Abweisungs mangels Masse
###Termine
###Verteilungsverzeichnisse
#Überwachte Insolvenzpläne
###Entscheidung im Verfahren
#Entscheidung nach Aufhebung des Verfahrens
#Entscheidung im Restschulbefreiungsverfahren
#Sonstiges

#Registerzeichen: IN, IK, IE

table(insolvency_data$insolvency_court)
dplyr::count(insolvency_data, insolvency_court, sort = TRUE) #176 insolvency courts

## Data cleaning

insolvency_data_cleaning <- function(data){
  data$date <- as.Date(data$date)
  data$insolvency_court <- as.factor(data$insolvency_court)
  data$subject <- as.factor(data$subject)
  return(data)
}

insolveny_data_clean <- insolvency_data_cleaning(insolvency_data)
str(insolveny_data_clean)

table(insolveny_data_clean$insolvency_court)

# load orbis dataset
orbis_data <- read.csv("orbis_wrds_de.csv", header = TRUE, sep = ",")
str(orbis_data)
# 43 variables
# 7.309.101 observations
# unique identifier: bvdid and year
duplicated(orbis_data$bvdid)
