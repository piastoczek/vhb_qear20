# ------------------------------------------------------------------------------
# VHB Course - Group 5
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Issue 2 - Solutions
# ------------------------------------------------------------------------------

# Set working directories

# Pia 
# setwd("F:/empirical_accounting_vhb/vhb_qear20")

# Simone
# setwd("C:/Users/Simone/git/vhb_qear20")

# Olga
#
 
# Tim
#

#Load Packages
library(tidyverse)


# Load datasets
orbis_data <- read_csv("raw_data/orbis_wrds_de.csv")
insolvency_data <- read_csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

#Overview
table(insolvency_data$date) #data from 01/07/2020 to 18/08/2020
table(insolvency_data$subject) # 10 different factors
table(insolvency_data$insolvency_court)
dplyr::count(insolvency_data, insolvency_court, sort = TRUE) #176 insolvency courts

#Frequency tables
#install.packages("summarytools")
library(summarytools)
summarytools::freq(insolvency_data$insolvency_court, order = "freq")
summarytools::freq(insolvency_data$subject, order = "freq")

#Basic bar charts
a <- ggplot(data = insolvency_data) + geom_bar(mapping = aes(x = subject, fill = subject)) 
a + theme(axis.text.x = element_blank())







