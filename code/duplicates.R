# read data
insolvency_data <- read.csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv", 
                            header = TRUE, 
                            sep = ",")
library(tidyverse)
# check for duplicates
duplicated(insolvency_data) %>% sum() # sum is 680. As it is a sum of logical factor (FALSE = 0, TRUE = 1). It means we have 680 duplicated rows in our data 

#display duplicates
duplicated <- insolvency_data[duplicated(insolvency_data),]
view(duplicated)

# save data as two sets to be sure we have all we need
insolvency_data_raw <- insolvency_data # it will contain raw data with duplicates (10035)
insolvency_data <- distinct(insolvency_data) # it will contain only distinct rows (9355 = 10035 - 680, that's ok). We need these clean data to be named "insolvency_data", because our previous code for plots and map uses this name.

# check once again
insolvency_data_raw %>% group_by(date, insolvency_court, court_file_number, subject, name_debtor, domicile_debtor) %>% count() # when we group by all columns, we get 9355 different groups
