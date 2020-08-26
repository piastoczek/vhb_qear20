# ------------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("dataMaid")

#set working directory
setwd("F:/empirical_accounting_vhb/vhb_qear20")

# import libraries
library(tidyverse)
library(dataMaid)
library(codebook)

orbis_raw <- read_csv("F:/empirical_accounting_vhb/vhb_qear20/raw_data/orbis_wrds_de.csv")

insolvency_filings <- read_csv("F:/empirical_accounting_vhb/vhb_qear20/raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

makeCodebook(insolvency_filings, TRUE)

codebook(insolvency_filings)




