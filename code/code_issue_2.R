# ------------------------------------------------------------------------------
# VHB Course - Group 5
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Issue 2 - Solutions
# ------------------------------------------------------------------------------

# Set working directory
# Enter your path in the first line and your windows user name in line 2 (replace your name) then the path will be set automatically based on the user
Paths = c("F:/empirical_accounting_vhb/vhb_qear20", "C:/Users/Simone/Documents/GitHub/vhb_qear20", "C://Tim/other/path", "C://Olga/other/path")
names(Paths) = c("pstoczek", "Simone", "Tim", "Olga")
setwd(Paths[Sys.info()[7]])

# Packages if not installed
#install.packages("summarytools")

#Load Packages
library(tidyverse)
library(ggplot2)
library(summarytools)
library(rmarkdown)

# Load datasets
orbis_data <- read_csv("raw_data/orbis_wrds_de.csv")
insolvency_data <- read_csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

#Overview
table(insolvency_data$date) #data from 01/07/2020 to 18/08/2020
table(insolvency_data$subject) # 10 different factors
table(insolvency_data$insolvency_court)
dplyr::count(insolvency_data, insolvency_court, sort = TRUE) #176 insolvency courts

#Frequency tables (no option to create pdf with summarytools)
summary_table_1 <- summarytools::freq(insolvency_data$insolvency_court, order = "freq")
view(summary_table_1, file = "~/summary_table_1.md")
summary_table_2 <- summarytools::freq(insolvency_data$subject, order = "freq")
view(summary_table_2, file = "~/summary_table_2.rmd")

#Basic bar charts
# Bar chart 1
pdf("output/insolvency_subject_barchart1.pdf")
insolvency_subject_bar1 <- ggplot(data = insolvency_data) + geom_bar(mapping = aes(x = subject, fill = subject)) 
insolvency_subject_bar1 + theme(axis.text.x = element_blank())
print(insolvency_subject_bar1)
dev.off()

# Bar chart 2
pdf("output/insolvency_subject_barchart2.pdf")
insolvency_subject_bar2 <- ggplot(insolvency_data) + geom_bar(mapping = aes(x = subject)) + coord_flip()
insolvency_subject_bar2 + theme(axis.text.x = element_blank())
print(insolvency_subject_bar2)
dev.off()

# Bar chart 3
pdf("output/insolvency_subject_barchart3.pdf")
insolvency_subject_bar3 <- insolvency_data %>% group_by(subject) %>% count() %>% arrange(desc(n))
ggplot(data = insolvency_subject_bar3) + 
  geom_col(mapping = aes(x = n, y = reorder(subject, n))) + 
  labs(title = "Insolvency by subject", x = "No. of cases", y = "Subject")
print(insolvency_subject_bar3)
dev.off()



