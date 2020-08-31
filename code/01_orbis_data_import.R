# ------------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("dataMaid")
#install.packages("maps")
#install.packages("GADMTools")
#install.packages("summarytools")

#set working directory
setwd("F:/empirical_accounting_vhb/vhb_qear20")

Paths = c("F:/empirical_accounting_vhb/vhb_qear20", "C:/Users/Simone/git/vhb_qear20", "C://Some/other/path", "C://Some/other/path")
names(Paths) = c("pstoczek", "Simone", "Tim", "Olga")
setwd(Paths[Sys.info()[7]])






# import libraries
library(tidyverse)
library(ggplot2)
library(maps)
#library(dataMaid)
#library(codebook)

orbis_data <- read_csv("raw_data/orbis_wrds_de.csv")

insolvency_data <- read_csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

#Check variable format
for(i in orbis_data){
  print(typeof(i))     #prints 'character'
}
for(i in insolvency_data){
  print(typeof(i))     #prints 'character'
}

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
pdf("output/insolvency_subject_barchart2.pdf")
insolvency_subject_bar <- ggplot(insolvency_data) + geom_bar(mapping = aes(x = subject)) + coord_flip()
insolvency_subject_bar + theme(axis.text.x = element_blank())
print(insolvency_subject_bar)
dev.off()

# or with export to the "output" folder in pdf format
pdf("output/insolvency_subject_barchart.pdf")
insolvency_by_subject <- insolvency_data %>% group_by(subject) %>% count() %>% arrange(desc(n))
ggplot(data = insolvency_by_subject) + 
  geom_col(mapping = aes(x = n, y = reorder(subject, n))) + 
  labs(title = "Insolvency by subject", x = "No. of cases", y = "Subject")
dev.off()


#Basic bar chart seperated by periods (Tim)

Jul.1 <- filter(select(insolvency_data, date:subject), date >= "2020-07-01" & date < "2020-07-15")
Jul.1$Month <- "(A) July, First Half " 
Jul.2 <- filter(select(insolvency_data, date:subject), date >= "2020-07-15" & date < "2020-08-01")
Jul.2$Month <- "(B) July, Second Half" 
Aug.1 <- filter(select(insolvency_data, date:subject), date >= "2020-08-01" & date < "2020-08-15")
Aug.1$Month <- "(C) August, First Half" 
Aug.2 <- filter(select(insolvency_data, date:subject), date >= "2020-08-15" & date < "2020-09-01")
Aug.2$Month <- "(D) August, Second Half" 
insolvency_datam <- rbind(Jul.1,Jul.2,Aug.1,Aug.2)

insolvency_data$date = as.Date(insolvency_data$date, format = "%Y/%m/%d")
w = table(insolvency_data$subject, insolvency_data$date)
t = as.data.frame(w)
names(t)[1] = 'subject'
names(t)[2] = 'date'




