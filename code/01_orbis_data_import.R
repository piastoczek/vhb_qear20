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

# import libraries
library(tidyverse)
library(ggplot2)
library(maps)
#library(dataMaid)
#library(codebook)

orbis_data <- read_csv("raw_data/orbis_wrds_de.csv")

insolvency_data <- read_csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv")

#makeCodebook(insolvency_filings, TRUE)
#codebook(insolvency_filings)

#Check variable format
for(i in orbis_raw){
  print(typeof(i))     #prints 'character'
}
for(i in insolvency_filings){
  print(typeof(i))     #prints 'character'
}

table(insolvency_data$date) #data from 01/07/2020 to 18/08/2020
table(insolvency_data$subject) # 10 different factors
table(insolvency_data$insolvency_court)
dplyr::count(insolvency_data, insolvency_court, sort = TRUE) #176 insolvency courts



#Basic bar charts
a <- ggplot(data = insolvency_data) + geom_bar(mapping = aes(x = subject, fill = subject)) 
a + theme(axis.text.x = element_blank())

# or with export to the "output" folder in pdf format
pdf("output/insolvency_subject_barchart.pdf")
ggplot(insolvency_data) + geom_bar(mapping = aes(x = subject)) + coord_flip()
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

b <- ggplot(data=insolvency_datam, aes(x=subject, fill= subject)) + geom_bar(col=289)+ facet_grid(~Month)
b + labs(title="Status by Period", x = "Status" , y= "Number", fill="Status")+ theme(axis.text.x = element_blank()) -> g
b + theme(plot.title = element_text(hjust = 0.5, face ="bold", colour = "black"))

# I've got problems with the code from this point. It doesn't work at my PC. Tim & Simone - how about you? 
ggplot(data = insolvency_data, mapping = aes(x = date))+
  geom_freqpoly(mapping = aes(color = subject), binwidth = 500)

c <- ggplot(data = insolvency_filings, aes(x=factor(date),y=subject, group=subject))+
  stat_summary(aes(color=subject), fun.y = length, geom = "line")+
  scale_color_discrete("subject")+
  labs(x="",y="Frequency")
c + theme(axis.text.y = element_blank())


d <- ggplot(insolvency_data, aes(x=date, y=subject)) +
  geom_line() + 
  xlab("")
d
d+scale_x_date(date_labels = "%b")
d+scale_x_date(date_labels = "%Y %b %d")
d+scale_x_date(date_labels = "%W")
d+scale_x_date(date_labels = "%m-%Y")

attach(insolvency_data)
table_a <- table(insolvency_court, subject)
ftable(table_a)
prop.table(table_a)

#install.packages("summarytools")
library(summarytools)
summarytools::freq(insolvency_data$insolvency_court, order = "freq")
summarytools::freq(insolvency_data$subject, order = "freq")

print(ctable(x = insolvency_data$insolvency_court, y = insolvency_data$subject, prop = "r"),
      method = "render")


