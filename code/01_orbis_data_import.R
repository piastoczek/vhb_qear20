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
b

insolvency_data$date = as.Date(insolvency_data$date, format = "%Y/%m/%d")
w = table(insolvency_data$subject, insolvency_data$date)
t = as.data.frame(w, as.Date(date, format = "%Y/%m/%d"))
names(t)[1] = 'subject'
names(t)[2] = 'date'

d1 <- ggplot(t, aes(x=date, y=Freq, group = subject['Termine'], color = subject)) + geom_line() + 
  geom_point( size=1, shape=21, fill="white") +
  theme(axis.text.x=element_blank()) + 
  labs (x="Date", y=expression(paste("Frequency of subjects")), title="Insolvency filings by subject over time")

d2 <- ggplot(data= t, aes(x=date, y=Freq,group=subject))+ geom_area(position='identity', aes(fill= subject), alpha=0.2)+
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))


  scale_x_date(date_breaks= "1 month", date_minor_breaks = "15 days", date_labels = "%b"+ geom_line(aes(color = subject))


d <- ggplot(t, aes(x=date, y=Freq, group = subject, color = subject)) + geom_line() + 
  geom_point( size=1, shape=21, fill="white") +
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
  labs (x="Date", y=expression(paste("Frequency of subjects")), title="Insolvency filings by subject over time")
d





library(tidyverse)
g <- insolvency_data
g + arrange(subject, date)
group_by(subject)
  



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


#install.packages("summarytools")
library(summarytools)
summarytools::freq(insolvency_data$insolvency_court, order = "freq")
summarytools::freq(insolvency_data$subject, order = "freq")

print(ctable(x = insolvency_data$insolvency_court, y = insolvency_data$subject, prop = "r"),
      method = "render")


