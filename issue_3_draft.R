library(tidyverse)
library(knitr)
library(kableExtra)

# only to build a data for trial coding (data_draft, we need to change it in the code later)


insolvency_dummy <- if_else(data_draft$X1 < 30000, 1, 0)

data_draft$insolvency = insolvency_dummy

so I have a dataset called "data_draft" with a dummy variable for insolvency (1 for insolvenct firms). Year - only 2018.

# select useful variables

data_draft_use <- data_draft %>%
  select(X1, major_sector, listed, accpractice, audstatus, cash, toas, shfd, ncli, culi, empl, opre, turn, exex, pl, insolvency)

# where are NA values?
na_vals <- data_draft_use %>%
  summarise_all(list( ~ sum(is.na(.)))) 

# there's a lot of missing values, particularly for unlisted firms. Count how many insolvent firm are listed or not.
data_draft_use %>%
  group_by(listed, insolvency) %>% 
  summarise(N = n()) %>% 
  kable() %>%
  kable_styling(full_width = FALSE)  
# Acctually it would be better to have two-dimensional table (contingency table), but I don't know how to do this. And maybe frequency?
# Please change the code if you have any idea

# the same we can do for listed and unlisted
data_draft_use %>%
  group_by(accpractice, insolvency) %>% 
  summarise(N = n()) %>% 
  kable() %>%
  kable_styling(full_width = FALSE)


# NA in "exex" (extraordinary and other expenses) are probably just 0. So let's assign 0 to exex NA values:

data_draft_use1 <- data_draft_use %>% mutate(exex = {if_else(is.na(data_draft_use$exex), 0,data_draft_use$exex)})
....

# add new variables which might differ between insolvent firms and others

data_draft_v <- data_draft_use1 %>%
  mutate(ROA = (pl+exex) / toas)

# make some nice plots

# add a nice table

# and fall asleep