######################################################################
## Title:Uptake of infant and pre-school immunisations in Scotland and England during the COVID-19 pandemic: an observational study of routinely collected data
## Short title: 
## DOI: Plos Medicine, accepted 18 Jan 2022
## Code author: Fiona McQuaid, Rachel Mulholland
## Description: Full analysis code. Data extracted from dashboard 3rd Feb 2021
######################################################################

##Figure 1
#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(plotrix)
library(ggpubr)
library(rstatix)

#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))

#Selecting out Scotland wide data and remove unwanted rows (eg summmary months and October, November)
Scotland_firstdose_6in1 = Full_firstdose_6in1 %>% 
  filter(area_name == "Scotland") %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Scotland_seconddose_6in1 = Full_seconddose_6in1%>% 
  filter(area_name == "Scotland")%>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Scotland_thirddose_6in1 = Full_thirddose_6in1%>% 
  filter(area_name == "Scotland")%>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Scotland_firstdose_MMR = Full_firstdose_MMR%>% 
  filter(area_name == "Scotland")%>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Scotland_seconddose_MMR = Full_seconddose_MMR%>% 
  filter(area_name == "Scotland")%>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))


#Add lockdown.factor and time.factor columns
Scotland_firstdose_6in1 = Scotland_firstdose_6in1 %>%
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  mutate(time.factor = 
           cohort %>% 
           factor())

Scotland_seconddose_6in1 = Scotland_seconddose_6in1 %>% 
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  mutate(time.factor = 
           cohort %>% 
           factor())

Scotland_thirddose_6in1 = Scotland_thirddose_6in1 %>% 
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  mutate(time.factor = 
           cohort %>% 
           factor())

Scotland_firstdose_MMR = Scotland_firstdose_MMR %>% 
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  mutate(time.factor = 
           cohort %>% 
           factor())

Scotland_seconddose_MMR = Scotland_seconddose_MMR %>% 
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  mutate(time.factor = 
           cohort %>% 
           factor())

#Relevel time.factor to be non alphabetical
Scotland_firstdose_6in1$time.factor = factor(Scotland_firstdose_6in1$time.factor, levels = Scotland_firstdose_6in1$time.factor)
Scotland_seconddose_6in1$time.factor = factor(Scotland_seconddose_6in1$time.factor, levels = Scotland_seconddose_6in1$time.factor)
Scotland_thirddose_6in1$time.factor = factor(Scotland_thirddose_6in1$time.factor, levels = Scotland_thirddose_6in1$time.factor)
Scotland_firstdose_MMR$time.factor = factor(Scotland_firstdose_MMR$time.factor, levels = Scotland_firstdose_MMR$time.factor)
Scotland_seconddose_MMR$time.factor = factor(Scotland_seconddose_MMR$time.factor, levels = Scotland_seconddose_MMR$time.factor)


##Combine data by LD period
#New tbl of data by LD, add unvaccinated column, summarise by LD period and turn vaccinated and unvaccianted into factors
First_6in1_by_LDperiod = Scotland_firstdose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_12weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor()) 


Second_6in1_by_LDperiod = Scotland_seconddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_16weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_16weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

Third_6in1_by_LDperiod = Scotland_thirddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_20weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_20weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

First_MMR_by_LDperiod = Scotland_firstdose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_13m_num, uptake_13m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_13m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_13m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_13m_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())


Second_MMR_by_LDperiod = Scotland_seconddose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_3y5m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_3y5m_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

##Logistic regression
#Set up required tbls, weekly data by LD period, add unvaccinated column, change vaccinated and unvaccinated into factors and do a boxplot if you want
#First dose 6in1
Weekly_first6in1_LDperiod = Scotland_firstdose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

#Second dose 6in1
Weekly_second6in1_LDperiod = Scotland_seconddose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

#Third dose 6in1
Weekly_third6in1_LDperiod = Scotland_thirddose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())


#First dose MMR
Weekly_firstMMR_LDperiod = Scotland_firstdose_MMR %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_13m_num, uptake_13m_percent)%>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

#Second dose MMR
Weekly_secondMMR_LDperiod = Scotland_seconddose_MMR %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent)%>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

##Set baseline level as 2019

Weekly_first6in1_LDperiod = Weekly_first6in1_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Weekly_second6in1_LDperiod = Weekly_second6in1_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Weekly_third6in1_LDperiod = Weekly_third6in1_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Weekly_firstMMR_LDperiod = Weekly_firstMMR_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Weekly_secondMMR_LDperiod = Weekly_secondMMR_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

##Set up the models for each vaccine
#First 6in1
First6in1_Single_regression_tbl = cbind(Weekly_first6in1_LDperiod$uptake_12weeks_num, Weekly_first6in1_LDperiod$unvaccinated)

model_first6in1_scotland_2019 = glm(First6in1_Single_regression_tbl ~ Weekly_first6in1_LDperiod$lockdown.factor,
                                    family="binomial")

summary(model_first6in1_scotland_2019)
confint(model_first6in1_scotland_2019)

exp(model_first6in1_scotland_2019$coefficients)
exp(confint(model_first6in1_scotland_2019))  

#Second 6in1
Second6in1_Single_regression_tbl = cbind(Weekly_second6in1_LDperiod$uptake_16weeks_num, Weekly_second6in1_LDperiod$unvaccinated)

model_second6in1_scotland_2019 = glm(Second6in1_Single_regression_tbl ~ Weekly_second6in1_LDperiod$lockdown.factor,
                                     family="binomial")

summary(model_second6in1_scotland_2019)

exp(model_second6in1_scotland_2019$coefficients)
exp(confint(model_second6in1_scotland_2019)) 

#Third 6in1
Third6in1_Single_regression_tbl = cbind(Weekly_third6in1_LDperiod$uptake_20weeks_num, Weekly_third6in1_LDperiod$unvaccinated)

model_third6in1_scotland_2019 = glm(Third6in1_Single_regression_tbl ~ Weekly_third6in1_LDperiod$lockdown.factor,
                                    family="binomial")

summary(model_third6in1_scotland_2019)

exp(model_third6in1_scotland_2019$coefficients)
exp(confint(model_third6in1_scotland_2019))

#First MMR
FirstMMR_Single_regression_tbl = cbind(Weekly_firstMMR_LDperiod$uptake_13m_num, Weekly_firstMMR_LDperiod$unvaccinated)

model_firstMMR_scotland_2019 = glm(FirstMMR_Single_regression_tbl ~ Weekly_firstMMR_LDperiod$lockdown.factor,
                                   family="binomial")

summary(model_firstMMR_scotland_2019)

exp(model_firstMMR_scotland_2019$coefficients)
exp(confint(model_firstMMR_scotland_2019))

#Second MMR
SecondMMR_Single_regression_tbl = cbind(Weekly_secondMMR_LDperiod$uptake_3y5m_num, Weekly_secondMMR_LDperiod$unvaccinated)

model_secondMMR_scotland_2019 = glm(SecondMMR_Single_regression_tbl ~ Weekly_secondMMR_LDperiod$lockdown.factor,
                                    family="binomial")

summary(model_secondMMR_scotland_2019)

exp(model_secondMMR_scotland_2019$coefficients)
exp(confint(model_secondMMR_scotland_2019))

#Make the OR and CI tbls, remove the intercept
library(broom)
First6in1_model_tbl = model_first6in1_scotland_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE)) %>% 
  mutate(lockdown.factor = term) 


Second6in1_model_tbl = model_second6in1_scotland_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

Third6in1_model_tbl = model_third6in1_scotland_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

FirstMMR_model_tbl = model_firstMMR_scotland_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

SecondMMR_model_tbl = model_secondMMR_scotland_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

##First 6in1

model_first6in1_scotland_2019

Section1tbl_First_6in1_by_LDperiod = First_6in1_by_LDperiod%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
Section1tbl_First_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
Section1tbl_First_6in1_by_LDperiod = Section1tbl_First_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="First6in1") %>% 
  mutate("Absolute % change from 2019" = mean_percent-94)
Section1tbl_First_6in1_by_LDperiod = Section1tbl_First_6in1_by_LDperiod[,c(3,1,2,4)]

First6in1_model_tbl$time_period <- c("Pre LD", "LD", "Post LD") 
First6in1_model_tbl = First6in1_model_tbl %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>%  
  mutate (p.value= round (p.value, digits = 2))

Section1tbl_First_6in1_by_LDperiod = full_join(Section1tbl_First_6in1_by_LDperiod, First6in1_model_tbl)

colnames(Section1tbl_First_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(Section1tbl_First_6in1_by_LDperiod, file = "Exported tables/Section1tbl_First6in1_byLDperiod.csv")

##Second 6in1

Section1tbl_Second_6in1_by_LDperiod = Second_6in1_by_LDperiod%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
Section1tbl_Second_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
Section1tbl_Second_6in1_by_LDperiod = Section1tbl_Second_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="Second6in1") %>% 
  mutate("Absolute % change from 2019" = mean_percent-84.8)
Section1tbl_Second_6in1_by_LDperiod = Section1tbl_Second_6in1_by_LDperiod[,c(3,1,2,4)]


Second6in1_model_tbl$time_period <- c("Pre LD", "LD", "Post LD") 
Second6in1_model_tbl = Second6in1_model_tbl %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>%  
  mutate (p.value= round (p.value, digits = 2))

Section1tbl_Second_6in1_by_LDperiod = full_join(Section1tbl_Second_6in1_by_LDperiod, Second6in1_model_tbl)

colnames(Section1tbl_Second_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

Section1_tbl1_allvaccines = rbind(Section1tbl_First_6in1_by_LDperiod, Section1tbl_Second_6in1_by_LDperiod)

##Third 6in1

Section1tbl_Third_6in1_by_LDperiod = Third_6in1_by_LDperiod%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
Section1tbl_Third_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
Section1tbl_Third_6in1_by_LDperiod = Section1tbl_Third_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="Third6in1") %>% 
  mutate("Absolute % change from 2019" = mean_percent-73)
Section1tbl_Third_6in1_by_LDperiod = Section1tbl_Third_6in1_by_LDperiod[,c(3,1,2,4)]


Third6in1_model_tbl$time_period <- c("Pre LD", "LD", "Post LD") 
Third6in1_model_tbl = Third6in1_model_tbl %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>%  
  mutate (p.value= round (p.value, digits = 2))

Section1tbl_Third_6in1_by_LDperiod = full_join(Section1tbl_Third_6in1_by_LDperiod, Third6in1_model_tbl)

colnames(Section1tbl_Third_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

##First MMR

Section1tbl_FirstMMR_by_LDperiod = First_MMR_by_LDperiod%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
Section1tbl_FirstMMR_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
Section1tbl_FirstMMR_by_LDperiod = Section1tbl_FirstMMR_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="FirstMMR") %>% 
  mutate("Absolute % change from 2019" = mean_percent-65.2)
Section1tbl_FirstMMR_by_LDperiod = Section1tbl_FirstMMR_by_LDperiod[,c(3,1,2,4)]


FirstMMR_model_tbl$time_period <- c("Pre LD", "LD", "Post LD") 
FirstMMR_model_tbl = FirstMMR_model_tbl %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>%  
  mutate (p.value= round (p.value, digits = 2))

Section1tbl_FirstMMR_by_LDperiod = full_join(Section1tbl_FirstMMR_by_LDperiod, FirstMMR_model_tbl)

colnames(Section1tbl_FirstMMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

##Second MMR

Section1tbl_SecondMMR_by_LDperiod = Second_MMR_by_LDperiod%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
Section1tbl_SecondMMR_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
Section1tbl_SecondMMR_by_LDperiod = Section1tbl_SecondMMR_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="SecondMMR") %>% 
  mutate("Absolute % change from 2019" = mean_percent-51.8)
Section1tbl_SecondMMR_by_LDperiod = Section1tbl_SecondMMR_by_LDperiod[,c(3,1,2,4)]


SecondMMR_model_tbl$time_period <- c("Pre LD", "LD", "Post LD") 
SecondMMR_model_tbl = SecondMMR_model_tbl %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>%  
  mutate (p.value= round (p.value, digits = 2))

Section1tbl_SecondMMR_by_LDperiod = full_join(Section1tbl_SecondMMR_by_LDperiod, SecondMMR_model_tbl)

colnames(Section1tbl_SecondMMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

#Merge all tables together and export as csv
Section1_tbl1_allvaccines = rbind(Section1tbl_First_6in1_by_LDperiod, Section1tbl_Second_6in1_by_LDperiod, Section1tbl_Third_6in1_by_LDperiod, Section1tbl_FirstMMR_by_LDperiod, Section1tbl_SecondMMR_by_LDperiod)

write_csv(Section1_tbl1_allvaccines, file = "Exported tables/Section1_tbl1_allvaccines.csv")

###Section 1, figure 1, line plot of weekly rates

#First dose 6in1, first remove 2019
Weekly_first_6in1_remove_2019 = Scotland_firstdose_6in1 %>% 
  filter(!(cohort=="2019")) %>% 
  select(immunisation, uptake_12weeks_percent,time.factor)
colnames(Weekly_first_6in1_remove_2019) = c("immunisation", "percent uptake", "time.factor")
#set levels
Weekly_first_6in1_remove_2019$time.factor = factor(Weekly_first_6in1_remove_2019$time.factor, levels = c('Jan-20', 'Feb-20', 'W/B 02-MAR-20', 'W/B 09-MAR-20', 'W/B 16-MAR-20', 'W/B 23-MAR-20','W/B 30-MAR-20','W/B 06-APR-20','W/B 13-APR-20','W/B 20-APR-20','W/B 27-APR-20','W/B 04-MAY-20','W/B 11-MAY-20','W/B 18-MAY-20','W/B 25-MAY-20','W/B 01-JUN-20','W/B 08-JUN-20','W/B 15-JUN-20','W/B 22-JUN-20','W/B 29-JUN-20','W/B 06-JUL-20','W/B 13-JUL-20','W/B 20-JUL-20','W/B 27-JUL-20', 'W/B 03-AUG-20','W/B 10-AUG-20','W/B 17-AUG-20','W/B 24-AUG-20','W/B 31-AUG-20','W/B 07-SEP-20','W/B 14-SEP-20','W/B 21-SEP-20','W/B 28-SEP-20'))
#Plot
Weekly_first_6in1_line = Weekly_first_6in1_remove_2019 %>% 
  ggplot(aes(x=time.factor, y=`percent uptake`, group=immunisation, color=immunisation)) +
  geom_line() +
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1")+
  expand_limits(y=80)+
  scale_y_continuous(breaks = c(80:100))

Weekly_first_6in1_line
#Add 2019 line
Weekly_first_6in1_line = Weekly_first_6in1_line + geom_hline(yintercept = 94, linetype="dashed", 
                                                             color = "red", size=0.75) + theme(axis.text.x = element_text(angle = 90)) 
Weekly_first_6in1_line
#Custom x axis to display certain ticks only and change names of ticks

Weekly_first_6in1_line = Weekly_first_6in1_line + scale_x_discrete(breaks =c("Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 30-MAR-20", "W/B 04-MAY-20", "W/B 01-JUN-20", "W/B 29-JUN-20", "W/B 03-AUG-20", "W/B 31-AUG-20"), label = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sept 20"))

Weekly_first_6in1_line

#Add LD rectangle

Weekly_first_6in1_line = Weekly_first_6in1_line +
  annotate("rect", xmin = "W/B 23-MAR-20", xmax = "W/B 03-AUG-20", ymin = 80, ymax = 100,
           alpha = .1,fill = "blue")

Weekly_first_6in1_line

###Add all 6in1 vaccines on single graph, first format the tables
#Second6in1
Weekly_second_6in1_remove_2019 = Scotland_seconddose_6in1 %>% 
  filter(!(cohort=="2019"))%>% 
  select(immunisation, uptake_16weeks_percent, time.factor)
colnames(Weekly_second_6in1_remove_2019) = c("immunisation", "percent uptake", "time.factor")

Weekly_second_6in1_remove_2019$time.factor = factor(Weekly_second_6in1_remove_2019$time.factor, levels = c('Jan-20', 'Feb-20', 'W/B 02-MAR-20', 'W/B 09-MAR-20', 'W/B 16-MAR-20', 'W/B 23-MAR-20','W/B 30-MAR-20','W/B 06-APR-20','W/B 13-APR-20','W/B 20-APR-20','W/B 27-APR-20','W/B 04-MAY-20','W/B 11-MAY-20','W/B 18-MAY-20','W/B 25-MAY-20','W/B 01-JUN-20','W/B 08-JUN-20','W/B 15-JUN-20','W/B 22-JUN-20','W/B 29-JUN-20','W/B 06-JUL-20','W/B 13-JUL-20','W/B 20-JUL-20','W/B 27-JUL-20', 'W/B 03-AUG-20','W/B 10-AUG-20','W/B 17-AUG-20','W/B 24-AUG-20','W/B 31-AUG-20','W/B 07-SEP-20','W/B 14-SEP-20','W/B 21-SEP-20','W/B 28-SEP-20'))
#Third 6in1
Weekly_third_6in1_remove_2019 = Scotland_thirddose_6in1 %>% 
  filter(!(cohort=="2019"))%>% 
  select(immunisation, uptake_20weeks_percent,time.factor)
colnames(Weekly_third_6in1_remove_2019) = c("immunisation", "percent uptake", "time.factor")
Weekly_third_6in1_remove_2019$time.factor = factor(Weekly_third_6in1_remove_2019$time.factor, levels = c('Jan-20', 'Feb-20', 'W/B 02-MAR-20', 'W/B 09-MAR-20', 'W/B 16-MAR-20', 'W/B 23-MAR-20','W/B 30-MAR-20','W/B 06-APR-20','W/B 13-APR-20','W/B 20-APR-20','W/B 27-APR-20','W/B 04-MAY-20','W/B 11-MAY-20','W/B 18-MAY-20','W/B 25-MAY-20','W/B 01-JUN-20','W/B 08-JUN-20','W/B 15-JUN-20','W/B 22-JUN-20','W/B 29-JUN-20','W/B 06-JUL-20','W/B 13-JUL-20','W/B 20-JUL-20','W/B 27-JUL-20', 'W/B 03-AUG-20','W/B 10-AUG-20','W/B 17-AUG-20','W/B 24-AUG-20','W/B 31-AUG-20','W/B 07-SEP-20','W/B 14-SEP-20','W/B 21-SEP-20','W/B 28-SEP-20'))
#combine and plot

Combined_weekly_6in1_no2019 = rbind(Weekly_first_6in1_remove_2019, Weekly_second_6in1_remove_2019, Weekly_third_6in1_remove_2019)
#colour blind friendly palete

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Combined_weekly_6in1_no2019_line = Combined_weekly_6in1_no2019 %>% 
  ggplot(aes(x=time.factor, y=`percent uptake`, group=immunisation, color=immunisation)) +
  geom_line(size = 0.6) +
  scale_color_brewer(palette="Set2", name = NULL, labels=c("First dose  6in1", "Second dose 6in1", "Third dose 6in1"))+
  theme_classic()+
  labs(x = "Date child became eligible for immunisation",
       y = "% vaccinated within 4 weeks of eligability",
       title = "6in1 vaccine")+
  expand_limits(y=70)+
  scale_y_continuous(breaks = seq(70,100,5)) +
  geom_hline(yintercept = 94, linetype="dashed", color = "#66c2a5", size=0.75)+
  geom_hline(yintercept = 84.8, linetype="dashed", color = "#fc8d62", size=0.75)+
  geom_hline(yintercept = 73, linetype="dashed", color = "#8da0cb", size=0.75)+
  theme(axis.text.x = element_text(angle = 90))+
  annotate("rect", xmin = "W/B 23-MAR-20", xmax = "W/B 03-AUG-20", ymin = 70, ymax = 100,
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = "Jan-20", xmax = "W/B 02-MAR-20", ymin = 68, ymax = 68.5,
           alpha = .1,fill = "black")+
  annotate("text", x = "Feb-20", y = 69, size = 2, label = "Monthly data")+
  scale_x_discrete(breaks =c("Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 30-MAR-20", "W/B 04-MAY-20", "W/B 01-JUN-20", "W/B 29-JUN-20", "W/B 03-AUG-20", "W/B 31-AUG-20"), label = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sept 20")) +
  annotate("rect", xmin = "W/B 01-JUN-20", xmax = "W/B 03-AUG-20", ymin = 100.5, ymax = 102,
           alpha = .1,fill = "purple")+
  annotate("text", x ="W/B 29-JUN-20", y = 101.5, 
           label = "Gradual easing of restrictions", color="black", 
           size=2)
Combined_weekly_6in1_no2019_line

###Add all MMR vaccines on single graph, first format the tables   
#First MMR
Weekly_firstMMR_remove_2019 = Scotland_firstdose_MMR %>% 
  filter(!(cohort=="2019"))%>% 
  select(immunisation, uptake_13m_percent, time.factor)
colnames(Weekly_firstMMR_remove_2019) = c("immunisation", "percent uptake", "time.factor")

Weekly_firstMMR_remove_2019$time.factor = factor(Weekly_firstMMR_remove_2019$time.factor, levels = c('Jan-20', 'Feb-20', 'W/B 02-MAR-20', 'W/B 09-MAR-20', 'W/B 16-MAR-20', 'W/B 23-MAR-20','W/B 30-MAR-20','W/B 06-APR-20','W/B 13-APR-20','W/B 20-APR-20','W/B 27-APR-20','W/B 04-MAY-20','W/B 11-MAY-20','W/B 18-MAY-20','W/B 25-MAY-20','W/B 01-JUN-20','W/B 08-JUN-20','W/B 15-JUN-20','W/B 22-JUN-20','W/B 29-JUN-20','W/B 06-JUL-20','W/B 13-JUL-20','W/B 20-JUL-20','W/B 27-JUL-20', 'W/B 03-AUG-20','W/B 10-AUG-20','W/B 17-AUG-20','W/B 24-AUG-20','W/B 31-AUG-20','W/B 07-SEP-20','W/B 14-SEP-20','W/B 21-SEP-20','W/B 28-SEP-20'))
#Second MMR
Weekly_secondMMR_remove_2019 = Scotland_seconddose_MMR %>% 
  filter(!(cohort=="2019"))%>% 
  select(immunisation, uptake_3y5m_percent,time.factor)
colnames(Weekly_secondMMR_remove_2019) = c("immunisation", "percent uptake", "time.factor")
Weekly_secondMMR_remove_2019$time.factor = factor(Weekly_secondMMR_remove_2019$time.factor, levels = c('Jan-20', 'Feb-20', 'W/B 02-MAR-20', 'W/B 09-MAR-20', 'W/B 16-MAR-20', 'W/B 23-MAR-20','W/B 30-MAR-20','W/B 06-APR-20','W/B 13-APR-20','W/B 20-APR-20','W/B 27-APR-20','W/B 04-MAY-20','W/B 11-MAY-20','W/B 18-MAY-20','W/B 25-MAY-20','W/B 01-JUN-20','W/B 08-JUN-20','W/B 15-JUN-20','W/B 22-JUN-20','W/B 29-JUN-20','W/B 06-JUL-20','W/B 13-JUL-20','W/B 20-JUL-20','W/B 27-JUL-20', 'W/B 03-AUG-20','W/B 10-AUG-20','W/B 17-AUG-20','W/B 24-AUG-20','W/B 31-AUG-20','W/B 07-SEP-20','W/B 14-SEP-20','W/B 21-SEP-20','W/B 28-SEP-20'))
#combine and plot

Combined_weekly_MMR_no2019 = rbind(Weekly_firstMMR_remove_2019, Weekly_secondMMR_remove_2019)
#colour blind friendly palete

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Combined_weekly_MMR_no2019_line = Combined_weekly_MMR_no2019 %>% 
  ggplot(aes(x=time.factor, y=`percent uptake`, group=immunisation, color=immunisation)) +
  geom_line(size = 0.6) +
  scale_color_brewer(palette="Set2", name = NULL, labels=c("First dose MMR", "Second dose MMR"))+
  theme_classic()+
  labs(x = "Date child became eligible for immunisation",
       y = "% vaccinated  within 4 weeks of eligibility",
       title = "MMR vaccine")+
  expand_limits(y=45)+
  scale_y_continuous(breaks = seq(45,100,5)) +
  geom_hline(yintercept = 65.2, linetype="dashed", color = "#66c2a5", size=0.75)+
  geom_hline(yintercept = 51.8, linetype="dashed", color = "#fc8d62", size=0.75)+
  theme(axis.text.x = element_text(angle = 90))+
  annotate("rect", xmin = "W/B 23-MAR-20", xmax = "W/B 03-AUG-20", ymin = 40, ymax = 95,
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = "Jan-20", xmax = "W/B 02-MAR-20", ymin = 40, ymax = 41,
           alpha = .1,fill = "black")+
  annotate("text", x = "Feb-20", y = 42, size = 2, label = "Monthly data")+
  scale_x_discrete(breaks =c("Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 30-MAR-20", "W/B 04-MAY-20", "W/B 01-JUN-20", "W/B 29-JUN-20", "W/B 03-AUG-20", "W/B 31-AUG-20"), label = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sept 20")) +
  annotate("rect", xmin = "W/B 01-JUN-20", xmax = "W/B 03-AUG-20", ymin = 97, ymax = 100,
           alpha = .1,fill = "purple")+
  annotate("text", x ="W/B 29-JUN-20", y = 98.5, 
           label = "Gradual easing of restrictions", color="black", 
           size=2)
Combined_weekly_MMR_no2019_line

##Export both plots together
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Figure_1_weeklylineplots = ggarrange(Combined_weekly_6in1_no2019_line, Combined_weekly_MMR_no2019_line,
                                     labels = c("A", "B"),
                                     legend=NULL,
                                     ncol = 1, nrow = 2)
Figure_1_weeklylineplots

####Table S1
#First select out the relevant colunms incl numerator and denominator then join together. NB need to have different column names so only joins by cohort
Section1_supptbl1_first6in1 = Scotland_firstdose_6in1 %>% 
  select(immunisation, cohort, uptake_12weeks_percent, uptake_12weeks_num, denominator)
colnames(Section1_supptbl1_first6in1) = c("First 6in1", "cohort", "% uptake 12wks", "12wk uptake", "Eligable first6in1")

Section1_supptbl1_second6in1 = Scotland_seconddose_6in1 %>% 
  select(immunisation, cohort, uptake_16weeks_percent, uptake_16weeks_num, denominator)
colnames(Section1_supptbl1_second6in1) = c("Second 6in1", "cohort", "% uptake 16wks", "16wk uptake", "Eligable second 6in1")

Section1_supptbl1_third6in1 = Scotland_thirddose_6in1 %>% 
  select(immunisation, cohort, uptake_20weeks_percent, uptake_20weeks_num, denominator)
colnames(Section1_supptbl1_third6in1) = c("Third 6in1", "cohort", "% uptake 20wks", "20wk uptake", "Eligable third 6in1")

Section1_supptbl1_all6in1 = full_join(Section1_supptbl1_first6in1, Section1_supptbl1_second6in1)
Section1_supptbl1_all6in1 = full_join(Section1_supptbl1_all6in1, Section1_supptbl1_third6in1)

Section1_supptbl1_firstMMR = Scotland_firstdose_MMR %>% 
  select(immunisation, cohort, uptake_13m_percent, uptake_13m_num, denominator)
colnames(Section1_supptbl1_firstMMR) = c("First MMR", "cohort", "% uptake 13m", "13m uptake", "Eligable firstMMR")

Section1_supptbl1_secondMMR = Scotland_seconddose_MMR %>% 
  select(immunisation, cohort, uptake_3y5m_percent, uptake_3y5m_num, denominator)
colnames(Section1_supptbl1_secondMMR) = c("Second MMR", "cohort", "% uptake 3y5m", "3y5m uptake", "Eligable secondMMR")

Section1_supptbl1_allMMR = full_join(Section1_supptbl1_firstMMR, Section1_supptbl1_secondMMR)

Section1_supptbl_full = full_join(Section1_supptbl1_all6in1, Section1_supptbl1_allMMR)

write_csv(Section1_supptbl_full, file = "Exported tables/Section1_supptbl_full.csv")

##For comparison with English data, 6 monthly uptakes
First_6in1_by_LDperiod_cfEngland = Scotland_firstdose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_24weeks_num, uptake_24weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_24weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_24weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_24weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor()) 


Second_6in1_by_LDperiod_cfEngland = Scotland_seconddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_28weeks_num, uptake_28weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_28weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_28weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_28weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

Third_6in1_by_LDperiod_cfEngland = Scotland_thirddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_32weeks_num, uptake_32weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_32weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_32weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_32weeks_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

First_MMR_by_LDperiod_cfEngland = Scotland_firstdose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_16m_num, uptake_16m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_16m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_16m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_16m_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

#Second MMR (no compaison for Enlgand)
Second_MMR_by_LDperiod_cfEngland = Scotland_seconddose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_3y8m_num, uptake_3y8m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_3y8m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_3y8m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_3y8m_percent)) %>% 
  mutate (vaccinated.factor = total_vaccinated %>% 
            factor())%>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

##LD bar plots for comparison with England
First_6in1_LDplot_cfEngland = First_6in1_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor))+
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 24weeks",
       title = "First dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "ns")+
  annotate("text", x = "LD_2020",  y = 107, label = "*")+
  annotate("text", x = "Post_LD_2020",  y = 112, label = "*")

# to add line of 2019 data can add this: First_6in1_LDplot_cfEngland= First_6in1_LDplot_cfEngland + geom_hline(yintercept = 97.9, linetype="dashed", color = "Purple", size=0.5)

First_6in1_LDplot_cfEngland


#Second dose 6in1
Second_6in1_LDplot_cfEngland = Second_6in1_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 28 weeks",
       title = "Second dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "ns")+
  annotate("text", x = "LD_2020",  y = 108, label = "ns")+
  annotate("text", x = "Post_LD_2020",  y = 113, label = "ns")


## to add line for 2019 add this: Second_6in1_LDplot_cfEngland= Second_6in1_LDplot_cfEngland + geom_hline(yintercept = 96.7, linetype="dashed", color = "purple", size=0.5)

Second_6in1_LDplot_cfEngland

#Third dose 6in1
Third_6in1_LDplot_cfEngland = Third_6in1_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 32 weeks",
       title = "Third dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 98, yend = 98, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 103, yend = 103, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 108, yend = 108, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 101, label = "ns")+
  annotate("text", x = "LD_2020",  y = 105, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 111, label = "ns")

## to add line for 2019 add this: Third_6in1_LDplot_cfEngland= Third_6in1_LDplot_cfEngland + geom_hline(yintercept = 94, linetype="dashed", color = "Purple", size=0.5)
Third_6in1_LDplot_cfEngland

#First dose MMR
First_MMR_LDplot_cfEngland = First_MMR_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by 16 months",
       title = "First dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 95, yend = 95, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 98, label = "ns")+
  annotate("text", x = "LD_2020",  y = 102, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 108, label = "ns")

##To add line for 2019: First_MMR_LDplot_cfEngland= First_MMR_LDplot_cfEngland + geom_hline(yintercept = 91.1, linetype="dashed", color = "Purple", size=0.5)

First_MMR_LDplot_cfEngland


##Second dose MMR (not for ENgland)
Second_MMR_LDplot_cfEngland = Second_MMR_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by 3y 8 months",
       title = "Second dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 86, yend = 86, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 91, yend = 91, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 96, yend = 96, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 88, label = "***")+
  annotate("text", x = "LD_2020",  y = 93, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 98, label = "***")

# to add 2019 line : Second_MMR_LDplot_cfEngland= Second_MMR_LDplot_cfEngland + geom_hline(yintercept = 80.8, linetype="dashed", color = "Purple", size=0.5)

Second_MMR_LDplot_cfEngland

Scottishuptake_forcomaprisonwithEnlgand = ggarrange(First_6in1_LDplot_cfEngland, Second_6in1_LDplot_cfEngland, Third_6in1_LDplot_cfEngland, First_MMR_LDplot_cfEngland, Second_MMR_LDplot_cfEngland,
                                                    labels = "A",
                                                    legend = NULL,
                                                    ncol = 3, nrow = 2)
Scottishuptake_forcomaprisonwithEnlgand

###Logestic regression for comparisons with England nad later time periods
#First6in1
Weekly_first6in1_LDperiod_cfEnlgand = Scotland_firstdose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_24weeks_num, uptake_24weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_24weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_24weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

Weekly_first6in1_LDperiod_cfEnlgand = Weekly_first6in1_LDperiod_cfEnlgand %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

First6in1_Single_regression_tbl_cfEnlgand = cbind(Weekly_first6in1_LDperiod_cfEnlgand$uptake_24weeks_num, Weekly_first6in1_LDperiod_cfEnlgand$unvaccinated)

model_first6in1_scotland_2019_cfEnlgand = glm(First6in1_Single_regression_tbl_cfEnlgand ~ Weekly_first6in1_LDperiod_cfEnlgand$lockdown.factor,
                                              family="binomial")

summary(model_first6in1_scotland_2019_cfEnlgand)

exp(model_first6in1_scotland_2019_cfEnlgand$coefficients)
exp(confint(model_first6in1_scotland_2019_cfEnlgand))  
#Second6in1
Weekly_second6in1_LDperiod_cfEnlgand = Scotland_seconddose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_28weeks_num, uptake_28weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_28weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_28weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

Weekly_second6in1_LDperiod_cfEnlgand = Weekly_second6in1_LDperiod_cfEnlgand %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Second6in1_Single_regression_tbl_cfEnlgand = cbind(Weekly_second6in1_LDperiod_cfEnlgand$uptake_28weeks_num, Weekly_second6in1_LDperiod_cfEnlgand$unvaccinated)

model_second6in1_scotland_2019_cfEnlgand = glm(Second6in1_Single_regression_tbl_cfEnlgand ~ Weekly_second6in1_LDperiod_cfEnlgand$lockdown.factor,
                                               family="binomial")

summary(model_second6in1_scotland_2019_cfEnlgand)

exp(model_second6in1_scotland_2019_cfEnlgand$coefficients)
exp(confint(model_second6in1_scotland_2019_cfEnlgand)) 

#Third6in1
Weekly_third6in1_LDperiod_cfEnlgand = Scotland_thirddose_6in1 %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_32weeks_num, uptake_32weeks_percent)%>% 
  mutate(unvaccinated = denominator-uptake_32weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_32weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

Weekly_third6in1_LDperiod_cfEnlgand = Weekly_third6in1_LDperiod_cfEnlgand %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Third6in1_Single_regression_tbl_cfEnlgand = cbind(Weekly_third6in1_LDperiod_cfEnlgand$uptake_32weeks_num, Weekly_third6in1_LDperiod_cfEnlgand$unvaccinated)

model_third6in1_scotland_2019_cfEnlgand = glm(Third6in1_Single_regression_tbl_cfEnlgand ~ Weekly_third6in1_LDperiod_cfEnlgand$lockdown.factor,
                                              family="binomial")

summary(model_third6in1_scotland_2019_cfEnlgand)

exp(model_third6in1_scotland_2019_cfEnlgand$coefficients)
exp(confint(model_third6in1_scotland_2019_cfEnlgand)) 

#First MME
Weekly_firstMMR_LDperiod_cfEnlgand = Scotland_firstdose_MMR %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_16m_num, uptake_16m_percent)%>% 
  mutate(unvaccinated = denominator-uptake_16m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

Weekly_firstMMR_LDperiod_cfEnlgand = Weekly_firstMMR_LDperiod_cfEnlgand %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

FirstMMR_Single_regression_tbl_cfEnlgand = cbind(Weekly_firstMMR_LDperiod_cfEnlgand$uptake_16m_num, Weekly_firstMMR_LDperiod_cfEnlgand$unvaccinated)

model_firstMMR_scotland_2019_cfEnlgand = glm(FirstMMR_Single_regression_tbl_cfEnlgand ~ Weekly_firstMMR_LDperiod_cfEnlgand$lockdown.factor,
                                             family="binomial")

summary(model_firstMMR_scotland_2019_cfEnlgand)

exp(model_firstMMR_scotland_2019_cfEnlgand$coefficients)
exp(confint(model_firstMMR_scotland_2019_cfEnlgand)) 

##Second MMR (NB no data for England)
Weekly_SecondMMR_LDperiod_cfEnlgand = Scotland_seconddose_MMR %>% 
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_3y8m_num, uptake_3y8m_percent)%>% 
  mutate(unvaccinated = denominator-uptake_3y8m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y8m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())

Weekly_SecondMMR_LDperiod_cfEnlgand = Weekly_SecondMMR_LDperiod_cfEnlgand %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

SecondMMR_Single_regression_tbl_cfEnlgand = cbind(Weekly_SecondMMR_LDperiod_cfEnlgand$uptake_3y8m_num, Weekly_SecondMMR_LDperiod_cfEnlgand$unvaccinated)

model_SecondMMR_scotland_2019_cfEnlgand = glm(SecondMMR_Single_regression_tbl_cfEnlgand ~ Weekly_SecondMMR_LDperiod_cfEnlgand$lockdown.factor,
                                              family="binomial")

summary(model_SecondMMR_scotland_2019_cfEnlgand)

exp(model_SecondMMR_scotland_2019_cfEnlgand$coefficients)
exp(confint(model_SecondMMR_scotland_2019_cfEnlgand)) 



##PUt above results into a nice table
library(broom)
First6in1_model_tbl_cfEnlgand = model_first6in1_scotland_2019_cfEnlgand%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE)) %>% 
  mutate(lockdown.factor = term) 
First6in1_model_tbl_cfEnlgand

Second6in1_model_tbl_cfEnlgand = model_second6in1_scotland_2019_cfEnlgand%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

Third6in1_model_tbl_cfEnlgand = model_third6in1_scotland_2019_cfEnlgand%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

FirstMMR_model_tbl_cfEnlgand = model_firstMMR_scotland_2019_cfEnlgand%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

SecondMMR_model_tbl_cfEnlgand = model_SecondMMR_scotland_2019_cfEnlgand%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low)%>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

###Making table S5 for later age comparisons (similar to table 10)
##First 6in1

TblS5_First_6in1_by_LDperiod = First_6in1_by_LDperiod_cfEngland%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
TblS5_First_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
TblS5_First_6in1_by_LDperiod = TblS5_First_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="First6in1") %>% 
  mutate("% point  change from 2019" = mean_percent-97.9)
TblS5_First_6in1_by_LDperiod = TblS5_First_6in1_by_LDperiod[,c(3,1,2,4)]

First6in1_model_tbl_cfEnlgand$time_period <- c("Pre LD", "LD", "Post LD") 
First6in1_model_tbl_cfEnlgand = First6in1_model_tbl_cfEnlgand %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2)) %>% 
  mutate (p.value = round (p.value, digits =2))

TblS5_First_6in1_by_LDperiod = full_join(TblS5_First_6in1_by_LDperiod, First6in1_model_tbl_cfEnlgand)

colnames(TblS5_First_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake", "% point change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(TblS5_First_6in1_by_LDperiod, file = "Exported tables/TblS5_First6in1_byLDperiod.csv")

##Second 6in1

TblS5_Second_6in1_by_LDperiod = Second_6in1_by_LDperiod_cfEngland%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
TblS5_Second_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
TblS5_Second_6in1_by_LDperiod = TblS5_Second_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="Second6in1") %>% 
  mutate("% point change from 2019" = mean_percent-96.7)
TblS5_Second_6in1_by_LDperiod = TblS5_Second_6in1_by_LDperiod[,c(3,1,2,4)]

Second6in1_model_tbl_cfEnlgand$time_period <- c("Pre LD", "LD", "Post LD") 
Second6in1_model_tbl_cfEnlgand = Second6in1_model_tbl_cfEnlgand %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>% 
  mutate (p.value = round (p.value, digits =2))

TblS5_Second_6in1_by_LDperiod = full_join(TblS5_Second_6in1_by_LDperiod, Second6in1_model_tbl_cfEnlgand)

colnames(TblS5_Second_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake", "% point change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(TblS5_Second_6in1_by_LDperiod, file = "Exported tables/TblS5_Second6in1_byLDperiod.csv")

##Third 6in1

TblS5_Third_6in1_by_LDperiod = Third_6in1_by_LDperiod_cfEngland%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
TblS5_Third_6in1_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
TblS5_Third_6in1_by_LDperiod = TblS5_Third_6in1_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="Third6in1") %>% 
  mutate("% point change from 2019" = mean_percent-94)
TblS5_Third_6in1_by_LDperiod = TblS5_Third_6in1_by_LDperiod[,c(3,1,2,4)]

Third6in1_model_tbl_cfEnlgand$time_period <- c("Pre LD", "LD", "Post LD") 
Third6in1_model_tbl_cfEnlgand = Third6in1_model_tbl_cfEnlgand %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>% 
  mutate (p.value = round (p.value, digits =2))

TblS5_Third_6in1_by_LDperiod = full_join(TblS5_Third_6in1_by_LDperiod, Third6in1_model_tbl_cfEnlgand)

colnames(TblS5_Third_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake", "% point change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(TblS5_Third_6in1_by_LDperiod, file = "Exported tables/TblS5_Third6in1_byLDperiod.csv")

##First MMR

TblS5_First_MMR_by_LDperiod = First_MMR_by_LDperiod_cfEngland%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
TblS5_First_MMR_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
TblS5_First_MMR_by_LDperiod = TblS5_First_MMR_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="FirstMMR") %>% 
  mutate("% point change from 2019" = mean_percent-91.1)
TblS5_First_MMR_by_LDperiod = TblS5_First_MMR_by_LDperiod[,c(3,1,2,4)]

FirstMMR_model_tbl_cfEnlgand$time_period <- c("Pre LD", "LD", "Post LD") 
FirstMMR_model_tbl_cfEnlgand = FirstMMR_model_tbl_cfEnlgand %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>% 
  mutate (p.value = round (p.value, digits =2))

TblS5_First_MMR_by_LDperiod = full_join(TblS5_First_MMR_by_LDperiod, FirstMMR_model_tbl_cfEnlgand)

colnames(TblS5_First_MMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake", "% point change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(TblS5_First_MMR_by_LDperiod, file = "Exported tables/TblS5_FirstMMR_byLDperiod.csv")

##Second MMR

TblS5_Second_MMR_by_LDperiod = Second_MMR_by_LDperiod_cfEngland%>% 
  mutate (mean_percent= round (mean_percent, digits = 1)) 
TblS5_Second_MMR_by_LDperiod$time_period <- c("2019", "Pre LD", "LD", "Post LD")
TblS5_Second_MMR_by_LDperiod = TblS5_Second_MMR_by_LDperiod %>% 
  select(time_period, mean_percent) %>% 
  mutate("Vaccine"="SecondMMR") %>% 
  mutate("% point change from 2019" = mean_percent-80.8)
TblS5_Second_MMR_by_LDperiod = TblS5_Second_MMR_by_LDperiod[,c(3,1,2,4)]

SecondMMR_model_tbl_cfEnlgand$time_period <- c("Pre LD", "LD", "Post LD") 
SecondMMR_model_tbl_cfEnlgand = SecondMMR_model_tbl_cfEnlgand %>% 
  select(time_period, OR, lowerCI, upperCI, p.value) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))%>% 
  mutate (p.value = round (p.value, digits =2))

TblS5_Second_MMR_by_LDperiod = full_join(TblS5_Second_MMR_by_LDperiod, SecondMMR_model_tbl_cfEnlgand)

colnames(TblS5_Second_MMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake", "% point change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI", "p-value")

write_csv(TblS5_Second_MMR_by_LDperiod, file = "Exported tables/TblS5_SecondMMR_byLDperiod.csv")


#Merge all tables together and export as csv
TblS5_allvaccines = rbind(TblS5_First_6in1_by_LDperiod, TblS5_Second_6in1_by_LDperiod, TblS5_Third_6in1_by_LDperiod, TblS5_First_MMR_by_LDperiod, TblS5_Second_MMR_by_LDperiod)

write_csv(TblS5_allvaccines, file = "Exported tables/TblS5_allvaccines.csv")

#######################
##Figure 2 HSCP data by LD period
##All vaccines
##Data extracted from dashboard 3rd Feb 2021

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(geojsonio)
library(rgdal)
library(tmap)
library(viridis)

phs_main <- rgb(67,53,139, maxColorValue = 255)
phs_purple<- rgb(150,64,145, maxColorValue = 255)
phs_purple2 <- rgb(208,145,205, maxColorValue = 255)
phs_spec2 <- colorRampPalette(c(phs_main, phs_purple2))


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))####excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

###Change island names for all at the beginning
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)

Full_seconddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_6in1$area_name)
Full_seconddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_6in1$area_name)

Full_thirddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_thirddose_6in1$area_name)
Full_thirddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_thirddose_6in1$area_name)

Full_firstdose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_MMR$area_name)
Full_firstdose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_MMR$area_name)

Full_seconddose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_MMR$area_name)
Full_seconddose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_MMR$area_name)

#Load HSCP map from https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=714BD98E15D22A8116824CA25B30DC02#/metadata/ac5e870f-8fc2-4c21-8b9c-3bd2311a583f
HSCP_map <- readOGR(here("Data"), layer="SG_NHS_IntegrationAuthority_2019", verbose=FALSE)

##Select out percentage uptake for 2019 for HSPC. Remove NHS boards and Scotland

#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map


#First 6in1 LD
HSCP_LDdata_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
##Need to get mean data for each HSCP then plot as above

#First dose MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

newPolyMMR <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPolyMMR)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% vacinnated")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map


##Plot percent change
#First 6in1
#Set up the percent change table by selecting out the 2019 and LD data into tables then join
#2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

First6in1_HSCP_2019_percent = HSCP_2019data_First6in1 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
First6in1_HSCP_LD_percent = Full_firstdose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data
First6in1_Islands_LD_percent = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

First6in1_Islands_LD_percent = First6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
First6in1_HSCP_LD_percent = rbind(First6in1_HSCP_LD_percent, First6in1_Islands_LD_percent)

#Join 2019 and LD tbls
First6in1_HSPC_percentchange_2019andLD = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city and Scottish borders

First6in1_HSPC_percentchange_2019andLD = First6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)
##Select out signifcnat changes only

First6in1_HSPC_percentchange_2019andLD_SIGONLY = First6in1_HSPC_percentchange_2019andLD %>% 
  filter(area_name %in% c("Clackmannanshire and Stirling", "East Dunbartonshire", "Edinburgh", "Falkirk", "Fife", "Glasgow City", "South Ayrshire", "South Lanarkshire"))

#Plot percent change 

library(tmap)
library(viridis)
First6in1_percentchange_poly <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map = tm_shape(First6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map

#For significant changes only 
First6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map_sig = tm_shape(First6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map_sig

#Bar chart plot
library(RColorBrewer)
First6in1_HSPC_percentchange_bar = First6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
First6in1_HSPC_percentchange_bar

#Second 6in1
#2010
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Second6in1_HSCP_2019_percent = HSCP_2019data_Second6in1 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
Second6in1_HSCP_LD_percent = Full_seconddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Second6in1_Islands_LD_percent = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Second6in1_Islands_LD_percent = Second6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Second6in1_HSCP_LD_percent = rbind(Second6in1_HSCP_LD_percent,Second6in1_Islands_LD_percent)

#Join 2019 and LD
Second6in1_HSPC_percentchange_2019andLD = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)  

Second6in1_HSPC_percentchange_2019andLD = Second6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Second6in1_percentchange_poly <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map = tm_shape(Second6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map

#For significant changes only 

Second6in1_HSPC_percentchange_2019andLD_SIGONLY = Second6in1_HSPC_percentchange_2019andLD %>% 
  filter(area_name %in% c("Argyll and Bute", "Clackmannanshire and Stirling", "Dumfries and Galloway", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East Renfrewshire", "Edinburgh", "Falkirk", "Fife", "Glasgow City", "Highland", "North Lanarkshire", "Orkney Islands", "Renfrewshire", "Shetland Islands",  "South Lanarkshire", "Westh Dunbartonshire", "West Lothian"))
Second6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map_sig = tm_shape(Second6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map_sig

#Bar chart plot
Second6in1_HSPC_percentchange_bar = Second6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Second6in1_HSPC_percentchange_bar
#Third 6in1

HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Third6in1_HSCP_2019_percent = HSCP_2019data_Third6in1 %>% 
  select(area_name, uptake_20weeks_percent)
colnames(Third6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")

Third6in1_HSCP_LD_percent = Full_thirddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Third6in1_Islands_LD_percent = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Third6in1_Islands_LD_percent = Third6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Third6in1_HSCP_LD_percent = rbind(Third6in1_HSCP_LD_percent,Third6in1_Islands_LD_percent)
#Join 2019 and LD

Third6in1_HSPC_percentchange_2019andLD = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)  

Third6in1_HSPC_percentchange_2019andLD = Third6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Third6in1_percentchange_poly <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map = tm_shape(Third6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map

#For significant changes only 

Third6in1_HSPC_percentchange_2019andLD_SIGONLY = Third6in1_HSPC_percentchange_2019andLD %>% 
  filter(!(area_name %in% c("Moray", "Inverclyde", "Angus", "Aberdeenshire")))
Third6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map_sig = tm_shape(Third6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title =  "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map_sig

#Bar chart plot
Third6in1_HSPC_percentchange_bar = Third6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Third6in1_HSPC_percentchange_bar

#First MMR
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

FirstMMR_HSCP_2019_percent = HSCP_2019data_FirstMMR %>% 
  select(area_name, uptake_13m_percent)
colnames(FirstMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

FirstMMR_HSCP_LD_percent = Full_firstdose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
FirstMMR_Islands_LD_percent = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

FirstMMR_Islands_LD_percent = FirstMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
FirstMMR_HSCP_LD_percent = rbind(FirstMMR_HSCP_LD_percent,FirstMMR_Islands_LD_percent)
#Join 2019 and LD
FirstMMR_HSPC_percentchange_2019andLD = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city 

FirstMMR_HSPC_percentchange_2019andLD = FirstMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
FirstMMR_percentchange_poly <- merge(x=HSCP_map, y=FirstMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_MMR_percentchange_map = tm_shape(FirstMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First MMR % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_percentchange_map ##NOte change in pallet as none fell. Cross checked for Moray +++++all theses results were sigfnicant 

First_MMR_percentchange_map_sig = First_MMR_percentchange_map
First_MMR_percentchange_map_sig
#Bar chart plot
FirstMMR_HSPC_percentchange_bar = FirstMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
FirstMMR_HSPC_percentchange_bar

#Second MMR ++++excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

SecondMMR_HSCP_2019_percent = HSCP_2019data_SecondMMR %>% 
  select(area_name, uptake_3y5m_percent)
colnames(SecondMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

SecondMMR_HSCP_LD_percent = Full_seconddose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
SecondMMR_Islands_LD_percent = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

SecondMMR_Islands_LD_percent = SecondMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
SecondMMR_HSCP_LD_percent = rbind(SecondMMR_HSCP_LD_percent,SecondMMR_Islands_LD_percent)
#Join 2019 and LD

SecondMMR_HSPC_percentchange_2019andLD = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)  

SecondMMR_HSPC_percentchange_2019andLD = SecondMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
SecondMMR_percentchange_poly <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map = tm_shape(SecondMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map

#For significant changes only 

SecondMMR_HSPC_percentchange_2019andLD_SIGONLY = SecondMMR_HSPC_percentchange_2019andLD %>% 
  filter(!(area_name %in% c("East Ayrshire", "Argyll and Bute")))

SecondMMR_percentchange_poly_sig <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map_sig = tm_shape(SecondMMR_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map_sig

#Bar chart plot
SecondMMR_HSPC_percentchange_bar = SecondMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
SecondMMR_HSPC_percentchange_bar

##Export the % change maps and bar charts in one pdf
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Percent_change_bar_HSCP_6in1 = ggarrange(First6in1_HSPC_percentchange_bar, Second6in1_HSPC_percentchange_bar, Third6in1_HSPC_percentchange_bar,
                                         labels = c("First6in1", "Second6in1", "Third6in1"), hjust = -1,
                                         legend=NULL,
                                         ncol = 2, nrow = 2)

Percent_change_bar_HSCP_6in1

Percent_change_bar_HSCP_MMR = ggarrange(FirstMMR_HSPC_percentchange_bar, SecondMMR_HSPC_percentchange_bar, 
                                        labels = c("First MMR","Second MMR"), hjust = -1.5,
                                        legend=NULL,
                                        ncol = 2, nrow = 1)

Percent_change_bar_HSCP_MMR


##To get a grouped bar chart

#Grouped bar chart LD vs 2019 percent uptake First 6in1

First6in1_HSCP_2019_percent = First6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(First6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(First6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

First6in1_Absolute_percentage = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)

Grouped_First6in1_2019LDpercentages = First6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Perth and Kinross", "Glasgow City", "Dundee City", "Highland", "West Dunbartonshire", "Edinburgh", "Orkney Islands", "Angus", "Aberdeen City","Argyll and Bute", "Clackmannanshire and Stirling", "Falkirk", "Midlothian", "Scottish Borders", "Dumfries and Galloway", "East Lothian", "West Lothian", "North Lanarkshire", "Fife", "Shetland Islands", "Western Isles", "Moray", "Renfrewshire", "South Lanarkshire", "East Dunbartonshire", "South Ayrshire", "Aberdeenshire", "North Ayrshire", "East Ayrshire", "East Renfrewshire", "Inverclyde")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 93.9, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 94.9, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_First6in1_2019LDpercentages


#Grouped bar chart LD vs 2019 percent uptake Second 6in1

Second6in1_HSCP_2019_percent = Second6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Second6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Second6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Second6in1_Absolute_percentage = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)

Grouped_Second6in1_2019LDpercentages = Second6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Dundee City", "Glasgow City", "West Dunbartonshire", "Highland", "Edinburgh", "Argyll and Bute", "Perth and Kinross", "Dumfries and Galloway", "Falkirk", "Angus", "West Lothian", "Midlothian", "East Lothian", "Aberdeen City", "Clackmannanshire and Stirling", "Orkney Islands", "Scottish Borders", "North Ayrshire", "Fife", "North Lanarkshire", "Renfrewshire", "South Lanarkshire", "East Dunbartonshire", "East Ayrshire", "Western Isles", "Shetland Islands", "Moray", "East Renfrewshire", "Inverclyde", "South Ayrshire", "Aberdeenshire")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 84.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 89.7, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_Second6in1_2019LDpercentages


#Grouped bar chart LD vs 2019 percent uptake Third 6in1

Third6in1_HSCP_2019_percent = Third6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Third6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Third6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Third6in1_Absolute_percentage = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)

Grouped_Third6in1_2019LDpercentages = Third6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Falkirk", "Dundee City", "West Lothian", "Highland", "West Dunbartonshire", "Edinburgh", "Dumfries and Galloway", "Glasgow City", "Midlothian", "Argyll and Bute", "Perth and Kinross", "East Lothian", "North Ayrshire", "Angus", "Clackmannanshire and Stirling", "Aberdeen City", "Western Isles", "North Lanarkshire", "Fife", "Orkney Islands", "Scottish Borders", "South Lanarkshire", "Shetland Islands", "Renfrewshire", "East Ayrshire", "East Dunbartonshire", "Moray", "South Ayrshire", "Inverclyde", "East Renfrewshire", "Aberdeenshire")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 73, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 82.1, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_Third6in1_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake First MMR

FirstMMR_HSCP_2019_percent = FirstMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(FirstMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(FirstMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

FirstMMR_Absolute_percentage = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)

Grouped_FirstMMR_2019LDpercentages = FirstMMR_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area,"Shetland Islands", "Aberdeenshire", "Western Isles", "Orkney Islands", "Moray", "Aberdeen City", "North Ayrshire", "Highland", "East Ayrshire", "Dundee City", "South Ayrshire", "Argyll and Bute", "Midlothian", "West Dunbartonshire", "Perth and Kinross", "West Lothian", "Fife", "Glasgow City", "Scottish Borders", "Edinburgh", "Falkirk", "Renfrewshire", "Angus", "North Lanarkshire", "South Lanarkshire", "Clackmannanshire and Stirling", "East Lothian", "Dumfries and Galloway", "East Renfrewshire", "Inverclyde", "East Dunbartonshire")) %>% 
  ggplot(aes(fill=`Time period`, y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 65.2, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 78.4, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_FirstMMR_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake Second MMR

SecondMMR_HSCP_2019_percent = SecondMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(SecondMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(SecondMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

SecondMMR_Absolute_percentage = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)

Grouped_SecondMMR_2019LDpercentages = SecondMMR_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area,"Shetland Islands", "Dundee City", "Orkney Islands", "North Ayrshire","East Ayrshire", "South Ayrshire", "Angus", "Perth and Kinross", "Western Isles", "Fife", "Falkirk", "Highland", "Glasgow City", "Clackmannanshire and Stirling", "North Lanarkshire", "West Dunbartonshire", "South Lanarkshire", "Scottish Borders", "Edinburgh", "East Dunbartonshire", "Argyll and Bute", "Renfrewshire", "West Lothian", "East Lothian", "Midlothian", "Inverclyde", "East Renfrewshire", "Dumfries and Galloway")) %>%   ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 51.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 66.1, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_SecondMMR_2019LDpercentages

###Export grouped bar charts as one figure
Grouped_bar_HSCP_LDvs2019percentages_6in1 = ggarrange(Grouped_First6in1_2019LDpercentages,Grouped_Second6in1_2019LDpercentages, Grouped_Third6in1_2019LDpercentages,
                                                      common.legend = TRUE, legend="bottom",
                                                      labels = NULL,
                                                      ncol = 3, nrow = 1)
Grouped_bar_HSCP_LDvs2019percentages_6in1

Grouped_bar_HSCP_LDvs2019percentages_MMR = ggarrange(Grouped_FirstMMR_2019LDpercentages, Grouped_SecondMMR_2019LDpercentages,
                                                     common.legend = TRUE, legend="bottom",
                                                     labels = NULL,
                                                     ncol = 2, nrow = 1)
Grouped_bar_HSCP_LDvs2019percentages_MMR

Grouped_bar_HSCP_LDvs2019percentages_allvaccine = ggarrange(Grouped_bar_HSCP_LDvs2019percentages_6in1, Grouped_bar_HSCP_LDvs2019percentages_MMR,
                                                            legend = NULL,
                                                            labels = c("A", "B"),
                                                            ncol = 1, nrow = 2)
Grouped_bar_HSCP_LDvs2019percentages_allvaccine

Alt_Grouped_bar_HSCP_LDvs2019percentages_allvaccine = ggarrange(Grouped_First6in1_2019LDpercentages,Grouped_Second6in1_2019LDpercentages, Grouped_Third6in1_2019LDpercentages,Grouped_FirstMMR_2019LDpercentages, Grouped_SecondMMR_2019LDpercentages,
                                                                common.legend = TRUE, legend="bottom",
                                                                labels = NULL,
                                                                ncol = 2, nrow = 3)
Alt_Grouped_bar_HSCP_LDvs2019percentages_allvaccine

####Logistic regression analysis of uptake rates between HSCP for 2019 and LD
##First 6in1 Log regression
#Fisrt 6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

First6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_First6in1$uptake_12weeks_num, HSCP_2019data_First6in1$unvaccinated)

model_first6in1_HSCP_2019 = glm(First6in1_HSCP2019_regression_tbl ~ HSCP_2019data_First6in1$area.factor,
                                family="binomial")

summary(model_first6in1_HSCP_2019)

exp(model_first6in1_HSCP_2019$coefficients)
exp(confint(model_first6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
First6in1_HSCP2019model_tbl = model_first6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#First 6in1 plot
ORandCI_first6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(First6in1_HSCP2019model_tbl$OR),
  boxCILow = c(First6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(First6in1_HSCP2019model_tbl$lowerCI))

First6in1_HSCP2019_forest <- ggplot(ORandCI_first6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
First6in1_HSCP2019_forest = First6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

First6in1_HSCP2019_forest

anova(model_first6in1_HSCP_2019, test="LRT")

##First 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get he correct results ? as using agregate data

#Set up tbl with vaccinated and unvaccinated factors

HSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  select(area.factor, uptake_12weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_First6in1 = SummaryHSCP_LDdata_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirst6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_First6in1$total_vaccinated, SummaryHSCP_LDdata_First6in1$total_unvaccinated)

Summarymodel_first6in1_HSCP_LD = glm(SummaryFirst6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_First6in1$area.factor,
                                     family="binomial")

summary(Summarymodel_first6in1_HSCP_LD)

exp(Summarymodel_first6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_first6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirst6in1_HSCPLDmodel_tbl = Summarymodel_first6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_first6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirst6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirst6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirst6in1_HSCPLDmodel_tbl$lowerCI))

SummaryFirst6in1_HSCPLD_forest <- ggplot(SummaryORandCI_first6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirst6in1_HSCPLD_forest = SummaryFirst6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirst6in1_HSCPLD_forest

anova(Summarymodel_first6in1_HSCP_LD, test="LRT")

#Export First 6in1 plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_6in1_OR_plots = ggarrange(First6in1_HSCP2019_forest, SummaryFirst6in1_HSCPLD_forest,
                                labels = c("First6in12019","First6in1LD" ),hjust = -1.5,
                                legend = NULL,
                                ncol = 2, nrow = 1)
First_6in1_OR_plots

#####Second 6in1 log regression
##Second6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Second6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Second6in1$uptake_16weeks_num, HSCP_2019data_Second6in1$unvaccinated)

model_second6in1_HSCP_2019 = glm(Second6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Second6in1$area.factor,
                                 family="binomial")

summary(model_second6in1_HSCP_2019)

exp(model_second6in1_HSCP_2019$coefficients)
exp(confint(model_second6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Second6in1_HSCP2019model_tbl = model_second6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second 6in1 2019 plot
ORandCI_second6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Second6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Second6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Second6in1_HSCP2019model_tbl$lowerCI))

Second6in1_HSCP2019_forest <- ggplot(ORandCI_second6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Second6in1_HSCP2019_forest = Second6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

Second6in1_HSCP2019_forest

anova(model_second6in1_HSCP_2019, test="LRT")

##Second 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  select(area.factor, uptake_16weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_16weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Second6in1 = SummaryHSCP_LDdata_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecond6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Second6in1$total_vaccinated, SummaryHSCP_LDdata_Second6in1$total_unvaccinated)

Summarymodel_second6in1_HSCP_LD = glm(SummarySecond6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Second6in1$area.factor,
                                      family="binomial")

summary(Summarymodel_second6in1_HSCP_LD)

exp(Summarymodel_second6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_second6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecond6in1_HSCPLDmodel_tbl = Summarymodel_second6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_second6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummarySecond6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecond6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecond6in1_HSCPLDmodel_tbl$lowerCI))

SummarySecond6in1_HSCPLD_forest <- ggplot(SummaryORandCI_second6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummarySecond6in1_HSCPLD_forest = SummarySecond6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecond6in1_HSCPLD_forest

anova(Summarymodel_second6in1_HSCP_LD, test="LRT")
#Export Second 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_6in1_OR_plots = ggarrange(Second6in1_HSCP2019_forest, SummarySecond6in1_HSCPLD_forest,
                                 labels = c("Second6in1 2019","Second6in1 LD" ),hjust = -0.75,
                                 legend = NULL,
                                 ncol = 2, nrow = 1)
Second_6in1_OR_plots

#####THird 6in1 log regression
##Third6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE- for this vaccine, midlothian uptake is lower than SCottish mean

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Third6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Third6in1$uptake_20weeks_num, HSCP_2019data_Third6in1$unvaccinated)

model_third6in1_HSCP_2019 = glm(Third6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Third6in1$area.factor,
                                family="binomial")

summary(model_third6in1_HSCP_2019)

exp(model_third6in1_HSCP_2019$coefficients)
exp(confint(model_third6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Third6in1_HSCP2019model_tbl = model_third6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls 
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Third 6in1 2019 plot
ORandCI_third6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Third6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Third6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Third6in1_HSCP2019model_tbl$lowerCI))

Third6in1_HSCP2019_forest <- ggplot(ORandCI_third6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Third6in1_HSCP2019_forest = Third6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
Third6in1_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Third 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  select(area.factor, uptake_20weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_20weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Third6in1 = SummaryHSCP_LDdata_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryThird6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Third6in1$total_vaccinated, SummaryHSCP_LDdata_Third6in1$total_unvaccinated)

Summarymodel_third6in1_HSCP_LD = glm(SummaryThird6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Third6in1$area.factor,
                                     family="binomial")

summary(Summarymodel_third6in1_HSCP_LD)

exp(Summarymodel_third6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_third6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryThird6in1_HSCPLDmodel_tbl = Summarymodel_third6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_third6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryThird6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryThird6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryThird6in1_HSCPLDmodel_tbl$lowerCI))

SummaryThird6in1_HSCPLD_forest <- ggplot(SummaryORandCI_third6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryThird6in1_HSCPLD_forest = SummaryThird6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryThird6in1_HSCPLD_forest

anova(Summarymodel_third6in1_HSCP_LD, test="LRT")
#Export Third 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Third_6in1_OR_plots = ggarrange(Third6in1_HSCP2019_forest, SummaryThird6in1_HSCPLD_forest,
                                labels = c("Third6in1 2019","Third6in1 LD" ),hjust = -0.75,
                                legend = NULL,
                                ncol = 2, nrow = 1)
Third_6in1_OR_plots

#####First MMR log regression
##First MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

FirstMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_FirstMMR$uptake_13m_num, HSCP_2019data_FirstMMR$unvaccinated)

model_firstMMR_HSCP_2019 = glm(FirstMMR_HSCP2019_regression_tbl ~ HSCP_2019data_FirstMMR$area.factor,
                               family="binomial")

summary(model_firstMMR_HSCP_2019)

exp(model_firstMMR_HSCP_2019$coefficients)
exp(confint(model_firstMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
FirstMMR_HSCP2019model_tbl = model_firstMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#FirstMMR 2019 plot
ORandCI_firstMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(FirstMMR_HSCP2019model_tbl$OR),
  boxCILow = c(FirstMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(FirstMMR_HSCP2019model_tbl$lowerCI))

FirstMMR_HSCP2019_forest <- ggplot(ORandCI_firstMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
FirstMMR_HSCP2019_forest = FirstMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
FirstMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##FirstMMR LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  select(area.factor, uptake_13m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_13m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_FirstMMR = SummaryHSCP_LDdata_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirstMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_FirstMMR$total_vaccinated, SummaryHSCP_LDdata_FirstMMR$total_unvaccinated)

Summarymodel_firstMMR_HSCP_LD = glm(SummaryFirstMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_FirstMMR$area.factor,
                                    family="binomial")

summary(Summarymodel_firstMMR_HSCP_LD)

exp(Summarymodel_firstMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_firstMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirstMMR_HSCPLDmodel_tbl = Summarymodel_firstMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_firstMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirstMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirstMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirstMMR_HSCPLDmodel_tbl$lowerCI))

SummaryFirstMMR_HSCPLD_forest <- ggplot(SummaryORandCI_firstMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirstMMR_HSCPLD_forest = SummaryFirstMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirstMMR_HSCPLD_forest

anova(Summarymodel_firstMMR_HSCP_LD, test="LRT")
#Export FirstMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_MMR_OR_plots = ggarrange(FirstMMR_HSCP2019_forest, SummaryFirstMMR_HSCPLD_forest,
                               labels = c("FirstMMR 2019","FirstMMR LD" ),hjust = -1,
                               legend = NULL,
                               ncol = 2, nrow = 1)
First_MMR_OR_plots


#####Second MMR log regression
##Second MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE Midlothian is average avergae for this dose

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

SecondMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_SecondMMR$uptake_3y5m_num, HSCP_2019data_SecondMMR$unvaccinated)

model_secondMMR_HSCP_2019 = glm(SecondMMR_HSCP2019_regression_tbl ~ HSCP_2019data_SecondMMR$area.factor,
                                family="binomial")

summary(model_secondMMR_HSCP_2019)

exp(model_secondMMR_HSCP_2019$coefficients)
exp(confint(model_secondMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
SecondMMR_HSCP2019model_tbl = model_secondMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls+++NOTE removed NHS Grampian areas
boxLabelsHSCPsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second MMR 2019 plot
ORandCI_secondMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCPsecondMMR):1,
  boxOdds = c(SecondMMR_HSCP2019model_tbl$OR),
  boxCILow = c(SecondMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(SecondMMR_HSCP2019model_tbl$lowerCI))

SecondMMR_HSCP2019_forest <- ggplot(ORandCI_secondMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCPsecondMMR))
SecondMMR_HSCP2019_forest = SecondMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
SecondMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Second MMR LD2020 
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD data table as not done above

HSCP_LDdata_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  select(area.factor, uptake_3y5m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_3y5m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_SecondMMR = SummaryHSCP_LDdata_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecondMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_SecondMMR$total_vaccinated, SummaryHSCP_LDdata_SecondMMR$total_unvaccinated)

Summarymodel_secondMMR_HSCP_LD = glm(SummarySecondMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_SecondMMR$area.factor,
                                     family="binomial")

summary(Summarymodel_secondMMR_HSCP_LD)

exp(Summarymodel_secondMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_secondMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecondMMR_HSCPLDmodel_tbl = Summarymodel_secondMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls ####NOTe removing NHS Grampian areas as above
boxLabelsHSCPLDsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney and NHS Grampian

SummaryORandCI_secondMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLDsecondMMR):1,
  boxOdds = c(SummarySecondMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecondMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecondMMR_HSCPLDmodel_tbl$lowerCI))

SummarySecondMMR_HSCPLD_forest <- ggplot(SummaryORandCI_secondMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLDsecondMMR))
SummarySecondMMR_HSCPLD_forest = SummarySecondMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecondMMR_HSCPLD_forest

anova(Summarymodel_secondMMR_HSCP_LD, test="LRT")
#Export SecondMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_MMR_OR_plots = ggarrange(SecondMMR_HSCP2019_forest, SummarySecondMMR_HSCPLD_forest,
                                labels = c("SecondMMR 2019","SecondMMR LD" ),hjust = -1,
                                legend = NULL,
                                ncol = 2, nrow = 1)
Second_MMR_OR_plots

###Secion 1 Figure 2 Make 2019 maps for all 
#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map

#Second 6in1 2019
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 

newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Second6in1, by.x = "HIAName", by.y = "area_name")

Second_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_16weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_2019_map

#Third 6in1 2019
HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Third6in1, by.x = "HIAName", by.y = "area_name")

Third_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_20weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_2019_map

#First MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map

#Second MMR 2019
HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_SecondMMR, by.x = "HIAName", by.y = "area_name")

Second_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_3y5m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_2019_map

##Export maps 2019 and percentage change

tmap_arrange(First_6in1_2019_map, First_6in1_percentchange_map, Second_6in1_2019_map, Second_6in1_percentchange_map, Third_6in1_2019_map, Third_6in1_percentchange_map,  ncol = 2,
             nrow = 3) 
tmap_arrange(First_MMR_2019_map, First_MMR_percentchange_map, Second_MMR_2019_map, Second_MMR_percentchange_map,  ncol = 2,
             nrow = 2)

###Export the sig only percent change maps with 2019
tmap_arrange(First_6in1_2019_map, First_6in1_percentchange_map_sig, Second_6in1_2019_map, Second_6in1_percentchange_map_sig, Third_6in1_2019_map, Third_6in1_percentchange_map_sig,  ncol = 2,
             nrow = 3) 
tmap_arrange(First_MMR_2019_map, First_MMR_percentchange_map_sig, Second_MMR_2019_map, Second_MMR_percentchange_map_sig,  ncol = 2,
             nrow = 2)
##Export the point change maps only
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map, ncol = 3,
             nrow = 2)

debuggingState(on=FALSE)
###########Make the table for supplementary info
####Making tablefor Section1 table 1
##First 6in1

Section2suptbl2_First6in1 = Full_firstdose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_First6in1_2019 = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_12weeks_num, uptake_12weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
First6in1_2019areapercentage = Section2suptbl2_First6in1_2019 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_First6in1_PreLD = full_join(Section2suptbl2_First6in1_PreLD, First6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
First6in1_Islands = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_First6in1_LD = rbind(Section2suptbl2_First6in1_LD, First6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_First6in1_LD = full_join(Section2suptbl2_First6in1_LD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
First6in1_Islands_PostLD = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_First6in1_PostLD = rbind(Section2suptbl2_First6in1_PostLD, First6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_First6in1_PostLD = full_join(Section2suptbl2_First6in1_PostLD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_First6in1 = full_join(Section2suptbl2_First6in1_2019, Section2suptbl2_First6in1_PreLD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_LD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_PostLD)
FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1%>% 
  mutate (uptake_12weeks_percent= round (uptake_12weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_First6in1, file = "Exported tables/FullSection2suptbl2_First6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_First6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
First6in1_HSCPORtbl_tidypreLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
First6in1_HSCPORtbl_tidyLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
First6in1_HSCPORtbl_tidyPostLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1, First6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1_OR, First6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1_OR, First6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_First6in1_OR, file = "Exported tables/FullSection2suptbl2_First6in1_OR.csv")
###################
##Second 6in1

Section2suptbl2_Second6in1 = Full_seconddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_Second6in1_2019 = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_16weeks_num, uptake_16weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
Second6in1_2019areapercentage = Section2suptbl2_Second6in1_2019 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_Second6in1_PreLD = full_join(Section2suptbl2_Second6in1_PreLD, Second6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1_PreLD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1_PreLD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
Second6in1_Islands = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_Second6in1_LD = rbind(Section2suptbl2_Second6in1_LD, Second6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_Second6in1_LD = full_join(Section2suptbl2_Second6in1_LD, Second6in1_2019areapercentage)
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1_LD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1_LD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
Second6in1_Islands_PostLD = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_Second6in1_PostLD = rbind(Section2suptbl2_Second6in1_PostLD, Second6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_Second6in1_PostLD = full_join(Section2suptbl2_Second6in1_PostLD, Second6in1_2019areapercentage)
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1_PostLD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1_PostLD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_Second6in1 = full_join(Section2suptbl2_Second6in1_2019, Section2suptbl2_Second6in1_PreLD)
FullSection2suptbl2_Second6in1 = full_join(FullSection2suptbl2_Second6in1, Section2suptbl2_Second6in1_LD)
FullSection2suptbl2_Second6in1 = full_join(FullSection2suptbl2_Second6in1, Section2suptbl2_Second6in1_PostLD)
FullSection2suptbl2_Second6in1 = FullSection2suptbl2_Second6in1%>% 
  mutate (uptake_16weeks_percent= round (uptake_16weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_Second6in1 = FullSection2suptbl2_Second6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_First6in1, file = "Exported tables/FullSection2suptbl2_First6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_Second6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
Second6in1_HSCPORtbl_tidypreLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
Second6in1_HSCPORtbl_tidyLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
Second6in1_HSCPORtbl_tidyPostLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1, Second6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1_OR, Second6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1_OR, Second6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_Second6in1_OR, file = "Exported tables/FullSection2suptbl2_Second6in1_OR.csv")

###################
##Third 6in1

Section2suptbl2_Third6in1 = Full_thirddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_Third6in1_2019 = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_20weeks_num, uptake_20weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
Third6in1_2019areapercentage = Section2suptbl2_Third6in1_2019 %>% 
  select(area_name, uptake_20weeks_percent)
colnames(Third6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_Third6in1_PreLD = full_join(Section2suptbl2_Third6in1_PreLD, Third6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1_PreLD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1_PreLD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
Third6in1_Islands = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_Third6in1_LD = rbind(Section2suptbl2_Third6in1_LD, Third6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_Third6in1_LD = full_join(Section2suptbl2_Third6in1_LD, Third6in1_2019areapercentage)
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1_LD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1_LD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
Third6in1_Islands_PostLD = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_Third6in1_PostLD = rbind(Section2suptbl2_Third6in1_PostLD, Third6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_Third6in1_PostLD = full_join(Section2suptbl2_Third6in1_PostLD, Third6in1_2019areapercentage)
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1_PostLD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1_PostLD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_Third6in1 = full_join(Section2suptbl2_Third6in1_2019, Section2suptbl2_Third6in1_PreLD)
FullSection2suptbl2_Third6in1 = full_join(FullSection2suptbl2_Third6in1, Section2suptbl2_Third6in1_LD)
FullSection2suptbl2_Third6in1 = full_join(FullSection2suptbl2_Third6in1, Section2suptbl2_Third6in1_PostLD)
FullSection2suptbl2_Third6in1 = FullSection2suptbl2_Third6in1%>% 
  mutate (uptake_20weeks_percent= round (uptake_20weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_Third6in1 = FullSection2suptbl2_Third6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_Third6in1, file = "Exported tables/FullSection2suptbl2_Third6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_Third6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
Third6in1_HSCPORtbl_tidypreLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
Third6in1_HSCPORtbl_tidyLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
Third6in1_HSCPORtbl_tidyPostLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1, Third6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1_OR, Third6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1_OR, Third6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_Third6in1_OR, file = "Exported tables/FullSection2suptbl2_Third6in1_OR.csv")

##First MMR

Section2suptbl2_FirstMMR = Full_firstdose_MMR %>% 
  select(area_name, cohort, denominator, uptake_13m_num, uptake_13m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_FirstMMR_2019 = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_13m_num, uptake_13m_percent, time_period)
#Take out the 2019 percentage to get % change for table
FirstMMR_2019areapercentage = Section2suptbl2_FirstMMR_2019 %>% 
  select(area_name, uptake_13m_percent)
colnames(FirstMMR_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_FirstMMR_PreLD = full_join(Section2suptbl2_FirstMMR_PreLD, FirstMMR_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR_PreLD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR_PreLD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)

#LD period
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
FirstMMR_Islands = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_FirstMMR_LD = rbind(Section2suptbl2_FirstMMR_LD, FirstMMR_Islands)

#Back to obtaining change from 2019
Section2suptbl2_FirstMMR_LD = full_join(Section2suptbl2_FirstMMR_LD, FirstMMR_2019areapercentage)
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR_LD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR_LD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)

#Post LD
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
FirstMMR_Islands_PostLD = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_13m_num, uptake_13m_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_FirstMMR_PostLD = rbind(Section2suptbl2_FirstMMR_PostLD, FirstMMR_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_FirstMMR_PostLD = full_join(Section2suptbl2_FirstMMR_PostLD, FirstMMR_2019areapercentage)
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR_PostLD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR_PostLD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_FirstMMR = full_join(Section2suptbl2_FirstMMR_2019, Section2suptbl2_FirstMMR_PreLD)
FullSection2suptbl2_FirstMMR = full_join(FullSection2suptbl2_FirstMMR, Section2suptbl2_FirstMMR_LD)
FullSection2suptbl2_FirstMMR = full_join(FullSection2suptbl2_FirstMMR, Section2suptbl2_FirstMMR_PostLD)
FullSection2suptbl2_FirstMMR = FullSection2suptbl2_FirstMMR%>% 
  mutate (uptake_13m_percent= round (uptake_13m_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_FirstMMR = FullSection2suptbl2_FirstMMR[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_FirstMMR, file = "Exported tables/FullSection2suptbl2_FirstMMR.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_FirstMMR) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
FirstMMR_HSCPORtbl_tidypreLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
FirstMMR_HSCPORtbl_tidyLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
FirstMMR_HSCPORtbl_tidyPostLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR, FirstMMR_HSCPORtbl_tidypreLD)
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR_OR, FirstMMR_HSCPORtbl_tidyLD)
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR_OR, FirstMMR_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_FirstMMR_OR, file = "Exported tables/FullSection2suptbl2_FirstMMR_OR.csv")

######Second MMR

Section2suptbl2_SecondMMR = Full_seconddose_MMR %>% 
  select(area_name, cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_SecondMMR_2019 = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_3y5m_num, uptake_3y5m_percent, time_period)
#Take out the 2019 percentage to get % change for table
SecondMMR_2019areapercentage = Section2suptbl2_SecondMMR_2019 %>% 
  select(area_name, uptake_3y5m_percent)
colnames(SecondMMR_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_SecondMMR_PreLD = full_join(Section2suptbl2_SecondMMR_PreLD, SecondMMR_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR_PreLD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR_PreLD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)

#LD period
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
SecondMMR_Islands = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_SecondMMR_LD = rbind(Section2suptbl2_SecondMMR_LD, SecondMMR_Islands)

#Back to obtaining change from 2019
Section2suptbl2_SecondMMR_LD = full_join(Section2suptbl2_SecondMMR_LD, SecondMMR_2019areapercentage)
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR_LD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR_LD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)

#Post LD
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
SecondMMR_Islands_PostLD = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_SecondMMR_PostLD = rbind(Section2suptbl2_SecondMMR_PostLD, SecondMMR_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_SecondMMR_PostLD = full_join(Section2suptbl2_SecondMMR_PostLD, SecondMMR_2019areapercentage)
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR_PostLD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR_PostLD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_SecondMMR = full_join(Section2suptbl2_SecondMMR_2019, Section2suptbl2_SecondMMR_PreLD)
FullSection2suptbl2_SecondMMR = full_join(FullSection2suptbl2_SecondMMR, Section2suptbl2_SecondMMR_LD)
FullSection2suptbl2_SecondMMR = full_join(FullSection2suptbl2_SecondMMR, Section2suptbl2_SecondMMR_PostLD)
FullSection2suptbl2_SecondMMR = FullSection2suptbl2_SecondMMR%>% 
  mutate (uptake_3y5m_percent= round (uptake_3y5m_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_SecondMMR = FullSection2suptbl2_SecondMMR[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_SecondMMR, file = "Exported tables/FullSection2suptbl2_SecondMMR.csv")

###Try to make the loop for HSCP comparing 2019 to LD

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(zoo)
library(lubridate)


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles) and change names of islands
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)

Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))####excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

Full_seconddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_6in1$area_name)
Full_seconddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_6in1$area_name)

Full_thirddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_thirddose_6in1$area_name)
Full_thirddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_thirddose_6in1$area_name)

Full_firstdose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_MMR$area_name)
Full_firstdose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_MMR$area_name)

Full_seconddose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_MMR$area_name)
Full_seconddose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_MMR$area_name)

#################
####First 6in1
##Set up loop as per Rachel (note my individual regression codes are below)
##Get the correct data tables
Section2suptbl2_First6in1_forloop = Full_firstdose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  select(-c(cohort, uptake_12weeks_percent)) %>% 
  group_by(area_name,lockdown.factor) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num)) %>% 
  mutate(unvaccinated = denominator- uptake_12weeks_num) %>% 
  mutate(area_name = as.factor(area_name))
# Create list of HSCP as a character
HSCP_levels <- levels(Section2suptbl2_First6in1_forloop$area_name)
# Create an empty data frame to store coefficients in
First6in1_HSCPORtbl <- data.frame(HSCP = HSCP_levels, OR_2019_PreLD = NA, OR_2019_PreLD_lwr = NA, 
                                  OR_2019_PreLD_upr = NA, OR_2019_PreLD_pvalue = NA, OR_2019_PreLD_sig_diff =NA, 
                                  OR_2019_LD = NA, OR_2019_LD_lwr = NA, 
                                  OR_2019_LD_upr = NA, OR_2019_LD_pvalue = NA, OR_2019_LD_sig_diff =NA, OR_2019_PostLD = NA, OR_2019_PostLD_lwr = NA, 
                                  OR_2019_PostLD_upr = NA, OR_2019_PostLD_pvalue = NA, OR_2019_PostLD_sig_diff =NA)

i <- 2

for(i in 1:nrow(First6in1_HSCPORtbl)){
  # Set reference level
  Section2suptbl2_First6in1_forloop <- within(Section2suptbl2_First6in1_forloop, area_name <- relevel(area_name, ref=HSCP_levels[i]))
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_HSCP <- cbind(Section2suptbl2_First6in1_forloop$uptake_12weeks_num, Section2suptbl2_First6in1_forloop$unvaccinated)
  # Column for time-period
  tp <- Section2suptbl2_First6in1_forloop$lockdown.factor
  # Column for Health Board
  HSCP <- Section2suptbl2_First6in1_forloop$area_name
  
  # Put into GLM with interaction
  First6in1model_HSCP<- glm(tbl_immun_HSCP ~ tp*HSCP,
                            family="binomial")
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(First6in1model_HSCP$coefficients)
  odd_ratio_confint <- exp(confint.default(First6in1model_HSCP))
  model_pvalues <- summary(First6in1model_HSCP)$coefficients[,4]
  
  # For 2019 vs preLD for HSCP level of interest (ith one), the OR of interest is the tp
  First6in1_HSCPORtbl[i,2] <- odd_ratios[2]
  First6in1_HSCPORtbl[i,3] <- odd_ratio_confint[2,1]
  First6in1_HSCPORtbl[i,4] <- odd_ratio_confint[2,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  First6in1_HSCPORtbl[i,5] <- model_pvalues[2]
  
  First6in1_HSCPORtbl[i,6] <- ifelse(First6in1_HSCPORtbl[i,5] < 0.05, 1, 0)
  
  # Repeat for LD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  First6in1_HSCPORtbl[i,7] <- odd_ratios[3]
  First6in1_HSCPORtbl[i,8] <- odd_ratio_confint[3,1]
  First6in1_HSCPORtbl[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  First6in1_HSCPORtbl[i,10] <- model_pvalues[3]
  
  First6in1_HSCPORtbl[i,11] <- ifelse(First6in1_HSCPORtbl[i,10] < 0.05, 1, 0)
  # Repeat for PostLD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  First6in1_HSCPORtbl[i,12] <- odd_ratios[4]
  First6in1_HSCPORtbl[i,13] <- odd_ratio_confint[4,1]
  First6in1_HSCPORtbl[i,14] <- odd_ratio_confint[4,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  First6in1_HSCPORtbl[i,15] <- model_pvalues[4]
  
  First6in1_HSCPORtbl[i,16] <- ifelse(First6in1_HSCPORtbl[i,15] < 0.05, 1, 0)
}

First6in1_HSCPORtbl
summary(First6in1model_HSCP)

anova(First6in1model_HSCP, test="LRT")
##Tidy the tbl with rounding
First6in1_HSCPORtbl_tidy = First6in1_HSCPORtbl %>% 
  mutate(OR_2019_PreLD= round (OR_2019_PreLD, digits = 2)) %>% 
  mutate(OR_2019_PreLD_lwr= round (OR_2019_PreLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_upr= round (OR_2019_PreLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_pvalue= round (OR_2019_PreLD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_LD= round (OR_2019_LD, digits = 2)) %>% 
  mutate(OR_2019_LD_lwr= round (OR_2019_LD_lwr, digits = 2)) %>% 
  mutate(OR_2019_LD_upr= round (OR_2019_LD_upr, digits = 2)) %>% 
  mutate(OR_2019_LD_pvalue= round (OR_2019_LD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_PostLD= round (OR_2019_PostLD, digits = 2)) %>% 
  mutate(OR_2019_PostLD_lwr= round (OR_2019_PostLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_upr= round (OR_2019_PostLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_pvalue= round (OR_2019_PostLD_pvalue, digits = 2)) 

###Quick plot if sig or not

First6in1_preLD_sigYN = First6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PreLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses6in1 PreLD")+
  coord_flip()

First6in1_preLD_sigYN

First6in1_LD_sigYN = First6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_LD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses6in1 LD")+
  coord_flip()

First6in1_LD_sigYN

First6in1_postLD_sigYN = First6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PostLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses6in1 Post LD")+
  coord_flip()

First6in1_postLD_sigYN

#########Second 6in1
Section2suptbl2_Second6in1_forloop = Full_seconddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  select(-c(cohort, uptake_16weeks_percent)) %>% 
  group_by(area_name,lockdown.factor) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num)) %>% 
  mutate(unvaccinated = denominator- uptake_16weeks_num) %>% 
  mutate(area_name = as.factor(area_name))
# Create list of HSCP as a character
HSCP_levels <- levels(Section2suptbl2_Second6in1_forloop$area_name)
# Create an empty data frame to store coefficients in
Second6in1_HSCPORtbl <- data.frame(HSCP = HSCP_levels, OR_2019_PreLD = NA, OR_2019_PreLD_lwr = NA, 
                                   OR_2019_PreLD_upr = NA, OR_2019_PreLD_pvalue = NA, OR_2019_PreLD_sig_diff =NA, 
                                   OR_2019_LD = NA, OR_2019_LD_lwr = NA, 
                                   OR_2019_LD_upr = NA, OR_2019_LD_pvalue = NA, OR_2019_LD_sig_diff =NA, OR_2019_PostLD = NA, OR_2019_PostLD_lwr = NA, 
                                   OR_2019_PostLD_upr = NA, OR_2019_PostLD_pvalue = NA, OR_2019_PostLD_sig_diff =NA)

i <- 2

for(i in 1:nrow(Second6in1_HSCPORtbl)){
  # Set reference level
  Section2suptbl2_Second6in1_forloop <- within(Section2suptbl2_Second6in1_forloop, area_name <- relevel(area_name, ref=HSCP_levels[i]))
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_HSCP <- cbind(Section2suptbl2_Second6in1_forloop$uptake_16weeks_num, Section2suptbl2_Second6in1_forloop$unvaccinated)
  # Column for time-period
  tp <- Section2suptbl2_Second6in1_forloop$lockdown.factor
  # Column for Health Board
  HSCP <- Section2suptbl2_Second6in1_forloop$area_name
  
  # Put into GLM with interaction
  Second6in1model_HSCP<- glm(tbl_immun_HSCP ~ tp*HSCP,
                             family="binomial")
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(Second6in1model_HSCP$coefficients)
  odd_ratio_confint <- exp(confint.default(Second6in1model_HSCP))
  model_pvalues <- summary(Second6in1model_HSCP)$coefficients[,4]
  
  # For 2019 vs preLD for HSCP level of interest (ith one), the OR of interest is the tp
  Second6in1_HSCPORtbl[i,2] <- odd_ratios[2]
  Second6in1_HSCPORtbl[i,3] <- odd_ratio_confint[2,1]
  Second6in1_HSCPORtbl[i,4] <- odd_ratio_confint[2,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Second6in1_HSCPORtbl[i,5] <- model_pvalues[2]
  
  Second6in1_HSCPORtbl[i,6] <- ifelse(Second6in1_HSCPORtbl[i,5] < 0.05, 1, 0)
  
  # Repeat for LD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  Second6in1_HSCPORtbl[i,7] <- odd_ratios[3]
  Second6in1_HSCPORtbl[i,8] <- odd_ratio_confint[3,1]
  Second6in1_HSCPORtbl[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Second6in1_HSCPORtbl[i,10] <- model_pvalues[3]
  
  Second6in1_HSCPORtbl[i,11] <- ifelse(Second6in1_HSCPORtbl[i,10] < 0.05, 1, 0)
  # Repeat for PostLD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  Second6in1_HSCPORtbl[i,12] <- odd_ratios[4]
  Second6in1_HSCPORtbl[i,13] <- odd_ratio_confint[4,1]
  Second6in1_HSCPORtbl[i,14] <- odd_ratio_confint[4,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Second6in1_HSCPORtbl[i,15] <- model_pvalues[4]
  
  Second6in1_HSCPORtbl[i,16] <- ifelse(Second6in1_HSCPORtbl[i,15] < 0.05, 1, 0)
}

Second6in1_HSCPORtbl
summary(Second6in1model_HSCP)
anova(Second6in1model_HSCP, test="LRT")
##Tidy the tbl with rounding
Second6in1_HSCPORtbl_tidy = Second6in1_HSCPORtbl %>% 
  mutate(OR_2019_PreLD= round (OR_2019_PreLD, digits = 2)) %>% 
  mutate(OR_2019_PreLD_lwr= round (OR_2019_PreLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_upr= round (OR_2019_PreLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_pvalue= round (OR_2019_PreLD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_LD= round (OR_2019_LD, digits = 2)) %>% 
  mutate(OR_2019_LD_lwr= round (OR_2019_LD_lwr, digits = 2)) %>% 
  mutate(OR_2019_LD_upr= round (OR_2019_LD_upr, digits = 2)) %>% 
  mutate(OR_2019_LD_pvalue= round (OR_2019_LD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_PostLD= round (OR_2019_PostLD, digits = 2)) %>% 
  mutate(OR_2019_PostLD_lwr= round (OR_2019_PostLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_upr= round (OR_2019_PostLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_pvalue= round (OR_2019_PostLD_pvalue, digits = 2)) 

###Quick plot if sig or not

Second6in1_preLD_sigYN = Second6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PreLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second doses6in1 PreLD")+
  coord_flip()

Second6in1_preLD_sigYN

Second6in1_LD_sigYN = Second6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_LD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second doses6in1 LD")+
  coord_flip()

Second6in1_LD_sigYN

Second6in1_postLD_sigYN = Second6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PostLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second doses6in1 Post LD")+
  coord_flip()

Second6in1_postLD_sigYN

#####Third 6in1

Section2suptbl2_Third6in1_forloop = Full_thirddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  select(-c(cohort, uptake_20weeks_percent)) %>% 
  group_by(area_name,lockdown.factor) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num)) %>% 
  mutate(unvaccinated = denominator- uptake_20weeks_num) %>% 
  mutate(area_name = as.factor(area_name))
# Create list of HSCP as a character
HSCP_levels <- levels(Section2suptbl2_Third6in1_forloop$area_name)
# Create an empty data frame to store coefficients in
Third6in1_HSCPORtbl <- data.frame(HSCP = HSCP_levels, OR_2019_PreLD = NA, OR_2019_PreLD_lwr = NA, 
                                  OR_2019_PreLD_upr = NA, OR_2019_PreLD_pvalue = NA, OR_2019_PreLD_sig_diff =NA, 
                                  OR_2019_LD = NA, OR_2019_LD_lwr = NA, 
                                  OR_2019_LD_upr = NA, OR_2019_LD_pvalue = NA, OR_2019_LD_sig_diff =NA, OR_2019_PostLD = NA, OR_2019_PostLD_lwr = NA, 
                                  OR_2019_PostLD_upr = NA, OR_2019_PostLD_pvalue = NA, OR_2019_PostLD_sig_diff =NA)

i <- 2

for(i in 1:nrow(Third6in1_HSCPORtbl)){
  # Set reference level
  Section2suptbl2_Third6in1_forloop <- within(Section2suptbl2_Third6in1_forloop, area_name <- relevel(area_name, ref=HSCP_levels[i]))
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_HSCP <- cbind(Section2suptbl2_Third6in1_forloop$uptake_20weeks_num, Section2suptbl2_Third6in1_forloop$unvaccinated)
  # Column for time-period
  tp <- Section2suptbl2_Third6in1_forloop$lockdown.factor
  # Column for Health Board
  HSCP <- Section2suptbl2_Third6in1_forloop$area_name
  
  # Put into GLM with interaction
  Third6in1model_HSCP<- glm(tbl_immun_HSCP ~ tp*HSCP,
                            family="binomial")
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(Third6in1model_HSCP$coefficients)
  odd_ratio_confint <- exp(confint.default(Third6in1model_HSCP))
  model_pvalues <- summary(Third6in1model_HSCP)$coefficients[,4]
  
  # For 2019 vs preLD for HSCP level of interest (ith one), the OR of interest is the tp
  Third6in1_HSCPORtbl[i,2] <- odd_ratios[2]
  Third6in1_HSCPORtbl[i,3] <- odd_ratio_confint[2,1]
  Third6in1_HSCPORtbl[i,4] <- odd_ratio_confint[2,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Third6in1_HSCPORtbl[i,5] <- model_pvalues[2]
  
  Third6in1_HSCPORtbl[i,6] <- ifelse(Third6in1_HSCPORtbl[i,5] < 0.05, 1, 0)
  
  # Repeat for LD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  Third6in1_HSCPORtbl[i,7] <- odd_ratios[3]
  Third6in1_HSCPORtbl[i,8] <- odd_ratio_confint[3,1]
  Third6in1_HSCPORtbl[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Third6in1_HSCPORtbl[i,10] <- model_pvalues[3]
  
  Third6in1_HSCPORtbl[i,11] <- ifelse(Third6in1_HSCPORtbl[i,10] < 0.05, 1, 0)
  # Repeat for PostLD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  Third6in1_HSCPORtbl[i,12] <- odd_ratios[4]
  Third6in1_HSCPORtbl[i,13] <- odd_ratio_confint[4,1]
  Third6in1_HSCPORtbl[i,14] <- odd_ratio_confint[4,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Third6in1_HSCPORtbl[i,15] <- model_pvalues[4]
  
  Third6in1_HSCPORtbl[i,16] <- ifelse(Third6in1_HSCPORtbl[i,15] < 0.05, 1, 0)
}

Third6in1_HSCPORtbl
summary(Third6in1model_HSCP)
anova(Third6in1model_HSCP, test="LRT")
##Tidy the tbl with rounding
Third6in1_HSCPORtbl_tidy = Third6in1_HSCPORtbl %>% 
  mutate(OR_2019_PreLD= round (OR_2019_PreLD, digits = 2)) %>% 
  mutate(OR_2019_PreLD_lwr= round (OR_2019_PreLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_upr= round (OR_2019_PreLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_pvalue= round (OR_2019_PreLD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_LD= round (OR_2019_LD, digits = 2)) %>% 
  mutate(OR_2019_LD_lwr= round (OR_2019_LD_lwr, digits = 2)) %>% 
  mutate(OR_2019_LD_upr= round (OR_2019_LD_upr, digits = 2)) %>% 
  mutate(OR_2019_LD_pvalue= round (OR_2019_LD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_PostLD= round (OR_2019_PostLD, digits = 2)) %>% 
  mutate(OR_2019_PostLD_lwr= round (OR_2019_PostLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_upr= round (OR_2019_PostLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_pvalue= round (OR_2019_PostLD_pvalue, digits = 2)) 

###Quick plot if sig or not

Third6in1_preLD_sigYN =Third6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PreLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Third doses6in1 PreLD")+
  coord_flip()

Third6in1_preLD_sigYN

Third6in1_LD_sigYN = Third6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_LD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Third doses6in1 LD")+
  coord_flip()

Third6in1_LD_sigYN

Third6in1_postLD_sigYN = Third6in1_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PostLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Third dose 6in1 Post LD")+
  coord_flip()

Third6in1_postLD_sigYN

###First MMR

Section2suptbl2_FirstMMR_forloop = Full_firstdose_MMR %>% 
  select(area_name, cohort, denominator, uptake_13m_num, uptake_13m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  select(-c(cohort, uptake_13m_percent)) %>% 
  group_by(area_name,lockdown.factor) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num)) %>% 
  mutate(unvaccinated = denominator- uptake_13m_num) %>% 
  mutate(area_name = as.factor(area_name))
# Create list of HSCP as a character
HSCP_levels <- levels(Section2suptbl2_FirstMMR_forloop$area_name)
# Create an empty data frame to store coefficients in
FirstMMR_HSCPORtbl <- data.frame(HSCP = HSCP_levels, OR_2019_PreLD = NA, OR_2019_PreLD_lwr = NA, 
                                 OR_2019_PreLD_upr = NA, OR_2019_PreLD_pvalue = NA, OR_2019_PreLD_sig_diff =NA, 
                                 OR_2019_LD = NA, OR_2019_LD_lwr = NA, 
                                 OR_2019_LD_upr = NA, OR_2019_LD_pvalue = NA, OR_2019_LD_sig_diff =NA, OR_2019_PostLD = NA, OR_2019_PostLD_lwr = NA, 
                                 OR_2019_PostLD_upr = NA, OR_2019_PostLD_pvalue = NA, OR_2019_PostLD_sig_diff =NA)

i <- 2

for(i in 1:nrow(FirstMMR_HSCPORtbl)){
  # Set reference level
  Section2suptbl2_FirstMMR_forloop <- within(Section2suptbl2_FirstMMR_forloop, area_name <- relevel(area_name, ref=HSCP_levels[i]))
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_HSCP <- cbind(Section2suptbl2_FirstMMR_forloop$uptake_13m_num, Section2suptbl2_FirstMMR_forloop$unvaccinated)
  # Column for time-period
  tp <- Section2suptbl2_FirstMMR_forloop$lockdown.factor
  # Column for Health Board
  HSCP <- Section2suptbl2_FirstMMR_forloop$area_name
  
  # Put into GLM with interaction
  FirstMMRmodel_HSCP<- glm(tbl_immun_HSCP ~ tp*HSCP,
                           family="binomial")
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(FirstMMRmodel_HSCP$coefficients)
  odd_ratio_confint <- exp(confint.default(FirstMMRmodel_HSCP))
  model_pvalues <- summary(FirstMMRmodel_HSCP)$coefficients[,4]
  
  # For 2019 vs preLD for HSCP level of interest (ith one), the OR of interest is the tp
  FirstMMR_HSCPORtbl[i,2] <- odd_ratios[2]
  FirstMMR_HSCPORtbl[i,3] <- odd_ratio_confint[2,1]
  FirstMMR_HSCPORtbl[i,4] <- odd_ratio_confint[2,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  FirstMMR_HSCPORtbl[i,5] <- model_pvalues[2]
  
  FirstMMR_HSCPORtbl[i,6] <- ifelse(FirstMMR_HSCPORtbl[i,5] < 0.05, 1, 0)
  
  # Repeat for LD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  FirstMMR_HSCPORtbl[i,7] <- odd_ratios[3]
  FirstMMR_HSCPORtbl[i,8] <- odd_ratio_confint[3,1]
  FirstMMR_HSCPORtbl[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  FirstMMR_HSCPORtbl[i,10] <- model_pvalues[3]
  
  FirstMMR_HSCPORtbl[i,11] <- ifelse(FirstMMR_HSCPORtbl[i,10] < 0.05, 1, 0)
  # Repeat for PostLD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  FirstMMR_HSCPORtbl[i,12] <- odd_ratios[4]
  FirstMMR_HSCPORtbl[i,13] <- odd_ratio_confint[4,1]
  FirstMMR_HSCPORtbl[i,14] <- odd_ratio_confint[4,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  FirstMMR_HSCPORtbl[i,15] <- model_pvalues[4]
  
  FirstMMR_HSCPORtbl[i,16] <- ifelse(FirstMMR_HSCPORtbl[i,15] < 0.05, 1, 0)
}

FirstMMR_HSCPORtbl
summary(FirstMMRmodel_HSCP)
anova(FirstMMRmodel_HSCP, test="LRT")
##Tidy the tbl with rounding
FirstMMR_HSCPORtbl_tidy = FirstMMR_HSCPORtbl %>% 
  mutate(OR_2019_PreLD= round (OR_2019_PreLD, digits = 2)) %>% 
  mutate(OR_2019_PreLD_lwr= round (OR_2019_PreLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_upr= round (OR_2019_PreLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_pvalue= round (OR_2019_PreLD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_LD= round (OR_2019_LD, digits = 2)) %>% 
  mutate(OR_2019_LD_lwr= round (OR_2019_LD_lwr, digits = 2)) %>% 
  mutate(OR_2019_LD_upr= round (OR_2019_LD_upr, digits = 2)) %>% 
  mutate(OR_2019_LD_pvalue= round (OR_2019_LD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_PostLD= round (OR_2019_PostLD, digits = 2)) %>% 
  mutate(OR_2019_PostLD_lwr= round (OR_2019_PostLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_upr= round (OR_2019_PostLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_pvalue= round (OR_2019_PostLD_pvalue, digits = 2)) 

###Quick plot if sig or not

FirstMMR_preLD_sigYN = FirstMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PreLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses MMR PreLD")+
  coord_flip()

FirstMMR_preLD_sigYN

FirstMMR_LD_sigYN = FirstMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_LD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses MMR LD")+
  coord_flip()

FirstMMR_LD_sigYN

FirstMMR_postLD_sigYN = FirstMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PostLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "First doses MMR Post LD")+
  coord_flip()

FirstMMR_postLD_sigYN

#####Second MMR


Section2suptbl2_SecondMMR_forloop = Full_seconddose_MMR %>% 
  select(area_name, cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) %>% 
  select(-c(cohort, uptake_3y5m_percent)) %>% 
  group_by(area_name,lockdown.factor) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num)) %>% 
  mutate(unvaccinated = denominator- uptake_3y5m_num) %>% 
  mutate(area_name = as.factor(area_name))
# Create list of HSCP as a character
HSCP_levels <- levels(Section2suptbl2_SecondMMR_forloop$area_name)
# Create an empty data frame to store coefficients in
SecondMMR_HSCPORtbl <- data.frame(HSCP = HSCP_levels, OR_2019_PreLD = NA, OR_2019_PreLD_lwr = NA, 
                                  OR_2019_PreLD_upr = NA, OR_2019_PreLD_pvalue = NA, OR_2019_PreLD_sig_diff =NA, 
                                  OR_2019_LD = NA, OR_2019_LD_lwr = NA, 
                                  OR_2019_LD_upr = NA, OR_2019_LD_pvalue = NA, OR_2019_LD_sig_diff =NA, OR_2019_PostLD = NA, OR_2019_PostLD_lwr = NA, 
                                  OR_2019_PostLD_upr = NA, OR_2019_PostLD_pvalue = NA, OR_2019_PostLD_sig_diff =NA)

i <- 2

for(i in 1:nrow(SecondMMR_HSCPORtbl)){
  # Set reference level
  Section2suptbl2_SecondMMR_forloop <- within(Section2suptbl2_SecondMMR_forloop, area_name <- relevel(area_name, ref=HSCP_levels[i]))
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_HSCP <- cbind(Section2suptbl2_SecondMMR_forloop$uptake_3y5m_num, Section2suptbl2_SecondMMR_forloop$unvaccinated)
  # Column for time-period
  tp <- Section2suptbl2_SecondMMR_forloop$lockdown.factor
  # Column for Health Board
  HSCP <- Section2suptbl2_SecondMMR_forloop$area_name
  
  # Put into GLM with interaction
  SecondMMRmodel_HSCP<- glm(tbl_immun_HSCP ~ tp*HSCP,
                            family="binomial")
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(SecondMMRmodel_HSCP$coefficients)
  odd_ratio_confint <- exp(confint.default(SecondMMRmodel_HSCP))
  model_pvalues <- summary(SecondMMRmodel_HSCP)$coefficients[,4]
  
  # For 2019 vs preLD for HSCP level of interest (ith one), the OR of interest is the tp
  SecondMMR_HSCPORtbl[i,2] <- odd_ratios[2]
  SecondMMR_HSCPORtbl[i,3] <- odd_ratio_confint[2,1]
  SecondMMR_HSCPORtbl[i,4] <- odd_ratio_confint[2,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  SecondMMR_HSCPORtbl[i,5] <- model_pvalues[2]
  
  SecondMMR_HSCPORtbl[i,6] <- ifelse(SecondMMR_HSCPORtbl[i,5] < 0.05, 1, 0)
  
  # Repeat for LD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  SecondMMR_HSCPORtbl[i,7] <- odd_ratios[3]
  SecondMMR_HSCPORtbl[i,8] <- odd_ratio_confint[3,1]
  SecondMMR_HSCPORtbl[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  SecondMMR_HSCPORtbl[i,10] <- model_pvalues[3]
  
  SecondMMR_HSCPORtbl[i,11] <- ifelse(SecondMMR_HSCPORtbl[i,10] < 0.05, 1, 0)
  # Repeat for PostLD
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  SecondMMR_HSCPORtbl[i,12] <- odd_ratios[4]
  SecondMMR_HSCPORtbl[i,13] <- odd_ratio_confint[4,1]
  SecondMMR_HSCPORtbl[i,14] <- odd_ratio_confint[4,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  SecondMMR_HSCPORtbl[i,15] <- model_pvalues[4]
  
  SecondMMR_HSCPORtbl[i,16] <- ifelse(SecondMMR_HSCPORtbl[i,15] < 0.05, 1, 0)
}

SecondMMR_HSCPORtbl
summary(SecondMMRmodel_HSCP)
anova(SecondMMRmodel_HSCP, test="LRT")
##Tidy the tbl with rounding
SecondMMR_HSCPORtbl_tidy = SecondMMR_HSCPORtbl %>% 
  mutate(OR_2019_PreLD= round (OR_2019_PreLD, digits = 2)) %>% 
  mutate(OR_2019_PreLD_lwr= round (OR_2019_PreLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_upr= round (OR_2019_PreLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PreLD_pvalue= round (OR_2019_PreLD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_LD= round (OR_2019_LD, digits = 2)) %>% 
  mutate(OR_2019_LD_lwr= round (OR_2019_LD_lwr, digits = 2)) %>% 
  mutate(OR_2019_LD_upr= round (OR_2019_LD_upr, digits = 2)) %>% 
  mutate(OR_2019_LD_pvalue= round (OR_2019_LD_pvalue, digits = 2)) %>% 
  mutate(OR_2019_PostLD= round (OR_2019_PostLD, digits = 2)) %>% 
  mutate(OR_2019_PostLD_lwr= round (OR_2019_PostLD_lwr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_upr= round (OR_2019_PostLD_upr, digits = 2)) %>% 
  mutate(OR_2019_PostLD_pvalue= round (OR_2019_PostLD_pvalue, digits = 2)) 

###Quick plot if sig or not

SecondMMR_preLD_sigYN = SecondMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PreLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second dose MMR PreLD")+
  coord_flip()

SecondMMR_preLD_sigYN

SecondMMR_LD_sigYN = SecondMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_LD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second doses MMR LD")+
  coord_flip()

SecondMMR_LD_sigYN

SecondMMR_postLD_sigYN = SecondMMR_HSCPORtbl_tidy %>%
  ggplot(aes(x=HSCP, y=OR_2019_PostLD_sig_diff)) +
  geom_point()+
  theme_bw()+
  labs(x = NULL,
       y = "Sig",
       title = "Second doses MMR Post LD")+
  coord_flip()

SecondMMR_postLD_sigYN

##########My log regression code, at present baseline HSCP is Aberdeen city by default
##Set up Full section tbl first

###Start loop code
First6in1_HSCP_forloop = FullSection2suptbl2_First6in1 %>% 
  select(area_name, denominator, uptake_12weeks_num, time_period) %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  select(area_name, uptake_12weeks_num, unvaccinated, time_period) %>% 
  mutate(area.factor = area_name) %>% 
  mutate(time.factor = time_period) %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Scottish Borders"))

tp <- First6in1_HSCP_forloop$time.factor
HSCP <- First6in1_HSCP_forloop$area.factor

##Proceed

First6in1_HSCP_loopregression_tbl = cbind(First6in1_HSCP_forloop$uptake_12weeks_num, First6in1_HSCP_forloop$unvaccinated)

model_first6in1_HSCP_loop = glm(First6in1_HSCP_loopregression_tbl ~ tp,
                                family="binomial")

summary(model_first6in1_HSCP_loop)
exp(model_first6in1_HSCP_loop$coefficients)
exp(confint(model_first6in1_HSCP_loop)) 

model_first6in1_HSCP_loopHSCPonly = glm(First6in1_HSCP_loopregression_tbl ~ HSCP,
                                        family="binomial")

summary(model_first6in1_HSCP_loopHSCPonly)
exp(model_first6in1_HSCP_loopHSCPonly$coefficients)
exp(confint(model_first6in1_HSCP_loopHSCPonly)) 
#Add interaction of HSCP
model_first6in1_HSCPinteraction_loop = glm(First6in1_HSCP_loopregression_tbl ~ tp*HSCP,
                                           family="binomial")

summary(model_first6in1_HSCPinteraction_loop)
exp(model_first6in1_HSCPinteraction_loop$coefficients)
exp(confint(model_first6in1_HSCPinteraction_loop)) 

library(broom)
First6in1_HSCPinteractionmodel_tbl = model_first6in1_HSCPinteraction_loop%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE)) %>% 
  mutate(HSCP = "Glasgow City")
First6in1_HSCPinteractionmodel_tbl
First6in1_HSCPinteractionmodel_tblLD = First6in1_HSCPinteractionmodel_tbl %>% 
  filter(str_detect(term,"tpLD", negate = FALSE))

#Plot- baseline = Moray
#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("tpLD", "Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Highland", "Inverclyde", "Midlothian", "Moray", "North Ayrshire", "North L'shire", "Orkney ISlands", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland Islands", "South Ayrshire", "South L'shire", "West D'shire", "Western Isles", "West Lothian")

First6in1_HSCPinteractionmodel_tblLDplot <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(First6in1_HSCPinteractionmodel_tblLD$OR),
  boxCILow = c(First6in1_HSCPinteractionmodel_tblLD$upperCI),
  boxCIHigh = c(First6in1_HSCPinteractionmodel_tblLD$lowerCI))

First6in1_HSCPinteractionmodel_tblLDforest <- ggplot(First6in1_HSCPinteractionmodel_tblLDplot, aes(x = boxOdds, y = boxLabelsHSCPLD))
First6in1_HSCPinteractionmodel_tblLDforest = First6in1_HSCPinteractionmodel_tblLDforest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "ROR compared to Glasgow City",
       y = NULL,
       title = NULL)
First6in1_HSCPinteractionmodel_tblLDforest

anova(model_first6in1_HSCPinteraction_loop, test="LRT")

library(broom)
First6in1_HSCPinteractionmodel_tbl = model_first6in1_HSCPinteraction_loop%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE)) %>% 
  mutate(HSCP = "Glasgow City")
First6in1_HSCPinteractionmodel_tbl
First6in1_HSCPinteractionmodel_tblLD = First6in1_HSCPinteractionmodel_tbl %>% 
  filter(str_detect(term,"tpLD", negate = FALSE))
First6in1_HSCPinteractionmodel_tblPreLD = First6in1_HSCPinteractionmodel_tbl %>% 
  filter(str_detect(term,"tpPreLD", negate = FALSE))
First6in1_HSCPinteractionmodel_tblPostLD = First6in1_HSCPinteractionmodel_tbl %>% 
  filter(str_detect(term,"tpPostLD", negate = FALSE))



###Try to add data from tidy loop tables (need to run loop script Second)
#change colnames
colnames(FullSection2suptbl2_SecondMMR) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
SecondMMR_HSCPORtbl_tidypreLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
SecondMMR_HSCPORtbl_tidyLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
SecondMMR_HSCPORtbl_tidyPostLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR, SecondMMR_HSCPORtbl_tidypreLD)
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR_OR, SecondMMR_HSCPORtbl_tidyLD)
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR_OR, SecondMMR_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_SecondMMR_OR, file = "Exported tables/FullSection2suptbl2_SecondMMR_OR.csv")

#######################
##Figure 2 HSCP data by LD period
##All vaccines
##Data extracted from dashboard 3rd Feb 2021

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(geojsonio)
library(rgdal)
library(tmap)
library(viridis)

phs_main <- rgb(67,53,139, maxColorValue = 255)
phs_purple<- rgb(150,64,145, maxColorValue = 255)
phs_purple2 <- rgb(208,145,205, maxColorValue = 255)
phs_spec2 <- colorRampPalette(c(phs_main, phs_purple2))


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))####excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

###Change island names for all at the beginning
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)

Full_seconddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_6in1$area_name)
Full_seconddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_6in1$area_name)

Full_thirddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_thirddose_6in1$area_name)
Full_thirddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_thirddose_6in1$area_name)

Full_firstdose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_MMR$area_name)
Full_firstdose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_MMR$area_name)

Full_seconddose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_MMR$area_name)
Full_seconddose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_MMR$area_name)

#Load HSCP map from https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=714BD98E15D22A8116824CA25B30DC02#/metadata/ac5e870f-8fc2-4c21-8b9c-3bd2311a583f
HSCP_map <- readOGR(here("Data"), layer="SG_NHS_IntegrationAuthority_2019", verbose=FALSE)

##Select out percentage uptake for 2019 for HSPC. Remove NHS boards and Scotland

#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map


#First 6in1 LD
HSCP_LDdata_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
##Need to get mean data for each HSCP then plot as above

#First dose MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

newPolyMMR <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPolyMMR)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% vacinnated")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map


##Plot percent change
#First 6in1
#Set up the percent change table by selecting out the 2019 and LD data into tables then join
#2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

First6in1_HSCP_2019_percent = HSCP_2019data_First6in1 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
First6in1_HSCP_LD_percent = Full_firstdose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data
First6in1_Islands_LD_percent = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

First6in1_Islands_LD_percent = First6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
First6in1_HSCP_LD_percent = rbind(First6in1_HSCP_LD_percent, First6in1_Islands_LD_percent)

#Join 2019 and LD tbls
First6in1_HSPC_percentchange_2019andLD = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city and Scottish borders

First6in1_HSPC_percentchange_2019andLD = First6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)
##Select out signifcnat changes only

First6in1_HSPC_percentchange_2019andLD_SIGONLY = First6in1_HSPC_percentchange_2019andLD %>% 
  filter(area_name %in% c("Clackmannanshire and Stirling", "East Dunbartonshire", "Edinburgh", "Falkirk", "Fife", "Glasgow City", "South Ayrshire", "South Lanarkshire"))

#Plot percent change 

library(tmap)
library(viridis)
First6in1_percentchange_poly <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map = tm_shape(First6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map

#For significant changes only 
First6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map_sig = tm_shape(First6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map_sig

#Bar chart plot
library(RColorBrewer)
First6in1_HSPC_percentchange_bar = First6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
First6in1_HSPC_percentchange_bar

#Second 6in1
#2010
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Second6in1_HSCP_2019_percent = HSCP_2019data_Second6in1 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
Second6in1_HSCP_LD_percent = Full_seconddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Second6in1_Islands_LD_percent = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Second6in1_Islands_LD_percent = Second6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Second6in1_HSCP_LD_percent = rbind(Second6in1_HSCP_LD_percent,Second6in1_Islands_LD_percent)

#Join 2019 and LD
Second6in1_HSPC_percentchange_2019andLD = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)  

Second6in1_HSPC_percentchange_2019andLD = Second6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Second6in1_percentchange_poly <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map = tm_shape(Second6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map

#For significant changes only 

Second6in1_HSPC_percentchange_2019andLD_SIGONLY = Second6in1_HSPC_percentchange_2019andLD %>% 
  filter(area_name %in% c("Argyll and Bute", "Clackmannanshire and Stirling", "Dumfries and Galloway", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East Renfrewshire", "Edinburgh", "Falkirk", "Fife", "Glasgow City", "Highland", "North Lanarkshire", "Orkney Islands", "Renfrewshire", "Shetland Islands",  "South Lanarkshire", "Westh Dunbartonshire", "West Lothian"))
Second6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map_sig = tm_shape(Second6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map_sig

#Bar chart plot
Second6in1_HSPC_percentchange_bar = Second6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Second6in1_HSPC_percentchange_bar
#Third 6in1

HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Third6in1_HSCP_2019_percent = HSCP_2019data_Third6in1 %>% 
  select(area_name, uptake_20weeks_percent)
colnames(Third6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")

Third6in1_HSCP_LD_percent = Full_thirddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Third6in1_Islands_LD_percent = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Third6in1_Islands_LD_percent = Third6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Third6in1_HSCP_LD_percent = rbind(Third6in1_HSCP_LD_percent,Third6in1_Islands_LD_percent)
#Join 2019 and LD

Third6in1_HSPC_percentchange_2019andLD = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)  

Third6in1_HSPC_percentchange_2019andLD = Third6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Third6in1_percentchange_poly <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map = tm_shape(Third6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map

#For significant changes only 

Third6in1_HSPC_percentchange_2019andLD_SIGONLY = Third6in1_HSPC_percentchange_2019andLD %>% 
  filter(!(area_name %in% c("Moray", "Inverclyde", "Angus", "Aberdeenshire")))
Third6in1_percentchange_poly_sig <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map_sig = tm_shape(Third6in1_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title =  "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map_sig

#Bar chart plot
Third6in1_HSPC_percentchange_bar = Third6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Third6in1_HSPC_percentchange_bar

#First MMR
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

FirstMMR_HSCP_2019_percent = HSCP_2019data_FirstMMR %>% 
  select(area_name, uptake_13m_percent)
colnames(FirstMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

FirstMMR_HSCP_LD_percent = Full_firstdose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
FirstMMR_Islands_LD_percent = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

FirstMMR_Islands_LD_percent = FirstMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
FirstMMR_HSCP_LD_percent = rbind(FirstMMR_HSCP_LD_percent,FirstMMR_Islands_LD_percent)
#Join 2019 and LD
FirstMMR_HSPC_percentchange_2019andLD = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city 

FirstMMR_HSPC_percentchange_2019andLD = FirstMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
FirstMMR_percentchange_poly <- merge(x=HSCP_map, y=FirstMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_MMR_percentchange_map = tm_shape(FirstMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First MMR % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_percentchange_map ##NOte change in pallet as none fell. Cross checked for Moray +++++all theses results were sigfnicant 

First_MMR_percentchange_map_sig = First_MMR_percentchange_map
First_MMR_percentchange_map_sig
#Bar chart plot
FirstMMR_HSPC_percentchange_bar = FirstMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
FirstMMR_HSPC_percentchange_bar

#Second MMR ++++excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

SecondMMR_HSCP_2019_percent = HSCP_2019data_SecondMMR %>% 
  select(area_name, uptake_3y5m_percent)
colnames(SecondMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

SecondMMR_HSCP_LD_percent = Full_seconddose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
SecondMMR_Islands_LD_percent = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

SecondMMR_Islands_LD_percent = SecondMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
SecondMMR_HSCP_LD_percent = rbind(SecondMMR_HSCP_LD_percent,SecondMMR_Islands_LD_percent)
#Join 2019 and LD

SecondMMR_HSPC_percentchange_2019andLD = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)  

SecondMMR_HSPC_percentchange_2019andLD = SecondMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
SecondMMR_percentchange_poly <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map = tm_shape(SecondMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR % change from 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map

#For significant changes only 

SecondMMR_HSPC_percentchange_2019andLD_SIGONLY = SecondMMR_HSPC_percentchange_2019andLD %>% 
  filter(!(area_name %in% c("East Ayrshire", "Argyll and Bute")))

SecondMMR_percentchange_poly_sig <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD_SIGONLY, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map_sig = tm_shape(SecondMMR_percentchange_poly_sig)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% point change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR % change from 2019 (stat sig)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map_sig

#Bar chart plot
SecondMMR_HSPC_percentchange_bar = SecondMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
SecondMMR_HSPC_percentchange_bar

##Export the % change maps and bar charts in one pdf
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Percent_change_bar_HSCP_6in1 = ggarrange(First6in1_HSPC_percentchange_bar, Second6in1_HSPC_percentchange_bar, Third6in1_HSPC_percentchange_bar,
                                         labels = c("First6in1", "Second6in1", "Third6in1"), hjust = -1,
                                         legend=NULL,
                                         ncol = 2, nrow = 2)

Percent_change_bar_HSCP_6in1

Percent_change_bar_HSCP_MMR = ggarrange(FirstMMR_HSPC_percentchange_bar, SecondMMR_HSPC_percentchange_bar, 
                                        labels = c("First MMR","Second MMR"), hjust = -1.5,
                                        legend=NULL,
                                        ncol = 2, nrow = 1)

Percent_change_bar_HSCP_MMR


##To get a grouped bar chart

#Grouped bar chart LD vs 2019 percent uptake First 6in1

First6in1_HSCP_2019_percent = First6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(First6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(First6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

First6in1_Absolute_percentage = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)

Grouped_First6in1_2019LDpercentages = First6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Perth and Kinross", "Glasgow City", "Dundee City", "Highland", "West Dunbartonshire", "Edinburgh", "Orkney Islands", "Angus", "Aberdeen City","Argyll and Bute", "Clackmannanshire and Stirling", "Falkirk", "Midlothian", "Scottish Borders", "Dumfries and Galloway", "East Lothian", "West Lothian", "North Lanarkshire", "Fife", "Shetland Islands", "Western Isles", "Moray", "Renfrewshire", "South Lanarkshire", "East Dunbartonshire", "South Ayrshire", "Aberdeenshire", "North Ayrshire", "East Ayrshire", "East Renfrewshire", "Inverclyde")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 93.9, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 94.9, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_First6in1_2019LDpercentages


#Grouped bar chart LD vs 2019 percent uptake Second 6in1

Second6in1_HSCP_2019_percent = Second6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Second6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Second6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Second6in1_Absolute_percentage = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)

Grouped_Second6in1_2019LDpercentages = Second6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Dundee City", "Glasgow City", "West Dunbartonshire", "Highland", "Edinburgh", "Argyll and Bute", "Perth and Kinross", "Dumfries and Galloway", "Falkirk", "Angus", "West Lothian", "Midlothian", "East Lothian", "Aberdeen City", "Clackmannanshire and Stirling", "Orkney Islands", "Scottish Borders", "North Ayrshire", "Fife", "North Lanarkshire", "Renfrewshire", "South Lanarkshire", "East Dunbartonshire", "East Ayrshire", "Western Isles", "Shetland Islands", "Moray", "East Renfrewshire", "Inverclyde", "South Ayrshire", "Aberdeenshire")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 84.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 89.7, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_Second6in1_2019LDpercentages


#Grouped bar chart LD vs 2019 percent uptake Third 6in1

Third6in1_HSCP_2019_percent = Third6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Third6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Third6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Third6in1_Absolute_percentage = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)

Grouped_Third6in1_2019LDpercentages = Third6in1_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area, "Falkirk", "Dundee City", "West Lothian", "Highland", "West Dunbartonshire", "Edinburgh", "Dumfries and Galloway", "Glasgow City", "Midlothian", "Argyll and Bute", "Perth and Kinross", "East Lothian", "North Ayrshire", "Angus", "Clackmannanshire and Stirling", "Aberdeen City", "Western Isles", "North Lanarkshire", "Fife", "Orkney Islands", "Scottish Borders", "South Lanarkshire", "Shetland Islands", "Renfrewshire", "East Ayrshire", "East Dunbartonshire", "Moray", "South Ayrshire", "Inverclyde", "East Renfrewshire", "Aberdeenshire")) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 73, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 82.1, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_Third6in1_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake First MMR

FirstMMR_HSCP_2019_percent = FirstMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(FirstMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(FirstMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

FirstMMR_Absolute_percentage = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)

Grouped_FirstMMR_2019LDpercentages = FirstMMR_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area,"Shetland Islands", "Aberdeenshire", "Western Isles", "Orkney Islands", "Moray", "Aberdeen City", "North Ayrshire", "Highland", "East Ayrshire", "Dundee City", "South Ayrshire", "Argyll and Bute", "Midlothian", "West Dunbartonshire", "Perth and Kinross", "West Lothian", "Fife", "Glasgow City", "Scottish Borders", "Edinburgh", "Falkirk", "Renfrewshire", "Angus", "North Lanarkshire", "South Lanarkshire", "Clackmannanshire and Stirling", "East Lothian", "Dumfries and Galloway", "East Renfrewshire", "Inverclyde", "East Dunbartonshire")) %>% 
  ggplot(aes(fill=`Time period`, y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 65.2, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 78.4, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_FirstMMR_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake Second MMR

SecondMMR_HSCP_2019_percent = SecondMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(SecondMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(SecondMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

SecondMMR_Absolute_percentage = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)

Grouped_SecondMMR_2019LDpercentages = SecondMMR_Absolute_percentage %>%
  mutate(Area = fct_relevel(Area,"Shetland Islands", "Dundee City", "Orkney Islands", "North Ayrshire","East Ayrshire", "South Ayrshire", "Angus", "Perth and Kinross", "Western Isles", "Fife", "Falkirk", "Highland", "Glasgow City", "Clackmannanshire and Stirling", "North Lanarkshire", "West Dunbartonshire", "South Lanarkshire", "Scottish Borders", "Edinburgh", "East Dunbartonshire", "Argyll and Bute", "Renfrewshire", "West Lothian", "East Lothian", "Midlothian", "Inverclyde", "East Renfrewshire", "Dumfries and Galloway")) %>%   ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 51.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 66.1, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(breaks = seq(0,100,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Grouped_SecondMMR_2019LDpercentages

###Export grouped bar charts as one figure
Grouped_bar_HSCP_LDvs2019percentages_6in1 = ggarrange(Grouped_First6in1_2019LDpercentages,Grouped_Second6in1_2019LDpercentages, Grouped_Third6in1_2019LDpercentages,
                                                      common.legend = TRUE, legend="bottom",
                                                      labels = NULL,
                                                      ncol = 3, nrow = 1)
Grouped_bar_HSCP_LDvs2019percentages_6in1

Grouped_bar_HSCP_LDvs2019percentages_MMR = ggarrange(Grouped_FirstMMR_2019LDpercentages, Grouped_SecondMMR_2019LDpercentages,
                                                     common.legend = TRUE, legend="bottom",
                                                     labels = NULL,
                                                     ncol = 2, nrow = 1)
Grouped_bar_HSCP_LDvs2019percentages_MMR

Grouped_bar_HSCP_LDvs2019percentages_allvaccine = ggarrange(Grouped_bar_HSCP_LDvs2019percentages_6in1, Grouped_bar_HSCP_LDvs2019percentages_MMR,
                                                            legend = NULL,
                                                            labels = c("A", "B"),
                                                            ncol = 1, nrow = 2)
Grouped_bar_HSCP_LDvs2019percentages_allvaccine

Alt_Grouped_bar_HSCP_LDvs2019percentages_allvaccine = ggarrange(Grouped_First6in1_2019LDpercentages,Grouped_Second6in1_2019LDpercentages, Grouped_Third6in1_2019LDpercentages,Grouped_FirstMMR_2019LDpercentages, Grouped_SecondMMR_2019LDpercentages,
                                                                common.legend = TRUE, legend="bottom",
                                                                labels = NULL,
                                                                ncol = 2, nrow = 3)
Alt_Grouped_bar_HSCP_LDvs2019percentages_allvaccine

####Logistic regression analysis of uptake rates between HSCP for 2019 and LD
##First 6in1 Log regression
#Fisrt 6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

First6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_First6in1$uptake_12weeks_num, HSCP_2019data_First6in1$unvaccinated)

model_first6in1_HSCP_2019 = glm(First6in1_HSCP2019_regression_tbl ~ HSCP_2019data_First6in1$area.factor,
                                family="binomial")

summary(model_first6in1_HSCP_2019)

exp(model_first6in1_HSCP_2019$coefficients)
exp(confint(model_first6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
First6in1_HSCP2019model_tbl = model_first6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#First 6in1 plot
ORandCI_first6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(First6in1_HSCP2019model_tbl$OR),
  boxCILow = c(First6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(First6in1_HSCP2019model_tbl$lowerCI))

First6in1_HSCP2019_forest <- ggplot(ORandCI_first6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
First6in1_HSCP2019_forest = First6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

First6in1_HSCP2019_forest

anova(model_first6in1_HSCP_2019, test="LRT")

##First 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get he correct results ? as using agregate data

#Set up tbl with vaccinated and unvaccinated factors

HSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  select(area.factor, uptake_12weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_First6in1 = SummaryHSCP_LDdata_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirst6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_First6in1$total_vaccinated, SummaryHSCP_LDdata_First6in1$total_unvaccinated)

Summarymodel_first6in1_HSCP_LD = glm(SummaryFirst6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_First6in1$area.factor,
                                     family="binomial")

summary(Summarymodel_first6in1_HSCP_LD)

exp(Summarymodel_first6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_first6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirst6in1_HSCPLDmodel_tbl = Summarymodel_first6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_first6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirst6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirst6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirst6in1_HSCPLDmodel_tbl$lowerCI))

SummaryFirst6in1_HSCPLD_forest <- ggplot(SummaryORandCI_first6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirst6in1_HSCPLD_forest = SummaryFirst6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirst6in1_HSCPLD_forest

anova(Summarymodel_first6in1_HSCP_LD, test="LRT")

#Export First 6in1 plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_6in1_OR_plots = ggarrange(First6in1_HSCP2019_forest, SummaryFirst6in1_HSCPLD_forest,
                                labels = c("First6in12019","First6in1LD" ),hjust = -1.5,
                                legend = NULL,
                                ncol = 2, nrow = 1)
First_6in1_OR_plots

#####Second 6in1 log regression
##Second6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Second6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Second6in1$uptake_16weeks_num, HSCP_2019data_Second6in1$unvaccinated)

model_second6in1_HSCP_2019 = glm(Second6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Second6in1$area.factor,
                                 family="binomial")

summary(model_second6in1_HSCP_2019)

exp(model_second6in1_HSCP_2019$coefficients)
exp(confint(model_second6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Second6in1_HSCP2019model_tbl = model_second6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second 6in1 2019 plot
ORandCI_second6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Second6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Second6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Second6in1_HSCP2019model_tbl$lowerCI))

Second6in1_HSCP2019_forest <- ggplot(ORandCI_second6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Second6in1_HSCP2019_forest = Second6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

Second6in1_HSCP2019_forest

anova(model_second6in1_HSCP_2019, test="LRT")

##Second 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  select(area.factor, uptake_16weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_16weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Second6in1 = SummaryHSCP_LDdata_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecond6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Second6in1$total_vaccinated, SummaryHSCP_LDdata_Second6in1$total_unvaccinated)

Summarymodel_second6in1_HSCP_LD = glm(SummarySecond6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Second6in1$area.factor,
                                      family="binomial")

summary(Summarymodel_second6in1_HSCP_LD)

exp(Summarymodel_second6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_second6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecond6in1_HSCPLDmodel_tbl = Summarymodel_second6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_second6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummarySecond6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecond6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecond6in1_HSCPLDmodel_tbl$lowerCI))

SummarySecond6in1_HSCPLD_forest <- ggplot(SummaryORandCI_second6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummarySecond6in1_HSCPLD_forest = SummarySecond6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecond6in1_HSCPLD_forest

anova(Summarymodel_second6in1_HSCP_LD, test="LRT")
#Export Second 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_6in1_OR_plots = ggarrange(Second6in1_HSCP2019_forest, SummarySecond6in1_HSCPLD_forest,
                                 labels = c("Second6in1 2019","Second6in1 LD" ),hjust = -0.75,
                                 legend = NULL,
                                 ncol = 2, nrow = 1)
Second_6in1_OR_plots

#####THird 6in1 log regression
##Third6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE- for this vaccine, midlothian uptake is lower than SCottish mean

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Third6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Third6in1$uptake_20weeks_num, HSCP_2019data_Third6in1$unvaccinated)

model_third6in1_HSCP_2019 = glm(Third6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Third6in1$area.factor,
                                family="binomial")

summary(model_third6in1_HSCP_2019)

exp(model_third6in1_HSCP_2019$coefficients)
exp(confint(model_third6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Third6in1_HSCP2019model_tbl = model_third6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls 
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Third 6in1 2019 plot
ORandCI_third6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Third6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Third6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Third6in1_HSCP2019model_tbl$lowerCI))

Third6in1_HSCP2019_forest <- ggplot(ORandCI_third6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Third6in1_HSCP2019_forest = Third6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
Third6in1_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Third 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  select(area.factor, uptake_20weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_20weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Third6in1 = SummaryHSCP_LDdata_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryThird6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Third6in1$total_vaccinated, SummaryHSCP_LDdata_Third6in1$total_unvaccinated)

Summarymodel_third6in1_HSCP_LD = glm(SummaryThird6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Third6in1$area.factor,
                                     family="binomial")

summary(Summarymodel_third6in1_HSCP_LD)

exp(Summarymodel_third6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_third6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryThird6in1_HSCPLDmodel_tbl = Summarymodel_third6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_third6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryThird6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryThird6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryThird6in1_HSCPLDmodel_tbl$lowerCI))

SummaryThird6in1_HSCPLD_forest <- ggplot(SummaryORandCI_third6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryThird6in1_HSCPLD_forest = SummaryThird6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryThird6in1_HSCPLD_forest

anova(Summarymodel_third6in1_HSCP_LD, test="LRT")
#Export Third 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Third_6in1_OR_plots = ggarrange(Third6in1_HSCP2019_forest, SummaryThird6in1_HSCPLD_forest,
                                labels = c("Third6in1 2019","Third6in1 LD" ),hjust = -0.75,
                                legend = NULL,
                                ncol = 2, nrow = 1)
Third_6in1_OR_plots

#####First MMR log regression
##First MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

FirstMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_FirstMMR$uptake_13m_num, HSCP_2019data_FirstMMR$unvaccinated)

model_firstMMR_HSCP_2019 = glm(FirstMMR_HSCP2019_regression_tbl ~ HSCP_2019data_FirstMMR$area.factor,
                               family="binomial")

summary(model_firstMMR_HSCP_2019)

exp(model_firstMMR_HSCP_2019$coefficients)
exp(confint(model_firstMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
FirstMMR_HSCP2019model_tbl = model_firstMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#FirstMMR 2019 plot
ORandCI_firstMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(FirstMMR_HSCP2019model_tbl$OR),
  boxCILow = c(FirstMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(FirstMMR_HSCP2019model_tbl$lowerCI))

FirstMMR_HSCP2019_forest <- ggplot(ORandCI_firstMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
FirstMMR_HSCP2019_forest = FirstMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
FirstMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##FirstMMR LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  select(area.factor, uptake_13m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_13m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_FirstMMR = SummaryHSCP_LDdata_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirstMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_FirstMMR$total_vaccinated, SummaryHSCP_LDdata_FirstMMR$total_unvaccinated)

Summarymodel_firstMMR_HSCP_LD = glm(SummaryFirstMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_FirstMMR$area.factor,
                                    family="binomial")

summary(Summarymodel_firstMMR_HSCP_LD)

exp(Summarymodel_firstMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_firstMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirstMMR_HSCPLDmodel_tbl = Summarymodel_firstMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_firstMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirstMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirstMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirstMMR_HSCPLDmodel_tbl$lowerCI))

SummaryFirstMMR_HSCPLD_forest <- ggplot(SummaryORandCI_firstMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirstMMR_HSCPLD_forest = SummaryFirstMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirstMMR_HSCPLD_forest

anova(Summarymodel_firstMMR_HSCP_LD, test="LRT")
#Export FirstMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_MMR_OR_plots = ggarrange(FirstMMR_HSCP2019_forest, SummaryFirstMMR_HSCPLD_forest,
                               labels = c("FirstMMR 2019","FirstMMR LD" ),hjust = -1,
                               legend = NULL,
                               ncol = 2, nrow = 1)
First_MMR_OR_plots


#####Second MMR log regression
##Second MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE Midlothian is average avergae for this dose

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

SecondMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_SecondMMR$uptake_3y5m_num, HSCP_2019data_SecondMMR$unvaccinated)

model_secondMMR_HSCP_2019 = glm(SecondMMR_HSCP2019_regression_tbl ~ HSCP_2019data_SecondMMR$area.factor,
                                family="binomial")

summary(model_secondMMR_HSCP_2019)

exp(model_secondMMR_HSCP_2019$coefficients)
exp(confint(model_secondMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
SecondMMR_HSCP2019model_tbl = model_secondMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls+++NOTE removed NHS Grampian areas
boxLabelsHSCPsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second MMR 2019 plot
ORandCI_secondMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCPsecondMMR):1,
  boxOdds = c(SecondMMR_HSCP2019model_tbl$OR),
  boxCILow = c(SecondMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(SecondMMR_HSCP2019model_tbl$lowerCI))

SecondMMR_HSCP2019_forest <- ggplot(ORandCI_secondMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCPsecondMMR))
SecondMMR_HSCP2019_forest = SecondMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
SecondMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Second MMR LD2020 
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD data table as not done above

HSCP_LDdata_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  select(area.factor, uptake_3y5m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_3y5m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_SecondMMR = SummaryHSCP_LDdata_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecondMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_SecondMMR$total_vaccinated, SummaryHSCP_LDdata_SecondMMR$total_unvaccinated)

Summarymodel_secondMMR_HSCP_LD = glm(SummarySecondMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_SecondMMR$area.factor,
                                     family="binomial")

summary(Summarymodel_secondMMR_HSCP_LD)

exp(Summarymodel_secondMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_secondMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecondMMR_HSCPLDmodel_tbl = Summarymodel_secondMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls ####NOTe removing NHS Grampian areas as above
boxLabelsHSCPLDsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney and NHS Grampian

SummaryORandCI_secondMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLDsecondMMR):1,
  boxOdds = c(SummarySecondMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecondMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecondMMR_HSCPLDmodel_tbl$lowerCI))

SummarySecondMMR_HSCPLD_forest <- ggplot(SummaryORandCI_secondMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLDsecondMMR))
SummarySecondMMR_HSCPLD_forest = SummarySecondMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecondMMR_HSCPLD_forest

anova(Summarymodel_secondMMR_HSCP_LD, test="LRT")
#Export SecondMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_MMR_OR_plots = ggarrange(SecondMMR_HSCP2019_forest, SummarySecondMMR_HSCPLD_forest,
                                labels = c("SecondMMR 2019","SecondMMR LD" ),hjust = -1,
                                legend = NULL,
                                ncol = 2, nrow = 1)
Second_MMR_OR_plots

###Secion 1 Figure 2 Make 2019 maps for all 
#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map

#Second 6in1 2019
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 

newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Second6in1, by.x = "HIAName", by.y = "area_name")

Second_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_16weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_2019_map

#Third 6in1 2019
HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Third6in1, by.x = "HIAName", by.y = "area_name")

Third_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_20weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_2019_map

#First MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map

#Second MMR 2019
HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_SecondMMR, by.x = "HIAName", by.y = "area_name")

Second_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_3y5m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_2019_map

##Export maps 2019 and percentage change

tmap_arrange(First_6in1_2019_map, First_6in1_percentchange_map, Second_6in1_2019_map, Second_6in1_percentchange_map, Third_6in1_2019_map, Third_6in1_percentchange_map,  ncol = 2,
             nrow = 3) 
tmap_arrange(First_MMR_2019_map, First_MMR_percentchange_map, Second_MMR_2019_map, Second_MMR_percentchange_map,  ncol = 2,
             nrow = 2)

###Export the sig only percent change maps with 2019
tmap_arrange(First_6in1_2019_map, First_6in1_percentchange_map_sig, Second_6in1_2019_map, Second_6in1_percentchange_map_sig, Third_6in1_2019_map, Third_6in1_percentchange_map_sig,  ncol = 2,
             nrow = 3) 
tmap_arrange(First_MMR_2019_map, First_MMR_percentchange_map_sig, Second_MMR_2019_map, Second_MMR_percentchange_map_sig,  ncol = 2,
             nrow = 2)
##Export the point change maps only
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map, ncol = 3,
             nrow = 2)

debuggingState(on=FALSE)
###########Make the table for supplementary info
####Making tablefor Section1 table 1
##First 6in1

Section2suptbl2_First6in1 = Full_firstdose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_First6in1_2019 = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_12weeks_num, uptake_12weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
First6in1_2019areapercentage = Section2suptbl2_First6in1_2019 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_First6in1_PreLD = full_join(Section2suptbl2_First6in1_PreLD, First6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
First6in1_Islands = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_First6in1_LD = rbind(Section2suptbl2_First6in1_LD, First6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_First6in1_LD = full_join(Section2suptbl2_First6in1_LD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
First6in1_Islands_PostLD = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_First6in1_PostLD = rbind(Section2suptbl2_First6in1_PostLD, First6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_First6in1_PostLD = full_join(Section2suptbl2_First6in1_PostLD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_First6in1 = full_join(Section2suptbl2_First6in1_2019, Section2suptbl2_First6in1_PreLD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_LD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_PostLD)
FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1%>% 
  mutate (uptake_12weeks_percent= round (uptake_12weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_First6in1, file = "Exported tables/FullSection2suptbl2_First6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_First6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
First6in1_HSCPORtbl_tidypreLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
First6in1_HSCPORtbl_tidyLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
First6in1_HSCPORtbl_tidyPostLD = First6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1, First6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1_OR, First6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_First6in1_OR = full_join(FullSection2suptbl2_First6in1_OR, First6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_First6in1_OR, file = "Exported tables/FullSection2suptbl2_First6in1_OR.csv")
###################
##Second 6in1

Section2suptbl2_Second6in1 = Full_seconddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_Second6in1_2019 = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_16weeks_num, uptake_16weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
Second6in1_2019areapercentage = Section2suptbl2_Second6in1_2019 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_Second6in1_PreLD = full_join(Section2suptbl2_Second6in1_PreLD, Second6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1_PreLD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_Second6in1_PreLD = Section2suptbl2_Second6in1_PreLD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
Second6in1_Islands = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_Second6in1_LD = rbind(Section2suptbl2_Second6in1_LD, Second6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_Second6in1_LD = full_join(Section2suptbl2_Second6in1_LD, Second6in1_2019areapercentage)
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1_LD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
Section2suptbl2_Second6in1_LD = Section2suptbl2_Second6in1_LD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
Second6in1_Islands_PostLD = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_16weeks_num = sum(uptake_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_Second6in1_PostLD = rbind(Section2suptbl2_Second6in1_PostLD, Second6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_Second6in1_PostLD = full_join(Section2suptbl2_Second6in1_PostLD, Second6in1_2019areapercentage)
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1_PostLD %>% 
  mutate(changecf2019 = uptake_16weeks_percent-percentuptake2019)
Section2suptbl2_Second6in1_PostLD = Section2suptbl2_Second6in1_PostLD %>% 
  select(area_name, denominator, uptake_16weeks_percent, uptake_16weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_Second6in1 = full_join(Section2suptbl2_Second6in1_2019, Section2suptbl2_Second6in1_PreLD)
FullSection2suptbl2_Second6in1 = full_join(FullSection2suptbl2_Second6in1, Section2suptbl2_Second6in1_LD)
FullSection2suptbl2_Second6in1 = full_join(FullSection2suptbl2_Second6in1, Section2suptbl2_Second6in1_PostLD)
FullSection2suptbl2_Second6in1 = FullSection2suptbl2_Second6in1%>% 
  mutate (uptake_16weeks_percent= round (uptake_16weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_Second6in1 = FullSection2suptbl2_Second6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_First6in1, file = "Exported tables/FullSection2suptbl2_First6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_Second6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
Second6in1_HSCPORtbl_tidypreLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
Second6in1_HSCPORtbl_tidyLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
Second6in1_HSCPORtbl_tidyPostLD = Second6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1, Second6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1_OR, Second6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_Second6in1_OR = full_join(FullSection2suptbl2_Second6in1_OR, Second6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_Second6in1_OR, file = "Exported tables/FullSection2suptbl2_Second6in1_OR.csv")

###################
##Third 6in1

Section2suptbl2_Third6in1 = Full_thirddose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_Third6in1_2019 = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_20weeks_num, uptake_20weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
Third6in1_2019areapercentage = Section2suptbl2_Third6in1_2019 %>% 
  select(area_name, uptake_20weeks_percent)
colnames(Third6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_Third6in1_PreLD = full_join(Section2suptbl2_Third6in1_PreLD, Third6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1_PreLD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_Third6in1_PreLD = Section2suptbl2_Third6in1_PreLD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
Third6in1_Islands = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_Third6in1_LD = rbind(Section2suptbl2_Third6in1_LD, Third6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_Third6in1_LD = full_join(Section2suptbl2_Third6in1_LD, Third6in1_2019areapercentage)
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1_LD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
Section2suptbl2_Third6in1_LD = Section2suptbl2_Third6in1_LD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
Third6in1_Islands_PostLD = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_20weeks_num = sum(uptake_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_Third6in1_PostLD = rbind(Section2suptbl2_Third6in1_PostLD, Third6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_Third6in1_PostLD = full_join(Section2suptbl2_Third6in1_PostLD, Third6in1_2019areapercentage)
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1_PostLD %>% 
  mutate(changecf2019 = uptake_20weeks_percent-percentuptake2019)
Section2suptbl2_Third6in1_PostLD = Section2suptbl2_Third6in1_PostLD %>% 
  select(area_name, denominator, uptake_20weeks_percent, uptake_20weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_Third6in1 = full_join(Section2suptbl2_Third6in1_2019, Section2suptbl2_Third6in1_PreLD)
FullSection2suptbl2_Third6in1 = full_join(FullSection2suptbl2_Third6in1, Section2suptbl2_Third6in1_LD)
FullSection2suptbl2_Third6in1 = full_join(FullSection2suptbl2_Third6in1, Section2suptbl2_Third6in1_PostLD)
FullSection2suptbl2_Third6in1 = FullSection2suptbl2_Third6in1%>% 
  mutate (uptake_20weeks_percent= round (uptake_20weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_Third6in1 = FullSection2suptbl2_Third6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_Third6in1, file = "Exported tables/FullSection2suptbl2_Third6in1.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#change colnames
colnames(FullSection2suptbl2_Third6in1) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
Third6in1_HSCPORtbl_tidypreLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
Third6in1_HSCPORtbl_tidyLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
Third6in1_HSCPORtbl_tidyPostLD = Third6in1_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1, Third6in1_HSCPORtbl_tidypreLD)
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1_OR, Third6in1_HSCPORtbl_tidyLD)
FullSection2suptbl2_Third6in1_OR = full_join(FullSection2suptbl2_Third6in1_OR, Third6in1_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_Third6in1_OR, file = "Exported tables/FullSection2suptbl2_Third6in1_OR.csv")

##First MMR

Section2suptbl2_FirstMMR = Full_firstdose_MMR %>% 
  select(area_name, cohort, denominator, uptake_13m_num, uptake_13m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_FirstMMR_2019 = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_13m_num, uptake_13m_percent, time_period)
#Take out the 2019 percentage to get % change for table
FirstMMR_2019areapercentage = Section2suptbl2_FirstMMR_2019 %>% 
  select(area_name, uptake_13m_percent)
colnames(FirstMMR_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_FirstMMR_PreLD = full_join(Section2suptbl2_FirstMMR_PreLD, FirstMMR_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR_PreLD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_FirstMMR_PreLD = Section2suptbl2_FirstMMR_PreLD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)

#LD period
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
FirstMMR_Islands = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_FirstMMR_LD = rbind(Section2suptbl2_FirstMMR_LD, FirstMMR_Islands)

#Back to obtaining change from 2019
Section2suptbl2_FirstMMR_LD = full_join(Section2suptbl2_FirstMMR_LD, FirstMMR_2019areapercentage)
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR_LD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
Section2suptbl2_FirstMMR_LD = Section2suptbl2_FirstMMR_LD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)

#Post LD
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
FirstMMR_Islands_PostLD = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_13m_num, uptake_13m_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_13m_num = sum(uptake_13m_num), uptake_13m_percent = mean(uptake_13m_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_FirstMMR_PostLD = rbind(Section2suptbl2_FirstMMR_PostLD, FirstMMR_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_FirstMMR_PostLD = full_join(Section2suptbl2_FirstMMR_PostLD, FirstMMR_2019areapercentage)
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR_PostLD %>% 
  mutate(changecf2019 = uptake_13m_percent-percentuptake2019)
Section2suptbl2_FirstMMR_PostLD = Section2suptbl2_FirstMMR_PostLD %>% 
  select(area_name, denominator, uptake_13m_percent, uptake_13m_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_FirstMMR = full_join(Section2suptbl2_FirstMMR_2019, Section2suptbl2_FirstMMR_PreLD)
FullSection2suptbl2_FirstMMR = full_join(FullSection2suptbl2_FirstMMR, Section2suptbl2_FirstMMR_LD)
FullSection2suptbl2_FirstMMR = full_join(FullSection2suptbl2_FirstMMR, Section2suptbl2_FirstMMR_PostLD)
FullSection2suptbl2_FirstMMR = FullSection2suptbl2_FirstMMR%>% 
  mutate (uptake_13m_percent= round (uptake_13m_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_FirstMMR = FullSection2suptbl2_FirstMMR[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_FirstMMR, file = "Exported tables/FullSection2suptbl2_FirstMMR.csv")

###Try to add data from tidy loop tables (need to run loop script first)
#cahnge colnames
colnames(FullSection2suptbl2_FirstMMR) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
FirstMMR_HSCPORtbl_tidypreLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
FirstMMR_HSCPORtbl_tidyLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
FirstMMR_HSCPORtbl_tidyPostLD = FirstMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR, FirstMMR_HSCPORtbl_tidypreLD)
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR_OR, FirstMMR_HSCPORtbl_tidyLD)
FullSection2suptbl2_FirstMMR_OR = full_join(FullSection2suptbl2_FirstMMR_OR, FirstMMR_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_FirstMMR_OR, file = "Exported tables/FullSection2suptbl2_FirstMMR_OR.csv")

######Second MMR

Section2suptbl2_SecondMMR = Full_seconddose_MMR %>% 
  select(area_name, cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_SecondMMR_2019 = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_3y5m_num, uptake_3y5m_percent, time_period)
#Take out the 2019 percentage to get % change for table
SecondMMR_2019areapercentage = Section2suptbl2_SecondMMR_2019 %>% 
  select(area_name, uptake_3y5m_percent)
colnames(SecondMMR_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_SecondMMR_PreLD = full_join(Section2suptbl2_SecondMMR_PreLD, SecondMMR_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR_PreLD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_SecondMMR_PreLD = Section2suptbl2_SecondMMR_PreLD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)

#LD period
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
SecondMMR_Islands = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_SecondMMR_LD = rbind(Section2suptbl2_SecondMMR_LD, SecondMMR_Islands)

#Back to obtaining change from 2019
Section2suptbl2_SecondMMR_LD = full_join(Section2suptbl2_SecondMMR_LD, SecondMMR_2019areapercentage)
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR_LD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
Section2suptbl2_SecondMMR_LD = Section2suptbl2_SecondMMR_LD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)

#Post LD
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
SecondMMR_Islands_PostLD = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_3y5m_num = sum(uptake_3y5m_num), uptake_3y5m_percent = mean(uptake_3y5m_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_SecondMMR_PostLD = rbind(Section2suptbl2_SecondMMR_PostLD, SecondMMR_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_SecondMMR_PostLD = full_join(Section2suptbl2_SecondMMR_PostLD, SecondMMR_2019areapercentage)
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR_PostLD %>% 
  mutate(changecf2019 = uptake_3y5m_percent-percentuptake2019)
Section2suptbl2_SecondMMR_PostLD = Section2suptbl2_SecondMMR_PostLD %>% 
  select(area_name, denominator, uptake_3y5m_percent, uptake_3y5m_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_SecondMMR = full_join(Section2suptbl2_SecondMMR_2019, Section2suptbl2_SecondMMR_PreLD)
FullSection2suptbl2_SecondMMR = full_join(FullSection2suptbl2_SecondMMR, Section2suptbl2_SecondMMR_LD)
FullSection2suptbl2_SecondMMR = full_join(FullSection2suptbl2_SecondMMR, Section2suptbl2_SecondMMR_PostLD)
FullSection2suptbl2_SecondMMR = FullSection2suptbl2_SecondMMR%>% 
  mutate (uptake_3y5m_percent= round (uptake_3y5m_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_SecondMMR = FullSection2suptbl2_SecondMMR[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_SecondMMR, file = "Exported tables/FullSection2suptbl2_SecondMMR.csv")

###Try to add data from tidy loop tables (need to run loop script Second)
#cahnge colnames
colnames(FullSection2suptbl2_SecondMMR) = c("HSCP", "time_period", "% uptake", "num uptake", "denominator", "%change from 2019")
#seprate out OR tbl by period and add column in order to join properly (need to run loop script)
SecondMMR_HSCPORtbl_tidypreLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PreLD, OR_2019_PreLD_lwr, OR_2019_PreLD_upr, OR_2019_PreLD_pvalue, OR_2019_PreLD_sig_diff)) %>% 
  mutate(time_period = "PreLD")
SecondMMR_HSCPORtbl_tidyLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_LD, OR_2019_LD_lwr, OR_2019_LD_upr, OR_2019_LD_pvalue, OR_2019_LD_sig_diff)) %>% 
  mutate(time_period = "LD")
SecondMMR_HSCPORtbl_tidyPostLD = SecondMMR_HSCPORtbl_tidy %>% 
  select(c(HSCP, OR_2019_PostLD, OR_2019_PostLD_lwr, OR_2019_PostLD_upr, OR_2019_PostLD_pvalue, OR_2019_PostLD_sig_diff)) %>% 
  mutate(time_period = "PostLD")
#Join
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR, SecondMMR_HSCPORtbl_tidypreLD)
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR_OR, SecondMMR_HSCPORtbl_tidyLD)
FullSection2suptbl2_SecondMMR_OR = full_join(FullSection2suptbl2_SecondMMR_OR, SecondMMR_HSCPORtbl_tidyPostLD)

write_csv(FullSection2suptbl2_SecondMMR_OR, file = "Exported tables/FullSection2suptbl2_SecondMMR_OR.csv")
####ENGLISH data
#Yuma Rai - 11/03/02021 
#England coverage data for Hexavalent (dose 1,2 and 3 at 6 months) and MMR (dose 1 at 18 months)

# Make sure the working directory is set
setwd("")

# Install packages that might be needed 

# Loading packages
library(tidyverse)
#library(here)
library(ggplot2)
#library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(ggpubr)
library(readr)
library(reshape)
library(data.table)
library(anytime)
library(lubridate)
library(stringr)

#Loading dataset
coverage <- read.csv("data_table.csv", header = TRUE)

#separate the data for each vaccine
variable_keep <- c("LD_period", "months", "one_dose_6in1_denominator", "one_dose_6in1_numerator", "one_dose_6in1_coverage", "one_dose_6in1_unvaccinated")
England_firstdose_6in1 <- coverage[variable_keep]

variable_keep <- c("LD_period", "months", "two_dose_6in1_denominator", "two_dose_6in1_numerator", "two_dose_6in1_coverage", "two_dose_6in1_unvaccinated")
England_seconddose_6in1 <- coverage[variable_keep]

variable_keep <- c("LD_period", "months", "three_dose_6in1_denominator", "three_dose_6in1_numerator", "three_dose_6in1_coverage", "three_dose_6in1_unvaccinated")
England_thirddose_6in1 <- coverage[variable_keep]

variable_keep <- c("LD_period", "months", "one_dose_mmr_denominator", "one_dose_mmr_numerator", "one_dose_mmr_coverage", "one_dose_mmr_unvaccinated")
England_firstdose_mmr <- coverage[variable_keep]
England_firstdose_mmr <- head(England_firstdose_mmr, -1) ##### remove this line when March 2021 becomes available!!!!


########### To produce bar graphs #################

# Hexavalent dose 1 bar plot
# First, calculate the average coverage in each LD period 

England_firstdose_6in1_coverage <- England_firstdose_6in1 %>%
  group_by(LD_period) %>%
  summarise(total_eligable = sum(one_dose_6in1_denominator), total_vaccinated = sum(one_dose_6in1_numerator), total_unvaccinated = sum(one_dose_6in1_unvaccinated), mean_coverage = ((sum(one_dose_6in1_numerator)/sum(one_dose_6in1_denominator))*100)) 

# reorder so the LD_period shows baseline, then pre_LD, LD and post_LD in order
England_firstdose_6in1_coverage <- England_firstdose_6in1_coverage[c(1,4,2,3),]

# plot the bar plot 
First_6in1_LDplot <- ggplot(England_firstdose_6in1_coverage,aes(x=LD_period, y=mean_coverage, fill=LD_period)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 6 months",
       title = "First dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0, 100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="RdBu")+
  theme(aspect.ratio = 1.5/1)+ 
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "***")+
  annotate("text", x = "LD_2020",  y = 107, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 112, label = "***")
###Annotations added by FM May 2020
First_6in1_LDplot


# Hexavalent dose 2 bar plot 
England_seconddose_6in1_coverage <- England_seconddose_6in1 %>%
  group_by(LD_period) %>%
  summarise(total_eligable = sum(two_dose_6in1_denominator), total_vaccinated = sum(two_dose_6in1_numerator), total_unvaccinated = sum(two_dose_6in1_unvaccinated), mean_coverage = ((sum(two_dose_6in1_numerator)/sum(two_dose_6in1_denominator))*100))  

England_seconddose_6in1_coverage <- England_seconddose_6in1_coverage[c(1,4,2,3),]


Second_6in1_LDplot <- ggplot(England_seconddose_6in1_coverage,aes(x=LD_period, y=mean_coverage, fill=LD_period)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 6 months",
       title = "Second dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),limits = c(0, 100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="RdBu")+
  theme(aspect.ratio = 1.5/1)+ 
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "***")+
  annotate("text", x = "LD_2020",  y = 107, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 112, label = "***")
###Annotations added by FM May 2020

Second_6in1_LDplot

#Hexavalent dose 3 bar plot
England_thirddose_6in1_coverage <- England_thirddose_6in1 %>%
  group_by(LD_period) %>%
  summarise(total_eligable = sum(three_dose_6in1_denominator), total_vaccinated = sum(three_dose_6in1_numerator), total_unvaccinated = sum(three_dose_6in1_unvaccinated), mean_coverage = ((sum(three_dose_6in1_numerator)/sum(three_dose_6in1_denominator))*100)) 

England_thirddose_6in1_coverage <- England_thirddose_6in1_coverage[c(1,4,2,3),]

Third_6in1_LDplot <- ggplot(England_thirddose_6in1_coverage,aes(x=LD_period, y=mean_coverage, fill=LD_period)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 6 months",
       title = "Third dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0, 100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="RdBu")+
  theme(aspect.ratio = 1.5/1)+
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "***")+
  annotate("text", x = "LD_2020",  y = 107, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 112, label = "***")
###Annotations added by FM May 2020

Third_6in1_LDplot

# MMR dose 1 bar plot
England_onedose_mmr_coverage <- England_firstdose_mmr %>%
  group_by(LD_period) %>%
  summarise(total_eligable = sum(one_dose_mmr_denominator), total_vaccinated = sum(one_dose_mmr_numerator),total_unvaccinated = sum(one_dose_mmr_unvaccinated), mean_coverage = ((sum(one_dose_mmr_numerator)/sum(one_dose_mmr_denominator))*100))

England_onedose_mmr_coverage <- England_onedose_mmr_coverage[c(1,4,2,3),]

First_mmr_LDplot <- ggplot(England_onedose_mmr_coverage,aes(x=LD_period, y=mean_coverage, fill=LD_period)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 18 months",
       title = "First dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD","Post LD"))+
  scale_fill_brewer(palette="RdBu")+
  theme(aspect.ratio = 1.5/1)+ 
  annotate("segment", x = "Baseline_2019", xend = "Pre_LD_2020", y = 100, yend = 100, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "LD_2020", y = 105, yend = 105, colour = "black")+
  annotate("segment", x = "Baseline_2019", xend = "Post_LD_2020", y = 110, yend = 110, colour = "black")+
  annotate("text", x = "Pre_LD_2020",  y = 103, label = "***")+
  annotate("text", x = "LD_2020",  y = 107, label = "***")+
  annotate("text", x = "Post_LD_2020",  y = 112, label = "***")
###Annotations added by FM May 2020
First_mmr_LDplot


## Export the four bar charts made above in one pdf
theme_set(theme_pubr())
LD_periods_by_vaccine <- ggarrange(First_6in1_LDplot,Second_6in1_LDplot,Third_6in1_LDplot, First_mmr_LDplot,
                                   labels = "England",
                                   legend=NULL,
                                   ncol = 2, nrow = 2)
LD_periods_by_vaccine

ggsave("Percent vaccinated by LD period England.pdf", width = 8, height = 8) 


########### Bionomial Logistic Regression #################################

# Create a table that shows vaccinated and unvaccinated as 1 and 0 respectively by
# expanding aggregated table for model (making sure there are correct totals for vaccinated and unvaccinated for each LD period)

# Create function for this where you just add the totals inside the 
# function create_binary_table(here is where the totals go) for the function to work

create_binary_table <- function(totals) {
  dt <- data.table(LD_period = c("baseline", "baseline", "pre_LD", "pre_LD", "LD", "LD", "post_LD", "post_LD"), 
                   vacc_or_not = c(1,0,1,0,1,0,1,0), totals)
  # expanded so that each each row represents an individual either vaccinated (1) or unvaccinated (0)
  dt_binary <- dt[rep(seq_len(nrow(dt)), dt$totals),]
  return(dt_binary)
}

# Hexavalent 1 model 

# type totals in order: c(baseline vacc, baseline, unvacc, pre_LD vacc, pre_LD unvacc, LD vacc, LD unvacc, post_LD vacc, post_LD unvacc)
# name the table suitably

first_6in1_binary <- create_binary_table(c(571531, 22169, 132779, 5829, 171660, 7520, 90702, 4160))

# check above line worked 
View(first_6in1_binary) 
# to double check the all vaccinated figures are correct
check_vaccinated <- first_6in1_binary %>% group_by(LD_period) %>% summarise(total_vaccinated = sum(vacc_or_not))
# to double check all entry (i.e. denominators) add up
check_all <- first_6in1_binary %>% group_by(LD_period) %>% summarise(no_rows = length(LD_period))


# log regression with the binary (vaccinated/unvaccinated) data
model_first6in1_england <- glm(first_6in1_binary$vacc_or_not ~ first_6in1_binary$LD_period, family="binomial")

summary(model_first6in1_england)

exp(model_first6in1_england$coefficients)
exp(confint(model_first6in1_england))  


# Hexavalent 2 model 
# use function create_binary_table to expand aggregated table 
second_6in1_binary <- create_binary_table(c(559382,36433, 131578, 11255, 164780, 12441, 87818, 7142))

# log regression with binary data
model_second6in1_england = glm(second_6in1_binary$vacc_or_not ~ second_6in1_binary$LD_period, family="binomial")

summary(model_second6in1_england)

exp(model_second6in1_england$coefficients)
exp(confint(model_second6in1_england))  


# Hexavalent 3 model 
# use function create_binary_table to expand aggregated table 
third_6in1_binary <- create_binary_table(c(529163,67192,126284,21195,154912,24041,79991,12999))

# log regression with binary data
model_third6in1_england = glm(third_6in1_binary$vacc_or_not ~ third_6in1_binary$LD_period, family="binomial")

summary(model_third6in1_england)

exp(model_third6in1_england$coefficients)
exp(confint(model_third6in1_england))  


# MMR 1 model 
# use function create_binary_table to expand aggregated table 
# change the last two when you get data for March 2021!!!!
first_mmr_binary <- create_binary_table(c(541107, 72816, 119885, 18468, 173451, 27713, 44014, 7153)) 

# log regression with binary data
model_onedose_mmr_england = glm(first_mmr_binary$vacc_or_not ~ first_mmr_binary$LD_period, family="binomial")

summary(model_onedose_mmr_england)

exp(model_onedose_mmr_england$coefficients)
exp(confint(model_onedose_mmr_england)) 


########### Make a plot showing OR and CI (compared to baseline 2019)

# Create a function to make the OR and CI tables 
OR_tables <- function(log_model) {
  temp_table <- log_model %>% 
    tidy(conf.int = TRUE, exp = TRUE) %>% 
    filter(str_detect(term,"(Intercept)", negate = TRUE)) %>% 
    mutate(LD_period = term) 
  
  # somehow the rename does not work on the code above so have renamed estimate[2], conf.low[6], conf.high[7] below
  names(temp_table)[2] <- "OR"
  names(temp_table)[6] <- "lowerCI"
  names(temp_table)[7] <- "upperCI"
  return(temp_table)
}

# Hexavalent 1 - make the tables using above fuction and the output from the log regression model
First6in1_model_tbl <- OR_tables(model_first6in1_england) # added models produced to create OR+CI table
First6in1_model_tbl <- First6in1_model_tbl[c(3,1,2),] # reorder so the LD_period shows pre_LD, LD then post_LD in order

# Hexavalent 2
Second6in1_model_tbl <- OR_tables(model_second6in1_england) 
Second6in1_model_tbl <- Second6in1_model_tbl[c(3,1,2),]

# Hexavalent 3
Third6in1_model_tbl <- OR_tables(model_third6in1_england) 
Third6in1_model_tbl <- Third6in1_model_tbl[c(3,1,2),]

# MMR 1
FirstMMR_model_tbl <- OR_tables(model_onedose_mmr_england) 
FirstMMR_model_tbl <- FirstMMR_model_tbl[c(3,1,2),]


# Create labels for the OR and CI plots
boxLabels = c("Pre lockdown","Lockdown", "Post lockdown")

# create function to enter summary data from above tables and to create OR and CI plots
ORandCI_plot <- function(model_table) {
  temp_table <- data.frame(
    yAxis = boxLabels,
    boxOdds = c(model_table$OR),
    boxCILow = c(model_table$lowerCI),
    boxCIHigh = c(model_table$upperCI)) # created table that will be plotted 
  
  temp_table %>%
    mutate(boxLabels = fct_relevel(boxLabels, "Post lockdown", "Lockdown", "Pre lockdown")) %>%
    ggplot(aes(x = boxOdds, y = boxLabels))+ # need to specify 2 decimal places for OR values on x axis
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    theme_bw()+
    labs(x = "OR compared to 2019",
         y = NULL,
         title = NULL)+ # created the plot using table created in the first part of this function
    scale_y_continuous(breaks = c(0.75, 0.80, 0.85, 0.90, 0.95, 1.00, 1.05))
}

# using "ORandCI_plot" function above, create plots for each vaccine and then run the anova test 
First6in1_forest <- ORandCI_plot(First6in1_model_tbl)
First6in1_forest
anova(model_first6in1_england, test="LRT")

Second6in1_forest <- ORandCI_plot(Second6in1_model_tbl)
Second6in1_forest
anova(model_second6in1_england, test="LRT")

Third6in1_forest <- ORandCI_plot(Third6in1_model_tbl)
Third6in1_forest
anova(model_third6in1_england, test="LRT")

FirstMMR_forest <- ORandCI_plot(FirstMMR_model_tbl)
FirstMMR_forest
anova(model_onedose_mmr_england, test="LRT")


#Export forest style plots in one pdf
ORandCI_plots_byLDperiod <- ggarrange(First6in1_forest, Second6in1_forest, Third6in1_forest, FirstMMR_forest,
                                      labels = c("First 6in1","Second 6in1","Third 6in1","First MMR"),
                                      ncol = 2, nrow = 2)

ORandCI_plots_byLDperiod

ggsave("OR and CI plots by LD period compared to 2019 in England.pdf", width = 8, height = 8)


######## line plot by month for Hexavalent (doses 1, 2, 3) and for MMR (dose 1) ###############

# From the vaccine table with coverage (orginal dataset), select relevant columns to join, 
# add a column for vaccine type and rename columns in preparation for joining

# Hexavalent dose 1
MonthlyEngland_firstdose_6in1 <- coverage %>% 
  select(months, one_dose_6in1_coverage)%>% 
  mutate("Vaccine"="First6in1")
colnames(MonthlyEngland_firstdose_6in1) = c("Month", "Percent_coverage_6months", "Vaccine")

# Hexavalent dose 2
MonthlyEngland_seconddose_6in1 <- coverage %>% 
  select(months, two_dose_6in1_coverage)%>% 
  mutate("Vaccine"="Second6in1")
colnames(MonthlyEngland_seconddose_6in1) = c("Month", "Percent_coverage_6months", "Vaccine")

# Hexavalent dose 3
MonthlyEngland_thirddose_6in1 <- coverage %>% 
  select(months, three_dose_6in1_coverage)%>% 
  mutate("Vaccine"="Third6in1")
colnames(MonthlyEngland_thirddose_6in1) = c("Month", "Percent_coverage_6months", "Vaccine")

# MMR dose 1
MonthlyEngland_firstdose_mmr <- coverage %>% 
  select(months, one_dose_mmr_coverage)%>% 
  mutate("Vaccine"="FirstMMR")
colnames(MonthlyEngland_firstdose_mmr) = c("Month", "Percent_coverage_18months", "Vaccine")
MonthlyEngland_firstdose_mmr<- head(MonthlyEngland_firstdose_mmr, -1) ### remove when you have march 2021 data!!!

# Join the Hexavalent tables 
All_6in1_bymonth <- full_join(MonthlyEngland_firstdose_6in1, MonthlyEngland_seconddose_6in1)
All_6in1_bymonth <- full_join(All_6in1_bymonth, MonthlyEngland_thirddose_6in1)


## Plot line graph the tables now 

All_6in1_bymonth$Month <- as.Date(All_6in1_bymonth$Month, "%d/%m/%Y") # specify months as date so time-series can be plotted

MonthlyEngland_6in1_line = All_6in1_bymonth %>%
  ggplot(aes(x=Month, y=Percent_coverage_6months, group=Vaccine, color=Vaccine)) +
  geom_line()+
  theme_bw()+
  scale_x_date(labels = function(x) format(x, "%b-%y"))+ 
  scale_y_continuous(breaks = c(70,75,80,85,90,95,100), limits = c(80, 100))+
  labs(x = "month vaccine first offered",
       y = "% vaccinated (6 months)",
       title = "All doses 6in1")

# Add vertical lines to show LD periods (using the months that Scotland used but this may vary for England)
MonthlyEngland_6in1_line <- MonthlyEngland_6in1_line + geom_vline(xintercept = as.Date("2020/04/01"), linetype="dotted", 
                                                                  color = "blue", size=1) ###LD starts
MonthlyEngland_6in1_line <- MonthlyEngland_6in1_line + geom_vline(xintercept = as.Date("2020/08/01"), linetype="dotted", 
                                                                  color = "blue", size=1) ####LD ends
MonthlyEngland_6in1_line <- MonthlyEngland_6in1_line + annotate("rect", xmin = "2020/06/01", xmax = "2020/08/01", ymin = 100.5, ymax = 102,
                                                                alpha = .1,fill = "purple")+ annotate("text", x ="2020/07/01", y = 101.5, 
                                                                                                      label = "Gradual easing of restrictions", color="black", 
                                                                                                      size=2)####Added by Fiona May 2021

MonthlyEngland_6in1_line


## Plot Line graph for MMR dose 1

MonthlyEngland_firstdose_mmr$Month <- as.Date(MonthlyEngland_firstdose_mmr$Month, "%d/%m/%Y") # specify months as date so time-series can be plotted

MonthlyEngland_firstdose_mmr <- MonthlyEngland_firstdose_mmr %>%
  ggplot(aes(x=Month, y=Percent_coverage_18months, group=Vaccine, color=Vaccine)) +
  geom_line()+
  theme_bw()+
  scale_x_date(labels = function(x) format(x, "%b-%y"))+
  scale_y_continuous(breaks = c(80,85,90,95,100), limits = c(80,100))+
  labs(x = "month vaccine first offered",
       y = "% vaccinated (18 months)",
       title = "One dose MMR")


# Add vertical lines to show LD periods (using the months that Scotland used but this may vary for England)
MonthlyEngland_firstdose_mmr <- MonthlyEngland_firstdose_mmr + geom_vline(xintercept = as.Date("2020/04/01"), linetype="dotted", 
                                                                          color = "blue", size=1) ###LD starts
MonthlyEngland_firstdose_mmr <- MonthlyEngland_firstdose_mmr + geom_vline(xintercept = as.Date("2020/08/01"), linetype="dotted", 
                                                                          color = "blue", size=1)###LD ends
MonthlyEngland_firstdose_mmr <- MonthlyEngland_firstdose_mmr + annotate("rect", xmin = "2020/06/01", xmax = "2020/08/01", ymin = 100.5, ymax = 102,
                                                                        alpha = .1,fill = "purple")+ annotate("text", x ="2020/07/01", y = 101.5, 
                                                                                                              label = "Gradual easing of restrictions", color="black", 
                                                                                                              size=2)####Added by Fiona May 2021
MonthlyEngland_firstdose_mmr

## Export the plots into pdf
theme_set(theme_pubr())
Line_plots_bymonth <- ggarrange(MonthlyEngland_6in1_line, MonthlyEngland_firstdose_mmr,
                                labels = NULL,
                                legend=NULL,
                                ncol = 1, nrow = 2) 
Line_plots_bymonth

ggsave("Line graph by month in England.pdf", width = 14, height = 8)

