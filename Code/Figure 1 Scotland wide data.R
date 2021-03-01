#######################
##Figure 1 Scotland wide data by LD period
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
library(plotrix)

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

##Plot weekly values for each vaccine, uptake within 4 weeks of becoming eligible


#First dose 6in1
Weekly_first_6in1 = Scotland_firstdose_6in1 %>% 
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1")


Weekly_first_6in1 = Weekly_first_6in1 + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1) + theme(axis.text.x = element_text(angle = 90)) 

#Second dose 6in1
Weekly_second_6in1 = Scotland_seconddose_6in1 %>% 
  ggplot(aes(x=time.factor, y=uptake_16weeks_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "% vaccinated (4 weeks)",
       title = "Second dose 6in1")

Weekly_second_6in1 = Weekly_second_6in1 + geom_hline(yintercept = 84.8, linetype="dashed", 
                               color = "blue", size=1) + theme(axis.text.x = element_text(angle = 90))

#Third dose 6in1
Weekly_third_6in1 = Scotland_thirddose_6in1 %>% 
  ggplot(aes(x=time.factor, y=uptake_20weeks_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "% vaccinated (4 weeks)",
       title = "Third dose 6in1")

Weekly_third_6in1 = Weekly_third_6in1 + geom_hline(yintercept = 73, linetype="dashed", 
                                color = "blue", size=1) + theme(axis.text.x = element_text(angle = 90))

#First dose MMR

Weekly_first_MMR = Scotland_firstdose_MMR %>% 
  ggplot(aes(x=time.factor, y=uptake_13m_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")

Weekly_first_MMR = Weekly_first_MMR + geom_hline(yintercept = 65.2, linetype="dashed", 
                               color = "blue", size=1) + theme(axis.text.x = element_text(angle = 90))

#Second dose MMR
Weekly_second_MMR = Scotland_seconddose_MMR %>% 
  ggplot(aes(x=time.factor, y=uptake_3y5m_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")

Weekly_second_MMR = Weekly_second_MMR + geom_hline(yintercept = 51.8, linetype="dashed", 
                              color = "blue", size=1) + theme(axis.text.x = element_text(angle = 90))

#Export plots on one pdf
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Weekly_by_vaccine = ggarrange(Weekly_first_6in1,Weekly_second_6in1,Weekly_third_6in1,Weekly_first_MMR,Weekly_second_MMR,
                              labels = NULL,
                              common.legend = TRUE, legend="bottom",
                              ncol = 3, nrow = 2)
Weekly_by_vaccine


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


#Plot mean percent uptake by lockdown period from above tibbles ++++++++++++++++++=need to change limets on y axis
#First dose 6in1
First_6in1_LDplot = First_6in1_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

First_6in1_LDplot= First_6in1_LDplot + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1)
First_6in1_LDplot

#Second dose 6in1
Second_6in1_LDplot = Second_6in1_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "Second dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Second_6in1_LDplot= Second_6in1_LDplot + geom_hline(yintercept = 84.8, linetype="dashed", 
                                                  color = "blue", size=1)
Second_6in1_LDplot

#Third dose 6in1
Third_6in1_LDplot = Third_6in1_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "Third dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Third_6in1_LDplot= Third_6in1_LDplot + geom_hline(yintercept = 73, linetype="dashed", 
                                                    color = "blue", size=1)
Third_6in1_LDplot

#First dose MMR
First_MMR_LDplot = First_MMR_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

First_MMR_LDplot= First_MMR_LDplot + geom_hline(yintercept = 65.2, linetype="dashed", 
                                                    color = "blue", size=1)
First_MMR_LDplot

#Second dose MMR
Second_MMR_LDplot = Second_MMR_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Second_MMR_LDplot= Second_MMR_LDplot + geom_hline(yintercept = 51.8, linetype="dashed", 
                                                color = "blue", size=1)
Second_MMR_LDplot

#Export in one pdf

LD_periods_by_vaccine = ggarrange(First_6in1_LDplot,Second_6in1_LDplot,Third_6in1_LDplot,First_MMR_LDplot,Second_MMR_LDplot,
                                  labels = NULL,
                                  legend=NULL,
                                  ncol = 3, nrow = 2)
LD_periods_by_vaccine

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

Boxplot_first6in1_LDperiod = Weekly_first6in1_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_12weeks_percent)) +
  geom_boxplot(fill="slateblue", alpha=0.2)+
  ylab("% vaccinated (4weeks)")+
  xlab(NULL)+
  ylim(70,100)+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Boxplot_first6in1_LDperiod

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

Boxplot_second6in1_LDperiod = Weekly_second6in1_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_16weeks_percent)) +
  geom_boxplot(fill="slateblue", alpha=0.2)+
  ylab("% vaccinated (4weeks)")+
  xlab(NULL)+
  ylim(70,100)+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Boxplot_second6in1_LDperiod

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

Boxplot_third6in1_LDperiod = Weekly_third6in1_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_20weeks_percent)) +
  geom_boxplot(fill="slateblue", alpha=0.2)+
  ylab("% vaccinated (4weeks)")+
  xlab(NULL)+
  ylim(70,100)+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Boxplot_third6in1_LDperiod

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

Boxplot_firstMMR_LDperiod = Weekly_firstMMR_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_13m_percent)) +
  geom_boxplot(fill="slateblue", alpha=0.2)+
  ylab("% vaccinated (4weeks)")+
  xlab(NULL)+
  ylim(40,100)+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Boxplot_firstMMR_LDperiod

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

Boxplot_secondMMR_LDperiod = Weekly_secondMMR_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_3y5m_percent)) +
  geom_boxplot(fill="slateblue", alpha=0.2)+
  ylab("% vaccinated (4weeks)")+
  xlab(NULL)+
  ylim(40,100)+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))

Boxplot_secondMMR_LDperiod

#Export boxplots ++++++++++++++++need to change plot label position

Boxplots_percentbyLDperiod = ggarrange(Boxplot_first6in1_LDperiod,Boxplot_second6in1_LDperiod,Boxplot_third6in1_LDperiod,Boxplot_firstMMR_LDperiod,Boxplot_secondMMR_LDperiod,
                                  labels = c("First 6in1","Second 6in1","Third 6in1","First MMR","Second MMR"),
                                  ncol = 3, nrow = 2)
Boxplots_percentbyLDperiod

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

##Make a plot showing OR and CI compared to baseline 2019
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

#Create labels and enter summary data from above tbls
boxLabels = c("Pre lockdown","Lockdown", "Post lockdown")

#First 6in1 plot
Plot_ORandCI_first6in1 <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(First6in1_model_tbl$OR),
  boxCILow = c(First6in1_model_tbl$upperCI),
  boxCIHigh = c(First6in1_model_tbl$lowerCI))

First6in1_forest <- ggplot(Plot_ORandCI_first6in1, aes(x = boxOdds, y = boxLabels))
First6in1_forest = First6in1_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "OR compared to 2019",
     y = NULL,
     title = NULL)
  

First6in1_forest ####Change the order


anova(model_first6in1_scotland_2019, test="LRT")


#Second 6in1 plot
Plot_ORandCI_second6in1 <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(Second6in1_model_tbl$OR),
  boxCILow = c(Second6in1_model_tbl$upperCI),
  boxCIHigh = c(Second6in1_model_tbl$lowerCI))

Second6in1_forest <- ggplot(Plot_ORandCI_second6in1, aes(x = boxOdds, y = boxLabels))
Second6in1_forest = Second6in1_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "OR compared to 2019",
       y = NULL,
       title = NULL)


Second6in1_forest ####Change the order


anova(model_second6in1_scotland_2019, test="LRT")

#Third 6in1 plot
Plot_ORandCI_third6in1 <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(Third6in1_model_tbl$OR),
  boxCILow = c(Third6in1_model_tbl$upperCI),
  boxCIHigh = c(Third6in1_model_tbl$lowerCI))

Third6in1_forest <- ggplot(Plot_ORandCI_third6in1, aes(x = boxOdds, y = boxLabels))
Third6in1_forest = Third6in1_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "OR compared to 2019",
       y = NULL,
       title = NULL)


Third6in1_forest ####Change the order


anova(model_third6in1_scotland_2019, test="LRT")

#First MMR plot
Plot_ORandCI_firstMMR <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(FirstMMR_model_tbl$OR),
  boxCILow = c(FirstMMR_model_tbl$upperCI),
  boxCIHigh = c(FirstMMR_model_tbl$lowerCI))

FirstMMR_forest <- ggplot(Plot_ORandCI_firstMMR, aes(x = boxOdds, y = boxLabels))
FirstMMR_forest = FirstMMR_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "OR compared to 2019",
       y = NULL,
       title = NULL)


FirstMMR_forest ####Change the order


anova(model_firstMMR_scotland_2019, test="LRT")

#Second MMR plot
Plot_ORandCI_secondMMR <- data.frame(
  yAxis = length(boxLabels):1,
  boxOdds = c(SecondMMR_model_tbl$OR),
  boxCILow = c(SecondMMR_model_tbl$upperCI),
  boxCIHigh = c(SecondMMR_model_tbl$lowerCI))

SecondMMR_forest <- ggplot(Plot_ORandCI_secondMMR, aes(x = boxOdds, y = boxLabels))
SecondMMR_forest = SecondMMR_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "OR compared to 2019",
       y = NULL,
       title = NULL)

SecondMMR_forest ####Change the order

anova(model_secondMMR_scotland_2019, test="LRT")

#Export forest style plots in one pdf

ORandCI_plots_byLDperiod = ggarrange(First6in1_forest, Second6in1_forest, Third6in1_forest,FirstMMR_forest,SecondMMR_forest,
                                     labels = c("First 6in1","Second 6in1","Third 6in1","First MMR","Second MMR"),
                                     ncol = 3, nrow = 2)

ORandCI_plots_byLDperiod


####Potential supplementary figure


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))

#Selecting out Scotland wide data and monthly rows +++have lef in Oct and Nov for interest, turn cohort into time.factor and relevel to be non alphabetical
MonthlyScotland_firstdose_6in1 = Full_firstdose_6in1 %>% 
  filter(area_name == "Scotland") %>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")) %>% 
  mutate(time.factor = 
         cohort %>% 
         factor() %>% 
         fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20", "Oct-20" = "Oct-20", "Nov-20" = "Nov-20")) 

MonthlyScotland_firstdose_6in1$time.factor = factor(MonthlyScotland_firstdose_6in1$time.factor, levels = MonthlyScotland_firstdose_6in1$time.factor)


MonthlyScotland_seconddose_6in1 = Full_seconddose_6in1%>% 
  filter(area_name == "Scotland")%>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))%>% 
  mutate(time.factor = 
           cohort %>% 
           factor() %>% 
           fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20", "Oct-20" = "Oct-20", "Nov-20" = "Nov-20")) 

MonthlyScotland_seconddose_6in1$time.factor = factor(MonthlyScotland_seconddose_6in1$time.factor, levels = MonthlyScotland_seconddose_6in1$time.factor)

MonthlyScotland_thirddose_6in1 = Full_thirddose_6in1%>% 
  filter(area_name == "Scotland")%>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))%>% 
  mutate(time.factor = 
           cohort %>% 
           factor() %>% 
           fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20", "Oct-20" = "Oct-20", "Nov-20" = "Nov-20")) 

MonthlyScotland_thirddose_6in1$time.factor = factor(MonthlyScotland_thirddose_6in1$time.factor, levels = MonthlyScotland_thirddose_6in1$time.factor)

MonthlyScotland_firstdose_MMR = Full_firstdose_MMR%>% 
  filter(area_name == "Scotland")%>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))%>% 
  mutate(time.factor = 
           cohort %>% 
           factor() %>% 
           fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20", "Oct-20" = "Oct-20", "Nov-20" = "Nov-20")) 

MonthlyScotland_firstdose_MMR$time.factor = factor(MonthlyScotland_firstdose_MMR$time.factor, levels = MonthlyScotland_firstdose_MMR$time.factor)

MonthlyScotland_seconddose_MMR = Full_seconddose_MMR%>% 
  filter(area_name == "Scotland")%>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))%>% 
  mutate(time.factor = 
           cohort %>% 
           factor() %>% 
           fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20", "Oct-20" = "Oct-20", "Nov-20" = "Nov-20")) 

MonthlyScotland_seconddose_MMR$time.factor = factor(MonthlyScotland_seconddose_MMR$time.factor, levels = MonthlyScotland_seconddose_MMR$time.factor)


#Select relevant columns to join, add a column for vaccine type and rename columns in preparatino for joining

MonthlyScotland_firstdose_6in1 = MonthlyScotland_firstdose_6in1 %>% 
  select(time.factor, uptake_12weeks_percent) %>% 
  mutate("Vaccine"="First6in1")
colnames(MonthlyScotland_firstdose_6in1) = c("Month", "Percent_4weeks", "Vaccine")

MonthlyScotland_seconddose_6in1 = MonthlyScotland_seconddose_6in1 %>% 
  select(time.factor, uptake_16weeks_percent)%>% 
  mutate("Vaccine"="Second6in1")
colnames(MonthlyScotland_seconddose_6in1) = c("Month", "Percent_4weeks", "Vaccine")

MonthlyScotland_thirddose_6in1 = MonthlyScotland_thirddose_6in1 %>% 
  select(time.factor, uptake_20weeks_percent)%>% 
  mutate("Vaccine"="Third6in1")
colnames(MonthlyScotland_thirddose_6in1) = c("Month", "Percent_4weeks", "Vaccine")

MonthlyScotland_firstdose_MMR = MonthlyScotland_firstdose_MMR %>% 
  select(time.factor, uptake_13m_percent) %>% 
  mutate("Vaccine"="FirstMMR")
colnames(MonthlyScotland_firstdose_MMR) = c("Month", "Percent_4weeks", "Vaccine")

MonthlyScotland_seconddose_MMR = MonthlyScotland_seconddose_MMR %>% 
  select(time.factor, uptake_3y5m_percent)%>% 
  mutate("Vaccine"="SecondMMR")
colnames(MonthlyScotland_seconddose_MMR) = c("Month", "Percent_4weeks", "Vaccine")

#Join 6in1 doses and plot
All_6in1_bymonth = full_join(MonthlyScotland_firstdose_6in1, MonthlyScotland_seconddose_6in1)
All_6in1_bymonth = full_join(All_6in1_bymonth, MonthlyScotland_thirddose_6in1)


MonthlyScotland_6in1_line = All_6in1_bymonth %>%
  ggplot(aes(x=Month, y=Percent_4weeks, group=Vaccine, color=Vaccine)) +
  geom_line()+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "All doses6in1")+
  scale_y_continuous(breaks = c(70,75,80,85,90,95,100))

MonthlyScotland_6in1_line = MonthlyScotland_6in1_line + geom_vline(xintercept = "Apr-20", linetype="dotted", 
                                                                                       color = "blue", size=1) ###LD starts

MonthlyScotland_6in1_line = MonthlyScotland_6in1_line + geom_vline(xintercept = "Aug-20", linetype="dotted", 
                                                                                      color = "blue", size=1) ####LD ends
MonthlyScotland_6in1_line = MonthlyScotland_6in1_line + geom_vline(xintercept = "Jun-20", linetype="dotted", 
                                                                 color = "purple", size=1) ###Easing began 28 May
MonthlyScotland_6in1_line

#Join MMR doses and plot
All_MMR_bymonth = full_join(MonthlyScotland_firstdose_MMR, MonthlyScotland_seconddose_MMR)

MonthlyScotland_MMR_line = All_MMR_bymonth %>%
  ggplot(aes(x=Month, y=Percent_4weeks, group=Vaccine, color=Vaccine)) +
  geom_line()+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "All doses MMR")+
  scale_y_continuous(breaks = c(50,55,60,65,70,75,80,85,90,95,100))

MonthlyScotland_MMR_line = MonthlyScotland_MMR_line + geom_vline(xintercept = "Apr-20", linetype="dotted", 
                                                                   color = "blue", size=1) ###LD starts

MonthlyScotland_MMR_line = MonthlyScotland_MMR_line + geom_vline(xintercept = "Aug-20", linetype="dotted", 
                                                                   color = "blue", size=1)###LD ends
MonthlyScotland_MMR_line = MonthlyScotland_MMR_line + geom_vline(xintercept = "Jun-20", linetype="dotted", 
                                                                 color = "purple", size=1)###Easing begins 28 May
MonthlyScotland_MMR_line

#Export
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Line_plots_bymonth = ggarrange(MonthlyScotland_6in1_line, MonthlyScotland_MMR_line,
                                  labels = NULL,
                                  legend=NULL,
                                  ncol = 1, nrow = 2)
Line_plots_bymonth

###Making tablefor Section1 table 1
##First 6in1

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
  select(time_period, OR, lowerCI, upperCI) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))

Section1tbl_First_6in1_by_LDperiod = full_join(Section1tbl_First_6in1_by_LDperiod, First6in1_model_tbl)

colnames(Section1tbl_First_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI")

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
  select(time_period, OR, lowerCI, upperCI) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))

Section1tbl_Second_6in1_by_LDperiod = full_join(Section1tbl_Second_6in1_by_LDperiod, Second6in1_model_tbl)

colnames(Section1tbl_Second_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI")

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
  select(time_period, OR, lowerCI, upperCI) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))

Section1tbl_Third_6in1_by_LDperiod = full_join(Section1tbl_Third_6in1_by_LDperiod, Third6in1_model_tbl)

colnames(Section1tbl_Third_6in1_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI")

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
  select(time_period, OR, lowerCI, upperCI) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))

Section1tbl_FirstMMR_by_LDperiod = full_join(Section1tbl_FirstMMR_by_LDperiod, FirstMMR_model_tbl)

colnames(Section1tbl_FirstMMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI")

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
  select(time_period, OR, lowerCI, upperCI) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>%  
  mutate (upperCI= round (upperCI, digits = 2))

Section1tbl_SecondMMR_by_LDperiod = full_join(Section1tbl_SecondMMR_by_LDperiod, SecondMMR_model_tbl)

colnames(Section1tbl_SecondMMR_by_LDperiod) = c("Vaccine", "Time period", "% uptake within 4 weeks of eligibility", "Absolute % change from 2019", "OR for uptake compared to 2019", "Lower 95% CI", "Upper 95% CI")

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
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
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
  scale_x_discrete(breaks =c("Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 30-MAR-20", "W/B 04-MAY-20", "W/B 01-JUN-20", "W/B 29-JUN-20", "W/B 03-AUG-20", "W/B 31-AUG-20"), label = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sept 20")) 
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
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "MMR vaccine")+
  expand_limits(y=45)+
  scale_y_continuous(breaks = seq(45,100,5)) +
  geom_hline(yintercept = 65.2, linetype="dashed", color = "#66c2a5", size=0.75)+
  geom_hline(yintercept = 51.8, linetype="dashed", color = "#fc8d62", size=0.75)+
  theme(axis.text.x = element_text(angle = 90))+
  annotate("rect", xmin = "W/B 23-MAR-20", xmax = "W/B 03-AUG-20", ymin = 40, ymax = 100,
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = "Jan-20", xmax = "W/B 02-MAR-20", ymin = 40, ymax = 41,
           alpha = .1,fill = "black")+
  annotate("text", x = "Feb-20", y = 42, size = 2, label = "Monthly data")+
  scale_x_discrete(breaks =c("Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 30-MAR-20", "W/B 04-MAY-20", "W/B 01-JUN-20", "W/B 29-JUN-20", "W/B 03-AUG-20", "W/B 31-AUG-20"), label = c("Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20", "Aug 20", "Sept 20")) 
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


############
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
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by age 24weeks",
       title = "First dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)

First_6in1_LDplot_cfEngland= First_6in1_LDplot_cfEngland + geom_hline(yintercept = 97.9, linetype="dashed", 
                                                  color = "Purple", size=0.5)
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
  theme(aspect.ratio = 1.5/1)
  

Second_6in1_LDplot_cfEngland= Second_6in1_LDplot_cfEngland + geom_hline(yintercept = 96.7, linetype="dashed", 
                                                    color = "purple", size=0.5)
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
  theme(aspect.ratio = 1.5/1)

Third_6in1_LDplot_cfEngland= Third_6in1_LDplot_cfEngland + geom_hline(yintercept = 94, linetype="dashed", 
                                                  color = "Purple", size=0.5)
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
  theme(aspect.ratio = 1.5/1)

First_MMR_LDplot_cfEngland= First_MMR_LDplot_cfEngland + geom_hline(yintercept = 91.1, linetype="dashed", 
                                                color = "Purple", size=0.5)
First_MMR_LDplot_cfEngland


##Second dose MMR (not for ENgland)
Second_MMR_LDplot_cfEngland = Second_MMR_by_LDperiod_cfEngland %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated by 16 months",
       title = "Second dose MMR")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_x_discrete(labels=c("2019", "PreLD", "LD", "Post LD"))+
  scale_fill_brewer(palette="PRGn")+
  theme(aspect.ratio = 1.5/1)

Second_MMR_LDplot_cfEngland= Second_MMR_LDplot_cfEngland + geom_hline(yintercept = 80.8, linetype="dashed", 
                                                                      color = "Purple", size=0.5)
Second_MMR_LDplot_cfEngland

Scottishuptake_forcomaprisonwithEnlgand = ggarrange(First_6in1_LDplot_cfEngland, Second_6in1_LDplot_cfEngland, Third_6in1_LDplot_cfEngland, First_MMR_LDplot_cfEngland, Second_MMR_LDplot_cfEngland,
                                                    labels = "Scotland",
                                                    legend = NULL,
                                                    ncol = 3, nrow = 2)
Scottishuptake_forcomaprisonwithEnlgand

###Logestic regression for comparisons with ENgland nad later time periods
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

