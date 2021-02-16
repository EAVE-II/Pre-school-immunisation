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

##Making table

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
