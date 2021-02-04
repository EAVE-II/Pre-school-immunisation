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
#New tbl of data by LD, add unvaccinated column and summarise by LD period
First_6in1_by_LDperiod = Scotland_firstdose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_12weeks_percent))

Second_6in1_by_LDperiod = Scotland_seconddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_16weeks_num, uptake_16weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_16weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_16weeks_percent))

Third_6in1_by_LDperiod = Scotland_thirddose_6in1 %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_20weeks_num, uptake_20weeks_percent) %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_20weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_20weeks_percent))

First_MMR_by_LDperiod = Scotland_firstdose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_13m_num, uptake_13m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_13m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_13m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_13m_percent))

Second_MMR_by_LDperiod = Scotland_seconddose_MMR %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_3y5m_num, uptake_3y5m_percent) %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num)%>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_3y5m_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_3y5m_percent))

#Plot mean percent uptake by lockdown period from above tibbles
#First dose 6in1
First_6in1_LDplot = First_6in1_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

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
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

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
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

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
       title = "Second dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

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
       title = "Second dose 6in1")+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))

Second_MMR_LDplot= Second_MMR_LDplot + geom_hline(yintercept = 51.8, linetype="dashed", 
                                                color = "blue", size=1)
Second_MMR_LDplot

#Export in one pdf

LD_periods_by_vaccine = ggarrange(First_6in1_LDplot,Second_6in1_LDplot,Third_6in1_LDplot,First_MMR_LDplot,Second_MMR_LDplot,
                                  labels = NULL,
                                  common.legend = TRUE, legend="bottom",
                                  ncol = 3, nrow = 2)
LD_periods_by_vaccine


