#First dose 6in1 clean code

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)

#Loading full dataset for first dose 6in1
Full_firstdose_6in1_download_Jan21 = read.csv(here("Data", "First_dose_6in1_downloaded_18_jan_21.csv"))

#Selecting out Scotland wide data
Scotland_firstdose_6in1 = Full_firstdose_6in1_download_Jan21 %>% 
  filter(area_name == "Scotland")

#Selecting out monthly data
Scotland_by_month = Scotland_firstdose_6in1 %>% 
  filter(cohort %in% c("2019", "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20"))

##Plot Scotland by month
#First turn the months into factors
Scotland_by_month = Scotland_by_month %>%
  mutate(time.factor = 
           cohort %>% 
           factor() %>% 
           fct_recode("2019" = "2019", "Jan-20" = "Jan-20", "Feb-20" = "Feb-20", "Mar-20" = "Mar-20","Apr-20"= "Apr-20", "May-20" = "May-20", "Jun-20" = "Jun-20", "Jul-20" = "Jul-20", "Aug-20" = "Aug-20", "Sep-20" = "Sep-20")) 

#Relevel to be non alphabetcial
Scotland_by_month$time.factor = factor(Scotland_by_month$time.factor, levels = Scotland_by_month$time.factor)

#Plot then  add a line to show 2019  level
Scotland_by_month %>% 
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, fill=time.factor)) + 
  geom_bar(stat = "identity", color = "blue", fill=rgb(0.1,0.4,0.5,0.7), width = 0.5)+
  theme(legend.position="none")+
  labs(x = "time",
       y = "Uptake within 4 weeks",
       title = "First dose 6in1 Scotland")


Fig1 = Scotland_by_month %>% 
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, fill=time.factor)) + 
  geom_bar(stat = "identity", color = "blue", fill=rgb(0.1,0.4,0.5,0.7), width = 0.5)+
  theme(legend.position="none")+
  labs(x = "time",
       y = "Percent Uptake within 4 weeks",
       title = "First dose 6in1 Scotland")

Fig1 + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1)

ggsave(Fig1, file = "Scotland_by_month.pdf", width = 5, height = 4) 

##Select out relevant weekly data

#Remove unwanted rows (eg summmary months and October)
Selected_Scotland_by_LDperiod = Scotland_firstdose_6in1 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20")))

##Plot uptake weekly. First change cohort into a factor
Selected_Scotland_by_LDperiod = Selected_Scotland_by_LDperiod %>%
  mutate(time.factor = 
           cohort %>% 
           factor()) 

#Relevel to be non alphabetical
Selected_Scotland_by_LDperiod$time.factor = factor(Selected_Scotland_by_LDperiod$time.factor, levels = Selected_Scotland_by_LDperiod$time.factor)

#Recode time.factor to LD period
Selected_Scotland_by_LDperiod = Selected_Scotland_by_LDperiod %>%
  mutate (lockdown.factor = 
            cohort %>% 
            factor() %>% 
            fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#Plot weekly values, flipped so can read
Fig2 = Selected_Scotland_by_LDperiod %>% 
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, fill=lockdown.factor)) + 
  geom_bar(stat = "identity", color = "blue", fill=rgb(0.1,0.4,0.5,0.7), width = 0.5)+
  theme(legend.position="none")+
  labs(x = "time",
       y = "Percent Uptake within 4 weeks",
       title = "First dose 6in1 Scotland by week")

Fig2 + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1)+
  coord_flip()

#Change coloumn colour by LD period and add BW theme and legend. Need to tidy up legend
Fig2 = Selected_Scotland_by_LDperiod %>% 
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  labs(x = "time",
       y = "Percent Uptake within 4 weeks",
       title = "First dose 6in1 Scotland by week")

Fig2 + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1)+
  coord_flip()

##New tibble of data by LD period
Data_summary_by_LDperiod = Selected_Scotland_by_LDperiod %>%
  group_by(lockdown.factor) %>% 
  summarise(denominator, uptake_12weeks_num, uptake_12weeks_percent)

#Add unvaccinated column
Data_summary_by_LDperiod = Data_summary_by_LDperiod %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num)

#Summarise data by period
Data_summary_by_LDperiod = Data_summary_by_LDperiod %>% 
  group_by(lockdown.factor)%>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated), mean_percent = mean(uptake_12weeks_percent))

##Plot percent uptake by lockdown period from data summary tibble
Fig3 = Data_summary_by_LDperiod %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, fill = lockdown.factor)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "Lockdown period",
       y = "Percent Uptake within 4 weeks",
       title = "First dose 6in1 Scotland by week by LD period")

Fig3 + geom_hline(yintercept = 93.9, linetype="dashed", 
                  color = "blue", size=1)

##Turn vaccianted and unvaccinated totals into factors
Data_summary_by_LDperiod = Data_summary_by_LDperiod %>%
  mutate (vaccinated.factor = 
            total_vaccinated %>% 
            factor())

Data_summary_by_LDperiod = Data_summary_by_LDperiod %>%
  mutate (unvaccinated.factor = 
            total_unvaccinated %>% 
            factor())

#To do a boxplot, go back a few steps. First make table of selected data by week
weekly_results_LDperiod = Selected_Scotland_by_LDperiod %>%
  group_by(lockdown.factor) %>% 
  summarise(cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent)

#Add unvaccinated column
weekly_results_LDperiod = weekly_results_LDperiod %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num)

##Turn vaccianted and unvaccinated totals into factors
weekly_results_LDperiod = weekly_results_LDperiod %>%
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor())

weekly_results_LDperiod = weekly_results_LDperiod %>%
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor())
#Box plot of percetage uptake by LD period
weekly_results_LDperiod %>% 
  ggplot(aes(x = lockdown.factor, y=uptake_12weeks_percent)) +
  geom_boxplot()

###ANOVA
Anova_first6in1 = aov(uptake_12weeks_percent ~ lockdown.factor, data = weekly_results_LDperiod)
summary(Anova_first6in1) #there is a signifcant difference somewhere


aov(uptake_12weeks_percent ~ lockdown.factor, data = weekly_results_LDperiod) %>% 
  tidy()

#to do multiple pairwise comparisons 
TukeyHSD(Anova_first6in1) #suggests only significant difference is btw preLD and LD

#check assumpations (homogenicity of variance and normality- see http://www.sthda.com/english/wiki/one-way-anova-test-in-r)
plot(Anova_first6in1, 1)
plot(Anova_first6in1, 2)

###Logistic regression method

##Set up a single tibble- have the baseline as 2019

weekly_results_LDperiod = weekly_results_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Baseline_2019"))

Single_regression_tbl = cbind(weekly_results_LDperiod$uptake_12weeks_num, weekly_results_LDperiod$unvaccinated)

model_scotland_2019_overall = glm(Single_regression_tbl ~ weekly_results_LDperiod$lockdown.factor,
                                family="binomial")

summary(model_scotland_2019_overall)

exp(model_scotland_2019_overall$coefficients)

exp(confint(model_scotland_2019_overall)) ##Baseline 2019 is only significantly different to the LD period


##Change the baseline comparison- compare all to LD period

weekly_results_LDperiod = weekly_results_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("LD_2020"))
                                 
Single_regression_tbl = cbind(weekly_results_LDperiod$uptake_12weeks_num, weekly_results_LDperiod$unvaccinated)                                 

model_scotland_2019_overall = glm(Single_regression_tbl ~ weekly_results_LDperiod$lockdown.factor,
                                  family="binomial")

summary(model_scotland_2019_overall)

exp(model_scotland_2019_overall$coefficients)

exp(confint(model_scotland_2019_overall)) ## suggests that LD was sig different to all other time periods

##Change the baseline comparison- compare all to pre LD
weekly_results_LDperiod = weekly_results_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Pre_LD_2020"))

Single_regression_tbl = cbind(weekly_results_LDperiod$uptake_12weeks_num, weekly_results_LDperiod$unvaccinated)                                 

model_scotland_2019_overall = glm(Single_regression_tbl ~ weekly_results_LDperiod$lockdown.factor,
                                  family="binomial")

summary(model_scotland_2019_overall)

exp(model_scotland_2019_overall$coefficients)

exp(confint(model_scotland_2019_overall)) ##Pre LD was sig different to LD only

##Change the baseline comparison - compare all to Post LD
weekly_results_LDperiod = weekly_results_LDperiod %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("Post_LD_2020"))

Single_regression_tbl = cbind(weekly_results_LDperiod$uptake_12weeks_num, weekly_results_LDperiod$unvaccinated)                                 

model_scotland_2019_overall = glm(Single_regression_tbl ~ weekly_results_LDperiod$lockdown.factor,
                                  family="binomial")

summary(model_scotland_2019_overall)

exp(model_scotland_2019_overall$coefficients)

exp(confint(model_scotland_2019_overall)) ##Post LD was sig different to LD only

###To summarise, using logistical regression; the number of infants vaccinated vs not vaccinated within 4 weeks of being eligible for their first dose of 6in1 vaccine was significantly higher during the LD period compared to all other time periods. There were no significant differences between other time periods.

### Proportions test (see http://www.sthda.com/english/wiki/two-proportions-z-test-in-r)
# Proportion test of 2019 vs LD

Prop_2019_LD = prop.test(x = c(47469,16293), n = c(50555,17164))
Prop_2019_LD

Prop_2019_LD = prop.test(x = c(Data_summary_by_LDperiod[1,3],Data_summary_by_LDperiod[3,3]),n = c(Data_summary_by_LDperiod[1,2],Data_summary_by_LDperiod[3,2]))
Prop_2019_LD


                                 