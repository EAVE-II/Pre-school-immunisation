#######################
##Figure 3 SIMD data by LD period
##All vaccines, Scotland wide
##Data extracted from dashboard 11rd Feb 2021

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
library(forcats)

#Loading datasets

Full_SIMD_First6in1 =  read.csv(here("Data", "Deprivation_First_6in1_11_feb_21.csv"))
Full_SIMD_Second6in1 = read.csv(here("Data", "Deprivation_Second_6in1_11_feb_21.csv"))
Full_SIMD_Third6in1 = read.csv(here("Data", "Deprivation_Third_6in1_11_feb_21.csv"))
Full_SIMD_FirstMMR = read.csv(here("Data", "Deprivation_First_MMR_11_feb_21.csv"))
Full_SIMD_SecondMMR = read.csv(here("Data", "Deprivation_Second_MMR_11_feb_21.csv"))

#Select out 2019 data
SIMD_First6in1_2019 = Full_SIMD_First6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_8weeks_2019_num", "children_rec_imm_12weeks_2019_num", "uptake_12weeks_2019_percent")
colnames(SIMD_First6in1_2019) = c("cohort", "deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent")
SIMD_First6in1_2019 = SIMD_First6in1_2019 %>%  
  filter(cohort=="Jan-20") %>% 
  select("deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent") %>% 
  mutate(lockdown.factor = "2019")

#Select out 2020 data
SIMD_First6in1_2020 = Full_SIMD_First6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_8weeks_num", "children_rec_imm_12weeks_num", "uptake_12weeks_percent")
colnames(SIMD_First6in1_2020) = c("cohort", "deprivation_quintile", "denominator", "num_vaccinated", "percent_uptake")
SIMD_First6in1_2020 = SIMD_First6in1_2020 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
SIMD_First6in1_2020 = SIMD_First6in1_2020 %>% 
  mutate (lockdown.factor = 
            cohort %>%
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) 
#Select time periods
SIMD_First6in1_preLD = SIMD_First6in1_2020 %>% 
  filter(lockdown.factor=="Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PreLD")
SIMD_First6in1_LD = SIMD_First6in1_2020 %>% 
  filter(lockdown.factor=="LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="LD")
SIMD_First6in1_PostLD = SIMD_First6in1_2020 %>% 
  filter(lockdown.factor=="Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PostLD")
#Group together then plot

SIMD_First6in1_grouped = full_join(SIMD_First6in1_2019, SIMD_First6in1_preLD)  
SIMD_First6in1_grouped = full_join(SIMD_First6in1_grouped, SIMD_First6in1_LD)
SIMD_First6in1_grouped = full_join(SIMD_First6in1_grouped, SIMD_First6in1_PostLD)
SIMD_First6in1_grouped = SIMD_First6in1_grouped %>% 
  mutate(lockdown.factor = lockdown.factor %>%
                  fct_relevel("2019"))

SIMD_First6in1_grouped$lockdown.factor = factor(SIMD_First6in1_grouped$lockdown.factor, levels = c('2019', 'PreLD', 'LD', 'PostLD'))


First6in1_SIMD_grouped = SIMD_First6in1_grouped %>%
  ggplot(aes(fill=lockdown.factor, y=mean_percent, x=deprivation_quintile)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 93.9, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "red", size=0.75)+
  geom_hline(yintercept = 94.9, linetype="dotted", ##Scotland wide mean uptake LD
             color = "blue", size=0.75)

First6in1_SIMD_grouped

