#Scottish analysis to Sept 2020. First dose 6in1

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)

#Loading full dataset for first dose 6in1
Full_firstdose_6in1_download_Jan21 = read.csv(here("Datasets", "First_dose_6in1_downloaded_18_jan_21.csv"))

#Selecting out Scotland wide data
Scotland_firstdose_6in1 = Full_firstdose_6in1_download_Jan21 %>% 
  filter(area_name == "Scotland")

##Define periods
#Baseline 2019
Baseline_2019 = Scotland_firstdose_6in1 %>% 
  filter(cohort == 2019)
#Pre lockdown 2020 (Jan to 23Mar2020 i.e. including W/B 16 Mar)
Pre_LD_2020 = Scotland_firstdose_6in1 %>% 
  filter(cohort %in% c("Jan-20","Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20"))
#Lockdown (W/B 23 Mar to W/B 27Jul)
LD_2020 = Scotland_firstdose_6in1 %>% 
  filter(cohort %in% c("W/B 23-MAR-20", "W/B 30-MAR-20", "W/B 06-APR-20", "W/B 13-APR-20", "W/B 20-APR-20", "W/B 27-APR-20", "W/B 04-MAY-20", "W/B 11-MAY-20", "W/B 18-MAY-20", "W/B 25-MAY-20", "W/B 01-JUN-20", "W/B 08-JUN-20", "W/B 15-JUN-20", "W/B 22-JUN-20", "W/B 29-JUN-20", "W/B 06-JUL-20", "W/B 13-JUL-20", "W/B 20-JUL-20", "	
W/B 27-JUL-20"))
#post lockdown (W/B 3 August til W/B28 Sept)
Post_LD_2020 = Scotland_firstdose_6in1 %>% 
  filter(cohort %in% c("W/B 03-AUG-20", "W/B 10-AUG-20", "W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20", "W/B 28-SEP-20"))
