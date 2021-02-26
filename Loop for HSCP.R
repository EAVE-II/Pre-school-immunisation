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
  First6in1_HSCPORtbl[i,12] <- odd_ratios[3]
  First6in1_HSCPORtbl[i,13] <- odd_ratio_confint[3,1]
  First6in1_HSCPORtbl[i,14] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  First6in1_HSCPORtbl[i,15] <- model_pvalues[3]
  
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
  Second6in1_HSCPORtbl[i,12] <- odd_ratios[3]
  Second6in1_HSCPORtbl[i,13] <- odd_ratio_confint[3,1]
  Second6in1_HSCPORtbl[i,14] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Second6in1_HSCPORtbl[i,15] <- model_pvalues[3]
  
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
  Third6in1_HSCPORtbl[i,12] <- odd_ratios[3]
  Third6in1_HSCPORtbl[i,13] <- odd_ratio_confint[3,1]
  Third6in1_HSCPORtbl[i,14] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  Third6in1_HSCPORtbl[i,15] <- model_pvalues[3]
  
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
  FirstMMR_HSCPORtbl[i,12] <- odd_ratios[3]
  FirstMMR_HSCPORtbl[i,13] <- odd_ratio_confint[3,1]
  FirstMMR_HSCPORtbl[i,14] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  FirstMMR_HSCPORtbl[i,15] <- model_pvalues[3]
  
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
  SecondMMR_HSCPORtbl[i,12] <- odd_ratios[3]
  SecondMMR_HSCPORtbl[i,13] <- odd_ratio_confint[3,1]
  SecondMMR_HSCPORtbl[i,14] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  SecondMMR_HSCPORtbl[i,15] <- model_pvalues[3]
  
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
##Set up Full section tbl first= easiest just to run Figure 2 code


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
