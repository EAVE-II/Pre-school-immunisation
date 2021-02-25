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


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)

#################
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

 ###For fun, plot if sig or not

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


################Rachels code starts here

# Filter to NHS HBs and 2019 vs Jan-Mar 2020

data_6in1_first_time_period_hb <- data_6in1_first_time_period %>%
  filter(str_detect(Area_name, "NHS")) %>% # Only filter to NHS HB
  select(Area_name, Time_period, Total, Total_immun, Total_not_immun) %>% # Select variables of interest
  mutate(Area_name = as.factor(Area_name)) # Make area name (NHS HB) a factor (for loop)
#done

# Create list of NHS HBs as a character
hb_levels <- levels(data_6in1_first_time_period_hb$Area_name)
#done

# Create an empty data frame to store coefficients in
OR_tbl2 <- data.frame(NHS_HB = hb_levels, OR_2019_JanMar = NA, OR_2019_JanMar_lwr = NA, 
                      OR_2019_JanMar_upr = NA, OR_2019_JanMar_pvalue = NA, OR_2019_JanMar_sig_diff =NA, 
                      OR_2019_AprAug = NA, OR_2019_AprAug_lwr = NA, 
                      OR_2019_AprAug_upr = NA, OR_2019_AprAug_pvalue = NA, OR_2019_AprAug_sig_diff =NA)
#done

i <- 2

for(i in 1:nrow(OR_tbl)){
  # Set reference level
  data_6in1_first_time_period_hb <- within(data_6in1_first_time_period_hb, Area_name <- relevel(Area_name, ref=hb_levels[i]))
#done
  
  # Extract columns of immunised and non-immunised for all NHS Health Boards and Time-periods
  tbl_immun_hb <- cbind(data_6in1_first_time_period_hb$Total_immun, data_6in1_first_time_period_hb$Total_not_immun)
  # Column for time-period
  tp <- data_6in1_first_time_period_hb$Time_period
  # Column for Health Board
  hb <- data_6in1_first_time_period_hb$Area_name
  
  #done
  
  # Put into GLM with interaction
  model_hb<- glm(tbl_immun_hb ~ tp*hb,
                 family="binomial")
  
  #done
  
  # Extract odd ratios and 95% CI
  odd_ratios <- exp(model_hb$coefficients)
  odd_ratio_confint <- exp(confint.default(model_hb))
  model_pvalues <- summary(model_hb)$coefficients[,4]
  #Done
  
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  OR_tbl2[i,2] <- odd_ratios[2]
  OR_tbl2[i,3] <- odd_ratio_confint[2,1]
  OR_tbl2[i,4] <- odd_ratio_confint[2,2]
  
  #done
  
  # Put in p-value and decide if sig diff (<0.05)
  OR_tbl2[i,5] <- model_pvalues[2]
  
  OR_tbl2[i,6] <- ifelse(OR_tbl2[i,5] < 0.05, 1, 0)
  #done
  
  # Repeat for Apr-Aug
  # For 2019 vs Jan-Mar for NHS HB level of interest (ith one), the OR of interest is the tp
  OR_tbl2[i,7] <- odd_ratios[3]
  OR_tbl2[i,8] <- odd_ratio_confint[3,1]
  OR_tbl2[i,9] <- odd_ratio_confint[3,2]
  
  # Put in p-value and decide if sig diff (<0.05)
  OR_tbl2[i,10] <- model_pvalues[3]
  
  OR_tbl2[i,11] <- ifelse(OR_tbl2[i,10] < 0.05, 1, 0)
}


#done
OR_tbl2


summary(model_hb)
anova(model_hb, test="LRT")

##########################END of loop from Rachel









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
