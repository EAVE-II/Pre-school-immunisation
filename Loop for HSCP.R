###Try to make the loop for HSCP comparing 2019 to LD

#Loading packages
library(tidyverse)
library(here)
library(ggplot2)
library(finalfit)
library(dplyr)
library(RColorBrewer)
library(broom)
library(viridis)


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)


###########Rachel's loop
# Fit model each time for each time-period to get estimates for slopes and intercepts
for(i in 1:3){
  # Redefine baseline level for each loop (once for each time period)
  scotland_data_subset <- within(scotland_data_subset, BA_Pandemic_Lockdown <- relevel(BA_Pandemic_Lockdown, ref= time_periods[i]))
  
  # Refit baseline model
  baseline_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=scotland_data_subset)
  
  # Capture estimates and their 95% CI
  baseline_coefs <- baseline_model$coefficients
  baseline_coefs_cis <- confint(baseline_model)
  
  # Populate the estimate table
  baseline_model_estimates$est[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs[1:2],3)
  
  baseline_model_estimates$lwr[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs_cis[1:2,1],3)
  
  baseline_model_estimates$upr[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs_cis[1:2,2],3)
  
  
  
}


########Another loop from stack overflow https://stackoverflow.com/questions/13418148/efficient-looping-logistic-regression-in-r

for (i in c(1:400000)){
  result<-(glm(mydataframe$O1 ~ mydatamatrix[,i] + as.factor(mydataframe$P2),
               family=binomial))
  row.names(output)<-row.names(mydatamatrix)
  output[i,1]<-coef(summary(result))[2,1]
  output[i,2]<-coef(summary(result))[2,2]
  output[i,3]<-coef(summary(result))[2,3]
  output[i,4]<-coef(summary(result))[2,4]
}
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
