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
  scale_fill_brewer(palette = "Greens")+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 93.9, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "Green", size=0.75)+
  geom_hline(yintercept = 94.9, linetype="dotted", ##Scotland wide mean uptake LD
             color = "black", size=0.75)

First6in1_SIMD_grouped

##Plot percent change
#First6in1, first format tables of percent only then join 2019 and LD
SIMD_First6in1_percent_2019 = SIMD_First6in1_2019 %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_First6in1_percent_2019)= c("deprivation_quintile", "uptake_2019_percent")

SIMD_First6in1_percent_LD = SIMD_First6in1_LD %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_First6in1_percent_LD)= c("deprivation_quintile", "uptake_LD_percent")

SIMD_First6in1_percentchange = full_join(SIMD_First6in1_percent_2019, SIMD_First6in1_percent_LD) 
SIMD_First6in1_percentchange = SIMD_First6in1_percentchange %>% 
  mutate(percent_change = uptake_LD_percent - uptake_2019_percent)
#Plot percent change first 6in1
library(RColorBrewer)
First6in1_SIMD_percentchange_bar = SIMD_First6in1_percentchange %>% 
  ggplot(aes(y=percent_change, x=deprivation_quintile)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))
First6in1_SIMD_percentchange_bar

#### regression analysis for First 6in1 SIMD
#Comaprison in 2019- Add unvaccianted column and relevel to most deprived

SIMD_First6in1_2019 = SIMD_First6in1_2019 %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_First6in1_2019 = SIMD_First6in1_2019 %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_First6in1_SIMD_regression_tbl_2019 = cbind(SIMD_First6in1_2019$total_vaccinated, SIMD_First6in1_2019$unvaccinated)

Summarymodel_first6in1_SIMD_2019 = glm(Summary_First6in1_SIMD_regression_tbl_2019 ~ SIMD_First6in1_2019$SIMD.factor,
                                     family="binomial")

summary(Summarymodel_first6in1_SIMD_2019)

exp(Summarymodel_first6in1_SIMD_2019$coefficients)
exp(confint(Summarymodel_first6in1_SIMD_2019))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirst6in1_SIMD2019model_tbl = Summarymodel_first6in1_SIMD_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_first6in1_SIMD2019 <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryFirst6in1_SIMD2019model_tbl$OR),
  boxCILow = c(SummaryFirst6in1_SIMD2019model_tbl$upperCI),
  boxCIHigh = c(SummaryFirst6in1_SIMD2019model_tbl$lowerCI))

SummaryFirst6in1_SIMD2019_forest <- ggplot(SummaryORandCI_first6in1_SIMD2019, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryFirst6in1_SIMD2019_forest = SummaryFirst6in1_SIMD2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "Summary2019 OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryFirst6in1_SIMD2019_forest

anova(Summarymodel_first6in1_SIMD_2019, test="LRT")

#Comparison in LD- Add unvaccinated column and relevel to most deprived

SIMD_First6in1_LD = SIMD_First6in1_LD %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_First6in1_LD = SIMD_First6in1_LD %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_First6in1_SIMD_regression_tbl_LD = cbind(SIMD_First6in1_LD$total_vaccinated, SIMD_First6in1_LD$unvaccinated)

Summarymodel_first6in1_SIMD_LD = glm(Summary_First6in1_SIMD_regression_tbl_LD ~ SIMD_First6in1_LD$SIMD.factor,
                                       family="binomial")

summary(Summarymodel_first6in1_SIMD_LD)

exp(Summarymodel_first6in1_SIMD_LD$coefficients)
exp(confint(Summarymodel_first6in1_SIMD_LD))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirst6in1_SIMDLDmodel_tbl = Summarymodel_first6in1_SIMD_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_first6in1_SIMDLD <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryFirst6in1_SIMDLDmodel_tbl$OR),
  boxCILow = c(SummaryFirst6in1_SIMDLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirst6in1_SIMDLDmodel_tbl$lowerCI))

SummaryFirst6in1_SIMDLD_forest <- ggplot(SummaryORandCI_first6in1_SIMDLD, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryFirst6in1_SIMDLD_forest = SummaryFirst6in1_SIMDLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryFirst6in1_SIMDLD_forest

anova(Summarymodel_first6in1_SIMD_LD, test="LRT")

##Log regression to copmare change btw 2910 and LD with interaction of SIMD 

SIMD_First6in1_grouped = SIMD_First6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_First6in1_grouped = SIMD_First6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
         fct_relevel("1 - most deprived"))


First6in1_interaction_tbl <- cbind(SIMD_First6in1_grouped$total_vaccinated, SIMD_First6in1_grouped$unvaccinated)

# Column for time-period
tp <- SIMD_First6in1_grouped$lockdown.factor
# Column for HSCP
SIMD <- SIMD_First6in1_grouped$SIMD.factor
# Put into GLM with interaction
model_SIMD_First6in1<- glm(First6in1_interaction_tbl ~ tp*SIMD,
                 family="binomial")                                 

summary(model_SIMD_First6in1)
exp(model_SIMD_First6in1$coefficients)
exp(confint(model_SIMD_First6in1))

library(broom)
First6in1_SIMDinteraction_tbl = model_SIMD_First6in1%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
#####With this interaction model, you can find the effect of the ineraction btw time point nad SIMD 
#(in this case baseline timepoint is 2019 and baseline SIMD is 1) by multiplying the effect of just the SIMD 
#(from the table, the OR for SIMD5 = 2.27) by the exp(coeff) of the tpLD:SIMD5 (from the table this is 0.85). See the healthy R book section 9.2.6
#This give the same info and values as comparing the the two forest plots. ie the interaction fo time point decreases the difference in odds
#btw SIMD 1 and 5. My interpretations is that this is decreasing the inequality. 
###Need to work on combining the forest plot tables. 