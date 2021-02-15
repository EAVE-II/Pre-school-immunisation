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

#Plot grouped by SIMD on line plot

First6in1_groupedSIMD_line = SIMD_First6in1_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  expand_limits(y=70)+
  scale_y_continuous(breaks = c(70,75,80,85,90,95,100))
First6in1_groupedSIMD_line

#Monthly by SIMD NB includes a relevel (had to restart R to get this to work not sure why)
First6in1_SIMD_monthlydata = Full_SIMD_First6in1 %>% 
  filter(cohort %in% c("Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))%>% 
  mutate(time.factor = cohort) 

Monthly_First6in1GroupedSIMD = First6in1_SIMD_monthlydata %>% 
  arrange(uptake_12weeks_percent) %>%
  mutate(time.factor = fct_relevel(time.factor, "Jan-20","Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")) %>%
  ggplot(aes(x=time.factor, y=uptake_12weeks_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.x = element_text(angle = 90))+
  expand_limits(y=85)+
  scale_y_continuous(breaks = c(70,75,80,85,90,95,100))
Monthly_First6in1GroupedSIMD
#Add in the LD lines 
Monthly_First6in1GroupedSIMD= Monthly_First6in1GroupedSIMD + geom_vline(xintercept = "Apr-20", linetype="dotted", 
                                                                   color = "blue", size=1) ###LD starts

Monthly_First6in1GroupedSIMD= Monthly_First6in1GroupedSIMD + geom_vline(xintercept = "Aug-20", linetype="dotted", 
                                                                   color = "blue", size=1) ####LD ends
Monthly_First6in1GroupedSIMD= Monthly_First6in1GroupedSIMD + geom_vline(xintercept = "Jun-20", linetype="dotted", 
                                                                   color = "purple", size=1)

Monthly_First6in1GroupedSIMD

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
  labs(x=NULL,y="Absolute % change 2019 vs LD")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))+
  annotate("text", x = 1, y = 2.05, label = "***")+
  annotate("text", x = 2, y = 2.05, label = "***")+
  annotate("text", x = 3, y = 1.55, label = "**")+
  annotate("text", x = 4, y = 0.5, label = "ns")+
  annotate("text", x = 5, y = 0.5, label = "ns")+
  annotate("text", x = 4, y = 1.75, label = "First 6in1")
First6in1_SIMD_percentchange_bar

#### regression analysis for First 6in1 SIMD
#Comparison in 2019- Add unvaccianted column and relevel to most deprived

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

##Log regression to compare change btw 2910 and LD with interaction of SIMD 

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

summary(model_SIMD_First6in1) ##cf 2019, change for SIMD1 was sig increase during LD and post LD but ns for pre LD
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

##Try to plot the forest plots together
SummaryFirst6in1_SIMD2019model_tbl= SummaryFirst6in1_SIMD2019model_tbl %>% 
  mutate(time.factor =  
           "2019" %>% 
           factor())
SummaryFirst6in1_SIMDLDmodel_tbl= SummaryFirst6in1_SIMDLDmodel_tbl %>% 
  mutate(time.factor =  
           "LD" %>% 
           factor())


Merge_First6in1_ORCI_df = rbind(SummaryFirst6in1_SIMD2019model_tbl, SummaryFirst6in1_SIMDLDmodel_tbl) %>% 
  mutate("label" = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD"))

boxLabelsSIMDmerge = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD")

MergeLD2019ORandCI_first6in1_SIMD <- data.frame(
  yAxis = length(boxLabelsSIMDmerge):1,
  boxOdds = c(Merge_First6in1_ORCI_df$OR),
  boxCILow = c(Merge_First6in1_ORCI_df$upperCI),
  boxCIHigh = c(Merge_First6in1_ORCI_df$lowerCI), 
  time = c(Merge_First6in1_ORCI_df$time.factor))

MergeLD2019ORandCI_first6in1_SIMD_forest <- ggplot(MergeLD2019ORandCI_first6in1_SIMD, aes(x = boxOdds, y = boxLabelsSIMDmerge, colour = time))
MergeLD2019ORandCI_first6in1_SIMD_forest = MergeLD2019ORandCI_first6in1_SIMD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5) +
  theme_bw()+
  labs(x = "First6in1 OR compared to most deprived",
       y = NULL,
       title = NULL)+
  theme(legend.position="none")

MergeLD2019ORandCI_first6in1_SIMD_forest

##Running the interaction model will tell you the odds and CI of 1. your baseline and the time points 2.your baseline and the other SIMD for the baseline timepoint 3. If you multiple the OR (ie the exp(coeff)) of the ineraction term e.g. 	
#(tpLD:SIMD5 OR) with the baseline SMID OR (SMID 5) you will get the OR of your baseline SMID vs SMID 5 for that time period e.g. 0.85x2.27 = 1.9 = OR of being vaccinated in SMID5 compared to SIMD 1 in LD. 

write_csv(First6in1_SIMDinteraction_tbl, file = "Exported tables/First6in1_SIMDinteraction_tbl.csv")

##Run the interaction model again but change the baseline to different SIMD levels. Will give you the comparisons for that level btw tp and the other SIMD
#Baseline least deprived:

SIMD_First6in1_grouped = SIMD_First6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_First6in1_grouped_BL5 = SIMD_First6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("5 - least deprived"))


First6in1_interaction_tbl_BL5 <- cbind(SIMD_First6in1_grouped_BL5$total_vaccinated, SIMD_First6in1_grouped_BL5$unvaccinated)

# Column for time-period
tp <- SIMD_First6in1_grouped_BL5$lockdown.factor
# Column for HSCP
SIMD <- SIMD_First6in1_grouped_BL5$SIMD.factor
# Put into GLM with interaction
model_SIMD_First6in1_BL5<- glm(First6in1_interaction_tbl_BL5 ~ tp*SIMD,
                           family="binomial")                                 

summary(model_SIMD_First6in1_BL5) ##cf 2019, change for SIMD5 was ns for other time periods
exp(model_SIMD_First6in1_BL5$coefficients)
exp(confint(model_SIMD_First6in1_BL5))

library(broom)
First6in1_SIMDinteraction_tbl_BL5 = model_SIMD_First6in1_BL5%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
First6in1_SIMDinteraction_tbl_BL5

write_csv(First6in1_SIMDinteraction_tbl_BL5, file = "Exported tables/First6in1_SIMDinteraction_tbl_BL5.csv")
#Baseline SIMD4
SIMD_First6in1_grouped_BL4 = SIMD_First6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("4"))

First6in1_interaction_tbl_BL4 <- cbind(SIMD_First6in1_grouped_BL4$total_vaccinated, SIMD_First6in1_grouped_BL4$unvaccinated)

# Column for time-period
tp <- SIMD_First6in1_grouped_BL4$lockdown.factor
# Column for HSCP
SIMD <- SIMD_First6in1_grouped_BL4$SIMD.factor
# Put into GLM with interaction
model_SIMD_First6in1_BL4<- glm(First6in1_interaction_tbl_BL4 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_First6in1_BL4) ##cf 2019, change for SIMD 4 was ns for preLD and LD but sig decreased post LD
exp(model_SIMD_First6in1_BL4$coefficients)
exp(confint(model_SIMD_First6in1_BL4))

library(broom)
First6in1_SIMDinteraction_tbl_BL4 = model_SIMD_First6in1_BL4%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
First6in1_SIMDinteraction_tbl_BL4
write_csv(First6in1_SIMDinteraction_tbl_BL4, file = "Exported tables/First6in1_SIMDinteraction_tbl_BL4.csv")

#Baseline SIMD3
SIMD_First6in1_grouped_BL3 = SIMD_First6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("3"))

First6in1_interaction_tbl_BL3 <- cbind(SIMD_First6in1_grouped_BL3$total_vaccinated, SIMD_First6in1_grouped_BL3$unvaccinated)

# Column for time-period
tp <- SIMD_First6in1_grouped_BL3$lockdown.factor
# Column for HSCP
SIMD <- SIMD_First6in1_grouped_BL3$SIMD.factor
# Put into GLM with interaction
model_SIMD_First6in1_BL3<- glm(First6in1_interaction_tbl_BL3 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_First6in1_BL3) ##cf 2019, change for SIMD 3  sig increase during LD and post LD, no change preLD
exp(model_SIMD_First6in1_BL3$coefficients)
exp(confint(model_SIMD_First6in1_BL3))

library(broom)
First6in1_SIMDinteraction_tbl_BL3 = model_SIMD_First6in1_BL3%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
First6in1_SIMDinteraction_tbl_BL3
write_csv(First6in1_SIMDinteraction_tbl_BL3, file = "Exported tables/First6in1_SIMDinteraction_tbl_BL3.csv")

#Baseline SIMD2
SIMD_First6in1_grouped_BL2 = SIMD_First6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("2"))

First6in1_interaction_tbl_BL2 <- cbind(SIMD_First6in1_grouped_BL2$total_vaccinated, SIMD_First6in1_grouped_BL2$unvaccinated)

# Column for time-period
tp <- SIMD_First6in1_grouped_BL2$lockdown.factor
# Column for HSCP
SIMD <- SIMD_First6in1_grouped_BL2$SIMD.factor
# Put into GLM with interaction
model_SIMD_First6in1_BL2<- glm(First6in1_interaction_tbl_BL2 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_First6in1_BL2) ##cf 2019, change for SIMD 2 ns for preLD, or post LD sig increase during LD 
exp(model_SIMD_First6in1_BL2$coefficients)
exp(confint(model_SIMD_First6in1_BL2))

library(broom)
First6in1_SIMDinteraction_tbl_BL2 = model_SIMD_First6in1_BL2%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
First6in1_SIMDinteraction_tbl_BL2
write_csv(First6in1_SIMDinteraction_tbl_BL2, file = "Exported tables/First6in1_SIMDinteraction_tbl_BL2.csv")

####################################################################
######As above but for second dose 6in1. Removed monthly plots
Full_SIMD_Second6in1 = read.csv(here("Data", "Deprivation_Second_6in1_11_feb_21.csv"))
#Select out 2019 data
SIMD_Second6in1_2019 = Full_SIMD_Second6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_12weeks_2019_num", "children_rec_imm_16weeks_2019_num", "uptake_16weeks_2019_percent")
colnames(SIMD_Second6in1_2019) = c("cohort", "deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent")
SIMD_Second6in1_2019 = SIMD_Second6in1_2019 %>%  
  filter(cohort=="Jan-20") %>% 
  select("deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent") %>% 
  mutate(lockdown.factor = "2019")

#Select out 2020 data
SIMD_Second6in1_2020 = Full_SIMD_Second6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_12weeks_num", "children_rec_imm_16weeks_num", "uptake_16weeks_percent")
colnames(SIMD_Second6in1_2020) = c("cohort", "deprivation_quintile", "denominator", "num_vaccinated", "percent_uptake")
SIMD_Second6in1_2020 = SIMD_Second6in1_2020 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
SIMD_Second6in1_2020 = SIMD_Second6in1_2020 %>% 
  mutate (lockdown.factor = 
            cohort %>%
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) 
#Select time periods
SIMD_Second6in1_preLD = SIMD_Second6in1_2020 %>% 
  filter(lockdown.factor=="Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PreLD")
SIMD_Second6in1_LD = SIMD_Second6in1_2020 %>% 
  filter(lockdown.factor=="LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="LD")
SIMD_Second6in1_PostLD = SIMD_Second6in1_2020 %>% 
  filter(lockdown.factor=="Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PostLD")

#Group together then plot

SIMD_Second6in1_grouped = full_join(SIMD_Second6in1_2019, SIMD_Second6in1_preLD)  
SIMD_Second6in1_grouped = full_join(SIMD_Second6in1_grouped, SIMD_Second6in1_LD)
SIMD_Secondin1_grouped = full_join(SIMD_Second6in1_grouped, SIMD_Second6in1_PostLD)
SIMD_Second6in1_grouped = SIMD_Second6in1_grouped %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("2019"))

SIMD_Second6in1_grouped$lockdown.factor = factor(SIMD_Second6in1_grouped$lockdown.factor, levels = c('2019', 'PreLD', 'LD', 'PostLD'))


Second6in1_SIMD_grouped = SIMD_Second6in1_grouped %>%
  ggplot(aes(fill=lockdown.factor, y=mean_percent, x=deprivation_quintile)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Greens")+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 84.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "Green", size=0.75)+
  geom_hline(yintercept = 90, linetype="dotted", ##Scotland wide mean uptake LD
             color = "black", size=0.75)

Second6in1_SIMD_grouped ##############for some reason hasn't plotted postLD

#Plot grouped by SIMD on line plot

Second6in1_groupedSIMD_line = SIMD_Second6in1_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_bw()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose6in1")+
  expand_limits(y=70)+
  scale_y_continuous(breaks = c(70,75,80,85,90,95,100))
Second6in1_groupedSIMD_line


##Plot percent change
#Second6in1, first format tables of percent only then join 2019 and LD
SIMD_Second6in1_percent_2019 = SIMD_Second6in1_2019 %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_Second6in1_percent_2019)= c("deprivation_quintile", "uptake_2019_percent")

SIMD_Second6in1_percent_LD = SIMD_Second6in1_LD %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_Second6in1_percent_LD)= c("deprivation_quintile", "uptake_LD_percent")

SIMD_Second6in1_percentchange = full_join(SIMD_Second6in1_percent_2019, SIMD_Second6in1_percent_LD) 
SIMD_Second6in1_percentchange = SIMD_Second6in1_percentchange %>% 
  mutate(percent_change = uptake_LD_percent - uptake_2019_percent)
#Plot percent change Second 6in1 NBB to get the significant levels you need to do the regression below
library(RColorBrewer)
Second6in1_SIMD_percentchange_bar = SIMD_Second6in1_percentchange %>% 
  ggplot(aes(y=percent_change, x=deprivation_quintile)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL,y="Absolute % change 2019 vs LD")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))+
  annotate("text", x = 1, y = 6.8, label = "***")+
  annotate("text", x = 2, y = 6.8, label = "***")+
  annotate("text", x = 3, y = 4.7, label = "***")+
  annotate("text", x = 4, y = 4.7, label = "***")+
  annotate("text", x = 5, y = 4.7, label = "***")+
  annotate("text", x = 4, y = 6, label = "Second 6in1")
Second6in1_SIMD_percentchange_bar

#### regression analysis for Second 6in1 SIMD
#Comparison in 2019- Add unvaccianted column and relevel to most deprived

SIMD_Second6in1_2019 = SIMD_Second6in1_2019 %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_Second6in1_2019 = SIMD_Second6in1_2019 %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_Second6in1_SIMD_regression_tbl_2019 = cbind(SIMD_Second6in1_2019$total_vaccinated, SIMD_Second6in1_2019$unvaccinated)

Summarymodel_second6in1_SIMD_2019 = glm(Summary_Second6in1_SIMD_regression_tbl_2019 ~ SIMD_Second6in1_2019$SIMD.factor,
                                       family="binomial")

summary(Summarymodel_second6in1_SIMD_2019)

exp(Summarymodel_second6in1_SIMD_2019$coefficients)
exp(confint(Summarymodel_second6in1_SIMD_2019))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecond6in1_SIMD2019model_tbl = Summarymodel_second6in1_SIMD_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_second6in1_SIMD2019 <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummarySecond6in1_SIMD2019model_tbl$OR),
  boxCILow = c(SummarySecond6in1_SIMD2019model_tbl$upperCI),
  boxCIHigh = c(SummarySecond6in1_SIMD2019model_tbl$lowerCI))

SummarySecond6in1_SIMD2019_forest <- ggplot(SummaryORandCI_second6in1_SIMD2019, aes(x = boxOdds, y = boxLabelsSIMD))
SummarySecond6in1_SIMD2019_forest = SummarySecond6in1_SIMD2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "Summary2019 OR compared to most deprived",
       y = NULL,
       title = NULL)

SummarySecond6in1_SIMD2019_forest

anova(Summarymodel_second6in1_SIMD_2019, test="LRT")

#Comparison in LD- Add unvaccinated column and relevel to most deprived

SIMD_Second6in1_LD = SIMD_Second6in1_LD %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_Second6in1_LD = SIMD_Second6in1_LD %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_Second6in1_SIMD_regression_tbl_LD = cbind(SIMD_Second6in1_LD$total_vaccinated, SIMD_Second6in1_LD$unvaccinated)

Summarymodel_second6in1_SIMD_LD = glm(Summary_Second6in1_SIMD_regression_tbl_LD ~ SIMD_Second6in1_LD$SIMD.factor,
                                     family="binomial")

summary(Summarymodel_second6in1_SIMD_LD)

exp(Summarymodel_second6in1_SIMD_LD$coefficients)
exp(confint(Summarymodel_second6in1_SIMD_LD))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecond6in1_SIMDLDmodel_tbl = Summarymodel_second6in1_SIMD_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCIsecond6in1_SIMDLD <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummarySecond6in1_SIMDLDmodel_tbl$OR),
  boxCILow = c(SummarySecond6in1_SIMDLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecond6in1_SIMDLDmodel_tbl$lowerCI))

SummarySecond6in1_SIMDLD_forest <- ggplot(SummaryORandCIsecond6in1_SIMDLD, aes(x = boxOdds, y = boxLabelsSIMD))
SummarySecond6in1_SIMDLD_forest = SummarySecond6in1_SIMDLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to most deprived",
       y = NULL,
       title = NULL)

SummarySecond6in1_SIMDLD_forest

anova(Summarymodel_second6in1_SIMD_LD, test="LRT")

##Try to plot the forest plots together
SummarySecond6in1_SIMD2019model_tbl= SummarySecond6in1_SIMD2019model_tbl %>% 
  mutate(time.factor =  
           "2019" %>% 
           factor())
SummarySecond6in1_SIMDLDmodel_tbl= SummarySecond6in1_SIMDLDmodel_tbl %>% 
  mutate(time.factor =  
           "LD" %>% 
           factor())


Merge_Second6in1_ORCI_df = rbind(SummarySecond6in1_SIMD2019model_tbl, SummarySecond6in1_SIMDLDmodel_tbl) %>% 
  mutate("label" = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD"))

boxLabelsSIMDmerge = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD")

MergeLD2019ORandCI_second6in1_SIMD <- data.frame(
  yAxis = length(boxLabelsSIMDmerge):1,
  boxOdds = c(Merge_Second6in1_ORCI_df$OR),
  boxCILow = c(Merge_Second6in1_ORCI_df$upperCI),
  boxCIHigh = c(Merge_Second6in1_ORCI_df$lowerCI), 
  time = c(Merge_Second6in1_ORCI_df$time.factor))

MergeLD2019ORandCI_second6in1_SIMD_forest <- ggplot(MergeLD2019ORandCI_second6in1_SIMD, aes(x = boxOdds, y = boxLabelsSIMDmerge, colour = time))
MergeLD2019ORandCI_second6in1_SIMD_forest = MergeLD2019ORandCI_second6in1_SIMD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5) +
  theme_bw()+
  labs(x = "Second6in1 OR compared to most deprived",
       y = NULL,
       title = NULL)+
  theme(legend.position="none")

MergeLD2019ORandCI_second6in1_SIMD_forest

##Log regression to compare change btw 2019 and LD with interaction of SIMD 

SIMD_Second6in1_grouped = SIMD_Second6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_Second6in1_grouped = SIMD_Second6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


Second6in1_interaction_tbl <- cbind(SIMD_Second6in1_grouped$total_vaccinated, SIMD_Second6in1_grouped$unvaccinated)

# Column for time-period
tp <- SIMD_Second6in1_grouped$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Second6in1_grouped$SIMD.factor
# Put into GLM with interaction
model_SIMD_Second6in1<- glm(Second6in1_interaction_tbl ~ tp*SIMD,
                           family="binomial")                                 

summary(model_SIMD_Second6in1) ##cf 2019, change for SIMD1 was sig increase during LD but ns for pre LD
exp(model_SIMD_Second6in1$coefficients)
exp(confint(model_SIMD_Second6in1))

library(broom)
Second6in1_SIMDinteraction_tbl = model_SIMD_Second6in1%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(Second6in1_SIMDinteraction_tbl, file = "Exported tables/Second6in1_SIMDinteraction_tbl.csv")

##Run the interaction model again but change the baseline to different SIMD levels. Will give you the comparisons for that level btw tp and the other SIMD
#Baseline least deprived:

SIMD_Second6in1_grouped = SIMD_Second6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_Second6in1_grouped_BL5 = SIMD_Second6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("5 - least deprived"))

Second6in1_interaction_tbl_BL5 <- cbind(SIMD_Second6in1_grouped_BL5$total_vaccinated, SIMD_Second6in1_grouped_BL5$unvaccinated)

# Column for time-period
tp <- SIMD_Second6in1_grouped_BL5$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Second6in1_grouped_BL5$SIMD.factor
# Put into GLM with interaction
model_SIMD_Second6in1_BL5<- glm(Second6in1_interaction_tbl_BL5 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_Second6in1_BL5) ##cf 2019, change for SIMD5 was ns for prelD but sig for LD***
exp(model_SIMD_Second6in1_BL5$coefficients)
exp(confint(model_SIMD_Second6in1_BL5))

library(broom)
Second6in1_SIMDinteraction_tbl_BL5 = model_SIMD_Second6in1_BL5%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(Second6in1_SIMDinteraction_tbl_BL5, file = "Exported tables/Second6in1_SIMDinteraction_tbl_BL5.csv")

#Baseline SIMD4
SIMD_Second6in1_grouped_BL4 = SIMD_Second6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("4"))

Second6in1_interaction_tbl_BL4 <- cbind(SIMD_Second6in1_grouped_BL4$total_vaccinated, SIMD_Second6in1_grouped_BL4$unvaccinated)

# Column for time-period
tp <- SIMD_Second6in1_grouped_BL4$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Second6in1_grouped_BL4$SIMD.factor
# Put into GLM with interaction
model_SIMD_Second6in1_BL4<- glm(Second6in1_interaction_tbl_BL4 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_Second6in1_BL4) ##cf 2019, change for SIMD 4 was ns for preLD but sig for LD ***
exp(model_SIMD_Second6in1_BL4$coefficients)
exp(confint(model_SIMD_Second6in1_BL4))

library(broom)
Second6in1_SIMDinteraction_tbl_BL4 = model_SIMD_Second6in1_BL4%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
write_csv(Second6in1_SIMDinteraction_tbl_BL4, file = "Exported tables/Second6in1_SIMDinteraction_tbl_BL4.csv")

#Baseline SIMD3
SIMD_Second6in1_grouped_BL3 = SIMD_Second6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("3"))

Second6in1_interaction_tbl_BL3 <- cbind(SIMD_Second6in1_grouped_BL3$total_vaccinated, SIMD_Second6in1_grouped_BL3$unvaccinated)

# Column for time-period
tp <- SIMD_Second6in1_grouped_BL3$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Second6in1_grouped_BL3$SIMD.factor
# Put into GLM with interaction
model_SIMD_Second6in1_BL3<- glm(Second6in1_interaction_tbl_BL3 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_Second6in1_BL3) ##cf 2019, change for SIMD 3  sig increase during LD *** no change preLD
exp(model_SIMD_Second6in1_BL3$coefficients)
exp(confint(model_SIMD_Second6in1_BL3))

library(broom)
Second6in1_SIMDinteraction_tbl_BL3 = model_SIMD_Second6in1_BL3%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
Second6in1_SIMDinteraction_tbl_BL3
write_csv(Second6in1_SIMDinteraction_tbl_BL3, file = "Exported tables/Second6in1_SIMDinteraction_tbl_BL3.csv")

#Baseline SIMD2
SIMD_Second6in1_grouped_BL2 = SIMD_Second6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("2"))

Second6in1_interaction_tbl_BL2 <- cbind(SIMD_Second6in1_grouped_BL2$total_vaccinated, SIMD_Second6in1_grouped_BL2$unvaccinated)

# Column for time-period
tp <- SIMD_Second6in1_grouped_BL2$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Second6in1_grouped_BL2$SIMD.factor
# Put into GLM with interaction
model_SIMD_Second6in1_BL2<- glm(Second6in1_interaction_tbl_BL2 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_Second6in1_BL2) ##cf 2019, change for SIMD 2 ns for preLD, sig increase during LD 
exp(model_SIMD_Second6in1_BL2$coefficients)
exp(confint(model_SIMD_Second6in1_BL2))

library(broom)
Second6in1_SIMDinteraction_tbl_BL2 = model_SIMD_Second6in1_BL2%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
Second6in1_SIMDinteraction_tbl_BL2
write_csv(Second6in1_SIMDinteraction_tbl_BL2, file = "Exported tables/Second6in1_SIMDinteraction_tbl_BL2.csv")
