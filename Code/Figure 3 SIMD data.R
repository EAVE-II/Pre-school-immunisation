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
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

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
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1")+
  expand_limits(y=85:100)+
  scale_y_continuous(breaks = c(85,90,95,100))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
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
SIMD_Second6in1_grouped = full_join(SIMD_Second6in1_grouped, SIMD_Second6in1_PostLD)
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

Second6in1_SIMD_grouped 

#Plot grouped by SIMD on line plot

Second6in1_groupedSIMD_line = SIMD_Second6in1_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose 6in1")+
  expand_limits(y=75:95)+
  scale_y_continuous(breaks = c(70,75,80,85,90,95))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
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

####################################################################
######As above but for third dose 6in1. Removed monthly plots
Full_SIMD_Third6in1 = read.csv(here("Data", "Deprivation_Third_6in1_11_feb_21.csv"))
#Select out 2019 data
SIMD_Third6in1_2019 = Full_SIMD_Third6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_16weeks_2019_num", "children_rec_imm_20weeks_2019_num", "uptake_20weeks_2019_percent")
colnames(SIMD_Third6in1_2019) = c("cohort", "deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent")
SIMD_Third6in1_2019 = SIMD_Third6in1_2019 %>%  
  filter(cohort=="Jan-20") %>% 
  select("deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent") %>% 
  mutate(lockdown.factor = "2019")

#Select out 2020 data
SIMD_Third6in1_2020 = Full_SIMD_Third6in1 %>% 
  select("cohort", "deprivation_quintile", "children_turn_16weeks_num", "children_rec_imm_20weeks_num", "uptake_20weeks_percent")
colnames(SIMD_Third6in1_2020) = c("cohort", "deprivation_quintile", "denominator", "num_vaccinated", "percent_uptake")
SIMD_Third6in1_2020 = SIMD_Third6in1_2020 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
SIMD_Third6in1_2020 = SIMD_Third6in1_2020 %>% 
  mutate (lockdown.factor = 
            cohort %>%
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) 
#Select time periods
SIMD_Third6in1_preLD = SIMD_Third6in1_2020 %>% 
  filter(lockdown.factor=="Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PreLD")
SIMD_Third6in1_LD = SIMD_Third6in1_2020 %>% 
  filter(lockdown.factor=="LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="LD")
SIMD_Third6in1_PostLD = SIMD_Third6in1_2020 %>% 
  filter(lockdown.factor=="Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PostLD")

#Group together then plot
SIMD_Third6in1_grouped = full_join(SIMD_Third6in1_2019, SIMD_Third6in1_preLD)  
SIMD_Third6in1_grouped = full_join(SIMD_Third6in1_grouped, SIMD_Third6in1_LD)
SIMD_Third6in1_grouped = full_join(SIMD_Third6in1_grouped, SIMD_Third6in1_PostLD)
SIMD_Third6in1_grouped = SIMD_Third6in1_grouped %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("2019"))

SIMD_Third6in1_grouped$lockdown.factor = factor(SIMD_Third6in1_grouped$lockdown.factor, levels = c('2019', 'PreLD', 'LD', 'PostLD'))


Third6in1_SIMD_grouped = SIMD_Third6in1_grouped %>%
  ggplot(aes(fill=lockdown.factor, y=mean_percent, x=deprivation_quintile)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Greens")+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 73, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "Green", size=0.75)+
  geom_hline(yintercept = 82, linetype="dotted", ##Scotland wide mean uptake LD
             color = "black", size=0.75)

Third6in1_SIMD_grouped ##############for some reason hasn't plotted postLD

#Plot grouped by SIMD on line plot

Third6in1_groupedSIMD_line = SIMD_Third6in1_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose 6in1")+
  expand_limits(y=65)+
  scale_y_continuous(breaks = c(65,70,75,80,85,90,95,100))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
Third6in1_groupedSIMD_line


##Plot percent change
#Third6in1, first format tables of percent only then join 2019 and LD
SIMD_Third6in1_percent_2019 = SIMD_Third6in1_2019 %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_Third6in1_percent_2019)= c("deprivation_quintile", "uptake_2019_percent")

SIMD_Third6in1_percent_LD = SIMD_Third6in1_LD %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_Third6in1_percent_LD)= c("deprivation_quintile", "uptake_LD_percent")

SIMD_Third6in1_percentchange = full_join(SIMD_Third6in1_percent_2019, SIMD_Third6in1_percent_LD) 
SIMD_Third6in1_percentchange = SIMD_Third6in1_percentchange %>% 
  mutate(percent_change = uptake_LD_percent - uptake_2019_percent)
#Plot percent change Second 6in1 NBB to get the significant levels you need to do the regression below
library(RColorBrewer)
Third6in1_SIMD_percentchange_bar = SIMD_Third6in1_percentchange %>% 
  ggplot(aes(y=percent_change, x=deprivation_quintile)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL,y="Absolute % change 2019 vs LD")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))+
  annotate("text", x = 1, y = 11.2, label = "***")+
  annotate("text", x = 2, y = 11.2, label = "***")+
  annotate("text", x = 3, y = 9.5, label = "***")+
  annotate("text", x = 4, y = 9.5, label = "***")+
  annotate("text", x = 5, y = 9.5, label = "***")+
  annotate("text", x = 4, y = 10, label = "Third 6in1")
Third6in1_SIMD_percentchange_bar

#### regression analysis for Second 6in1 SIMD
#Comparison in 2019- Add unvaccianted column and relevel to most deprived

SIMD_Third6in1_2019 = SIMD_Third6in1_2019 %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_Third6in1_2019 = SIMD_Third6in1_2019 %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_Third6in1_SIMD_regression_tbl_2019 = cbind(SIMD_Third6in1_2019$total_vaccinated, SIMD_Third6in1_2019$unvaccinated)

Summarymodel_Third6in1_SIMD_2019 = glm(Summary_Third6in1_SIMD_regression_tbl_2019 ~ SIMD_Third6in1_2019$SIMD.factor,
                                        family="binomial")

summary(Summarymodel_Third6in1_SIMD_2019)

exp(Summarymodel_Third6in1_SIMD_2019$coefficients)
exp(confint(Summarymodel_Third6in1_SIMD_2019))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryThird6in1_SIMD2019model_tbl = Summarymodel_Third6in1_SIMD_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_Third6in1_SIMD2019 <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryThird6in1_SIMD2019model_tbl$OR),
  boxCILow = c(SummaryThird6in1_SIMD2019model_tbl$upperCI),
  boxCIHigh = c(SummaryThird6in1_SIMD2019model_tbl$lowerCI))

SummaryThird6in1_SIMD2019_forest <- ggplot(SummaryORandCI_Third6in1_SIMD2019, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryThird6in1_SIMD2019_forest = SummaryThird6in1_SIMD2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "Summary2019 OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryThird6in1_SIMD2019_forest

anova(Summarymodel_Third6in1_SIMD_2019, test="LRT")

#Comparison in LD- Add unvaccinated column and relevel to most deprived

SIMD_Third6in1_LD = SIMD_Third6in1_LD %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_Third6in1_LD = SIMD_Third6in1_LD %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_Third6in1_SIMD_regression_tbl_LD = cbind(SIMD_Third6in1_LD$total_vaccinated, SIMD_Third6in1_LD$unvaccinated)

Summarymodel_Third6in1_SIMD_LD = glm(Summary_Third6in1_SIMD_regression_tbl_LD ~ SIMD_Third6in1_LD$SIMD.factor,
                                      family="binomial")

summary(Summarymodel_Third6in1_SIMD_LD)

exp(Summarymodel_Third6in1_SIMD_LD$coefficients)
exp(confint(Summarymodel_Third6in1_SIMD_LD))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryThird6in1_SIMDLDmodel_tbl = Summarymodel_Third6in1_SIMD_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCIThird6in1_SIMDLD <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryThird6in1_SIMDLDmodel_tbl$OR),
  boxCILow = c(SummaryThird6in1_SIMDLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryThird6in1_SIMDLDmodel_tbl$lowerCI))

SummaryThird6in1_SIMDLD_forest <- ggplot(SummaryORandCIThird6in1_SIMDLD, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryThird6in1_SIMDLD_forest = SummaryThird6in1_SIMDLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryThird6in1_SIMDLD_forest

anova(Summarymodel_Third6in1_SIMD_LD, test="LRT")

##Try to plot the forest plots together
SummaryThird6in1_SIMD2019model_tbl= SummaryThird6in1_SIMD2019model_tbl %>% 
  mutate(time.factor =  
           "2019" %>% 
           factor())
SummaryThird6in1_SIMDLDmodel_tbl= SummaryThird6in1_SIMDLDmodel_tbl %>% 
  mutate(time.factor =  
           "LD" %>% 
           factor())


Merge_Third6in1_ORCI_df = rbind(SummaryThird6in1_SIMD2019model_tbl, SummaryThird6in1_SIMDLDmodel_tbl) %>% 
  mutate("label" = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD"))

boxLabelsSIMDmerge = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD")

MergeLD2019ORandCI_Third6in1_SIMD <- data.frame(
  yAxis = length(boxLabelsSIMDmerge):1,
  boxOdds = c(Merge_Third6in1_ORCI_df$OR),
  boxCILow = c(Merge_Third6in1_ORCI_df$upperCI),
  boxCIHigh = c(Merge_Third6in1_ORCI_df$lowerCI), 
  time = c(Merge_Third6in1_ORCI_df$time.factor))

MergeLD2019ORandCI_Third6in1_SIMD_forest <- ggplot(MergeLD2019ORandCI_Third6in1_SIMD, aes(x = boxOdds, y = boxLabelsSIMDmerge, colour = time))
MergeLD2019ORandCI_Third6in1_SIMD_forest = MergeLD2019ORandCI_Third6in1_SIMD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5) +
  theme_bw()+
  labs(x = "Third6in1 OR compared to most deprived",
       y = NULL,
       title = NULL)+
  theme(legend.position="none")

MergeLD2019ORandCI_Third6in1_SIMD_forest

##Log regression to compare change btw 2019 and LD with interaction of SIMD 

SIMD_Third6in1_grouped = SIMD_Third6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_Third6in1_grouped = SIMD_Third6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


Third6in1_interaction_tbl <- cbind(SIMD_Third6in1_grouped$total_vaccinated, SIMD_Third6in1_grouped$unvaccinated)

# Column for time-period
tp <- SIMD_Third6in1_grouped$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Third6in1_grouped$SIMD.factor
# Put into GLM with interaction
model_SIMD_Third6in1<- glm(Third6in1_interaction_tbl ~ tp*SIMD,
                            family="binomial")                                 

summary(model_SIMD_Third6in1) ##cf 2019, change for SIMD1 was sig increase during LD but ns for pre LD
exp(model_SIMD_Third6in1$coefficients)
exp(confint(model_SIMD_Third6in1))

library(broom)
Third6in1_SIMDinteraction_tbl = model_SIMD_Third6in1%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(Third6in1_SIMDinteraction_tbl, file = "Exported tables/Third6in1_SIMDinteraction_tbl.csv")

##Run the interaction model again but change the baseline to different SIMD levels. Will give you the comparisons for that level btw tp and the other SIMD
#Baseline least deprived:

SIMD_Third6in1_grouped = SIMD_Third6in1_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_Third6in1_grouped_BL5 = SIMD_Third6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("5 - least deprived"))

Third6in1_interaction_tbl_BL5 <- cbind(SIMD_Third6in1_grouped_BL5$total_vaccinated, SIMD_Third6in1_grouped_BL5$unvaccinated)

# Column for time-period
tp <- SIMD_Third6in1_grouped_BL5$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Third6in1_grouped_BL5$SIMD.factor
# Put into GLM with interaction
model_SIMD_Third6in1_BL5<- glm(Third6in1_interaction_tbl_BL5 ~ tp*SIMD,
                                family="binomial")                                 

summary(model_SIMD_Third6in1_BL5) ##cf 2019, change for SIMD5 was ns for preLD but sig for LD***
exp(model_SIMD_Third6in1_BL5$coefficients)
exp(confint(model_SIMD_Third6in1_BL5))

library(broom)
Third6in1_SIMDinteraction_tbl_BL5 = model_SIMD_Third6in1_BL5%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(Third6in1_SIMDinteraction_tbl_BL5, file = "Exported tables/Third6in1_SIMDinteraction_tbl_BL5.csv")

#Baseline SIMD4
SIMD_Third6in1_grouped_BL4 = SIMD_Third6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("4"))

Third6in1_interaction_tbl_BL4 <- cbind(SIMD_Third6in1_grouped_BL4$total_vaccinated, SIMD_Third6in1_grouped_BL4$unvaccinated)

# Column for time-period
tp <- SIMD_Third6in1_grouped_BL4$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Third6in1_grouped_BL4$SIMD.factor
# Put into GLM with interaction
model_SIMD_Third6in1_BL4<- glm(Third6in1_interaction_tbl_BL4 ~ tp*SIMD,
                                family="binomial")                                 

summary(model_SIMD_Third6in1_BL4) ##cf 2019, change for SIMD 4 was ns for preLD but sig for LD ***
exp(model_SIMD_Third6in1_BL4$coefficients)
exp(confint(model_SIMD_Third6in1_BL4))

library(broom)
Third6in1_SIMDinteraction_tbl_BL4 = model_SIMD_Third6in1_BL4%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
write_csv(Third6in1_SIMDinteraction_tbl_BL4, file = "Exported tables/Third6in1_SIMDinteraction_tbl_BL4.csv")

#Baseline SIMD3
SIMD_Third6in1_grouped_BL3 = SIMD_Third6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("3"))

Third6in1_interaction_tbl_BL3 <- cbind(SIMD_Third6in1_grouped_BL3$total_vaccinated, SIMD_Third6in1_grouped_BL3$unvaccinated)

# Column for time-period
tp <- SIMD_Third6in1_grouped_BL3$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Third6in1_grouped_BL3$SIMD.factor
# Put into GLM with interaction
model_SIMD_Third6in1_BL3<- glm(Third6in1_interaction_tbl_BL3 ~ tp*SIMD,
                                family="binomial")                                 

summary(model_SIMD_Third6in1_BL3) ##cf 2019, change for SIMD 3  sig increase during LD *** no change preLD
exp(model_SIMD_Third6in1_BL3$coefficients)
exp(confint(model_SIMD_Third6in1_BL3))

library(broom)
Third6in1_SIMDinteraction_tbl_BL3 = model_SIMD_Third6in1_BL3%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
Third6in1_SIMDinteraction_tbl_BL3
write_csv(Third6in1_SIMDinteraction_tbl_BL3, file = "Exported tables/Third6in1_SIMDinteraction_tbl_BL3.csv")

#Baseline SIMD2
SIMD_Third6in1_grouped_BL2 = SIMD_Third6in1_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("2"))

Third6in1_interaction_tbl_BL2 <- cbind(SIMD_Third6in1_grouped_BL2$total_vaccinated, SIMD_Third6in1_grouped_BL2$unvaccinated)

# Column for time-period
tp <- SIMD_Third6in1_grouped_BL2$lockdown.factor
# Column for HSCP
SIMD <- SIMD_Third6in1_grouped_BL2$SIMD.factor
# Put into GLM with interaction
model_SIMD_Third6in1_BL2<- glm(Third6in1_interaction_tbl_BL2 ~ tp*SIMD,
                                family="binomial")                                 

summary(model_SIMD_Third6in1_BL2) ##cf 2019, change for SIMD 2 ns for preLD, sig increase during LD 
exp(model_SIMD_Third6in1_BL2$coefficients)
exp(confint(model_SIMD_Third6in1_BL2))

library(broom)
Third6in1_SIMDinteraction_tbl_BL2 = model_SIMD_Third6in1_BL2%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
Third6in1_SIMDinteraction_tbl_BL2
write_csv(Third6in1_SIMDinteraction_tbl_BL2, file = "Exported tables/Third6in1_SIMDinteraction_tbl_BL2.csv")

####################################################################
######As above but for first dose MMR. Removed monthly plots
Full_SIMD_FirstMMR = read.csv(here("Data", "Deprivation_First_MMR_11_feb_21.csv"))
#Select out 2019 data
SIMD_FirstMMR_2019 = Full_SIMD_FirstMMR %>% 
  select("cohort", "deprivation_quintile", "children_turn_12months_2019_num", "children_rec_imm_13months_2019_num", "uptake_13months_2019_percent")
colnames(SIMD_FirstMMR_2019) = c("cohort", "deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent")
SIMD_FirstMMR_2019 = SIMD_FirstMMR_2019 %>%  
  filter(cohort=="Jan-20") %>% 
  select("deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent") %>% 
  mutate(lockdown.factor = "2019")

#Select out 2020 data
SIMD_FirstMMR_2020 = Full_SIMD_FirstMMR %>% 
  select("cohort", "deprivation_quintile", "children_turn_12months_num", "children_rec_imm_13months_num", "uptake_13months_percent")
colnames(SIMD_FirstMMR_2020) = c("cohort", "deprivation_quintile", "denominator", "num_vaccinated", "percent_uptake")
SIMD_FirstMMR_2020 = SIMD_FirstMMR_2020 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
SIMD_FirstMMR_2020 = SIMD_FirstMMR_2020 %>% 
  mutate (lockdown.factor = 
            cohort %>%
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) 
#Select time periods
SIMD_FirstMMR_preLD = SIMD_FirstMMR_2020 %>% 
  filter(lockdown.factor=="Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PreLD")
SIMD_FirstMMR_LD = SIMD_FirstMMR_2020 %>% 
  filter(lockdown.factor=="LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="LD")
SIMD_FirstMMR_PostLD = SIMD_FirstMMR_2020 %>% 
  filter(lockdown.factor=="Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PostLD")

#Group together then plot

SIMD_FirstMMR_grouped = full_join(SIMD_FirstMMR_2019, SIMD_FirstMMR_preLD)  
SIMD_FirstMMR_grouped = full_join(SIMD_FirstMMR_grouped, SIMD_FirstMMR_LD)
SIMD_FirstMMR_grouped = full_join(SIMD_FirstMMR_grouped, SIMD_FirstMMR_PostLD)
SIMD_FirstMMR_grouped = SIMD_FirstMMR_grouped %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("2019"))

SIMD_FirstMMR_grouped$lockdown.factor = factor(SIMD_FirstMMR_grouped$lockdown.factor, levels = c('2019', 'PreLD', 'LD', 'PostLD'))


FirstMMR_SIMD_grouped = SIMD_FirstMMR_grouped %>%
  ggplot(aes(fill=lockdown.factor, y=mean_percent, x=deprivation_quintile)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Greens")+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "FirstMMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 65.2, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "Green", size=0.75)+
  geom_hline(yintercept = 78, linetype="dotted", ##Scotland wide mean uptake LD
             color = "black", size=0.75)

FirstMMR_SIMD_grouped 

#Plot grouped by SIMD on line plot

FirstMMR_groupedSIMD_line = SIMD_FirstMMR_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")+
  expand_limits(y=45)+
  scale_y_continuous(breaks = c(45,50,55,60,65,70,75,80,85))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
FirstMMR_groupedSIMD_line


##Plot percent change
#FirstMMR, first format tables of percent only then join 2019 and LD
SIMD_FirstMMR_percent_2019 = SIMD_FirstMMR_2019 %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_FirstMMR_percent_2019)= c("deprivation_quintile", "uptake_2019_percent")

SIMD_FirstMMR_percent_LD = SIMD_FirstMMR_LD %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_FirstMMR_percent_LD)= c("deprivation_quintile", "uptake_LD_percent")

SIMD_FirstMMR_percentchange = full_join(SIMD_FirstMMR_percent_2019, SIMD_FirstMMR_percent_LD) 
SIMD_FirstMMR_percentchange = SIMD_FirstMMR_percentchange %>% 
  mutate(percent_change = uptake_LD_percent - uptake_2019_percent)
#Plot percent change FirstMMR NBB to get the significant levels you need to do the regression below
library(RColorBrewer)
FirstMMR_SIMD_percentchange_bar = SIMD_FirstMMR_percentchange %>% 
  ggplot(aes(y=percent_change, x=deprivation_quintile)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL,y="Absolute % change 2019 vs LD")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))+
  annotate("text", x = 1, y = 15, label = "***")+
  annotate("text", x = 2, y = 15, label = "***")+
  annotate("text", x = 3, y = 15, label = "***")+
  annotate("text", x = 4, y = 15, label = "***")+
  annotate("text", x = 5, y = 15, label = "***")+
  annotate("text", x = 1, y = 12, label = "FirstMMR")
FirstMMR_SIMD_percentchange_bar

#### regression analysis for FirstMMR SIMD
#Comparison in 2019- Add unvaccianted column and relevel to most deprived

SIMD_FirstMMR_2019 = SIMD_FirstMMR_2019 %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_FirstMMR_2019 = SIMD_FirstMMR_2019 %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_FirstMMR_SIMD_regression_tbl_2019 = cbind(SIMD_FirstMMR_2019$total_vaccinated, SIMD_FirstMMR_2019$unvaccinated)

Summarymodel_FirstMMR_SIMD_2019 = glm(Summary_FirstMMR_SIMD_regression_tbl_2019 ~ SIMD_FirstMMR_2019$SIMD.factor,
                                       family="binomial")

summary(Summarymodel_FirstMMR_SIMD_2019)

exp(Summarymodel_FirstMMR_SIMD_2019$coefficients)
exp(confint(Summarymodel_FirstMMR_SIMD_2019))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirstMMR_SIMD2019model_tbl = Summarymodel_FirstMMR_SIMD_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_FirstMMR_SIMD2019 <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryFirstMMR_SIMD2019model_tbl$OR),
  boxCILow = c(SummaryFirstMMR_SIMD2019model_tbl$upperCI),
  boxCIHigh = c(SummaryFirstMMR_SIMD2019model_tbl$lowerCI))

SummaryFirstMMR_SIMD2019_forest <- ggplot(SummaryORandCI_FirstMMR_SIMD2019, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryFirstMMR_SIMD2019_forest = SummaryFirstMMR_SIMD2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "Summary2019 OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryFirstMMR_SIMD2019_forest

anova(Summarymodel_FirstMMR_SIMD_2019, test="LRT")

#Comparison in LD- Add unvaccinated column and relevel to most deprived

SIMD_FirstMMR_LD = SIMD_FirstMMR_LD %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_FirstMMR_LD = SIMD_FirstMMR_LD %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_FirstMMR_SIMD_regression_tbl_LD = cbind(SIMD_FirstMMR_LD$total_vaccinated, SIMD_FirstMMR_LD$unvaccinated)

Summarymodel_FirstMMR_SIMD_LD = glm(Summary_FirstMMR_SIMD_regression_tbl_LD ~ SIMD_FirstMMR_LD$SIMD.factor,
                                     family="binomial")

summary(Summarymodel_FirstMMR_SIMD_LD)

exp(Summarymodel_FirstMMR_SIMD_LD$coefficients)
exp(confint(Summarymodel_FirstMMR_SIMD_LD))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirstMMR_SIMDLDmodel_tbl = Summarymodel_FirstMMR_SIMD_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCIFirstMMR_SIMDLD <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummaryFirstMMR_SIMDLDmodel_tbl$OR),
  boxCILow = c(SummaryFirstMMR_SIMDLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirstMMR_SIMDLDmodel_tbl$lowerCI))

SummaryFirstMMR_SIMDLD_forest <- ggplot(SummaryORandCIFirstMMR_SIMDLD, aes(x = boxOdds, y = boxLabelsSIMD))
SummaryFirstMMR_SIMDLD_forest = SummaryFirstMMR_SIMDLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to most deprived",
       y = NULL,
       title = NULL)

SummaryFirstMMR_SIMDLD_forest

anova(Summarymodel_FirstMMR_SIMD_LD, test="LRT")

##Try to plot the forest plots together
SummaryFirstMMR_SIMD2019model_tbl= SummaryFirstMMR_SIMD2019model_tbl %>% 
  mutate(time.factor =  
           "2019" %>% 
           factor())
SummaryFirstMMR_SIMDLDmodel_tbl= SummaryFirstMMR_SIMDLDmodel_tbl %>% 
  mutate(time.factor =  
           "LD" %>% 
           factor())


Merge_FirstMMR_ORCI_df = rbind(SummaryFirstMMR_SIMD2019model_tbl, SummaryFirstMMR_SIMDLDmodel_tbl) %>% 
  mutate("label" = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD"))

boxLabelsSIMDmerge = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD")

MergeLD2019ORandCI_FirstMMR_SIMD <- data.frame(
  yAxis = length(boxLabelsSIMDmerge):1,
  boxOdds = c(Merge_FirstMMR_ORCI_df$OR),
  boxCILow = c(Merge_FirstMMR_ORCI_df$upperCI),
  boxCIHigh = c(Merge_FirstMMR_ORCI_df$lowerCI), 
  time = c(Merge_FirstMMR_ORCI_df$time.factor))

MergeLD2019ORandCI_FirstMMR_SIMD_forest <- ggplot(MergeLD2019ORandCI_FirstMMR_SIMD, aes(x = boxOdds, y = boxLabelsSIMDmerge, colour = time))
MergeLD2019ORandCI_FirstMMR_SIMD_forest = MergeLD2019ORandCI_FirstMMR_SIMD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5) +
  theme_bw()+
  labs(x = "FirstMMR OR compared to most deprived",
       y = NULL,
       title = NULL)+
  theme(legend.position="none")

MergeLD2019ORandCI_FirstMMR_SIMD_forest

##Log regression to compare change btw 2019 and LD with interaction of SIMD 

SIMD_FirstMMR_grouped = SIMD_FirstMMR_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_FirstMMR_grouped = SIMD_FirstMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


FirstMMR_interaction_tbl <- cbind(SIMD_FirstMMR_grouped$total_vaccinated, SIMD_FirstMMR_grouped$unvaccinated)

# Column for time-period
tp <- SIMD_FirstMMR_grouped$lockdown.factor
# Column for HSCP
SIMD <- SIMD_FirstMMR_grouped$SIMD.factor
# Put into GLM with interaction
model_SIMD_FirstMMR<- glm(FirstMMR_interaction_tbl ~ tp*SIMD,
                           family="binomial")                                 

summary(model_SIMD_FirstMMR) ##cf 2019, change for SIMD1 was sig increase pre, post and during LD
exp(model_SIMD_FirstMMR$coefficients)
exp(confint(model_SIMD_FirstMMR))
exp(0.346090)

library(broom)
FirstMMR_SIMDinteraction_tbl = model_SIMD_FirstMMR%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(FirstMMR_SIMDinteraction_tbl, file = "Exported tables/FirstMMR_SIMDinteraction_tbl.csv")

##Run the interaction model again but change the baseline to different SIMD levels. Will give you the comparisons for that level btw tp and the other SIMD
#Baseline least deprived:

SIMD_FirstMMR_grouped = SIMD_FirstMMR_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_FirstMMR_grouped_BL5 = SIMD_FirstMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("5 - least deprived"))

FirstMMR_interaction_tbl_BL5 <- cbind(SIMD_FirstMMR_grouped_BL5$total_vaccinated, SIMD_FirstMMR_grouped_BL5$unvaccinated)

# Column for time-period
tp <- SIMD_FirstMMR_grouped_BL5$lockdown.factor
# Column for HSCP
SIMD <- SIMD_FirstMMR_grouped_BL5$SIMD.factor
# Put into GLM with interaction
model_SIMD_FirstMMR_BL5<- glm(FirstMMR_interaction_tbl_BL5 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_FirstMMR_BL5) ##cf 2019, change for SIMD5  sig inc for preLD, post LD ad LD***
exp(model_SIMD_FirstMMR_BL5$coefficients)
exp(confint(model_SIMD_FirstMMR_BL5))

library(broom)
FirstMMR_SIMDinteraction_tbl_BL5 = model_SIMD_FirstMMR_BL5%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(FirstMMR_SIMDinteraction_tbl_BL5, file = "Exported tables/FirstMMR_SIMDinteraction_tbl_BL5.csv")

#Baseline SIMD4
SIMD_FirstMMR_grouped_BL4 = SIMD_FirstMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("4"))

FirstMMR_interaction_tbl_BL4 <- cbind(SIMD_FirstMMR_grouped_BL4$total_vaccinated, SIMD_FirstMMR_grouped_BL4$unvaccinated)

# Column for time-period
tp <- SIMD_FirstMMR_grouped_BL4$lockdown.factor
# Column for HSCP
SIMD <- SIMD_FirstMMR_grouped_BL4$SIMD.factor
# Put into GLM with interaction
model_SIMD_FirstMMR_BL4<- glm(FirstMMR_interaction_tbl_BL4 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_FirstMMR_BL4) ##cf 2019, change for SIMD 4 was sig for pre, post and LD ***
exp(model_SIMD_FirstMMR_BL4$coefficients)
exp(confint(model_SIMD_FirstMMR_BL4))

library(broom)
FirstMMR_SIMDinteraction_tbl_BL4 = model_SIMD_FirstMMR_BL4%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
write_csv(FirstMMR_SIMDinteraction_tbl_BL4, file = "Exported tables/FirstMMR_SIMDinteraction_tbl_BL4.csv")

#Baseline SIMD3
SIMD_FirstMMR_grouped_BL3 = SIMD_FirstMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("3"))

FirstMMR_interaction_tbl_BL3 <- cbind(SIMD_FirstMMR_grouped_BL3$total_vaccinated, SIMD_FirstMMR_grouped_BL3$unvaccinated)

# Column for time-period
tp <- SIMD_FirstMMR_grouped_BL3$lockdown.factor
# Column for HSCP
SIMD <- SIMD_FirstMMR_grouped_BL3$SIMD.factor
# Put into GLM with interaction
model_SIMD_FirstMMR_BL3<- glm(FirstMMR_interaction_tbl_BL3 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_FirstMMR_BL3) ##cf 2019, change for SIMD 3  sig increase for all periods
exp(model_SIMD_FirstMMR_BL3$coefficients)
exp(confint(model_SIMD_FirstMMR_BL3))

library(broom)
FirstMMR_SIMDinteraction_tbl_BL3 = model_SIMD_FirstMMR_BL3%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
FirstMMR_SIMDinteraction_tbl_BL3
write_csv(FirstMMR_SIMDinteraction_tbl_BL3, file = "Exported tables/FirstMMR_SIMDinteraction_tbl_BL3.csv")

#Baseline SIMD2
SIMD_FirstMMR_grouped_BL2 = SIMD_FirstMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("2"))

FirstMMR_interaction_tbl_BL2 <- cbind(SIMD_FirstMMR_grouped_BL2$total_vaccinated, SIMD_FirstMMR_grouped_BL2$unvaccinated)

# Column for time-period
tp <- SIMD_FirstMMR_grouped_BL2$lockdown.factor
# Column for HSCP
SIMD <- SIMD_FirstMMR_grouped_BL2$SIMD.factor
# Put into GLM with interaction
model_SIMD_FirstMMR_BL2<- glm(FirstMMR_interaction_tbl_BL2 ~ tp*SIMD,
                               family="binomial")                                 

summary(model_SIMD_FirstMMR_BL2) ##cf 2019, change for SIMD 2 sig increase for all time periods
exp(model_SIMD_FirstMMR_BL2$coefficients)
exp(confint(model_SIMD_FirstMMR_BL2))

library(broom)
FirstMMR_SIMDinteraction_tbl_BL2 = model_SIMD_FirstMMR_BL2%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
FirstMMR_SIMDinteraction_tbl_BL2
write_csv(FirstMMR_SIMDinteraction_tbl_BL2, file = "Exported tables/FirstMMR_SIMDinteraction_tbl_BL2.csv")

####################################################################
######As above but for second dose MMR. Removed monthly plots
Full_SIMD_SecondMMR = read.csv(here("Data", "Deprivation_Second_MMR_11_feb_21.csv"))
#Select out 2019 data
SIMD_SecondMMR_2019 = Full_SIMD_SecondMMR %>% 
  select("cohort", "deprivation_quintile", "children_turn_3y4months_2019_num", "children_rec_imm_3y5months_2019_num", "uptake_3y5months_2019_percent")
colnames(SIMD_SecondMMR_2019) = c("cohort", "deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent")
SIMD_SecondMMR_2019 = SIMD_SecondMMR_2019 %>%  
  filter(cohort=="Jan-20") %>% 
  select("deprivation_quintile", "total_eligable", "total_vaccinated", "mean_percent") %>% 
  mutate(lockdown.factor = "2019")

#Select out 2020 data
SIMD_SecondMMR_2020 = Full_SIMD_SecondMMR %>% 
  select("cohort", "deprivation_quintile", "children_turn_3y4months_num", "children_rec_imm_3y5months_num", "uptake_3y5months_percent")
colnames(SIMD_SecondMMR_2020) = c("cohort", "deprivation_quintile", "denominator", "num_vaccinated", "percent_uptake")
SIMD_SecondMMR_2020 = SIMD_SecondMMR_2020 %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
SIMD_SecondMMR_2020 = SIMD_SecondMMR_2020 %>% 
  mutate (lockdown.factor = 
            cohort %>%
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20")) 
#Select time periods
SIMD_SecondMMR_preLD = SIMD_SecondMMR_2020 %>% 
  filter(lockdown.factor=="Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PreLD")
SIMD_SecondMMR_LD = SIMD_SecondMMR_2020 %>% 
  filter(lockdown.factor=="LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="LD")
SIMD_SecondMMR_PostLD = SIMD_SecondMMR_2020 %>% 
  filter(lockdown.factor=="Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(total_eligable = sum(denominator), total_vaccinated = sum(num_vaccinated), mean_percent = mean(percent_uptake)) %>% 
  mutate(lockdown.factor="PostLD")

#Group together then plot

SIMD_SecondMMR_grouped = full_join(SIMD_SecondMMR_2019, SIMD_SecondMMR_preLD)  
SIMD_SecondMMR_grouped = full_join(SIMD_SecondMMR_grouped, SIMD_SecondMMR_LD)
SIMD_SecondMMR_grouped = full_join(SIMD_SecondMMR_grouped, SIMD_SecondMMR_PostLD)
SIMD_SecondMMR_grouped = SIMD_SecondMMR_grouped %>% 
  mutate(lockdown.factor = lockdown.factor %>%
           fct_relevel("2019"))

SIMD_SecondMMR_grouped$lockdown.factor = factor(SIMD_SecondMMR_grouped$lockdown.factor, levels = c('2019', 'PreLD', 'LD', 'PostLD'))


SecondMMR_SIMD_grouped = SIMD_SecondMMR_grouped %>%
  ggplot(aes(fill=lockdown.factor, y=mean_percent, x=deprivation_quintile)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Greens")+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "SecondMMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 51.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "Green", size=0.75)+
  geom_hline(yintercept = 61, linetype="dotted", ##Scotland wide mean uptake LD
             color = "black", size=0.75)

SecondMMR_SIMD_grouped 

#Plot grouped by SIMD on line plot

SecondMMR_groupedSIMD_line = SIMD_SecondMMR_grouped %>%
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")+
  expand_limits(y=45:85)+
  scale_y_continuous(breaks = c(45,50,55,60,65,70,75,80,85))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")+
  coord_cartesian(expand = TRUE)

SecondMMR_groupedSIMD_line


##Plot percent change
#SecondMMR, first format tables of percent only then join 2019 and LD
SIMD_SecondMMR_percent_2019 = SIMD_SecondMMR_2019 %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_SecondMMR_percent_2019)= c("deprivation_quintile", "uptake_2019_percent")

SIMD_SecondMMR_percent_LD = SIMD_SecondMMR_LD %>% 
  select(deprivation_quintile, mean_percent)
colnames(SIMD_SecondMMR_percent_LD)= c("deprivation_quintile", "uptake_LD_percent")

SIMD_SecondMMR_percentchange = full_join(SIMD_SecondMMR_percent_2019, SIMD_SecondMMR_percent_LD) 
SIMD_SecondMMR_percentchange = SIMD_SecondMMR_percentchange %>% 
  mutate(percent_change = uptake_LD_percent - uptake_2019_percent)
#Plot percent change SecondMMR NBB to get the significant levels you need to do the regression below
library(RColorBrewer)
SecondMMR_SIMD_percentchange_bar = SIMD_SecondMMR_percentchange %>% 
  ggplot(aes(y=percent_change, x=deprivation_quintile)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL,y="Absolute % change 2019 vs LD")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10))+
  annotate("text", x = 1, y = 17, label = "***")+
  annotate("text", x = 2, y = 17, label = "***")+
  annotate("text", x = 3, y = 17, label = "***")+
  annotate("text", x = 4, y = 17, label = "***")+
  annotate("text", x = 5, y = 17, label = "***")+
  annotate("text", x = 3, y = 15, label = "SecondMMR")
SecondMMR_SIMD_percentchange_bar

#### regression analysis for SecondMMR SIMD
#Comparison in 2019- Add unvaccianted column and relevel to most deprived

SIMD_SecondMMR_2019 = SIMD_SecondMMR_2019 %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_SecondMMR_2019 = SIMD_SecondMMR_2019 %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_SecondMMR_SIMD_regression_tbl_2019 = cbind(SIMD_SecondMMR_2019$total_vaccinated, SIMD_SecondMMR_2019$unvaccinated)

Summarymodel_SecondMMR_SIMD_2019 = glm(Summary_SecondMMR_SIMD_regression_tbl_2019 ~ SIMD_SecondMMR_2019$SIMD.factor,
                                      family="binomial")

summary(Summarymodel_SecondMMR_SIMD_2019)

exp(Summarymodel_SecondMMR_SIMD_2019$coefficients)
exp(confint(Summarymodel_SecondMMR_SIMD_2019))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecondMMR_SIMD2019model_tbl = Summarymodel_SecondMMR_SIMD_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCI_SecondMMR_SIMD2019 <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummarySecondMMR_SIMD2019model_tbl$OR),
  boxCILow = c(SummarySecondMMR_SIMD2019model_tbl$upperCI),
  boxCIHigh = c(SummarySecondMMR_SIMD2019model_tbl$lowerCI))

SummarySecondMMR_SIMD2019_forest <- ggplot(SummaryORandCI_SecondMMR_SIMD2019, aes(x = boxOdds, y = boxLabelsSIMD))
SummarySecondMMR_SIMD2019_forest = SummarySecondMMR_SIMD2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "Summary2019 OR compared to most deprived",
       y = NULL,
       title = NULL)

SummarySecondMMR_SIMD2019_forest

anova(Summarymodel_SecondMMR_SIMD_2019, test="LRT")

#Comparison in LD- Add unvaccinated column and relevel to most deprived

SIMD_SecondMMR_LD = SIMD_SecondMMR_LD %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())

SIMD_SecondMMR_LD = SIMD_SecondMMR_LD %>%
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


#Set up regression tbls

Summary_SecondMMR_SIMD_regression_tbl_LD = cbind(SIMD_SecondMMR_LD$total_vaccinated, SIMD_SecondMMR_LD$unvaccinated)

Summarymodel_SecondMMR_SIMD_LD = glm(Summary_SecondMMR_SIMD_regression_tbl_LD ~ SIMD_SecondMMR_LD$SIMD.factor,
                                    family="binomial")

summary(Summarymodel_SecondMMR_SIMD_LD)

exp(Summarymodel_SecondMMR_SIMD_LD$coefficients)
exp(confint(Summarymodel_SecondMMR_SIMD_LD))

###Make a plot showing OR and CI compared to baseline most deprived
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecondMMR_SIMDLDmodel_tbl = Summarymodel_SecondMMR_SIMD_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsSIMD = c("2","3","4","5")

SummaryORandCISecondMMR_SIMDLD <- data.frame(
  yAxis = length(boxLabelsSIMD):1,
  boxOdds = c(SummarySecondMMR_SIMDLDmodel_tbl$OR),
  boxCILow = c(SummarySecondMMR_SIMDLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecondMMR_SIMDLDmodel_tbl$lowerCI))

SummarySecondMMR_SIMDLD_forest <- ggplot(SummaryORandCISecondMMR_SIMDLD, aes(x = boxOdds, y = boxLabelsSIMD))
SummarySecondMMR_SIMDLD_forest = SummarySecondMMR_SIMDLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to most deprived",
       y = NULL,
       title = NULL)

SummarySecondMMR_SIMDLD_forest

anova(Summarymodel_SecondMMR_SIMD_LD, test="LRT")

##Try to plot the forest plots together
SummarySecondMMR_SIMD2019model_tbl= SummarySecondMMR_SIMD2019model_tbl %>% 
  mutate(time.factor =  
           "2019" %>% 
           factor())
SummarySecondMMR_SIMDLDmodel_tbl= SummarySecondMMR_SIMDLDmodel_tbl %>% 
  mutate(time.factor =  
           "LD" %>% 
           factor())


Merge_SecondMMR_ORCI_df = rbind(SummarySecondMMR_SIMD2019model_tbl, SummarySecondMMR_SIMDLDmodel_tbl) %>% 
  mutate("label" = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD"))

boxLabelsSIMDmerge = c("2-2019", "3-2019", "4-2019", "5-2019","2-LD", "3-LD", "4-LD", "5-LD")

MergeLD2019ORandCI_SecondMMR_SIMD <- data.frame(
  yAxis = length(boxLabelsSIMDmerge):1,
  boxOdds = c(Merge_SecondMMR_ORCI_df$OR),
  boxCILow = c(Merge_SecondMMR_ORCI_df$upperCI),
  boxCIHigh = c(Merge_SecondMMR_ORCI_df$lowerCI), 
  time = c(Merge_SecondMMR_ORCI_df$time.factor))

MergeLD2019ORandCI_SecondMMR_SIMD_forest <- ggplot(MergeLD2019ORandCI_SecondMMR_SIMD, aes(x = boxOdds, y = boxLabelsSIMDmerge, colour = time))
MergeLD2019ORandCI_SecondMMR_SIMD_forest = MergeLD2019ORandCI_SecondMMR_SIMD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5) +
  theme_bw()+
  labs(x = "SecondMMR OR compared to most deprived",
       y = NULL,
       title = NULL)+
  theme(legend.position="none")

MergeLD2019ORandCI_SecondMMR_SIMD_forest

##Log regression to compare change btw 2019 and LD with interaction of SIMD 

SIMD_SecondMMR_grouped = SIMD_SecondMMR_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_SecondMMR_grouped = SIMD_SecondMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("1 - most deprived"))


SecondMMR_interaction_tbl <- cbind(SIMD_SecondMMR_grouped$total_vaccinated, SIMD_SecondMMR_grouped$unvaccinated)

# Column for time-period
tp <- SIMD_SecondMMR_grouped$lockdown.factor
# Column for HSCP
SIMD <- SIMD_SecondMMR_grouped$SIMD.factor
# Put into GLM with interaction
model_SIMD_SecondMMR<- glm(SecondMMR_interaction_tbl ~ tp*SIMD,
                          family="binomial")                                 

summary(model_SIMD_SecondMMR) ##cf 2019, change for SIMD1 was sig increase pre, post and during LD
exp(model_SIMD_SecondMMR$coefficients)
exp(confint(model_SIMD_SecondMMR))

library(broom)
SecondMMR_SIMDinteraction_tbl = model_SIMD_SecondMMR%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(SecondMMR_SIMDinteraction_tbl, file = "Exported tables/SecondMMR_SIMDinteraction_tbl.csv")

##Run the interaction model again but change the baseline to different SIMD levels. Will give you the comparisons for that level btw tp and the other SIMD
#Baseline least deprived:

SIMD_SecondMMR_grouped = SIMD_SecondMMR_grouped %>% 
  mutate(unvaccinated = total_eligable-total_vaccinated) %>% 
  mutate(SIMD.factor =  
           deprivation_quintile %>% 
           factor())
SIMD_SecondMMR_grouped_BL5 = SIMD_SecondMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("5 - least deprived"))

SecondMMR_interaction_tbl_BL5 <- cbind(SIMD_SecondMMR_grouped_BL5$total_vaccinated, SIMD_SecondMMR_grouped_BL5$unvaccinated)

# Column for time-period
tp <- SIMD_SecondMMR_grouped_BL5$lockdown.factor
# Column for HSCP
SIMD <- SIMD_SecondMMR_grouped_BL5$SIMD.factor
# Put into GLM with interaction
model_SIMD_SecondMMR_BL5<- glm(SecondMMR_interaction_tbl_BL5 ~ tp*SIMD,
                              family="binomial")                                 

summary(model_SIMD_SecondMMR_BL5) ##cf 2019, change for SIMD5  sig inc for preLD, post LD ad LD***
exp(model_SIMD_SecondMMR_BL5$coefficients)
exp(confint(model_SIMD_SecondMMR_BL5))

library(broom)
SecondMMR_SIMDinteraction_tbl_BL5 = model_SIMD_SecondMMR_BL5%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

write_csv(SecondMMR_SIMDinteraction_tbl_BL5, file = "Exported tables/SecondMMR_SIMDinteraction_tbl_BL5.csv")

#Baseline SIMD4
SIMD_SecondMMR_grouped_BL4 = SIMD_SecondMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("4"))

SecondMMR_interaction_tbl_BL4 <- cbind(SIMD_SecondMMR_grouped_BL4$total_vaccinated, SIMD_SecondMMR_grouped_BL4$unvaccinated)

# Column for time-period
tp <- SIMD_SecondMMR_grouped_BL4$lockdown.factor
# Column for HSCP
SIMD <- SIMD_SecondMMR_grouped_BL4$SIMD.factor
# Put into GLM with interaction
model_SIMD_SecondMMR_BL4<- glm(SecondMMR_interaction_tbl_BL4 ~ tp*SIMD,
                              family="binomial")                                 

summary(model_SIMD_SecondMMR_BL4) ##cf 2019, change for SIMD 4 was sig for pre, post and LD ***
exp(model_SIMD_SecondMMR_BL4$coefficients)
exp(confint(model_SIMD_SecondMMR_BL4))

library(broom)
SecondMMR_SIMDinteraction_tbl_BL4 = model_SIMD_SecondMMR_BL4%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
write_csv(SecondMMR_SIMDinteraction_tbl_BL4, file = "Exported tables/SecondMMR_SIMDinteraction_tbl_BL4.csv")

#Baseline SIMD3
SIMD_SecondMMR_grouped_BL3 = SIMD_SecondMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("3"))

SecondMMR_interaction_tbl_BL3 <- cbind(SIMD_SecondMMR_grouped_BL3$total_vaccinated, SIMD_SecondMMR_grouped_BL3$unvaccinated)

# Column for time-period
tp <- SIMD_SecondMMR_grouped_BL3$lockdown.factor
# Column for HSCP
SIMD <- SIMD_SecondMMR_grouped_BL3$SIMD.factor
# Put into GLM with interaction
model_SIMD_SecondMMR_BL3<- glm(SecondMMR_interaction_tbl_BL3 ~ tp*SIMD,
                              family="binomial")                                 

summary(model_SIMD_SecondMMR_BL3) ##cf 2019, change for SIMD 3  sig increase for LD and post LD but not preLD
exp(model_SIMD_SecondMMR_BL3$coefficients)
exp(confint(model_SIMD_SecondMMR_BL3))

library(broom)
SecondMMR_SIMDinteraction_tbl_BL3 = model_SIMD_SecondMMR_BL3%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
SecondMMR_SIMDinteraction_tbl_BL3
write_csv(SecondMMR_SIMDinteraction_tbl_BL3, file = "Exported tables/SecondMMR_SIMDinteraction_tbl_BL3.csv")

#Baseline SIMD2
SIMD_SecondMMR_grouped_BL2 = SIMD_SecondMMR_grouped %>% 
  mutate(SIMD.factor = SIMD.factor %>% 
           fct_relevel("2"))

SecondMMR_interaction_tbl_BL2 <- cbind(SIMD_SecondMMR_grouped_BL2$total_vaccinated, SIMD_SecondMMR_grouped_BL2$unvaccinated)

# Column for time-period
tp <- SIMD_SecondMMR_grouped_BL2$lockdown.factor
# Column for HSCP
SIMD <- SIMD_SecondMMR_grouped_BL2$SIMD.factor
# Put into GLM with interaction
model_SIMD_SecondMMR_BL2<- glm(SecondMMR_interaction_tbl_BL2 ~ tp*SIMD,
                              family="binomial")                                 

summary(model_SIMD_SecondMMR_BL2) ##cf 2019, change for SIMD 2 sig increase for all time periods
exp(model_SIMD_SecondMMR_BL2$coefficients)
exp(confint(model_SIMD_SecondMMR_BL2))

library(broom)
SecondMMR_SIMDinteraction_tbl_BL2 = model_SIMD_SecondMMR_BL2%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))
SecondMMR_SIMDinteraction_tbl_BL2
write_csv(SecondMMR_SIMDinteraction_tbl_BL2, file = "Exported tables/SecondMMR_SIMDinteraction_tbl_BL2.csv")

######EXporting graphs in single PDFS
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
#Grouped bar charts

Grouped_bar_6in1_SMID_all_periods = ggarrange(First6in1_SIMD_grouped, Second6in1_SIMD_grouped, Third6in1_SIMD_grouped, 
                                                      common.legend = TRUE, legend="bottom",
                                                      labels = NULL,
                                                      ncol = 2, nrow = 2)
Grouped_bar_6in1_SMID_all_periods


Grouped_bar_MMR_SMID_all_periods = ggarrange(FirstMMR_SIMD_grouped, SecondMMR_SIMD_grouped, 
                                              common.legend = TRUE, legend="bottom",
                                              labels = NULL,
                                              ncol = 2, nrow = 1)
Grouped_bar_MMR_SMID_all_periods

#Grouped line plots
Grouped_line_6in1_SMID_all_periods = ggarrange(First6in1_groupedSIMD_line, Second6in1_groupedSIMD_line, Third6in1_groupedSIMD_line, 
                                              common.legend = TRUE, legend="bottom",
                                              labels = NULL,
                                              ncol = 1, nrow = 3)
Grouped_line_6in1_SMID_all_periods


Grouped_line_MMR_SMID_all_periods = ggarrange(FirstMMR_groupedSIMD_line, SecondMMR_groupedSIMD_line, 
                                             common.legend = TRUE, legend="bottom",
                                             labels = NULL,
                                             ncol = 1, nrow = 2)
Grouped_line_MMR_SMID_all_periods

#Comparison of OR btw LD and 2019
OR2019vsLD_6in1_SMID_all_periods = ggarrange(MergeLD2019ORandCI_first6in1_SIMD_forest, MergeLD2019ORandCI_second6in1_SIMD_forest, MergeLD2019ORandCI_Third6in1_SIMD_forest, MergeLD2019ORandCI_FirstMMR_SIMD_forest, MergeLD2019ORandCI_SecondMMR_SIMD_forest,
                                               legend=NULL,
                                               labels = NULL,
                                               ncol = 2, nrow = 3)
OR2019vsLD_6in1_SMID_all_periods

#percentage change plots

Percentchange_2019vsLD_SMID_6in1 = ggarrange(First6in1_SIMD_percentchange_bar, Second6in1_SIMD_percentchange_bar, Third6in1_SIMD_percentchange_bar,
                                             legend=NULL,
                                             labels = NULL,
                                             ncol = 2, nrow = 2)
Percentchange_2019vsLD_SMID_6in1

Percentchange_2019vsLD_SMID_MMR = ggarrange(FirstMMR_SIMD_percentchange_bar, SecondMMR_SIMD_percentchange_bar, 
                                             legend=NULL,
                                             labels = NULL,
                                             ncol = 2, nrow = 1)
Percentchange_2019vsLD_SMID_MMR

################################
##Table for supplementary info with % change from 2019
##First 6in1
#FIrst pick the data from full dataset

Section3_supptbl3_first6in1 = Full_SIMD_First6in1 %>% 
  select(cohort, deprivation_quintile, children_turn_8weeks_num, children_rec_imm_12weeks_num, uptake_12weeks_percent, absolute_change_from_baseline_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20"))) %>% 
  mutate (lockdown.factor = cohort %>% 
          factor() %>% 
          fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))
#Separate into time periods to summarise data 
Section3_supptbl3_first6in1_PreLD = Section3_supptbl3_first6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_8weeks_num), uptake_12weeks_num = sum(children_rec_imm_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PreLD")

Section3_supptbl3_first6in1_LD = Section3_supptbl3_first6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_8weeks_num), uptake_12weeks_num = sum(children_rec_imm_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "LD")

Section3_supptbl3_first6in1_PostLD = Section3_supptbl3_first6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_8weeks_num), uptake_12weeks_num = sum(children_rec_imm_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PostLD")
#Select out 2019 data from full dataset
Section3_supptbl3_first6in1_2019 = Full_SIMD_First6in1 %>% 
  filter(cohort=="Jan-20") %>% 
  select(deprivation_quintile, children_turn_8weeks_2019_num, children_rec_imm_12weeks_2019_num, uptake_12weeks_2019_percent) %>% 
  mutate(Percentagechange2019 = 0) %>% 
  mutate(time_period = "2019")
colnames(Section3_supptbl3_first6in1_2019) = c("deprivation_quintile", "denominator", "uptake_12weeks_num", "uptake_12weeks_percent", "Percentagechange2019", "time_period")
#Join together and round
Section3_supptbl3_first6in1 = full_join(Section3_supptbl3_first6in1_2019, Section3_supptbl3_first6in1_PreLD)
Section3_supptbl3_first6in1 = full_join(Section3_supptbl3_first6in1, Section3_supptbl3_first6in1_LD)
Section3_supptbl3_first6in1 = full_join(Section3_supptbl3_first6in1, Section3_supptbl3_first6in1_PostLD)

Section3_supptbl3_first6in1 = Section3_supptbl3_first6in1 %>% 
  mutate (uptake_12weeks_percent= round (uptake_12weeks_percent, digits = 1)) %>%
  mutate (Percentagechange2019 = round (Percentagechange2019, digits = 1))

##To add OR columns for comparisons with 2019
#SIMD 1
ORSIMD1vs2019column = First6in1_SIMDinteraction_tbl %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "1 - most deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 2
ORSIMD2vs2019column = First6in1_SIMDinteraction_tbl_BL2 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "2") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 3
ORSIMD3vs2019column = First6in1_SIMDinteraction_tbl_BL3 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "3") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 4
ORSIMD4vs2019column = First6in1_SIMDinteraction_tbl_BL4 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "4") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 5
ORSIMD5vs2019column = First6in1_SIMDinteraction_tbl_BL5 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "5 - least deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 
  
#Join the oR tbls together

ORallSIMDvs2019column = rbind(ORSIMD1vs2019column, ORSIMD2vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD3vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD4vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD5vs2019column)

Section3_supptbl3_first6in1_final = full_join(Section3_supptbl3_first6in1, ORallSIMDvs2019column) %>% 
  mutate (OR= round (OR, digits = 1)) %>%
  mutate (lowerCI = round (lowerCI, digits = 1)) %>% 
  mutate (upperCI = round (upperCI, digits = 1)) %>% 
  mutate("Immunisation" = "First 6in1")

####
##Second 6in1
#FIrst pick the data from full dataset

Section3_supptbl3_second6in1 = Full_SIMD_Second6in1 %>% 
  select(cohort, deprivation_quintile, children_turn_12weeks_num, children_rec_imm_16weeks_num, uptake_16weeks_percent, absolute_change_from_baseline_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20"))) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))
#Separate into time periods to summarise data 
Section3_supptbl3_second6in1_PreLD = Section3_supptbl3_second6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12weeks_num), uptake_16weeks_num = sum(children_rec_imm_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PreLD")

Section3_supptbl3_second6in1_LD = Section3_supptbl3_second6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12weeks_num), uptake_16weeks_num = sum(children_rec_imm_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "LD")

Section3_supptbl3_second6in1_PostLD = Section3_supptbl3_second6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12weeks_num), uptake_16weeks_num = sum(children_rec_imm_16weeks_num), uptake_16weeks_percent = mean(uptake_16weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PostLD")
#Select out 2019 data from full dataset
Section3_supptbl3_second6in1_2019 = Full_SIMD_Second6in1 %>% 
  filter(cohort=="Jan-20") %>% 
  select(deprivation_quintile, children_turn_12weeks_2019_num, children_rec_imm_16weeks_2019_num, uptake_16weeks_2019_percent) %>% 
  mutate(Percentagechange2019 = 0) %>% 
  mutate(time_period = "2019")
colnames(Section3_supptbl3_second6in1_2019) = c("deprivation_quintile", "denominator", "uptake_16weeks_num", "uptake_16weeks_percent", "Percentagechange2019", "time_period")
#Join together and round
Section3_supptbl3_second6in1 = full_join(Section3_supptbl3_second6in1_2019, Section3_supptbl3_second6in1_PreLD)
Section3_supptbl3_second6in1 = full_join(Section3_supptbl3_second6in1, Section3_supptbl3_second6in1_LD)
Section3_supptbl3_second6in1 = full_join(Section3_supptbl3_second6in1, Section3_supptbl3_second6in1_PostLD)

Section3_supptbl3_second6in1 = Section3_supptbl3_second6in1 %>% 
  mutate (uptake_16weeks_percent= round (uptake_16weeks_percent, digits = 1)) %>%
  mutate (Percentagechange2019 = round (Percentagechange2019, digits = 1))

##To add OR columns for comparisons with 2019
#SIMD 1
ORSIMD1vs2019column = Second6in1_SIMDinteraction_tbl %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "1 - most deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 2
ORSIMD2vs2019column = Second6in1_SIMDinteraction_tbl_BL2 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "2") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 3
ORSIMD3vs2019column = Second6in1_SIMDinteraction_tbl_BL3 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "3") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 4
ORSIMD4vs2019column = Second6in1_SIMDinteraction_tbl_BL4 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "4") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 5
ORSIMD5vs2019column = Second6in1_SIMDinteraction_tbl_BL5 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "5 - least deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#Join the oR tbls together

ORallSIMDvs2019column = rbind(ORSIMD1vs2019column, ORSIMD2vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD3vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD4vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD5vs2019column)

Section3_supptbl3_second6in1_final = full_join(Section3_supptbl3_second6in1, ORallSIMDvs2019column) %>% 
  mutate (OR= round (OR, digits = 1)) %>%
  mutate (lowerCI = round (lowerCI, digits = 1)) %>% 
  mutate (upperCI = round (upperCI, digits = 1)) %>% 
  mutate("Immunisation" = "Second 6in1")

####
##Third 6in1
#FIrst pick the data from full dataset

Section3_supptbl3_third6in1 = Full_SIMD_Third6in1 %>% 
  select(cohort, deprivation_quintile, children_turn_16weeks_num, children_rec_imm_20weeks_num, uptake_20weeks_percent, absolute_change_from_baseline_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20"))) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))
#Separate into time periods to summarise data 
Section3_supptbl3_third6in1_PreLD = Section3_supptbl3_third6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_16weeks_num), uptake_20weeks_num = sum(children_rec_imm_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PreLD")

Section3_supptbl3_third6in1_LD = Section3_supptbl3_third6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_16weeks_num), uptake_20weeks_num = sum(children_rec_imm_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "LD")

Section3_supptbl3_third6in1_PostLD = Section3_supptbl3_third6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_16weeks_num), uptake_20weeks_num = sum(children_rec_imm_20weeks_num), uptake_20weeks_percent = mean(uptake_20weeks_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PostLD")
#Select out 2019 data from full dataset
Section3_supptbl3_third6in1_2019 = Full_SIMD_Third6in1 %>% 
  filter(cohort=="Jan-20") %>% 
  select(deprivation_quintile, children_turn_16weeks_2019_num, children_rec_imm_20weeks_2019_num, uptake_20weeks_2019_percent) %>% 
  mutate(Percentagechange2019 = 0) %>% 
  mutate(time_period = "2019")
colnames(Section3_supptbl3_third6in1_2019) = c("deprivation_quintile", "denominator", "uptake_20weeks_num", "uptake_20weeks_percent", "Percentagechange2019", "time_period")
#Join together and round
Section3_supptbl3_third6in1 = full_join(Section3_supptbl3_third6in1_2019, Section3_supptbl3_third6in1_PreLD)
Section3_supptbl3_third6in1 = full_join(Section3_supptbl3_third6in1, Section3_supptbl3_third6in1_LD)
Section3_supptbl3_third6in1 = full_join(Section3_supptbl3_third6in1, Section3_supptbl3_third6in1_PostLD)

Section3_supptbl3_third6in1 = Section3_supptbl3_third6in1 %>% 
  mutate (uptake_20weeks_percent= round (uptake_20weeks_percent, digits = 1)) %>%
  mutate (Percentagechange2019 = round (Percentagechange2019, digits = 1))

##To add OR columns for comparisons with 2019
#SIMD 1
ORSIMD1vs2019column = Third6in1_SIMDinteraction_tbl %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "1 - most deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 2
ORSIMD2vs2019column = Third6in1_SIMDinteraction_tbl_BL2 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "2") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 3
ORSIMD3vs2019column = Third6in1_SIMDinteraction_tbl_BL3 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "3") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 4
ORSIMD4vs2019column = Third6in1_SIMDinteraction_tbl_BL4 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "4") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 5
ORSIMD5vs2019column = Third6in1_SIMDinteraction_tbl_BL5 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "5 - least deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#Join the oR tbls together

ORallSIMDvs2019column = rbind(ORSIMD1vs2019column, ORSIMD2vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD3vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD4vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD5vs2019column)

Section3_supptbl3_third6in1_final = full_join(Section3_supptbl3_third6in1, ORallSIMDvs2019column) %>% 
  mutate (OR= round (OR, digits = 1)) %>%
  mutate (lowerCI = round (lowerCI, digits = 1)) %>% 
  mutate (upperCI = round (upperCI, digits = 1)) %>% 
  mutate("Immunisation" = "Third 6in1")

##First MMR
#FIrst pick the data from full dataset

Section3_supptbl3_firstMMR = Full_SIMD_FirstMMR %>% 
  select(cohort, deprivation_quintile, children_turn_12months_num, children_rec_imm_13months_num, uptake_13months_percent, absolute_change_from_baseline_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20"))) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))
#Separate into time periods to summarise data 
Section3_supptbl3_firstMMR_PreLD = Section3_supptbl3_firstMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12months_num), uptake_13months_num = sum(children_rec_imm_13months_num), uptake_13months_percent = mean(uptake_13months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PreLD")

Section3_supptbl3_firstMMR_LD = Section3_supptbl3_firstMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12months_num), uptake_13months_num = sum(children_rec_imm_13months_num), uptake_13months_percent = mean(uptake_13months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "LD")

Section3_supptbl3_firstMMR_PostLD = Section3_supptbl3_firstMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_12months_num), uptake_13months_num = sum(children_rec_imm_13months_num), uptake_13months_percent = mean(uptake_13months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PostLD")
#Select out 2019 data from full dataset
Section3_supptbl3_firstMMR_2019 = Full_SIMD_FirstMMR %>% 
  filter(cohort=="Jan-20") %>% 
  select(deprivation_quintile, children_turn_12months_2019_num, children_rec_imm_13months_2019_num, uptake_13months_2019_percent) %>% 
  mutate(Percentagechange2019 = 0) %>% 
  mutate(time_period = "2019")
colnames(Section3_supptbl3_firstMMR_2019) = c("deprivation_quintile", "denominator", "uptake_13months_num", "uptake_13months_percent", "Percentagechange2019", "time_period")
#Join together and round
Section3_supptbl3_firstMMR = full_join(Section3_supptbl3_firstMMR_2019, Section3_supptbl3_firstMMR_PreLD)
Section3_supptbl3_firstMMR = full_join(Section3_supptbl3_firstMMR, Section3_supptbl3_firstMMR_LD)
Section3_supptbl3_firstMMR = full_join(Section3_supptbl3_firstMMR, Section3_supptbl3_firstMMR_PostLD)

Section3_supptbl3_firstMMR = Section3_supptbl3_firstMMR %>% 
  mutate (uptake_13months_percent= round (uptake_13months_percent, digits = 1)) %>%
  mutate (Percentagechange2019 = round (Percentagechange2019, digits = 1))

##To add OR columns for comparisons with 2019
#SIMD 1
ORSIMD1vs2019column = FirstMMR_SIMDinteraction_tbl %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "1 - most deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 2
ORSIMD2vs2019column = FirstMMR_SIMDinteraction_tbl_BL2 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "2") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 


#SIMD 3
ORSIMD3vs2019column = FirstMMR_SIMDinteraction_tbl_BL3 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "3") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 4
ORSIMD4vs2019column = FirstMMR_SIMDinteraction_tbl_BL4 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "4") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 5
ORSIMD5vs2019column = FirstMMR_SIMDinteraction_tbl_BL5 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "5 - least deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#Join the oR tbls together

ORallSIMDvs2019column = rbind(ORSIMD1vs2019column, ORSIMD2vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD3vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD4vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD5vs2019column)

Section3_supptbl3_firstMMR_final = full_join(Section3_supptbl3_firstMMR, ORallSIMDvs2019column) %>% 
  mutate (OR= round (OR, digits = 1)) %>%
  mutate (lowerCI = round (lowerCI, digits = 1)) %>% 
  mutate (upperCI = round (upperCI, digits = 1)) %>% 
  mutate("Immunisation" = "First MMR")

##Second MMR
#FIrst pick the data from full dataset

Section3_supptbl3_secondMMR = Full_SIMD_SecondMMR %>% 
  select(cohort, deprivation_quintile, children_turn_3y4months_num, children_rec_imm_3y5months_num, uptake_3y5months_percent, absolute_change_from_baseline_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20"))) %>% 
  mutate (lockdown.factor = cohort %>% 
            factor() %>% 
            fct_recode("Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))
#Separate into time periods to summarise data 
Section3_supptbl3_secondMMR_PreLD = Section3_supptbl3_secondMMR %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_3y4months_num), uptake_3y5months_num = sum(children_rec_imm_3y5months_num), uptake_3y5months_percent = mean(uptake_3y5months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PreLD")

Section3_supptbl3_secondMMR_LD = Section3_supptbl3_secondMMR %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_3y4months_num), uptake_3y5months_num = sum(children_rec_imm_3y5months_num), uptake_3y5months_percent = mean(uptake_3y5months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "LD")

Section3_supptbl3_secondMMR_PostLD = Section3_supptbl3_secondMMR %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(deprivation_quintile) %>% 
  summarise(denominator = sum(children_turn_3y4months_num), uptake_3y5months_num = sum(children_rec_imm_3y5months_num), uptake_3y5months_percent = mean(uptake_3y5months_percent), Percentagechange2019 = mean(absolute_change_from_baseline_percent)) %>% 
  mutate(time_period = "PostLD")
#Select out 2019 data from full dataset
Section3_supptbl3_secondMMR_2019 = Full_SIMD_SecondMMR %>% 
  filter(cohort=="Jan-20") %>% 
  select(deprivation_quintile, children_turn_3y4months_2019_num, children_rec_imm_3y5months_2019_num, uptake_3y5months_2019_percent) %>% 
  mutate(Percentagechange2019 = 0) %>% 
  mutate(time_period = "2019")
colnames(Section3_supptbl3_secondMMR_2019) = c("deprivation_quintile", "denominator", "uptake_3y5months_num", "uptake_3y5months_percent", "Percentagechange2019", "time_period")
#Join together and round
Section3_supptbl3_secondMMR = full_join(Section3_supptbl3_secondMMR_2019, Section3_supptbl3_secondMMR_PreLD)
Section3_supptbl3_secondMMR = full_join(Section3_supptbl3_secondMMR, Section3_supptbl3_secondMMR_LD)
Section3_supptbl3_secondMMR = full_join(Section3_supptbl3_secondMMR, Section3_supptbl3_secondMMR_PostLD)

Section3_supptbl3_secondMMR = Section3_supptbl3_secondMMR %>% 
  mutate (uptake_3y5months_percent= round (uptake_3y5months_percent, digits = 1)) %>%
  mutate (Percentagechange2019 = round (Percentagechange2019, digits = 1))

##To add OR columns for comparisons with 2019
#SIMD 1
ORSIMD1vs2019column = SecondMMR_SIMDinteraction_tbl %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "1 - most deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 2
ORSIMD2vs2019column = SecondMMR_SIMDinteraction_tbl_BL2 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "2") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 


#SIMD 3
ORSIMD3vs2019column = FirstMMR_SIMDinteraction_tbl_BL3 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "3") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 4
ORSIMD4vs2019column = SecondMMR_SIMDinteraction_tbl_BL4 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "4") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#SIMD 5
ORSIMD5vs2019column = SecondMMR_SIMDinteraction_tbl_BL5 %>% 
  filter(term == c("tpPreLD", "tpLD", "tpPostLD")) %>% 
  select(term, OR, lowerCI, upperCI) %>% 
  mutate(deprivation_quintile = "5 - least deprived") %>% 
  mutate(time_period = c("PreLD", "LD", "PostLD")) %>% 
  select(deprivation_quintile, OR, lowerCI, upperCI, time_period) 

#Join the oR tbls together

ORallSIMDvs2019column = rbind(ORSIMD1vs2019column, ORSIMD2vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD3vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD4vs2019column)
ORallSIMDvs2019column = rbind(ORallSIMDvs2019column, ORSIMD5vs2019column)

Section3_supptbl3_secondMMR_final = full_join(Section3_supptbl3_secondMMR, ORallSIMDvs2019column) %>% 
  mutate (OR= round (OR, digits = 1)) %>%
  mutate (lowerCI = round (lowerCI, digits = 1)) %>% 
  mutate (upperCI = round (upperCI, digits = 1)) %>% 
  mutate("Immunisation" = "Second MMR")

##########
##Join tables together for giant table
colnames(Section3_supptbl3_first6in1_final) = c("Deprivation quintile", "number eligable", "number received within 4 weeks", "% received within 4 weeks", "Absolute % change from 2019", "time period", "OR", "lowerCI", "upperCI", "Immunisation")
colnames(Section3_supptbl3_second6in1_final) = c("Deprivation quintile", "number eligable", "number received within 4 weeks", "% received within 4 weeks", "Absolute % change from 2019", "time period", "OR", "lowerCI", "upperCI", "Immunisation")
colnames(Section3_supptbl3_third6in1_final) = c("Deprivation quintile", "number eligable", "number received within 4 weeks", "% received within 4 weeks", "Absolute % change from 2019", "time period", "OR", "lowerCI", "upperCI", "Immunisation")
colnames(Section3_supptbl3_firstMMR_final) = c("Deprivation quintile", "number eligable", "number received within 4 weeks", "% received within 4 weeks", "Absolute % change from 2019", "time period", "OR", "lowerCI", "upperCI", "Immunisation")
colnames(Section3_supptbl3_secondMMR_final) = c("Deprivation quintile", "number eligable", "number received within 4 weeks", "% received within 4 weeks", "Absolute % change from 2019", "time period", "OR", "lowerCI", "upperCI", "Immunisation")

Section3_supptbl3_allvaccine= rbind(Section3_supptbl3_first6in1_final, Section3_supptbl3_second6in1_final, Section3_supptbl3_third6in1_final, Section3_supptbl3_firstMMR_final, Section3_supptbl3_secondMMR_final)

Section3_supptbl3_allvaccine = Section3_supptbl3_allvaccine[,c(10,1,6,2,3,4,5,7,8,9)]
write.csv(Section3_supptbl3_allvaccine, file = "Exported tables/Section3_supptbl3_allvaccine.csv")

###Experimental graphs- scatter of %change from 2019 by LD for section 3 supp

Experimental_scatter_LD = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "LD") %>% 
  mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
  ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, color = `Deprivation quintile`, size = `time period`))+
  geom_point(size = 4, shape = 18)+
  scale_y_continuous(breaks = seq(0,20,2))+
  scale_color_brewer(palette="Set2")+
  labs(x = NULL, y = "Absolute % change from 2019", title = "LD")+
  annotate("text", x= "Second 6in1", y = 14, label = "All comparisons with 2019 reach statistical significance
           except First dose 6in1 for SIMD 4 &5, see table S3", size = 3) 
  Experimental_scatter_LD
  
  Experimental_scatter_preLD = Section3_supptbl3_allvaccine %>% 
    filter(`time period`== "PreLD") %>% 
    mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
    ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, color = `Deprivation quintile`, size = `time period`))+
    geom_point(size = 4, shape = 18)+
    scale_y_continuous(breaks = seq(-2,5,1))+
    scale_color_brewer(palette="Set2")+
    geom_hline(yintercept = 0, linetype="dashed", color = "#66c2a5", size=0.75)+
    labs(x = NULL,
         y = "Absolute % change from 2019",
         title = "Pre LD")+
    annotate("text", x= "Second 6in1", y = 3, label = "Comparison with 2019 for all doses 6in1 are ns, 
           changes for MMR reach satistical significance 
             (except First MMR SIMD 2,4 and second MMR SIMD 4), see table S3", size = 3)
Experimental_scatter_preLD

Experimental_scatter_postLD = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "PostLD") %>% 
  mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
  ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, color = `Deprivation quintile`, size = `time period`))+
  geom_point(size = 4, shape = 18)+
  scale_y_continuous(breaks = seq(-2,20,2))+
  scale_color_brewer(palette="Set2")+
  labs(x = NULL,
       y = "Absolute % change from 2019",
       title = "Post LD")+
  geom_hline(yintercept = 0, linetype="dashed", color = "#66c2a5", size=0.75)+
  annotate("text", x= "Second 6in1", y = 11, label = "All comparisons with 2019 reach statistical significance
           except first dose 6in1 for SIMD 2, 3, 4 & 5 
           and second dose 6in1 for SIMD 4, see table S3", size = 3)
Experimental_scatter_postLD


#Sig checks
Experimental_scatter_preLD_sig = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "PreLD") ###add a annotate to graph that 6in1 changes are NS, MMR are
Experimental_scatter_LD_sig = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "LD") ###All sig except first 6in1 SIMD 4 and 5
Experimental_scatter_postLD_sig = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "PostLD")##First6in1 2 4 and 5 ns, others are sig
##Bar charts
Experimental_bar_LD = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "LD") %>% 
  mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
  ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, fill = `Deprivation quintile`))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks = seq(0,20,2))+
  scale_fill_brewer(palette="GnBu")+
  labs(x = NULL, y = "Absolute % change from 2019", title = "LD")+
  annotate("text", x= "Second 6in1", y = 14, label = "All comparisons with 2019 reach statistical significance
           except First dose 6in1 for SIMD 4 &5, see table", size = 3) 
Experimental_bar_LD

Experimental_bar_preLD = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "PreLD") %>% 
  mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
  ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, fill = `Deprivation quintile`))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks = seq(-2,5,1))+
  scale_fill_brewer(palette="GnBu")+
  geom_hline(yintercept = 0, linetype="dashed", color = "#66c2a5", size=0.75)+
  labs(x = NULL,
       y = "Absolute % change from 2019",
       title = "Pre LD")+
  annotate("text", x= "Second 6in1", y = 3, label = "Comparison with 2019 for all doses 6in1 are ns, 
           changes for MMR reach satistical significance, see table", size = 3) 
Experimental_bar_preLD

Experimental_bar_postLD = Section3_supptbl3_allvaccine %>% 
  filter(`time period`== "PostLD") %>% 
  mutate(Immunisation = factor(Immunisation, levels=c("First 6in1", "Second 6in1", "Third 6in1", "First MMR", "Second MMR"))) %>%
  ggplot(aes(x= `Immunisation`, y= `Absolute % change from 2019`, fill = `Deprivation quintile`))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(breaks = seq(-2,20,2))+
  scale_fill_brewer(palette="GnBu")+
  labs(x = NULL,
       y = "Absolute % change from 2019",
       title = "Post LD")+
  geom_hline(yintercept = 0, linetype="dashed", color = "#66c2a5", size=0.75)+
  annotate("text", x= "Second 6in1", y = 11, label = "All comparisons with 2019 reach statistical significance
           except First dose 6in1 for SIMD 2, 4 & 5, see table", size = 3) 
Experimental_bar_postLD
#Export as pdf
Changefrom2019_SIMD_allvaccines = ggarrange(Experimental_bar_preLD, Experimental_bar_LD, Experimental_bar_postLD,
                                            labels = NULL,
                                            common.legend = TRUE, legend="bottom",
                                            ncol = 2, nrow = 2)  
Changefrom2019_SIMD_allvaccines

Changefrom2019_SIMD_allvaccines_scatter = ggarrange(Experimental_scatter_preLD, Experimental_scatter_LD, Experimental_scatter_postLD,
                                            labels = NULL,
                                            common.legend = TRUE, legend="bottom",
                                            ncol = 2, nrow = 2)  
Changefrom2019_SIMD_allvaccines_scatter

###Plot lines just between 2019 and LD by SIMD
#First6in1
First6in1_groupedSIMD_2019_line = SIMD_First6in1_grouped %>%
  filter(lockdown.factor== "2019")
First6in1_groupedSIMD_LD_line = SIMD_First6in1_grouped %>%
  filter(lockdown.factor== "LD")
First6in1_groupedSIMD_2019LD_line = rbind(First6in1_groupedSIMD_2019_line, First6in1_groupedSIMD_LD_line)

First6in1_groupedSIMD_2019LD_lineplot = First6in1_groupedSIMD_2019LD_line %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose 6in1 2019 to LD")+
  expand_limits(y=85:100)+
  scale_y_continuous(breaks = c(85,90,95,100))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
First6in1_groupedSIMD_2019LD_lineplot

#Third6in1

Third6in1_groupedSIMD_2019_line = SIMD_Third6in1_grouped %>%
  filter(lockdown.factor== "2019")
Third6in1_groupedSIMD_LD_line = SIMD_Third6in1_grouped %>%
  filter(lockdown.factor== "LD")
Third6in1_groupedSIMD_2019LD_line = rbind(Third6in1_groupedSIMD_2019_line, Third6in1_groupedSIMD_LD_line)

Third6in1_groupedSIMD_2019LD_lineplot = Third6in1_groupedSIMD_2019LD_line %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose 6in1 2019 to LD")+
  expand_limits(y=65:100)+
  scale_y_continuous(breaks = c(65,70,75,80,85,90,95,100))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
Third6in1_groupedSIMD_2019LD_lineplot

#First MMR
FirstMMR_groupedSIMD_2019_line = SIMD_FirstMMR_grouped %>%
  filter(lockdown.factor== "2019")
FirstMMR_groupedSIMD_LD_line = SIMD_FirstMMR_grouped %>%
  filter(lockdown.factor== "LD")
FirstMMR_groupedSIMD_2019LD_line = rbind(FirstMMR_groupedSIMD_2019_line, FirstMMR_groupedSIMD_LD_line)

FirstMMR_groupedSIMD_2019LD_lineplot = FirstMMR_groupedSIMD_2019LD_line %>% 
  ggplot(aes(x=lockdown.factor, y=mean_percent, group=deprivation_quintile, color=deprivation_quintile)) +
  geom_line()+
  theme_classic()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "FirstMMR 2019 to LD")+
  expand_limits(y=60:100)+
  scale_y_continuous(breaks = c(60,65,70,75,80,85,90,95,100))+
  scale_color_brewer(palette="Set2", name = "Deprivation quintile")
FirstMMR_groupedSIMD_2019LD_lineplot

###Try to make table S4 interaction table, first arrange tables for each vaccine

FigS4_First6in1_SIMDinteractiontbl = First6in1_SIMDinteraction_tbl %>% 
  slice(8:19) %>% 
  mutate(Immunisation = "First 6in1") %>% 
  select(term, OR, p.value, lowerCI, upperCI, Immunisation) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (p.value= round (p.value, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>% 
  mutate (upperCI= round (upperCI, digits = 2)) 

FigS4_Second6in1_SIMDinteractiontbl = Second6in1_SIMDinteraction_tbl %>% 
  slice(8:19) %>% 
  mutate(Immunisation = "Second 6in1") %>% 
  select(term, OR, p.value, lowerCI, upperCI, Immunisation) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (p.value= round (p.value, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>% 
  mutate (upperCI= round (upperCI, digits = 2)) 

FigS4_Third6in1_SIMDinteractiontbl = Third6in1_SIMDinteraction_tbl %>% 
  slice(8:19) %>% 
  mutate(Immunisation = "Third 6in1") %>% 
  select(term, OR, p.value, lowerCI, upperCI, Immunisation) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (p.value= round (p.value, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>% 
  mutate (upperCI= round (upperCI, digits = 2)) 

FigS4_FirstMMR_SIMDinteractiontbl = FirstMMR_SIMDinteraction_tbl %>% 
  slice(8:19) %>% 
  mutate(Immunisation = "First MMR") %>% 
  select(term, OR, p.value, lowerCI, upperCI, Immunisation) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (p.value= round (p.value, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>% 
  mutate (upperCI= round (upperCI, digits = 2)) 

FigS4_SecondMMR_SIMDinteractiontbl = SecondMMR_SIMDinteraction_tbl %>% 
  slice(8:19) %>% 
  mutate(Immunisation = "Second MMR") %>% 
  select(term, OR, p.value, lowerCI, upperCI, Immunisation) %>% 
  mutate (OR= round (OR, digits = 2)) %>% 
  mutate (p.value= round (p.value, digits = 2)) %>% 
  mutate (lowerCI= round (lowerCI, digits = 2)) %>% 
  mutate (upperCI= round (upperCI, digits = 2)) 
#and join

FigS4_allvaccines_SIMDinteractiontbl = rbind(FigS4_First6in1_SIMDinteractiontbl, FigS4_Second6in1_SIMDinteractiontbl, FigS4_Third6in1_SIMDinteractiontbl, FigS4_FirstMMR_SIMDinteractiontbl, FigS4_SecondMMR_SIMDinteractiontbl)
FigS4_allvaccines_SIMDinteractiontbl = FigS4_allvaccines_SIMDinteractiontbl[,c(6,1,2,3,4,5)]

write_csv(FigS4_allvaccines_SIMDinteractiontbl, file = "Exported tables/FigS4_allvaccines_SIMDinteractiontbl.csv")
