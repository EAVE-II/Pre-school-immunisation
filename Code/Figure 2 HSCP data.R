#######################
##Figure 2 HSCP data by LD period
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
library(geojsonio)
library(rgdal)
library(tmap)
library(viridis)

phs_main <- rgb(67,53,139, maxColorValue = 255)
phs_purple<- rgb(150,64,145, maxColorValue = 255)
phs_purple2 <- rgb(208,145,205, maxColorValue = 255)
phs_spec2 <- colorRampPalette(c(phs_main, phs_purple2))


#Loading datasets (pre load modification, removed "NHS" from Orkney, Shetland and Western Isles)
Full_firstdose_6in1 = read.csv(here("Data", "First_dose_6in1_3_feb_21.csv"))
Full_seconddose_6in1 = read.csv(here("Data", "Second_dose_6in1_3_feb_21.csv"))
Full_thirddose_6in1 = read.csv(here("Data", "Third_dose_6in1_3_feb_21.csv"))
Full_firstdose_MMR = read.csv(here("Data", "First_dose_MMR_3_feb_21.csv"))
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))####excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

###Change island names for all at the beginning
Full_firstdose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_6in1$area_name)
Full_firstdose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_6in1$area_name)

Full_seconddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_6in1$area_name)
Full_seconddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_6in1$area_name)

Full_thirddose_6in1$area_name = gsub("Shetland", "Shetland Islands", Full_thirddose_6in1$area_name)
Full_thirddose_6in1$area_name = gsub("Orkney", "Orkney Islands", Full_thirddose_6in1$area_name)

Full_firstdose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_firstdose_MMR$area_name)
Full_firstdose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_firstdose_MMR$area_name)

Full_seconddose_MMR$area_name = gsub("Shetland", "Shetland Islands", Full_seconddose_MMR$area_name)
Full_seconddose_MMR$area_name = gsub("Orkney", "Orkney Islands", Full_seconddose_MMR$area_name)

#Load HSCP map from https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=714BD98E15D22A8116824CA25B30DC02#/metadata/ac5e870f-8fc2-4c21-8b9c-3bd2311a583f
HSCP_map <- readOGR(here("Data"), layer="SG_NHS_IntegrationAuthority_2019", verbose=FALSE)

##Select out percentage uptake for 2019 for HSPC. Remove NHS boards and Scotland

#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map


#First 6in1 LD
HSCP_LDdata_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
##Need to get mean data for each HSCP then plot as above

#First dose MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

newPolyMMR <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPolyMMR)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% vacinnated")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map


##Plot percent change
#First 6in1
#Set up the percent change table by selecting out the 2019 and LD data into tables then join
#2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

First6in1_HSCP_2019_percent = HSCP_2019data_First6in1 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
First6in1_HSCP_LD_percent = Full_firstdose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
  
First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data
First6in1_Islands_LD_percent = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

First6in1_Islands_LD_percent = First6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
First6in1_HSCP_LD_percent = rbind(First6in1_HSCP_LD_percent, First6in1_Islands_LD_percent)

#Join 2019 and LD tbls
First6in1_HSPC_percentchange_2019andLD = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city and Scottish borders

First6in1_HSPC_percentchange_2019andLD = First6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)


#Plot percent change 

library(tmap)
library(viridis)
First6in1_percentchange_poly <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map = tm_shape(First6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "Absolute % change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map
#Bar chart plot
library(RColorBrewer)
First6in1_HSPC_percentchange_bar = First6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  labs(x=NULL)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
First6in1_HSPC_percentchange_bar

#Second 6in1
#2010
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Second6in1_HSCP_2019_percent = HSCP_2019data_Second6in1 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")
#LD
Second6in1_HSCP_LD_percent = Full_seconddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Second6in1_Islands_LD_percent = Full_seconddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Second6in1_Islands_LD_percent = Second6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Second6in1_HSCP_LD_percent = rbind(Second6in1_HSCP_LD_percent,Second6in1_Islands_LD_percent)

#Join 2019 and LD
Second6in1_HSPC_percentchange_2019andLD = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)  

Second6in1_HSPC_percentchange_2019andLD = Second6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Second6in1_percentchange_poly <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map = tm_shape(Second6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "Absolute % change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map

#Bar chart plot
Second6in1_HSPC_percentchange_bar = Second6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Second6in1_HSPC_percentchange_bar
#Third 6in1

HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Third6in1_HSCP_2019_percent = HSCP_2019data_Third6in1 %>% 
  select(area_name, uptake_20weeks_percent)
colnames(Third6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")

Third6in1_HSCP_LD_percent = Full_thirddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
Third6in1_Islands_LD_percent = Full_thirddose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

Third6in1_Islands_LD_percent = Third6in1_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_20weeks_percent))
colnames(Third6in1_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
Third6in1_HSCP_LD_percent = rbind(Third6in1_HSCP_LD_percent,Third6in1_Islands_LD_percent)
#Join 2019 and LD

Third6in1_HSPC_percentchange_2019andLD = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)  

Third6in1_HSPC_percentchange_2019andLD = Third6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Third6in1_percentchange_poly <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map = tm_shape(Third6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "Absolute % change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map

#Bar chart plot
Third6in1_HSPC_percentchange_bar = Third6in1_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7))+
  labs(x=NULL)+
  coord_flip()
Third6in1_HSPC_percentchange_bar

#First MMR
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

FirstMMR_HSCP_2019_percent = HSCP_2019data_FirstMMR %>% 
  select(area_name, uptake_13m_percent)
colnames(FirstMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

FirstMMR_HSCP_LD_percent = Full_firstdose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
FirstMMR_Islands_LD_percent = Full_firstdose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

FirstMMR_Islands_LD_percent = FirstMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_13m_percent))
colnames(FirstMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
FirstMMR_HSCP_LD_percent = rbind(FirstMMR_HSCP_LD_percent,FirstMMR_Islands_LD_percent)
#Join 2019 and LD
FirstMMR_HSPC_percentchange_2019andLD = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city 

FirstMMR_HSPC_percentchange_2019andLD = FirstMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
FirstMMR_percentchange_poly <- merge(x=HSCP_map, y=FirstMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_MMR_percentchange_map = tm_shape(FirstMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "Absolute % change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First MMR",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_percentchange_map ##NOte change in pallet as none fell. Cross checked for Moray
#Bar chart plot
FirstMMR_HSPC_percentchange_bar = FirstMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
FirstMMR_HSPC_percentchange_bar

#Second MMR ++++excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

SecondMMR_HSCP_2019_percent = HSCP_2019data_SecondMMR %>% 
  select(area_name, uptake_3y5m_percent)
colnames(SecondMMR_HSCP_2019_percent) = c("area_name", "Uptake_2019")

SecondMMR_HSCP_LD_percent = Full_seconddose_MMR %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_HSCP_LD_percent) = c("area_name", "Uptake_LD")

##To get Islands only data for LD
SecondMMR_Islands_LD_percent = Full_seconddose_MMR %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20")))

SecondMMR_Islands_LD_percent = SecondMMR_Islands_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_3y5m_percent))
colnames(SecondMMR_Islands_LD_percent) = c("area_name", "Uptake_LD")

#Join island LD with other HSCP
SecondMMR_HSCP_LD_percent = rbind(SecondMMR_HSCP_LD_percent,SecondMMR_Islands_LD_percent)
#Join 2019 and LD

SecondMMR_HSPC_percentchange_2019andLD = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)  

SecondMMR_HSPC_percentchange_2019andLD = SecondMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
SecondMMR_percentchange_poly <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map = tm_shape(SecondMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "Absolute % change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map
#Bar chart plot
SecondMMR_HSPC_percentchange_bar = SecondMMR_HSPC_percentchange_2019andLD %>% 
  ggplot(aes(y=percent_change, x=area_name)) + 
  geom_bar(position="dodge", stat="identity", color=rgb(0.1,0.4,0.5,0.7), fill= rgb(0.1,0.4,0.5,0.7))+
  theme(legend.position="none")+
  theme_bw()+
  labs(x=NULL)+
  theme(axis.text.y = element_text(size = 7))+
  coord_flip()
SecondMMR_HSPC_percentchange_bar

##Export the % change maps and bar charts in one pdf
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Percent_change_bar_HSCP_6in1 = ggarrange(First6in1_HSPC_percentchange_bar, Second6in1_HSPC_percentchange_bar, Third6in1_HSPC_percentchange_bar,
                               labels = c("First6in1", "Second6in1", "Third6in1"), hjust = -1,
                               legend=NULL,
                               ncol = 2, nrow = 2)

Percent_change_bar_HSCP_6in1

Percent_change_bar_HSCP_MMR = ggarrange(FirstMMR_HSPC_percentchange_bar, SecondMMR_HSPC_percentchange_bar, 
                                         labels = c("First MMR","Second MMR"), hjust = -1.5,
                                         legend=NULL,
                                         ncol = 2, nrow = 1)

Percent_change_bar_HSCP_MMR


##To get a grouped bar chart

#Grouped bar chart LD vs 2019 percent uptake First 6in1

First6in1_HSCP_2019_percent = First6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(First6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(First6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

First6in1_Absolute_percentage = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)

Grouped_First6in1_2019LDpercentages = First6in1_Absolute_percentage %>%
  mutate(Area = fct_reorder(Area, Uptake)) %>% 
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  coord_flip()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 93.9, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "#fc9272", size=0.75)+
  geom_hline(yintercept = 94.9, linetype="dotted", ##Scotland wide mean uptake LD
             color = "#de2d26", size=0.75)+
  scale_fill_brewer(palette="Reds")

Grouped_First6in1_2019LDpercentages


#Grouped bar chart LD vs 2019 percent uptake Second 6in1

Second6in1_HSCP_2019_percent = Second6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Second6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Second6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Second6in1_Absolute_percentage = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)

Grouped_Second6in1_2019LDpercentages = Second6in1_Absolute_percentage %>%
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  coord_flip()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 84.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "red", size=0.75)+
  geom_hline(yintercept = 90, linetype="dotted", ##Scotland wide mean uptake LD+++CONFIRM
             color = "blue", size=0.75)

Grouped_Second6in1_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake Third 6in1

Third6in1_HSCP_2019_percent = Third6in1_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(Third6in1_HSCP_2019_percent)=c("Area","Uptake","Time period")
Third6in1_HSCP_LD_percent = Third6in1_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(Third6in1_HSCP_LD_percent)=c("Area","Uptake","Time period")

Third6in1_Absolute_percentage = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)

Grouped_Third6in1_2019LDpercentages = Third6in1_Absolute_percentage %>%
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  coord_flip()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Third dose6in1")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 73, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "red", size=0.75)+
  geom_hline(yintercept = 83, linetype="dotted", ##Scotland wide mean uptake LD+++CONFIRM
             color = "blue", size=0.75)

Grouped_Third6in1_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake First MMR

FirstMMR_HSCP_2019_percent = FirstMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(FirstMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
FirstMMR_HSCP_LD_percent = FirstMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(FirstMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

FirstMMR_Absolute_percentage = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)

Grouped_FirstMMR_2019LDpercentages = FirstMMR_Absolute_percentage %>%
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  coord_flip()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "First dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 65.2, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "red", size=0.75)+
  geom_hline(yintercept = 78, linetype="dotted", ##Scotland wide mean uptake LD+++CONFIRM
             color = "blue", size=0.75)

Grouped_FirstMMR_2019LDpercentages

#Grouped bar chart LD vs 2019 percent uptake Second MMR

SecondMMR_HSCP_2019_percent = SecondMMR_HSCP_2019_percent %>% 
  mutate("Time period" = "2019")
colnames(SecondMMR_HSCP_2019_percent)=c("Area","Uptake","Time period")
SecondMMR_HSCP_LD_percent = SecondMMR_HSCP_LD_percent %>% 
  mutate("Time period" = "LD")
colnames(SecondMMR_HSCP_LD_percent)=c("Area","Uptake","Time period")

SecondMMR_Absolute_percentage = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)

Grouped_SecondMMR_2019LDpercentages = SecondMMR_Absolute_percentage %>%
  ggplot(aes(fill=`Time period` , y=Uptake, x=Area)) +
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  coord_flip()+
  labs(x = NULL,
       y = "% vaccinated (4 weeks)",
       title = "Second dose MMR")+
  theme(axis.text.y = element_text(size = 7))+
  geom_hline(yintercept = 51.8, linetype="dotted", ##Scotland wide mean uptake 2019
             color = "red", size=0.75)+
  geom_hline(yintercept = 61, linetype="dotted", ##Scotland wide mean uptake LD+++CONFIRM
             color = "blue", size=0.75)

Grouped_SecondMMR_2019LDpercentages

###Export grouped bar charts as one figure
Grouped_bar_HSCP_LDvs2019percentages_6in1 = ggarrange(Grouped_First6in1_2019LDpercentages,Grouped_Second6in1_2019LDpercentages, Grouped_Third6in1_2019LDpercentages,
                                  common.legend = TRUE, legend="bottom",
                                  labels = NULL,
                                  ncol = 2, nrow = 2)
Grouped_bar_HSCP_LDvs2019percentages_6in1

Grouped_bar_HSCP_LDvs2019percentages_MMR = ggarrange(Grouped_FirstMMR_2019LDpercentages, Grouped_SecondMMR_2019LDpercentages,
                                                      common.legend = TRUE, legend="bottom",
                                                      labels = NULL,
                                                      ncol = 2, nrow = 1)
Grouped_bar_HSCP_LDvs2019percentages_MMR

####Logistic regression analysis of uptake rates between HSCP for 2019 and LD
##First 6in1 Log regression
#Fisrt 6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_First6in1 = HSCP_2019data_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls
  
First6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_First6in1$uptake_12weeks_num, HSCP_2019data_First6in1$unvaccinated)

model_first6in1_HSCP_2019 = glm(First6in1_HSCP2019_regression_tbl ~ HSCP_2019data_First6in1$area.factor,
                                    family="binomial")

summary(model_first6in1_HSCP_2019)

exp(model_first6in1_HSCP_2019$coefficients)
exp(confint(model_first6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
First6in1_HSCP2019model_tbl = model_first6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#First 6in1 plot
ORandCI_first6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(First6in1_HSCP2019model_tbl$OR),
  boxCILow = c(First6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(First6in1_HSCP2019model_tbl$lowerCI))

First6in1_HSCP2019_forest <- ggplot(ORandCI_first6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
First6in1_HSCP2019_forest = First6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

First6in1_HSCP2019_forest

anova(model_first6in1_HSCP_2019, test="LRT")

##First 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get he correct results ? as using agregate data

#Set up tbl with vaccinated and unvaccinated factors

HSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  mutate(unvaccinated = denominator-uptake_12weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_12weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_First6in1 = HSCP_LDdata_First6in1 %>% 
  select(area.factor, uptake_12weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_12weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_First6in1 = SummaryHSCP_LDdata_First6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirst6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_First6in1$total_vaccinated, SummaryHSCP_LDdata_First6in1$total_unvaccinated)

Summarymodel_first6in1_HSCP_LD = glm(SummaryFirst6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_First6in1$area.factor,
                              family="binomial")

summary(Summarymodel_first6in1_HSCP_LD)

exp(Summarymodel_first6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_first6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirst6in1_HSCPLDmodel_tbl = Summarymodel_first6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_first6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirst6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirst6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirst6in1_HSCPLDmodel_tbl$lowerCI))

SummaryFirst6in1_HSCPLD_forest <- ggplot(SummaryORandCI_first6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirst6in1_HSCPLD_forest = SummaryFirst6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirst6in1_HSCPLD_forest

anova(Summarymodel_first6in1_HSCP_LD, test="LRT")

#Export First 6in1 plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_6in1_OR_plots = ggarrange(First6in1_HSCP2019_forest, SummaryFirst6in1_HSCPLD_forest,
                              labels = c("First6in12019","First6in1LD" ),hjust = -1.5,
                              legend = NULL,
                              ncol = 2, nrow = 1)
First_6in1_OR_plots

#####Second 6in1 log regression
##Second6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_Second6in1 = HSCP_2019data_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Second6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Second6in1$uptake_16weeks_num, HSCP_2019data_Second6in1$unvaccinated)

model_second6in1_HSCP_2019 = glm(Second6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Second6in1$area.factor,
                                family="binomial")

summary(model_second6in1_HSCP_2019)

exp(model_second6in1_HSCP_2019$coefficients)
exp(confint(model_second6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Second6in1_HSCP2019model_tbl = model_second6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second 6in1 2019 plot
ORandCI_second6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Second6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Second6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Second6in1_HSCP2019model_tbl$lowerCI))

Second6in1_HSCP2019_forest <- ggplot(ORandCI_second6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Second6in1_HSCP2019_forest = Second6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)

Second6in1_HSCP2019_forest

anova(model_second6in1_HSCP_2019, test="LRT")

##Second 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  mutate(unvaccinated = denominator-uptake_16weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_16weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Second6in1 = HSCP_LDdata_Second6in1 %>% 
  select(area.factor, uptake_16weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_16weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Second6in1 = SummaryHSCP_LDdata_Second6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecond6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Second6in1$total_vaccinated, SummaryHSCP_LDdata_Second6in1$total_unvaccinated)

Summarymodel_second6in1_HSCP_LD = glm(SummarySecond6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Second6in1$area.factor,
                                     family="binomial")

summary(Summarymodel_second6in1_HSCP_LD)

exp(Summarymodel_second6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_second6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecond6in1_HSCPLDmodel_tbl = Summarymodel_second6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_second6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummarySecond6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecond6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecond6in1_HSCPLDmodel_tbl$lowerCI))

SummarySecond6in1_HSCPLD_forest <- ggplot(SummaryORandCI_second6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummarySecond6in1_HSCPLD_forest = SummarySecond6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecond6in1_HSCPLD_forest

anova(Summarymodel_second6in1_HSCP_LD, test="LRT")
#Export Second 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_6in1_OR_plots = ggarrange(Second6in1_HSCP2019_forest, SummarySecond6in1_HSCPLD_forest,
                                labels = c("Second6in1 2019","Second6in1 LD" ),hjust = -0.75,
                                legend = NULL,
                                ncol = 2, nrow = 1)
Second_6in1_OR_plots

#####THird 6in1 log regression
##Third6in1 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE- for this vaccine, midlothian uptake is lower than SCottish mean

HSCP_2019data_Third6in1 = HSCP_2019data_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

Third6in1_HSCP2019_regression_tbl = cbind(HSCP_2019data_Third6in1$uptake_20weeks_num, HSCP_2019data_Third6in1$unvaccinated)

model_third6in1_HSCP_2019 = glm(Third6in1_HSCP2019_regression_tbl ~ HSCP_2019data_Third6in1$area.factor,
                                 family="binomial")

summary(model_third6in1_HSCP_2019)

exp(model_third6in1_HSCP_2019$coefficients)
exp(confint(model_third6in1_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
Third6in1_HSCP2019model_tbl = model_third6in1_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls 
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Third 6in1 2019 plot
ORandCI_third6in1_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(Third6in1_HSCP2019model_tbl$OR),
  boxCILow = c(Third6in1_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(Third6in1_HSCP2019model_tbl$lowerCI))

Third6in1_HSCP2019_forest <- ggplot(ORandCI_third6in1_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
Third6in1_HSCP2019_forest = Third6in1_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
Third6in1_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Third 6in1 LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  mutate(unvaccinated = denominator-uptake_20weeks_num) %>% 
  mutate (vaccinated.factor = 
            uptake_20weeks_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_Third6in1 = HSCP_LDdata_Third6in1 %>% 
  select(area.factor, uptake_20weeks_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_20weeks_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

SummaryHSCP_LDdata_Third6in1 = SummaryHSCP_LDdata_Third6in1 %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryThird6in1_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_Third6in1$total_vaccinated, SummaryHSCP_LDdata_Third6in1$total_unvaccinated)

Summarymodel_third6in1_HSCP_LD = glm(SummaryThird6in1_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_Third6in1$area.factor,
                                      family="binomial")

summary(Summarymodel_third6in1_HSCP_LD)

exp(Summarymodel_third6in1_HSCP_LD$coefficients)
exp(confint(Summarymodel_third6in1_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryThird6in1_HSCPLDmodel_tbl = Summarymodel_third6in1_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_third6in1_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryThird6in1_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryThird6in1_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryThird6in1_HSCPLDmodel_tbl$lowerCI))

SummaryThird6in1_HSCPLD_forest <- ggplot(SummaryORandCI_third6in1_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryThird6in1_HSCPLD_forest = SummaryThird6in1_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryThird6in1_HSCPLD_forest

anova(Summarymodel_third6in1_HSCP_LD, test="LRT")
#Export Third 6in1 OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Third_6in1_OR_plots = ggarrange(Third6in1_HSCP2019_forest, SummaryThird6in1_HSCPLD_forest,
                                 labels = c("Third6in1 2019","Third6in1 LD" ),hjust = -0.75,
                                 legend = NULL,
                                 ncol = 2, nrow = 1)
Third_6in1_OR_plots

#####First MMR log regression
##First MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian as it follows the Scottish pattern (alternative could be the borders)

HSCP_2019data_FirstMMR = HSCP_2019data_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

FirstMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_FirstMMR$uptake_13m_num, HSCP_2019data_FirstMMR$unvaccinated)

model_firstMMR_HSCP_2019 = glm(FirstMMR_HSCP2019_regression_tbl ~ HSCP_2019data_FirstMMR$area.factor,
                                family="binomial")

summary(model_firstMMR_HSCP_2019)

exp(model_firstMMR_HSCP_2019$coefficients)
exp(confint(model_firstMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
FirstMMR_HSCP2019model_tbl = model_firstMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCP = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#FirstMMR 2019 plot
ORandCI_firstMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCP):1,
  boxOdds = c(FirstMMR_HSCP2019model_tbl$OR),
  boxCILow = c(FirstMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(FirstMMR_HSCP2019model_tbl$lowerCI))

FirstMMR_HSCP2019_forest <- ggplot(ORandCI_firstMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCP))
FirstMMR_HSCP2019_forest = FirstMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
FirstMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##FirstMMR LD2020
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD daa table as not done above

HSCP_LDdata_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  mutate(unvaccinated = denominator-uptake_13m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_13m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_FirstMMR = HSCP_LDdata_FirstMMR %>% 
  select(area.factor, uptake_13m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_13m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_FirstMMR = SummaryHSCP_LDdata_FirstMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummaryFirstMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_FirstMMR$total_vaccinated, SummaryHSCP_LDdata_FirstMMR$total_unvaccinated)

Summarymodel_firstMMR_HSCP_LD = glm(SummaryFirstMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_FirstMMR$area.factor,
                                     family="binomial")

summary(Summarymodel_firstMMR_HSCP_LD)

exp(Summarymodel_firstMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_firstMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummaryFirstMMR_HSCPLDmodel_tbl = Summarymodel_firstMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls
boxLabelsHSCPLD = c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "Moray", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney

SummaryORandCI_firstMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLD):1,
  boxOdds = c(SummaryFirstMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummaryFirstMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummaryFirstMMR_HSCPLDmodel_tbl$lowerCI))

SummaryFirstMMR_HSCPLD_forest <- ggplot(SummaryORandCI_firstMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLD))
SummaryFirstMMR_HSCPLD_forest = SummaryFirstMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummaryFirstMMR_HSCPLD_forest

anova(Summarymodel_firstMMR_HSCP_LD, test="LRT")
#Export FirstMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

First_MMR_OR_plots = ggarrange(FirstMMR_HSCP2019_forest, SummaryFirstMMR_HSCPLD_forest,
                                labels = c("FirstMMR 2019","FirstMMR LD" ),hjust = -1,
                                legend = NULL,
                                ncol = 2, nrow = 1)
First_MMR_OR_plots


#####Second MMR log regression
##Second MMR 2019
#Set up tbl with vaccinated and unvaccianted factors

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>%  ###For some reason, the vaccinated/unvaccinated factors don't copy into the regression table
            factor()) %>% 
  mutate(area.factor = area_name)
#Relevel to comparison with Midlothian NOTE Midlothian is average avergae for this dose

HSCP_2019data_SecondMMR = HSCP_2019data_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))
#Set up regression tbls

SecondMMR_HSCP2019_regression_tbl = cbind(HSCP_2019data_SecondMMR$uptake_3y5m_num, HSCP_2019data_SecondMMR$unvaccinated)

model_secondMMR_HSCP_2019 = glm(SecondMMR_HSCP2019_regression_tbl ~ HSCP_2019data_SecondMMR$area.factor,
                               family="binomial")

summary(model_secondMMR_HSCP_2019)

exp(model_secondMMR_HSCP_2019$coefficients)
exp(confint(model_secondMMR_HSCP_2019)) 
##Make a plot showing OR and CI compared to baseline Midlothian in 2019
#Make the OR and CI tbls, remove the intercept
library(broom)
SecondMMR_HSCP2019model_tbl = model_secondMMR_HSCP_2019%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls+++NOTE removed NHS Grampian areas
boxLabelsHSCPsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Orkney", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "Shetland", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian", "Western Isles")

#Second MMR 2019 plot
ORandCI_secondMMR_HSCP2019 <- data.frame(
  yAxis = length(boxLabelsHSCPsecondMMR):1,
  boxOdds = c(SecondMMR_HSCP2019model_tbl$OR),
  boxCILow = c(SecondMMR_HSCP2019model_tbl$upperCI),
  boxCIHigh = c(SecondMMR_HSCP2019model_tbl$lowerCI))

SecondMMR_HSCP2019_forest <- ggplot(ORandCI_secondMMR_HSCP2019, aes(x = boxOdds, y = boxLabelsHSCPsecondMMR))
SecondMMR_HSCP2019_forest = SecondMMR_HSCP2019_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "2019 OR compared to Midlothian",
       y = NULL,
       title = NULL)
SecondMMR_HSCP2019_forest

anova(model_third6in1_HSCP_2019, test="LRT")

##Second MMR LD2020 
##Summarise data before entering into the model- you have to do this to get the correct results ? as using aggregate data

#Set up tbl with vaccinated and unvaccinated factors. NB need to generate the LD data table as not done above

HSCP_LDdata_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

HSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  mutate(unvaccinated = denominator-uptake_3y5m_num) %>% 
  mutate (vaccinated.factor = 
            uptake_3y5m_num %>% 
            factor()) %>% 
  mutate (unvaccinated.factor = 
            unvaccinated %>% 
            factor()) %>% 
  mutate(area.factor = area_name)
#Summarise the data

SummaryHSCP_LDdata_SecondMMR = HSCP_LDdata_SecondMMR %>% 
  select(area.factor, uptake_3y5m_num, unvaccinated) %>% 
  group_by(area.factor) %>% 
  summarise(total_vaccinated = sum(uptake_3y5m_num), total_unvaccinated = sum(unvaccinated))
#Relevel to comparison with Midlothian as it follows the Scottish pattern NB MIdlothian above SCottish average fo this dose

SummaryHSCP_LDdata_SecondMMR = SummaryHSCP_LDdata_SecondMMR %>% 
  mutate(area.factor = area.factor %>%
           fct_relevel("Midlothian"))

#Set up regression tbls
SummarySecondMMR_HSCPLD_regression_tbl = cbind(SummaryHSCP_LDdata_SecondMMR$total_vaccinated, SummaryHSCP_LDdata_SecondMMR$total_unvaccinated)

Summarymodel_secondMMR_HSCP_LD = glm(SummarySecondMMR_HSCPLD_regression_tbl ~ SummaryHSCP_LDdata_SecondMMR$area.factor,
                                    family="binomial")

summary(Summarymodel_secondMMR_HSCP_LD)

exp(Summarymodel_secondMMR_HSCP_LD$coefficients)
exp(confint(Summarymodel_secondMMR_HSCP_LD))

###Make a plot showing OR and CI compared to baseline Midlothian in LD
#Make the OR and CI tbls, remove the intercept
library(broom)
SummarySecondMMR_HSCPLDmodel_tbl = Summarymodel_secondMMR_HSCP_LD%>% 
  tidy(conf.int = TRUE, exp = TRUE) %>% 
  rename(OR = estimate, upperCI = conf.high, lowerCI = conf.low) %>% 
  filter(str_detect(term,"(Intercept)", negate = TRUE))

#Create labels and enter summary data from above tbls ####NOTe removing NHS Grampian areas as above
boxLabelsHSCPLDsecondMMR = c("Angus", "Argyll&Bute", "C'shire&Stirling", "Dumfries&G'way", "Dundee city", "East Ayrshire", "East Dunbartonshire", "East Lothian", "East R'shire", "Edinburgh", "Falkirk", "Fife", "Glasgow city", "Highland", "Inverclyde", "North Ayrshire", "North L'shire", "Perth&Kinross", "Renfrewshire", "Scottish Borders", "South Ayrshire", "South L'shire", "West D'shire", "West Lothian") ##not shetland, W isles and orkney and NHS Grampian

SummaryORandCI_secondMMR_HSCPLD <- data.frame(
  yAxis = length(boxLabelsHSCPLDsecondMMR):1,
  boxOdds = c(SummarySecondMMR_HSCPLDmodel_tbl$OR),
  boxCILow = c(SummarySecondMMR_HSCPLDmodel_tbl$upperCI),
  boxCIHigh = c(SummarySecondMMR_HSCPLDmodel_tbl$lowerCI))

SummarySecondMMR_HSCPLD_forest <- ggplot(SummaryORandCI_secondMMR_HSCPLD, aes(x = boxOdds, y = boxLabelsHSCPLDsecondMMR))
SummarySecondMMR_HSCPLD_forest = SummarySecondMMR_HSCPLD_forest + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw()+
  labs(x = "SummaryLD OR compared to Midlothian",
       y = NULL,
       title = NULL)

SummarySecondMMR_HSCPLD_forest

anova(Summarymodel_secondMMR_HSCP_LD, test="LRT")
#Export SecondMMR OR plots

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

Second_MMR_OR_plots = ggarrange(SecondMMR_HSCP2019_forest, SummarySecondMMR_HSCPLD_forest,
                               labels = c("SecondMMR 2019","SecondMMR LD" ),hjust = -1,
                               legend = NULL,
                               ncol = 2, nrow = 1)
Second_MMR_OR_plots

###Secion 1 Figure 2 Make 2019 maps for all 
#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_2019_map

#Second 6in1 2019
HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 

newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Second6in1, by.x = "HIAName", by.y = "area_name")

Second_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_16weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_2019_map

#Third 6in1 2019
HSCP_2019data_Third6in1 <- Full_thirddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_Third6in1, by.x = "HIAName", by.y = "area_name")

Third_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_20weeks_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_2019_map

#First MMR 2019
HSCP_2019data_FirstMMR <- Full_firstdose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_FirstMMR, by.x = "HIAName", by.y = "area_name")

First_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_13m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "First MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_2019_map

#Second MMR 2019
HSCP_2019data_SecondMMR <- Full_seconddose_MMR %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

library(tmap) 
newPoly <- merge(x=HSCP_map, y=HSCP_2019data_SecondMMR, by.x = "HIAName", by.y = "area_name")

Second_MMR_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_3y5m_percent", palette = phs_spec2(7), title = "% immunised within 4 wks")+
  tm_layout(frame = FALSE, main.title = "Second MMR 2019",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_2019_map

##Export maps 2019 and percentage change

tmap_arrange(First_6in1_2019_map, First_6in1_percentchange_map, Second_6in1_2019_map, Second_6in1_percentchange_map, Third_6in1_2019_map, Third_6in1_percentchange_map,  ncol = 2,
             nrow = 3) 
tmap_arrange(First_MMR_2019_map, First_MMR_percentchange_map, Second_MMR_2019_map, Second_MMR_percentchange_map,  ncol = 2,
             nrow = 2)

###########Make the table for supplementary info
####Making tablefor Section1 table 1
##First 6in1

Section2suptbl2_First6in1 = Full_firstdose_6in1 %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  filter(!(cohort %in% c("Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))%>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  mutate (lockdown.factor = cohort %>% 
  factor() %>% 
  fct_recode("Baseline_2019"="2019", "Pre_LD_2020"="Jan-20", "Pre_LD_2020"="Feb-20", "Pre_LD_2020"="W/B 02-MAR-20","Pre_LD_2020"="W/B 09-MAR-20", "Pre_LD_2020"="W/B 16-MAR-20", "LD_2020"="W/B 23-MAR-20","LD_2020"="W/B 30-MAR-20","LD_2020"="W/B 06-APR-20","LD_2020"="W/B 13-APR-20","LD_2020"="W/B 20-APR-20","LD_2020"="W/B 27-APR-20","LD_2020"="W/B 04-MAY-20","LD_2020"="W/B 11-MAY-20","LD_2020"="W/B 18-MAY-20","LD_2020"="W/B 25-MAY-20","LD_2020"="W/B 01-JUN-20","LD_2020"="W/B 08-JUN-20","LD_2020"="W/B 15-JUN-20","LD_2020"="W/B 22-JUN-20","LD_2020"="W/B 29-JUN-20","LD_2020"="W/B 06-JUL-20","LD_2020"="W/B 13-JUL-20","LD_2020"="W/B 20-JUL-20","LD_2020"="W/B 27-JUL-20", "Post_LD_2020"="W/B 03-AUG-20","Post_LD_2020"="W/B 10-AUG-20","Post_LD_2020"="W/B 17-AUG-20","Post_LD_2020"="W/B 24-AUG-20","Post_LD_2020"="W/B 31-AUG-20","Post_LD_2020"="W/B 07-SEP-20","Post_LD_2020"="W/B 14-SEP-20","Post_LD_2020"="W/B 21-SEP-20","Post_LD_2020"="W/B 28-SEP-20"))

#2019
Section2suptbl2_First6in1_2019 = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Baseline_2019") %>% 
  mutate(time_period = "2019") %>% 
  select(area_name, denominator, uptake_12weeks_num, uptake_12weeks_percent, time_period)
#Take out the 2019 percentage to get % change for table
First6in1_2019areapercentage = Section2suptbl2_First6in1_2019 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_2019areapercentage) = c("area_name", "percentuptake2019")

#preLD period
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Pre_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PreLD")
#join preLD and 2019percentage tbls
Section2suptbl2_First6in1_PreLD = full_join(Section2suptbl2_First6in1_PreLD, First6in1_2019areapercentage)
#add column for difference btw time period and 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
#Remove 2019
Section2suptbl2_First6in1_PreLD = Section2suptbl2_First6in1_PreLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#LD period
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#Add in Island data
First6in1_Islands = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "Mar-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20"))) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "LD")
#and join
Section2suptbl2_First6in1_LD = rbind(Section2suptbl2_First6in1_LD, First6in1_Islands)

#Back to obtaining change from 2019
Section2suptbl2_First6in1_LD = full_join(Section2suptbl2_First6in1_LD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_LD = Section2suptbl2_First6in1_LD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)

#Post LD
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1 %>% 
  filter(lockdown.factor == "Post_LD_2020") %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")

#Add in Island data
First6in1_Islands_PostLD = Full_firstdose_6in1 %>% 
  filter(area_name %in% c("Orkney Islands", "Shetland Islands", "Western Isles")) %>% 
  filter(cohort== c("Aug-20", "Sep-20")) %>% 
  select(area_name, cohort, denominator, uptake_12weeks_num, uptake_12weeks_percent) %>% 
  group_by(area_name) %>% 
  summarise(denominator = sum(denominator), uptake_12weeks_num = sum(uptake_12weeks_num), uptake_12weeks_percent = mean(uptake_12weeks_percent)) %>% 
  mutate(time_period = "PostLD")
#and join
Section2suptbl2_First6in1_PostLD = rbind(Section2suptbl2_First6in1_PostLD, First6in1_Islands_PostLD)
#Get change cf 2019
Section2suptbl2_First6in1_PostLD = full_join(Section2suptbl2_First6in1_PostLD, First6in1_2019areapercentage)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  mutate(changecf2019 = uptake_12weeks_percent-percentuptake2019)
Section2suptbl2_First6in1_PostLD = Section2suptbl2_First6in1_PostLD %>% 
  select(area_name, denominator, uptake_12weeks_percent, uptake_12weeks_num, time_period, changecf2019)


#Join together
FullSection2suptbl2_First6in1 = full_join(Section2suptbl2_First6in1_2019, Section2suptbl2_First6in1_PreLD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_LD)
FullSection2suptbl2_First6in1 = full_join(FullSection2suptbl2_First6in1, Section2suptbl2_First6in1_PostLD)
FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1%>% 
  mutate (uptake_12weeks_percent= round (uptake_12weeks_percent, digits = 1)) %>% 
  mutate (changecf2019= round (changecf2019,digits = 1))

FullSection2suptbl2_First6in1 = FullSection2suptbl2_First6in1[,c(1,5,4,3,2,6)]

write_csv(FullSection2suptbl2_First6in1, file = "Exported tables/FullSection2suptbl2_First6in1.csv")
