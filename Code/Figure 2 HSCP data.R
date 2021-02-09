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
Full_seconddose_MMR = read.csv(here("Data", "Second_dose_MMR_3_feb_21.csv"))  ####excludes Aberdeen city, Aberdeenshire and Moray as given second MMR at different time, hese were removed pre enering into dashboard

#Load HSCP map from https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search;jsessionid=714BD98E15D22A8116824CA25B30DC02#/metadata/ac5e870f-8fc2-4c21-8b9c-3bd2311a583f
HSCP_map <- readOGR(here("Data"), layer="SG_NHS_IntegrationAuthority_2019", verbose=FALSE)

######ggplot method dosen't seem to work # 'fortify' the data to get a dataframe format required by ggplot2
#library(broom)
#HSCP_fortified <- tidy(HSCP_map, region = "HIAName") 

# Plot it
#library(ggplot2)
#ggplot() +
  #geom_polygon(data = HSCP_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  #theme_void() 

##Select out percentage uptake for 2019 for HSPC. Remove NHS boards and Scotland

#First 6in1 2019
HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

######ggplot method dosen't seem to work HSCP_2019map_First6in1 = HSCP_fortified %>% 
  #left_join(., HSCP_2019data_First6in1, by=c("id"="area_name")) %>% 
  #select(long,lat, group, id, uptake_12weeks_percent)

#First6in1_2019_map = ggplot() +
 # geom_polygon(data = HSCP_2019map_First6in1, aes(fill = uptake_12weeks_percent, x = long, y = lat, group = id)) +
  #theme_void() +
  #coord_map()
#First6in1_2019_map  

library(tmap) ############this works


newPoly <- merge(x=HSCP_map, y=HSCP_2019data_First6in1, by.x = "HIAName", by.y = "area_name")

First_6in1_2019_map = tm_shape(newPoly)+
  tm_fill(col="uptake_12weeks_percent", palette = phs_spec2(7), title = "% vacinnated")+
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

HSCP_2019data_First6in1 <- Full_firstdose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

First6in1_HSCP_2019_percent = HSCP_2019data_First6in1 %>% 
  select(area_name, uptake_12weeks_percent)
colnames(First6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")

First6in1_HSCP_LD_percent = Full_firstdose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))
  
First6in1_HSCP_LD_percent = First6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_12weeks_percent))
colnames(First6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

First6in1_HSPC_percentchange_2019andLD = full_join(First6in1_HSCP_2019_percent, First6in1_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city and Scottish borders

First6in1_HSPC_percentchange_2019andLD = First6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

#Plot percent change 

library(tmap)
library(viridis)
First6in1_percentchange_poly <- merge(x=HSCP_map, y=First6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_6in1_percentchange_map = tm_shape(First6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First 6in1 Percent change",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_6in1_percentchange_map

#Second 6in1

HSCP_2019data_Second6in1 <- Full_seconddose_6in1 %>%
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE)) %>% 
  filter(cohort == 2019)

Second6in1_HSCP_2019_percent = HSCP_2019data_Second6in1 %>% 
  select(area_name, uptake_16weeks_percent)
colnames(Second6in1_HSCP_2019_percent) = c("area_name", "Uptake_2019")

Second6in1_HSCP_LD_percent = Full_seconddose_6in1 %>% ###Note this selects out LD data for Shetland, Orkney and Western Isles, need to fix
  filter(str_detect(area_name,"NHS", negate=TRUE)) %>% 
  filter(str_detect(area_name,"Scotland", negate = TRUE))%>% 
  filter(!(cohort %in% c("2019", "Jan-20", "Feb-20", "W/B 02-MAR-20", "W/B 09-MAR-20", "W/B 16-MAR-20", "Mar-20","Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "W/B 03-AUG-20", "W/B 10-AUG-20","W/B 17-AUG-20", "W/B 24-AUG-20", "W/B 24-AUG-20", "W/B 31-AUG-20", 	
                         "W/B 07-SEP-20", "W/B 14-SEP-20", "W/B 21-SEP-20","W/B 28-SEP-20", "W/B 05-OCT-20", "W/B 12-OCT-20", "W/B 19-OCT-20", "W/B 26-OCT-20", "W/B 02-NOV-20", "W/B 09-NOV-20", "W/B 16-NOV-20", "W/B 23-NOV-20", "W/B 30-NOV-20", "W/B 07-DEC-20")))

Second6in1_HSCP_LD_percent = Second6in1_HSCP_LD_percent%>%
  group_by(area_name) %>% 
  summarise(mean_percentuptake_LD = mean(uptake_16weeks_percent))
colnames(Second6in1_HSCP_LD_percent) = c("area_name", "Uptake_LD")

Second6in1_HSPC_percentchange_2019andLD = full_join(Second6in1_HSCP_2019_percent, Second6in1_HSCP_LD_percent)  

Second6in1_HSPC_percentchange_2019andLD = Second6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Second6in1_percentchange_poly <- merge(x=HSCP_map, y=Second6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_6in1_percentchange_map = tm_shape(Second6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second 6in1 Percent change",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_6in1_percentchange_map

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

Third6in1_HSPC_percentchange_2019andLD = full_join(Third6in1_HSCP_2019_percent, Third6in1_HSCP_LD_percent)  

Third6in1_HSPC_percentchange_2019andLD = Third6in1_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
Third6in1_percentchange_poly <- merge(x=HSCP_map, y=Third6in1_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Third_6in1_percentchange_map = tm_shape(Third6in1_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "RdYlGn", title = "% change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Third 6in1 Percent change",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Third_6in1_percentchange_map

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

FirstMMR_HSPC_percentchange_2019andLD = full_join(FirstMMR_HSCP_2019_percent, FirstMMR_HSCP_LD_percent)  #has been cross checked with excel for aberdeen city 

FirstMMR_HSPC_percentchange_2019andLD = FirstMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
FirstMMR_percentchange_poly <- merge(x=HSCP_map, y=FirstMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

First_MMR_percentchange_map = tm_shape(FirstMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "First MMR Percent change",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

First_MMR_percentchange_map ##NOte change in pallet as none fell. Cross checked for Moray

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

SecondMMR_HSPC_percentchange_2019andLD = full_join(SecondMMR_HSCP_2019_percent, SecondMMR_HSCP_LD_percent)  

SecondMMR_HSPC_percentchange_2019andLD = SecondMMR_HSPC_percentchange_2019andLD %>% 
  mutate(percent_change = Uptake_LD-Uptake_2019)

library(tmap)
library(viridis)
SecondMMR_percentchange_poly <- merge(x=HSCP_map, y=SecondMMR_HSPC_percentchange_2019andLD, by.x = "HIAName", by.y = "area_name")

Second_MMR_percentchange_map = tm_shape(SecondMMR_percentchange_poly)+
  tm_fill(col= "percent_change", palette = "YlGn", title = "% change LD vs 2019")+
  tm_layout(frame = FALSE, main.title = "Second MMR Percent change",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

Second_MMR_percentchange_map

##Export the % change maps in one pdf
tmap_arrange(First_6in1_percentchange_map, Second_6in1_percentchange_map, Third_6in1_percentchange_map, First_MMR_percentchange_map, Second_MMR_percentchange_map)
