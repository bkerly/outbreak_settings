library(tidyverse)
library(lubridate)
library(viridis)
library(forcats)

outbreak_data <- read_csv("https://data.chhs.ca.gov/dataset/e3507cc2-5a02-496c-bcff-bdf37b45a0e3/resource/165980a2-ecb7-464a-980e-f432701d77ef/download/covid-19outbreakstotal2021byindustry_04apr23.csv")

outbreaks_by_sector <- outbreak_data %>%
  filter(IndividualSetting != "All Settings",
         IndustrySector != "Total") %>%
  group_by(IndustrySector) %>%
  summarize(Outbreaks = sum(Outbreaks,na.rm=TRUE),
            Cases = sum(Cases,na.rm=TRUE),
            Outbreak_Size = Cases / Outbreaks) 

g <- outbreaks_by_sector%>%
  mutate(IndustrySector = forcats::fct_reorder(IndustrySector,Outbreak_Size)) %>%
  ggplot(aes(x=IndustrySector,y=Outbreak_Size,
             fill = Outbreak_Size)) +
  geom_bar(stat="Identity") + 
  coord_flip() + 
  scale_fill_viridis(guide= "none")+
  xlab("")+
  ylab("Average Cases per Outbreak")+
  labs(title = "Outbreak Size, 2021-2023")

ggsave(g, path = "Outbreak_Size_by_Sector.png")


# Transpo and Warehouse Only ----------------------------------------------

tw_outbreaks <- outbreak_data %>%
  filter(IndividualSetting != "All Settings",
         IndustrySector != "Total") %>%
  filter(IndustrySector == "Transportation and Warehousing") %>%
  group_by(IndividualSetting) %>%
  summarize(Outbreaks = sum(Outbreaks,na.rm=TRUE),
            Cases = sum(Cases,na.rm=TRUE),
            Outbreak_Size = Cases / Outbreaks) %>%
  mutate(IndividualSetting = as.character(IndividualSetting))

g <- tw_outbreaks%>%
  mutate(IndividualSetting = forcats::fct_reorder(IndividualSetting,Outbreak_Size)) %>%
  ggplot(aes(x=IndividualSetting,y=Outbreak_Size,
             fill = Outbreak_Size)) +
  geom_bar(stat="Identity") + 
  coord_flip() + 
  scale_fill_viridis(guide= "none")+
  xlab("")+
  ylab("Average Cases per Outbreak")+
  labs(title = "Transportation and Warehousing Outbreaks, 2021-2023")

ggsave(g, path = "TW_Outbreak_Size_by_Sector.png")

# Transpo and Warehouse Only ----------------------------------------------

biggest_outbreaks <- outbreak_data %>%
  filter(IndividualSetting != "All Settings",
         IndustrySector != "Total") %>%
  group_by(IndividualSetting) %>%
  summarize(Outbreaks = sum(Outbreaks,na.rm=TRUE),
            Cases = sum(Cases,na.rm=TRUE),
            Outbreak_Size = Cases / Outbreaks) %>%
  ungroup() %>%
  mutate(IndividualSetting = as.character(IndividualSetting)) %>%
  slice_max(order_by = Outbreak_Size,n=20)

g <- biggest_outbreaks%>%
  mutate(IndividualSetting = forcats::fct_reorder(IndividualSetting,Outbreak_Size)) %>%
  ggplot(aes(x=IndividualSetting,y=Outbreak_Size,
             fill = Outbreak_Size)) +
  geom_bar(stat="Identity") + 
  coord_flip() + 
  scale_fill_viridis(guide= "none")+
  xlab("")+
  ylab("Average Cases per Outbreak")+
  labs(title = "Average Outbreak Size, 2021-2023",
       subtitle = "Top 20 Individual Settings")

ggsave(g, path = "TW_Outbreak_Size_by_Sector.png")