rm(list = ls())

library(tidyverse)
library(lubridate)
library(weathermetrics) 

setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\outcome\\")

temp = list.files(pattern="\\.csv$")
df <- data.frame()

for (i in temp) {
  df <- rbind(df, read.csv(i))
}

names(df) <- c("row", "response_date", "length_sec", "postal", "type", "amb_num", "fire_num", "tot_num")

df <- df %>%
  select(-c("row")) %>%
  mutate(date=strptime(response_date, format = "%m/%d/%y %I:%M %p")) %>%
  mutate(yr=year(date),
         mo=month(date)) %>%
  mutate(day_mo=mday(date),
         date_only=make_date(year=yr, month=mo, day=day_mo),
         date=as.Date(date))

# saveRDS(df, "df.rds")

df_freeze <- readRDS("df.rds") %>%
  filter(fire_num!="NULL") %>%
  group_by(date) %>%
  summarise(fire=sum(as.numeric(fire_num)), amb=sum(as.numeric(amb_num)), tot=sum(as.numeric(tot_num)), call=n()) %>%
  ungroup() %>%
  mutate(date_num = as.numeric(date)) %>%
  mutate(yr=year(date),
         mo=month(date)) %>%
  filter(mo %in% c(1:3)) %>%
  mutate(doy=yday(date),
         harm_d=(2*pi*doy)/365,
         tot_here=fire + amb)

# saveRDS(df_freeze, "C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\tx_freeze\\df_freeze.rds")

df_freeze <- readRDS("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\tx_freeze\\df_freeze.rds")

## sig cold snaps dec 1983
## great tx freeze feb 11-20,2021
## sig cold snap feb 2011 Groundhog Day blizzard only in north tx feb 1-2, 2011


setwd("C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\temp\\")

# PRISM exposure
prism_tmax <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_tmax.csv")
prism_tmin <- read.csv("PopWeightPRISM_Dallas\\PopWeightPRISM_Dallas_tmin.csv") %>% select(tmin)

#vapd: vapor pressure deficit

prism <- prism_tmax %>%
  mutate(date=seq(as.Date('2010-12-01'),as.Date('2023-12-31'), 'day')) %>%
  select(date, tmax) %>%
  cbind(prism_tmin)

df_all <- prism %>%
  left_join(df_freeze) %>%
  drop_na(fire)

# saveRDS(df_all, "C:\\Users\\veron\\OneDrive - Harvard University\\smartsurfaces\\Dallas_911calls\\tx_freeze\\df_all_freeze.rds")

