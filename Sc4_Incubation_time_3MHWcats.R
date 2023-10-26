# 11 Aug 2023
# R v 4.2.2
#--------------------------------


# Determines incubation time based on cumulative degree days and 
#  determines how much of the difference in hatch dates could be due to different incubation temperatures among periods



### Paired with

#Data on all fish captured with ages from otolith interpretation or estimated from age-length key
# "D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv"

#Interpolated daily temperatures from deep areas where Pcod hatch
# "D3_interpolatedGAK100_250m_Feb2023_JM.csv"






#### Packages needed -------------------------------------------------------------------------------

library(tidyverse)
library(rstudioapi)
library(ggplot2)



### Inputting data ---------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in





### Info needed---------------

# Narimatsu et al. (2007) found an incubation time of 13 d at 8-9 deg. C

Total.incu.DD = 8.5 * 13

# this also matches with Ben's experiment which had temps at 4-5 deg. C and hatch time of 22 d



### Input data needed ---------------------------------------------------------------------------------------------------------------------------------


all.fish.ages.applied <- read.csv("D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv", header = TRUE) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2018-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, format = "%m-%d"))

GAKDeep <- read.csv("D3_interpolatedGAK100_250m_Feb2023_JM.csv", header = TRUE) %>%
  mutate(Date = as.Date(Dates, format = "%m/%d/%Y")) %>%
  select(-Dates, -X) %>%
  mutate(mo = as.numeric(format(Date, format = "%m"))) %>%
  filter(mo < 8) %>%
  select(-mo) %>%
  rename(DeepTemp = SplineFit) %>%
  mutate(doy = format(Date, format = "%j")) %>%
  mutate(day = as.numeric(doy)) %>%
  mutate(Year = format(Date, format = "%Y"))

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Wrangling data, calculating cumulative degree days, determining incubation duration and mean incubation temperature


hdates <- all.fish.ages.applied %>%
  select(Year, ID, hdate, hatch_DATE, hatch_DATE_noYear) %>%
  unique() %>%
  mutate(IDs = as.character(ID))


max.hdates <- hdates %>%
  group_by(Year) %>%
  summarize(maxhdate = max(hdate)) %>%
  mutate(Years = as.character(Year))



doy = rep(c(127:1), 494) #need list of doy in reverse associated with each of the 494 IDs - started with max hdate and will filter later

IDs = rep(c(hdates$ID), 127) #need list of IDs replicated for each doy
IDs[] = IDs[(order(IDs))] #need to order IDs to make sure the 127:1 list is for each ID


rev.doy <- as.data.frame(cbind(IDs, doy))

rev.doy2 <- merge(rev.doy, hdates) %>%
  mutate(doy.num = as.numeric(doy)) %>%
  mutate(Year = as.character(Year)) %>%
  group_by(IDs) %>%
  filter(doy.num <= hdate) %>%
  select(IDs, Year, doy.num) %>%
  mutate(day = doy.num)



rev.doy.w.temp <- left_join(rev.doy2, GAKDeep, by = c("Year", "day")) %>%
  group_by(IDs) %>%
  mutate(DD = cumsum(DeepTemp)) #Creating a column with cumulative degree days



CDD.incubation.time <- rev.doy.w.temp %>%
  filter(DD <= Total.incu.DD) 

Total.incu.time <- CDD.incubation.time %>%
  group_by(IDs, Year) %>%
  count(IDs) %>%
  rename(days.to.hatch = n)

Avg.incu.temp <- CDD.incubation.time %>%
  group_by(IDs, Year) %>%
  summarize(Avg.incu.temp = mean(DeepTemp)) %>%
  mutate(Ben.est.days.to.hatch = 46.597 - (4.0179 * Avg.incu.temp))


Incubation.data <- merge(Total.incu.time, Avg.incu.temp)

Incu.hatch.data <- merge(Incubation.data, hdates) %>%
  select(-IDs, -hatch_DATE_noYear) %>%
  mutate(Yearf = as.factor(Year)) %>%
  mutate(hatch_DATE2 = as.Date(hatch_DATE, format = "%Y-%m-%d")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE2, "%b-%d")) %>%
  mutate(HWf = as.factor(if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))) %>%
  mutate(scaled.incu.temp = scale(Avg.incu.temp, center = TRUE))

# write.csv(Incu.hatch.data, file = "04_Incubation_hatch_data.csv", row.names = FALSE)












## Determining how much of the hatch date difference could be attributed to thermal differences in the three periods


Med.incu.hatch.data2 <- Incu.hatch.data %>%
  group_by(Year) %>%
  mutate(Avg.annual.incu.temp = mean(Avg.incu.temp)) %>%
  mutate(med.hdate = median(hdate)) %>%
  mutate(diff.w.med = med.hdate - hdate) %>%
  filter(diff.w.med == min(abs(diff.w.med))) %>%
  select(Year, HWf, hdate, Avg.annual.incu.temp) %>%
  unique() %>%
  group_by(HWf) %>%
  summarize(hdate = mean(hdate), Avg.HW.incu.temp = mean(Avg.annual.incu.temp))


before.hdate <- Med.incu.hatch.data2 %>%
  ungroup() %>%
  filter(HWf == "Before MHW")


during.hdate <- Med.incu.hatch.data2 %>%
  ungroup() %>%
  filter(HWf == "During MHW")


between.hdate <- Med.incu.hatch.data2 %>%
  ungroup() %>%
  filter(HWf == "Between MHW")




before.during.HD.diffs <- 14  #Using estimated diffs from pairwise comparisons from LMM results (see "Sc5_Hatch_date_analyses_plots.R")
before.between.HD.diffs <- 26  #Using estimated diffs from pairwise comparisons from LMM results (see "Sc5_Hatch_date_analyses_plots.R")
during.between.HD.diffs <- 12  #Using estimated diffs from pairwise comparisons from LMM results (see "Sc5_Hatch_date_analyses_plots.R")

before.during.temp.diffs <- abs(before.hdate$Avg.HW.incu.temp - during.hdate$Avg.HW.incu.temp)    # difference in temperatures between the before and during periods
before.during.pred.days.diff <- before.during.temp.diffs * 4.0179    # ~7.5 days could be accounted for in the difference between mean hatch dates between HW periods
before.during.percent.diff.to.temp <- (before.during.pred.days.diff / before.during.HD.diffs) * 100
#52.9 %

before.between.temp.diffs <- abs(before.hdate$Avg.HW.incu.temp - between.hdate$Avg.HW.incu.temp)    # difference in temperatures between the before and between periods
before.between.pred.days.diff <- before.between.temp.diffs * 4.0179    # ~4 days could be accounted for in the difference between mean hatch dates between HW periods
before.between.percent.diff.to.temp <- (before.between.pred.days.diff / before.between.HD.diffs) * 100
#16.1 %

during.between.temp.diffs <- abs(during.hdate$Avg.HW.incu.temp - between.hdate$Avg.HW.incu.temp)    # difference in temperatures between the during and between periods
during.between.pred.days.diff <- during.between.temp.diffs * 4.0179    # ~3 days could be accounted for in the difference between mean hatch dates between HW periods
during.between.percent.diff.to.temp <- (during.between.pred.days.diff / during.between.HD.diffs) * 100
#26.8 %







