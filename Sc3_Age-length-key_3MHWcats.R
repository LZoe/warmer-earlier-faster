# 08/11/2023
# R v 4.2.2


# Cleaned up for submission


# 3 MHW categories (before, during, between) rather than 2 (before, since)

#Created annual age-length keys with simple linear model
#Removed fish ID 78 (from 2009) and 2136 (from 2014) because their SL were > 10 mm larger than the largest aged fish in those years





### Paired with

#Collection and size information from all Pcod juveniles captured (not just those aged)
# "D7_All_Pcod_collected.csv"

#Data from aged individuals including increments in "wide" format
# "D8_July_dailies_wide_final.csv"




### Packages needed ------------------------------------

library(rstudioapi)    # for setting working directory
library(tidyverse)     # data wrangling
library(FSA)
library(plotrix)
library(ggpubr)     # needed to plot panels together
library(cowplot)    # needed to plot panels together




setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in



### Input data needed ---------------------------------------------------------------------------------------------------------------------------------

all.fish <- read.csv("D7_All_Pcod_collected.csv", header = TRUE) %>%
  filter(Month == 7) %>%
  mutate(SL_mm = round(SL_mm, digits = 1)) %>%
  mutate(ID = as.character(gsub("BL", "", FISHID))) %>%
  mutate(ID = as.factor(ID)) %>%
  mutate(cap_DATE = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d")) %>%
  mutate(cap_DATE_noyr = format(cap_DATE, format = "%m-%d")) %>%
  mutate(cap_doy_corr = as.numeric(strftime(cap_DATE, format = "%j"))) %>%
  filter(ID != "78" & ID != "2136")
#removed fish 78 and 2136 because individuals were much larger than other individuals and the age-length key could not be reliably applied for individuals that far outside of the aged set


all.dailies <- read.csv("D8_July_dailies_wide_final.csv", header = TRUE) %>%
  rename(Final.Age = age, Year = yr, Month = mos, Day = day, ID = id) %>%
  mutate(SL_mm = round(sl, digits = 1)) %>%
  mutate(ID = as.factor(ID)) %>%
  mutate(cap_DATE = as.Date(paste(Year, Month, Day, sep = "-"), "%Y-%m-%d")) %>%
  mutate(cap_DATE_noyr = format(cap_DATE, format = "%m-%d")) %>%
  mutate(cap_doy_corr = as.numeric(strftime(cap_DATE, format = "%j")))

#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Wrangling and exploring what the data look like


aged.fish <- all.dailies %>%
  select(Year, ID, SL_mm, cap_DATE_noyr, cap_doy_corr, Final.Age) %>%
  mutate(Yearf = as.factor(Year))



cor(aged.fish$SL_mm, aged.fish$Final.Age)
#0.83

plot(aged.fish$SL_mm, aged.fish$Final.Age)

x11()
age.length_yr <- ggplot(data = aged.fish, aes(SL_mm, Final.Age)) + 
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ Year) +
  xlab("SL (mm)") + 
  ylab("Age (days)") +
  theme_classic() +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

age.length_yr


x11()
age.length_yr <- ggplot(data = aged.fish, aes(SL_mm, Final.Age, color = as.factor(Year))) + 
  geom_point() +
  geom_smooth(method = lm) +
  xlab("SL (mm)") + 
  ylab("Age (days)") +
  theme_classic() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_blank())

age.length_yr






### LM relationship between age and SL with interaction with Year ###

lm.years <- lm(Final.Age ~ SL_mm * Yearf, data = aged.fish)
summary(lm.years)

#Seeing if there's a significant interaction between SL and year
library(car)
Anova(lm.years, type = 3)

### ---------------------------------------------------------- ###








all.fish.unagedtoo <- all.fish %>%
  select(Year, ID, SL_mm, cap_DATE_noyr, cap_doy_corr) %>%
  mutate(Yearf = as.factor(Year))


all.fish.aged.unaged <- full_join(aged.fish, all.fish.unagedtoo)








aged.fish.annual.sum <- aged.fish %>%
  group_by(Year) %>%
  count()

aged.fish.annual.summaries <- aged.fish %>%
  group_by(Year) %>%
  reframe(median.SL = median(SL_mm), range.SL = range(SL_mm),
            median.age = median(Final.Age), range.age = range(Final.Age))

all.fish.annual.sum <- all.fish.unagedtoo %>%
  group_by(Year) %>%
  count()






unaged.fish <- subset(all.fish.aged.unaged, is.na(Final.Age))







### Predicting ages with lm approach ###

lm.year.ages.applied <- predict(lm.years, unaged.fish, interval = "confidence")

lm.ages.applied.unaged <- merge(unaged.fish, lm.year.ages.applied, by = "row.names") %>%
  select(-Row.names, -Final.Age, -lwr, -upr) %>%
  mutate(Final.Age = round(fit)) %>%
  select(-fit)

lm.ages.all.fish <- rbind(aged.fish, lm.ages.applied.unaged)








# ---------------------------------------- Data summaries -----------------------------------------

lm.all.fish.annual.summaries <- lm.ages.all.fish %>%
  group_by(Year) %>%
  reframe(median.SL = median(SL_mm), min.SL = min(SL_mm), max.SL = max(SL_mm),
            median.age = median(Final.Age), min.age = min(Final.Age), max.age = max(Final.Age))
lm.all.fish.annual.summaries <- lm.ages.all.fish %>%
  group_by(Year) %>%
  reframe(median.SL = median(SL_mm), range.SL = range(SL_mm),
            median.age = median(Final.Age), range.age = range(Final.Age))
lm.all.fish.annual.summaries <- lm.ages.all.fish %>%
  group_by(Year) %>%
  reframe(mean.SL = mean(SL_mm), se.SL = sd(SL_mm)/sqrt(length(SL_mm)),
            mean.age = mean(Final.Age), se.age = sd(Final.Age)/sqrt(length(Final.Age)))


lm.all.fish.sum <- Summarize(SL_mm ~ Final.Age, data = lm.ages.all.fish, digits = 2)




x11()
histStack(SL_mm ~ Final.Age, data = lm.ages.all.fish, breaks = seq(29, 99, 5), xlab = "Standard Length (mm)")


# write.csv(lm.ages.all.fish, "D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv", row.names = FALSE)









# -------------------------------------------------------- Extrapolating to hatch-dates -----------------------------------------------------------

Extrap.HD.lm.allfish <- lm.ages.all.fish %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2020-12-31")) %>%
  mutate(hatch_DATE_noyr = format(hatch_DATE, format = "%b-%d")) %>%
  mutate(HW = if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))



Hatch_date_summary_all <- Extrap.HD.lm.allfish %>%
  group_by(Year) %>%
  reframe(meanage = round(mean(Final.Age)), medianage = round(median(Final.Age)),
            min.age = round(min(Final.Age)), max.age = round(max(Final.Age)),
            meanHD = round(mean(hdate)), medianHD = round(median(hdate)),
            min.HD = round(min(hdate)), max.HD = round(max(hdate))) %>%
  mutate(meanHD_DATE = as.Date(meanHD, format = "%b-%d", origin = "Dec-31"), medianHD_DATE = as.Date(medianHD, format = "%b-%d", origin = "Dec-31"),
         minHD_DATE = as.Date(min.HD, format = "%b-%d", origin = "Dec-31"), maxHD_DATE = as.Date(max.HD, format = "%b-%d", origin = "Dec-31"))




x11()
hatchdates_yr <- ggplot(data = Extrap.HD.lm.allfish, aes(as.Date(hatch_DATE_noyr, format = "%b-%d"))) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~ Year) +
  xlab("Hatch Date") + 
  ylab("Frequency") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

hatchdates_yr






x11()
hatchdates_yr2 <- ggplot(data = Extrap.HD.lm.allfish, aes(as.factor(Year), as.Date(hatch_DATE_noyr, format = "%b-%d"), fill = HW)) + 
  geom_boxplot() +
  geom_point() +
  xlab("Year") + 
  ylab("Hatch Date") +
  scale_y_date(date_breaks = "1 month", date_labels = "%b-%d") +
  theme_classic() +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

hatchdates_yr2






x11()
hatchdates_yr_lm <- ggplot(data = Extrap.HD.lm.allfish, aes(x = as.Date(hatch_DATE_noyr, format = "%b-%d"), group_by = Yearf, fill = HW)) + 
  geom_density(alpha = 0.4, position = "identity") +
  xlab("Hatch date for all fish") + 
  ylab("Density") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") +
  scale_fill_manual(values = c("blue", "yellow", "red"), labels = c("Before MHW", "Between MHW", "During MHW")) +
  theme_classic() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank(),
        legend.position = c(0.25, 0.75))

hatchdates_yr_lm














### Comparing to aged fish only ###

Extrap.HD.agedfish <- aged.fish %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2020-12-31")) %>%
  mutate(hatch_DATE_noyr = format(hatch_DATE, format = "%b-%d")) %>%
  mutate(HW = if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))






#Hatch date data summary
Hatch_date_summary_aged <- Extrap.HD.agedfish %>%
  group_by(Year) %>%
  reframe(meanHD = round(mean(hdate)), medianHD = round(median(hdate)),
            min.HD = round(min(hdate)), max.HD = round(max(hdate))) %>%
  mutate(meanHD_DATE = as.Date(meanHD, format = "%b-%d", origin = "Dec-31"), medianHD_DATE = as.Date(medianHD, format = "%b-%d", origin = "Dec-31"),
         minHD_DATE = as.Date(min.HD, format = "%b-%d", origin = "Dec-31"), maxHD_DATE = as.Date(max.HD, format = "%b-%d", origin = "Dec-31"))




x11()
hatchdates_yr_aged <- ggplot(data = Extrap.HD.agedfish, aes(x = as.Date(hatch_DATE_noyr, format = "%b-%d"), group_by = Yearf, fill = HW)) + 
  geom_density(alpha = 0.4, position = "identity") +
  xlab("Hatch date for aged fish") + 
  ylab("Density") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") +
  scale_fill_manual(values = c("blue", "yellow", "red"), labels = c("Before MHW", "Between MHW", "During MHW")) +
  theme_classic() +
theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
      legend.text = element_text(size = 9), legend.title = element_blank(),
      legend.position = "none")

hatchdates_yr_aged

###





x11()
HDAll <- ggarrange(hatchdates_yr_lm, hatchdates_yr_aged, ncol = 1, nrow = 2, common.legend = TRUE, legend = "top")

HDAll






# ggsave("02_Annual_HD_distributions.jpg", plot = last_plot(), device = "jpeg",
#        width = 6.5, height = 6, units = "in", dpi = 300)








