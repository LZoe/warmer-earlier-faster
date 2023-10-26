#08/11/2023


### SL at capture analyses

# cleaned up for submission


### Paired with

#Data on all fish captured with ages from otolith interpretation or estimated from age-length key
# "D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv"




library(tidyverse)  # data wrangling
library(lme4)       # for linear mixed modeling
library(car)        # for Type III ANOVA
library(FSA)        # to get the functionality of looking at histograms across levels of a factor
library(effects)    # to extract model effects
library(ggpubr)     # to plot panels together
library(cowplot)    # to plot panels together
library(rstudioapi) # for setting working directory




setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in





### Input data needed ---------------------------------------------------------------------------------------------------------------------------------

all.fish.ages.applied <- read.csv("D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv", header = TRUE)



#------------------------------------ Wrangling data ---------------------------------------------
#calculating hatch date and converting from doy to date

#Assigning years to pre HW (2014 & before) and post HW (2015 & after) years



fish.2007 <- all.fish.ages.applied %>%
  filter(Year == 2007) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2006-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2009 <- all.fish.ages.applied %>%
  filter(Year == 2009) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2008-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2010 <- all.fish.ages.applied %>%
  filter(Year == 2010) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2009-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2012 <- all.fish.ages.applied %>%
  filter(Year == 2012) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2011-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2013 <- all.fish.ages.applied %>%
  filter(Year == 2013) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2012-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2014 <- all.fish.ages.applied %>%
  filter(Year == 2014) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2013-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "before")

fish.2015 <- all.fish.ages.applied %>%
  filter(Year == 2015) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2014-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "during")

fish.2016 <- all.fish.ages.applied %>%
  filter(Year == 2016) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2015-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "during")

fish.2017 <- all.fish.ages.applied %>%
  filter(Year == 2017) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2016-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "between")

fish.2018 <- all.fish.ages.applied %>%
  filter(Year == 2018) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2017-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "between")

fish.2019 <- all.fish.ages.applied %>%
  filter(Year == 2019) %>%
  mutate(hdate = cap_doy_corr - Final.Age) %>%
  mutate(hatch_DATE = as.Date(hdate, origin = "2018-12-31")) %>%
  mutate(hatch_DATE_noYear = format(hatch_DATE, "%b-%d")) %>%
  mutate(HW = "during")






fish.hDATE.2.years <- bind_rows(fish.2007, fish.2009)
fish.hDATE.3.years <- bind_rows(fish.hDATE.2.years, fish.2010)
fish.hDATE.4.years <- bind_rows(fish.hDATE.3.years, fish.2012)
fish.hDATE.5.years <- bind_rows(fish.hDATE.4.years, fish.2013)
fish.hDATE.6.years <- bind_rows(fish.hDATE.5.years, fish.2014)
fish.hDATE.7.years <- bind_rows(fish.hDATE.6.years, fish.2015)
fish.hDATE.8.years <- bind_rows(fish.hDATE.7.years, fish.2016)
fish.hDATE.9.years <- bind_rows(fish.hDATE.8.years, fish.2017)
fish.hDATE.10.years <- bind_rows(fish.hDATE.9.years, fish.2018)
fish.hDATE.11.years <- bind_rows(fish.hDATE.10.years, fish.2019) %>%
  mutate(Yearf = as.factor(Year)) %>%
  mutate(HWn = as.factor(if_else(Year < 2015, "before", if_else(Year %in% c(2017, 2018), "between", "during"))))






year.labels = c("2007" = "'07", "2009" = "'09", "2010" = "'10", "2012" = "'12", "2013" = "'13", "2014" = "'14", "2015" = "'15", "2016" = "'16", "2017" = "'17", "2018" = "'18", "2019" = "'19")



x11()
SL_yr <- ggplot(data = fish.hDATE.11.years, aes(Yearf, SL_mm, fill = HWn)) + 
  geom_boxplot(size = 0.5, outlier.alpha = 0) +
  geom_point(size = 0.25) +
  xlab("Year") + 
  ylab("SL at capture (mm)") +
  scale_x_discrete(name = "Year",
                   labels = year.labels) +
  scale_fill_manual(values = c("grey35", "steelblue3", "lightsteelblue3"), labels = c("Before", "Between", "During"), name = "Years binned by\nheatwave status") +
  theme_classic() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())

SL_yr











###------------------------------------ Analysis ------------------------------------

data.for.analysis <- fish.hDATE.11.years %>%
  filter(hdate > 40) %>%   # Removed hdates < 40 because there's such few data out there and very little between those dates and the next making it hard to trust the model's interpolation
  select(Year, ID, SL_mm, HW, HWn, Final.Age, hdate) %>%
  mutate(scaledSL = scale(SL_mm, center = TRUE)) %>%
  mutate(scaledAge = scale(Final.Age, center = TRUE)) %>%
  mutate(scaledhdate = scale(hdate)) %>%
  mutate(Yearf = as.factor(Year)) %>%
  mutate(HWf = as.factor(HW))
 






analysislmer <- lmer(SL_mm ~ HWf * scaledAge + (1|Yearf), data = data.for.analysis)
analysislmer


summary(analysislmer)



Anova(analysislmer, type = 3)





### Model diagnostics

plot(analysislmer) #standardized residuals used
plot(data.for.analysis$Yearf, resid(analysislmer, type = "pearson"))
plot(data.for.analysis$scaledAge, resid(analysislmer, type = "pearson"))
plot(data.for.analysis$HWf, resid(analysislmer, type = "pearson"))
qqnorm(resid(analysislmer, type = "pearson"))
qqline(resid(analysislmer, type = "pearson"))
hist(residuals(analysislmer, type = "pearson"), xlab = "Standardized residuals", ylab = "Frequency", main = NULL)
hist(residuals(analysislmer, type = "pearson") ~ data.for.analysis$Yearf, xlab = "Standardized residuals", ylab = "Frequency", main = NULL)
hist(residuals(analysislmer, type = "pearson") ~ data.for.analysis$HWf, xlab = "Standardized residuals", ylab = "Frequency", main = NULL)




###------------------------------------ Plotting effects ------------------------------------

AgeLim <- data.for.analysis %>%
  group_by(HWf) %>%
  summarise(minage = min(Final.Age), maxage = max(Final.Age))



SL.effects <- effect(term = "HWf:scaledAge", mod = analysislmer, xlevels = 200)
summary(SL.effects)
x_SL.effects <- as.data.frame(SL.effects) %>%
  mutate(Age = (scaledAge * sd(data.for.analysis$Final.Age)) + mean(data.for.analysis$Final.Age)) %>%
  filter(case_when(HWf == "during" ~ Age > 78 & Age < 139,
                   HWf == "between" ~ Age > 88,
                   T ~ Age < 133))







x11()
SL.effects.plot <- ggplot() +
  geom_point(data = data.for.analysis, aes(Final.Age, SL_mm, color = (HWf)), size = 0.25) +
  geom_line(data = x_SL.effects, aes(x = Age, y = fit, color = (HWf)), size = 0.75) +
  geom_ribbon(data = x_SL.effects, aes(x = Age, ymin = lower, ymax = upper, fill = HWf), alpha = 0.25) +
  scale_color_manual(values = c("black", "steelblue", "lightsteelblue3"), labels = c("Before", "Between", "During"), name = "Years binned by\nheatwave status") +
  scale_fill_manual(values = c("grey35", "steelblue3", "lightsteelblue3"), labels = c("Before", "Between", "During"), name = "Years binned by\nheatwave status") +
  labs(x = "Age (days)", y = "SL at capture (mm)") +
  theme_classic() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())
SL.effects.plot


















x11()
SL1P <- ggarrange(SL_yr, SL.effects.plot, ncol = 1, nrow = 2, common.legend = TRUE, legend = "top") +
  draw_plot_label(label = c("(A)", "(B)"), size = 12, x = c(0.01, 0.01), y = c(0.93, 0.49))

SL1P


# ggsave("08_SL.jpg", plot = last_plot(), device = "jpeg",
#        width = 3.25, height = 4, units = "in", dpi = 600)




















