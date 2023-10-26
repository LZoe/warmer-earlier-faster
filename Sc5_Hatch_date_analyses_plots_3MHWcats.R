#08/11/2023
#R v 4.2.2


### Hatch Date analyses

# Cleaned up for submission



### Paired with

#Data on all fish captured with ages from otolith interpretation or estimated from age-length key
# "D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv"





library(rstudioapi)
library(tidyverse)
library(ggpubr)     # needed to plot panels together
library(cowplot)    # needed to plot panels together




setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in


### Input data needed ---------------------------------------------------------------------------------------------------------------------------------

all.fish.ages.applied <- read.csv("D9_03_All_July_fish_ages_applied_lmAnnualLenKey.csv", header = TRUE)



#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Wrangling data






#----------------------------------------------------------------------------------------------------------------------------------------------------------
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
  mutate(HWf = as.factor(HW))



#Summary of mean hatch date for each year
hDATE.summary <- fish.hDATE.11.years %>%
  group_by(Year) %>%
  summarize(meanHD = round(mean(hdate))) %>%
  mutate(mean.hDATE = as.Date(meanHD, origin = "2012-12-31"))





#............................ Plots ................................


x11()
hatchdates_yr <- ggplot(data = fish.hDATE.11.years, aes(x = as.Date(hatch_DATE_noYear, format = "%b-%d"), fill = HWf)) + 
  geom_density(alpha = 0.4, position = "identity") +
  xlab("Hatch date") + 
  ylab("Density") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%d") +
  scale_fill_manual(values = c("black", "steelblue", "lightsteelblue3"), labels = c("Before", "Between", "During"), name = "Years binned by\nheatwave status") +
  theme_classic() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())

hatchdates_yr




year.labels = c("2007" = "'07", "2009" = "'09", "2010" = "'10", "2012" = "'12", "2013" = "'13", "2014" = "'14", "2015" = "'15", "2016" = "'16", "2017" = "'17", "2018" = "'18", "2019" = "'19")


x11()
hatchdates_yr2 <- ggplot(data = fish.hDATE.11.years, aes(as.factor(Year), as.Date(hatch_DATE_noYear, format = "%b-%d"), fill = HW)) + 
  geom_boxplot(size = 0.5, outlier.alpha = 0) +
  geom_point(size = 0.25) +
  xlab("Year") + 
  ylab("Hatch date") +
  scale_x_discrete(name = "Year",
                   labels = year.labels) +
  scale_y_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_manual(values = c("grey35", "steelblue3", "lightsteelblue3"), labels = c("Before", "Between", "During"), name = "Years binned by\nheatwave status") +
  # scale_fill_manual(values = c("lightsteelblue3", "grey35"), labels = c("Since MHW", "Before MHW"), name = "Years binned by\nheatwave status") +
  theme_classic() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())

hatchdates_yr2








x11()
HD1P <- ggarrange(hatchdates_yr2, hatchdates_yr, ncol = 1, nrow = 2, common.legend = TRUE, legend = "top") +
  draw_plot_label(label = c("(A)", "(B)"), size = 12, x = c(0.01, 0.01), y = c(0.99, 0.49))

HD1P


# ggsave("08_HD_phenology.jpg", plot = last_plot(), device = "jpeg",
#        width = 3.25, height = 4, units = "in", dpi = 600)










#............................ Analyses ................................



library(lme4)
HWHD <- lmer(hdate ~ HWf + (1 | Yearf), data = fish.hDATE.11.years)
summary(HWHD)

# Linear mixed model fit by REML ['lmerMod']
# Formula: hdate ~ HWf + (1 | Yearf)
# Data: fish.hDATE.11.years
# 
# REML criterion at convergence: 3765.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.3196 -0.6486  0.0447  0.6294  2.6816 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Yearf    (Intercept) 100.1    10.00   
# Residual             114.9    10.72   
# Number of obs: 494, groups:  Yearf, 11
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)   99.180      4.134  23.990
# HWfbetween   -25.902      8.286  -3.126
# HWfduring    -13.917      7.244  -1.921
# 
# Correlation of Fixed Effects:
#   (Intr) HWfbtw
# HWfbetween -0.499       
# HWfduring  -0.571  0.285




library(car)
Anova(HWHD)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: hdate
#     Chisq Df Pr(>Chisq)   
# HWf 10.929  2   0.004235 **



library(emmeans)
emmeans(HWHD, list(pairwise ~ HWf))
# $`emmeans of HWf`
# HWf     emmean   SE   df lower.CL upper.CL
# before    99.2 4.13 7.85     89.6    108.7
# between   73.3 7.18 7.94     56.7     89.9
# during    85.3 5.95 8.39     71.6     98.9
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 
# 
# $`pairwise differences of HWf`
# 1                estimate   SE   df t.ratio p.value
# before - between     25.9 8.29 7.92   3.126  0.0341
# before - during      13.9 7.25 8.21   1.921  0.1934
# between - during    -12.0 9.33 8.12  -1.285  0.4409
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 

#Day 73 = March 14
#Day 85 = March 26
#Day 99 = April 9


library(FSA) #to get the functionality of looking at histograms across levels of a factor
plot(HWHD)
plot(as.factor(fish.hDATE.11.years$HW), resid(HWHD, type = "pearson"))
qqnorm(resid(HWHD))
qqline(resid(HWHD))
hist(residuals(HWHD), xlab = "Standardized residuals", ylab = "Frequency", main = NULL)
hist(residuals(HWHD, type = "pearson") ~ as.factor(fish.hDATE.11.years$HW), xlab = "Standardized residuals", ylab = "Frequency", main = NULL)















