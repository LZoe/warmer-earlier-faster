#08/28/2023
# R v. 4.2.2



# Back-calculating size/growth using Linear Biological Intercept back-calculation model (Campana 1990)


### Paired with

#Daily increment widths in "long" format
# "D6_JulyDailies_final.csv"




### Packages needed ------------------------------------

library(tidyverse)    # data wrangling
library(ggplot2)      # for plotting
library(rstudioapi)   # for setting working directory



setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in




### Biological intercept ###

## Equation from Campana 1990:


# L(agex) = L(capture) + ((O(age) - O(capture))(L(capture) - L(age0))) / (O(capture) - O(age0))


BI.fun <- function(Lc, Ri, Rc, L0p, R0p) {Lc + (Ri - Rc) * (Lc - L0p) / (Rc - R0p)}



## Biological intercepts from Narimatsu et al. 2007

#Fish TL at hatching = 4.1 mm - converted to SL below (see side-bar in "05_back-calc_prep.R" for SL-TL conversion for larvae)
#Otolith nucleus check = 8.3 um


L0p.TL <- 4.1
R0p <- 8.3

L0p <- 0.952*L0p.TL







#----------------------------------------- Back-calculating size-at-age ---------------------------------------



all.dailies <- read.csv("D6_JulyDailies_final.csv", header = TRUE) #%>%



#calculating a radius at age with a cumulative sum of increment widths
data.for.bc <- all.dailies %>%
  select(Year, ID, sl, radius, Increment, IW, Final.Age) %>%
  group_by(ID) %>%
  mutate(cum_IW = cumsum(IW)) %>%
  mutate(rad_at_age = cum_IW + R0p)






data.w.Length.bc <- data.for.bc

#Applying the linear biological intercept back-calculation model 
data.w.Length.bc$bcSL <- with(data.w.Length.bc, BI.fun(sl, rad_at_age, radius, L0p, R0p))





bc.checking2 <- data.w.Length.bc %>%
  group_by(Year, ID, sl, radius, Final.Age) %>%
  summarize(max.bcSL = max(bcSL), max.age = max(Increment))




# Checking to make sure that our calculated final SL is close to the measured SL at capture

x11()
bc.check1 <- ggplot(data = bc.checking2, aes(sl, max.bcSL)) + 
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), color = "darkred", size = 1) +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  xlab("Measured SL at capture") + 
  ylab("Back-calculated SL at capture") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

bc.check1



x11()
bc.check2 <- ggplot(data = bc.checking2, aes(sl, max.bcSL)) + 
  geom_point() +
  facet_wrap(~ Year) +
  geom_abline(aes(intercept = 0, slope = 1), color = "darkred", size = 1) +
  geom_smooth(method = "lm", color = "blue", size = 1) +
  xlab("Measured SL at capture") + 
  ylab("Back-calculated SL at capture") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

bc.check2



x11()
bc.check3 <- ggplot(data = bc.checking2, aes(sl, max.bcSL, color = as.factor(Year))) + 
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1), color = "darkred", size = 1) +
  geom_smooth(method = "lm", size = 1) +
  xlab("Measured SL at capture") + 
  ylab("Back-calculated SL at capture") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

bc.check3













# Checking cumulative back-calculated growth in SL at each age

x11()
length.at.age.bc <- ggplot(data = data.w.Length.bc, aes(Increment, bcSL, group = ID, color = as.factor(Year))) + 
  geom_line(size = 1) +
  xlab("Age") + 
  ylab("Back-calculated SL") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

length.at.age.bc






### ----------- Calculating daily growth in back-calculated SL ------------

growth.by.sl <- data.w.Length.bc %>%
  group_by(ID) %>%
  mutate(daily.g.sl1 = c(0, diff(bcSL))) %>%
  mutate(daily.g.sl2 = ifelse(daily.g.sl1 == 0, bcSL - L0p, daily.g.sl1))





# Checking the daily back-calculated growth in SL at each age

x11()
daily.g.sl.bc1 <- ggplot(data = growth.by.sl, aes(Increment, daily.g.sl2, group = ID, color = as.factor(Year))) + 
  geom_line(size = 1) +
  # facet_wrap(~ Year) +
  xlab("Age") + 
  ylab("Back-calculated daily growth in SL") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

daily.g.sl.bc1





x11()
daily.g.sl.bc2 <- ggplot(subset(growth.by.sl, ID %in% c("770", "994", "2140", "2029"))) + 
  geom_line(aes(Increment, daily.g.sl2, group = ID, color = as.factor(ID)), size = 1) +
  xlab("Age") + 
  ylab("Back-calculated daily growth in SL") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

daily.g.sl.bc2










# Comparing back-calculated growth in SL to IW (should all be straight lines)

x11()
IW.v.sl <- ggplot(data = growth.by.sl, aes(IW, daily.g.sl2, group = ID, color = as.factor(Year))) +
  geom_line(size = 1) + 
  xlab("Increment Width") + 
  ylab("Back-calculated daily growth in SL") +
  theme_classic()  +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.text = element_text(size = 18), legend.title = element_blank())

IW.v.sl














# Exporting data to use in analyses of growth


data.for.export <- full_join(growth.by.sl, all.dailies) %>%
  select(Year, ID, sl, radius, Increment, IW, Final.Age, bcSL, daily.g.sl2, hdate, hatch_DATE, hatch_DATE_noYear, age_DATE, age_DATE_noYear, HW) %>%
  rename(bc_gr = daily.g.sl2)

# write.csv(data.for.export, file = "D1_01_Dailies_w_bc_growth.csv", row.names = FALSE)






