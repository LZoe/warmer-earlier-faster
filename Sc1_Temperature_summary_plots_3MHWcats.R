# 08/11/2023
# v 4.2.2
### 



#Summaries of interpolated temperatures from 2006 on



### Paired with

#Daily back-calculated growth and size (informs fish ages for each year)
# "D1_01_Dailies_w_bc_growth.csv"

#Interpolated daily temperatures for surface values and deep values
# "D2_interpolatedGAK0_30m_Feb2023_JM.csv"
# "D3_interpolatedGAK100_250m_Feb2023_JM.csv"

#Information on incubation period
# "D4_03_Incubation_hatch_data.csv"

#Interpolated Trident temperatures for daily 10m temperatures near the nurseries on Kodiak
# "D5_FORANALYSIS_TridentMeans_InterpolatedData.csv




### Packages needed ------------------------------------

library(rstudioapi) # needed for setting working directory
library(tidyverse)  # needed for data wrangling
library(ggplot2)    # needed for plotting
library(ggpubr)     # needed to plot panels together
library(cowplot)    # needed to plot panels together



### Setting working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets WD to folder this is saved in


### Input data needed ---------------------------------------------------------------------------------------------------------------------------------

# Need 'dailies' data because it informs fish ages for each year
dailies <- read.csv("D1_01_Dailies_w_bc_growth.csv", header = TRUE) %>%
  mutate(age_doy = hdate + Increment)%>%
  mutate(Date = mdy(age_DATE)) %>%
  mutate(age_mo = format(Date, format = "%m"))
# Interpolated surface (0-30m) temperatures
GAKSurf <- read.csv("D2_interpolatedGAK0_30m_Feb2023_JM.csv", header = TRUE) %>%
  mutate(Date = mdy(Dates)) %>%
  select(-Dates, -X) %>%
  mutate(mo = as.numeric(format(Date, format = "%m"))) %>%
  filter(mo < 8) %>%
  select(-mo) %>%
  rename(SurfTemp = SplineFit)
# Interpolated bottom (100-250m) temperatures
GAKDeep <- read.csv("D3_interpolatedGAK100_250m_Feb2023_JM.csv", header = TRUE) %>%
  mutate(Date = mdy(Dates)) %>%
  select(-Dates, -X) %>%
  mutate(mo = as.numeric(format(Date, format = "%m"))) %>%
  filter(mo < 8) %>%
  select(-mo) %>%
  rename(DeepTemp = SplineFit)

#
Incu.hatch.data <- read.csv("D4_03_Incubation_hatch_data.csv", header = TRUE)

# Interpolated surface data from Trident
Trident.temps <- read.csv("D5_FORANALYSIS_TridentMeans_InterpolatedData.csv", header = TRUE) %>%
  mutate(Date = mdy(Date))


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#Wrangling data

data_w_Surf <- left_join(dailies, GAKSurf, by = "Date")


data_w_allTemp <- left_join(data_w_Surf, GAKDeep, by = "Date") %>%
  mutate(TempApplied = if_else(Increment < 6, DeepTemp, SurfTemp)) %>% # Temp experienced is bottom temp when 5 dph or younger, then they pretty much experience surface temps (see email exchange and calculations using Li et al. 2015)
  filter(Increment < 100) %>%  # n < 5 in each year after ages above 99 - limit to increments 99 days or younger
  filter(hdate > 40) # Removed the 2 earliest hatchers since they're so few and there's nothing in between them and the next hatch date much later



Bottom.duration.range <- data_w_allTemp %>%
  group_by(Year) %>%
  summarize(earliest = min(hdate),
            latest = max(hdate)) %>%
  mutate(day5 = latest + 5)

Just_temp <- data_w_allTemp %>%
  select(Year, age_DATE_noYear, age_doy, SurfTemp, DeepTemp) %>%
  unique()

Bottom_temp <- left_join(Just_temp, Bottom.duration.range, by = "Year") %>%
  filter(age_doy > (earliest - 1), age_doy < (day5 + 1))

Surf_temp <- left_join(Just_temp, Bottom.duration.range, by = "Year") %>%
  filter(age_doy > (earliest + 5))



  



data_w_allTemp_forHD <- left_join(data_w_Surf, GAKDeep, by = "Date") %>%
  mutate(TempApplied = if_else(Increment < 6, DeepTemp, SurfTemp)) %>% # Temp experienced is bottom temp when 5 dph or younger, then they pretty much experience surface temps (see email exchange and calculations using Li et al. 2015)
  filter(Increment < 100) #%>%  # n < 5 in each year after ages above 99 - limit to increments 99 days or younger



HD.range <- data_w_allTemp_forHD %>%
  group_by(Year) %>%
  summarize(earliest = min(hdate),
            latest = max(hdate)) %>%
  mutate(month.before = earliest - 28)



early_temp <- GAKDeep %>%
  mutate(Date.as.date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Year = as.numeric(format(Date.as.date, format = "%Y"))) %>%
  mutate(DATE_noYear = format(Date.as.date, format = "%b-%d")) %>%
  filter(Year != 2006, Year != 2008, Year != 2011) %>%
  mutate(doy = as.numeric(strftime(Date.as.date, format = "%j")))


HD_temp <- left_join(early_temp, HD.range, by = "Year") %>%
  filter(doy > (month.before - 1), doy < (latest + 1))

temp_during_hatching <- HD_temp %>%
  filter(doy > (earliest - 1), doy < (latest + 1))






temp_data_compare <- left_join(data_w_allTemp_forHD, Trident.temps, by = c("Date", "Year")) %>%
  select(Date, Year, age_DATE_noYear, SurfTemp, DeepTemp, TempApplied, TridentTemp) %>%
  unique()
  



#----------------------------------------------------------------------------------------------------------------------------------------------------------
# Looking at averages


# Temperatures
Avg_surf_temp <- Surf_temp %>%
  group_by(Year) %>%
  summarize(avg_surf_temp = mean(SurfTemp),
            sd_surf_temp = sd(SurfTemp)) %>%
  mutate(min_surf_temp = avg_surf_temp - sd_surf_temp) %>%
  mutate(max_surf_temp = avg_surf_temp + sd_surf_temp) %>%
  mutate(HW = if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))


Avg_bottom_temp <- Bottom_temp %>%
  group_by(Year) %>%
  summarize(avg_bottom_temp = mean(DeepTemp),
            sd_bottom_temp = sd(DeepTemp)) %>%
  mutate(min_bottom_temp = avg_bottom_temp - sd_bottom_temp) %>%
  mutate(max_bottom_temp = avg_bottom_temp + sd_bottom_temp) %>%
  mutate(HW = if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))


Avg_incu_temp <- Incu.hatch.data %>%
  group_by(Year) %>%
  summarize(avg_incu_temp = mean(Avg.incu.temp),
            sd_incu_temp = sd(Avg.incu.temp)) %>%
  mutate(min_incu_temp = avg_incu_temp - sd_incu_temp) %>%
  mutate(max_incu_temp = avg_incu_temp + sd_incu_temp) %>%
  mutate(HW = if_else(Year < 2015, "Before MHW", if_else(Year == 2017 | Year == 2018, "Between MHW", "During MHW")))




x11()
surfP <- ggplot(Avg_surf_temp,  aes(x = Year, y = avg_surf_temp, color = HW)) +
  geom_point(aes(), size = 1) +
  geom_errorbar(aes(x = Year, ymax = (max_surf_temp), ymin = (min_surf_temp), color = HW), width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("black", "steelblue", "lightsteelblue3"), labels = c("Before MHW", "Between MHW", "During MHW")) +
  labs(x = "Year", y = "Surface temperature (\u00B0C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())
surfP



x11()
bottomP <- ggplot(Avg_bottom_temp,  aes(x = Year, y = avg_bottom_temp, color = HW)) +
  geom_point(aes(), size = 1) +
  geom_errorbar(aes(x = Year, ymax = (max_bottom_temp), ymin = (min_bottom_temp), color = HW), width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("black", "steelblue", "lightsteelblue3"), labels = c("Before MHW", "Between MHW", "During MHW")) +
  labs(x = "Year", y = "Bottom temperature (\u00B0C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())
bottomP



x11()
incuP <- ggplot(Avg_incu_temp,  aes(x = Year, y = avg_incu_temp, color = HW)) +
  geom_point(aes(), size = 1) +
  geom_errorbar(aes(x = Year, ymax = (max_incu_temp), ymin = (min_incu_temp), color = HW), width = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("black", "steelblue", "lightsteelblue3"), labels = c("Before MHW", "Between MHW", "During MHW")) +
  labs(x = "Year", y = "Incubation temperature (\u00B0C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank())
incuP




















#----------------------------------------------------------------------------------------------------------------------------------------------------------
# Plotting




year.colors = c("purple4", "navy", "blue3", "royalblue", "skyblue3", "skyblue1", "orangered", "red1", "orange2", "darkorange2", "red4")





x11()
HD_temps <- ggplot() + 
  geom_line(data = HD_temp, aes(as.Date(DATE_noYear, format = "%b-%d"), DeepTemp, color = as.factor(Year)), size = 0.75) +
  geom_line(data = temp_during_hatching, aes(as.Date(DATE_noYear, format = "%b-%d"), DeepTemp, color = as.factor(Year)), size = 1.5) +
  xlab("Date") + 
  ylab("Daily bottom temperature (\u00b0C)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b%d") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = year.colors) +
  theme_classic()  +
  # theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24),
  #       legend.text = element_text(size = 20), legend.title = element_blank())
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank(),
        legend.margin = margin(t = 0, unit = 'cm'),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.1, unit = "cm"))

HD_temps








x11()
Surf <- ggplot(data = Surf_temp, aes(as.Date(age_DATE_noYear, format = "%d-%b"), SurfTemp, color = as.factor(Year))) + 
  geom_line(size = 0.75) +
  xlab("Date") + 
  ylab("Daily surface temperature (\u00b0C)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b%d") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = year.colors) +
  theme_classic()  +
  # theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24),
  #       legend.text = element_text(size = 20), legend.title = element_blank())
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank(),
        legend.margin = margin(l = 0, unit = 'cm'),
        plot.margin = margin(t = 0.2, r = 0.5, b = 0.2, l = 0.1, unit = "cm"))

Surf







x11()
Deep <- ggplot(data = Bottom_temp, aes(as.Date(age_DATE_noYear, format = "%d-%b"), DeepTemp, color = as.factor(Year))) + 
  geom_line(size = 0.75) +
  xlab("Date") + 
  ylab("Daily bottom temperature (\u00b0C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b%d") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = year.colors) +
  theme_classic()  +
  # theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24),
  #       legend.text = element_text(size = 20), legend.title = element_blank())
  theme(axis.text = element_text(size = 7.5), axis.title = element_text(size = 10),
        legend.text = element_text(size = 9), legend.title = element_blank(),
        legend.margin = margin(t = 0, unit = 'cm'),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.1, unit = "cm"))

Deep








x11()
temps1 <- ggarrange(HD_temps, Deep, Surf, ncol = 3, nrow = 1, common.legend = TRUE, legend = "right") +
  draw_plot_label(label = c("(A)", "(B)", "(C)"), size = 12, x = c(0.07, 0.37, 0.67), y = c(0.99, 0.99, 0.99)) +#
  theme(legend.box.background = element_blank())

temps1

x11()
temps2 <- ggarrange(incuP, bottomP, surfP, ncol = 3, nrow = 1, common.legend = TRUE, legend = "top") +
  draw_plot_label(label = c("(D)", "(E)", "(F)"), size = 12, x = c(0.07, 0.40, 0.76), y = c(0.88, 0.88, 0.88))

temps2

x11()
tempsAll <- ggarrange(temps1, temps2, ncol = 1, nrow = 2, common.legend = FALSE, legend = "right")

tempsAll






# ggsave("08_Data_summaries_temps.jpg", plot = last_plot(), device = "jpeg",
#        width = 6.5, height = 6, units = "in", dpi = 600)











#----------------------------------------------------------------------------------------------------------------------------------------------------------
# Comparing temperatures GAK to Trident





x11()
comp_temps <- ggplot() + 
  geom_line(data = temp_data_compare, aes(as.Date(age_DATE_noYear, format = "%d-%b"), SurfTemp, color = "GAKSurface"), size = 0.75) +
  geom_line(data = temp_data_compare, aes(as.Date(age_DATE_noYear, format = "%d-%b"), DeepTemp, color = "GAKBottom"), size = 0.75) +
  geom_line(data = temp_data_compare, aes(as.Date(age_DATE_noYear, format = "%d-%b"), TridentTemp, color = "Trident"), size = 0.75) +
  facet_wrap(vars(Year)) +
  xlab("Date") + 
  ylab("Daily temperature (\u00b0C)") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b%d") +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("navy", "red", "green")) +
  theme_classic()  +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10),
        legend.text = element_text(size = 8), legend.title = element_blank(), legend.position = "top")
  # theme(axis.text = element_text(size = 10), axis.title = element_text(size = 12))

comp_temps





# ggsave("01_Temp_comparisons.jpg", plot = last_plot(), device = "jpeg",
#        width = 6.5, height = 4, units = "in", dpi = 300)




x11()
comp_temps2 <- ggplot() +
  geom_point(data = temp_data_compare, aes(SurfTemp, TridentTemp), size = 0.25) +
  geom_smooth(data = temp_data_compare, aes(SurfTemp, TridentTemp), method = "lm") +
  geom_abline(slope = 1, color = "red", size = 0.75, linetype = 2) +
  xlab("GAK surface temperature (\u00b0C)") +
  ylab("Trident temperature (\u00b0C)") +
  theme_classic() +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10))

comp_temps2


# ggsave("01_GAKSurf_Trident_Corr.jpg", plot = last_plot(), device = "jpeg",
#        width = 4, height = 4, units = "in", dpi = 300)




cor.test(temp_data_compare$SurfTemp, temp_data_compare$TridentTemp)




