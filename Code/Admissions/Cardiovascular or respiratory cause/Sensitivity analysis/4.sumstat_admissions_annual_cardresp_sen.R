### Emergency admissions (adm) - Cardiorespiratory cause (secondary outcome) - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 4.0 Prelimary analysis and descriptive statistics to explore and understand the data
# Admissions first, then environmental exposures

# 4.1 Cardiovascular or respiratory admissions data

# a)  Distribution of admissions across temperature ranges

# Sensitivity analysis - Keeping only summary statistics and charts to be used in the main body of the report 

# All

admsenfreqplot <- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = total)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Number of admissions")+
  theme_classic(base_size = 10)

admsenfreqplot # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig1_cardrespadmsenfreqplot_sen.svg", width=7, height=2) # save to files

# 0-4

admsenfreqplota04 <- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = a04)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 0-4)")+
  theme_classic(base_size = 10)

admsenfreqplota04 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig2_cardrespadmsenfreqplota04_sen.svg", width=7, height=2) # save to files

# 5-14

admsenfreqplota514 <- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = a514)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 5-14)")+
  theme_classic(base_size = 10)

admsenfreqplota514 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig3_cardrespadmsenfreqplota514_sen.svg", width=7, height=2) # save to files

# 15-64

admsenfreqplota1564<- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = a1564)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 15-64)")+
  theme_classic(base_size = 10)

admsenfreqplota1564 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig4_cardrespadmsenfreqplota1564_sen.svg", width=7, height=2) # save to files

# 65-74

admsenfreqplota6574<- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = a6574)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 65-74)")+
  theme_classic(base_size = 10)

admsenfreqplota6574 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig5_cardrespadmsenfreqplota6574_sen.svg", width=7, height=2) # save to files

# 75+

admsenfreqplota75 <- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean, weight = a75plus)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 75+)")+
  theme_classic(base_size = 10)

admsenfreqplota75 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig6_cardrespadmsenfreqplota75_sen.svg", width=7, height=2) # save to files


# b) Cardiovascular or respiratory admissions over time by age group - keeping to demonstrate Covid impact

# Create age group column (pivot)
data_adm_cardresp_sen_aggr_agegroup <- data_adm_cardresp_sen_aggr %>%
  mutate(month_date = floor_date(date, "month")) %>%
  pivot_longer(cols = c(a04,a514,a1564,a6574,a75plus,total),
               names_to = "age_group",
               values_to = "admissions") %>%
  mutate(age_group = recode(age_group,a04 = "Aged 0-4", a514= "Aged 5-14", a1564 = "Aged 15-64", a6574 = "Aged 65-74", a75plus = "Aged 75+", total   = "All ages")) %>%
  group_by(month_date, age_group) %>%
  summarise(admissions = sum(admissions, na.rm = TRUE),.groups = "drop")

# Plot and include total
ggplot(data_adm_cardresp_sen_aggr_agegroup,
       aes(x = month_date, y = admissions))+
  geom_line(col = "black")+
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +
  theme_light() +
  labs(title = "Total monthly admissions by age group during analysis period",
       x = "Year",
       y = "Total monthly emergency admissions")

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig7_total_monthly_adm_sen_agegroup_linechart.svg", width=10, height=5)

# 4.2 Exposure data

# a)  Admissions plot to determine frequency at different temperatures
tempfreqplot <- ggplot(data_adm_cardresp_sen_aggr, aes(x = tasmean)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Number of days")+
  theme_classic(base_size = 10)

tempfreqplot # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig8_tempfreqplot_sen.svg", width=7, height=2) # save to files

# b) Summary statistics (temperature) - including the Covid-19 pandemic, so not used as descriptive statistics in the main body

# Mean and SD (normally distributed)
summary(daily_temp_aggr$tasmean) # Mean
sd(daily_temp_aggr$tasmean) # SD

# Temperature percentiles
daily_temp_LSOA %>%
  summarise(p025 = quantile(tasmean, 0.025, na.rm = TRUE), # to be used as extreme cold
            p25 = quantile(tasmean, 0.25, na.rm = TRUE),
            p50 = quantile(tasmean, 0.50, na.rm = TRUE),
            p75 = quantile(tasmean, 0.75, na.rm = TRUE),
            p975 = quantile(tasmean, 0.975, na.rm = TRUE)) # to be used as extreme heat

# Hottest and coldest day by daily mean temp by LSOA
daily_temp_LSOA[which.max(tasmean)] # Highest
daily_temp_LSOA[which.min(tasmean)] # Lowest

# Hottest and coldest day by daily mean temp across Bham
daily_temp_aggr[which.max(tasmean)] # Highest
daily_temp_aggr[which.min(tasmean)] # Lowest

# c) Observing temperature over time - daily temperature by year

ggplot(daily_temp_aggr,
       aes(x = date, y = tasmean)) +
  geom_line(col = "black") +
  theme_light() +
  labs(title = "Daily mean temperature in Birmingham during analysis period",
       x = "Year",
       y = "Mean daily temperature °C")

ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/1. Summary statistics/fig9_daily_mean_temp_linechart_sen.svg", width=10, height=3)

#Remove files and data not required
rm(dailypm25,daily_temp_aggr,daily_temp_LSOA,data_adm_cardresp_sen_aggr_agegroup) # remove data tables not required
rm(seqlsoa) # remove objects/plots/functions not required

# 4.0 END