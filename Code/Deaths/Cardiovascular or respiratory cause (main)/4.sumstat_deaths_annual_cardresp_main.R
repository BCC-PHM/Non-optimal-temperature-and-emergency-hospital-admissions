### Deaths - Cardio or respiratory cause - Main analysis period (excluding Covid-19 pandemic)

##---- Step 4.0 Preliminary analysis and descriptive statistics
# deaths first, then environmental exposures

# 4.1 Cardio or respiratory cause deaths data

# a)  Distribution of deaths across temperature ranges

# All

cardrespdeathsfreqplot <- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = total)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Number of deaths")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplot # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig1_cardrespdeathsfreqplot_main.svg", width=7, height=2) # save to files

# 0-4

cardrespdeathsfreqplotd04 <- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = d04)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Deaths (aged 0-4)")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplotd04 # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig2_cardrespdeathsfreqplotd04_main.svg", width=7, height=2) # save to files

# 5-14

cardrespdeathsfreqplotd514 <- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = d514)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Deaths (aged 5-14)")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplotd514 # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig3_cardrespdeathsfreqplotd514_main.svg", width=7, height=2) # save to files

# 15-64

cardrespdeathsfreqplotd1564<- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = d1564)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Deaths (aged 15-64)")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplotd1564 # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig4_cardrespdeathsfreqplotd1564_main.svg", width=7, height=2) # save to files

# 65-74

cardrespdeathsfreqplotd6574<- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = d6574)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Deaths (aged 65-74)")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplotd6574 # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig5_cardrespdeathsfreqplotd6574_main.svg", width=7, height=2) # save to files

# 75+

cardrespdeathsfreqplotd75 <- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean, weight = d75plus)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Deaths (aged 75+)")+
  theme_classic(base_size = 10)

cardrespdeathsfreqplotd75 # view plot

# Save the  plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig6_cardrespdeathsfreqplotd75_main.svg", width=7, height=2) # save to files


# b) Plot the aggregated time series dataset - for one year (2022)
plot_deaths_cardresp_main_aggr <- subset(data_deaths_cardresp_main_aggr, year==2022) |>
  ggplot(aes(x=date, y=total)) +
  geom_line() +
  geom_point(shape=19) +
  labs(x="Date", y="deaths") +
  theme_bw()

plot_deaths_cardresp_main_aggr

ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig7_plot_deaths_cardresp_main_aggr2022.svg", width=10, height=5) # save to files

# c) Cardio or respiratory cause deaths by month

table_deaths_cardresp_main_month <- data_deaths_cardresp_main_aggr %>% # new table
  group_by(month) %>% # create a column and group by month
  summarise(total_deaths = sum(total, na.rm = TRUE), # ignore missing values
            mean_temperature = mean(tasmean, na.rm = TRUE), # mean of daily mean temperatures
            .groups = "drop")

table_deaths_cardresp_main_month <- table_deaths_cardresp_main_month %>% # amend existing table
  mutate(month_name = month.abb[month]) # add month name (e.g. 6 = Jun)

table_deaths_cardresp_main_month <- table_deaths_cardresp_main_month %>%
  select(month_name, total_deaths, mean_temperature)# select only those rows

table_deaths_cardresp_main_month <- table_deaths_cardresp_main_month %>% # turn month into factor to re-order
  mutate(month_name = factor(month_name,
                             levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
  arrange(month_name)

table_deaths_cardresp_main_month # display table

table_deaths_cardresp_main_month %>% # use this table to plot column chart with month and deaths
  ggplot()+
  geom_col(fill = "grey",
           mapping = aes(x = month_name,y = total_deaths))+
  theme_light() +
  labs(title = "Total deaths by month in Birmingham during analysis period",
       x = "Month",
       y = "Total deaths")

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig8_total_monthly_deaths_main_linechart.svg", width=10, height=5)

# d) Cardio or respiratory cause deaths by day of week (similar process as month)

table_deaths_cardresp_main_dow <- data_deaths_cardresp_main_aggr %>% # new table
  group_by(dow) %>% # create a column and group by day of week
  summarise(total_deaths = sum(total, na.rm = TRUE), # ignore missing values
            mean_temperature = mean(tasmean, na.rm = TRUE), # mean of daily mean temperatures
            .groups = "drop")

table_deaths_cardresp_main_dow <- table_deaths_cardresp_main_dow %>% # naming columns so it is clear that 1 is Sun (will then remove numbers)
  mutate(dow = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

table_deaths_cardresp_main_dow <- table_deaths_cardresp_main_dow %>% # turn dow into factor to re-order
  mutate(dow = factor(dow,levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
  arrange(dow)

table_deaths_cardresp_main_dow # display table

table_deaths_cardresp_main_dow %>% # Use table to plot column chart by day of week
  ggplot()+
  geom_col(fill = "grey",
           mapping = aes(
             x = dow,
             y = total_deaths)) +
  theme_light() +
  labs(title = "Total deaths by day of the week in Birmingham during analysis period",
       x = "Day of week",
       y = "Total deaths")

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig9_total_deaths_main_dow.svg", width=10, height=5)

# e) Cardio or respiratory cause deaths over time by age group

# Create age group column (pivot)
data_deaths_cardresp_main_aggr_agegroup <- data_deaths_cardresp_main_aggr %>%
  mutate(month_date = floor_date(date, "month")) %>%
  pivot_longer(cols = c(d04,d514,d1564,d6574,d75plus,total),
               names_to = "age_group",
               values_to = "deaths") %>%
  mutate(age_group = recode(age_group,d04 = "Aged 0-4", d514= "Aged 5-14", d1564 = "Aged 15-64", d6574 = "Aged 65-74", d75plus = "Aged 75+", total   = "All ages")) %>%
  group_by(month_date, age_group) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE),.groups = "drop")

# Plot and include total
ggplot(data_deaths_cardresp_main_aggr_agegroup,
       aes(x = month_date, y = deaths))+
  geom_line(col = "black")+
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +
  theme_light() +
  labs(title = "Total monthly deaths by age group during analysis period",
       x = "Year",
       y = "Total monthly deaths")

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig10_total_monthly_deaths_main_agegroup_linechart.svg", width=10, height=5)

# 4.2 Exposure data

# Covered in admissions dataset

# a)  deaths plot to determine frequency at different temperatures
# tempfreqplot <- ggplot(data_deaths_cardresp_main_aggr, aes(x = tasmean)) +
#  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
#  labs(x = "Daily mean temperature (°C)", y = "Number of days")+
#  theme_classic(base_size = 10)

# tempfreqplot # view plot

# Save the  plot
# ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/figX_tempfreqplot_main.svg", width=7, height=2) # save to files

# b) Summary statistics (temperature)

# Mean and SD (normally distributed)
# summary(daily_temp_aggr$tasmean) # Mean 10.95
# sd(daily_temp_aggr$tasmean) # SD 5.24

# Temporal (between days) variation - Average SD across days by LSOA
# mean(data_deaths_cardresp_main_cts[, list(sd=sd(tasmean)), by=LSOA21CD]$sd) # temporal variation

# Spatial (between LSOAs) variation - Average SD across LSOA by day
# mean(data_deaths_cardresp_main_cts[, list(sd=sd(tasmean)), by=date]$sd) # spatial variation 

# Temperature percentiles
daily_temp_LSOA %>%
  summarise(p025 = quantile(tasmean, 0.025, na.rm = TRUE), # to be used as extreme cold
            p25 = quantile(tasmean, 0.25, na.rm = TRUE),
            p50 = quantile(tasmean, 0.50, na.rm = TRUE),
            p75 = quantile(tasmean, 0.75, na.rm = TRUE),
            p975 = quantile(tasmean, 0.975, na.rm = TRUE)) # to be used as extreme heat

# Hottest and coldest day by daily mean temp by LSOA
# daily_temp_LSOA[which.max(tasmean)] # Highest
# daily_temp_LSOA[which.min(tasmean)] # Lowest

# Hottest and coldest day by daily mean temp across Bham
# daily_temp_aggr[which.max(tasmean)] # Highest
# daily_temp_aggr[which.min(tasmean)] # Lowest

# c) Observing temperature over time - daily temperature by year

# ggplot(daily_temp_aggr,
#       aes(x = date, y = tasmean))+
#  geom_line(col = "black") +
#  theme_light() +
#  labs(title = "Daily mean temperature in Birmingham during analysis period",
#       x = "Year",
#       y = "Mean daily temperature °C")

# ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/figX_daily_mean_temp_linechart_main.svg", width=10, height=5)

# d) Plots of daily temperature across LSOA - using July 2022 heatwave as an example

# Create heat colours 
# fcol <- colorRampPalette(c(heat.colors(16, rev=T), paste0("red",2:4))) # heat-style colours

# Plot three days in July 2022 (17-19)
# bhamlsoashp %>%
#  merge(subset(data_deaths_cardresp_main_cts, date %in% seqdate[2665:2667])) |> # picks July heatwave 2022
#  ggplot() +
#  geom_sf(aes(fill=tasmean), size=0.2, col=1) +
#  scale_fill_gradientn(colours=fcol(10)) + 
#  guides(fill=guide_colourbar(title.position="left", barwidth=15,
#                              barheight=0.5)) +
#  labs(fill="\u00B0C") +
#  coord_sf() +
#  theme_void() +
#  theme(legend.position="bottom") +
#  facet_wrap(~date, nrow=1, labeller=function(x) format(x, format="%d %B %Y"))

# Save the plot
# ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/figX_daily_temp_plots_July22.svg", width=10, height=5)


# 4.3 Cardio or respiratory cause deaths data - scatter plot showing relationship with same day temperature

ggplot(data_deaths_cardresp_main_aggr,
       aes(x = tasmean,y = total)) +
  geom_point(col = "black") +
  theme_classic() +
  labs(title = "Daily deaths and daily mean temperature in Birmingham during analysis period",
       x = "Daily mean temperature (°C)",
       y = "Total daily deaths")

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/1. Summary statistics/fig11_total_deaths_meantemp_scatter.svg", width=10, height=5)

# 4.4 Summary statistics (air quality/PM2.5)

# hist_aq <- hist(dailypm25$pm25,
#                main="Frequency of fine particulate matter average in Birmingham during analysis period",
#                xlab="Daily fine particulate matter (PM2.5) average (ugm-3)",
#                xlim=c(-5,70),
#                ylim=c(0,1000),
#                col="grey",
#                border="black") # skewed

# summary(dailypm25$pm25) # Median 6.96 (IQR 4.96-10.60)

# 4.5 Pearson co-efficient to determine correlation between two environmental exposures (PM2.5 and temperature)
# cor(data_deaths_cardresp_main_aggr$pm25, data_deaths_cardresp_main_aggr$tasmean) # -0.15

#Remove files and data not required
rm(dailypm25,daily_temp_aggr,daily_temp_LSOA,table_deaths_cardresp_main_dow,table_deaths_cardresp_main_month,data_deaths_cardresp_main_aggr_agegroup) # remove data tables not required
rm(plot_deaths_cardresp_main_aggr,seqlsoa) # remove objects/plots/functions not required

# 4.0 END