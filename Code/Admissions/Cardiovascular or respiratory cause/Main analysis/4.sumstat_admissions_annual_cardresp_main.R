### Emergency admissions (adm) - Cardio-respiratory cause (secondary outcome) - Main analysis (excluding Covid-19 pandemic)

##---- Step 4.0 Prelimary analysis and descriptive statistics to explore and understand the data
# Admissions first, then environmental exposures

# 4.1 All cause admissions data

# a)  Distribution of cardresp admissions across temperature ranges

# All

cardrespadmfreqplot <- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = total)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Number of admissions")+
  theme_classic(base_size = 10)

cardrespadmfreqplot # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig1_cardrespadmfreqplot_main.svg", width=7, height=2) # save to files

# 0-4

cardrespadmfreqplota04 <- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = a04)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 0-4)")+
  theme_classic(base_size = 10)

cardrespadmfreqplota04 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig2_cardrespadmfreqplota04_main.svg", width=7, height=2) # save to files

# 5-14

cardrespadmfreqplota514 <- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = a514)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 5-14)")+
  theme_classic(base_size = 10)

cardrespadmfreqplota514 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig3_cardrespadmfreqplota514_main.svg", width=7, height=2) # save to files

# 15-64

cardrespadmfreqplota1564<- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = a1564)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 15-64)")+
  theme_classic(base_size = 10)

cardrespadmfreqplota1564 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig4_cardrespadmfreqplota1564_main.svg", width=7, height=2) # save to files

# 65-74

cardrespadmfreqplota6574<- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = a6574)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 65-74)")+
  theme_classic(base_size = 10)

cardrespadmfreqplota6574 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig5_cardrespadmfreqplota6574_main.svg", width=7, height=2) # save to files

# 75+

cardrespadmfreqplota75 <- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean, weight = a75plus)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Admissions (aged 75+)")+
  theme_classic(base_size = 10)

cardrespadmfreqplota75 # view plot

# Save the  plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig6_cardrespadmfreqplota75_main.svg", width=7, height=2) # save to files


# d) Plot the aggregated time series dataset - for one year (2022)
plot_adm_cardresp_main_aggr <- subset(data_adm_cardresp_main_aggr, year==2022) |>
  ggplot(aes(x=date, y=total)) +
  geom_line() +
  geom_point(shape=19) +
  labs(x="Date", y="Admissions") +
  theme_bw()

plot_adm_cardresp_main_aggr

ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig7_plot_adm_cardresp_main_aggr2022.svg", width=10, height=5) # save to files

# e) Cardresp cause admissions by month

table_adm_cardresp_main_month <- data_adm_cardresp_main_aggr %>% # new table
  group_by(month) %>% # create a column and group by month
  summarise(total_admissions = sum(total, na.rm = TRUE), # ignore missing values
    mean_temperature = mean(tasmean, na.rm = TRUE), # mean of daily mean temperatures
    .groups = "drop")

table_adm_cardresp_main_month <- table_adm_cardresp_main_month %>% # amend existing table
  mutate(month_name = month.abb[month]) # add month name (e.g. 6 = Jun)

table_adm_cardresp_main_month <- table_adm_cardresp_main_month %>%
  select(month_name, total_admissions, mean_temperature)# select only those rows

table_adm_cardresp_main_month <- table_adm_cardresp_main_month %>% # turn month into factor to re-order
  mutate(month_name = factor(month_name,
                 levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
  arrange(month_name)

table_adm_cardresp_main_month # display table

table_adm_cardresp_main_month %>% # use this table to plot column chart with month and admissions
  ggplot()+
  geom_col(fill = "grey",
    mapping = aes(x = month_name,y = total_admissions))+
theme_light() +
  labs(title = "Total admissions by month in Birmingham during analysis period",
       x = "Month",
       y = "Total emergency admissions")

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig8_total_monthly_cardrespadm_main_linechart.svg", width=10, height=5)

# f) Cardresp cause admissions by day of week (similar process as month)

table_adm_cardresp_main_dow <- data_adm_cardresp_main_aggr %>% # new table
  group_by(dow) %>% # create a column and group by day of week
  summarise(total_admissions = sum(total, na.rm = TRUE), # ignore missing values
    mean_temperature = mean(tasmean, na.rm = TRUE), # mean of daily mean temperatures
    .groups = "drop")

table_adm_cardresp_main_dow <- table_adm_cardresp_main_dow %>% # naming columns so it is clear that 1 is Sun (will then remove numbers)
  mutate(dow = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

table_adm_cardresp_main_dow <- table_adm_cardresp_main_dow %>% # turn dow into factor to re-order
  mutate(dow = factor(dow,levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
  arrange(dow)

table_adm_cardresp_main_dow # display table

table_adm_cardresp_main_dow %>% # Use table to plot column chart by day of week
  ggplot()+
  geom_col(fill = "grey",
    mapping = aes(
      x = dow,
      y = total_admissions)) +
  theme_light() +
  labs(title = "Total admissions by day of the week in Birmingham during analysis period",
       x = "Day of week",
       y = "Total emergency admissions")

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig9_total_cardrespadm_main_dow.svg", width=10, height=5)

# g) Cardresp cause admissions by day of week (similar process as month)

table_adm_cardresp_main_year <- data_adm_cardresp_main_aggr %>% # new table
  group_by(year) %>% # create a column and group by year
  summarise(total_admissions = sum(total, na.rm = TRUE), # ignore missing values
            mean_temperature = mean(tasmean, na.rm = TRUE), # mean of daily mean temperatures
            .groups = "drop")

table_adm_cardresp_main_year # display table

# h) All cause admissions over time by age group

# Create age group column (pivot)
data_adm_cardresp_main_aggr_agegroup <- data_adm_cardresp_main_aggr %>%
  mutate(month_date = floor_date(date, "month")) %>%
  pivot_longer(cols = c(a04,a514,a1564,a6574,a75plus,total),
    names_to = "age_group",
    values_to = "admissions") %>%
  mutate(age_group = recode(age_group,a04 = "Aged 0-4", a514= "Aged 5-14", a1564 = "Aged 15-64", a6574 = "Aged 65-74", a75plus = "Aged 75+", total   = "All ages")) %>%
  group_by(month_date, age_group) %>%
  summarise(admissions = sum(admissions, na.rm = TRUE),.groups = "drop")

# Plot and include total
ggplot(data_adm_cardresp_main_aggr_agegroup,
  aes(x = month_date, y = admissions))+
  geom_line(col = "black")+
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +
  theme_light() +
  labs(title = "Total monthly admissions by age group during analysis period",
    x = "Year",
    y = "Total monthly emergency admissions")

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/figX_total_monthly_adm_main_agegroup_linechart.svg", width=10, height=5)

# 4.2 Exposure data

# This has been saved when analysing the all cause admissions dataset

# a)  Admissions plot to determine frequency at different temperatures
tempfreqplot <- ggplot(data_adm_cardresp_main_aggr, aes(x = tasmean)) +
  geom_histogram(binwidth = 1, fill = "grey", colour = "black") +
  labs(x = "Daily mean temperature (°C)", y = "Number of days")+
  theme_classic(base_size = 10)

tempfreqplot # view plot

# Save the  plot
# ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/figX_tempfreqplot_main.svg", width=7, height=2) # save to files

# b) Summary statistics (temperature)

# Mean and SD (normally distributed)
# summary(daily_temp_aggr$tasmean) # Mean
# sd(daily_temp_aggr$tasmean) # SD 

# Temporal (between days) variation - Average SD across days by LSOA - all cause
# mean(data_adm_cardresp_main_cts[, list(sd=sd(tasmean)), by=LSOA21CD]$sd) # temporal variation

# Spatial (between LSOAs) variation - Average SD across LSOA by day - all cause
# mean(data_adm_cardresp_main_cts[, list(sd=sd(tasmean)), by=date]$sd) # spatial variation

# Temperature percentiles
# daily_temp_LSOA %>%
#  summarise(p025 = quantile(tasmean, 0.025, na.rm = TRUE), # to be used as extreme cold
#            p25 = quantile(tasmean, 0.25, na.rm = TRUE),
#            p50 = quantile(tasmean, 0.50, na.rm = TRUE),
#            p75 = quantile(tasmean, 0.75, na.rm = TRUE),
#            p975 = quantile(tasmean, 0.975, na.rm = TRUE)) # to be used as extreme heat

# Hottest and coldest day by daily mean temp by LSOA
# daily_temp_LSOA[which.max(tasmean)] # Highest
# daily_temp_LSOA[which.min(tasmean)] # Lowest

# Hottest and coldest day by daily mean temp across Bham
# daily_temp_aggr[which.max(tasmean)] # Highest
# daily_temp_aggr[which.min(tasmean)] # Lowest

# c) Observing temperature over time - daily temperature by year

# ggplot(daily_temp_aggr,
#       aes(x = date, y = tasmean)) +
#  geom_line(col = "black") +
#  theme_light() +
#  labs(title = "Daily mean temperature in Birmingham during analysis period",
#       x = "Year",
#       y = "Mean daily temperature °C")

# ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/figX_daily_mean_temp_linechart_main.svg", width=10, height=5)

# d) Plots of daily temperature across LSOA - using July 2022 heatwave as an example

# Create heat colours 
# fcol <- colorRampPalette(c(heat.colors(16, rev=T), paste0("red",2:4))) # heat-style colours

# Plot three days in July 2022 (17-19)
# bhamlsoashp %>%
#  merge(subset(data_adm_cardresp_main_cts, date %in% seqdate[2665:2667])) |> # picks July heatwave 2022
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
# ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/figX_daily_temp_plots_July22.svg", width=10, height=5)


# 4.3 All cause admissions data - scatter plot showing relationship with same day temperature

ggplot(data_adm_cardresp_main_aggr,
       aes(x = tasmean,y = total)) +
  geom_point(col = "black") +
  theme_classic() +
  labs(title = "Daily emergency admissions and daily mean temperature in Birmingham during analysis period",
       x = "Daily mean temperature (°C)",
       y = "Total daily emergency admissions")

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/fig10_total_adm_meantemp_scatter.svg", width=10, height=5)

# 4.4 Summary statistics (air quality/PM2.5)

hist_aq <- hist(dailypm25$pm25,
                main="Frequency of PM2.5 average in Birmingham during analysis period",
                xlab="Daily fine particulate matter (PM2.5) average (ugm-3)",
                xlim=c(-5,70),
                ylim=c(0,1000),
                col="grey",
                border="black") # skewed

# dev.print(svg, file="Outputs/Admissions/Cardresp cause/Main/1. Summary statistics/figX_pm2.5_hist_adm_main.svg", width=10, height=5)

# summary(dailypm25$pm25) # Median 7.03 (IQR 5.04-10.63)

# 4.5 Pearson co-efficient to determine correlation between two environmental exposures (PM2.5 and temperature)
# cor(data_adm_cardresp_main_aggr$pm25, data_adm_cardresp_main_aggr$tasmean) # -0.16

#Remove files and data not required
rm(dailypm25,daily_temp_aggr,daily_temp_LSOA,table_adm_cardresp_main_dow,table_adm_cardresp_main_month,data_adm_cardresp_main_aggr_agegroup) # remove data tables not required
rm(hist_aq,plot_adm_cardresp_main_aggr,seqlsoa) # remove objects/plots/functions not required

# 4.0 END