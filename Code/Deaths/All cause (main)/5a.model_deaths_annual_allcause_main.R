### Deaths - All cause - Main analysis (excluding Covid-19 pandemic)

##---- Step 5a Quasi-poisson model (not DLNM)
# Uses splines for non-linear relationship (and temperature as categorical variable) but lag effects are 7 day average

# 5.1 Model seasonality, long term trends and day of the week (not exposure)

# Flexible spline functions to let data determine smooth trend over time
# Number of calendar years (9.75 - 2015 starts in April) * 7), but
# Removal of Covid years for main analysis = 8.59 * 7 = 60 (therefore df=59)
# Natural spline as the standard approach
data_deaths_allcause_main_aggr$dayseq <- seq(nrow(data_deaths_allcause_main_aggr))
spline <- ns(data_deaths_allcause_main_aggr$dayseq,df=59)

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel1 <- glm(total ~ spline,data_deaths_allcause_main_aggr,family=quasipoisson)
summary(qpmodel1)

# Predict number of deaths from qpmodel1
pred1 <- predict(qpmodel1,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to determine whether the spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_allcause_main_aggr,
       aes(x=date))+
  geom_point(aes(y=total),colour = "black",size = 0.6) +
  geom_line(aes(y=pred1),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham",
       x = "Date",
       y = "Number of deaths") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig12_splinemodel_seasonality_trends_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals <- residuals(qpmodel1,type="response")
ggplot(data_deaths_allcause_main_aggr,
       aes(x=date,residuals)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in deaths after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig13_resvar1_main.svg", width=10, height=5)

# Day of the week is probably playing a role from descriptive statistics
# dow is currently numerical - needs to be a factor to be included
data_deaths_allcause_main_aggr$dow <- as.factor(data_deaths_allcause_main_aggr$dow)

# Fit second quasi-poisson regression model and include dow
qpmodel2 <- glm(total ~ spline+factor(dow),data_deaths_allcause_main_aggr,family=quasipoisson)
summary(qpmodel2)

# Predict number of deaths from qpmodel2
pred2 <- predict(qpmodel2,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to see if dow + spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_allcause_main_aggr,
       aes(x=date)) +
  geom_point(aes(y=total),colour = "black", size = 0.6) +
  geom_line(aes(y=pred2), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham",
       x = "Date",
       y = "Number of deaths") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig14_spline+dow_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals <- residuals(qpmodel2,type="response")
ggplot(data_deaths_allcause_main_aggr,
       aes(x=date,residuals))+
  geom_point(
    colour = "black",
    size = 0.6)+
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(
    title = "Residual variation in deaths after adjusting for seasonality, long term trends and day of week",
    x = "Date",
    y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig15_resvardow2_main.svg", width=10, height=5)


# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Create categories

data_deaths_allcause_main_aggr_tempgroup <- data_deaths_allcause_main_aggr %>%
  mutate(temp_group = cut(tasmeanavg7,
                          breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
                          labels = c("<0°C","0-4°C", "5-9°C", "10-14°C", "15-19°C", ">20°C")))

# set reference level to 15-19°C
data_deaths_allcause_main_aggr_tempgroup$temp_group <- relevel(data_deaths_allcause_main_aggr_tempgroup$temp_group, ref = "15-19°C") # Apply relevel function

# Unadjusted model - only temperature (categorical) and total deaths
unadjmodeltg_deaths <- glm(total ~ temp_group, data_deaths_allcause_main_aggr_tempgroup,family = quasipoisson())

summary(unadjmodeltg_deaths)

# Exponentiate to get RR from log function
effunadjmodeltg_deaths <- ci.lin(unadjmodeltg_deaths,
                          subset= "temp_group",
                          Exp=T)
effunadjmodeltg_deaths # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_deaths1 <- glm(total ~ temp_group + spline,data_deaths_allcause_main_aggr_tempgroup,family = quasipoisson())

summary(adjmodeltg_deaths1)

# Exponentiate to get RR from log function
effadjmodeltg_deaths1 <- ci.lin(adjmodeltg_deaths1,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_deaths1 # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_deaths2 <- glm(
  total ~ temp_group + spline + dow,
  data_deaths_allcause_main_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_deaths2)

# Exponentiate to get RR from log function
effadjmodeltg_deaths2 <- ci.lin(adjmodeltg_deaths2, subset= "temp_group", Exp=T)
effadjmodeltg_deaths2 # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_deaths3 <- glm(
  total ~ temp_group + spline + dow + pm25,
  data_deaths_allcause_main_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_deaths3)

# Exponentiate to get RR from log function
effadjmodeltg_deaths3 <- ci.lin(adjmodeltg_deaths3,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_deaths3 # view

effunadjmodeltg_deaths # unadjusted
effadjmodeltg_deaths1 # + seasonality and long term trends
effadjmodeltg_deaths2 # dow
effadjmodeltg_deaths3 # pm2.5

tabadjmodeltg_deaths <- adjmodeltg_deaths3 %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round to 2 decimal places

tabadjmodeltg_deaths

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_deaths <-rbind(round(effunadjmodeltg_deaths,2),round(effadjmodeltg_deaths1,2),round(effadjmodeltg_deaths2,2),round(effadjmodeltg_deaths3,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_deaths) <-list(c("Unadjusted <0°C",
                            "Unadjusted 0-4°C",
                            "Unadjusted 5-9°C",
                            "Unadjusted 10-14°C",
                            "Unadjusted >20°C",
                            "+ Seasonality and long term trends <0°C",
                            "+ Seasonality and long term trends 0-4°C",
                            "+ Seasonality and long term trends 5-9°C",
                            "+ Seasonality and long term trends 10-14°C",
                            "+ Seasonality and long term trends >20°C",
                            "+ day of week <0°C",
                            "+ day of week 0-4°C",
                            "+ day of week 5-9°C",
                            "+ day of week 10-14°C",
                            "+ day of week  >20°C",
                            "+ pm2.5 <0°C",
                            "+ pm2.5 0-4°C",
                            "+ pm2.5 5-9°C",
                            "+ pm2.5 10-14°C",
                            "+ pm2.5  >20°C")) # renames rows
colnames(tabefftg_deaths) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_deaths ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_deaths

tabadjmodeltg_deaths %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (RR) on the x axis
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of mortality compared to reference group 15-19°C ",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig16_forestplotqpregresstotal_main.svg", width=10, height=5)

# Model check using a scatter plot of deviance residuals vs time 

# Deviance residuals (previously it was response residuals)
devresdeaths <- residuals(adjmodeltg_deaths3,type="deviance")

devresdeathsplot <- plot(data_deaths_allcause_main_aggr_tempgroup$date,devresdeaths,ylim=c(-5,10),pch=19,cex=0.7,col=grey(0.6),
                   main="Deviance residuals from main model over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)

# Save/print plot
dev.print(svg, file="Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig17_devrianceesidualsplot_main.svg", width=10, height=5)

# Partial autocorrelation function to measure degree of autocorrelation between days
pacf(devresdeaths,na.action=na.omit,main="Partial autocorrelation plot of deviance residuals")

# Save/print plot
dev.print(svg, file="Outputs/Deaths/All cause/Main/2. Quasi-poisson model (before DLNM)/fig18_partialautocorrelationplot_main.svg", width=10, height=5)

rm(adjmodeltg_deaths1,adjmodeltg_deaths2,adjmodeltg_deaths3, unadjmodeltg_deaths, effadjmodeltg_deaths1,effadjmodeltg_deaths2,effadjmodeltg_deaths3, effunadjmodeltg_deaths, qpmodel1,qpmodel2,tabefftg_deaths,tabadjmodeltg_deaths) # remove data tables not required
rm(pred1,pred2,residuals,devresdeaths,devresdeathsplot) # remove objects/plots/functions not required

# 5a END