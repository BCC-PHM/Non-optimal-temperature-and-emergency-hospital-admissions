### Emergency admissions (adm) - All cause - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 5a Quasi-poisson model (not DLNM)
# Uses splines for non-linear relationship (and temperature as categorical variable) but lag effects are 7 day average

# 5.1 Model seasonality, long term trends and day of the week (not exposure)

# Flexible spline functions to let data determine smooth trend over time
# Number of calendar years * 7 -1 
# Natural spline as the standard approach
data_adm_allcause_sen_aggr$dayseq <- seq(nrow(data_adm_allcause_sen_aggr))
spline <- ns(data_adm_allcause_sen_aggr$dayseq,df=67)

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodeladmsen1 <- glm(total ~ spline,data_adm_allcause_sen_aggr,family=quasipoisson)
summary(qpmodeladmsen1)

# Predict number of admissions from qpmodeladmsen1
pred1 <- predict(qpmodeladmsen1,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
#ggplot(data_adm_allcause_sen_aggr,
#       aes(x=date))+
#  geom_point(aes(y=total),colour = "black",size = 0.6) +
#  geom_line(aes(y=pred1),linewidth = 1) +
#  labs(title ="Natural spline to model seasonality and long term trends in Birmingham",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_splinemodel_seasonality_trends_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
# residuals <- residuals(qpmodeladmsen1,type="response")
# ggplot(data_adm_allcause_sen_aggr,
#       aes(x=date,residuals)) +
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7) +
#  labs(title = "Residual variation in admissions after adjusting for seasonality and long term trends",
#       x = "Date",
#       y = "Residual variation (Actual-Fitted)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvar1_main.svg", width=10, height=5)

# Day of the week is probably playing a role from descriptive statistics
# dow is currently numerical - needs to be a factor to be included
data_adm_allcause_sen_aggr$dow <- as.factor(data_adm_allcause_sen_aggr$dow)

# Fit second quasi-poisson regression model and include dow
qpmodeladmsen2 <- glm(total ~ spline+factor(dow),data_adm_allcause_sen_aggr,family=quasipoisson)
summary(qpmodeladmsen2)

# Predict number of admissions from qpmodeladmsen2
pred2 <- predict(qpmodeladmsen2,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_allcause_sen_aggr,
#     aes(x=date)) +
#  geom_point(aes(y=total),colour = "black", size = 0.6) +
#  geom_line(aes(y=pred2), linewidth = 1) +
#  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_spline+dow_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
# residuals <- residuals(qpmodeladmsen2,type="response")
# ggplot(data_adm_allcause_sen_aggr,
#       aes(x=date,residuals))+
#  geom_point(
#    colour = "black",
#    size = 0.6)+
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7)+
#  labs(
#    title = "Residual variation in admissions after adjusting for seasonality, long term trends and day of week",
#    x = "Date",
#    y = "Residual variation") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvardow2_main.svg", width=10, height=5)


# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Create categories

data_adm_allcause_sen_aggr_tempgroup <- data_adm_allcause_sen_aggr %>%
  mutate(temp_group = cut(tasmeanavg7,
                          breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
                          labels = c("<0°C","0-4°C", "5-9°C", "10-14°C", "15-19°C", ">20°C")))

# set reference level to 15-19°C
data_adm_allcause_sen_aggr_tempgroup$temp_group <- relevel(data_adm_allcause_sen_aggr_tempgroup$temp_group, ref = "15-19°C") # Apply relevel function

# Unadjusted model - only temperature (categorical) and total admissions
unadjmodeltg_admsen <- glm(total ~ temp_group, data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())

summary(unadjmodeltg_admsen)

# Exponentiate to get RR from log function
effunadjmodeltg_admsen <- ci.lin(unadjmodeltg_admsen,
                          subset= "temp_group",
                          Exp=T)
effunadjmodeltg_admsen # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_admsen1 <- glm(total ~ temp_group + spline,data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())

summary(adjmodeltg_admsen1)

# Exponentiate to get RR from log function
effadjmodeltg_admsen1 <- ci.lin(adjmodeltg_admsen1,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_admsen1 # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_admsen2 <- glm(
  total ~ temp_group + spline + dow,
  data_adm_allcause_sen_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_admsen2)

# Exponentiate to get RR from log function
effadjmodeltg_admsen2 <- ci.lin(adjmodeltg_admsen2, subset= "temp_group", Exp=T)
effadjmodeltg_admsen2 # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_admsen3 <- glm(
  total ~ temp_group + spline + dow + pm25,
  data_adm_allcause_sen_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_admsen3)

# Exponentiate to get RR from log function
effadjmodeltg_admsen3 <- ci.lin(adjmodeltg_admsen3,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_admsen3 # view

effunadjmodeltg_admsen # unadjusted
effadjmodeltg_admsen1 # + seasonality and long term trends
effadjmodeltg_admsen2 # dow
effadjmodeltg_admsen3 # pm2.5

tabadjmodeltg_admsen <- adjmodeltg_admsen3 %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round to 2 decimal places

tabadjmodeltg_admsen

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_admsen <-rbind(round(effunadjmodeltg_admsen,2),round(effadjmodeltg_admsen1,2),round(effadjmodeltg_admsen2,2),round(effadjmodeltg_admsen3,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_admsen) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_admsen) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_admsen ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_admsen

tabadjmodeltg_admsen %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (RR) on the x axis
  xlim(0.9, 1.10) +
  geom_point(
    fill = "black") + # show the estimate as a point
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C ",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/fig10_forestplotqpregresstotal_sen.svg", width=10, height=5)

# Model check using a scatter plot of deviance residuals vs time 

# Deviance residuals (previously it was response residuals)
devres_adm <- residuals(adjmodeltg_admsen3,type="deviance")

#devres_admplot <- plot(data_adm_allcause_sen_aggr_tempgroup$date,devres_adm,ylim=c(-5,10),pch=19,cex=0.7,col=grey(0.6),
#                   main="Deviance residuals from main model over time",ylab="Deviance residuals",xlab="Date")
# abline(h=0,lty=2,lwd=2)

# Save/print plot
# dev.print(svg, file="Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_devrianceesidualsplot_main.svg", width=10, height=5)

# Partial autocorrelation function to measure degree of autocorrelation between days
pacf(devres_adm,na.action=na.omit,main="Partial autocorrelation plot of deviance residuals")

# Save/print plot
# dev.print(svg, file="Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_partialautocorrelationplot_main.svg", width=10, height=5)

rm(adjmodeltg_admsen1,adjmodeltg_admsen2,adjmodeltg_admsen3, unadjmodeltg_admsen, effadjmodeltg_admsen1,effadjmodeltg_admsen2,effadjmodeltg_admsen3, effunadjmodeltg_admsen, qpmodeladmsen1,qpmodeladmsen2,tabefftg_admsen,tabadjmodeltg_admsen) # remove data tables not required
rm(pred1,pred2,devres_adm) # remove objects/plots/functions not required

# 5a END