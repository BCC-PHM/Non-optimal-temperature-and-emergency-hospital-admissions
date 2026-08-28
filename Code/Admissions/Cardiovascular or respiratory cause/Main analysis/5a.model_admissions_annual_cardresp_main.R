### Emergency admissions (adm) - Cardio-respiratory cause (secondary outcome) - Main analysis (excluding Covid-19 pandemic)

##---- Step 5a Quasi-poisson model (not DLNM)
# Uses splines for non-linear relationship (and temperature as categorical variable) but lag effects are 7 day average

# 5.1 Model seasonality, long term trends and day of the week (not exposure)

# Flexible spline functions to let data determine smooth trend over time
# Number of calendar years (9.75 - 2015 starts in April) * 7), but
# Removal of Covid years for main analysis = 8.59 * 7 = 60 (therefore df=59)
# Natural spline as the standard approach
data_adm_cardresp_main_aggr$dayseq <- seq(nrow(data_adm_cardresp_main_aggr))
spline <- ns(data_adm_cardresp_main_aggr$dayseq,df=59)

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodelcardrespadm1 <- glm(total ~ spline,data_adm_cardresp_main_aggr,family=quasipoisson)
summary(qpmodelcardrespadm1)

# Predict number of admissions from qpmodelcardrespadm1
pred1 <- predict(qpmodelcardrespadm1,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr,
       aes(x=date))+
  geom_point(aes(y=total),colour = "black",size = 0.6) +
  geom_line(aes(y=pred1),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig11_splinemodel_seasonality_trends_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals <- residuals(qpmodelcardrespadm1,type="response")
ggplot(data_adm_cardresp_main_aggr,
       aes(x=date,residuals)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in admissions after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig12_resvar1_cardrespmain.svg", width=10, height=5)

# Day of the week is probably playing a role from descriptive statistics
# dow is currently numerical - needs to be a factor to be included
data_adm_cardresp_main_aggr$dow <- as.factor(data_adm_cardresp_main_aggr$dow)

# Fit second quasi-poisson regression model and include dow
qpmodelcardrespadm2 <- glm(total ~ spline+factor(dow),data_adm_cardresp_main_aggr,family=quasipoisson)
summary(qpmodelcardrespadm2)

# Predict number of admissions from qpmodelcardrespadm2
pred2 <- predict(qpmodelcardrespadm2,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr,
       aes(x=date)) +
  geom_point(aes(y=total),colour = "black", size = 0.6) +
  geom_line(aes(y=pred2), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig13_spline+dow_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals <- residuals(qpmodelcardrespadm2,type="response")
ggplot(data_adm_cardresp_main_aggr,
       aes(x=date,residuals))+
  geom_point(
    colour = "black",
    size = 0.6)+
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(
    title = "Residual variation in admissions after adjusting for seasonality, long term trends and day of week",
    x = "Date",
    y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig14_resvardow2_cardrespmain.svg", width=10, height=5)


# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Create categories

data_adm_cardresp_main_aggr_tempgroup <- data_adm_cardresp_main_aggr %>%
  mutate(temp_group = cut(tasmeanavg7,
                          breaks = c(-Inf, 0, 5, 10, 15, 20, Inf),
                          labels = c("<0°C","0-4°C", "5-9°C", "10-14°C", "15-19°C", ">20°C")))

# set reference level to 15-19°C
data_adm_cardresp_main_aggr_tempgroup$temp_group <- relevel(data_adm_cardresp_main_aggr_tempgroup$temp_group, ref = "15-19°C") # Apply relevel function

# Unadjusted model - only temperature (categorical) and total admissions
unadjmodeltg_cardrespadm <- glm(total ~ temp_group, data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())

summary(unadjmodeltg_cardrespadm)

# Exponentiate to get RR from log function
effunadjmodeltg_cardrespadm <- ci.lin(unadjmodeltg_cardrespadm,
                          subset= "temp_group",
                          Exp=T)
effunadjmodeltg_cardrespadm # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_cardrespadm1 <- glm(total ~ temp_group + spline,data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())

summary(adjmodeltg_cardrespadm1)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadm1 <- ci.lin(adjmodeltg_cardrespadm1,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_cardrespadm1 # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_cardrespadm2 <- glm(
  total ~ temp_group + spline + dow,
  data_adm_cardresp_main_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_cardrespadm2)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadm2 <- ci.lin(adjmodeltg_cardrespadm2, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadm2 # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_cardrespadm3 <- glm(
  total ~ temp_group + spline + dow + pm25,
  data_adm_cardresp_main_aggr_tempgroup,
  family = quasipoisson())

summary(adjmodeltg_cardrespadm3)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadm3 <- ci.lin(adjmodeltg_cardrespadm3,
                         subset= "temp_group",
                         Exp=T)
effadjmodeltg_cardrespadm3 # view

effunadjmodeltg_cardrespadm # unadjusted
effadjmodeltg_cardrespadm1 # + seasonality and long term trends
effadjmodeltg_cardrespadm2 # dow
effadjmodeltg_cardrespadm3 # pm2.5

tabadjmodeltg_cardrespadm <- adjmodeltg_cardrespadm3 %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round to 2 decimal places

tabadjmodeltg_cardrespadm

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_adm <-rbind(round(effunadjmodeltg_cardrespadm,2),round(effadjmodeltg_cardrespadm1,2),round(effadjmodeltg_cardrespadm2,2),round(effadjmodeltg_cardrespadm3,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_adm) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_adm) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_adm ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_cardrespadm

tabadjmodeltg_cardrespadm %>%# remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (RR) on the x axis
  xlim(0.8, 1.2) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C ",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig15_forestplotqpregresstotal_cardrespmain.svg", width=10, height=5)

# Model check using a scatter plot of deviance residuals vs time 

# Deviance residuals (previously it was response residuals)
devres_adm <- residuals(adjmodeltg_cardrespadm3,type="deviance")

devres_admplot <- plot(data_adm_cardresp_main_aggr_tempgroup$date,devres_adm,ylim=c(-5,10),pch=19,cex=0.7,col=grey(0.6),
                   main="Deviance residuals from main model over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)

# Save/print plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig16_devrianceesidualsplot_cardrespmain.svg", width=10, height=5)

# Partial autocorrelation function to measure degree of autocorrelation between days
pacf(devres_adm,na.action=na.omit,main="Partial autocorrelation plot of deviance residuals")

# Save/print plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig17_partialautocorrelationplot_cardrespmain.svg", width=10, height=5)

rm(adjmodeltg_cardrespadm1,adjmodeltg_cardrespadm2,adjmodeltg_cardrespadm3, unadjmodeltg_cardrespadm, effadjmodeltg_cardrespadm1,effadjmodeltg_cardrespadm2,effadjmodeltg_cardrespadm3, effunadjmodeltg_cardrespadm, qpmodelcardrespadm1,qpmodelcardrespadm2,tabefftg_adm,tabadjmodeltg_cardrespadm) # remove data tables not required
rm(pred1,pred2,residuals,devres_adm,devres_admplot) # remove objects/plots/functions not required

# 5a END