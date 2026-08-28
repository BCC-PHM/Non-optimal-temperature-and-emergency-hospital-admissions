### Emergency admissions (adm) - All cause - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 5b Quasi-poisson model (not DLNM) - Age groups

# Uses splines for non-linear relationship (and temperature as categorical variable)

# AGE 0-74 YEARS

data_adm_allcause_sen_aggr$a074 <- data_adm_allcause_sen_aggr$total-data_adm_allcause_sen_aggr$a75plus
data_adm_allcause_sen_aggr_tempgroup$a074 <- data_adm_allcause_sen_aggr_tempgroup$total-data_adm_allcause_sen_aggr_tempgroup$a75plus

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_admsen074a <- glm(a074 ~ spline,data_adm_allcause_sen_aggr,family=quasipoisson)
summary(qpmodel_admsen074a)

# Predict number of admissions from qpmodel_admsen074a
preda <- predict(qpmodel_admsen074a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_allcause_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a074),colour = "black",size = 0.6) +
#  geom_line(aes(y=preda),linewidth = 1) +
#  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 0-74)",
#       x = "Date",
#       y = "Number of admissions (aged 0-74)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_splinemodel_seasonality_trends074_sen.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
# residuals074a <- residuals(qpmodel_admsen074a,type="response")
# ggplot(data_adm_allcause_sen_aggr, aes(x=date,residuals074a)) +
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7) +
#  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality and long term trends",
#       x = "Date",
#       y = "Residual variation (Actual-Fitted)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvar074a_sen.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_admsen074b <- glm(a074 ~ spline + factor(dow), data_adm_allcause_sen_aggr, family=quasipoisson)
summary(qpmodel_admsen074b)

# Predict number of admissions from qpmodel_admsen074b
predb <- predict(qpmodel_admsen074b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_allcause_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a074),colour = "black", size = 0.6) +
#  geom_line(aes(y=predb), linewidth = 1) +
#  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 0-74)",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_spline+dow074_sen.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
# residuals074b <- residuals(qpmodel_admsen074b,type="response")
# ggplot(data_adm_allcause_sen_aggr,
#       aes(x=date,residuals074b))+
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7)+
#  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality, long term trends and day of week",
#       x = "Date",
#       y = "Residual variation") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvardow2074_sen.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and a074 admissions
unadjmodeltg_admsena074 <- glm(a074 ~ temp_group, data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_admsena074)

# Exponentiate to get RR from log function
effunadjmodeltg_admsena074 <- ci.lin(unadjmodeltg_admsena074, subset= "temp_group", Exp=T)
effunadjmodeltg_admsena074 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_admsena074a <- glm(a074 ~ temp_group + spline,data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_admsena074a)

# Exponentiate to get RR from log function
effadjmodeltg_admsena074a <- ci.lin(adjmodeltg_admsena074a, subset= "temp_group", Exp=T)
effadjmodeltg_admsena074a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_admsena074b <- glm(a074 ~ temp_group + spline + dow, data_adm_allcause_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admsena074b)

# Exponentiate to get RR from log function
effadjmodeltg_admsena074b <- ci.lin(adjmodeltg_admsena074b,
                             subset= "temp_group",
                             Exp=T)
effadjmodeltg_admsena074b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_admsena074c <- glm(a074 ~ temp_group + spline + dow + pm25, data_adm_allcause_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admsena074c)

# Exponentiate to get RR from log function
effadjmodeltg_admsena074c <- ci.lin(adjmodeltg_admsena074c, subset= "temp_group", Exp=T)
effadjmodeltg_admsena074c # view

effunadjmodeltg_admsena074 # unadjusted
effadjmodeltg_admsena074a # + seasonality and long term trends
effadjmodeltg_admsena074b # dow
effadjmodeltg_admsena074c # pm2.5

tabadjmodeltg_admsen074 <- adjmodeltg_admsena074c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_admsen074 <-rbind(round(effunadjmodeltg_admsena074,2),round(effadjmodeltg_admsena074a,2),round(effadjmodeltg_admsena074b,2),round(effadjmodeltg_admsena074c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_admsen074) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_admsen074) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_admsen074 ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_admsen074

tabadjmodeltg_admsen074 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.85, 1.15) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 0-74)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/fig11_forestplotqpregress74_sen.svg", width=10, height=5)

rm(adjmodeltg_admsena074a,adjmodeltg_admsena074b,adjmodeltg_admsena074c,unadjmodeltg_admsena074, effadjmodeltg_admsena074a,effadjmodeltg_admsena074b,effadjmodeltg_admsena074c, effunadjmodeltg_admsena074, qpmodel_admsen074a,qpmodel_admsen074b,tabefftg_admsen074,tabadjmodeltg_admsen074) # remove data tables not required
rm(preda,predb)  # remove objects/plots/functions not required
# rm(residuals074a,residuals074b) # remove objects/plots/functions not required

# AGE 75+ YEARS

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_admsen75a <- glm(a75plus ~ spline,data_adm_allcause_sen_aggr,family=quasipoisson)
summary(qpmodel_admsen75a)

# Predict number of admissions from qpmodel_admsen75a
predc <- predict(qpmodel_admsen75a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_allcause_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a75plus),colour = "black",size = 0.6) +
#  geom_line(aes(y=predc),linewidth = 1) +
#  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 75+)",
#       x = "Date",
#       y = "Number of admissions (aged 75+)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_splinemodel_seasonality_trends75_sen.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
# residuals75a <- residuals(qpmodel_admsen75a,type="response")
# ggplot(data_adm_allcause_sen_aggr, aes(x=date,residuals75a)) +
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7) +
#  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality and long term trends",
#       x = "Date",
#       y = "Residual variation (Actual-Fitted)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvar75a_sen.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_admsen75b <- glm(a75plus ~ spline + factor(dow), data_adm_allcause_sen_aggr, family=quasipoisson)
summary(qpmodel_admsen75b)

# Predict number of admissions from qpmodel_admsen75b
predd <- predict(qpmodel_admsen75b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_allcause_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a75plus),colour = "black", size = 0.6) +
#  geom_line(aes(y=predd), linewidth = 1) +
#  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 75+)",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_spline+dow75_sen.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
# residuals75b <- residuals(qpmodel_admsen75b,type="response")
# ggplot(data_adm_allcause_sen_aggr,
#       aes(x=date,residuals75b))+
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7)+
#  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality, long term trends and day of week",
#       x = "Date",
#       y = "Residual variation") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvardow275_sen.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d75 admissions
unadjmodeltg_admsend75 <- glm(a75plus ~ temp_group, data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_admsend75)

# Exponentiate to get RR from log function
effunadjmodeltg_admsend75 <- ci.lin(unadjmodeltg_admsend75, subset= "temp_group", Exp=T)
effunadjmodeltg_admsend75 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_admsend75a <- glm(a75plus ~ temp_group + spline,data_adm_allcause_sen_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_admsend75a)

# Exponentiate to get RR from log function
effadjmodeltg_admsend75a <- ci.lin(adjmodeltg_admsend75a, subset= "temp_group", Exp=T)
effadjmodeltg_admsend75a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_admsend75b <- glm(a75plus ~ temp_group + spline + dow, data_adm_allcause_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admsend75b)

# Exponentiate to get RR from log function
effadjmodeltg_admsend75b <- ci.lin(adjmodeltg_admsend75b,
                            subset= "temp_group",
                            Exp=T)
effadjmodeltg_admsend75b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_admsend75c <- glm(a75plus ~ temp_group + spline + dow + pm25, data_adm_allcause_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admsend75c)

# Exponentiate to get RR from log function
effadjmodeltg_admsend75c <- ci.lin(adjmodeltg_admsend75c, subset= "temp_group", Exp=T)
effadjmodeltg_admsend75c # view

effunadjmodeltg_admsend75 # unadjusted
effadjmodeltg_admsend75a # + seasonality and long term trends
effadjmodeltg_admsend75b # dow
effadjmodeltg_admsend75c # pm2.5

tabadjmodeltg_admsen75 <- adjmodeltg_admsend75c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_admsen75 <-rbind(round(effunadjmodeltg_admsend75,2),round(effadjmodeltg_admsend75a,2),round(effadjmodeltg_admsend75b,2),round(effadjmodeltg_admsend75c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_admsen75) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_admsen75) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_admsen75 # View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_admsen75

tabadjmodeltg_admsen75 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.85, 1.15) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 75+)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/All cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/fig12_forestplotqpregress75_sen.svg", width=10, height=5)

rm(adjmodeltg_admsend75a,adjmodeltg_admsend75b,adjmodeltg_admsend75c,unadjmodeltg_admsend75, effadjmodeltg_admsend75a,effadjmodeltg_admsend75b,effadjmodeltg_admsend75c, effunadjmodeltg_admsend75, qpmodel_admsen75a,qpmodel_admsen75b,tabefftg_admsen75,tabadjmodeltg_admsen75) # remove data tables not required
rm(predc,predd) # remove objects/plots/functions not required

# 5b END