### Emergency admissions (adm) - Cardiorespiratory cause (secondary outcome) - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 5b Quasi-poisson model (not DLNM) - Age groups

# Uses splines for non-linear relationship (and temperature as categorical variable)

# AGE 0-74 YEARS

data_adm_cardresp_sen_aggr$a074 <- data_adm_cardresp_sen_aggr$total-data_adm_cardresp_sen_aggr$a75plus
data_adm_cardresp_sen_aggr_tempgroup$a074 <- data_adm_cardresp_sen_aggr_tempgroup$total-data_adm_cardresp_sen_aggr_tempgroup$a75plus

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_cardrespadmsen074a <- glm(a074 ~ spline,data_adm_cardresp_sen_aggr,family=quasipoisson)
summary(qpmodel_cardrespadmsen074a)

# Predict number of admissions from qpmodel_cardrespadmsen074a
preda <- predict(qpmodel_cardrespadmsen074a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a074),colour = "black",size = 0.6) +
#  geom_line(aes(y=preda),linewidth = 1) +
#  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 0-74)",
#       x = "Date",
#       y = "Number of admissions (aged 0-74)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_splinemodel_seasonality_trends074_sen.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
# residuals074a <- residuals(qpmodel_cardrespadmsen074a,type="response")
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date,residuals074a)) +
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7) +
#  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality and long term trends",
#       x = "Date",
#       y = "Residual variation (Actual-Fitted)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvar074a_sen.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_cardrespadmsen074b <- glm(a074 ~ spline + factor(dow), data_adm_cardresp_sen_aggr, family=quasipoisson)
summary(qpmodel_cardrespadmsen074b)

# Predict number of admissions from qpmodel_cardrespadmsen074b
predb <- predict(qpmodel_cardrespadmsen074b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a074),colour = "black", size = 0.6) +
#  geom_line(aes(y=predb), linewidth = 1) +
#  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 0-74)",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_spline+dow074_sen.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
# residuals074b <- residuals(qpmodel_cardrespadmsen074b,type="response")
# ggplot(data_adm_cardresp_sen_aggr,
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
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvardow2074_sen.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and a074 admissions
unadjmodeltg_cardrespadmsena074 <- glm(a074 ~ temp_group, data_adm_cardresp_sen_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_cardrespadmsena074)

# Exponentiate to get RR from log function
effunadjmodeltg_cardrespadmsena074 <- ci.lin(unadjmodeltg_cardrespadmsena074, subset= "temp_group", Exp=T)
effunadjmodeltg_cardrespadmsena074 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_cardrespadmsena074a <- glm(a074 ~ temp_group + spline,data_adm_cardresp_sen_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_cardrespadmsena074a)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsena074a <- ci.lin(adjmodeltg_cardrespadmsena074a, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmsena074a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_cardrespadmsena074b <- glm(a074 ~ temp_group + spline + dow, data_adm_cardresp_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmsena074b)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsena074b <- ci.lin(adjmodeltg_cardrespadmsena074b,
                                            subset= "temp_group",
                                            Exp=T)
effadjmodeltg_cardrespadmsena074b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_cardrespadmsena074c <- glm(a074 ~ temp_group + spline + dow + pm25, data_adm_cardresp_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmsena074c)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsena074c <- ci.lin(adjmodeltg_cardrespadmsena074c, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmsena074c # view

effunadjmodeltg_cardrespadmsena074 # unadjusted
effadjmodeltg_cardrespadmsena074a # + seasonality and long term trends
effadjmodeltg_cardrespadmsena074b # dow
effadjmodeltg_cardrespadmsena074c # pm2.5

tabadjmodeltg_cardrespadmsen074 <- adjmodeltg_cardrespadmsena074c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_cardrespadmsen074 <-rbind(round(effunadjmodeltg_cardrespadmsena074,2),round(effadjmodeltg_cardrespadmsena074a,2),round(effadjmodeltg_cardrespadmsena074b,2),round(effadjmodeltg_cardrespadmsena074c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_cardrespadmsen074) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_cardrespadmsen074) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_cardrespadmsen074 ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_cardrespadmsen074

tabadjmodeltg_cardrespadmsen074 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.5, 1.5) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 0-74)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/fig11_forestplotqpregress74_sen.svg", width=10, height=5)

rm(adjmodeltg_cardrespadmsena074a,adjmodeltg_cardrespadmsena074b,adjmodeltg_cardrespadmsena074c,unadjmodeltg_cardrespadmsena074, effadjmodeltg_cardrespadmsena074a,effadjmodeltg_cardrespadmsena074b,effadjmodeltg_cardrespadmsena074c, effunadjmodeltg_cardrespadmsena074, qpmodel_cardrespadmsen074a,qpmodel_cardrespadmsen074b,tabefftg_cardrespadmsen074,tabadjmodeltg_cardrespadmsen074) # remove data tables not required
rm(preda,predb) # remove objects/plots/functions not required

# AGE 75+ YEARS

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_cardrespadmsen75a <- glm(a75plus ~ spline,data_adm_cardresp_sen_aggr,family=quasipoisson)
summary(qpmodel_cardrespadmsen75a)

# Predict number of admissions from qpmodel_cardrespadmsen75a
predc <- predict(qpmodel_cardrespadmsen75a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a75plus),colour = "black",size = 0.6) +
#  geom_line(aes(y=predc),linewidth = 1) +
#  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 75+)",
#       x = "Date",
#       y = "Number of admissions (aged 75+)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_splinemodel_seasonality_trends75_sen.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
# residuals75a <- residuals(qpmodel_cardrespadmsen75a,type="response")
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date,residuals75a)) +
#  geom_point(colour = "black",size = 0.6) +
#  geom_hline(yintercept = 0, # 0 is the reference
#             linetype = "dashed",
#             linewidth = 0.7) +
#  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality and long term trends",
#       x = "Date",
#       y = "Residual variation (Actual-Fitted)") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvar75a_sen.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_cardrespadmsen75b <- glm(a75plus ~ spline + factor(dow), data_adm_cardresp_sen_aggr, family=quasipoisson)
summary(qpmodel_cardrespadmsen75b)

# Predict number of admissions from qpmodel_cardrespadmsen75b
predd <- predict(qpmodel_cardrespadmsen75b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
# ggplot(data_adm_cardresp_sen_aggr, aes(x=date)) +
#  geom_point(aes(y=a75plus),colour = "black", size = 0.6) +
#  geom_line(aes(y=predd), linewidth = 1) +
#  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 75+)",
#       x = "Date",
#       y = "Number of admissions") +
#  theme_classic()

# Save the plot
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_spline+dow75_sen.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
# residuals75b <- residuals(qpmodel_cardrespadmsen75b,type="response")
# ggplot(data_adm_cardresp_sen_aggr,
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
# ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/figX_resvardow275_sen.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d75 admissions
unadjmodeltg_cardrespadmsend75 <- glm(a75plus ~ temp_group, data_adm_cardresp_sen_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_cardrespadmsend75)

# Exponentiate to get RR from log function
effunadjmodeltg_cardrespadmsend75 <- ci.lin(unadjmodeltg_cardrespadmsend75, subset= "temp_group", Exp=T)
effunadjmodeltg_cardrespadmsend75 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_cardrespadmsend75a <- glm(a75plus ~ temp_group + spline,data_adm_cardresp_sen_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_cardrespadmsend75a)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsend75a <- ci.lin(adjmodeltg_cardrespadmsend75a, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmsend75a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_cardrespadmsend75b <- glm(a75plus ~ temp_group + spline + dow, data_adm_cardresp_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmsend75b)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsend75b <- ci.lin(adjmodeltg_cardrespadmsend75b,
                                           subset= "temp_group",
                                           Exp=T)
effadjmodeltg_cardrespadmsend75b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_cardrespadmsend75c <- glm(a75plus ~ temp_group + spline + dow + pm25, data_adm_cardresp_sen_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmsend75c)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmsend75c <- ci.lin(adjmodeltg_cardrespadmsend75c, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmsend75c # view

effunadjmodeltg_cardrespadmsend75 # unadjusted
effadjmodeltg_cardrespadmsend75a # + seasonality and long term trends
effadjmodeltg_cardrespadmsend75b # dow
effadjmodeltg_cardrespadmsend75c # pm2.5

tabadjmodeltg_cardrespadmsen75 <- adjmodeltg_cardrespadmsend75c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_cardrespadmsen75 <-rbind(round(effunadjmodeltg_cardrespadmsend75,2),round(effadjmodeltg_cardrespadmsend75a,2),round(effadjmodeltg_cardrespadmsend75b,2),round(effadjmodeltg_cardrespadmsend75c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_cardrespadmsen75) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_cardrespadmsen75) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_cardrespadmsen75 # View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_cardrespadmsen75

tabadjmodeltg_cardrespadmsen75 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.5, 1.5) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 75+)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/Cardresp cause/Sensitivity analysis/2. Quasi-poisson model (before DLNM)/fig12_forestplotqpregress75_sen.svg", width=10, height=5)

rm(adjmodeltg_cardrespadmsend75a,adjmodeltg_cardrespadmsend75b,adjmodeltg_cardrespadmsend75c,unadjmodeltg_cardrespadmsend75, effadjmodeltg_cardrespadmsend75a,effadjmodeltg_cardrespadmsend75b,effadjmodeltg_cardrespadmsend75c, effunadjmodeltg_cardrespadmsend75, qpmodel_cardrespadmsen75a,qpmodel_cardrespadmsen75b,tabefftg_cardrespadmsen75,tabadjmodeltg_cardrespadmsen75) # remove data tables not required
rm(predc,predd) # remove objects/plots/functions not required

# 5b END
