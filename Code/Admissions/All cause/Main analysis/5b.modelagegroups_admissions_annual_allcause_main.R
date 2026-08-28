### Emergency admissions (adm) - All cause - Main analysis (excluding Covid-19 pandemic)

##---- Step 5b Quasi-poisson model (not DLNM) - Age groups

# Uses splines for non-linear relationship (and temperature as categorical variable)

# AGE 0-74 YEARS

data_adm_allcause_main_aggr$a074 <- data_adm_allcause_main_aggr$total-data_adm_allcause_main_aggr$a75plus
data_adm_allcause_main_aggr_tempgroup$a074 <- data_adm_allcause_main_aggr_tempgroup$total-data_adm_allcause_main_aggr_tempgroup$a75plus

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_adm074a <- glm(a074 ~ spline,data_adm_allcause_main_aggr,family=quasipoisson)
summary(qpmodel_adm074a)

# Predict number of admissions from qpmodel_adm074a
preda <- predict(qpmodel_adm074a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_allcause_main_aggr, aes(x=date)) +
  geom_point(aes(y=a074),colour = "black",size = 0.6) +
  geom_line(aes(y=preda),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of admissions (aged 0-74)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig21_splinemodel_seasonality_trends074_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals074a <- residuals(qpmodel_adm074a,type="response")
ggplot(data_adm_allcause_main_aggr, aes(x=date,residuals074a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig22_resvar074a_main.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_adm074b <- glm(a074 ~ spline + factor(dow), data_adm_allcause_main_aggr, family=quasipoisson)
summary(qpmodel_adm074b)

# Predict number of admissions from qpmodel_adm074b
predb <- predict(qpmodel_adm074b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_allcause_main_aggr, aes(x=date)) +
  geom_point(aes(y=a074),colour = "black", size = 0.6) +
  geom_line(aes(y=predb), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig23_spline+dow074_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals074b <- residuals(qpmodel_adm074b,type="response")
ggplot(data_adm_allcause_main_aggr,
       aes(x=date,residuals074b))+
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality, long term trends and day of week",
       x = "Date",
       y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig24_resvardow2074_main.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and a074 admissions
unadjmodeltg_adma074 <- glm(a074 ~ temp_group, data_adm_allcause_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_adma074)

# Exponentiate to get RR from log function
effunadjmodeltg_adma074 <- ci.lin(unadjmodeltg_adma074, subset= "temp_group", Exp=T)
effunadjmodeltg_adma074 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_adma074a <- glm(a074 ~ temp_group + spline,data_adm_allcause_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_adma074a)

# Exponentiate to get RR from log function
effadjmodeltg_adma074a <- ci.lin(adjmodeltg_adma074a, subset= "temp_group", Exp=T)
effadjmodeltg_adma074a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_adma074b <- glm(a074 ~ temp_group + spline + dow, data_adm_allcause_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_adma074b)

# Exponentiate to get RR from log function
effadjmodeltg_adma074b <- ci.lin(adjmodeltg_adma074b,
                             subset= "temp_group",
                             Exp=T)
effadjmodeltg_adma074b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_adma074c <- glm(a074 ~ temp_group + spline + dow + pm25, data_adm_allcause_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_adma074c)

# Exponentiate to get RR from log function
effadjmodeltg_adma074c <- ci.lin(adjmodeltg_adma074c, subset= "temp_group", Exp=T)
effadjmodeltg_adma074c # view

effunadjmodeltg_adma074 # unadjusted
effadjmodeltg_adma074a # + seasonality and long term trends
effadjmodeltg_adma074b # dow
effadjmodeltg_adma074c # pm2.5

tabadjmodeltg_adm074 <- adjmodeltg_adma074c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_adm074 <-rbind(round(effunadjmodeltg_adma074,2),round(effadjmodeltg_adma074a,2),round(effadjmodeltg_adma074b,2),round(effadjmodeltg_adma074c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_adm074) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_adm074) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_adm074 ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_adm074

tabadjmodeltg_adm074 %>% # remove intercept term from the multi-variable results
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

ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig25_forestplotqpregress74_main.svg", width=10, height=5)

rm(adjmodeltg_adma074a,adjmodeltg_adma074b,adjmodeltg_adma074c,unadjmodeltg_adma074, effadjmodeltg_adma074a,effadjmodeltg_adma074b,effadjmodeltg_adma074c, effunadjmodeltg_adma074, qpmodel_adm074a,qpmodel_adm074b,tabefftg_adm074,tabadjmodeltg_adm074) # remove data tables not required
rm(preda,predb,residuals074a,residuals074b) # remove objects/plots/functions not required


# AGE 75+ YEARS

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel_adm75a <- glm(a75plus ~ spline,data_adm_allcause_main_aggr,family=quasipoisson)
summary(qpmodel_adm75a)

# Predict number of admissions from qpmodel_adm75a
predc <- predict(qpmodel_adm75a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_allcause_main_aggr, aes(x=date)) +
  geom_point(aes(y=a75plus),colour = "black",size = 0.6) +
  geom_line(aes(y=predc),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of admissions (aged 75+)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig26_splinemodel_seasonality_trends75_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals75a <- residuals(qpmodel_adm75a,type="response")
ggplot(data_adm_allcause_main_aggr, aes(x=date,residuals75a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig27_resvar75a_main.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel_adm75b <- glm(a75plus ~ spline + factor(dow), data_adm_allcause_main_aggr, family=quasipoisson)
summary(qpmodel_adm75b)

# Predict number of admissions from qpmodel_adm75b
predd <- predict(qpmodel_adm75b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_allcause_main_aggr, aes(x=date)) +
  geom_point(aes(y=a75plus),colour = "black", size = 0.6) +
  geom_line(aes(y=predd), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig28_spline+dow75_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals75b <- residuals(qpmodel_adm75b,type="response")
ggplot(data_adm_allcause_main_aggr,
       aes(x=date,residuals75b))+
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality, long term trends and day of week",
       x = "Date",
       y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig29_resvardow275_main.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d75 admissions
unadjmodeltg_admd75 <- glm(a75plus ~ temp_group, data_adm_allcause_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_admd75)

# Exponentiate to get RR from log function
effunadjmodeltg_admd75 <- ci.lin(unadjmodeltg_admd75, subset= "temp_group", Exp=T)
effunadjmodeltg_admd75 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_admd75a <- glm(a75plus ~ temp_group + spline,data_adm_allcause_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_admd75a)

# Exponentiate to get RR from log function
effadjmodeltg_admd75a <- ci.lin(adjmodeltg_admd75a, subset= "temp_group", Exp=T)
effadjmodeltg_admd75a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_admd75b <- glm(a75plus ~ temp_group + spline + dow, data_adm_allcause_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admd75b)

# Exponentiate to get RR from log function
effadjmodeltg_admd75b <- ci.lin(adjmodeltg_admd75b,
                            subset= "temp_group",
                            Exp=T)
effadjmodeltg_admd75b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_admd75c <- glm(a75plus ~ temp_group + spline + dow + pm25, data_adm_allcause_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_admd75c)

# Exponentiate to get RR from log function
effadjmodeltg_admd75c <- ci.lin(adjmodeltg_admd75c, subset= "temp_group", Exp=T)
effadjmodeltg_admd75c # view

effunadjmodeltg_admd75 # unadjusted
effadjmodeltg_admd75a # + seasonality and long term trends
effadjmodeltg_admd75b # dow
effadjmodeltg_admd75c # pm2.5

tabadjmodeltg_adm75 <- adjmodeltg_admd75c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_adm75 <-rbind(round(effunadjmodeltg_admd75,2),round(effadjmodeltg_admd75a,2),round(effadjmodeltg_admd75b,2),round(effadjmodeltg_admd75c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg_adm75) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg_adm75) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg_adm75 # View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg_adm75

tabadjmodeltg_adm75 %>% ## remove intercept term from the multi-variable results
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

ggsave("Outputs/Admissions/All cause/Main/2. Quasi-poisson model (before DLNM)/fig30_forestplotqpregress75_main.svg", width=10, height=5)

rm(adjmodeltg_admd75a,adjmodeltg_admd75b,adjmodeltg_admd75c,unadjmodeltg_admd75, effadjmodeltg_admd75a,effadjmodeltg_admd75b,effadjmodeltg_admd75c, effunadjmodeltg_admd75, qpmodel_adm75a,qpmodel_adm75b,tabefftg_adm75,tabadjmodeltg_adm75) # remove data tables not required
rm(predc,predd,residuals75a,residuals75b) # remove objects/plots/functions not required

# 5b END