### Emergency admissions (adm) - Cardio-respiratory cause (secondary outcome) - Main analysis (excluding Covid-19 pandemic)

##---- Step 5b Quasi-poisson model (not DLNM) - Age groups

# Uses splines for non-linear relationship (and temperature as categorical variable)

# AGE 0-74 YEARS

data_adm_cardresp_main_aggr$a074 <- data_adm_cardresp_main_aggr$total-data_adm_cardresp_main_aggr$a75plus
data_adm_cardresp_main_aggr_tempgroup$a074 <- data_adm_cardresp_main_aggr_tempgroup$total-data_adm_cardresp_main_aggr_tempgroup$a75plus

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodelcardrespadm074a <- glm(a074 ~ spline,data_adm_cardresp_main_aggr,family=quasipoisson)
summary(qpmodelcardrespadm074a)

# Predict number of admissions from qpmodelcardrespadm074a
preda <- predict(qpmodelcardrespadm074a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=a074),colour = "black",size = 0.6) +
  geom_line(aes(y=preda),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of admissions (aged 0-74)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig18_splinemodel_seasonality_trends074_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals074a <- residuals(qpmodelcardrespadm074a,type="response")
ggplot(data_adm_cardresp_main_aggr, aes(x=date,residuals074a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in admissions (aged 0-74) after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig19_resvar074a_cardrespmain.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodelcardrespadm074b <- glm(a074 ~ spline + factor(dow), data_adm_cardresp_main_aggr, family=quasipoisson)
summary(qpmodelcardrespadm074b)

# Predict number of admissions from qpmodelcardrespadm074b
predb <- predict(qpmodelcardrespadm074b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=a074),colour = "black", size = 0.6) +
  geom_line(aes(y=predb), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig20_spline+dow074_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals074b <- residuals(qpmodelcardrespadm074b,type="response")
ggplot(data_adm_cardresp_main_aggr,
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
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig21_resvardow2074_cardrespmain.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and a074 admissions
unadjmodeltg_cardrespadma074 <- glm(a074 ~ temp_group, data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_cardrespadma074)

# Exponentiate to get RR from log function
effunadjmodeltg_cardrespadma074 <- ci.lin(unadjmodeltg_cardrespadma074, subset= "temp_group", Exp=T)
effunadjmodeltg_cardrespadma074 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_cardrespadma074a <- glm(a074 ~ temp_group + spline,data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_cardrespadma074a)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadma074a <- ci.lin(adjmodeltg_cardrespadma074a, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadma074a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_cardrespadma074b <- glm(a074 ~ temp_group + spline + dow, data_adm_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadma074b)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadma074b <- ci.lin(adjmodeltg_cardrespadma074b,
                             subset= "temp_group",
                             Exp=T)
effadjmodeltg_cardrespadma074b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_cardrespadma074c <- glm(a074 ~ temp_group + spline + dow + pm25, data_adm_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadma074c)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadma074c <- ci.lin(adjmodeltg_cardrespadma074c, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadma074c # view

effunadjmodeltg_cardrespadma074 # unadjusted
effadjmodeltg_cardrespadma074a # + seasonality and long term trends
effadjmodeltg_cardrespadma074b # dow
effadjmodeltg_cardrespadma074c # pm2.5

tabadjmodeltg_cardrespadm074 <- adjmodeltg_cardrespadma074c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_adm074 <-rbind(round(effunadjmodeltg_cardrespadma074,2),round(effadjmodeltg_cardrespadma074a,2),round(effadjmodeltg_cardrespadma074b,2),round(effadjmodeltg_cardrespadma074c,2)) [,5:7] # stacks three models and uses CI columns

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

tabadjmodeltg_cardrespadm074

tabadjmodeltg_cardrespadm074 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.8, 1.2) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 0-74)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig22_forestplotqpregress74_cardrespmain.svg", width=10, height=5)

rm(adjmodeltg_cardrespadma074a,adjmodeltg_cardrespadma074b,adjmodeltg_cardrespadma074c,unadjmodeltg_cardrespadma074, effadjmodeltg_cardrespadma074a,effadjmodeltg_cardrespadma074b,effadjmodeltg_cardrespadma074c, effunadjmodeltg_cardrespadma074, qpmodelcardrespadm074a,qpmodelcardrespadm074b,tabefftg_adm074,tabadjmodeltg_cardrespadm074) # remove data tables not required
rm(preda,predb,residuals074a,residuals074b) # remove objects/plots/functions not required


# AGE 75+ YEARS

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodelcardrespadm75a <- glm(a75plus ~ spline,data_adm_cardresp_main_aggr,family=quasipoisson)
summary(qpmodelcardrespadm75a)

# Predict number of admissions from qpmodelcardrespadm75a
predc <- predict(qpmodelcardrespadm75a,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to determine whether the spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=a75plus),colour = "black",size = 0.6) +
  geom_line(aes(y=predc),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of admissions (aged 75+)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig23_splinemodel_seasonality_trends75_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals75a <- residuals(qpmodelcardrespadm75a,type="response")
ggplot(data_adm_cardresp_main_aggr, aes(x=date,residuals75a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in admissions (aged 75+) after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig24_resvar75a_cardrespmain.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodelcardrespadm75b <- glm(a75plus ~ spline + factor(dow), data_adm_cardresp_main_aggr, family=quasipoisson)
summary(qpmodelcardrespadm75b)

# Predict number of admissions from qpmodelcardrespadm75b
predd <- predict(qpmodelcardrespadm75b,type="response") # expected admissions for each date in dataset

# Plot observed and predicted admissions to see if dow + spline captures the admissions pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_adm_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=a75plus),colour = "black", size = 0.6) +
  geom_line(aes(y=predd), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of admissions") +
  theme_classic()

# Save the plot
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig25_spline+dow75_cardrespmain.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals75b <- residuals(qpmodelcardrespadm75b,type="response")
ggplot(data_adm_cardresp_main_aggr,
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
ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig26_resvardow275_cardrespmain.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d75 admissions
unadjmodeltg_cardrespadmd75 <- glm(a75plus ~ temp_group, data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltg_cardrespadmd75)

# Exponentiate to get RR from log function
effunadjmodeltg_cardrespadmd75 <- ci.lin(unadjmodeltg_cardrespadmd75, subset= "temp_group", Exp=T)
effunadjmodeltg_cardrespadmd75 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltg_cardrespadmd75a <- glm(a75plus ~ temp_group + spline,data_adm_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltg_cardrespadmd75a)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmd75a <- ci.lin(adjmodeltg_cardrespadmd75a, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmd75a # view

# Adjusted model (2) to include spline + day of week

adjmodeltg_cardrespadmd75b <- glm(a75plus ~ temp_group + spline + dow, data_adm_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmd75b)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmd75b <- ci.lin(adjmodeltg_cardrespadmd75b,
                            subset= "temp_group",
                            Exp=T)
effadjmodeltg_cardrespadmd75b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltg_cardrespadmd75c <- glm(a75plus ~ temp_group + spline + dow + pm25, data_adm_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltg_cardrespadmd75c)

# Exponentiate to get RR from log function
effadjmodeltg_cardrespadmd75c <- ci.lin(adjmodeltg_cardrespadmd75c, subset= "temp_group", Exp=T)
effadjmodeltg_cardrespadmd75c # view

effunadjmodeltg_cardrespadmd75 # unadjusted
effadjmodeltg_cardrespadmd75a # + seasonality and long term trends
effadjmodeltg_cardrespadmd75b # dow
effadjmodeltg_cardrespadmd75c # pm2.5

tabadjmodeltg_cardrespadm75 <- adjmodeltg_cardrespadmd75c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg_adm75 <-rbind(round(effunadjmodeltg_cardrespadmd75,2),round(effadjmodeltg_cardrespadmd75a,2),round(effadjmodeltg_cardrespadmd75b,2),round(effadjmodeltg_cardrespadmd75c,2)) [,5:7] # stacks three models and uses CI columns

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

tabadjmodeltg_cardrespadm75

tabadjmodeltg_cardrespadm75 %>% # remove intercept term from the multi-variable results
  filter(grepl("^temp_group", term)) %>% # only keep terms with temp_group
  mutate(
    term = recode(term,  "temp_group<0°C" = "<0°C", "temp_group0-4°C" = "0-4°C", "temp_group5-9°C" = "5-9°C", "temp_group10-14°C" = "10-14°C", "temp_group>20°C" = ">20°C"), # change names to more presentable categories
    term = fct_relevel(term,"<0°C", "0-4°C", "5-9°C", "10-14°C", ">20°C")) %>% #set order of levels to appear along y-axis
  ggplot(aes(x = estimate, y = term)) + # plot with variable on the y axis and estimate (OR) on the x axis
  xlim(0.8, 1.2) +
  geom_point(
    fill = "black") + # show the estimate as a point 
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +  # add in an error bar for the confidence intervals
  geom_vline(xintercept = 1, linetype = "dashed") + # show where RR = 1 is for reference as a dashed line
  theme_classic() +
  labs(
    title = "RR of emergency admissions compared to reference group 15-19°C (aged 75+)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Admissions/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig27_forestplotqpregress75_cardrespmain.svg", width=10, height=5)

rm(adjmodeltg_cardrespadmd75a,adjmodeltg_cardrespadmd75b,adjmodeltg_cardrespadmd75c,unadjmodeltg_cardrespadmd75, effadjmodeltg_cardrespadmd75a,effadjmodeltg_cardrespadmd75b,effadjmodeltg_cardrespadmd75c, effunadjmodeltg_cardrespadmd75, qpmodelcardrespadm75a,qpmodelcardrespadm75b,tabefftg_adm75,tabadjmodeltg_cardrespadm75) # remove data tables not required
rm(predc,predd,residuals75a,residuals75b) # remove objects/plots/functions not required

# 5b END