### Deaths - Cardio or respiratory cause - Main analysis period (excluding Covid-19 pandemic)

##---- Step 5b Quasi-poisson model (not DLNM) - Age groups

# Uses splines for non-linear relationship (and temperature as categorical variable)

# AGE 0-74 YEARS

data_deaths_cardresp_main_aggr$d074 <- data_deaths_cardresp_main_aggr$total-data_deaths_cardresp_main_aggr$d75plus
data_deaths_cardresp_main_aggr_tempgroup$d074 <- data_deaths_cardresp_main_aggr_tempgroup$total-data_deaths_cardresp_main_aggr_tempgroup$d75plus

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel074a <- glm(d074 ~ spline,data_deaths_cardresp_main_aggr,family=quasipoisson)
summary(qpmodel074a)

# Predict number of deaths from qpmodel074a
preda <- predict(qpmodel074a,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to determine whether the spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=d074),colour = "black",size = 0.6) +
  geom_line(aes(y=preda),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of deaths (aged 0-74)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig19_splinemodel_seasonality_trends074_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals074a <- residuals(qpmodel074a,type="response")
ggplot(data_deaths_cardresp_main_aggr, aes(x=date,residuals074a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in deaths after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig20_resvar074a_main.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel074b <- glm(d074 ~ spline + factor(dow), data_deaths_cardresp_main_aggr, family=quasipoisson)
summary(qpmodel074b)

# Predict number of deaths from qpmodel074b
predb <- predict(qpmodel074b,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to see if dow + spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=d074),colour = "black", size = 0.6) +
  geom_line(aes(y=predb), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 0-74)",
       x = "Date",
       y = "Number of deaths") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig21_spline+dow074_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals074b <- residuals(qpmodel074b,type="response")
ggplot(data_deaths_cardresp_main_aggr,
       aes(x=date,residuals074b))+
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(title = "Residual variation in deaths (aged 0-74) after adjusting for seasonality, long term trends and day of week",
       x = "Date",
       y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig22_resvardow2074_main.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d074 deaths
unadjmodeltgd074 <- glm(d074 ~ temp_group, data_deaths_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltgd074)

# Exponentiate to get RR from log function
effunadjmodeltgd074 <- ci.lin(unadjmodeltgd074, subset= "temp_group", Exp=T)
effunadjmodeltgd074 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltgd074a <- glm(d074 ~ temp_group + spline,data_deaths_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltgd074a)

# Exponentiate to get RR from log function
effadjmodeltgd074a <- ci.lin(adjmodeltgd074a, subset= "temp_group", Exp=T)
effadjmodeltgd074a # view

# Adjusted model (2) to include spline + day of week

adjmodeltgd074b <- glm(d074 ~ temp_group + spline + dow, data_deaths_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltgd074b)

# Exponentiate to get RR from log function
effadjmodeltgd074b <- ci.lin(adjmodeltgd074b,
                             subset= "temp_group",
                             Exp=T)
effadjmodeltgd074b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltgd074c <- glm(d074 ~ temp_group + spline + dow + pm25, data_deaths_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltgd074c)

# Exponentiate to get RR from log function
effadjmodeltgd074c <- ci.lin(adjmodeltgd074c, subset= "temp_group", Exp=T)
effadjmodeltgd074c # view

effunadjmodeltgd074 # unadjusted
effadjmodeltgd074a # + seasonality and long term trends
effadjmodeltgd074b # dow
effadjmodeltgd074c # pm2.5

tabadjmodeltg074 <- adjmodeltgd074c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg074 <-rbind(round(effunadjmodeltgd074,2),round(effadjmodeltgd074a,2),round(effadjmodeltgd074b,2),round(effadjmodeltgd074c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg074) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg074) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg074 ## View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg074

tabadjmodeltg074 %>% # remove intercept term from the multi-variable results
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
    title = "RR of mortality compared to reference group 15-19°C (aged 0-74)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig23_forestplotqpregress74_cardrespdeathsmain.svg", width=10, height=5)

rm(adjmodeltgd074a,adjmodeltgd074b,adjmodeltgd074c,unadjmodeltgd074, effadjmodeltgd074a,effadjmodeltgd074b,effadjmodeltgd074c, effunadjmodeltgd074, qpmodel074a,qpmodel074b,tabefftg074,tabadjmodeltg074) # remove data tables not required
rm(preda,predb,residuals074a,residuals074b) # remove objects/plots/functions not required


# AGE 75+ YEARS

# Fit quasi-poisson regression model (quasi-poisson for overdispersion - variance > mean)
qpmodel75a <- glm(d75plus ~ spline,data_deaths_cardresp_main_aggr,family=quasipoisson)
summary(qpmodel75a)

# Predict number of deaths from qpmodel75a
predc <- predict(qpmodel75a,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to determine whether the spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=d75plus),colour = "black",size = 0.6) +
  geom_line(aes(y=predc),linewidth = 1) +
  labs(title ="Natural spline to model seasonality and long term trends in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of deaths (aged 75+)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig24_splinemodel_seasonality_trends75_main.svg", width=10, height=5)

# Plot response residuals from model (observed - expected)
residuals75a <- residuals(qpmodel75a,type="response")
ggplot(data_deaths_cardresp_main_aggr, aes(x=date,residuals75a)) +
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7) +
  labs(title = "Residual variation in deaths after adjusting for seasonality and long term trends",
       x = "Date",
       y = "Residual variation (Actual-Fitted)") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig25_resvar75a_main.svg", width=10, height=5)

# Fit second quasi-poisson regression model and include dow
qpmodel75b <- glm(d75plus ~ spline + factor(dow), data_deaths_cardresp_main_aggr, family=quasipoisson)
summary(qpmodel75b)

# Predict number of deaths from qpmodel75b
predd <- predict(qpmodel75b,type="response") # expected deaths for each date in dataset

# Plot observed and predicted deaths to see if dow + spline captures the deaths pattern
# The curve should follow roughly without trying to follow every single value
ggplot(data_deaths_cardresp_main_aggr, aes(x=date)) +
  geom_point(aes(y=d75plus),colour = "black", size = 0.6) +
  geom_line(aes(y=predd), linewidth = 1) +
  labs(title ="Natural spline to model seasonality, long term trends and day of week in Birmingham (aged 75+)",
       x = "Date",
       y = "Number of deaths") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig26_spline+dow75_main.svg", width=10, height=5)

# Plot response residuals from model and generate residuals (observed - expected)
residuals75b <- residuals(qpmodel75b,type="response")
ggplot(data_deaths_cardresp_main_aggr,
       aes(x=date,residuals75b))+
  geom_point(colour = "black",size = 0.6) +
  geom_hline(yintercept = 0, # 0 is the reference
             linetype = "dashed",
             linewidth = 0.7)+
  labs(title = "Residual variation in deaths (aged 75+) after adjusting for seasonality, long term trends and day of week",
       x = "Date",
       y = "Residual variation") +
  theme_classic()

# Save the plot
ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig27_resvardow275_main.svg", width=10, height=5)

# 5.2 Exposure/temperature as a categorical variable - unadjusted and adjusted model

# Unadjusted model - only temperature (categorical) and d75 deaths
unadjmodeltgd75 <- glm(d75plus ~ temp_group, data_deaths_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(unadjmodeltgd75)

# Exponentiate to get RR from log function
effunadjmodeltgd75 <- ci.lin(unadjmodeltgd75, subset= "temp_group", Exp=T)
effunadjmodeltgd75 # view

# Adjusted model (1) to include spline (seasonality and long term trends)
adjmodeltgd75a <- glm(d75plus ~ temp_group + spline,data_deaths_cardresp_main_aggr_tempgroup,family = quasipoisson())
summary(adjmodeltgd75a)

# Exponentiate to get RR from log function
effadjmodeltgd75a <- ci.lin(adjmodeltgd75a, subset= "temp_group", Exp=T)
effadjmodeltgd75a # view

# Adjusted model (2) to include spline + day of week

adjmodeltgd75b <- glm(d75plus ~ temp_group + spline + dow, data_deaths_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltgd75b)

# Exponentiate to get RR from log function
effadjmodeltgd75b <- ci.lin(adjmodeltgd75b,
                            subset= "temp_group",
                            Exp=T)
effadjmodeltgd75b # view

# Adjusted model (3) to include spline + day of week + PM2.5

adjmodeltgd75c <- glm(d75plus ~ temp_group + spline + dow + pm25, data_deaths_cardresp_main_aggr_tempgroup, family = quasipoisson())
summary(adjmodeltgd75c)

# Exponentiate to get RR from log function
effadjmodeltgd75c <- ci.lin(adjmodeltgd75c, subset= "temp_group", Exp=T)
effadjmodeltgd75c # view

effunadjmodeltgd75 # unadjusted
effadjmodeltgd75a # + seasonality and long term trends
effadjmodeltgd75b # dow
effadjmodeltgd75c # pm2.5

tabadjmodeltg75 <- adjmodeltgd75c %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))  ## round to 2 decimal places

# Summary table (temperature as categorical variable) showing relative risks compared with the reference category
tabefftg75 <-rbind(round(effunadjmodeltgd75,2),round(effadjmodeltgd75a,2),round(effadjmodeltgd75b,2),round(effadjmodeltgd75c,2)) [,5:7] # stacks three models and uses CI columns

dimnames(tabefftg75) <-list(c("Unadjusted <0°C",
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
colnames(tabefftg75) <- c("Relative Risk","Lower Confidence Interval","Upper Confidence Interval") # column names

tabefftg75 # View

# Forest plot to display regression results - model which adjusts for seasonality, long term trends and day of week + pm2.5

tabadjmodeltg75

tabadjmodeltg75 %>% # remove intercept term from the multi-variable results
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
    title = "RR of mortality compared to reference group 15-19°C (aged 75+)",
    x = "Relative Risk",
    y = "Temperature group (categorical)")

ggsave("Outputs/Deaths/Cardresp cause/Main/2. Quasi-poisson model (before DLNM)/fig28_forestplotqpregress75_cardrespdeathsmain.svg", width=10, height=5)

rm(adjmodeltgd75a,adjmodeltgd75b,adjmodeltgd75c,unadjmodeltgd75, effadjmodeltgd75a,effadjmodeltgd75b,effadjmodeltgd75c, effunadjmodeltgd75, qpmodel75a,qpmodel75b,tabefftg75,tabadjmodeltg75) # remove data tables not required
rm(predc,predd,residuals75a,residuals75b) # remove objects/plots/functions not required

# 5b END
