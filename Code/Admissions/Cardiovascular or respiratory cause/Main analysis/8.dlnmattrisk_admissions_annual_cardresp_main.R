### Emergency admissions (adm) - Cardio-respiratory cause (secondary outcome) - Main analysis (excluding Covid-19 pandemic)

##---- Step 8.0: Calculate attributable risk

# Load attrdl.R function
source("attrdl.R")
set.seed(72)

# Sum of total admissions during the analysis period
sum(data_adm_cardresp_main_aggr$total)

# Temperature percentiles
data_adm_cardresp_main_aggr %>%
  summarise(p025 = quantile(tasmean, 0.025, na.rm = TRUE), # 2.5 percentile
            p25 = quantile(tasmean, 0.25, na.rm = TRUE),
            p50 = quantile(tasmean, 0.50, na.rm = TRUE),
            p75 = quantile(tasmean, 0.75, na.rm = TRUE),
            p975 = quantile(tasmean, 0.975, na.rm = TRUE)) # 97.5 percentile

# Attributable risk
overallAN <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,type="an",cen=20)
overallAR <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20)*100

overallAN

# Obtain confidence intervals using Monte Carlo simulation method
overalleci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20)*100,c(0.025,0.975))

# Attributable fraction (cold)
coldAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(-6, 14.99))*100
coldeci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(-6, 14.99))*100,c(0.025,0.975))

# Attributable fraction (all heat)
heatAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(15.01,30))*100
heateci <-quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(15.01,30))*100,c(0.025,0.975))

# Attributable fraction (mild cold)
mildcoldAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(1.38, 14.99))*100
mildcoldeci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(1.38, 14.99))*100,c(0.025,0.975))

# Attributable fraction (mild heat)
mildheatAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(15.01,20.6))*100
mildheateci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(15.01,20.6))*100,c(0.025,0.975))

# Attributable fraction (extreme cold) defined as temperatures below the 2.5th percentile (1.37°C)
extcoldAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(-6, 1.37))*100
extcoldeci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(-6, 1.37))*100,c(0.025,0.975))

# Attributable fraction (extreme heat) - defined as temperatures above the 97.5th percentile (20.7°C)
extheatAF <- attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,cen=20,range=c(20.7,30))*100
extheateci <- quantile(attrdl(data_adm_cardresp_main_aggr$tasmean,cbtmeanaggr,data_adm_cardresp_main_aggr$total,full_model_adm_aggr,sim=T,nsim=5000,cen=20,range=c(20.7,30))*100,c(0.025,0.975))

# Create table using tibble and objects above
tableattributableriskcardrespadm <- tibble(
  Exposure = c("All non-optimal temperatures", "Cold", "Heat", "Mild cold", "Mild heat", "Extreme cold", "Extreme heat"),
  `Attributable fraction (%)` = c(overallAR, coldAF, heatAF, mildcoldAF, mildheatAF, extcoldAF, extheatAF),
  `Lower empirical confidence interval (LeCI)` = c(overalleci[1], coldeci[1], heateci[1], mildcoldeci[1], mildheateci[1], extcoldeci[1], extheateci[1]),
  `Higher empirical confidence interval (HeCI)` = c(overalleci[2], coldeci[2], heateci[2], mildcoldeci[2], mildheateci[2], extcoldeci[2], extheateci[2]))

# Create flextable
tableattributableriskcardrespadmfinal <- tableattributableriskcardrespadm %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  flextable()

tableattributableriskcardrespadmfinal # View table

# Save the table
save_as_image(tableattributableriskcardrespadmfinal, "Outputs/Admissions/Cardresp cause/Main/3. Main model (DLNM)/fig38_cardrespadmmain_AFtable.svg")

# 8.0 END