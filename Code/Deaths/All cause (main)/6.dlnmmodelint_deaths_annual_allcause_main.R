### Deaths - All cause - Main analysis (excluding Covid-19 pandemic)

##---- Step 6.0: Main model and interactions (IMD)

# Step 6.1: Run the model using the case time series dataset

# Use splines for day of the year
splinedayofyear <- onebasis(data_deaths_allcause_main_cts$doy, "ns", df=3) # to capture within year seasonal trends

# Define cross-basis for temperature from exposure history matrix
# Use group to identify lack of continuity in series by LSOA and year
cb_temp <- list(fun="ns", knots=quantile(data_deaths_allcause_main_cts$tasmean, c(10,50,90)/100, na.rm=TRUE))
cb_lag <- list(fun="ns", df=5)
group <- factor(paste(data_deaths_allcause_main_cts$LSOA21CD, data_deaths_allcause_main_cts$period, sep="-"))
cbtmean <- crossbasis(data_deaths_allcause_main_cts$tasmean, lag=14, argvar=cb_temp, arglag=cb_lag, group=group)

summary(cbtmean) # summary of cross-basis

# Define the strata - LSOA, year, month 
data_deaths_allcause_main_cts[, stratum:=factor(paste(LSOA21CD, year, month, sep=":"))] # to compare days within the same LSOA and same month and year

# Run the model, including empty strata (otherwise bias in gnm with quasipoisson)
data_deaths_allcause_main_cts[,  keep:=sum(total)>0, by=stratum]
full_model <- gnm(total ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                  eliminate=stratum,
                  data=data_deaths_allcause_main_cts,
                  family=quasipoisson,
                  subset=keep)

# Step 6.2: Run the model using the aggregated dataset

# Re-define cross-basis, similar approach but use group for years
cbtmeanaggr <- crossbasis(data_deaths_allcause_main_aggr$tasmean, lag=14, argvar=cb_temp, arglag=cb_lag,
                          group=data_deaths_allcause_main_aggr$year)

# Run the model
full_model_aggr <- gnm(total ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                       data=data_deaths_allcause_main_aggr, family=quasipoisson)

# Step 6.3: Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
cpfull <- crosspred(cbtmean, full_model, cen=15)
cpaggr <- crosspred(cbtmeanaggr, full_model_aggr, cen=15)
rm(full_model)

# Plot settings
col <- c("darkgoldenrod3", "aquamarine3") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
fullmainplot <- plot(cpfull, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
                     xlab=expression(paste("Temperature ("*degree,"C)")), 
                     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggr, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case TS", "Aggregated TS"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Deaths/All cause/Main/3. Main model (DLNM)/fig29_allcausedeaths_main_ctsaggr_plot.svg", width=7, height=4)

# Step 6.4: Load IMD data by LSOA and merge with main model

# Load IMD (2019) data
bhamlsoaimd <- read.csv("Data/Interactions/bhamlsoaimd2019.csv")

# Use LSOA11 to LSOA21 lookup to add LSOA21 to IMD table
bhamlsoa21imd <- bhamlsoaimd %>% # Join data with the lookup
  left_join(lsoalookup) %>% # Joining with `by = join_by(LSOA11CD)`
  group_by(LSOA11CD) %>% # group by the original geography (LSOA11)
  mutate(weight = 1 / n_distinct(LSOA21CD)) %>% # Creates new column with value based on how many LSOAs (21) compare to original (11)
  ungroup() %>% # to ensure next step applies to whole dataset
  mutate(across(c(imdscore, imdrank, indoorssubscore), ~ .x * weight)) %>% # apply weight to all the (relevant) columns
  group_by(LSOA21CD) %>% # group by the final columns I want to include
  summarise(across(c(imdscore, imdrank, indoorssubscore), mean)) %>% # combine rows so we have one LSOA for each date
  ungroup()

# Join by LSOA
data_deaths_allcause_main_cts <- data_deaths_allcause_main_cts %>%
  left_join( bhamlsoa21imd) # Joining with `by = join_by(LSOA21)

rm(bhamlsoaimd,bhamlsoashp)

# Reorder and remove additional columns from cts IMD table
data_deaths_allcause_main_cts <- data_deaths_allcause_main_cts %>%
  dplyr::select(-imdrank)

# Step 6.5: Define interaction cross-bases and run model, testing significance (IMD score)

# Define interaction cross-bases and create percentiles (low)
intval <- quantile(bhamlsoa21imd$imdscore, c(0.2, 0.8))
cbint1 <- cbtmean * (data_deaths_allcause_main_cts$imdscore - intval[1])

# Run the model
modint1 <- gnm(total ~ cbtmean + splinedayofyear:factor(year) + factor(dow)+ pm25 + cbint1, 
               eliminate=stratum,
               data=data_deaths_allcause_main_cts,
               family=quasipoisson,
               subset=keep)

rm(cbint1)

# Predict exposure-responsive curve for low IMD
cpint1 <- crosspred(cbtmean, modint1, cen=15)

# Define interaction cross-bases and create percentiles (high)
cbint2<- cbtmean * (data_deaths_allcause_main_cts$imdscore - intval[2])

modint2 <- gnm(total ~ cbtmean + splinedayofyear:factor(year) + factor(dow)+ pm25 + cbint2, 
               eliminate=stratum,
               data=data_deaths_allcause_main_cts,
               family=quasipoisson,
               subset=keep)

rm(cbint2)

# Predict exposure-responsive curve for high IMD
cpint2 <- crosspred(cbtmean, modint2, cen=15)

# Test statistical significance
# anova(full_model, modint1, test = "Chisq") # To compare the two models - likelyhood ratio / chi sq test to test the null hypothesis
# p = 0.2462 which means it there is no statistically significant difference between temperature and deaths and temperature x IMD and deaths. IMD not having an impact.

# Step 6.6: Plot the curves and save  (IMD score)

# Plot curve
col <- c("royalblue", "tomato3")
parold <- par(no.readonly=T)
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0))
plot(cpint1, "overall", ylim=c(0.7,1.5), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpint2, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))
legend("top", c("Low IMD score", "High IMD score"), lty=1, lwd=1.5, col=col,
       bty="n", inset=0.05, y.intersp=2, cex=0.8)
par(parold)

# Save the plot
dev.print(svg, file="Outputs/Deaths/All cause/Main/3. Main model (DLNM)/fig30_bhamimdplotallcausedeaths.svg", width=7, height=4)

rm(modint1,modint2)

# Step 6.7: Define interaction cross-bases and run model, testing significance (IMD sub-domain score)

# Define interaction cross-bases and create percentiles (low)
intvalsubdomain <- quantile(bhamlsoa21imd$indoorssubscore, c(0.2, 0.8))
cbint3 <- cbtmean * (data_deaths_allcause_main_cts$indoorssubscore - intvalsubdomain[1])

# Run the model
modint3 <- gnm(total ~ cbtmean + splinedayofyear:factor(year) + factor(dow)+ pm25 + cbint3, 
               eliminate=stratum,
               data=data_deaths_allcause_main_cts,
               family=quasipoisson,
               subset=keep)
rm(cbint3)

# Predict exposure-responsive curve for low IMD
cpint3 <- crosspred(cbtmean, modint3, cen=15)

# Define interaction cross-bases and create percentiles (high)
cbint4 <- cbtmean * (data_deaths_allcause_main_cts$indoorssubscore - intvalsubdomain[2])

modint4 <- gnm(total ~ cbtmean + splinedayofyear:factor(year) + factor(dow)+ pm25 + cbint4, 
               eliminate=stratum,
               data=data_deaths_allcause_main_cts,
               family=quasipoisson,
               subset=keep)

rm(cbint4)

# Predict exposure-responsive curve for high IMD
cpint4 <- crosspred(cbtmean, modint4, cen=15)

# Test statistical significance
# anova(full_model, modint3, test="Chisq") # To compare the two models - likelyhood ratio / chi sq test to test the null hypothesis

# Step 6.8: Plot the curves and save  (IMD sub-domain score))

# Plot curve
col <- c("royalblue", "tomato3")
parold <- par(no.readonly=T)
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0))
plot(cpint3, "overall", ylim=c(0.7,1.5), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpint4, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))
legend("top", c("Low IMD sub domain score", "High IMD sub domain score"), lty=1, lwd=1.5, col=col,
       bty="n", inset=0.05, y.intersp=2, cex=0.8)
par(parold)

# Save the plot
dev.print(svg, file="Outputs/Deaths/All cause/Main/3. Main model (DLNM)/fig31_bhamsimdsubdomainplotallcausedeaths.svg", width=7, height=4)

rm(cpfull,cpint1, cpint2, cpint3, cpint4,modint3,modint4,bhamlsoa21imd) # remove data tables not required
rm(intval,intvalsubdomain) # # remove objects/plots/functions not required

# 6.0 END