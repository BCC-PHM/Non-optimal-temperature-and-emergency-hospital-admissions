
### Deaths - Cardio or respiratory cause - Main analysis period (excluding Covid-19 pandemic)

##---- Step 7.0 Age group analyses

# 0-4 and 5-14 removed due to low counts affecting process

# Step 7.1: Age 0-4 years

sum(data_deaths_cardresp_main_aggr$d04)

# Not included due to very small counts

# Run the model (0-4) using the cts dataset
# fullctsmodeld04 <- gnm(d04 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
#                       eliminate=stratum,
#                       data=data_deaths_cardresp_main_cts,
#                       family=quasipoisson,
#                       subset=keep)

# Run the model (0-4) using the aggregated dataset
# fullaggrmodeld04 <- gnm(d04 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
#                        data=data_deaths_cardresp_main_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
# cpfulld04 <- crosspred(cbtmean, fullctsmodeld04, cen=15)
# cpaggrd04 <- crosspred(cbtmeanaggr, fullaggrmodeld04, cen=15)

# Plot settings
# col <- c("darkgoldenrod3", "aquamarine3") # colours
# parold <- par(no.readonly=T) # save settings
# par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
#plot(cpfulld04, "overall", ylim=c(0.4,1.6), ylab="RR", col=col[1], lwd=1.5,
#     xlab=expression(paste("Temperature ("*degree,"C)")), 
#     ci.arg=list(col=alpha(col[1], 0.2)))
# lines(cpaggrd04, "overall", ci="area",col=col[2],  lwd=1.5,
#      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
#legend("top", c("Case Time Series (aged 0-4)", "Aggregated Time Series (aged 0-4)"), lty=1, lwd=1.5, col=col, bty="n",
#       inset=0.05, y.intersp=2, cex=0.8)
# par(parold) # restore original plot settings

# Save the plot
# dev.print(svg, file="Outputs/Deaths/Cardresp cause/Main/3. Main model (DLNM)/figX_cardrespdeaths_main_ctsaggr04_plot.svg", width=7, height=4)

# cardrespdeathsfreqplotd04 # view plot

# rm(cpaggrd04, cpfulld04, fullctsmodeld04,fullaggrmodeld04) # remove objects/plots/functions not required

# Step 7.2: Age 5-14 years

# Run the model (5-14) using the cts dataset
# fullctsmodeld514 <- gnm(d514 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
#                        eliminate=stratum,
#                        data=data_deaths_cardresp_main_cts,
#                        family=quasipoisson,
#                        subset=keep)

# Run the model (5-14) using the aggregated dataset
# fullaggrmodeld514 <- gnm(d514 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
#                         data=data_deaths_cardresp_main_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
#cpfulld514 <- crosspred(cbtmean, fullctsmodeld514, cen=15)
#cpaggrd514 <- crosspred(cbtmeanaggr, fullaggrmodeld514, cen=15)

# Plot settings
#col <- c("darkgoldenrod3", "aquamarine3") # colours
#parold <- par(no.readonly=T) # save settings
#par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
#plot(cpfulld514, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
#     xlab=expression(paste("Temperature ("*degree,"C)")), 
#     ci.arg=list(col=alpha(col[1], 0.2)))
#lines(cpaggrd514, "overall", ci="area",col=col[2],  lwd=1.5,
#      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
#legend("top", c("Case Time Series (aged 5-14)", "Aggregated Time Series (aged 5-14)"), lty=1, lwd=1.5, col=col, bty="n",
#       inset=0.05, y.intersp=2, cex=0.8)
#par(parold) # restore original plot settings

# Save the plot
#dev.print(svg, file="Outputs/Deaths/Cardresp cause/Main/3. Main model (DLNM)/fig32_cardrespdeaths_main_ctsaggrd514_plot.svg", width=7, height=4)

# rm(cpaggrd514, cpfulld514, fullctsmodeld514,fullaggrmodeld514) # remove objects/plots/functions not required

# Step 7.3: Age 15-64 years

# Run the model (15-64) using the cts dataset
fullctsmodeld1564 <- gnm(d1564 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                         eliminate=stratum,
                         data=data_deaths_cardresp_main_cts,
                         family=quasipoisson,
                         subset=keep)

# Run the model (15-64) using the aggregated dataset
fullaggrmodeld1564 <- gnm(d1564 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                          data=data_deaths_cardresp_main_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
cpfulld1564<- crosspred(cbtmean, fullctsmodeld1564, cen=15)
cpaggrd1564 <- crosspred(cbtmeanaggr, fullaggrmodeld1564, cen=15)

# Plot settings
col <- c("darkgoldenrod3", "aquamarine3") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
plot(cpfulld1564, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggrd1564, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 15-64)", "Aggregated Time Series (aged 15-64)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Deaths/Cardresp cause/Main/3. Main model (DLNM)/fig33_cardrespdeaths_main_ctsaggr1564_plot.svg", width=7, height=4)

cardrespdeathsfreqplotd1564 # view plot

rm(cpaggrd1564, cpfulld1564, fullctsmodeld1564,fullaggrmodeld1564) # remove objects/plots/functions not required

# Step 7.4: Age 65-74 years

# Run the model (65-74) using the cts dataset
fullctsmodeld6574 <- gnm(d6574 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                         eliminate=stratum,
                         data=data_deaths_cardresp_main_cts,
                         family=quasipoisson,
                         subset=keep)

# Run the model (65-74) using the aggregated dataset
fullaggrmodeld6574 <- gnm(d6574 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                          data=data_deaths_cardresp_main_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
cpfulld6574 <- crosspred(cbtmean, fullctsmodeld6574, cen=15)
cpaggrd6574 <- crosspred(cbtmeanaggr, fullaggrmodeld6574, cen=15)

# Plot settings
col <- c("darkgoldenrod3", "aquamarine3") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
plot(cpfulld6574, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggrd6574, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 65-74)", "Aggregated Time Series (aged 65-74)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Deaths/Cardresp cause/Main/3. Main model (DLNM)/fig34_cardrespdeaths_main_ctsaggr6574_plot.svg", width=7, height=4)

cardrespdeathsfreqplotd6574 # view plot

rm(cpaggrd6574, cpfulld6574, fullctsmodeld6574,fullaggrmodeld6574) # remove objects/plots/functions not required

# Step 7.5 Age 75+ years

# Run the model (75+) using the cts dataset
fullctsmodeld75 <- gnm(d75plus ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                       eliminate=stratum,
                       data=data_deaths_cardresp_main_cts,
                       family=quasipoisson,
                       subset=keep)

# Run the model (75+) using the aggregated dataset
fullaggrmodeld75 <- gnm(d75plus ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                        data=data_deaths_cardresp_main_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and then aggr model
cpfulld75 <- crosspred(cbtmean, fullctsmodeld75, cen=15)
cpaggrd75 <- crosspred(cbtmeanaggr, fullaggrmodeld75, cen=15)

# Plot settings
col <- c("darkgoldenrod3", "aquamarine3") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model using dlnm
plot(cpfulld75, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggrd75, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 75+)", "Aggregated Time Series (aged 75+)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Deaths/Cardresp cause/Main/3. Main model (DLNM)/fig35_cardrespdeaths_main_ctsaggr75_plot.svg", width=7, height=4)

rm(cpaggrd75,cpfulld75, fullctsmodeld75, fullaggrmodeld75) # remove data tables not required

# 7.0 END