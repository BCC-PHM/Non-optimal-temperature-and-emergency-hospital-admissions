### Emergency admissions (adm) - Cardiovascular or respiratory cause (secondary outcome) - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 7.0 Age group analyses

# Step 7.1: Age 0-4 years

# Run the model (0-4) using the cts dataset
fullctsmodel_cardrespadmsen_a04 <- gnm(a04 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                                       eliminate=stratum,
                                       data=data_adm_cardresp_sen_cts,
                                       family=quasipoisson,
                                       subset=keep)

# Run the model (0-4) using the aggregated dataset
fullaggrmodel_cardrespadmsen_a04 <- gnm(a04 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                                        data=data_adm_cardresp_sen_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and model
cpfulla04 <- crosspred(cbtmean, fullctsmodel_cardrespadmsen_a04, cen=15)
cpaggra04 <- crosspred(cbtmeanaggr, fullaggrmodel_cardrespadmsen_a04, cen=15)

# Plot settings
col <- c("darkorange3", "darkblue") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model (sensitivity analysis) using dlnm approach
plot(cpfulla04, "overall", ylim=c(0.4,2.0), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggra04, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 0-4)", "Aggregated Time Series (aged 0-4)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Sensitivity analysis/3. Main model (DLNM)/fig16_cardresp_sen_ctsaggr04_plotadm.svg", width=7, height=4)

admsenfreqplota04 # view plot

rm(cpaggra04, cpfulla04, fullctsmodel_cardrespadmsen_a04,fullaggrmodel_cardrespadmsen_a04) # remove objects/plots/functions not required

# Step 7.2: Age 5-14 years

# Run the model (5-14) using the cts dataset
fullctsmodel_cardrespadmsen_a514 <- gnm(a514 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                                        eliminate=stratum,
                                        data=data_adm_cardresp_sen_cts,
                                        family=quasipoisson,
                                        subset=keep)

# Run the model (5-14) using the aggregated dataset
fullaggrmodel_cardrespadmsen_a514 <- gnm(a514 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                                         data=data_adm_cardresp_sen_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and model
cpfulla514 <- crosspred(cbtmean, fullctsmodel_cardrespadmsen_a514, cen=15)
cpaggra514 <- crosspred(cbtmeanaggr, fullaggrmodel_cardrespadmsen_a514, cen=15)

# Plot settings
col <- c("darkorange3", "darkblue") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model (sensitivity analysis) using dlnm approach
plot(cpfulla514, "overall", ylim=c(0.4,2.0), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggra514, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 5-14)", "Aggregated Time Series (aged 5-14)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Sensitivity analysis/3. Main model (DLNM)/fig17_cardresp_sen_ctsaggra514_plotadm.svg", width=7, height=4)

admsenfreqplota514 # view plot

rm(cpaggra514, cpfulla514, fullctsmodel_cardrespadmsen_a514,fullaggrmodel_cardrespadmsen_a514) # remove objects/plots/functions not required

# Step 7.3: Age 15-64 years

# Run the model (15-64) using the cts dataset
fullctsmodel_cardrespadmsen_a1564 <- gnm(a1564 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                                         eliminate=stratum,
                                         data=data_adm_cardresp_sen_cts,
                                         family=quasipoisson,
                                         subset=keep)

# Run the model (15-64) using the aggregated dataset
fullaggrmodel_cardrespadmsen_a1564 <- gnm(a1564 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                                          data=data_adm_cardresp_sen_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and model
cpfulla1564<- crosspred(cbtmean, fullctsmodel_cardrespadmsen_a1564, cen=15)
cpaggra1564 <- crosspred(cbtmeanaggr, fullaggrmodel_cardrespadmsen_a1564, cen=15)

# Plot settings
col <- c("darkorange3", "darkblue") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model (sensitivity analysis) using dlnm approach
plot(cpfulla1564, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggra1564, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 15-64)", "Aggregated Time Series (aged 15-64)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Sensitivity analysis/3. Main model (DLNM)/fig18_cardresp_sen_ctsaggr1564_plotadm.svg", width=7, height=4)

admsenfreqplota1564 # view plot

rm(cpaggra1564, cpfulla1564, fullctsmodel_cardrespadmsen_a1564,fullaggrmodel_cardrespadmsen_a1564) # remove objects/plots/functions not required

# Step 7.4: Age 65-74 years

# Run the model (65-74) using the cts dataset
fullctsmodel_cardrespadmsen_a6574 <- gnm(a6574 ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                                         eliminate=stratum,
                                         data=data_adm_cardresp_sen_cts,
                                         family=quasipoisson,
                                         subset=keep)

# Run the model (65-74) using the aggregated dataset
fullaggrmodel_cardrespadmsen_a6574 <- gnm(a6574 ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                                          data=data_adm_cardresp_sen_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and model
cpfulla6574 <- crosspred(cbtmean, fullctsmodel_cardrespadmsen_a6574, cen=15)
cpaggra6574 <- crosspred(cbtmeanaggr, fullaggrmodel_cardrespadmsen_a6574, cen=15)

# Plot settings
col <- c("darkorange3", "darkblue") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model (sensitivity analysis) using dlnm approach
plot(cpfulla6574, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggra6574, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 65-74)", "Aggregated Time Series (aged 65-74)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Sensitivity analysis/3. Main model (DLNM)/fig19_cardresp_sen_ctsaggra6574_plotadm.svg", width=7, height=4)

admsenfreqplota6574 # view plot

rm(cpaggra6574, cpfulla6574, fullctsmodel_cardrespadmsen_a6574,fullaggrmodel_cardrespadmsen_a6574) # remove objects/plots/functions not required

# Step 7.5 Age 75+ years

# Run the model (75+) using the cts dataset
fullctsmodel_cardrespadmsen_a75 <- gnm(a75plus ~ cbtmean + splinedayofyear:factor(year) + factor(dow) + pm25, 
                                       eliminate=stratum,
                                       data=data_adm_cardresp_sen_cts,
                                       family=quasipoisson,
                                       subset=keep)

# Run the model (75+) using the aggregated dataset
fullaggrmodel_cardrespadmsen_a75 <- gnm(a75plus ~ cbtmeanaggr + ns(doy, df=3):factor(year) + factor(dow) + pm25,
                                        data=data_adm_cardresp_sen_aggr, family=quasipoisson)

# Predict and plot cumulative exposure response curves

# Predict for CTS and model
cpfulla75 <- crosspred(cbtmean, fullctsmodel_cardrespadmsen_a75, cen=15)
cpaggra75 <- crosspred(cbtmeanaggr, fullaggrmodel_cardrespadmsen_a75, cen=15)

# Plot settings
col <- c("darkorange3", "darkblue") # colours
parold <- par(no.readonly=T) # save settings
par(mar=c(4,4,1,0.5), las=1, mgp=c(2.5,1,0)) # plot layout

# Plot full main model (sensitivity analysis) using dlnm approach
plot(cpfulla75, "overall", ylim=c(0.8,1.4), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
lines(cpaggra75, "overall", ci="area",col=col[2],  lwd=1.5,
      ci.arg=list(col=alpha(col[2], 0.2)))

# Add legend to plot
legend("top", c("Case Time Series (aged 75+)", "Aggregated Time Series (aged 75+)"), lty=1, lwd=1.5, col=col, bty="n",
       inset=0.05, y.intersp=2, cex=0.8)
par(parold) # restore original plot settings

# Save the plot
dev.print(svg, file="Outputs/Admissions/Cardresp cause/Sensitivity analysis/3. Main model (DLNM)/fig20_cardresp_sen_ctsaggr75_plotadm.svg", width=7, height=4)

rm(cpaggra75,cpfulla75, fullctsmodel_cardrespadmsen_a75, fullaggrmodel_cardrespadmsen_a75) # remove data tables not required

# 7.0 END