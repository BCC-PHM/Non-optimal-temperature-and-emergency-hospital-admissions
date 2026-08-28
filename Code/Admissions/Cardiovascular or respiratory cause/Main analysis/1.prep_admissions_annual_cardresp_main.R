### Emergency admissions (adm) - Cardiorespiratory cause (secondary outcome) - Main analysis (excluding Covid-19 pandemic)

##---- Step 1.0: Load required packages and set working directory

rm(list = ls()) # clear environment

library(data.table) # for large datasets
library(dlnm) # dlnm package
library(gnm) # generalised non-linear model
library(splines) # splines to be able to smooth non-linear data
library(sf) # handles shape files
library(dplyr) # data management
library(tidyr) # data management
library(ggplot2) # for plotting
library(patchwork) # combines multiple ggplot plots
library(flextable) # designing tables
library(lubridate) # working with date and time
library(forcats) # categorical variables
library(rmarkdown) # to create report
library(gtsummary) # for tables following regression
library(Epi) # for statistics

# 1.0 END