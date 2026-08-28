### Emergency admissions (adm) - All cause - Main analysis (excluding Covid-19 pandemic)

##---- Step 3.0: Link the exposure data to each time series

# Step 3.1: Load shapefiles in Birmingham and temperature data

# LSOA shapefiles boundaries

unzip("Data/Mapping/boundaries-lsoa-2021-birmingham.zip") # extracts multiple shapefiles
bhamlsoashp <- st_read("boundaries-lsoa-2021-birmingham.shp") # Load LSOA boundaries
file.remove("boundaries-lsoa-2021-birmingham.shx", "boundaries-lsoa-2021-birmingham.cpg", "boundaries-lsoa-2021-birmingham.dbf", "boundaries-lsoa-2021-birmingham.prj", "boundaries-lsoa-2021-birmingham.shp")

# Load in daily LSOA mean temperature and check formatting

daily_temp_LSOA <- as.data.table(read.csv("Data/Exposure/daily_tasmean_LSOA.csv"))

#Ensure formatting (date) is correct before joining
class(daily_temp_LSOA$date) # check formatting - it is character
class(data_adm_allcause_main_cts$date) # check formatting - it is date
daily_temp_LSOA$date <- as.Date(daily_temp_LSOA$date) # change from character to date
class(daily_temp_LSOA$date)# check formatting - it is now date

# Create daily temperature across Bham (mean of all LSOAs / day)
daily_temp_aggr <- daily_temp_LSOA[, lapply(.SD, mean), by=date, # calculation by date
                                                   .SDcols=c("tasmean")] # only use these columns for the calculation

#Ensure formatting (date) is correct before joining
class(daily_temp_aggr$date) # check formatting - it is date
class(data_adm_allcause_main_aggr$date) # check formatting - it is date

# Remove Covid-19 time period as main analysis
daily_temp_LSOA <- daily_temp_LSOA %>%
  filter(!between(date, as.Date("2020-03-01"), as.Date("2021-04-30")))
daily_temp_aggr <- daily_temp_aggr %>%
  filter(!between(date, as.Date("2020-03-01"), as.Date("2021-04-30")))

# Step 3.2: Merge LSOA mean temperature (and PM2.5) with main dataset (all cause) and create lagged environmental series by LSOA

# All cause - Join by LSOA21CD and date
data_adm_allcause_main_cts <- data_adm_allcause_main_cts %>%
  left_join(daily_temp_LSOA) # Joining with `by = join_by(LSOA21CD, date)`

# All cause - Reorder and remove additional columns from tasmean table
data_adm_allcause_main_cts <- data_adm_allcause_main_cts %>%
  dplyr::select(LSOA21CD, date, year, month, day, a04, a514, a1564, a6574, a75plus, total, doy, dow, tasmean)

# All cause - add in PM2.5 values
# Load air quality
dailypm25 <- as.data.table(read.csv("Data/Interactions/Air quality/daily_pm25.csv"))
dailypm25$Date <- as.Date(dailypm25$Date, format = "%d/%m/%Y") # change from character to date
dailypm25 <- dailypm25 %>% rename(date = Date, pm25 = Value) # change column name

# Remove Covid-19 time period as main analysis
dailypm25 <- dailypm25 %>%
  filter(!between(date, as.Date("2020-03-01"), as.Date("2021-04-30")))

# Add/merge Pm2.5 to cts dataset
data_adm_allcause_main_cts <- data_adm_allcause_main_cts %>% # Join data with 'date'
  left_join(dailypm25) # Joining with `by = join_by(date)

# Create additional columns with the lag temperatures (previous days)
data_adm_allcause_main_cts[, paste("tmean", 1:21, sep="_"):=data.table::shift(tasmean, 1:21),
                      by=LSOA21CD] # create lag variables - 1-21 days, creates extra columns

# Create 7 day temperature  average (for step preceding DLNM)
data_adm_allcause_main_cts[,tasmeanavg7:= data.table::frollmean(tasmean,n = 7,align = "right", adaptive = FALSE, na.rm = TRUE),
                      by = LSOA21CD]
data_adm_allcause_main_cts[is.na(tasmeanavg7), tasmeanavg7 := tasmean] # days 1-6 in dataset use daily temperature

# Check it has worked
head(data_adm_allcause_main_cts) # check data frame - can see the 21 added columns and 7-day average

# Step 3.3: Merge mean temperature (and PM2.5) with aggregated dataset

# Create daily temperature across Bham (mean of all LSOAs / day)
daily_temp_aggr <- daily_temp_LSOA[, lapply(.SD, mean), by=date, # calculation by date
                                                   .SDcols=c("tasmean")] # only use these columns for the calculation

#Ensure formatting (date) is correct before joining
class(daily_temp_aggr$date) # check formatting - it is date
class(data_adm_allcause_main_aggr$date) # check formatting - it is date

# Join by date
data_adm_allcause_main_aggr <- data_adm_allcause_main_aggr %>%
  left_join(daily_temp_aggr) # Joining with `by = join_by(date)

# Add/merge Pm2.5 to aggr dataset
data_adm_allcause_main_aggr <- data_adm_allcause_main_aggr %>% # Join data with 'date'
  left_join(dailypm25) # Joining with `by = join_by(date)

# Step 3b: Create lagged environmental series in the aggregated dataset

# Create additional columns
data_adm_allcause_main_aggr[, paste("tmean", 1:21, sep="_"):=data.table::shift(tasmean, 1:21)] # create lag variables - 1-21 days, creates extra columns

# Create 7 day temperature  average for aggregate dataset - for simple 7 day moving average
data_adm_allcause_main_aggr[,tasmeanavg7:= data.table::frollmean(tasmean,n = 7,align = "right", adaptive = FALSE, na.rm = TRUE),]
data_adm_allcause_main_aggr[is.na(tasmeanavg7),tasmeanavg7 := tasmean] # for the first 6 rows, use tasmean

# 3.0 END