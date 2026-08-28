### Emergency admissions (adm) - All cause - Sensitivity analysis (including Covid-19 pandemic)

##---- Step 2.0: Prepare time series data - All cause admissions - Sensitivity analysis (including Covid-19 pandemic)

# 2.1: Load admissions data, format and sort

# Load data from SQL Server output (csv)
data_adm_allcause_sen <- as.data.table(read.csv("Data/Outcomes/admissions/bhamlsoaadmsen.csv"))

# Rename columns
names(data_adm_allcause_sen) <- c("year",
                                  "month",
                                  "day",
                                  "LSOA11CD",
                                  "a04",
                                  "a514",
                                  "a1564",
                                  "a6574",
                                  "a75plus",
                                  "total")

# Create date from year, month and date, separated by /
data_adm_allcause_sen[, date:=as.Date(paste(year, month, day, sep="/"))]

# Define series of unique LSOAs and dates
seqlsoa <- sort(unique(data_adm_allcause_sen$LSOA11CD)) # sorted list of unique LSOA codes
seqdate <- sort(unique(data_adm_allcause_sen$date)) # sorted list of unique dates
table(diff(seqdate)) # checking gaps between consecutive dates - should be 1 day gaps

# No gaps but create continuous sequence for completeness

# Create a continuous sequence of dates
seqdate <- seq(min(data_adm_allcause_sen$date), # first date in dataset
               max(data_adm_allcause_sen$date), # last date in dataset
               by = "day") # separate by a day

# Step 2.2: Prepare the case time series dataset (stratified by LSOA)

# Check all LSOAs are present
data_adm_allcause_sen[, .(n_lsoa = uniqueN(LSOA11CD)), by = year] # count unique LSOAs per year

data_adm_allcause_sen[, uniqueN(LSOA11CD)] # check across the whole dataset - some missing from some years which may have had zero emergency admissions

# Develop full dataset including days with zero admissions
data_adm_allcause_sen_cts <- expand.grid(LSOA11CD=seqlsoa, date=seqdate) |>
  data.table() |> # converts it to data table
  merge(data_adm_allcause_sen, by = c("LSOA11CD", "date"), all.x=T) # merge  in admission counts and keep all rows from new dataset

# Replace N/A with 0 
data_adm_allcause_sen_cts[is.na(data_adm_allcause_sen_cts)] <- 0 # replace N/A with zero when no adm

# Re-create time variables including day of year and week
data_adm_allcause_sen_cts[, `:=`(year=year(date),
                                 month=month(date),
                                 day=mday(date),
                                 doy=yday(date),
                                 dow=wday(date))]

# Load LSOA11 to LSOA21 lookup, apply weighting (0.5 if split)
lsoalookup <- as.data.table(read.csv("Data/Mapping/lsoa-2011-to-lsoa-2021-to-local-authority-district-2022-best-fit-bham.csv"))
data_adm_allcause_sen_cts <- data_adm_allcause_sen_cts %>% # Join data with the lookup
  left_join(lsoalookup) %>% # Joining with `by = join_by(LSOA11CD)`
  group_by(LSOA11CD) %>% # group by the original geography (LSOA11)
  mutate(weight = 1 / n_distinct(LSOA21CD)) %>% # Creates new column with value based on how many LSOAs (21) compare to original (11)
  ungroup() %>% # to ensure next step applies to whole dataset
  mutate(across(c(total, a04, a514, a1564, a6574, a75plus), ~ .x * weight)) %>% # apply weight to all the (relevant) columns
  group_by(LSOA21CD, date, year, month, day, doy, dow) %>% # group by the final columns I want to include
  summarise(across(c(total, a04, a514, a1564, a6574, a75plus), sum)) %>% # combine rows so we have one LSOA for each date
  ungroup()

#Reorder and remove additional columns from lookup
data_adm_allcause_sen_cts <- as.data.table(data_adm_allcause_sen_cts) %>%
  dplyr::select(LSOA21CD, date, year, month, day, a04, a514, a1564, a6574, a75plus, total, doy, dow) # some issues with select due to other packages, so confirming dplyr

# Sort by LSOA21 and then by date
setkey(data_adm_allcause_sen_cts, LSOA21CD, date) # data.table command - sorts by LSOA21 and then by date

# Step 2.3: Prepare the aggregated time series dataset

# One row per date, not by LSOA

# Create a new table from the full set, one row per date and only use adm columns (to sum)
data_adm_allcause_sen_aggr <- data_adm_allcause_sen[, lapply(.SD, sum), by=date, # calculation by date
                                                    .SDcols=c("a04","a514","a1564", "a6574","a75plus","total")] # only use these columns for the calculation

# Re-create time variables including day of year and week
data_adm_allcause_sen_aggr[, `:=`(year=year(date),
                                  month=month(date),
                                  day=mday(date),
                                  doy=yday(date),
                                  dow=wday(date))] # still need in standard - control for time patterns

# Sort by date
setkey(data_adm_allcause_sen_aggr, date) # data.table command - sorts by date

#Remove files and data not required
rm(data_adm_allcause_sen) # Original table no longer required

# 2.0 END