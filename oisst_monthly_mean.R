# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
# library(doParallel) # For parallel processing
library(furrr)

OISST_dates <- data.frame(month = seq(as.Date("1982-01-01"), as.Date("2019-12-31"), by = "month"))

tictoc::tic()
OISST_month <- OISST_dates %>% 
  group_by(month) %>% 
  summarize(data = purrr::map(.x = month, function(x) OISST_load_month(month = x,
                                                                       lon1 = 360-77, lon2 = 360-65, 
                                                                       lat1 = 35, lat2 = 45)))
tictoc::toc()
saveRDS(object = OISST_month, file = "data/oisst_monthly_mean.rds")
