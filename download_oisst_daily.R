# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
# library(doParallel) # For parallel processing
library(furrr)


# First we tell R where the data are on the interwebs
OISST_base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
# Note that one may go to this URL in any web browser to manually inspect the files

# Now we create a data.frame that contains all of the dates we want to download
# NB: In order to change the dates download changes the dates in the following line
OISST_dates <- data.frame(t = seq(as.Date("1982-01-01"), as.Date("2019-12-31"), by = "day"))

# To finish up this step we add some text to those dates so they match the OISST file names
OISST_files <- OISST_dates %>% 
  head(100) %>% 
  mutate(t_day = gsub("-", "", t),
         t_month = substr(t_day, 1, 6),
         t_year = year(t),
         file_name = paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc")) 

# OISST_files %>% 
#   pull(file_name) %>% 
#   map(function(x) OISST_url_daily_dl(x))

## Create monthly products
tictoc::tic()
future::plan(future::multiprocess)
OISST_dat <- OISST_files %>%
  # head(10) %>% 
  select(t_day, t_month, file_path) %>% 
  filter(t_month == "198201") %>%
  mutate(file_path = sprintf("data/OISST/oisst-avhrr-v02r01.%s.nc", t_day)) %>% 
  mutate(data = furrr::future_map(.x = file_path, function(x) OISST_load(file_name = x,
                                                                     lon1 = 360-77, lon2 = 360-65, 
                                                                     lat1 = 35, lat2 = 45))) %>% 
  tidyr::unnest(cols = c("data")) %>% 
  group_by(t_month, lon, lat) %>% 
  summarize(mean_temp = mean(temp, na.rm = TRUE),
            cv_temp = sd(temp, na.rm = TRUE)/ temp)
tictoc::toc()


file_path <- sprintf("data/OISST/oisst-avhrr-v02r01.%s.nc",  OISST_files$t_day)[1:30]
substr("198201", start = 5, stop = 6)

x <- as.Date(paste0("198201", "01"), format = "%Y%m%d")
seq.Date(from = x,
         by = "day",
         length.out = lubridate::days_in_month(x)[[1]])

seq.Date()


OISST_dates <- data.frame(month = seq(as.Date("1982-01-01"), as.Date("1982-12-31"), by = "month"))


tictoc::tic()
OISST_month <- OISST_dates %>% 
  group_by(month) %>% 
  summarize(data = purrr::map(.x = month, function(x) OISST_load_month(month = x,
                                                                       lon1 = 360-77, lon2 = 360-65, 
                                                                       lat1 = 35, lat2 = 45)))
tictoc::toc()



  # month_out <- month_dat %>% 


# %>% 
#   tidyr::unnest(cols = c("data"))

# mutate(file_path = dir("data/OISST", full.names = T),
#          data = furrr::future_map(.x = file_path, function(x) OISST_load(file_name = x,
#                                                                      lon1 = 360-77, lon2 = 360-65, 
#                                                                      lat1 = 35, lat2 = 45))) %>% 
  # select(data) %>% 
  #   tidyr::unnest(c(data))
  
OISST_dat %>% 
  # filter(t == "2019-12-01") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = mean_temp)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")

td <- OISST_load(file_name = OISST_files[1], lon1 = 360-77, lon2 = 360-65, lat1 = 35, lat2 = 45)


td %>%
  # filter(month_year == "2019-10") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp)) +
  # borders() + # Activate this line to see the global map
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) 

tt <- OISST_dat$file_path[1] %>% 
  tidync() %>% 
  activate("sst") %>%
  hyper_tibble()



zlev = c(0, 0)
latitude = c(35, 45)
longitude = c(-65, -77)

# Load the data in parallel
OISST_dat <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                         lon1 = 270, lon2 = 320, lat1 = 30, lat2 = 50)