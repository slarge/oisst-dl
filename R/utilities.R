
# This function will go about downloading each day of data as a NetCDF file
# Note that this will download files into a 'data/OISST' folder in the root directory
# If this folder does not exist it will create it
# If it does not automatically create the folder it will need to be done manually
# The folder that is created must be a new folder with no other files in it
# A possible bug with netCDF files in R is they won't load correctly from 
# existing folders with other file types in them
# This function will also check if the file has been previously downloaded
# If it has it will not download it again
OISST_url_daily_dl <- function(target_URL){

  file_name <- paste0("data/OISST/",
                      sapply(strsplit(target_URL, split = "/"), "[[", 10))
  if(!file.exists(file_name)) {
    Sys.sleep(3)
    download.file(url = target_URL, method = "libcurl", destfile = file_name)
  } else{
    print(file_name)
  }
}


OISST_load <- function(file_name, lon1, lon2, lat1, lat2){
  OISST_dat <- tidync(file_name) %>%
    hyper_filter(lon = between(lon, lon1, lon2),
                 lat = between(lat, lat1, lat2)) %>% 
    hyper_tibble() %>% 
    select(lon, lat, time, sst) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    mutate(t = as.Date(t, origin = "1978-01-01"))
  return(OISST_dat)
}

OISST_load_month <- function(month, lon1, lon2, lat1, lat2){
  
  date_seq <- seq.Date(from = month,
                       by = "day",
                       length.out = lubridate::days_in_month(month)[[1]])
  
  future::plan(future::multiprocess)
  month_dat <- data.frame(month = month,
                          t_day = format(date_seq, "%Y%m%d"),
                          t_month = format(month, "%Y-%m")) %>% 
    mutate(file_path = sprintf("data/OISST/oisst-avhrr-v02r01.%s.nc", t_day)) %>% 
    summarize(t_month,
              data = furrr::future_map(file_path, function(x) {
                tidync(x) %>%
                  hyper_filter(lon = between(lon, lon1, lon2),
                               lat = between(lat, lat1, lat2)) %>% 
                  hyper_tibble() %>% 
                  select(lon, lat, time, sst) %>% 
                  dplyr::rename(t = time, temp = sst) %>% 
                  mutate(t = as.Date(t, origin = "1978-01-01"))}
              )) %>% 
    tidyr::unnest(cols = c("data")) %>% 
    group_by(t_month, lon, lat) %>% 
    summarize(temp_mean = mean(temp, na.rm = TRUE),
              temp_cv = sd(temp, na.rm = TRUE)/temp_mean)
  return(month_dat)
}