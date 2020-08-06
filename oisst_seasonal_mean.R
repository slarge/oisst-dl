# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
# library(doParallel) # For parallel processing
library(furrr)

OISST_dates <- data.frame(month = seq(as.Date("1982-03-01"), as.Date("2019-11-30"), by = "month")) %>% 
  mutate(t_month = format(month, "%m"),
         t_year = as.numeric(format(month, "%Y")),
         t_year = ifelse(t_month %in% c("01", "02"), 
                         t_year - 1,
                         t_year),
         season = case_when(t_month %in% c("03", "04", "05") ~ "spring_",
                            t_month %in% c("06", "07", "08") ~ "summer_",
                            t_month %in% c("09", "10", "11") ~ "fall_",
                            t_month %in% c("12", "01", "02") ~ "winter_",
                            TRUE ~ NA_character_),
         season = paste0(season, t_year)) %>% 
  select(season) %>% 
  distinct(.keep_all = TRUE)


tictoc::tic()
OISST_season <- OISST_dates %>% 
  group_by(season) %>% 
  summarize(data = purrr::map(.x = season, function(x) OISST_load_season(season = x,
                                                                       lon1 = 360-77, lon2 = 360-65, 
                                                                       lat1 = 35, lat2 = 45)))
tictoc::toc()


OISST_season %>%
  tidyr::unnest(cols = c("data")) %>%
  mutate(year = as.numeric(gsub(".*_", "", season)),
         season = factor(gsub("_.*", "", season), levels = c("winter", "spring", "summer", "fall"))) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = temp_mean)) +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
    facet_grid(year ~ season)

saveRDS(object = OISST_season, file = "data/oisst_seasonal_mean.rds")
