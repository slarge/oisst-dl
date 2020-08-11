## Download OISST for NEUS
## From: https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html




# The packages we will need
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidync")
# install.packages("doParallel")
# install.packages("rerddap")
# install.packages("plyr") # Note that this library should never be loaded, only installed

# The packages we will use
library(dplyr) # A staple for modern data management in R
# library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
# library(tidync) # For easily dealing with NetCDF data
# library(rerddap) # For easily downloading subsets of data
# library(doParallel) # For parallel processing
# library(stars)
# remotes::install_github("spatial-ews/spatialwarningsGis")
# library(future)
library(spatialwarnings)
library(spatialwarningsGis)
# library(ecodata)
library(sf)

source("R/utilities.R")

OISST_season <- readRDS("Data/oisst_seasonal_mean.rds")

df_nest <- OISST_season %>%
  tidyr::unnest(cols = c("data")) %>% 
  mutate(season_year = season,
         year = as.numeric(gsub(".*_", "", season_year)),
         lon = lon - 360,
         season = factor(gsub("_.*", "", season_year), levels = c("winter", "spring", "summer", "fall"))) %>% 
  dplyr::select(season_year,
                x = lon,
                y = lat,
                z = temp_mean) %>%
  group_by(season_year) %>%
  tidyr::nest()

#Read in EPU shapefile (will be downsampled to match OI raster resolution)
epu <- ecodata::epu_sf

season_list <-  c("winter", "spring", "summer", "fall")
epu_list <- c("GB", "GOM", "MAB", "SS")

season_indices <- data.frame()

for(j in epu_list){
  for(i in season_list){
    
    ## Select the season
    df_season <- df_nest %>% 
      dplyr::filter(grepl(i, season_year))
    
    df_l <- lapply(1:nrow(df_season), function(x) raster::rasterFromXYZ(df_season$data[[x]]))

    ## Select the EPU    
    td_mask <- lapply(1:nrow(df_season), function(x) raster::mask(df_l[[x]], epu[epu$EPU == j, ]))

    # future::plan(future::multiprocess)
    
    indices <- spatialwarnings::compute_indicator(td_mask, fun = na_aware_ews,
                                                  cg_subsize = 2)
    
    indices_test <- spatialwarnings::indictest(indices,
                                               nulln = 999,
                                               null_method = randomize_matrix_no_na)
    season_indices <- rbind(season_indices, cbind(season = i, epu = j, as.data.frame(indices_test)))
  }
}
  
saveRDS(season_indices, file = "data/season_indices_EPU.rds")

library(ggplot2)
pp1 <- ggplot(season_indices %>% filter(indic == "variance"), aes(x = matrixn, y = value, color = season))+
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Set2") +
  labs(x = "Year", y = "Variance", title = "NEUS SST spatial variance by EPU and season", caption = "OISST v2.1 daily 1/4°") +
  facet_wrap(~ epu) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = "figures/seasonal_var_epu.pdf", plot = pp1)
