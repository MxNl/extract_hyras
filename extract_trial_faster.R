library(dplyr)
library(sf)
library(exactextractr)
library(purrr)
library(furrr)
library(lubridate)
library(stringr)
library(tidyr)
library(ncdf4)
library(ggplot2)
library(raster)


# Config ------------------------------------------------------------------
N_STATIONS <- 1000
N_DAYS <- 365
N_YEARS <- 3
N_CORES <- N_YEARS
SUBSET_NETCDF <- TRUE
SUBSET_STATIONS <- TRUE


# Source ------------------------------------------------------------------
"R" |> 
  list.files(full.names = TRUE) |> 
  map(source)


# Import ------------------------------------------------------------------

## HYRAS -------------------------------------------------------------------
# path_to_hyras <- "J:/PROJEKTE/FARM/Daten/Klimadaten/hyras_de_dwd/daily"
path_to_hyras <- "D:/Data/farm_sw/klimadaten/hyras_de_dwd/daily"
# path_to_hyras <- "data"

files_list <- list.files(path_to_hyras, pattern = ".nc$", full.names = TRUE)

if(SUBSET_NETCDF) files_list <- files_list |> magrittr::extract(1:N_YEARS)

# hyras_brick <- files_list |> 
#   map(brick, varname = "pr") |> 
#   reduce(raster::stack)

# if(SUBSET_NETCDF) hyras_brick <- hyras_brick |> raster::subset(1:N_DAYS)

# Stations ----------------------------------------------------------------
load("J:/PROJEKTE/FARM/Daten/Datenanalyse/INVEKOS_metrics/Output/alle_messstellen1000.RData")
stations <- alle_messstellen_1000

if(SUBSET_STATIONS) stations <- stations |> slice_sample(n = N_STATIONS)

stations <- stations |>
  st_transform(st_crs(stars::read_stars(files_list[1], proxy = TRUE)))


# Value Extraction --------------------------------------------------------
future::plan(multisession, workers = N_CORES)

# tictoc::tic()
hyras_extraction_df <- files_list |> 
  future_map_dfr(extract_hyras, sf_points = stations, .progress = TRUE)
# tictoc::toc()



# Plotting ----------------------------------------------------------------

if (SUBSET_NETCDF & SUBSET_STATIONS & (N_DAYS <= 10)) {
  hyras_extraction_df |> 
    inner_join(stations, by = c("station_id" ="messstellen_id" )) |> 
    st_as_sf() |> 
    st_transform(st_crs(alle_messstellen_1000)) |> 
    ggplot() +
    geom_sf(aes(fill = prec,
                colour = prec),
            size = 2) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    theme_minimal() +
    facet_wrap(~ as.character(date))
}

if (SUBSET_NETCDF & SUBSET_STATIONS & (N_STATIONS <= 10)) {
  hyras_extraction_df |> 
    inner_join(stations, by = c("station_id" ="messstellen_id" )) |> 
    st_as_sf() |> 
    st_transform(st_crs(alle_messstellen_1000)) |> 
    st_centroid() %>%
    mutate(x = st_coordinates(.)[1],
           y = st_coordinates(.)[2]) |>
    st_drop_geometry() |> 
    arrange(-y, x) |> 
    ggplot(aes(date, prec)) +
    geom_col(aes(fill = prec)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    facet_wrap(~ station_id, ncol = 1)
}
