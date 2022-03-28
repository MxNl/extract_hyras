library(dplyr)
library(sf)
library(purrr)
library(furrr)
library(tidyr)
library(ggplot2)
library(stars)


# Config ------------------------------------------------------------------
N_CORES <- 3
N_STATIONS <- 2
N_DAYS <- 3
SUBSET_NETCDF <- TRUE
SUBSET_STATIONS <- TRUE


# Import ------------------------------------------------------------------

## HYRAS -------------------------------------------------------------------
path_to_hyras <- "J:/PROJEKTE/FARM/Daten/Klimadaten/hyras_de_dwd/daily"

files_list <- list.files(path_to_hyras, pattern = ".nc$", full.names = TRUE)

if(SUBSET_NETCDF) files_list <- files_list |> magrittr::extract(1:2)

netcdf_list <- files_list |>
  read_stars(proxy = TRUE, along = "time")

if(SUBSET_NETCDF) netcdf_list <- netcdf_list |> slice(index = c(1,100,300), along = "time")

# Stations ----------------------------------------------------------------
load("J:/PROJEKTE/FARM/Daten/Datenanalyse/INVEKOS_metrics/Output/alle_messstellen1000.RData")
stations <- alle_messstellen_1000

if(SUBSET_STATIONS) stations <- stations |> slice_sample(n = 2)

stations <- stations |> 
  st_transform(st_crs(netcdf_list))


# Aggregate spatially -----------------------------------------------------
stars_to_sf <- function(stars) {
  stars |>
    st_as_sf() |> 
    set_names(c(time_staps, "geometry")) |> 
    mutate(station_id = stations$messstellen_id, .before = 1) |> 
    # st_drop_geometry() |> 
    # as_tibble() |> 
    pivot_longer(-all_of(c("station_id", "geometry"))) |> 
    rename(date = name,
           precipitation = value) |> 
    mutate(date = as.Date(date),
           precipitation = as.numeric(precipitation)) |> 
    relocate(station_id, date, precipitation)
}


tictoc::tic()
hyras_stations <- netcdf_list |> aggregate(by = stations, FUN = mean, na.rm=TRUE)
tictoc::toc()

time_staps <- hyras_stations |> 
  st_get_dimension_values("time") |> 
  as.Date() |> 
  as.character()

hyras_stations_df <- hyras_stations |> stars_to_sf()

if (SUBSET_NETCDF & SUBSET_STATIONS) {
hyras_stations_df |> 
  ggplot() +
  geom_sf(aes(fill = precipitation),
          colour = NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  facet_wrap(~ date)
}


