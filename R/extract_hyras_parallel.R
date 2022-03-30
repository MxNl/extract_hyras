library(styler)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(purrr)
library(furrr)
library(stars)


# Config ------------------------------------------------------------------
N_CORES <- 3
N_STATIONS <- 20
N_DAYS <- 5
N_YEARS <- N_CORES * 3
SUBSET_NETCDF <- TRUE
SUBSET_STATIONS <- TRUE



# Import ------------------------------------------------------------------

## HYRAS -------------------------------------------------------------------
path_to_hyras <- "J:/PROJEKTE/FARM/Daten/Klimadaten/hyras_de_dwd/daily"

files_list <- list.files(path_to_hyras, pattern = ".nc$", full.names = TRUE)

if(SUBSET_NETCDF) files_list <- files_list |> magrittr::extract(1:N_YEARS)

files_list <- files_list |> 
  as_tibble() |> 
  mutate(groups = sort(rep_len(1:N_CORES, length(files_list)))) |> 
  group_by(groups) |> 
  group_split()

# Stations ----------------------------------------------------------------
load("J:/PROJEKTE/FARM/Daten/Datenanalyse/INVEKOS_metrics/Output/alle_messstellen1000.RData")
stations <- alle_messstellen_1000

if(SUBSET_STATIONS) stations <- stations |> slice_sample(n = N_STATIONS)

stations <- stations |> 
  st_transform(st_crs(read_stars(files_list[[1]]$value)))



# Aggregate spatially -----------------------------------------------------
read_and_aggregate_ncdf <- function(files_list, stations) {
  netcdf_list <- files_list |>
    # chuck(1) |> 
    pull(value) |> 
    read_stars(proxy = TRUE, along = "time")

  # sample.int(chuck(dim(netcdf_list), "time"), N_DAYS)
  if (SUBSET_NETCDF) netcdf_list <- netcdf_list |> slice(index = 1:N_DAYS, along = "time")

  aggregate(netcdf_list, by = stations, FUN = mean, na.rm = TRUE)
}

future::plan(multisession, workers = N_CORES)

tictoc::tic()
hyras_stations <- files_list |> 
  future_map(~ read_and_aggregate_ncdf(.x, stations))
tictoc::toc()


# Aggregated Stars as Sf --------------------------------------------------
stars_to_sf <- function(stars, time_staps, stations) {
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

get_time_stamps <- function(hyras_stations) {
  hyras_stations |> 
    st_get_dimension_values("time") |> 
    as.Date() |> 
    as.character()
}

time_stamps <- hyras_stations |> 
  map(get_time_stamps)

hyras_stations_df <- hyras_stations |> 
  map2_df(time_stamps, stars_to_sf, stations) |> 
  st_transform(st_crs(alle_messstellen_1000)) |> 
  arrange(station_id, date)

if (SUBSET_NETCDF & SUBSET_STATIONS) {
  extract_hyras_parallel_results$hyras_stations_df |> 
    ggplot() +
    geom_sf(aes(fill = precipitation,
                colour = precipitation),
            size = 2) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    theme_minimal() +
    facet_wrap(~ date)
}

if (SUBSET_NETCDF & SUBSET_STATIONS) {
  extract_hyras_parallel_results$hyras_stations_df |> 
    st_drop_geometry() |> 
    as_tibble() |> 
    ggplot(aes(date, precipitation)) +
    geom_col() +
    theme_minimal() +
    facet_wrap(~ station_id)
}
