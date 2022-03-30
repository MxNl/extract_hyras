library(dplyr)
library(sf)
library(exactextractr)
library(purrr)
library(furrr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(raster)


# Config ------------------------------------------------------------------
N_CORES <- 3
N_STATIONS <- 10
N_DAYS <- 365
N_YEARS <- 3
SUBSET_NETCDF <- TRUE
SUBSET_STATIONS <- TRUE


# Import ------------------------------------------------------------------

## HYRAS -------------------------------------------------------------------
path_to_hyras <- "J:/PROJEKTE/FARM/Daten/Klimadaten/hyras_de_dwd/daily"

files_list <- list.files(path_to_hyras, pattern = ".nc$", full.names = TRUE)

if(SUBSET_NETCDF) files_list <- files_list |> magrittr::extract(1:N_YEARS)

hyras_brick <- files_list |> 
  map(brick, varname = "pr") |> 
  reduce(raster::stack)

if(SUBSET_NETCDF) hyras_brick <- hyras_brick |> raster::subset(1:N_DAYS)

# Stations ----------------------------------------------------------------
load("J:/PROJEKTE/FARM/Daten/Datenanalyse/INVEKOS_metrics/Output/alle_messstellen1000.RData")
stations <- alle_messstellen_1000

if(SUBSET_STATIONS) stations <- stations |> slice_sample(n = N_STATIONS)

stations <- stations |>
  st_transform(st_crs(stars::read_stars(files_list[1], proxy = TRUE)))

# stations <- stations |> 
#   mutate(groups = sort(rep_len(1:N_CORES, nrow(stations)))) |> 
#   group_by(groups) |> 
#   group_split()
# 
# future::plan(multisession, workers = N_CORES)
# tictoc::tic()
# test <- stations |> 
#   future_map_dfr(~ exact_extract(x = hyras_brick, y = .x, fun = "mean"))
# tictoc::toc()


# Value Extraction --------------------------------------------------------

tictoc::tic()
hyras_extraction_df <- stations |> 
  # reduce(bind_rows) |> 
  exact_extract(x = hyras_brick, fun = "mean") |> 
  as_tibble()
tictoc::toc()

hyras_extraction_df <- hyras_extraction_df |> 
  as_tibble() |> 
  mutate(station_id = stations$messstellen_id, .before = 1) |> 
  pivot_longer(-station_id, names_to = "date", values_to = "prec") |> 
  mutate(date = str_remove(date, "mean.X")) |> 
  mutate(date = ymd_hms(date))


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
    as_tibble() |> 
    ggplot(aes(date, prec)) +
    geom_col(aes(fill = prec)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    facet_wrap(~ station_id, ncol = 1)
}
