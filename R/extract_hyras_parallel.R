library(dplyr)
library(sf)
library(purrr)
library(furrr)
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

files_list <- files_list |> 
  as_tibble() |> 
  mutate(groups = sort(rep_len(1:N_CORES, length(files_list)))) |> 
  group_by(groups) |> 
  group_split()

netcdf_list <- files_list |>
  map(~ read_stars(.x$value, proxy = TRUE, along = "time"))

if(SUBSET_NETCDF) netcdf_list <- netcdf_list |> map(~ slice(.x, index = c(1,100,300), along = "time"))

# Stations ----------------------------------------------------------------
load("J:/PROJEKTE/FARM/Daten/Datenanalyse/INVEKOS_metrics/Output/alle_messstellen1000.RData")
stations <- alle_messstellen_1000

if(SUBSET_STATIONS) stations <- stations |> slice_sample(n = 2)

stations <- stations |> 
  st_transform(st_crs(netcdf_list[[1]]))


# Aggregate spatially -----------------------------------------------------


future::plan(multisession, workers = N_CORES)

tictoc::tic()
hyras_stations <- map(.x = netcdf_list, ~aggregate(.x, by = stations, FUN = mean, na.rm=TRUE))
tictoc::toc()




hyras_stations[stations] |> 
  st_as_sf()

hyras_stations |> 
  st_as_sf()
