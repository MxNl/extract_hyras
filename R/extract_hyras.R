extract_hyras <- function(ncdf_path, sf_points) {
  
  hyras_brick <- ncdf_path |> 
    brick()
  
  year <- ncdf_path |> 
    basename() |> 
    stringr::word(4, 4, sep = "_")
  n_stations <- sf_points |> nrow()
  n_days <- hyras_brick |> nlayers()
  
  tictoc::tic()
  hyras_extraction_df <- sf_points |> 
    exact_extract(x = hyras_brick, fun = "mean") |> 
    as_tibble() |> 
    mutate(station_id = sf_points$messstellen_id, .before = 1) |> 
    pivot_longer(-station_id, names_to = "date", values_to = "prec") |> 
    mutate(date = str_remove(date, "mean.X")) |> 
    mutate(date = ymd_hms(date))
  tictoc::toc()
  
  hyras_extraction_df |> 
    saveRDS(stringr::str_glue("data_export/hyrasextract_{year}_{n_days}_{n_stations}.rds"))
  
  hyras_extraction_df
}
