library(dplyr)
library(ggplot2)
library(ggridges)


read.csv("J:/PROJEKTE/FARM/Daten/Datenanalyse/Chem_Parameter/Zeitreihe_Messwerte_GW_Temperatur.csv") |> 
  as_tibble() |> 
  filter(jahr <= 2022,
         messwert <= 15 & messwert >= 5,
         monat <= 8 & messwert >= 6) |> 
  ggplot(aes(messwert, jahr, group = jahr, fill = stat(x))) +
  ggridges::geom_density_ridges_gradient(colour = "white", alpha = .75) +
  scale_y_reverse() +
  theme_ridges(grid = FALSE,
               font_size = 10) +
  scale_fill_viridis_c(name = "Temp. [°C]", option = "C") +
  facet_wrap(~bundesland, scales = "free")

seasons <- c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Autumn")

upper_threshold <- function(x) {
  q <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  q[2] + 1.5 * iqr
}

lower_threshold <- function(x) {
  q <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  q[1] - 1.5 * iqr
}

read.csv("J:/PROJEKTE/FARM/Daten/Datenanalyse/Chem_Parameter/Zeitreihe_Messwerte_GW_Temperatur.csv") |> 
  as_tibble() |> 
  filter(jahr <= 2022 & jahr >= 1990) |> 
  mutate(quarter = quarter(monat)) |> 
  group_by(jahr, quarter) |> 
  filter(messwert <= upper_threshold(messwert) & messwert >= lower_threshold(messwert)) |> 
  ungroup() |> 
  ggplot() +
  ggridges::geom_density_ridges_gradient(aes(messwert, jahr, group = jahr, fill = stat(x)), 
                                         colour = "white", 
                                         alpha = .75
                                         # quantile_lines = TRUE,
                                         # quantiles = 2
                                         ) +
  scale_y_reverse() +
  theme_ridges(grid = FALSE,
               font_size = 10) +
  scale_fill_viridis_c(name = "Temp. [°C]", option = "C") +
  facet_wrap(~quarter, scales = "free", nrow = 1, labeller = as_labeller(seasons))

