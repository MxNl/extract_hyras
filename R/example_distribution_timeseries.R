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
