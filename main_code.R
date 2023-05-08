# install.packages("pacman")

setwd("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios")
pacman::p_load(ggthemr, shades, extrafont) # Allow to create the plots
pacman::p_load(zoo) # Allows working with dates 
pacman::p_load(tidyverse, ggplot2, GGally, scales, clipr) # Common use
pacman::p_load(spatstat)
pacman::p_load(xts, TTR, forecast, readxl, corrplot, caret, tsDyn, urca)

common_params <- theme(
  text = element_text(size = 20, family = "Gill Sans Nova Book"),
  axis.title = element_text(size = 15, family = "Gill Sans Nova Book"),
  axis.text = element_text(size = 15, family = "Gill Sans Nova Book"),
  plot.title = element_text(size = 15, family = "Gill Sans Nova Book", hjust = 0.5),
  plot.subtitle = element_text(size = 15, family = "Gill Sans Nova Book", hjust = 0.5)
)


source("5_scripts/00_color_palette.R")