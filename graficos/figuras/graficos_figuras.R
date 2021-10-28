# Gráficos sobre nuestros datos ----

library(ggplot2)
library(dplyr)
library(gghighlight)
library(countrycode)
library(readr)

#Cargando los datos
datos_2015= `2015.datos`
datos_2017 <- read_csv("data_set/2017-datos.csv")
datos_2016= `2016.datos`
