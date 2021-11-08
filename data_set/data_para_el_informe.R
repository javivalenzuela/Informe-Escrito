## CARGAR LIBRERÍAS ##
library(readxl)
library(readr)
library(esquisse)
library(tidyverse)
library(dplyr)
library(countrycode)
#### ============ ####

## == Cargar los datos == ##
rank_felicidad <- read_excel("data_set/OPCION 2/DataForFigure2.1WHR2021C2 (1).xls") %>% 
  as_tibble()
felicidad_en_tiempo <- read_excel("data_set/OPCION 2/DataPanelWHR2021C2 (1).xls") %>%
  as_tibble()
mortalidad <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx") %>% 
  as_tibble()
inidice_de_libertad_2020 <- read_csv("data_set/OPCION 2/SIN-human-freedom-index-2020.csv", 
                                         na = "NA") %>% 
  as_tibble()
indice_gini <- read_csv("data_set/OPCION 2/indice_gini.csv", 
                        na = "NA") %>% as_tibble()
economia_2019 <- read_delim("data_set/OPCION 2/economicdata2019-2019.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                    na = "NA", trim_ws = TRUE)
names(rank_felicidad)
###  ================  ###





