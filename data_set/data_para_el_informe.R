## CARGAR LIBRERÍAS ##
library(readxl)
library(readr)
#### ============ ####

## == Cargar los datos == ##
rank_felicidad <- read_excel("data_set/OPCION 2/DataForFigure2.1WHR2021C2 (1).xls")
felicidad_en_tiempo <- read_excel("data_set/OPCION 2/DataPanelWHR2021C2 (1).xls")
mortalidad <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx")
inidice_de_libertad_2020 <- read_csv("data_set/OPCION 2/SIN-human-freedom-index-2020.csv", 
                                         na = "NA")
indice_gini <- read_csv("data_set/OPCION 1/archivos_con_la_data/indice_gini.csv", 
                        col_types = cols(`1803` = col_number()), 
                        na = "NA")
###  ================  ###







