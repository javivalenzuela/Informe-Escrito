## CARGAR LIBRERÍAS ##
library(readxl)
library(readr)
library(esquisse)
library(tidyverse)
library(dplyr)
library(countrycode)
#### ============ ####

## == Cargar los datos == ##----
ranking_felicidad_2021 <- read_excel("data_set/OPCION 2/ranking_felicidad_2021.xls", 
                                     na = "NA")
evolucion_felicidad <- read_excel("data_set/OPCION 2/evolucion_felicidad.xls", 
                                  na = "NA")
mortalidad <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx") %>% 
  as_tibble()

economia_2019 <- read_delim("data_set/OPCION 2/economicdata2019-2019.csv", 
                                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                    na = "NA", trim_ws = TRUE)
indice_libertad_2020 <- read_excel("data_set/OPCION 2/human-freedom-index-2020 (1).xlsx", 
                                   na = "NA") %>% 
  as_tibble()
###  === Limpieza de datos ===  ###----

ranking_felicidad_2021= ranking_felicidad_2021 %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Western Europe",
         "Europe", `Regional indicator`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Central and Eastern Europe",
                        "Europe", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "East Asia",
                        "Asia", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "South Asia",
                        "Asia", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Southeast Asia",
                        "Asia", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Commonwealth of Independent States",
                        "Asia", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "North America and ANZ",
                        "Americas", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Latin America and Caribbean",
                        "Americas", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Middle East and North Africa",
                        "Africa", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Regional indicator`== "Sub-Saharan Africa",
                        "Africa", `Continente`), .after= `Country name`) %>%
  mutate(Continente= ifelse(`Country name`== "Australia",
                           "Oceania", `Continente`), .after= `Country name`) %>% 
  mutate(Continente= ifelse(`Country name`== "New Zealand",
                           "Oceania", `Continente`), .after= `Country name`) %>% 
  select(-`Regional indicator`)

## == Creación de gráfico 1 == ##----
ranking_felicidad_2021 %>% 
  ggplot(aes(`Ladder score`, `Logged GDP per capita`, color= Continente))+
  geom_point()+
  scale_x_continuous(limits = c(0,8))+
  scale_y_log10(labels = scales::dollar)+
  labs(title = "PIB per cápita y Escala de la Felicidad",
       subtitle = "Cada punto representa PIB/Felicidad de un país",
       x = "Escala de Felicidad (Puntaje)",
       y = "PIB per cápita",
       caption = "Elaboración propia a partir de datos de worldhappiness.report")

ggsave("figuras/funciones_para_graficos.png", height = 5, width = 8)
