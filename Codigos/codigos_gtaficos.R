# LIMPIEZA DE GRÁFICOS 2 #
library(readxl)
library(readr)
library(esquisse)
library(tidyverse)
library(dplyr)
library(countrycode)
library(ggthemes)
library(mapdata)
library(ggplot2)
library(maps)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rworldmap)
library(flextable)

#CARGAR DATOS#
economia_2019= read_delim("data_set/OPCION 2/economicdata2019-2019.csv", 
                          delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

evolucion_felicidad <- read_excel("data_set/OPCION 2/evolucion_felicidad.xls", na = "NA")

indice_de_libertad_2021 <- read_excel("data_set/OPCION 2/human-freedom-index-2020 (1).xlsx", 
                                      na = "NA")
mortalidad_2020 <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx", 
                              na = "NA")

ranking_felicidad_2021 <- read_excel("data_set/OPCION 2/ranking_felicidad_2021.xls", 
                                     na = "NA")

## LIMPIEZA DE DATOS ##
grafico_1= ranking_felicidad_2021 %>% 
  select(`Ladder score`, `Country name`)

infelices= grafico_1[order(grafico_1$`Ladder score`, grafico_1$`Country name`),]
infelices= infelices[1:5,]
felices= grafico_1[order(grafico_1$`Ladder score`),]
felices= felices[nrow(felices):1,]
felices= felices[1:5,]


infelices= infelices %>% 
  mutate(ISO=countrycode(sourcevar = infelices$`Country name`, origin = "country.name", destination = "iso3c")) %>% 
  mutate(country= infelices$`Country name`) %>% 
  select(-`Country name`) %>% 
  mutate(score= infelices$`Ladder score`) %>% 
  select(-`Ladder score`) %>% 
  rowid_to_column() %>% 
  as.data.frame()

options(digits = 10)
latitudes= c(33.93911, -19.015438, -1.940278, -22.328474, -29.609988)
longitudes= c(67.709953, 29.154857, 29.873888, 24.684866, 28.233608)

infelices= infelices %>% 
  mutate(longitud= longitudes) %>% 
  mutate(latitud= latitudes)

infelices_tabla= infelices %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the countriesvisited data.")  %>%
  flextable::border_outer()

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))

visitedMap <- joinCountryData2Map(infelices, 
                                  joinCode = "ISO3",
                                  nameJoinColumn = "ISO")
names(infelices)
mapParams <- mapCountryData(visitedMap, 
                            nameColumnToPlot="country",
                            oceanCol = "cadetblue3",
                            catMethod = "categorical",
                            mapRegion = "africa",
                            missingCountryCol = gray(.8),
                            colourPalette = c("lightsalmon",
                                              "purple",
                                              "red", "black", 
                                              "forestgreen"),
                            addLegend = F,
                            mapTitle = "Los 5 países con puntaje de Escala de Felicidad más bajo",
                            border = "gray57",
                            lwd= 1,
                            aspect= 0.8,
                            add=T)

do.call(addMapLegendBoxes, c(mapParams,
                             x = 'bottomright',
                             title = "Países",
                             horiz = F,
                             bg = "white",
                             bty = "n",
                             cex= 0.6))
infelices

paises_inf= c("Afghanistan", "Zimbabwe", "Rwanda", "Botswana", "Lesotho")
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

some.eu.maps <- map_data("world", region = paises_inf)
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 4, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


3############################################
grafico_1= ranking_felicidad_2021 %>% 
  select(`Ladder score`, `Country name`)

pais_feliz= grafico_1[1:5,]
aux_1= grafico_1[order(grafico_1$`Ladder score`, grafico_1$`Country name`),]
pais_infeliz= aux_1[1:5,]

options(digits = 10)
latitudes= c(33.93911, -19.015438, -1.940278, -22.328474, -29.609988)
longitudes= c(67.709953, 29.154857, 29.873888, 24.684866, 28.233608)

infelices= infelices %>% 
  mutate(longitud= longitudes) %>% 
  mutate(latitud= latitudes)

infelices= pais_infeliz %>% 
  rowid_to_column() %>% 
  mutate(group= rowid) %>% 
  mutate(ISO= countrycode(sourcevar = pais_infeliz$`Country name`, origin = "country.name", destination = "iso3c")) %>% 
  mutate(country= `Country name`) %>% 
  mutate(score= pais_infeliz$`Ladder score`) %>% 
  select(-`Ladder score`, -`Country name`, -rowid) %>% 
  mutate(lon= c(67.709953, 29.154857, 29.873888, 24.684866, 28.233608)) %>% 
  mutate(lat= c(33.93911, -19.015438, -1.940278, -22.328474, -29.609988)) %>% 
  as.data.frame()
infelices