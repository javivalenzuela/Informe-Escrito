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

#############################3
economia_2019= read_delim("data_set/OPCION 2/economicdata2019-2019.csv", 
                          delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

evolucion_felicidad <- read_excel("data_set/OPCION 2/evolucion_felicidad.xls", na = "NA")

indice_de_libertad_2021 <- read_excel("data_set/OPCION 2/human-freedom-index-2020 (1).xlsx", 
                                      na = "NA")
mortalidad_2020 <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx", 
                              na = "NA")

ranking_felicidad_2021 <- read_excel("data_set/OPCION 2/ranking_felicidad_2021.xls", 
                                     na = "NA")

# GRAFICOS PREGUNTA 1 -----------------------------------------------------
grafico_1= ranking_felicidad_2021 %>% 
  select(`Ladder score`, `Country name`)

pais_feliz= grafico_1[1:5,]
aux_1= grafico_1[order(grafico_1$`Ladder score`, grafico_1$`Country name`),]
pais_infeliz= aux_1[1:5,]

options(digits = 10)

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

world <- map_data("world")
a= c("Afghanistan" = "violetred3","Zimbabwe" = "blue2", 
     "Rwanda"= "springgreen4","Botswana"= 'brown4',"Lesotho"= "red")
grafico_infelices= ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey57", fill = "peachpuff", size = 0.5
  ) +
  geom_point(data = infelices,
    aes(lon, lat, color = country),
    alpha = 1, size=2, shape= 19)+
  theme_grey() +
  scale_color_manual(name= "Países", values = a)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "paleturquoise1", colour = "paleturquoise1"))+
  labs( x = "Longitud", y = "Latitud") +
  labs(title="Los peores países en la Escala de Felicidad",
       subtitle = "Se muestran los 5 países con peores puntajes según la encuesta realizada por World Happiness Report.")
grafico_infelices

ggsave("figuras/grafico_paises_infelices.png", height = 5, width = 8)


world <- map_data("world")

felices= pais_feliz %>% 
  rowid_to_column() %>% 
  mutate(ISO= countrycode(sourcevar = pais_feliz$`Country name`, origin= "country.name", destination = "iso3c")) %>% 
  mutate(country= `Country name`) %>% 
  mutate(score= pais_feliz$`Ladder score`) %>% 
  select(-`Ladder score`, -`Country name`, -rowid) %>% 
  mutate(lon= c(21.22177696, 9.860644341, 8.231933594, -19.77827072, 7.117089748)) %>% 
  mutate(lat= c(63.25912857, 55.51547623, 46.34121323, 63.53657150, 52.88701248)) %>% 
  as.data.frame()

datos= c("Finland" = "purple3","Denmark" = "grey1", 
     "Switzerland"= "green3", "Iceland" = 'darkorange4', "Netherlands" = "deeppink1")

grafico_felices= ggplot()+
  geom_map(data=world, map= world,
    aes(long, lat, map_id= region), 
    color = "grey57", fill = "peachpuff", size = 0.5)+
  geom_point(data=felices, aes(lon, lat, color= country), alpha=1,
             size=2, shape=19)+
  theme_grey()+
  scale_color_manual(name= "Países", values = datos)+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "paleturquoise1", colour = "paleturquoise1"))+
  labs( x = "Longitud", y = "Latitud") +
  labs(title="Los mejores países en la Escala de Felicidad",
       subtitle = "Se muestran los 5 países con mejores puntajes según la encuesta realizada por World Happiness Report.")
grafico_felices
ggsave("figuras/grafico_paises_felices.png", height = 5, width = 8)





