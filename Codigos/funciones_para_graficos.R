## LIBRERIAS ##----
library(readxl)
library(readr)
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
library(RColorBrewer)
library(scales)
## Carga de datos ##----
economia_2019= read_delim("data_set/OPCION 2/economicdata2019-2019.csv", 
                          delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

evolucion_felicidad <- read_excel("data_set/OPCION 2/evolucion_felicidad.xls", na = "NA")

indice_de_libertad_2021 <- read_excel("data_set/OPCION 2/human-freedom-index-2020 (1).xlsx", 
                                      na = "NA")
mortalidad_2020 <- read_excel("data_set/OPCION 2/MortalityDataWHR2021C2.xlsx", 
                              na = "NA")

ranking_felicidad_2021 <- read_excel("data_set/OPCION 2/ranking_felicidad_2021.xls", 
                                     na = "NA")

## GRÁFICOS NIVEL MUNDIAL ## ----

grafico_felicidad= ranking_felicidad_2021 %>% 
  mutate(Región= countrycode(sourcevar = `Country name`, origin= "country.name", destination = "continent")) %>% 
  mutate(Países= `Country name`) %>% 
  mutate("Puntaje en Escalera de Cantrill"= round(`Ladder score`, 3)) %>% 
  mutate("PIB per cápita registrado"= round(`Logged GDP per capita`,3)) %>% 
  mutate("Soporte Social"= round(`Social support`,3)) %>% 
  mutate("Esperanza de vida"= round(`Healthy life expectancy`,3)) %>% 
  mutate("Libertad para tomar decisiones de vida"= round(`Freedom to make life choices`,3)) %>% 
  mutate(Generosidad= round(Generosity,3)) %>% 
  mutate("Percepción de corrupción"= round(`Perceptions of corruption`,3)) %>% 
  select(`Países`, Región, `Puntaje en Escalera de Cantrill`, 
         `PIB per cápita registrado`, `Soporte Social`, `Esperanza de vida`, 
         `Libertad para tomar decisiones de vida`, `Generosidad`, `Percepción de corrupción`) %>% 
  as.data.frame()

grafico_libertad= indice_de_libertad_2021 %>% 
  select(Countries, Year, `PERSONAL FREEDOM (Rank)`, `PERSONAL FREEDOM (Score)`,
         `HUMAN FREEDOM (Score)`,`HUMAN FREEDOM (Rank)`,
         `ECONOMIC FREEDOM (Rank)`, `ECONOMIC FREEDOM (Score)`) %>% 
  mutate(Países= Countries) %>% 
  filter(Year==2018) %>% 
  mutate(Año= Year) %>% 
  mutate(Región= countrycode(sourcevar = Countries, origin= "country.name", destination = "continent")) %>% 
  mutate("Libertad personal (ranking)"= as.numeric(`PERSONAL FREEDOM (Rank)`)) %>% 
  mutate("Libertad personal (puntaje)"= round(as.double(`PERSONAL FREEDOM (Score)`),3)) %>% 
  mutate("Libertad humana (ranking)"= as.numeric(`HUMAN FREEDOM (Rank)`)) %>%
  mutate("Libertad humana (puntaje)"= as.numeric(`HUMAN FREEDOM (Score)`)) %>% 
  mutate("Libertad económica (ranking)"= as.numeric(`ECONOMIC FREEDOM (Rank)`)) %>% 
  mutate("Libertad económica (puntaje)"= as.numeric(`ECONOMIC FREEDOM (Score)`)) %>% 
  select(-Countries,-Year, -`PERSONAL FREEDOM (Rank)`, -`PERSONAL FREEDOM (Score)`,
         -`HUMAN FREEDOM (Score)`,-`HUMAN FREEDOM (Rank)`,
         -`ECONOMIC FREEDOM (Rank)`, -`ECONOMIC FREEDOM (Score)`) %>% 
  as.data.frame()

graficos_mundiales= merge(grafico_felicidad, grafico_libertad,)

cbbPalette <- c("#000000", "darksalmon", "forestgreen", "purple3", "chocolate4", "#0072B2", "#90005D")

graficos_mundiales%>% 
  gather(Total, Value, -Región, -Países, -Año, -`Libertad personal (ranking)`,
         -`Libertad humana (ranking)`, -`Libertad económica (ranking)`, -`Esperanza de vida`,
         -`PIB per cápita registrado`, -`Puntaje en Escalera de Cantrill`) %>% 
  ggplot(aes(Value, Región, fill= Total))+
  geom_bar(position = "dodge", stat= "identity")+
  xlim(0,10)+
  scale_fill_manual(values = cbbPalette)+
  theme_igray()+
  theme(legend.position = "right")+
  labs(title = "Puntaje de los indicadores para conocer el nivel de Felicidad y Bienestar por continente",
       subtitle = "Cada barra representa el puntaje alcanzado de 0 a 10 en la Escala de Felcidad de Cantrill y libertades individuales.",
       x = "Puntaje en la Escalera de Cantrill por cada uno de los indicadores",
       y = "Continentes",
       caption = "Elaboración propia a partir de los datos de worldhappiness.report y cato.org",
       fill= "Indicadores")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRAFICO EN CARPETA ###
ggsave("figuras/grafico_indicadores_mundiales.png", height = 5, width = 10)
###

determinantes_felicidad= ranking_felicidad_2021 %>%
  select(`Country name`,`Regional indicator`,
         `Ladder score`,`Explained by: Freedom to make life choices`, `Explained by: Generosity`,
         `Explained by: Social support`, `Explained by: Perceptions of corruption`,
         `Explained by: Log GDP per capita`, `Explained by: Healthy life expectancy`,
         `Dystopia + residual`) %>% 
  mutate(Región= ifelse(`Country name`== "Kosovo",
                        "Europe", `Regional indicator`)) %>% 
  mutate(Región= ifelse(`Country name`!= "Kosovo", countrycode(sourcevar = `Country name`, origin= "country.name", destination = "continent"),
                        Región)) %>% 
  mutate(Países=`Country name`) %>% 
  mutate("Puntaje en Escalera de Cantrill"= `Ladder score`) %>% 
  mutate("Explicado por: 'Libertad para tomar decisiones"= `Explained by: Freedom to make life choices`) %>% 
  mutate("Explicado por: 'Generosidad'"= `Explained by: Generosity`) %>% 
  mutate("Explicado por: 'Apoyo social'"= `Explained by: Social support`) %>% 
  mutate("Explicado por: 'Percepción de corrupción'"= `Explained by: Perceptions of corruption`) %>% 
  mutate("Explicado por: 'PIB per cápita registrado'"= `Explained by: Log GDP per capita`) %>% 
  mutate("Explicado por: 'Esperanza de vida'"=`Explained by: Healthy life expectancy`) %>% 
  mutate("Dystopia + residuo"=`Dystopia + residual`) %>% 
  select(-`Country name`,-`Ladder score`,-`Explained by: Freedom to make life choices`,
         -`Explained by: Generosity`,-`Explained by: Social support`, 
         -`Explained by: Perceptions of corruption`,-`Explained by: Log GDP per capita`, 
         -`Explained by: Healthy life expectancy`, -`Dystopia + residual`, -`Regional indicator`)

determinantes_felicidad %>% 
  gather(Total, Values, -Región, -`Puntaje en Escalera de Cantrill`, -Países, `Explicado por: 'Libertad para tomar decisiones`,
         `Explicado por: 'Percepción de corrupción'`, `Explicado por: 'PIB per cápita registrado'`,
         `Explicado por: 'Esperanza de vida'`, `Explicado por: 'Generosidad'`, `Explicado por: 'Apoyo social'`) %>% 
  ggplot(aes(Values, Región, fill=Total))+
  geom_bar(position = "dodge", stat= "identity")+
  scale_fill_brewer(type = "qual" , palette = "Set1")+
  theme_igray()+
  theme(legend.position = "right")+
  labs(title = "Incidencia de cada determinante en la Escalera de Cantrill",
       subtitle = "Cada barra representa la incidencia de cada variable en el puntaje de felicidad obtenido según la Escalera de Cantrill.",
       x = "Incidencia del puntaje en la Escalera de Cantrill",
       y = "Continentes",
       caption = "Elaboración propia a partir de datos de worldhappiness.report y cato.org",
       fill= "Indicadores")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/grafico_determinantes_felicidad_mundial.png", height = 5, width = 10)
### 

cbp1= c("purple", "black", "blue", "#009E73",
        "red")

graficos_mundiales%>% 
  ggplot(aes(`Puntaje en Escalera de Cantrill`, `PIB per cápita registrado`, color= Región))+
  geom_point()+
  xlim(0,10)+
  scale_y_continuous(labels = scales::dollar)+
  theme_igray()+
  scale_color_manual(values = cbp1)+
  labs(title = "Relación PIB per cápita registrado v/s Puntaje en la Escalera de Cantrill",
       subtitle = "Cada punto representa la relación entre PIB pér cápita y el puntaje obtenido en la Escala de Cantrill por continente.",
       x = "Puntaje en la Escalera de Cantrill",
       y = "PIB pér cápita registrado en miles de dólares",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Continentes")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/relacion_pib_felicidad.png", height = 5, width = 10)
###

graficos_mundiales%>% 
  ggplot(aes(`Puntaje en Escalera de Cantrill`, `PIB per cápita registrado`, color= Región))+
  geom_point()+
  xlim(0,10)+
  scale_y_continuous(labels = scales::dollar)+
  theme_igray()+
  scale_color_manual(values = cbp1)+
  labs(title = "Relación PIB per cápita registrado v/s Puntaje en la Escalera de Cantrill",
       subtitle = "Cada punto representa la relación entre PIB pér cápita y el puntaje obtenido en la Escala de Cantrill separado por continente.",
       x = "Puntaje en la Escalera de Cantrill",
       y = "PIB pér cápita registrado en miles de dólares",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Continentes")+
  facet_wrap(vars(Región), scales= "free", ncol=3, nrow=3)+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/relacion_pib_felicidad_SEPARADO.png", height = 6, width = 12)
###

graficos_mundiales%>% 
  ggplot(aes(`Puntaje en Escalera de Cantrill`, `Libertad personal (puntaje)`, color= Región))+
  geom_point()+
  xlim(0,10)+
  ylim(0,10)+
  theme_igray()+
  scale_color_manual(values = cbp1)+
  labs(title = "Puntaje de la Libertad personal v/s Puntaje en la Escala de Cantrill",
       subtitle = "Cada punto representa la relación entre los puntajes del Índice de Libertad personal y la Escala de Cantrill por continente.",
       x = "Puntaje en la Escalera de Cantrill",
       y = "Libertad personal",
       caption = "Elaboración propia a partir de datos de worldhappiness.report y cato.org",
       fill= "Continentes")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)) 

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/libertad_personal_y_felicidad.png", height = 5, width = 10)
###

graficos_mundiales%>% 
  ggplot(aes(`Puntaje en Escalera de Cantrill`, `Libertad personal (puntaje)`, color= Región))+
  geom_point()+
  xlim(0,10)+
  ylim(0,10)+
  theme_igray()+
  scale_color_manual(values = cbp1)+
  labs(title = "Puntaje de la Libertad personal v/s Puntaje en la Escala de Cantrill",
       subtitle = "Cada punto representa la relación entre los puntajes del Índice de libertad y la Escala de Cantrill separado por continente.",
       x = "Puntaje en la Escalera de Cantrill",
       y = "Libertad personal",
       caption = "Elaboración propia a partir de datos de worldhappiness.report y cato.org",
       fill= "Continentes")+
  facet_wrap(vars(Región), scales="free", nrow= 3, ncol=3)+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)) 

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/libertad_personal_y_felicidad_SEPARADO.png", height = 6, width = 12)
###

muertes_1= c("#B980A7", "#36628F")
muertes %>% 
  gather(Total, Values, -Países, -Región, `Población 2020`,
         `Población 2019`,-`COVID-19: Muertes cada 100.000 habitantes en 2020`,
         -`Edad promedio`, -`Índice de Gini según Ingresos`) %>% 
  ggplot(aes(Values, Región, fill= Total))+
  geom_bar(stat = "identity", position= position_dodge(width = 0.5))+
  coord_flip()+
  scale_x_continuous(labels = label_comma(), breaks = breaks_width(100000000))+
  theme_igray()+
  scale_fill_manual(values = muertes_1)+
  labs(title = "Población 2019 v/s Población 2020",
       subtitle = "Cada barra es la representación en millones de habitantes entre los años 2019 y 2020 por continente.",
       x = "Continentes",
       y = "Millones de habitantes",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Población")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/muertes_2019_vs_2020_continente.png", height = 5, width = 10)
###

muertes %>% 
  gather(Total, Values, -Países, -Región, -`Población 2020`,
         -`Población 2019`,`COVID-19: Muertes cada 100.000 habitantes en 2020`,
         -`Edad promedio`, -`Índice de Gini según Ingresos`) %>% 
  ggplot(aes(Values, Región, fill= Región))+
  geom_bar(stat = "identity", position= position_dodge(width = 0.5))+
  coord_flip()+
  scale_x_continuous(labels = label_number_auto(), breaks = breaks_width(10))+
  scale_fill_brewer(type = "qual" , palette = "Dark2")+
  theme_igray()+
  theme(legend.position = "right")+
  labs(title = "Decesos por COVID-19",
       subtitle = "Se representa mediante un gráfico de barras la cantidad de muertes por continente cada 100.000 habitantes en 2020.",
       y = "Continentes",
       x = "Decesos",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Continentes")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO EN CARPETA ###
ggsave("figuras/muertes_COVID_continente.png", height = 5, width = 10)
###


# GRAFICOS PAISES (10 ANÁLISIS) -------------------------------------------

limpieza= ranking_felicidad_2021 %>% 
  select(`Ladder score`, `Country name`)

pais_feliz= limpieza[1:5,]
aux_1= limpieza[order(limpieza$`Ladder score`, limpieza$`Country name`),]
pais_infeliz= aux_1[2:6,]
arreglo= aux_1[1:6,]

options(digits = 10)

infelices= pais_infeliz %>% 
  rowid_to_column() %>% 
  mutate(group= rowid) %>% 
  mutate(ISO= countrycode(sourcevar = pais_infeliz$`Country name`, origin = "country.name", destination = "iso3c")) %>% 
  mutate(country= `Country name`) %>% 
  mutate(score= pais_infeliz$`Ladder score`) %>% 
  select(-`Ladder score`, -`Country name`, -rowid) %>% 
  mutate(lon= c(29.154857, 29.873888, 24.684866, 28.233608, 34.301525)) %>% 
  mutate(lat= c(-19.015438, -1.940278, -22.328474, -29.609988, -13.254308)) %>% 
  as.data.frame()

world <- map_data("world")
a= c("Zimbabwe" = "deepskyblue3", "Rwanda"= "chartreuse4",
     "Botswana"= 'brown4',"Lesotho"= "red", "Malawi" = "grey8")
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
  labs(title="Los países más infelices según la Escalera de Felicidad de Cantrill",
       subtitle = "Se muestran los 5 países con peores puntajes según la encuesta realizada por World Happiness Report.",
       caption= "Elaboración propia a partir de los datos de la librería 'Map' de R y worldhappiness.report")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

grafico_infelices

### GUARDAR GRÁFICO CARPETA ###
ggsave("figuras/grafico_paises_infelices.png", height = 5, width = 8)
###

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
  labs(title="Los países más felices según la Escalera de Felicidad de Cantrill",
       subtitle = "Se muestran los 5 países con mejores puntajes según la encuesta realizada por World Happiness Report.",
       caption= "Elaboración propia a partir de los datos de la librería 'Map' de R y worldhappiness.report")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

grafico_felices

### GUARDAR GRÁFICO CARPETA
ggsave("figuras/grafico_paises_felices.png", height = 5, width = 8)
###

paises= c("Finland", "Denmark", "Switzerland","Iceland", "Netherlands",
          "Malawi", "Lesotho", "Botswana", "Rwanda", "Zimbabwe")

datos= nuevos_datos %>%
  filter(`Country name` %in% paises) %>% 
  select(`Country name`,`Ladder score`,`Explained by: Freedom to make life choices`, `Explained by: Generosity`,
         `Explained by: Social support`, `Explained by: Perceptions of corruption`,
         `Explained by: Log GDP per capita`, `Explained by: Healthy life expectancy`,
         `Dystopia + residual`) %>% 
  mutate(Países=`Country name`) %>% 
  mutate("Puntaje Escalera de Cantrill"= `Ladder score`) %>% 
  mutate("Explicado por: 'Libertad para tomar decisiones"= `Explained by: Freedom to make life choices`) %>% 
  mutate("Explicado por: 'Generosidad'"= `Explained by: Generosity`) %>% 
  mutate("Explicado por: 'Apoyo social'"= `Explained by: Social support`) %>% 
  mutate("Explicado por: 'Percepción de corrupción'"= `Explained by: Perceptions of corruption`) %>% 
  mutate("Explicado por: 'PIB per cápita registrado'"= `Explained by: Log GDP per capita`) %>% 
  mutate("Explicado por: 'Esperanza de vida'"=`Explained by: Healthy life expectancy`) %>% 
  mutate("Dystopia + residuo"=`Dystopia + residual`) %>% 
  select(-`Country name`,-`Ladder score`,-`Explained by: Freedom to make life choices`,
         -`Explained by: Generosity`,-`Explained by: Social support`, 
         -`Explained by: Perceptions of corruption`,-`Explained by: Log GDP per capita`, 
         -`Explained by: Healthy life expectancy`, -`Dystopia + residual`)

incidencia_por_pais= datos %>% 
  gather(Total, Values, -`Puntaje Escalera de Cantrill`, -Países, `Explicado por: 'Libertad para tomar decisiones`,
         `Explicado por: 'Percepción de corrupción'`, `Explicado por: 'PIB per cápita registrado'`,
         `Explicado por: 'Esperanza de vida'`, `Explicado por: 'Generosidad'`, `Explicado por: 'Apoyo social'`) %>% 
  ggplot(aes(Values, Países, fill=Total))+
  geom_bar(position = "dodge", stat= "identity")+
  scale_fill_brewer(type = "qual" , palette = "Set1")+
  theme_igray()+
  theme(legend.position = "right")+
  labs(title = "Incidencia de cada determinante en puntaje de la Escalera de Felicidad de Cantrill",
       subtitle = "Cada barra representa la incidencia de cada variable en el puntaje obtenido en la Escalera de Cantrill.",
       x = "Puntaje en la Escala de Cantrill por cada indicador",
       y = "Continentes",
       caption = "Elaboración propia a partir de datos de worldhappiness.report y cato.org",
       fill= "Indicadores")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

incidencia_por_pais

### GUARDAR GRÁFICO CARPETA
ggsave("figuras/grafico_incidencia_por_pais.png", height = 5, width = 10)
###

muertes_2= c("#982A6A", "#E0AA85")
muertes %>% 
  filter(Países %in% paises) %>% 
  gather(Total, Values, -Países, -Región, `Población 2020`,
         `Población 2019`, -`COVID-19: Muertes cada 100.000 habitantes en 2020`, 
         -`Índice de Gini según Ingresos`, -`Edad promedio`) %>% 
  ggplot(aes(Values, Países, fill=Total))+
  geom_bar(stat = "identity", position= position_dodge(width = 0.6))+
  coord_flip()+
  theme_igray()+
  scale_x_continuous(labels = label_number(), breaks = breaks_width(1000000))+
  scale_fill_manual(values = muertes_2)+
  labs(title = "Población 2019 v/s Población 2020",
       subtitle = "Cada barra es la comparación en millones de habitantes entre los años 2019 y 2020 en los 10 países analizados.",
       x = "Continentes",
       y = "Millones de habitantes",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Población")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

### GUARDAR GRÁFICO CARPETA
ggsave("figuras/grafico_poblacion_2019_2020_paises.png", height = 5, width = 10)
###

muertes_3= c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
             "black", "#A65628", "lightseagreen", "#999999", "#F781BF")

muertes %>% 
  filter(Países %in% paises) %>% 
  gather(Total, Values, -Países, -Región, -`Población 2020`,
         -`Población 2019`, `COVID-19: Muertes cada 100.000 habitantes en 2020`, 
         -`Índice de Gini según Ingresos`, -`Edad promedio`) %>% 
  ggplot(aes(Values, Países, fill=Países))+
  geom_bar(stat = "identity", position= position_dodge(width = 0.6))+
  coord_flip()+
  theme_igray()+
  scale_x_continuous(labels = label_number(), breaks = breaks_width(5))+
  scale_fill_manual(values = muertes_3)+
  labs(title = "Decesos por COVID-19",
       subtitle = "Cada barra representa la cantidad de decesos por cada 100 000 habitantes por país en el año 2020.",
       x = "Decesos",
       y = "Países",
       caption = "Elaboración propia a partir de datos de worldhappiness.report",
       fill= "Población")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))


### GUARDAR GRÁFICO CARPETA
ggsave("figuras/decesos_COVID_por_paises.png", height = 5, width = 10)
###

# TABLAS ------------------------------------------------------------------

paises= c("Zimbabwe", "Rwanda", "Botswana", "Lesotho", "Finland",
          "Denmark", "Switzerland", "Iceland", "Netherlands", "Malawi")

## TABLA 1 ##

libertad= indice_de_libertad_2021 %>% 
  filter(Countries %in% paises, Year==2018) %>% 
  select(Countries, `Rule of Law`,`Women's Security & Safety`, `Women's Movement`, 
         `Freedom of Religion`,`Media Freedom`, `Same-Sex Relationships`, 
         `Sound Money`, `Expression & Information`, `Reliability of police`, 
         `Military interference in rule of law and politics`) %>% 
  mutate(Países= Countries) %>% 
  mutate("Estado de derecho"= round(as.double(`Rule of Law`),3)) %>% 
  mutate("Seguridad y Protección de las mujeres"= as.double(`Women's Security & Safety`)) %>% 
  mutate("Desplazamiento femenino"= as.double(`Women's Movement`)) %>% 
  mutate("Libertad de religión"= round(as.double(`Freedom of Religion`),3)) %>% 
  mutate("Libertad de prensa"= as.double(`Media Freedom`)) %>% 
  mutate("Relaciones entre mismo sexo"= as.double(`Same-Sex Relationships`)) %>% 
  mutate("Dinero sólido"= round(as.double(`Sound Money`),3)) %>% 
  mutate("Expresión e información"= round(as.double(`Expression & Information`),3)) %>% 
  mutate("Confianza en la policía"= round(as.double(`Reliability of police`),3)) %>% 
  mutate("Inteferencia militar en estado de derecho y política"= round(as.double(`Military interference in rule of law and politics`),3)) %>% 
  select(-Countries, -`Rule of Law`,-`Women's Security & Safety`, -`Women's Movement`, 
         -`Freedom of Religion`,-`Media Freedom`, -`Same-Sex Relationships`, 
         -`Sound Money`, -`Expression & Information`, -`Reliability of police`, 
         -`Military interference in rule of law and politics`) %>% 
  as.data.frame()

tabla= libertad %>% 
  head(10) %>% 
  flextable() %>% 
  flextable::set_table_properties(width = .5, layout = "autofit") %>% 
  flextable::fontsize(size=12) %>% 
  flextable::theme_vanilla() %>% 
  flextable::align_text_col(align = "center") %>% 
  flextable::fontsize(size=13, part= "header") %>% 
  flextable::border_outer() %>% 
  flextable::border_inner() %>% 
  flextable::color(part= "body", color= "black")

tabla_libertad_caracteristicas= tabla %>% 
  add_header_lines(values= "Tabla 1.1: Puntajes de los factores que influyen en los índices de bienestar y libertad en 10 países") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>% 
  add_footer_lines("Datos recuperados del informe: 'The Human Freedom Index 2020'.") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Estado de derecho", "Seguridad y Protección de las mujeres",
          "Desplazamiento femenino", "Libertad de religión", "Libertad de prensa",
          "Relaciones entre mismo sexo", "Dinero sólido", "Expresión e información",
          "Confianza en la policía", "Inteferencia militar en estado de derecho y política"),
     bg="gray92", part="body")

tabla_libertad_caracteristicas

## TABLA 2 ##

libertad_2= indice_de_libertad_2021 %>% 
  filter(Countries %in% paises, Year==2018) %>% 
  select(Countries, `PERSONAL FREEDOM (Rank)`, `PERSONAL FREEDOM (Score)`,
         `HUMAN FREEDOM (Score)`,`HUMAN FREEDOM (Rank)`,
         `ECONOMIC FREEDOM (Rank)`, `ECONOMIC FREEDOM (Score)`) %>% 
  mutate(Países= Countries) %>% 
  mutate("Libertad personal (ranking)"= as.numeric(`PERSONAL FREEDOM (Rank)`)) %>% 
  mutate("Libertad personal (puntaje)"= round(as.double(`PERSONAL FREEDOM (Score)`),3)) %>% 
  mutate("Libertad humana (ranking)"= as.numeric(`HUMAN FREEDOM (Rank)`)) %>%
  mutate("Libertad humana (puntaje)"= as.numeric(`HUMAN FREEDOM (Score)`)) %>% 
  mutate("Libertad económica (ranking)"= as.numeric(`ECONOMIC FREEDOM (Rank)`)) %>% 
  mutate("Libertad económica (puntaje)"= as.numeric(`ECONOMIC FREEDOM (Score)`)) %>% 
  select(-Countries, -`PERSONAL FREEDOM (Rank)`, -`PERSONAL FREEDOM (Score)`,
         -`HUMAN FREEDOM (Score)`,-`HUMAN FREEDOM (Rank)`,
         -`ECONOMIC FREEDOM (Rank)`, -`ECONOMIC FREEDOM (Score)`) %>% 
  as.data.frame()

tabla_2= libertad_2 %>% 
  head(10) %>% 
  flextable() %>% 
  flextable::set_table_properties(width = .5, layout = "autofit") %>% 
  flextable::fontsize(size=12) %>% 
  flextable::theme_vanilla() %>% 
  flextable::align_text_col(align = "center") %>% 
  flextable::fontsize(size=13, part= "header") %>% 
  flextable::border_outer() %>% 
  flextable::border_inner() %>% 
  flextable::color(part= "body", color= "black")

tabla_libertad_indices= tabla_2 %>% 
  add_header_lines(values= "Tabla 1.2: Puntaje y ranking de los índices de bienestar y libertad en los 10 países analizados") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>%
  add_footer_lines("Datos recuperados del informe: 'The Human Freedom Index 2020'.") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Libertad personal (ranking)", "Libertad personal (puntaje)",
          "Libertad humana (ranking)", "Libertad humana (puntaje)",
          "Libertad económica (ranking)", "Libertad económica (puntaje)"),
     bg="gray92", part="body")

tabla_libertad_indices

## TABLA 3 ##

nuevos_datos= ranking_felicidad_2021 %>% 
  filter(`Country name` %in% paises)

reporte= nuevos_datos %>% 
  mutate(Países= `Country name`) %>% 
  mutate("Puntaje en la Escalera de Cantrill"= round(`Ladder score`, 3)) %>% 
  mutate("PIB per cápita registrado"= round(`Logged GDP per capita`,3)) %>% 
  mutate("Soporte Social"= round(`Social support`,3)) %>% 
  mutate("Esperanza de vida"= round(`Healthy life expectancy`,3)) %>% 
  mutate("Libertad para tomar decisiones de vida"= round(`Freedom to make life choices`,3)) %>% 
  mutate(Generosidad= round(Generosity,3)) %>% 
  mutate("Percepción de corrupción"= round(`Perceptions of corruption`,3)) %>% 
  select(`Países`, `Puntaje en la Escalera de Cantrill`, 
         `PIB per cápita registrado`, `Soporte Social`, `Esperanza de vida`, 
         `Libertad para tomar decisiones de vida`, `Generosidad`, `Percepción de corrupción`) %>% 
  as.data.frame()

tabla_reporte_aux= reporte %>% 
  head(10) %>% 
  flextable() %>% 
  flextable::theme_vanilla() %>% 
  flextable::set_table_properties(width = .5, layout = "autofit") %>% 
  flextable::fontsize(size=12) %>% 
  flextable::align_text_col(align = "center") %>% 
  flextable::fontsize(size=13, part= "header") %>% 
  flextable::border_outer() %>% 
  flextable::border_inner() %>% 
  flextable::color(part= "body", color= "grey0")

tabla_reporte= tabla_reporte_aux %>% 
  add_header_lines(values= "Tabla 1.3: Resultados de los 6 indicadores del nivel de felicidad en los países analizados") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>%
  align_text_col(align = "center") %>% 
  add_footer_lines("Datos recuperados del informe: 'The World Happiness Report'.") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Puntaje en la Escalera de Cantrill", "PIB per cápita registrado",
          "Soporte Social", "Esperanza de vida", "Libertad para tomar decisiones de vida",
          "Generosidad", "Percepción de corrupción"),
     bg="gray92", part="body")

tabla_reporte

require(webshot)
webshot::install_phantomjs()

tabla_libertad %>% 
  flextable::save_as_image(zoom=18, expand= 13, path= "libertad_caracteristica.png", webshot = "webshot")

tabla_libertad_2 %>% 
  flextable::save_as_image(zoom=18, expand=13, path= "libertad_indices.png", webshot= "webshot")

tabla_reporte %>% 
  flextable::save_as_image(zoom=18, expand=13, path= "reporte_felicidad.png", webshot= "webshot")




