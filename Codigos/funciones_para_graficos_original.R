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

# Grafico 1: Arreglar base de datos ---------------------------------------
grafico_1= ranking_felicidad_2021 %>% 
  select(`Ladder score`, `Country name`)

pais_feliz= grafico_1[1:5,]
aux_1= grafico_1[order(grafico_1$`Ladder score`, grafico_1$`Country name`),]
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

# Gráfico 1: Creación de mapas --------------------------------------------
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
  labs(title="Los peores países según la Escala de Felicidad de Cantrill",
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
  labs(title="Los mejores países segúin la Escala de Felicidad de Cantrill",
       subtitle = "Se muestran los 5 países con mejores puntajes según la encuesta realizada por World Happiness Report.")
grafico_felices
ggsave("figuras/grafico_paises_felices.png", height = 5, width = 8)




#GRÁFICOS PREGUNTA 2 ------------------------------------------------------

# Gráfico 2: Libertad_1 variables -----------------------------------------

paises= c("Zimbabwe", "Rwanda", "Botswana", "Lesotho", "Finland",
          "Denmark", "Switzerland", "Iceland", "Netherlands", "Malawi")

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

tabla_libertad= tabla %>% 
  add_header_lines(values= "Tabla 1.1: Puntajes de factores de los índices de libertad de los 10 países analizados") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>% 
  add_footer_lines("Datos recuperados del informe: 'The Human Freedom Index 2020'") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Estado de derecho", "Seguridad y Protección de las mujeres",
          "Desplazamiento femenino", "Libertad de religión", "Libertad de prensa",
          "Relaciones entre mismo sexo", "Dinero sólido", "Expresión e información",
          "Confianza en la policía", "Inteferencia militar en estado de derecho y política"),
     bg="gray92", part="body")

# Gráfico 2: Libertad_2 con índices -----------------------------------------
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

tabla_libertad_2= tabla_2 %>% 
  add_header_lines(values= "Tabla 1.2: Índices según puntaje y ranking de los 10 países analizados") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>%
  add_footer_lines("Datos recuperados del informe: 'The Human Freedom Index 2020'") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Libertad personal (ranking)", "Libertad personal (puntaje)",
          "Libertad humana (ranking)", "Libertad humana (puntaje)",
          "Libertad económica (ranking)", "Libertad económica (puntaje)"),
     bg="gray92", part="body")

# Gráfico 2: Reporte: World Happiness Report ---------------------------------------
nuevos_datos= ranking_felicidad_2021 %>% 
  filter(`Country name` %in% paises)
reporte= nuevos_datos %>% 
  mutate(Países= `Country name`) %>% 
  mutate("Puntaje en Escalera de Felicidad"= round(`Ladder score`, 3)) %>% 
  mutate("PIB per cápita registrado"= round(`Logged GDP per capita`,3)) %>% 
  mutate("Soporte Social"= round(`Social support`,3)) %>% 
  mutate("Esperanza de vida"= round(`Healthy life expectancy`,3)) %>% 
  mutate("Libertad para tomar decisiones de vida"= round(`Freedom to make life choices`,3)) %>% 
  mutate(Generosidad= round(Generosity,3)) %>% 
  mutate("Percepción de corrupción"= round(`Perceptions of corruption`,3)) %>% 
  select(`Países`, `Puntaje en Escalera de Felicidad`, 
         `PIB per cápita registrado`, `Soporte Social`, `Esperanza de vida`, 
         `Libertad para tomar decisiones de vida`, `Generosidad`, `Percepción de corrupción`) %>% 
  as.data.frame()
  
tabla_reporte_aux= reporte %>% 
  head(10) %>% 
  flextable() %>% 
  flextable::set_table_properties(width = .5, layout = "autofit") %>% 
  flextable::fontsize(size=12) %>% 
  flextable::theme_zebra %>% 
  flextable::align_text_col(align = "center") %>% 
  flextable::fontsize(size=13, part= "header") %>% 
  flextable::border_outer() %>% 
  flextable::border_inner() %>% 
  flextable::color(part= "body", color= "grey0")

tabla_reporte= tabla_reporte_aux %>% 
  add_header_lines(values= "Tabla 1.3: Resultados de las variables del Informe de Felicidad en el mundo") %>% 
  fontsize(size= 15, part="header", i=1) %>% 
  color(part="header", color = "gray0", i=1) %>% 
  italic(italic=TRUE, part="header") %>%
  add_footer_lines("Datos recuperados del informe: 'The World Happiness Report'") %>% 
  bg(bg = "#e05297", part = "header") %>% 
  bg(j= c("Países", "Puntaje en Escalera de Felicidad", "PIB per cápita registrado",
          "Soporte Social", "Esperanza de vida", "Libertad para tomar decisiones de vida",
          "Generosidad", "Percepción de corrupción"),
     bg="gray92", part="body")

# Gráfico 2: Guardar los 3 gráficos en png --------------------------------
require(webshot)
webshot::install_phantomjs()

tabla_libertad %>% 
  flextable::save_as_image(zoom=18, expand= 13, path= "libertad_1.png", webshot = "webshot")

tabla_libertad_2 %>% 
  flextable::save_as_image(zoom=18, expand=13, path= "libertad_2.png", webshot= "webshot")

tabla_reporte %>% 
  flextable::save_as_image(zoom=18, expand=13, path= "reporte_felicidad.png", webshot= "webshot")


#GRÁFICOS PREGUNTA 3-------------------------------------------------------
datos= nuevos_datos %>%
  filter(`Country name` %in% paises) %>% 
  select(`Country name`,`Ladder score`,`Explained by: Freedom to make life choices`, `Explained by: Generosity`,
         `Explained by: Social support`, `Explained by: Perceptions of corruption`,
         `Explained by: Log GDP per capita`, `Explained by: Healthy life expectancy`,
         `Dystopia + residual`) %>% 
  mutate(Países=`Country name`) %>% 
  mutate("Puntaje Escalera"= `Ladder score`) %>% 
  mutate("Explicado por: 'Libertad para tomar decisiones"= `Explained by: Freedom to make life choices`) %>% 
  mutate("Explicado por: 'Generosidad"= `Explained by: Generosity`) %>% 
  mutate("Explicado por: 'Apoyo social'"= `Explained by: Social support`) %>% 
  mutate("Explicado por: 'Percepción de corrupción'"= `Explained by: Perceptions of corruption`) %>% 
  mutate("Explicado por: 'PIB per cápita registrado'"= `Explained by: Log GDP per capita`) %>% 
  mutate("Explicado por: 'Esperanza de vida'"=`Explained by: Healthy life expectancy`) %>% 
  mutate("Dystopia + residuo"=`Dystopia + residual`) %>% 
  
  select(-`Country name`, -`Ladder score`,-`Explained by: Freedom to make life choices`, -`Explained by: Generosity`,
                -`Explained by: Social support`, -`Explained by: Perceptions of corruption`,
                -`Explained by: Log GDP per capita`, -`Explained by: Healthy life expectancy`,
                -`Dystopia + residual`) %>% 
  mutate(Valores= rowSums(select(., contains(c("Explicado","residuo"))))) %>% 
  as.data.frame()

clase= c("Explicado por: 'Libertad para tomar decisiones",
         "Explicado por: 'Apoyo social'", "Explicado por: 'Generosidad",
  "Explicado por: 'Percepción de corrupción'", "Explicado por: 'PIB per cápita registrado'", 
         "Explicado por: 'Esperanza de vida'", "Dystopia + residuo")

paises= c("Finland", "Denmark", "Switzerland","Iceland", "Netherlands",
          "Malawi", "Lesotho", "Botswana", "Rwanda", "Zimbabwe")







#Gráfico 4----------------------------------------------

u=libertad %>% 
  as.data.frame
uu= libertad_2 %>% 
  as.data.frame()
uuu= reporte %>% 
  as.data.frame()
sal= merge(x = u, y = uu, by = "Países")
salida= merge(x=sal, y= uuu, by="Países")

salida %>% 
  ggplot(aes(`Libertad económica (puntaje)`, `Puntaje en Escalera de Felicidad`), color=Países)+
  geom_point()

  


