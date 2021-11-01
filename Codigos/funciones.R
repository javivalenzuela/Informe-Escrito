require(esquisse)
require(ggplot2)
library(readr)
require(dplyr)
require(tidyverse)
datos <- read_delim("data_set/archivos_con_la_data/data-trascurso-de-los-años.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)

felicidad <- read_delim("data_set/archivos_con_la_data/rank-happ-2020-vs-otros-años_fuente-extra.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

ggplot(felicidad) +
 aes(x = `Regional indicator`, y = `Ladder score`, colour = `Regional indicator`) +
 geom_boxplot(shape = "circle", fill = "#112446") +
 scale_color_manual(values = c(`Central and Eastern Europe` = "#F8766D", 
`Commonwealth of Independent States` = "#D78E0C", `East Asia` = "#A1A400", `Latin America and Caribbean` = "#31B425", 
`Middle East and North Africa` = "#00BD71", `North America and ANZ` = "#00BDBD", `South Asia` = "#20AFEC", 
`Southeast Asia` = "#7C92FE", `Sub-Saharan Africa` = "#DE70F4", `Western Europe` = "#FF61C3")) +
 labs(title = "Nivel de felicidad por zona geográfica",
      subtitle = "Cada boxplot representa la escala de felicidad alcanzado por cada habitante por zona",
      x = "Región geográfica", 
 y = "Nivel de Felicidad", color = "Zona Geográfica") +
 coord_flip() +
 theme_gray() +
 theme(legend.position = "right")
