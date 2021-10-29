library(ggplot2)
library(readr)
library(dplyr)

datos_2020 <- read_delim("data_set/recopilacion-2015_2021/2020-datos.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)

datos_2021 <- read_delim("data_set/recopilacion-2015_2021/2021-datos.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)

datos_2020 %>% 
  ggplot(aes(`Regional indicator`, `Ladder score`, fill=`Regional indicator`))+
  geom_bar(stat = "identity")


