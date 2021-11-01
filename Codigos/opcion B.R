require(esquisse)
require(ggplot2)
library(readr)
datos <- read_delim("data_set/archivos_con_la_data/data-trascurso-de-los-años.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)

