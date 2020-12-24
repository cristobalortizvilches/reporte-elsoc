rm(list=ls())

#Cargar paquetes
library(tidyverse)
library(sjlabelled)
library(sjmisc)

setwd('C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019')

#Importar bases de datos WIDE
load('Bases/ELSOC_Wide_2016_2019_v1.00_R.RData')

# Nuevo objeto
elsoc_panel_01 <- elsoc_wide_2016_2019

# Base en formato long:
datos_long_01 <- elsoc_panel_01 %>%
    pivot_longer( cols = contains('_w'), names_to = c('.value', 'ola'), names_sep = '_w')

# Copiar etiquetas de valores a base long:
elsoc_aux <- elsoc_panel_01 %>% select(!contains('_w') | contains('_w04'))  # Base auxiliar con nombres de variables igual a formato long para copiar labels
names(elsoc_aux) <- str_replace_all(names(elsoc_aux), c("_w04" = ""))

datos_long_01 <- sjlabelled::copy_labels(df_origin = elsoc_aux, df_new = datos_long_01)

# Agregar etiqueta a ola
datos_long_01$ola <- sjlabelled::set_labels(datos_long_01$ola, 
                                        labels = c('01' = "2016", '02' = "2017", '03' = "2018", '04' = "2019"))

attr(datos_long_01$ola, which = 'label') <- 'Ola de encuesta'
# attr(datos_long_01$ola, which = 'labels') <-  c("2016", "2017", "2018", "2019")
# names(attr(datos_long_01$ola, which = 'labels')) <-  c("2016", "2017", "2018", "2019")

# Quitar a?os copiados de etiquetas de variables
var_labels <- trimws(str_replace_all(sjlabelled::get_label(datos_long_01), 
                     c('\\(2016\\)' = '',
                       '\\(2017\\)' = '',
                       '\\(2018\\)' = '', 
                       '\\(2019\\)' = '' )))

elsoc_long_2016_2019 <- sjlabelled::set_label(datos_long_01, var_labels)

elsoc_long_2016_2019 <- elsoc_long_2016_2019 %>% filter(!is.na(segmento))

save(elsoc_long_2016_2019, var_labels,  file = "Bases/ELSOC_Long_2016_2019_labelled_v2.00.RData")
