library(tidyverse)
library(sjmisc)
library(survey)

dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_edgardo)

#Mapa de variables + bases
load("Colaboracion con academicos/Insumos para discusion/transformacion_variables/datos_a_usar.RData")
# Funciones
load("Colaboracion con academicos/Insumos para discusion/funciones/funciones_a_usar.RData")

#OBJETO SURVEY
disenno_01 <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, nest = TRUE, 
                        data = subset(elsoc_long, is.na(segmento)== FALSE & 
                                        tipo_atricion == 1 & tipo_caso != 2))

disenno_02 <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, nest = TRUE, 
                        data = subset(elsoc_long, is.na(segmento)== FALSE & 
                                        tipo_atricion == 10 & tipo_caso != 2))

# ---------------FUNCIONES QUE DEVUELVEN LAS TABLAS QUE LUEGO SON GRAFICADAS--------------------

### Genera tablas de frecuencias: ###
frecuencias <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel, REC = NULL) {
  
  var_y <- as.name(var_y)
  
  # Si REC != NULL se recodifica:
  if (!is.null(REC)) {
    data_rec <- data %>% # Recodificar variable a graficar según espeicificado en REC
      mutate(var_y = fct_collapse(!!var_y, 
                                    '1' = REC,
                                    other_level = '0'))
  } else {
    data_rec <- data %>% 
      mutate(var_y = !!var_y)
  }

  disenno <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, nest = TRUE,
                       data = data_rec)
  
  if (is.null(var_z)){
    by_vars <- as.formula(paste0("~", var_x))
  } else {
    by_vars <- as.formula(paste0("~", var_x,  "+", var_z))
  }
    
  svyby(formula = ~var_y, 
              by = by_vars,
              design = disenno, FUN = svymean, na.rm = TRUE)
  }

#------- grafico de barra -------------------
bar_freq <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL, data = elsoc_panel, 
                            titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL) {
  
  tabla <- frecuencias(var_y = var_y, var_x = var_x, var_z = var_z, data = data, REC = REC )

  var_x <- as.name(var_x)
  var_y <- as.name(var_y)
  var_z <- as.name(var_z)
  
  ggp <- ggplot(tabla, 
                aes(x = !!var_x, y = `var_y1`, fill = !!var_z)) +
    geom_bar(stat = "identity", 
             position = "dodge2") + 
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    labs( y = "Porcentaje",   # titulos de ejes y leyenda
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
          fill = cortar_texto(attr(getElement(data, var_z), which = 'label'), largo = 35)) +
    ggtitle(label = cortar_texto(ifelse(!is.null(titulo), # Titulo y subtítulo del gráfico
                                        titulo,
                                        attr(getElement(data, var_z), which = 'label')), largo = 70),
            subtitle = subtitulo) + 
    geom_text(aes(label = paste0(round(`var_y1`*100), "%")), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    theme_bw()
  
  # Etiqueta de valores de eje fill si se definen
  if (is.null(label_fill)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'B',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'B',
                                      label = lapply(label_fill, cortar_texto, largo = 30))
  } 
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  print(ggp)
}

bar_freq('c01', var_z = 'm0_sexo', REC = c('Nada satisfecho'))
  

