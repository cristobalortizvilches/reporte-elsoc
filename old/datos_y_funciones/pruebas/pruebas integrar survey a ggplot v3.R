library(tidyverse)
library(sjmisc)
library(survey)
dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_carlos)

#Mapa de variables + bases
load("Presentacion/RCS2020/Presentacion final/Rmarkdowns/funciones_y_otros/datos_a_usar.RData")

#------GRÁFICO DE BARRAS de frecuencias de var_y por var_x ------
gr.bar.freq <- function(var_y, var_x = 'ola', data = elsoc_panel, ponderador = 'ponderador02',
                        posicion = 'dodge2',
                        titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_x) & !is.na(!!var_y)) %>% 
    group_by(!!var_x, !!var_y) %>% 
    summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
    group_by(!!var_x) %>% 
    mutate(n1 = sum(n2, na.rm = TRUE),
           perc = n2/n1, 
           pctlabel = paste0(round(perc*100), "%")) %>% 
    ungroup() %>% 
    drop_na()
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico, 
                aes(x = !!var_x, y = perc, fill = !!var_y)) +
    geom_col(position = posicion) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    # Titulo del gráfico
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) +
    theme_bw() + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Etiqueta de valores de eje y si se definen
  if (is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = 30))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  print(ggp)
}

#------GRÁFICO DE BARRAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------

gr.bar.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL,  data = elsoc_panel, ponderador = 'ponderador02',
                        titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
  # var_y y var_x se tienen que definir, pero var_z (en leyenda) es opcional  
  # var_x = 'ola' por defecto
  if (is.null(var_z)) {
    data <- mutate(data, 
                   var_z_aux = factor('1'))
    var_z <- 'var_z_aux'
  }
  
  # Recodificacion no es opcional. Si no se especifica, se asume que corresponde a la suma de los niveles n_1 y n:
  if (is.null(REC)) {
    n <- length(levels(getElement(data, var_y)))
    REC <- c(levels(getElement(data, var_y))[n-1], levels(getElement(data, var_y))[n])
  }
  
  data <- mutate(data, var_y_rec = forcats::fct_collapse(!!as.name(var_y), 
                                                         '1' = REC,
                                                         other_level = '0'))
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name('var_y_rec')
  var_x <- as.name(var_x)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_z)) %>% 
    group_by(!!var_y, !!var_x, !!var_z) %>% 
    summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
    group_by(!!var_x, !!var_z) %>% 
    mutate(n1 = sum(n2, na.rm = TRUE),
           perc = n2/n1, 
           pctlabel = paste0(round(perc*100), "%")) %>% 
    ungroup() %>% 
    drop_na() %>%
    filter(!!var_y == '1')
  
  #Llamado ggplot
  
  # Si eje Z tiene un único nivel, se usan colores según eje X, para que no se vea tan plano
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = !!var_x, y = perc, fill = !!var_z))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = !!var_x, y = perc, fill = !!var_x)) + 
      guides(fill=FALSE)
  }
  
  # Etiqueta de valores de eje y si se definen
  if (is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = 30))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  ggp <- ggp +
    theme_bw() + 
    geom_col(position = 'dodge2') +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes:
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  print(ggp)
}

#------GRÁFICO DE LINEAS de frecuencias de var_y por var_x ------
gr.line.freq <- function(var_y, var_x = 'ola', data = elsoc_panel, ponderador = 'ponderador02',
                        titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_x) & !is.na(!!var_y)) %>% 
    group_by(!!var_x, !!var_y) %>% 
    summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
    group_by(!!var_x) %>% 
    mutate(n1 = sum(n2, na.rm = TRUE),
           perc = n2/n1, 
           pctlabel = paste0(round(perc*100), "%")) %>% 
    ungroup() %>% 
    drop_na()
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico, 
                aes(x = !!var_x, y = perc, linetype = !!var_y, color = !!var_y, group = !!var_y)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    # Titulo del gráfico
    ggrepel::geom_text_repel(aes(label = pctlabel), 
              vjust = -0.8, 
              size= 2.7) +
    theme_bw() + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Etiqueta de valores de eje y si se definen
  if (is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = 30))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  print(ggp)
}


#------GRÁFICO DE LINEAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
gr.line.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL,  data = elsoc_panel, ponderador = 'ponderador02',
                            titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
  # var_y y var_x se tienen que definir, pero var_z (en leyenda) es opcional  
  # var_x = 'ola' por defecto
  if (is.null(var_z)) {
    data <- mutate(data, 
                   var_z_aux = factor('1'))
    var_z <- 'var_z_aux'
  }
  
  # Recodificacion no es opcional. Si no se especifica, se asume que corresponde a la suma de los niveles n_1 y n:
  if (is.null(REC)) {
    n <- length(levels(getElement(data, var_y)))
    REC <- c(levels(getElement(data, var_y))[n-1], levels(getElement(data, var_y))[n])
  }
  
  data <- mutate(data, var_y_rec = forcats::fct_collapse(!!as.name(var_y), 
                                                         '1' = REC,
                                                         other_level = '0'))
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name('var_y_rec')
  var_x <- as.name(var_x)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_z)) %>% 
    group_by(!!var_y, !!var_x, !!var_z) %>% 
    summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
    group_by(!!var_x, !!var_z) %>% 
    mutate(n1 = sum(n2, na.rm = TRUE),
           perc = n2/n1, 
           pctlabel = paste0(round(perc*100), "%")) %>% 
    ungroup() %>% 
    drop_na() %>%
    filter(!!var_y == '1')
  
  # Llamado ggplot
  
  # Si la variable del eje Z tiene un único nivel
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = !!var_x, y = perc, color = !!var_z, linetype = !!var_z, group = !!var_z))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = !!var_x, y = perc, group = 1))
  }
  
  ggp <- ggp + 
    geom_line() +
    geom_point()+
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    ggrepel::geom_text_repel(aes(label = pctlabel), 
              vjust = -0.8, 
              size= 2.7) +
    theme_bw() + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Etiqueta de valores de eje y si se definen
  if (is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = 30))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  print(ggp)
}

gr.bar.freq('c01')
gr.bar.freq.rec('c01')
