library(tidyverse)
library(sjmisc)
library(survey)
library(spatstat) #función weighted.median

dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_edgardo)

#------------------------------------------------
#-------A.- FUNCIONES AUXILIARES ----------------
#------------------------------------------------

# Funcion para cortar textos (en caso de titulos o etiquetas muy largas)
cortar_texto <- function(texto, largo = 70){
  return(paste0(strwrap(texto, width = largo), collapse = "\n"))
}

# Operador NOT IN:
`%notin%` <- Negate(`%in%`)

#------------------------------------------------
#----------- B.- GRAFICOS DE BARRA --------------
#------------------------------------------------

#------GRÁFICO DE BARRAS de frecuencias de var_y por var_x ------
gr.bar.freq <- function(var_y, var_x = 'ola', data = elsoc_panel_m1, ponderador = 'ponderador02',
                        posicion = 'dodge2', suma100 = 'x',
                        titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_x) & !is.na(!!var_y)) %>% 
    group_by(!!var_x, !!var_y) %>% 
    summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
    drop_na()
  
  # Porcentajes calculados sobre qué base:
  if (suma100 == 'y') {
    datos.grafico <- datos.grafico %>% 
      group_by(!!var_y) %>% 
      mutate(n1 = sum(n2, na.rm = TRUE),
             perc = n2/n1, 
             pctlabel = paste0(round(perc*100), "%")) %>% 
      ungroup() 
  } else {
    datos.grafico <- datos.grafico %>% 
      group_by(!!var_x) %>% 
      mutate(n1 = sum(n2, na.rm = TRUE),
             perc = n2/n1, 
             pctlabel = paste0(round(perc*100), "%")) %>% 
      ungroup()
  }
  
  largo_max_label_x <- round(120/length(unique(getElement(datos.grafico, var_x))),0) - 10
  largo_max_label_y <- round(120/length(unique(getElement(datos.grafico, var_y))),0) 
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico, 
                aes(x = as.factor(!!var_x), y = perc, fill = as.factor(!!var_y))) +
    geom_col(position = posicion) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) + 
    theme_bw() +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    # Titulo del gráfico
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                                label = function(k) lapply(k, cortar_texto, largo = largo_max_label_y)) +
    scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = largo_max_label_x)) + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
    
  # Etiqueta de valores de eje Y si se definen
  if (!is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = largo_max_label_y))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (!is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = largo_max_label_x))
  }

  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  return(ggp)
}

#------GRÁFICO DE BARRAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
gr.bar.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL,  data = elsoc_panel_m1, 
                            ponderador = 'ponderador02', 
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
    drop_na() %>% 
    group_by(!!var_x, !!var_z) %>% 
      mutate(n1 = sum(n2, na.rm = TRUE),
             perc = n2/n1, 
             pctlabel = paste0(round(perc*100), "%")) %>% 
      ungroup() %>% 
      filter(!!var_y == '1')
  
  #Llamado ggplot
  
  # Si eje Z tiene un único nivel, se usan colores según eje X, para que no se vea tan plano
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = perc, fill = as.factor(!!var_z)))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = !!var_x, y = perc, fill = !!var_x)) + 
      guides(fill=FALSE)
  }
  
  largo_max_label_x <- round(120/length(unique(getElement(datos.grafico, var_x))),0) - 10
  largo_max_label_z <- round(120/length(unique(getElement(datos.grafico, var_z))),0) 
  
  # Todos los demás detalles del gráfico:
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
          legend.title = element_blank()) + 
    scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                                                 label = function(k) lapply(k, cortar_texto, largo = largo_max_label_z)) +
    scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = largo_max_label_x))
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  
  # Etiqueta de valores de eje y si se definen
  if (!is.null(label_y)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = largo_max_label_z))
  } 
  
  # Etiqueta de valores de eje X si se definen
  if (!is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = largo_max_label_x))
  }
  
  return(ggp)
}



#------GRÁFICO DE BARRAS de frecuencias de lista de variables en var_y recodificado por var_z ------
gr.bar.freq.rec.list <- function(var_x, var_z = 'ola', REC = NULL,  data = elsoc_panel_m1, 
                            ponderador = 'ponderador02',
                            titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL) {
  
  # Recodificacion no es opcional. Si no se especifica, se asume que corresponde a la suma de los niveles n_1 y n de la primera variable en la lista:
  if (is.null(REC)) {
    n <- length(levels(getElement(data, var_x[1])))
    REC <- c(levels(getElement(data, var_x[1]))[n-1], levels(getElement(data, var_x[1]))[n])
  }
  
  datos.grafico <- data %>% 
    select(var_z, all_of(var_x), ponderador) %>% 
    pivot_longer(cols = all_of(var_x), names_to = 'variable', values_to = 'categoria') %>%
    drop_na() %>% 
    mutate(var_x_rec = forcats::fct_collapse(categoria, 
                                                   '1' = REC,
                                                   other_level = '0')) %>% 
    group_by(variable, !!as.name(var_z), var_x_rec) %>% 
    summarize(n2 = sum(!!as.name(ponderador), na.rm = TRUE)) %>% 
    group_by(variable, !!as.name(var_z)) %>% 
    mutate(n1 = sum(n2, na.rm = TRUE),
           perc = n2/n1,
           pctlabel = paste0(round(perc*100), "%")) %>%
    filter(var_x_rec == '1')
    
  #Llamado ggplot
  # Si eje Z tiene un único nivel, se usan colores según eje X, para que no se vea tan plano
  if (length(unique(getElement(datos.grafico, var_z))) > 1) {
    ggp <- ggplot(datos.grafico,
                  aes(x = as.factor(variable), y = perc, fill = as.factor(!!as.name(var_z)) ))
  } else {
    ggp <- ggplot(datos.grafico,
                  aes(x = as.factor(variable), y = perc, fill = as.factor(variable) )) +
      guides(fill=FALSE)
  }
    
  largo_max_label_z <- round(120/length(unique(getElement(datos.grafico, var_z))),0) 
  
  # Todos los demás detalles del gráfico:
  ggp <- ggp +
    theme_bw() + 
    geom_col(position = 'dodge2') +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes:
    labs( y = 'Porcentaje',
          x = NULL) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    theme(legend.position = 'top', 
          legend.title = element_blank()) +
    scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                         label = function(k) lapply(k, cortar_texto, largo = largo_max_label_z))
  
  # Etiqueta de valores de eje X si se definen
  if (!is.null(label_x)) {
    largo_max_label_x <- round(120/length(unique(label_x)),0) - 10
    ggp <- ggp + 
      scale_x_discrete(label = lapply(label_x, cortar_texto, largo = largo_max_label_x))
  }
  
  # Etiqueta de valores de eje Z si se definen
  if (!is.null(label_z)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_y, cortar_texto, largo = largo_max_label_z))
  }
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  return(ggp)
}


#------GRÁFICO DE BARRAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.bar.media <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                         titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, limits_y = c(0, NA),
                         n_decimales = 1) {
  
  # var_y y var_x se tienen que definir, pero var_z (en leyenda) es opcional  
  # var_x = 'ola' por defecto
  if (is.null(var_z)) {
    data <- mutate(data, 
                   var_z_aux = factor('1'))
    var_z <- 'var_z_aux'
  }
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_z)) %>% 
    group_by(!!var_x, !!var_z) %>% 
    summarize(media = weighted.mean(x = !!var_y, w = !!ponderador, na.rm = TRUE)) %>% 
    drop_na()
  
  #Llamado ggplot
  
  # Si eje Z tiene un único nivel, se usan colores según eje X, para que no se vea tan plano
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = media, fill = as.factor(!!var_z)))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = media, fill = as.factor(!!var_x))) + 
      guides(fill=FALSE)
  }
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  # Etiqueta de valores de eje Z si se definen
  if (is.null(label_z)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_z, cortar_texto, largo = 30))
  } 
  
  # Todos los demás detalles del gráfico:
  ggp <- ggp +
    theme_bw() + 
    geom_col(position = 'dodge2') +
    # titulos de ejes:
    labs( y = "Valor promedio", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    geom_text(aes(label = format(media, digits = 2, nsmall = n_decimales, big.mark = '.', decimal.mark = ',')),
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    scale_y_continuous(labels = function(k) format(k, nsmall = 0, big.mark = '.', decimal.mark = ','),
                       limits = limits_y) +
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  return(ggp)
}


#------------------------GRAFICO DE BARRAS CON MEDIANA----------------------------------------------
gr.bar.mediana <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                           titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, 
                           limits_y = c(0, NA), n_decimales = 1) {
  
  # var_y y var_x se tienen que definir, pero var_z (en leyenda) es opcional  
  # var_x = 'ola' por defecto
  if (is.null(var_z)) {
    data <- mutate(data, 
                   var_z_aux = factor('1'))
    var_z <- 'var_z_aux'
  }
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_z)) %>% 
    group_by(!!var_x, !!var_z) %>% 
    dplyr::summarize(mediana = weighted.median(x = !!var_y, w= !!ponderador, na.rm = TRUE)) %>% 
    drop_na()
  
  #Llamado ggplot
  
  # Si eje Z tiene un único nivel, se usan colores según eje X, para que no se vea tan plano
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = mediana, fill = as.factor(!!var_z)))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = mediana, fill = as.factor(!!var_x))) + 
      guides(fill=FALSE)
  }
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  # Etiqueta de valores de eje Z si se definen
  if (is.null(label_z)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_z, cortar_texto, largo = 30))
  } 
  
  # Todos los demás detalles del gráfico:
  ggp <- ggp +
    theme_bw() + 
    geom_col(position = 'dodge2') +
    # titulos de ejes:
    labs( y = "Valor promedio", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    geom_text(aes(label = format(mediana, digits = 2, nsmall = n_decimales, big.mark = '.', decimal.mark = ',')),
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    scale_y_continuous(labels = function(k) format(k, nsmall = 0, big.mark = '.', decimal.mark = ','),
                       limits = limits_y) +
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  return(ggp)
}


#------------------------------------------------
#----------- C.- GRAFICOS DE LINEA --------------
#------------------------------------------------

#------GRÁFICO DE LINEAS de frecuencias de var_y por var_x ------
gr.line.freq <- function(var_y, var_x = 'ola', data = elsoc_panel_m1, ponderador = 'ponderador02',
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
                aes(x = as.factor(!!var_x), y = perc, linetype = as.factor(!!var_y), color = as.factor(!!var_y), group = as.factor(!!var_y))) +
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
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  return(ggp)
}


#------GRÁFICO DE LINEAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
gr.line.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL,  data = elsoc_panel_m1, ponderador = 'ponderador02',
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
                  aes(x = as.factor(!!var_x), y = perc, color = as.factor(!!var_z), linetype = as.factor(!!var_z), group = as.factor(!!var_z)))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = perc, group = 1))
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
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  return(ggp)
}


#------GRÁFICO DE LINEAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.line.media <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                         titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, 
                         limits_y = c(0, NA), n_decimales = 1 ) {
  
  # var_y y var_x se tienen que definir, pero var_z (en leyenda) es opcional  
  # var_x = 'ola' por defecto
  if (is.null(var_z)) {
    data <- mutate(data, 
                   var_z_aux = factor('1'))
    var_z <- 'var_z_aux'
  }
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_x <- as.name(var_x)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  datos.grafico <- data %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_z)) %>% 
    group_by(!!var_x, !!var_z) %>% 
    summarize(media = weighted.mean(x = !!var_y, w = !!ponderador, na.rm = TRUE)) %>% 
    drop_na()
  
  #Llamado ggplot
  # Si la variable del eje Z tiene un único nivel
  if (length(levels(getElement(data, var_z))) > 1) {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = media, color = as.factor(!!var_z), linetype = as.factor(!!var_z), group = as.factor(!!var_z)))
  } else {
    ggp <- ggplot(datos.grafico, 
                  aes(x = as.factor(!!var_x), y = media, group = 1))
  }
  
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  # Etiqueta de valores de eje Z si se definen
  if (is.null(label_z)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_z, cortar_texto, largo = 30))
  } 
  
  ggp <- ggp + 
    theme_bw() +
    geom_line() +
    geom_point() +
    # titulos de ejes:
    labs( y = "Valor promedio", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50)) +
    ggrepel::geom_text_repel(aes(label = format(media, digits = 2, nsmall = n_decimales, big.mark = '.', decimal.mark = ',')),
              size= 2.7) + 
    scale_y_continuous(labels = function(k) format(k, nsmall = 0, big.mark = '.', decimal.mark = ','),
                       limits = limits_y) +
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  return(ggp)
}


#---------------------------------------------------------
#----------- D.- TABLAS con diseño muestral --------------
#---------------------------------------------------------

#------TABLA de frecuencias de var_y por var_x ------
tb.freq <- function(var_y, var_x = 'ola', 
                    data = elsoc_panel_m1, disenno = NULL) {

  # Si no se declara el diseño, se construye a partir de los datos  
  # Recomendado declarar diseño para aumentar eficiencia de estimaciones
  if (is.null(disenno)) {
    disenno <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, nest = TRUE,
                         data = data)
  }

  tabla <- svyby(as.formula(paste0("~", var_y)), 
        by = as.formula(paste0("~", var_x)),
        design = disenno, FUN = svymean, na.rm = TRUE)
  return(tabla)
}

#------TABLA de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
tb.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, REC = NULL,
                        data = elsoc_panel_m1, disenno = NULL) {

  # Si no se declara el diseño, se construye a partir de los datos  
  # (Recomendado declarar diseño para aumentar eficiencia de estimaciones)
  if (is.null(disenno)) {
    disenno <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, nest = TRUE,
                         data = data)
  }
  
  # Recodificacion no es opcional. Si no se especifica, se asume que corresponde a la suma de los niveles n_1 y n:
  data <- disenno$variables     # Datos se sacan del diseño (para la recodificacion)
  if (is.null(REC)) {
    n <- length(levels(getElement(data, var_y)))
    REC <- c(levels(getElement(data, var_y))[n-1], levels(getElement(data, var_y))[n])
  }
  
  disenno <- update(disenno, 
                    var_y_rec = forcats::fct_collapse(getElement(data, var_y), 
                                                '1' = REC,
                                                other_level = '0'))
  
  if (is.null(var_z)){
    by_vars <- as.formula(paste0("~", var_x))
  } else {
    by_vars <- as.formula(paste0("~", var_x,  "+", var_z))
  }
  
  svyby(formula = ~var_y_rec, 
        by = by_vars,
        design = disenno, FUN = svymean, na.rm = TRUE, vartype = c('se', 'ci'))
}


#---------------------------------------------------------
#---------------- E.- GRÁFICO ALLUVIAL -------------------
#---------------------------------------------------------


#------GRÁFICO ALLUVIAL de frecuencias de var_y por ola ------
gr.alluvial.freq <- function(var_y, data = elsoc_panel_m1, ponderador = 'ponderador02', 
                              REC = NULL, flow = 'forward', ocultar = NULL, destacar = NULL, invertir_labels = FALSE,
                             titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL, limits_y = c(0, 1), rango_alpha =  c(0.75, 0.4)) {
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  ponderador <- as.name(ponderador)
  
  # Recodificacion es opcional.
  if (!is.null(REC)) {
    
    niveles <- levels(getElement(data, var_y))
    REC_1 <- niveles[!niveles %in% REC] 
    
    # Generar etiquetas de valores recodificadas
    if (length(REC)>1){
      lrec <- paste0('Suma de respuestas ', paste0(REC, collapse = ' + '))
    } else {
      lrec <- REC
    }
    
    data <- mutate(data, var_y_rec = factor(forcats::fct_collapse(!!var_y, 
                                                                  '1' = REC,
                                                                  '0' = REC_1),
                                            levels = c('1', '0'),
                                            labels = c(lrec, 'Otras respuestas')))
    var_y <- as.name('var_y_rec')
  }
  
  # Corregir datos para no tener problemas con grafico
  datos.grafico <- data %>% 
    select(!!var_y, ola, idencuesta, !!ponderador) %>% 
    filter(!is.na(!!var_y)) %>% # Sacar casos NA:
    group_by(idencuesta) %>% 
    mutate(n_obs = sum(!is.na(idencuesta), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(n_ok = (n_obs == max(n_obs, na.rm = TRUE))) %>% 
    filter(n_ok) %>% # Quedarse solo con los casos con observaciones completas:
    group_by(ola) %>% 
    mutate(perc = !!ponderador/sum(!!ponderador, na.rm = TRUE)) %>% # ponderador se recodifica para sumar 100
    ungroup()
  
  # Definir categorías que se van a "ocultar" o "destacar"
  if (!is.null(ocultar)) {
    datos.grafico <- datos.grafico %>%
      mutate(cond_hide = (!!var_y == ocultar))
  } else if (!is.null(destacar)) {
    datos.grafico <- datos.grafico %>%
      mutate(cond_hide = (!!var_y != destacar))
  } else {
    datos.grafico <- datos.grafico %>% 
      mutate(cond_hide = FALSE)
  }
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico,
                aes(x = ola, alluvium = idencuesta,
                    y = perc,
                    alpha = cond_hide,
                    fill = !!var_y,
                    stratum = !!var_y)) +
    theme_bw() +
    ggalluvial::geom_flow(aes.flow = flow) + 
    ggalluvial::geom_stratum() +
    scale_y_continuous(labels = scales::percent, # Escalas de eje Y
                       limits = c(0,1),
                       breaks = seq(0, 1, .2)) +
    scale_fill_discrete(guide = guide_legend(reverse = invertir_labels,
                                             override.aes = list(alpha = .5)) ) +
    coord_cartesian(ylim = limits_y ) +
    scale_alpha_discrete(range = rango_alpha,
                         guide = FALSE) + 
    labs(y = 'Porcentaje',
         x = 'Ola de encuesta') +
    theme(legend.position = 'top',
          legend.title = element_blank())
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  return(ggp)
  
}


#------GRÁFICO ALLUVIAL de frecuencias de var_y por ola y por otras variables ------
gr.alluvial.freq.facet <- function(var_y, var_z, data = elsoc_panel_m1, ponderador = 'ponderador02', 
                                   REC = NULL, flow = 'forward', ocultar = NULL, destacar = NULL, invertir_labels = FALSE,
                                   titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL, rango_alpha =  c(0.75, 0.4)) {
  
  # Obtener resutlados estadísticos a graficar
  var_y <- as.name(var_y)
  var_z <- as.name(var_z)
  ponderador <- as.name(ponderador)
  
  # Recodificacion es opcional.
  if (!is.null(REC)) {
    
    niveles <- levels(getElement(data, var_y))
    REC_1 <- niveles[!niveles %in% REC] 
    
    # Generar etiquetas de valores recodificadas
    if (length(REC)>1){
      lrec <- paste0('Suma de respuestas ', paste0(REC, collapse = ' + '))
    } else {
      lrec <- REC
    }
    
    data <- mutate(data, var_y_rec = factor(forcats::fct_collapse(!!var_y, 
                                                                  '1' = REC,
                                                                  '0' = REC_1),
                                            levels = c('1', '0'),
                                            labels = c(lrec, 'Otras respuestas')))
    var_y <- as.name('var_y_rec')
  }
  
  # Corregir datos para no tener problemas con grafico
  datos.grafico <- data %>% 
    select(!!var_y, !!var_z, ola, idencuesta, !!ponderador) %>% 
    filter(!is.na(!!var_y) & !is.na(!!var_z)) %>% # Sacar casos NA:
    group_by(idencuesta) %>% 
    mutate(n_obs = sum(!is.na(idencuesta), na.rm = TRUE),
           fijo = (n_distinct(!!var_z)==1)) %>% 
    ungroup() %>% 
    mutate(n_ok = (n_obs == max(n_obs, na.rm = TRUE))) %>% 
    filter(n_ok & fijo) %>% # Quedarse solo con los casos con observaciones completas y que no cambian en Z
    group_by(ola, !!var_z) %>% 
    mutate(perc = !!ponderador/sum(!!ponderador, na.rm = TRUE)) %>% # ponderador se recodifica para sumar 100
    ungroup()
  
  if (!is.null(ocultar)) {
    datos.grafico <- datos.grafico %>%
      mutate(cond_hide = (!!var_y == ocultar))
  } else if (!is.null(destacar)) {
    datos.grafico <- datos.grafico %>%
      mutate(cond_hide = (!!var_y != destacar))
  } else {
    datos.grafico <- datos.grafico %>% 
      mutate(cond_hide = FALSE)
  }
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico,
                aes(x = ola, alluvium = idencuesta,
                    y = perc,
                    alpha = cond_hide,
                    fill = !!var_y,
                    stratum = !!var_y)) +
    theme_bw() +
    ggalluvial::geom_flow(aes.flow = flow ) + 
    ggalluvial::geom_stratum(width = 0.35) +
    scale_y_continuous(labels = scales::percent, # Escalas de eje Y
                       limits = c(0,1),
                       breaks = seq(0,1,.2)) +
    labs(y = 'Porcentaje',
         x = 'Ola de encuesta') +
    theme(legend.position = 'top',
          legend.title = element_blank()) +
    scale_alpha_discrete(range = rango_alpha, 
                         guide = FALSE) +
    scale_fill_discrete(guide = guide_legend(reverse = invertir_labels,
                                             override.aes = list(alpha = .5)) ) +
    facet_wrap(as.formula(paste0('.~',var_z)))

  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
    return(ggp)
}


################################################
#--------------GUARDAR OBJETOS DEL ENVIROMENT EN ARCHIVO-----------------
save(gr.bar.mediana, gr.alluvial.freq.facet, gr.bar.freq, gr.bar.freq.rec, gr.bar.media, gr.line.freq, gr.line.freq.rec, gr.line.media, gr.alluvial.freq, tb.freq, tb.freq.rec, gr.bar.freq.rec.list, cortar_texto,
     file = "Presentacion/RCS2020 - Presentacion final/funciones_y_otros/funciones_a_usar.RData")

#---------------LIMPIAR ENVIROMENT SALIDA------------------
# rm(list = ls())

