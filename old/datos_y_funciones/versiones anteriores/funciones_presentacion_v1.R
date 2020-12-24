library(tidyverse)
library(sjmisc)
library(survey)
dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_carlos)

#-------0.- FUNCIONES AUXILIARES ------

# Funcion para cortar textos (en caso de titulos o etiquetas muy largas)
cortar_texto <- function(texto, largo = 70){
  return(paste0(strwrap(texto, width = largo), collapse = "\n"))
}

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
                aes(x = as.factor(!!var_x), y = perc, fill = as.factor(!!var_y))) +
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
  
  # Agregar titulo según recodificación (si no está predefinido como opción)
  if (is.null(titulo)) {
    titulo <- paste0('Suma de respuestas ', paste0(REC, collapse = ' + '))
  }
  
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
                  aes(x = as.factor(!!var_x), y = perc, fill = as.factor(!!var_z)))
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
    # Titulo del gráfico
    ggtitle(label = cortar_texto(titulo, largo = 70)) +
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
  
  print(ggp)
}


#------GRÁFICO DE BARRAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.bar.media <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel, ponderador = 'ponderador02',
                            titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, limits_y = c(NA, NA)) {
  
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
    geom_text(aes(label = format(media, nsmall = 1, big.mark = '.', decimal.mark = ',')),
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) + 
    scale_y_continuous(labels = function(k) format(k, nsmall = 0, big.mark = '.', decimal.mark = ','),
                       limits = limits_y) +
    # Titulo del gráfico
    ggtitle(label = cortar_texto(titulo, largo = 70)) +
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  
  print(ggp)
}


#------GRÁFICO DE LINEAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.line.media <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel, ponderador = 'ponderador02',
                         titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, limits_y = c(NA, NA)) {
  
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
    ggrepel::geom_text_repel(aes(label = format(media, nsmall = 1, big.mark = '.', decimal.mark = ',')),
              size= 2.7) + 
    scale_y_continuous(labels = function(k) format(k, nsmall = 0, big.mark = '.', decimal.mark = ','),
                       limits = limits_y) +
    # Titulo del gráfico
    ggtitle(label = cortar_texto(titulo, largo = 70)) +
    theme(legend.position = 'top', 
          legend.title = element_blank()) 
  print(ggp)
}

# ---------------FUNCIONES QUE DEVUELVEN LAS TABLAS QUE LUEGO SON GRAFICADAS--------------------

#------TABLA de frecuencias de var_y por var_x ------
tb.freq <- function(var_y, var_x = 'ola', 
                    data = elsoc_panel, disenno = NULL) {

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
                        data = elsoc_panel, disenno = NULL) {

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

#------GRÁFICO ALLUVIAL de frecuencias de var_y por ola ------
gr.alluvial.freq <- function(var_y, data = elsoc_panel, ponderador = 'ponderador02', REC = NULL,
                             titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL) {
  
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
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico,
                aes(x = ola, alluvium = idencuesta,
                    y = perc,
                    fill = !!var_y,
                    stratum = !!var_y)) +
    theme_bw() +
    ggalluvial::geom_flow() + 
    ggalluvial::geom_stratum(alpha = 0.4, width = 0.35) +
    scale_y_continuous(labels = scales::percent, # Escalas de eje Y
                       limits = c(0,1),
                       breaks = seq(0,1,.2)) +
    labs(y = 'Porcentaje',
         x = 'Ola de encuesta') +
    theme(legend.position = 'top',
          legend.title = element_blank()) 
  # ggrepel::geom_text_repel(label = paste0(round(perc*100), "%"),
  #                          vjust = -0.8,
  #                          size= 2.7) + 
  # scale_fill(label = function(k) lapply(k, cortar_texto, largo = 30)) 
  print(ggp)
}

#####################################
#---------III.- FUNCIONES PARA TRANSFORMACION DE DATOS------
#------A.- NOT IN OPERATOR----
`%notin%` <- Negate(`%in%`)

################################################
#--------------GUARDAR OBJETOS DEL ENVIROMENT EN ARCHIVO-----------------
save(gr.bar.freq, gr.bar.freq.rec, gr.bar.media, gr.line.freq, gr.line.freq.rec, gr.line.media, gr.alluvial.freq, tb.freq, tb.freq.rec, cortar_texto,
     file = "Presentacion/RCS2020 - Presentacion final/funciones_y_otros/funciones_a_usar.RData")

#---------------LIMPIAR ENVIROMENT SALIDA------------------
# rm(list = ls())

