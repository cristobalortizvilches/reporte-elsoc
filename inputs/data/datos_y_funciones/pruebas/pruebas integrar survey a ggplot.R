library(tidyverse)
library(sjmisc)
library(survey)
dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_carlos)

#Mapa de variables + bases
load("Colaboracion con academicos/Insumos para discusion/transformacion_variables/datos_a_usar.RData")
# Funciones
load("Colaboracion con academicos/Insumos para discusion/funciones/funciones_a_usar.RData")


#OBJETO SURVEY
elsoc_panel_01 <- filter(elsoc_long, is.na(segmento)== FALSE)
m_orig_des2a <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, data = elsoc_panel_01, nest = TRUE )

#Filtrar datos con 3 mediciones
elsoc_panel_m1 <- elsoc_long %>% dplyr::filter(tipo_atricion == 1 & tipo_caso != 2)
elsoc_panel_m2 <- elsoc_long %>% dplyr::filter(tipo_atricion == 10 & tipo_caso != 2)

#declarar diseño
elsoc_panel_m2_01 <- filter(elsoc_panel_m2, is.na(segmento)== FALSE)

dis_01_m1    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador01, data = elsoc_panel_m1)
dis_02_m1    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, data = elsoc_panel_m1)

dis_01_m2    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador01, data = elsoc_panel_m2_01)
dis_02_m2    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, data = elsoc_panel_m2_01)


dis_01_m1    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador01, data = elsoc_panel)
dis_02_m1    <- svydesign(ids = ~segmento, strata = ~estrato, weights = ~ponderador02, data = elsoc_panel)




#GRAFICO QUE PONDERA
gf.bar.pond <- function(var_fill, var_x = 'ola', data= elsoc_panel, diseño = m_orig_des2a,
                        titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL){
  var_fill <- as.name(var_fill)
  var_x    <- as.name(var_x)
  t1 <- svytable(formula = as.formula(paste0("~", var_fill,  "+", var_x)), design = diseño) %>% 
    broom::tidy() %>%
    group_by(!!var_x) %>%
    mutate(n_grupo = sum(n), perc = n/n_grupo) %>%
    filter(n_grupo != 0) %>%
    ungroup()
  
  t2 <- t1 %>% mutate(var_fac = factor(!!var_fill, levels = unique(!!var_fill)),
                      var_num = as.numeric(var_fac))
  
  datos.grafo <- t2 %>% mutate(pctlabel = paste0(round(perc*100), "%"))
  
  ggp <- ggplot(datos.grafo, 
                aes(x = factor(!!var_x, levels = unique(!!var_x)), y = perc, fill = factor(!!var_fill, levels = unique(!!var_fill)))) +
    geom_bar(stat = "identity", 
             position = "dodge2") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
          fill = cortar_texto(attr(getElement(data, var_fill), which = 'label'), largo = 35))+
    # Titulo del gráfico
    ggtitle(label = cortar_texto(ifelse(!is.null(titulo),
                                        titulo,
                                        attr(getElement(data, var_fill), which = 'label')), largo = 70),
            subtitle = subtitulo) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) +
    theme_bw()
  # Etiqueta de valores de eje fill si se definen
  if (is.null(label_fill)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
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



#GRAFICO QUE RECODIFICA CON NUMERO Y PONDERADO

gf.rec.pond <- function(var_y, var_x, var_fill = 'ola', data= elsoc_panel, rec = "c(1,2,3)=0;c(4,5)=1", diseño = m_orig_des2a,
                         titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL){
  var_fill <- as.name(var_fill)
  var_x    <- as.name(var_x)
  var_y    <- as.name(var_y)

  t1 <- svytable(formula = as.formula(paste0("~", var_y,"+", var_x, "+", var_fill)), design = diseño) %>% 
    broom::tidy() %>%
    group_by(!!var_x, !!var_fill) %>%
    mutate(n_grupo = sum(n), 
           perc = n/n_grupo) %>%
    filter(n_grupo != 0) %>%
    ungroup()
  
  t2 <- t1 %>% mutate(var_fac = factor(!!var_y, levels = unique(!!var_y)),
                      var_num = as.numeric(var_fac))
  t3 <- add_column(t2, 
                      var.rec.temp = car::recode(getElement(t2, 
                                                            quote(var_num)), 
                                                            rec))
  
  datos.grafo <-t3 %>% 
    group_by(!!var_fill, !!var_x, var.rec.temp) %>%
    summarise(perc = sum(perc)) %>% 
    drop_na() %>%
    mutate(pctlabel = paste0(round(perc*100), "%")) %>%
    filter(var.rec.temp == 1)

  ggp <- ggplot(datos.grafo, 
                aes(x = factor(!!var_x, levels = unique(!!var_x)), y = perc, fill = factor(!!var_fill, levels = unique(!!var_fill)))) +
    geom_bar(stat = "identity", 
             position = "dodge2") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
          fill = cortar_texto(attr(getElement(data, var_fill), which = 'label'), largo = 35))+
    # Titulo del gráfico
    ggtitle(label = cortar_texto(ifelse(!is.null(titulo),
                                        titulo,
                                        attr(getElement(data, var_y), which = 'label')), largo = 70),
            subtitle = subtitulo) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) +
    theme_bw()
  # Etiqueta de valores de eje fill si se definen
  if (is.null(label_fill)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
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

# ---------------FUNCIONES QUE DEVUELVEN LAS TABLAS QUE LUEGO SON GRAFICADAS--------------------

gf.bar.pond.tb <- function(var_fill, var_x = 'ola', data= elsoc_panel, diseño = m_orig_des2a,
                        titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL){
  var_fill <- as.name(var_fill)
  var_x    <- as.name(var_x)
  t1 <- svytable(formula = as.formula(paste0("~", var_fill,  "+", var_x)), design = diseño) %>% 
    broom::tidy() %>%
    group_by(!!var_x) %>%
    mutate(n_grupo = sum(n), perc = n/n_grupo) %>%
    filter(n_grupo != 0) %>%
    ungroup()
  
  t2 <- t1 %>% mutate(var_fac = factor(!!var_fill, levels = unique(!!var_fill)),
                      var_num = as.numeric(var_fac))
  
  datos.grafo <- t2 %>% mutate(pctlabel = paste0(round(perc*100), "%"))
  
  ggp <- ggplot(datos.grafo, 
                aes(x = factor(!!var_x, levels = unique(!!var_x)), y = perc, fill = factor(!!var_fill, levels = unique(!!var_fill)))) +
    geom_bar(stat = "identity", 
             position = "dodge2") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
          fill = cortar_texto(attr(getElement(data, var_fill), which = 'label'), largo = 35))+
    # Titulo del gráfico
    ggtitle(label = cortar_texto(ifelse(!is.null(titulo),
                                        titulo,
                                        attr(getElement(data, var_fill), which = 'label')), largo = 70),
            subtitle = subtitulo) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) +
    theme_bw()
  # Etiqueta de valores de eje fill si se definen
  if (is.null(label_fill)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_fill, cortar_texto, largo = 30))
  } 
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  out <- list(t1,t2,datos.grafo)
  print(out)
}


gf.rec.pond.tb <- function(var_y, var_x, var_fill = 'ola', data= elsoc_panel, rec = "c(1,2,3)=0;c(4,5)=1", diseño = m_orig_des2a,
                        titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL){
  var_fill <- as.name(var_fill)
  var_x    <- as.name(var_x)
  var_y    <- as.name(var_y)
  
  t1 <- svytable(formula = as.formula(paste0("~", var_y,"+", var_x, "+", var_fill)), design = diseño) %>% 
    broom::tidy() %>%
    group_by(!!var_x, !!var_fill) %>%
    mutate(n_grupo = sum(n), 
           perc = n/n_grupo) %>%
    filter(n_grupo != 0) %>%
    ungroup()
  
  t2 <- t1 %>% mutate(var_fac = factor(!!var_y, levels = unique(!!var_y)),
                      var_num = as.numeric(var_fac))
  t3 <- add_column(t2, 
                   var.rec.temp = car::recode(getElement(t2, 
                                                         quote(var_num)), 
                                              rec))
  
  datos.grafo <-t3 %>% 
    group_by(!!var_fill, !!var_x, var.rec.temp) %>%
    summarise(perc = sum(perc)) %>% 
    drop_na() %>%
    mutate(pctlabel = paste0(round(perc*100), "%")) %>%
    filter(var.rec.temp == 1)
  
  ggp <- ggplot(datos.grafo, 
                aes(x = factor(!!var_x, levels = unique(!!var_x)), y = perc, fill = factor(!!var_fill, levels = unique(!!var_fill)))) +
    geom_bar(stat = "identity", 
             position = "dodge2") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1), 
                       breaks = seq(0,1,.2)) +
    # titulos de ejes y leyenda
    labs( y = "Porcentaje", 
          x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
          fill = cortar_texto(attr(getElement(data, var_fill), which = 'label'), largo = 35))+
    # Titulo del gráfico
    ggtitle(label = cortar_texto(ifelse(!is.null(titulo),
                                        titulo,
                                        attr(getElement(data, var_y), which = 'label')), largo = 70),
            subtitle = subtitulo) +
    geom_text(aes(label = pctlabel), 
              vjust = -0.8, 
              position = position_dodge(width = 1),
              size= 2.7) +
    theme_bw()
  # Etiqueta de valores de eje fill si se definen
  if (is.null(label_fill)) {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = function(k) lapply(k, cortar_texto, largo = 30))
  } else {
    ggp <- ggp + scale_fill_viridis_d(end = 0.9, direction = -1, option = 'viridis',
                                      label = lapply(label_fill, cortar_texto, largo = 30))
  } 
  # Etiqueta de valores de eje X si se definen
  if (is.null(label_x)) {
    ggp <- ggp + scale_x_discrete(label = function(k) lapply(k, cortar_texto, largo = 25))
  } else {
    ggp <- ggp + scale_x_discrete(label = lapply(label_x, cortar_texto, largo = 25))
  }
  
  
  out <- list(t1, t2, t3, datos.grafo)
  print(out)
}

