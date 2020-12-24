library(tidyverse)
library(sjmisc)
library(survey)
library(spatstat) #función weighted.median
library(sticky)

# dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
# dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC/4_Presentaciones_de_Lanzamientos_y_Eventos_Publicos/15_Lanzamiento_Global_2020"

# setwd(dir_edgardo)
# load("Presentacion/RCS2020 - Presentacion final/funciones_y_otros/datos_a_usar.RData")

# load("Presentacion/RCS2020 - Presentacion final/funciones_y_otros/datos_a_usar.RData")

#------------------------------------------------
#-------A.- FUNCIONES AUXILIARES ----------------
#------------------------------------------------

# Funcion para cortar textos (en caso de titulos o etiquetas muy largas)
cortar_texto <- function(texto, largo = 70){
  return(paste0(strwrap(texto, width = largo), collapse = "\n"))
}

# Ajustar largo y tamaño de texto de lista de leyendas/etiquetas:
ajustar_texto <- function(texto, return_tamano = FALSE) {
  
  max <- max(nchar(texto))
  tamanno_texto <- round(exp(-0.1*max + log(.5)) + .8, 2)
  n_largo <- round(120/length(texto),0) - 5
  if (return_tamano) {
    return(max(min(1, tamanno_texto),.8))
  } else {
    return(unlist(lapply(texto, cortar_texto, largo = n_largo)))
  }
}

#------------------------------------------------
#----------- B.- GRAFICOS DE BARRA --------------
#------------------------------------------------

# Función para generar datos de frecuencia para gráficos
datos.stat <- function(var_y, var_x = NULL, var_z = NULL, var_w = NULL, REC = NULL, stat = 'freq',
                       data = subset(elsoc_panel, tipo_atricion == 1 & tipo_caso != 2) , ponderador = 'ponderador02',
                       suma100 = 'x',
                       reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE,
                       expand = FALSE, full = FALSE) {
  
  # si no se define var_x, var_z y var_w, se crean variables auxiliares para mantener código más 'simple' 
  if (is.null(var_x)) {
    data$var_x = factor('1')
    var_x <- 'var_x'
  }
  
  if (is.null(var_z)) {
    data$var_z = factor('1')
    var_z <- 'var_z'
  }
  
  if (is.null(var_w)) {
    data$var_w = factor('1')
    var_w <- 'var_w'
  }
  
  if (is.null(ponderador)) {
    data$ponderador = as.numeric(1)
    ponderador <- 'ponderador'
  }
  
  
  # Categorías tienen que ser factores. Se reconvierten si no lo son
    datos.grafico <- data %>% 
      dplyr::select(!!as.name(var_y), !!as.name(var_x), !!as.name(var_z), !!as.name(var_w), !!as.name(ponderador), contains('idencuesta')) %>% 
      lapply(sticky::unsticky) %>% data.frame()
    
    if (stat == 'freq') {
      datos.grafico <- datos.grafico %>% 
      mutate(var_y = as.factor(!!as.name(var_y)),
             var_x = as.factor(!!as.name(var_x)),
             var_z = as.factor(!!as.name(var_z)),
             var_w = as.factor(!!as.name(var_w)),
             ponderador = as.numeric(!!as.name(ponderador)))
    } else {
      datos.grafico <- datos.grafico %>% 
        mutate(var_y = as.numeric(!!as.name(var_y)),
               var_x = as.factor(!!as.name(var_x)),
               var_z = as.factor(!!as.name(var_z)),
               var_w = as.factor(!!as.name(var_w)),
               ponderador = as.numeric(!!as.name(ponderador)))
    }
    
    if (reverse_y) {
      datos.grafico$var_y <- factor(datos.grafico$var_y, levels=rev(levels(datos.grafico$var_y)))
    }
    
    if (reverse_x) {
      datos.grafico$var_x <- factor(datos.grafico$var_x, levels=rev(levels(datos.grafico$var_x)))
    }

    if (reverse_z) {
      datos.grafico$var_z <- factor(datos.grafico$var_z, levels=rev(levels(datos.grafico$var_z)))
    }
    
    
    # Si Full=TRUE, se eliminan observaciones con NA en alguna de las muestras
    if (full) {
      datos.grafico <- datos.grafico %>% 
        filter(!is.na(var_y) & !is.na(var_x) & !is.na(var_z) & !is.na(var_w) & !is.na(idencuesta)) %>% # Sacar casos NA:
        group_by(idencuesta) %>% 
        mutate(n_obs = n(),
               fijo = (n_distinct(var_z)==1)) %>% 
        ungroup() %>% 
        mutate(n_ok = (n_obs == max(n_obs))) %>% 
        filter(n_ok & fijo) # Quedarse solo con los casos con observaciones completas y que no cambian en Z
    }
      
    
  # Recodificacion es opcional
  if (!is.null(REC)) {
    # Si REC=TRUE se recodifica a la suma de los valores 4 y 5 por defecto
    if (REC[1] == TRUE) {
      REC <- c(levels(datos.grafico$var_y)[4], levels(datos.grafico$var_y)[5])
    }
    
    # Si REC=c() se recodifica a los valores iguales a la lista
    datos.grafico <- mutate(datos.grafico, var_y = forcats::fct_collapse(var_y,
                                                             '1' = REC,
                                                             other_level = '0'))
    }
    
    
  # Obtener resultados estadísticos a graficar
    if (stat == 'freq') {
      datos.grafico <- datos.grafico %>%
        filter(!is.na(var_y) & !is.na(var_x) & !is.na(var_z) & !is.na(var_w)) %>%
        group_by(var_x, var_y, var_z, var_w) %>%
        summarize(n_wgt = sum(ponderador, na.rm = TRUE),
                  n_obs = n(),
                  .groups = 'drop')

            # Porcentajes calculados sobre qué base:
      if (suma100 == 'y') {
        datos.grafico <- datos.grafico %>% 
          group_by(var_y) %>% 
          mutate(n_grupo = sum(n_wgt, na.rm = TRUE),
                 stat = n_wgt/n_grupo)
      } else if (suma100 == 'xz') {
        datos.grafico <- datos.grafico %>% 
          group_by(var_x, var_z) %>% 
          mutate(n_grupo = sum(n_wgt, na.rm = TRUE),
                 stat = n_wgt/n_grupo)
      } else {
        datos.grafico <- datos.grafico %>% 
        group_by(var_x, var_z, var_w) %>% 
          mutate(n_grupo = sum(n_wgt, na.rm = TRUE),
                 stat = n_wgt/n_grupo)
        }
    } else {
      datos.grafico <- datos.grafico %>%
        filter(!is.na(var_y) & !is.na(var_x) & !is.na(var_z) & !is.na(var_w)) %>%
        group_by(var_x, var_z, var_w) %>%
        summarize(stat = weighted.mean(x = var_y, w = ponderador, na.rm = TRUE),
                  n_obs = n(),
                  n_wgt = sum(ponderador, na.rm = TRUE),
                  .groups = 'drop') %>%  
        ungroup() %>% drop_na() %>% 
        mutate(var_y = factor('1'))
    }
    
    
    datos.grafico <- datos.grafico %>% 
      ungroup() %>% 
      dplyr::select(var_y, var_x, var_z, var_w, n_obs, n_wgt, stat) %>% 
      data.frame() %>% 
      arrange(var_x, var_y, var_z, var_w)

      # Expandir datos para incluir barras para toda la combinatoria de var_y/var_x
  if (expand) {
    # DF auxiliar con todas las combinaciones posibles de var_y, var_x y var_z, con n = 0
    aux <- tidyr::expand(datos.grafico, var_x, var_y, var_z, var_w) %>% 
      drop_na() %>% 
      mutate(stat = 0, n_obs = 0, n_wgt = 0)
    
    # Mantener solo las que no están ya en datos.grafico
    aux <- anti_join(aux, datos.grafico, by = c('var_x', 'var_y', 'var_z', 'var_w'))
    
        # Agregarla a datos grafico
    datos.grafico <- rbind(datos.grafico, aux) %>% 
      data.frame() %>%
      group_by(var_x) %>% 
      mutate(n_aux = sum(n_obs)) %>% 
      filter(n_aux!=0) %>% dplyr::select(-n_aux) %>% 
      group_by(var_z) %>% 
      mutate(n_aux = sum(n_obs)) %>% 
      filter(n_aux!=0) %>% dplyr::select(-n_aux) %>% 
      group_by(var_w) %>% 
      mutate(n_aux = sum(n_obs)) %>% 
      filter(n_aux!=0) %>% dplyr::select(-n_aux) %>%
      group_by(var_y) %>% 
      mutate(n_aux = sum(n_obs)) %>% 
      filter(n_aux!=0) %>% dplyr::select(-n_aux) %>% 
      arrange(var_x, var_y, var_z, var_w)
    
    # Filtrar casos en que no hay observaciones en el eje x, z y w
  }

    return(datos.grafico)
  }

### Función para generar gráficos de barra con diseño para presentación:
grafico <- function(eje_x = 'var_x', eje_color = 'var_y', eje_facet = 'var_z', data = datos.grafico, ponderador = 'ponderador',
                   posicion = 'dodge2', suma100 = 'x', formato = 'bar',
                   titulo = NULL, subtitulo = NULL, 
                   label_x = NULL, label_color = NULL, label_facet = NULL, no_label_texto = FALSE, invertir_labels = FALSE,
                   titulo_x = NULL, titulo_y = NULL,
                   posicion_leyenda = 'top', posicion_facet = 'top',
                   datos = FALSE, expand = FALSE,
                   limits_y = c(0, NA),
                   colores = c(0, .85, -1), colores_tipo = 'viridis',
                   tamano_letra = 1) {
  
  # Ajustar tamaño de letra
  tamano_letra <- 25*tamano_letra
  
  #Llamado ggplot según formato de gráfico
  if (formato == 'bar') {
    ggp <- data %>%
      ggplot(aes(x = !!as.name(eje_x), y = stat, fill = !!as.name(eje_color))) +
      geom_col(position = posicion)
    scale_color <- scale_fill_viridis_d
    } else if (formato == 'line') {
      ggp <- data %>%
      ggplot(aes(x = !!as.name(eje_x), y = stat, color = !!as.name(eje_color), group = !!as.name(eje_color))) +
      geom_line(size = 1) +
      geom_point(size = 1.8)    
    scale_color <- scale_color_viridis_d
    } else if (formato == 'bar_n') {
      ggp <- data %>%
        ggplot(aes(x = !!as.name(eje_x), y = n_obs, fill = !!as.name(eje_color))) +
        geom_col(position = posicion)
      scale_color <- scale_fill_viridis_d
    }
  
  # Definir escala de eje y texto:
  if (max(data$stat)>10 |  formato == 'bar_n') {
    formato_stat <- function(k) format(round(k,0), nsmall = 0, big.mark = '.', decimal.mark = ',')
    } else if (max(data$stat)>1) {
      formato_stat <- function(k) format(round(k,1), nsmall = 1, big.mark = '.', decimal.mark = ',')
    } else {
      formato_stat <- function(k) case_when(k == 0 ~ '',
                                            k> 0 & k < .1 ~ scales::percent(k, accuracy = .1 , big.mark = '.', decimal.mark = ','), 
                                            TRUE ~ scales::percent(k, accuracy = .1 , big.mark = '.', decimal.mark = ','))
    }

      # Todos los demás detalles:
  ggp <- ggp +    
    theme_bw(base_size = tamano_letra) +
    # titulos de ejes:
    labs(y = titulo_y, 
         x = titulo_x) +
    # etiquetas de valores de eje Y
    scale_y_continuous(labels = formato_stat) +
    coord_cartesian(ylim = limits_y)
  
  # Texto en gráfico
  if (!no_label_texto & posicion == 'stack') {
    
    n <- length(unique(getElement(data, eje_color)))
    negro_blanco <- case_when(colores[3] == 1  ~ ifelse((colores[1] + seq(1:n)*(colores[2]-colores[1])/(n+1)) < .45, 'white', 'black'),
                              colores[3] == -1 ~ ifelse((colores[2] - seq(1:n)*(colores[2]-colores[1])/(n+1)) < .45, 'white', 'black'))

        ggp <- ggp + geom_text(aes(label = case_when(formato == 'bar_n' & stat > 0 ~ scales::percent(stat, accuracy=.1, big.mark='.', decimal.mark=','),
                                                     stat>.03 ~ formato_stat(stat),
                                                     TRUE ~ '')), 
                           position = position_stack(vjust = .5),
                           size= tamano_letra/5,
                           show.legend = FALSE,
                           color = rep(negro_blanco, length(unique(getElement(data, eje_x)))*length(unique(getElement(data, eje_facet))) ))
  } else if (!no_label_texto & formato == 'bar') {
    ggp <- ggp + geom_text(aes(label = as.character(formato_stat(stat))),
                           vjust = -0.8, 
                           position = position_dodge(width = .9),
                           size= tamano_letra/5)
  } else if (!no_label_texto) {
    ggp <- ggp + ggrepel::geom_text_repel(aes(label = as.character(formato_stat(stat))), 
                                          size= tamano_letra/5,
                                          force = 2,
                                          nudge_y = .015)
  }
  
  # Etiquetas de eje X:
  if (!is.null(label_x)) {     # Etiqueta de valores de eje X si se definen
    ggp <- ggp + scale_x_discrete(label = ajustar_texto(label_x)) +
      theme(axis.text.x = element_text(size = rel(ajustar_texto(label_x, return_tamano = TRUE))))
  } else {
    ggp <- ggp + scale_x_discrete(labels = function(k) ajustar_texto(k)) +
      theme(axis.text.x = element_text(size = rel(ajustar_texto(levels(getElement(data, eje_x)), return_tamano = TRUE))))
  }
  
  # Etiquetas y colores de eje color
  if (length(unique(getElement(data, eje_color))) == 1 ) { 
    ggp <- ggp +    
      theme(legend.position = 'none') + 
      scale_color(begin = .6)
  } else {
    ggp <- ggp + 
      theme(legend.position = posicion_leyenda, 
            legend.title = element_blank(),
            legend.text = element_text(size = rel(0.65*ajustar_texto(levels(getElement(data, eje_color)), return_tamano = TRUE))))

    if (length(unique(getElement(data, eje_color))) == 2 & identical(colores, c(0, .85, -1))) {
      colores <- c(0.33,.66,-1)
    }
    
    if (!is.null(label_color)) { # Si se definen etiquetas del eje y
      ggp <- ggp + scale_color(begin = colores[1], end = colores[2], direction = colores[3], option = colores_tipo,
                               guide = guide_legend(reverse = invertir_labels),
                               labels = ajustar_texto(label_color))
        
    } else {
      ggp <- ggp + scale_color(begin = colores[1], end = colores[2], direction = colores[3], option = colores_tipo,
                               guide = guide_legend(reverse = invertir_labels),
                               labels = function(k) ajustar_texto(k))
    }
  } 
    
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  # Agregar facet
  if (length(levels(getElement(data, eje_facet))) > 1) {
    
    ggp <- ggp +
      facet_wrap(as.formula(paste0('.~',eje_facet)),
                 strip.position = posicion_facet) +
      theme(strip.placement = 'outside',
            strip.background = element_blank())
  }
  
  return(ggp)
}


#### Función para obtener número de observacoines, con formato correcto ###
getN <- function(grafico, var_ola='var_x', var_lista=NULL) {
  
  if (is.null(var_ola)) {
    grafico$datos.grafico$var_ola <- factor('1')
    var_ola <- 'var_ola'
  }
  
  if (is.null(var_lista)) {
    grafico$datos.grafico$var_lista <- factor('1')
    var_lista <- 'var_lista'
  }
  
  n <- grafico$datos.grafico %>% group_by(!!as.name(var_ola), !!as.name(var_lista)) %>% 
    mutate(n = sum(n_obs)) %>% ungroup() %>% 
    summarize(n = max(n)) %>% 
    pull()
  
  return(format(round(n,0), nsmall = 0, big.mark = '.', decimal.mark = ','))
}

#------GRÁFICO DE BARRAS de frecuencias de var_y por var_x ------
gr.bar.freq <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                        suma100 = 'x', expand = FALSE, datos = FALSE, limits_y = c(0,1), posicion = 'dodge2', 
                        reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, ... ) {

  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, data = data, ponderador = ponderador, 
                              expand = ifelse(posicion=='stack', TRUE, expand), suma100 = suma100,
                              stat = 'freq', reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z)
  
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 
  
  # Se genera el grafico
  ggp <- grafico(eje_x = 'var_x', eje_color = 'var_y', eje_facet = 'var_z', data = datos.grafico, limits_y = limits_y, 
                 formato = 'bar', posicion = posicion, ...)
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
}

#------GRÁFICO DE BARRAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
gr.bar.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                            REC = TRUE, limits_y = c(0,1), posicion = 'dodge2', reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, 
                            suma100 = 'x', expand = FALSE, datos = FALSE, ...) {  
  
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, var_w = var_w, data = data, ponderador = ponderador, 
                              expand = ifelse(posicion=='stack', TRUE, expand), REC = REC, stat = 'freq', 
                              reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z) 
  
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 
  
    # Se genera el grafico
  ggp <- grafico(eje_x = 'var_x', eje_color = 'var_z', eje_facet = 'var_w', 
                 data = subset(datos.grafico, var_y == '1'), 
                 limits_y = limits_y, formato = 'bar', posicion = posicion, ...)
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
}

#------GRÁFICO DE BARRAS de frecuencias de lista de variables en var_y, por var_z ------
gr.bar.freq.list <- function(var_y, var_z = NULL, REC = NULL,  data = elsoc_panel_m1, ponderador = 'ponderador02', modo = 1, 
                             suma100 = 'x', expand = FALSE, datos = FALSE, label_list=NULL, reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE,  ...) {  
  
  if (length(var_y)>1) {
    if (modo==1) {
      datos.grafico <- data %>% 
        dplyr::select(all_of(var_y), var_z, ponderador) %>% 
        lapply(sticky::unsticky) %>% data.frame() %>% 
        pivot_longer(cols = all_of(var_y), names_to = 'var_x', values_to = 'var_y') %>%
        drop_na()
      
      if (!is.null(label_list)) {
        datos.grafico <- datos.grafico %>% 
          mutate(var_x = factor(var_x, labels = label_list)) 
      }
      
      ggp <- gr.bar.freq('var_y', 'var_x', var_z, data = datos.grafico, ponderador = ponderador, 
                         reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, posicion_facet = 'bottom', ...)
    } else {
      datos.grafico <- data %>% 
        dplyr::select(all_of(var_y), var_z, ponderador) %>% 
        lapply(sticky::unsticky) %>% data.frame() %>% 
        pivot_longer(cols = all_of(var_y), names_to = 'var_q', values_to = 'var_y') %>%
        drop_na()
      
      if (!is.null(label_list)) {
        datos.grafico <- datos.grafico %>% 
          mutate(var_q = factor(var_q, labels = label_list)) 
      }
      
      ggp <- gr.bar.freq('var_y', var_x = var_z, var_z = 'var_q', data = datos.grafico, ponderador = ponderador, 
                         reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, posicion_facet = 'bottom', ...)
      
    }
  
  } else if (length(var_z)>1) {
    datos.grafico <- data %>% 
      dplyr::select(var_y, all_of(var_z), ponderador) %>% 
      lapply(sticky::unsticky) %>% data.frame() %>% 
      pivot_longer(cols = all_of(var_z), names_to = 'var_z', values_to = 'var_x') %>%
      drop_na()
    
    if (!is.null(label_list)) {
      datos.grafico <- datos.grafico %>% 
        mutate(var_z = factor(var_z, labels = label_list)) 
    }
    
    ggp <- gr.bar.freq(var_y, 'var_x', 'var_z', data = datos.grafico, ponderador = ponderador, 
                       reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, posicion_facet = 'bottom', ...)
  }
  
  return(ggp)
  }


#------GRÁFICO DE BARRAS de frecuencias de lista de variables en var_y recodificado por var_z ------
gr.bar.freq.rec.list <- function(var_y, var_z = 'ola', var_w = NULL, REC = TRUE,  data = elsoc_panel_m1, 
                             ponderador = 'ponderador02', label_list = NULL, modo = 1,
                             reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, 
                             suma100 = 'x', expand = FALSE, datos = FALSE, ...) {  

  
  if (length(var_y)>1) {
    
    # Se reordenan datos para que sea en función de listado de variables
    datos.grafico <- data %>% 
      dplyr::select(all_of(var_y), var_z, var_w, ponderador) %>%
      lapply(sticky::unsticky) %>% data.frame() %>% 
      pivot_longer(cols = all_of(var_y), names_to = 'var_q', values_to = 'var_y') %>%
      drop_na()

        if (!is.null(label_list)) {
          datos.grafico <- datos.grafico %>%
            mutate(var_q = factor(var_q, labels = label_list))
          }
    
    if (modo == 1) {
      ggp <- gr.bar.freq.rec('var_y', 'var_q', var_z, var_w, data = datos.grafico, ponderador = ponderador, 
                             reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, 
                             REC = REC, posicion_facet = 'bottom', ...)
    } else if (modo == 2) {
      ggp <- gr.bar.freq.rec('var_y', var_z, 'var_q', var_w, data = datos.grafico, ponderador = ponderador, 
                             reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, 
                             REC = REC, posicion_facet = 'bottom', ...)
    } else {
      ggp <- gr.bar.freq.rec('var_y', var_z, var_w, 'var_q', data = datos.grafico, ponderador = ponderador, 
                             reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, 
                             REC = REC, posicion_facet = 'bottom', ...)
    }
    
    
  } else if (length(var_z)>1) {
    datos.grafico <- data %>% 
      dplyr::select(var_y, all_of(var_z), var_w, ponderador) %>% 
      lapply(sticky::unsticky) %>% data.frame() %>% 
      pivot_longer(cols = all_of(var_z), names_to = 'var_z', values_to = 'var_x') %>%
      drop_na()
    
    if (!is.null(label_list)) {
      datos.grafico <- datos.grafico %>% 
        mutate(var_z = factor(var_z, labels = label_list)) 
    }
    
    ggp <- gr.bar.freq.rec(var_y, 'var_x', 'var_z', var_w, data = datos.grafico, ponderador = ponderador, 
                           reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, 
                           REC = REC, posicion_facet = 'bottom', ...)
    
  } else if (length(var_w)>1) {
    datos.grafico <- data %>% 
      dplyr::select(var_y, var_z, all_of(var_w), ponderador) %>% 
      lapply(sticky::unsticky) %>% data.frame() %>% 
      pivot_longer(cols = all_of(var_w), names_to = 'var_w', values_to = 'var_x') %>%
      drop_na()
    
    if (!is.null(label_list)) {
      datos.grafico <- datos.grafico %>% 
        mutate(var_w = factor(var_w, labels = label_list)) 
    }
    
    ggp <- gr.bar.freq.rec(var_y, 'var_x', var_z, 'var_w', data = datos.grafico, ponderador = ponderador, 
                           reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z, datos = datos, 
                           REC = REC, posicion_facet = 'bottom', ...)
  }
  
  
    return(ggp)
}

#------GRÁFICO DE BARRAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.bar.media <- function(var_y, var_x = 'ola', var_z = NULL, var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02', 
                         suma100 = 'x', expand = FALSE, datos = FALSE, reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, ...) {  
  
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, var_w = var_w, data = data, ponderador = ponderador, expand = expand,
                              stat = 'media', reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z)
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  }

    # Se genera el grafico
  ggp <- grafico(eje_color = 'var_z', eje_x = 'var_x', eje_facet = 'var_w', data = datos.grafico, ... )
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
  }

#------GRÁFICO DE BARRAS de MEDIAS de lista var_y y, opcionalmente, var_z ------

gr.bar.media.list <-  function(var_y, var_z = 'ola', var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02', 
                               datos = FALSE, reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, ... ) {
  
  # Se reordenan datos para que sea en función de listado de variables
  datos.grafico <- data %>% 
    dplyr::select(all_of(var_y), var_z, var_w, ponderador) %>% 
    lapply(sticky::unsticky) %>% data.frame() %>% 
    pivot_longer(cols = all_of(var_y), names_to = 'var_x', values_to = 'var_y') %>%
    drop_na()
  
  ggp <- gr.bar.media('var_y', 'var_x', var_z, var_w, data = datos.grafico, ponderador = ponderador,
                      datos = datos, reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z,  ...)
  
    return(ggp)

  }

#------------------------------------------------
#----------- C.- GRAFICOS DE LINEA --------------
#------------------------------------------------

#------GRÁFICO DE LINEAS de frecuencias de var_y por var_x ------
gr.line.freq <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                         expand = FALSE, datos = FALSE, limits_y = c(0,1), colores = c(0,.8,-1), ...) {
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, data = data, ponderador = ponderador, expand = expand,
                              stat = 'freq')
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 

  # Se genera el grafico
  ggp <- grafico(eje_x = 'var_x', eje_color = 'var_y', eje_facet = 'var_z', data = datos.grafico, limits_y = limits_y,
                 formato = 'line', colores = colores, ...)

  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
}

#------GRÁFICO DE LINEAS de frecuencias de var_y recodificado por var_x y, opcionalmente, var_z ------
gr.line.freq.rec <- function(var_y, var_x = 'ola', var_z = NULL, var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                            REC = TRUE, expand = FALSE, datos = FALSE, limits_y = c(0,1), colores = c(0,.8,-1), ... ) {
  
  
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, var_w = var_w, data = data, ponderador = ponderador, expand = expand, 
                              REC = REC, stat = 'freq')

  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 
  
    # Se genera el grafico
  ggp <- grafico(eje_x = 'var_x', eje_color = 'var_z', eje_facet = 'var_w', data = subset(datos.grafico, var_y == '1'),
                 limits_y = limits_y, colores = colores,
                 formato = 'line', ...)
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
}

#------GRÁFICO DE LINEAS de frecuencias de lista de variables en var_y recodificado por var_z ------
gr.line.freq.rec.list <- function(var_y, var_x = 'ola', var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                                  REC = TRUE, suma100 = 'x', expand = FALSE, label_list = NULL, datos = FALSE, ...) {
  
  # Se reordenan datos para que sea en función de listado de variables
  datos.grafico <- data %>% 
    dplyr::select(all_of(var_y), var_x, var_w, ponderador) %>% 
    lapply(sticky::unsticky) %>% data.frame() %>% 
    pivot_longer(cols = all_of(var_y), names_to = 'var_z', values_to = 'var_y') %>%
    drop_na()
  
  if (!is.null(label_list)) {
    datos.grafico <- datos.grafico %>%
      mutate(var_z = factor(var_z, labels = label_list))
  }
  
  ggp <- gr.line.freq.rec('var_y', var_x, 'var_z', var_w, data = datos.grafico, ponderador = ponderador, 
                         REC = REC, datos = datos, ... )
  
    return(ggp)
  
}

#------GRÁFICO DE LINEAS de MEDIAS de var_y por var_x y, opcionalmente, var_z ------
gr.line.media <- function(var_y, var_x = 'ola', var_z = NULL, var_w = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02', 
                         suma100 = 'x', expand = FALSE, datos = FALSE, colores = c(0,.8,-1), ...) {  
  
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, var_w = var_w, data = data, ponderador = ponderador, expand = expand,
                              stat = 'media')
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 
  
  # Se genera el grafico
  ggp <- grafico(eje_color = 'var_z', eje_x = 'var_x', eje_facet = 'var_w', formato = 'line',
                data = datos.grafico, colores = colores, ... )

  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
    return(ggp)
}


#---------------------------------------------------------
#---------------- D.- GRÁFICO ALLUVIAL -------------------
#---------------------------------------------------------


#------GRÁFICO ALLUVIAL de frecuencias de var_y por ola y por otras variables ------
gr.alluvial.freq <- function(var_y, var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                             REC = NULL, flow = 'forward', invertir_labels = FALSE,
                             titulo = NULL, subtitulo = NULL, label_y = NULL, label_x = NULL,
                             titulo_x = NULL, titulo_y = NULL, posicion_leyenda = 'right',
                             datos = FALSE, limits_y = c(0,1),
                             reverse_y = FALSE, reverse_x = FALSE, reverse_z = FALSE, 
                             colores = c(0, .9, -1),
                             colores_tipo = 'viridis',
                             tamano_letra = 1) {
  
  datos.grafico <- datos.stat(var_y = var_y, var_x = 'ola', var_z = var_z, var_w = 'idencuesta', data = data, ponderador = ponderador,
                     stat = 'freq', REC = REC, suma100 = 'xz', full = T, reverse_y = reverse_y, reverse_x = reverse_x, reverse_z = reverse_z)

  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  }

  freqs <- datos.stat(var_y = 'var_y', var_x = 'var_x', var_z = 'var_z', data = datos.grafico, ponderador = 'stat',
                            stat = 'freq', expand =T) %>% 
    filter(var_x %in% datos.grafico$var_x)
  
  # Ajustar tamaño de letra
  tamano_letra <- 20*tamano_letra
  
  # Colores de eje color 
  if (length(unique(datos.grafico$var_y)) == 2 & identical(colores, c(0, .9, -1))) {
    colores <- c(0.33,.66,-1)
  }
  
  #Llamado ggplot
  ggp <- ggplot(datos.grafico,
                aes(x = var_x, 
                    alluvium = var_w,
                    y = stat,
                    fill = var_y,
                    stratum = var_y)) +
    theme_bw(base_size = tamano_letra) +
    ggalluvial::geom_flow(aes.flow = flow,
                          alpha = .66) + 
    ggalluvial::geom_stratum(linetype = 0) +
    scale_y_continuous(labels = scales::percent, # Escalas de eje Y
                       limits = c(0,1)) +
    labs(y = titulo_y,
         x = titulo_x) +
    theme(legend.position = posicion_leyenda,
          legend.title = element_blank()) +
    coord_cartesian(ylim = limits_y)

  # Agregar etiquetas de valores:
  n <- length(unique(freqs$var_y))
  negro_blanco <- case_when(colores[3] == 1  ~ ifelse((colores[1] + seq(1:n)*(colores[2]-colores[1])/(n+1)) < .5, 'white', 'black'),
                            colores[3] == -1 ~ ifelse((colores[2] - seq(1:n)*(colores[2]-colores[1])/(n+1)) < .5, 'white', 'black'))
  ggp <- ggp + geom_text(data = freqs, aes(label = ifelse(stat<.03, '', scales::percent(stat, accuracy = .1, big.mark = '.', decimal.mark = ',' ))),
                         position = position_stack(vjust = .5), 
                         size= tamano_letra/5,
                         show.legend = FALSE,
                         color = rep(negro_blanco, length(unique(freqs$var_x))*length(unique(freqs$var_z)))
                         )

  # Etiqueta de eje y
  if (!is.null(label_y)) { # Si se definen etiquetas del eje y
    ggp <- ggp + scale_fill_viridis_d(begin = colores[1], end = colores[2], direction = colores[3], option = colores_tipo,
                         labels = ajustar_texto(label_y),
                         guide = guide_legend(reverse = invertir_labels))  
    
  } else {
    ggp <- ggp + scale_fill_viridis_d(begin = colores[1], end = colores[2], direction = colores[3], option = colores_tipo,
                                      labels = function(k) ajustar_texto(k))
    guide = guide_legend(reverse = invertir_labels)
  }
    
    # Agregar facet
  if (length(levels(datos.grafico$var_z)) > 1) {
    ggp <- ggp +
      facet_wrap(.~var_z)
  }
  
  # Agregar título si se especifíca:
  if (!is.null(titulo)) {
    ggp <- ggp + 
      ggtitle(label = cortar_texto(titulo, largo = 70))
  }
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
    return(ggp)
}

#---------------------------------------------------------
#----------- E.- TABLAS con diseño muestral --------------
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

#-------------------------------------------------------------------
#----------- F.- Gráfico de frecuencias + porcentajes --------------
#-------------------------------------------------------------------

#------GRÁFICO DE BARRAS de frecuencias de var_y por var_x ------
gr.bar.n <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel_m1, ponderador = 'ponderador02',
                     suma100 = 'x', expand = FALSE, datos = FALSE, limits_y = c(0,1), posicion = 'dodge2', ...) {
  
  # Generar datos para gráfico
  datos.grafico <- datos.stat(var_y = var_y, var_x = var_x, var_z = var_z, data = data, ponderador = ponderador, 
                              expand = ifelse(posicion=='stack', TRUE, expand), suma100 = suma100,
                              stat = 'freq')
  
  # Si se pide, se entregan los datos en vez de el grafico
  if (datos) {
    return(datos.grafico)
  } 
  
  # Se genera el grafico
  ggp <- grafico(eje_x = 'var_x', eje_color = 'var_y', eje_facet = 'var_z', data = datos.grafico, limits_y = limits_y, 
                 formato = 'bar_n', posicion = posicion, ...)
  
  # Se agregan datos:
  ggp$datos.grafico <- datos.grafico
  
  return(ggp)
}

################################################
#--------------GUARDAR OBJETOS DEL ENVIROMENT EN ARCHIVO-----------------
save(datos.stat, grafico, ajustar_texto, getN, gr.bar.media.list, gr.bar.freq, gr.bar.freq.rec, gr.bar.media, gr.line.freq, gr.line.freq.rec, gr.line.media, gr.alluvial.freq, tb.freq, tb.freq.rec, gr.bar.freq.list, gr.bar.freq.rec.list, gr.line.freq.rec.list, cortar_texto, gr.bar.n, 
     file = "inputs/data/datos_y_funciones/funciones_a_usar.RData")


#---------------LIMPIAR ENVIROMENT SALIDA------------------
# rm(list = ls())

