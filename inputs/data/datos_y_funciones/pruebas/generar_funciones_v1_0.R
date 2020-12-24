rm(list = ls())
dir_carlos <- "C:/Users/carlo/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"
dir_monserrat <- 'asd'

library(tidyverse)
library(sticky)

setwd(dir_edgardo)
#Mapa devariables + bases
load("Colaboracion con academicos/Insumos para discusion/transformacion_variables/datos_a_usar.RData")

######################################################

#-------0.- FUNCIONES AUXILIARES ------

# Funcion para cortar textos (en caso de titulos o etiquetas muy largas)
cortar_texto <- function(texto, largo = 70){
    return(paste0(strwrap(texto, width = largo), collapse = "\n"))
}

#-------I.- FUNCIONES VISUALES------

#------GRÁFICO DE BARRAS de frecuencias de var_fill por var_x ------
bar_freq_lp <- function(var_fill, var_x = 'ola', data = elsoc_panel, ponderador = 'ponderador02', 
                        titulo = NULL, subtitulo = NULL, var_y = NULL, label_fill = NULL, label_x = NULL) {
    
    # Obtener resutlados estadísticos a graficar
    var_fill <- as.name(var_fill)
    var_x    <- as.name(var_x)
    ponderador <- as.name(ponderador)

    if (!is.null(var_y)) {
        var_y <- as.name(var_y)
        
        datos.grafo <- data %>% 
            filter(!is.na(!!var_y) & !is.na(!!var_x) & !is.na(!!var_fill)) %>% 
            group_by(!!var_x, !!var_y, !!var_fill) %>% 
            summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
            group_by(!!var_x, !!var_fill) %>% 
            mutate(n1 = sum(n2, na.rm = TRUE),
                   perc = n2/n1, 
                   pctlabel = paste0(round(perc*100), "%")) %>% 
            ungroup() %>% 
            drop_na() %>%
            filter(!!var_y == '1')
    } else {
        datos.grafo <- data %>% 
            filter(!is.na(!!var_x) & !is.na(!!var_fill)) %>% 
            group_by(!!var_x, !!var_fill) %>% 
            summarize(n2 = sum(!!ponderador, na.rm = TRUE)) %>% 
            group_by(!!var_x) %>% 
            mutate(n1 = sum(n2, na.rm = TRUE),
                   perc = n2/n1, 
                   pctlabel = paste0(round(perc*100), "%")) %>% 
            ungroup() %>% 
            drop_na()
    }
    
    #Llamado ggplot
    ggp <- ggplot(datos.grafo, 
                  aes(x = as.factor(!!var_x), y = perc, fill = as.factor(!!var_fill))) +
        geom_bar(stat = "identity", 
                 position = "dodge2") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0,1), 
                           breaks = seq(0,1,.2)) +
        # titulos de ejes y leyenda
        labs( y = "Porcentaje", 
              x = cortar_texto(attr(getElement(data, var_x), which = 'label'), largo = 50), 
              fill = cortar_texto(attr(getElement(data, var_fill), which = 'label'), largo = 32)) +
        # Titulo del gráfico
        ggtitle(label = cortar_texto(ifelse(!is.null(titulo),
                                    titulo,
                                    paste0(attr(getElement(data, var_fill), which = 'label'), ' (', var_fill, ')')), largo = 70),
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

#------- Funcion que recodifica variable a dicotómica, y grafica -------------------
bar_freq_rec_lp <- function(var_y, var_x , var_fill = 'ola', data = elsoc_panel, REC = NULL, 
                            titulo = NULL, subtitulo = NULL, label_fill = NULL, label_x = NULL) {
    var_y <- as.name(var_y)
    var_x <- as.name(var_x)
    
    if (is.null(REC)) {
        REC <- c(levels(getElement(data, var_y))[4], levels(getElement(data, var_y))[5])
    }
    
    data_rec <- data %>% # Recodificar variable a graficar según espeicificado en REC
        mutate(var_y_rec = fct_collapse(!!var_y, 
                                           '1' = REC,
                                           other_level = '0'))

    # Agregar titulo y subtitulo según recodificación (si no está predefinido como opción)
    if (is.null(subtitulo)) {
        subtitulo <- paste0('Porcentaje ', paste0(REC, collapse = ' + '))
    }
    
    if (is.null(titulo)) {
        titulo <- paste0(attr(getElement(data, var_y), which = 'label'), ' (', var_y, ')', 
                         ', según ', 
                         attr(getElement(data, var_x), which = 'label'), ' (', var_x, ')')
    }
    
    # Graficar:
    bar_freq_lp(var_y = 'var_y_rec', var_x = var_x, var_fill = var_fill, data = data_rec, titulo = titulo, subtitulo = subtitulo)
}


#------GRAFICO DE BOXPLOT, con ola en eje X ------
grafo.box.lp <- function(varp, data = elsoc_panel, paleta = 'Dark2'){
    varp <- as.name(varp)
    ggp <- ggplot(data, 
                  aes(x = ola, 
                      y = !!varp)) +
        labs( x= "Ola de encuesta") +
        geom_boxplot() +
        scale_y_log10() +
        ggtitle(paste(strwrap(attributes(getElement(data, varp))$label, width = 70), collapse = "\n")) +
        theme_bw()
    print(ggp)
}

######################################################
#-------II.- FUNCIONES DESCRIPTIVAS-------
#------A.- DTIVOS------
dtivos.elsoc <- function(datos){
    n.obs <- function(var){
        return(sum(!is.na(var)))
    } 
    
    n.nsnr <- function(var){
        return(sum(var == -999 | var == -888, na.rm = TRUE)/sum(!is.na(var), na.rm = TRUE))
    }  
    
    rango <- function(var){
        return(paste(range(as.numeric(var), na.rm = TRUE), collapse = "-"))
    }  
    
    media <- function(var){
        return(round(mean(as.numeric(var), na.rm = TRUE), 2))
    }  
    
    mediana <- function(var){
        return(median(as.numeric(var), na.rm = TRUE))
    }  
    
    desvest <- function(var){
        return(round(sd(as.numeric(var), na.rm = TRUE), 2))
    }  
    
    cv <- function(var){
        return(round(sd(as.numeric(var), na.rm=TRUE)/mean(as.numeric(var), na.rm=TRUE), 2))
    }  
    
    mod <- function(var){
        var <- as.numeric(var)
        return(as.numeric(names(table(var))[table(var)==max(table(var))]))
    }
    
    por.moda <- function(var) {   
        var <- as.numeric(var)
        moda <- mod(var)    
        return(round(sum(var == moda, na.rm = TRUE) / sum(!is.na(var), na.rm = TRUE), 2)) # Moda dividida por n?mero de observaciones validass para cada pregunta
    }
    
    #Objetos
    df <- datos
    df1 <- sjlabelled::set_na(datos, na = c(-888, -999))
    #código
    tabla <- df %>% 
        map_dbl(~{n.obs(.x)}) %>% # NUMERO DE OBSERVACIONES VALIDAS
        enframe() %>%
        rename('N_obs' = value) %>%
        as_tibble %>%
        left_join(df %>% 
                      map_dbl(~{n.nsnr(.x)}) %>% # PORCENTAJE NS/NR
                      enframe() %>% 
                      as_tibble %>%
                      rename('NS_NR' = value)) %>%
        left_join(df1 %>% 
                      map_chr(~{rango(.x)}) %>% #RANGO
                      enframe() %>% 
                      as_tibble %>%
                      rename("rango" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{media(.x)}) %>% #MEDIA
                      enframe() %>% 
                      as_tibble %>%
                      rename("media" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{mediana(.x)}) %>% # MEDIANA
                      enframe() %>% 
                      as_tibble %>%
                      rename("mediana" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{desvest(.x)}) %>% # DESV EST
                      enframe() %>% 
                      as_tibble %>%
                      rename("sd" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{cv(.x)}) %>%  # CV
                      enframe() %>% 
                      as_tibble %>%
                      rename("coef_var" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{mod(.x)}) %>% # MODA
                      enframe() %>% 
                      as_tibble %>%
                      rename("moda" = value)) %>%
        left_join(df1 %>% 
                      map_dbl(~{por.moda(.x)}) %>% # PORCENTAJE MODA
                      enframe() %>% 
                      as_tibble %>%
                      rename("por_moda" = value))
    
    tabla <- dplyr::rename(tabla, Nombre = name) 
    return(tabla)
}

#####################################
#---------III.- FUNCIONES PARA TRANSFORMACION DE DATOS------
#------A.- NOT IN OPERATOR----
`%notin%` <- Negate(`%in%`)

################################################
#--------------GUARDAR OBJETOS DEL ENVIROMENT EN ARCHIVO-----------------
rm(dir_carlos, dir_edgardo, dir_monse)
save(list = ls(.GlobalEnv), file = "Colaboracion con academicos/Insumos para discusion/funciones/funciones_a_usar.RData")

#---------------LIMPIAR ENVIROMENT SALIDA------------------
# rm(list = ls())
