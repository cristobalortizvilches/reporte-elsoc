rm(list = ls())
dir_carlos <- "C:/Users/carlo/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"
dir_monserrat <- 'asd'

library(tidyverse)
library(sticky)
library(survey)
library(ggrepel)
library(ggalluvial)

setwd(dir_edgardo)
#Mapa devariables + bases
load("Colaboracion con academicos/Insumos para discusion/transformacion_variables/datos_a_usar.RData")

# Funcion para cortar textos (en caso de titulos o etiquetas muy largas)
cortar_texto <- function(texto, largo = 70){
    return(paste0(strwrap(texto, width = largo), collapse = "\n"))
}

### --------------------------------------- ###

#------GRÁFICO alluvial de frecuencias de var_y por ola ------
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
