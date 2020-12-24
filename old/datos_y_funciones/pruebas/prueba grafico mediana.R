

gr.bar.mediana <- function(var_y, var_x = 'ola', var_z = NULL, data = elsoc_panel, ponderador = 'ponderador02',
                         titulo = NULL, subtitulo = NULL, label_x = NULL, label_z = NULL, limits_y = c(0, NA)) {
  
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
    geom_text(aes(label = format(mediana, digits = 2, nsmall = 1, big.mark = '.', decimal.mark = ',')),
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
  print(ggp)
}
