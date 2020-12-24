#Recodificación Contacto Clase Alta
elsoc_panel_m1$d07_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d07), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto', 'Contacto Medio', 'Alto Contacto'))
#Recodificación Contacto Clase baja
elsoc_panel_m1$d13_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d13), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto', 'Contacto Medio', 'Alto Contacto'))


#Recodificación Contacto  Positivo Clase Alta
elsoc_panel_m1$d08_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d08), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto Posistivo', 'Medio Contacto Positivo', 'Alto Contacto Positivo'))
#Recodificación Contacto  Positivo Clase baja
elsoc_panel_m1$d14_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d14), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto Posistivo', 'Medio Contacto Positivo', 'Alto Contacto Positivo'))
Porcentajes de contacto positivo (d08_rec y d14_rec) por ola
Porcentaje de Contacto positivo de clase social alta y baja por clase social c34 para 2019

#Recodificación Contacto  negativo Clase Alta
elsoc_panel_m1$d09_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d09), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto Negativo', 'Medio Contacto Negativo', 'Alto Contacto Negativo'))
#Recodificación Contacto  negativo Clase baja
elsoc_panel_m1$d15_rec <- factor(car::recode(as.numeric(elsoc_panel_m1$d15), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                              levels = c(1,2,3),
                              labels = c('Bajo Contacto Posistivo', 'Medio Contacto Positivo', 'Alto Contacto Positivo'))

# TRato Justo por grupos (d25)
elsoc_panel_m1$trato_justo_cambio_prom <- with(elsoc_panel_m1, c(as.numeric(d25_01)+as.numeric(d25_02)+ as.numeric(d25_03) +
                                                                 as.numeric(d25_04) + as.numeric(d25_05) + as.numeric(d25_06))/6)

elsoc_panel_m1$trato_justo_cambio <- factor(with(elsoc_panel_m1, 
                                                    case_when(elsoc_panel_m1$trato_justo_cambio_prom  <= 3 ~ 1,
                                                              elsoc_panel_m1$trato_justo_cambio_prom < 7 ~ 4,
                                                              elsoc_panel_m1$trato_justo_cambio_prom <= 10 ~ 8)),
                                                    labels = c('Mal Trato', 'Trato Medio', 'Trato Alto'))
# Trato respetuoso a grupos (c35)
elsoc_panel_m1$trato_respetuoso_prom <- with(elsoc_panel_m1, c(as.numeric(c35_01)+as.numeric(c35_02)+ as.numeric(c35_03) +
                                                            as.numeric(c35_04) /4)

# Deprivación relativa
#invertir items
elsoc_panel_m1$d27_02_inv <- car::recode(as.numeric(elsoc_panel_m1$d27_02), '1=5; 2=4; 3=3; 4=2; 5=1', as.factor = TRUE)
elsoc_panel_m1$d27_04_inv <- car::recode(as.numeric(elsoc_panel_m1$d27_04), '1=5; 2=4; 3=3; 4=2; 5=1', as.factor = TRUE)

#Índice de Deprivación Relativa Individual;
elsoc_panel_m1$dep.indi <- with(elsoc_panel_m1, c(as.numeric(d27_01)+as.numeric(d27_02_inv))/2)


#Índice de Deprivación Relativa grupal;

elsoc_panel_m1$dep.grup <- (elsoc_panel_m1$d27_03_w04+elsoc_panel_m1$d27_04_w04_inversa+elsoc_panel_m1$d27_05_w04)/3

elsoc_panel_m1$dep.grup <- with(elsoc_panel_m1, c(as.numeric(d27_03)+as.numeric(d27_04_inv)+as.numeric(d27_05))/3)

