#Limpiar enviroment entrada
rm(list = ls())
####### Script para generar datos a usar en archivo de insumos para discusión con académicos######
library(tidyverse)
library(sticky)

dir_carlos <- "C:/Users/pgmej/Dropbox/RCS2020"
dir_edgardo <- "C:/Users/edgar/Dropbox/ELSOC_personal/RCS2019"

setwd(dir_edgardo)

# MAPA.VAR   <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a-4zz3VWKYARUT89iyspWUoLGFpdUcvNmqkjN16rT8g/edit#gid=1286914746",
#                                         sheet = "Temario y CONCEPTO GENERAL")

# Importar bases de datos LONG
load("Bases/ELSOC_Long_2016_2019_labelled_v2.00.RData")

#Importar bases de datos WIDE
load('Bases/ELSOC_Wide_2016_2019_v1.00_R.RData')

elsoc_long <- sjlabelled::set_na(elsoc_long_2016_2019, na = c(-888, -999))
elsoc_wide  <- elsoc_wide_2016_2019
elsoc_wide[elsoc_wide == -888 | elsoc_wide == -999] <- NA

# Factorizar todas las variables posibles
pasar_a_factor <- function(var) {
    if ((length(na.omit(unique(var))) <= 5 )  & 
        length(na.omit(unique(var))) == length(attr(var, which = 'labels')) & 
        class(var) != 'factor') { 
        return(factor(var, labels = names(attr(var, which = 'labels'))))
    } else {
        return(var)
    }
}

elsoc_long <- lapply(elsoc_long, pasar_a_factor) %>% data.frame()
elsoc_long <- sjlabelled::set_label(elsoc_long, var_labels)

#---------A.-RECODIFICACION EDUCACION--------------------
elsoc_long$educ <- car::recode(elsoc_long$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_long$educ <- factor(elsoc_long$educ,labels = c("Basica","Media","Tecnica","Universitaria"))

#-----A.1--PONER ATRIBUTOS LABEL Y LABELS
elsoc_long$educ <- sjlabelled::set_label(elsoc_long$educ, label = c("Nivel Educacional"))
elsoc_long$educ <- sjlabelled::set_labels(elsoc_long$educ, labels = c("Basica", "Media", "Tecnica", "Universitaria"))

#----B.- RECODIFICACION RELIGION
elsoc_long$relig <- car::recode(elsoc_long$m38, "1=1;c(2,3)= 2; c(4,5,6)= 3;c(7,8,9)=4")
elsoc_long$relig <- factor(elsoc_long$relig,labels = c("Católico","Evangélico","Otro Credo","No Creyente"))

#-----B.1 PONER ATRIBUTOS LABEL Y LABELS
elsoc_long$relig <- sjlabelled::set_label(elsoc_long$relig, label = c("Religion entrevistado"))
elsoc_long$relig <- sjlabelled::set_labels(elsoc_long$relig, labels = c("Catolico", "Evangelico", "Otro Credo", "No Creyente"))

elsoc_long$estrato <- factor(elsoc_long$estrato, 
                             levels = c(1,2,3,4,5,6),
                             labels = names(attributes(elsoc_long$estrato)$labels) 
                             )

elsoc_long$estrato <- sjlabelled::set_label(elsoc_long$estrato, label = c("Estrato Muestral"))

#-----E REACOFDICACION EDAD-------------------
elsoc_long$edadt <- factor(car::recode(elsoc_long$m0_edad, "18:29=1;30:49=2;50:64=3;65:150=4"),
                           labels = c('18-29', '30-49', '50-64', '65 o más'))
attr(elsoc_long$edadt, which = 'label') <- 'Tramo de edad'

#-----E.1 ATRIBUTOS EDAD----------------------
elsoc_long$edadt <- sjlabelled::set_label(elsoc_long$edadt, label = c("Edad en Tramos"))
elsoc_long$edadt <- sjlabelled::set_labels(elsoc_long$edadt, labels = c("18 a 29", "30 a 49", "50 a 64", " 65 o más"))

#------F RECODiFiCACION ZONA GEOGRAFICA-------------------------
elsoc_long$zona1  <- car::recode(elsoc_long$region,"c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')= 1;c('Valparaiso','Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule','Bio Bio')= 2;c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;'Metropolitana'= 4")
elsoc_long$zona1  <- factor(elsoc_long$zona1,levels=c("1","2","3","4"),
                            labels = c("Norte","Centro","Sur","Metropolitana"))
#------F.1 ATRIBUTOS ZONA--------
elsoc_long$zona1 <- sjlabelled::set_label(elsoc_long$zona1, label = c("Zona Geográfica"))
elsoc_long$zona1 <- sjlabelled::set_labels(elsoc_long$zona1, labels = c("Norte", "Centro", "Sur", "Metropolitana"))

#------F.2 ZONA GEOGRÁFICA SIN GRANDES URBES
elsoc_long$zona2  <- car::recode(elsoc_long$region,"c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')= 1;c('Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule')= 2;c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;else=NA")
elsoc_long$zona2  <- factor(elsoc_long$zona2,levels=c("1","2","3"),
                            labels = c("Norte","Centro","Sur"))

elsoc_long$zona2 <- sjlabelled::set_label(elsoc_long$zona2, label = c("Zona Geográfica"))
elsoc_long$zona2 <- sjlabelled::set_labels(elsoc_long$zona2, labels = c("Norte", "Centro", "Sur"))


#-------G.- SINTOMATOLOGIA DEPRESION--------------

# Variables de sintomatología depresiva: 
elsoc_long$s11_01_rec <- as.numeric(car::recode(elsoc_long$s11_01, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_02_rec <- as.numeric(car::recode(elsoc_long$s11_02, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_03_rec <- as.numeric(car::recode(elsoc_long$s11_03, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_04_rec <- as.numeric(car::recode(elsoc_long$s11_04, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_05_rec <- as.numeric(car::recode(elsoc_long$s11_05, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_06_rec <- as.numeric(car::recode(elsoc_long$s11_06, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_07_rec <- as.numeric(car::recode(elsoc_long$s11_07, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_08_rec <- as.numeric(car::recode(elsoc_long$s11_08, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_09_rec <- as.numeric(car::recode(elsoc_long$s11_09, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1

#PHQ-9: Índice Aditivo de Puntajes de Síntomas Depresivos
elsoc_long$suma_dep <- with(elsoc_long, s11_01_rec + s11_02_rec + s11_03_rec + s11_04_rec + s11_05_rec + s11_06_rec + s11_07_rec + s11_08_rec + s11_09_rec)

elsoc_long$depr <- car::recode(elsoc_long$suma_dep,"c(0,1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")
elsoc_long$depr <- factor(elsoc_long$depr,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                            "Depresion Moderada Severa a Severa"))  

#------------------G.1 ATRIBUTOS DEPR---------------
elsoc_long$depr <- sjlabelled::set_label(elsoc_long$depr, label = c("Sintomatologia Depresiva"))

#-------H RECODIFICACION EMPLEO
elsoc_long$empleo <- car::recode(elsoc_long$m02,"c(1,2,3) = 1; 6 = 2; 7 = 3; 5 = 4; c(4, 8, 9) = 5")
elsoc_long$empleo <- factor(elsoc_long$empleo, 
                            labels = c("Ocupado", "Desempleado", "Trabajo no remunerado", "Jubilado o pensionado", "Otros inactivos"))

attr(elsoc_long$empleo, which = 'label') <- 'Situacion ocupacional'

#-----------I.- INMOBILIRARIO PUBLICO
elsoc_long$inm.pub <- car::recode(elsoc_long$f05_09,"c('Nunca se justifica', 'Pocas veces se justifica') = 0;c('Algunas veces se justifica', 'Muchas veces se justifica', 'Siempre se justifica')=1")
elsoc_long$inm.pub <- sjlabelled::set_label(elsoc_long$inm.pub, label = c(" Se justifica el daño al inmobiliario público"))
elsoc_long$inm.pub <- sjlabelled::set_labels(elsoc_long$inm.pub, labels = c("No se Justifica", "Se justifica"))

# --- J.- Quintiles de ingreso (del hogar) --- *

#Imputar punto medio de rangos de ingreso
elsoc_long$m30 <- as.numeric(car::recode(elsoc_long$m30,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA"))
elsoc_long$m29_imp <- ifelse(!is.na(elsoc_long$m29), elsoc_long$m29, elsoc_long$m30)

# Deflactar a precios de cada año:
elsoc_long$deflactor <- with(elsoc_long, case_when(
    ola == 2016 ~ 113.88/123.82,
    ola == 2017 ~ 116.46/123.82,
    ola == 2018 ~ 119.45/123.82,
    ola == 2019 ~ 123.82/123.82
))

# N hogar:
elsoc_long$n_hogar <- with(elsoc_long, case_when(
    ola == 2016 ~ nhogar1,
    ola == 2017 ~ m46_nhogar,
    ola == 2018 ~ m54,
    ola == 2019 ~ m54
))

# Ingreso per capita del hogar:
elsoc_long$ing_pc <- elsoc_long$m29_imp/elsoc_long$n_hogar

elsoc_long <- elsoc_long %>% 
    group_by(ola) %>% 
    mutate(quintil = ntile(-desc(ing_pc), 5)) %>% 
    ungroup()
elsoc_long$quintil <- as.factor(elsoc_long$quintil) # Quintiles como factores

# Agregar etiquetas
attr(elsoc_long$quintil, which = 'label') <- 'Quintil de Ingreso per cápita del hogar'
attr(elsoc_long$ing_pc, which = 'label') <- 'Ingreso per cápita del hogar' 


#-----------K.- Identificación política

#Recodificar posición ideológica
elsoc_long$pos_id <- car::recode(elsoc_long$c15, recodes = "c(0,1,2,3, 4)=1; c(5)=2; c(6,7,8,9,10)=3; c(11,12)=4; else=NA", as.factor = TRUE)

#Pasar a factor
elsoc_long$pos_id <- factor(elsoc_long$pos_id,
                            levels = c(1, 2, 3, 4),
                            labels = c("Izquierda", "Centro", "Derecha", "No se identifica"))
#Atributos
elsoc_long$pos_id <- sjlabelled::set_label(elsoc_long$pos_id, "Posición ideológica")
elsoc_long$pos_id <- sjlabelled::set_labels(elsoc_long$pos_id, labels = c("Izquierda", "Centro", "Derecha", "No se identifica"))

# Identificación con partidos

elsoc_long$idpart <- factor(car::recode(elsoc_long$c16,
                                 "c(1,2)='PC+PH';c(3,4)='PRO+RD';c(5,7,9)='EVO+AMP+PRI';
                                 c(6,8,12,13)='PPD+PDC+PS+PR';c(10,11)='RN+UDI';14='Otro';15='Ninguno'"),
                            levels = c('PC+PH', 'PRO+RD', 'EVO+AMP+PRI', 'PPD+PDC+PS+PR', 'RN+UDI', 'Otro', 'Ninguno'))

elsoc_long$idcoal <- factor(car::recode(elsoc_long$c17, "c('Otro. Especifique cual')='Otro'"),
                            levels = c('Chile Vamos', 'Nueva Mayoria', 'Frente Amplio',
                                       'Otro', 'Ninguna')) 

elsoc_long$id_sin <- factor(elsoc_long$idpart == 'Ninguno' & elsoc_long$idcoal == 'Ninguna',
                               levels = c(FALSE, TRUE),
                               labels = c('Se identifica con algún partido y/o coalición política', 'No se identifica con ningún partido ni coalición política'))

# Participación electoral retrospectiva
elsoc_long <- elsoc_long %>% mutate(
    particip_electoral = factor(case_when(c11 == 1 & ola == 2016 ~ "Sí (en 2013)",
                                   (c11 == 2 | c11 == 3) & ola == 2016 ~ "No (en 2013)",
                                   c11 == 1 & ola == 2018 ~ "Sí (en 2017)",
                                   c11 == 2 & ola == 2018 ~ "No (en 2017)")))

#-------------L.- Participación en movimientos sociales-----

elsoc_long$participa <- car::recode(as.numeric(elsoc_long$c22), recodes = "c(1)=1; c(2,3,4,5)=2; else=NA", as.factor = TRUE)

elsoc_long$participa <- factor(elsoc_long$participa,
                               levels = c(1,2),
                               labels = c("No Participa", "Participa"))
elsoc_long$participa <- sjlabelled::set_label(elsoc_long$participa, "Participación en Mov. Sociales")
elsoc_long$participa <- sjlabelled::set_labels(elsoc_long$participa, labels = c("No Participa", "Participa"))


#-----------------M.- Clase subjetiva en cinco categorías-------

elsoc_long$clase.sub2 <- car::recode(as.numeric(elsoc_long$d01_01), recodes = "c(1,2)=1; c(3,4)=2; c(5,6)=3; c(7,8)=4; c(9,10)=5; else=NA", as.factor = TRUE)

elsoc_long$clase.sub2 <- factor(elsoc_long$clase.sub2,
                                levels = c(1,2,3,4,5),
                                labels = c("Más Bajo", "Bajo", "Medio", "Alto", "Más Alto"))
elsoc_long$clase.sub2 <- sjlabelled::set_label(elsoc_long$clase.sub2, "Lugar subjetivo en la sociedad chilena")
elsoc_long$clase.sub2 <- sjlabelled::set_labels(elsoc_long$clase.sub2, labels = c("Más Bajo", "Bajo", "Medio", "Alto", "Más Alto"))


#--------------------N.- Clase subjetiva en tres categorías-------------------------
elsoc_long$clase.sub <- car::recode(as.numeric(elsoc_long$d01_01), recodes = "c(1,2,3,4)=1; c(5)=2; c(6,7,8,9,10)=3;else=NA", as.factor = TRUE)

elsoc_long$clase.sub <- factor(elsoc_long$clase.sub,
                               levels = c(1,2,3),
                               labels = c("Bajo", "Medio", "Alto"))
elsoc_long$clase.sub <- sjlabelled::set_label(elsoc_long$clase.sub, "Lugar subjetivo en la sociedad chilena")
elsoc_long$clase.sub <- sjlabelled::set_labels(elsoc_long$clase.sub, labels = c("Bajo", "Medio", "Alto"))




#----------------------------------
#--------TRATAMIENTO DE OUTLIERS (Se crea una nueva variable con el 98% de la distribución y el resto NA)
#2.- d03_02
elsoc_long$d03_02_r <- elsoc_long$d03_02
elsoc_long$d03_02_r[elsoc_long$d03_02_r < quantile(elsoc_long$d03_02_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d03_02_r > quantile(elsoc_long$d03_02_r, .99, na.rm = TRUE)[[1]]] <- NA
#4.- d04_02
elsoc_long$d04_02_r <- elsoc_long$d04_02
elsoc_long$d04_02_r[elsoc_long$d04_02_r < quantile(elsoc_long$d04_02_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d04_02_r > quantile(elsoc_long$d04_02_r, .99, na.rm = TRUE)[[1]]] <- NA
#5.- m15
elsoc_long$m15_r <- elsoc_long$m15
elsoc_long$m15_r[elsoc_long$m15_r < quantile(elsoc_long$m15_r, .01, na.rm = TRUE)[[1]] | elsoc_long$m15_r > quantile(elsoc_long$m15_r, .99, na.rm = TRUE)[[1]]] <- NA
#1.- d03_01
elsoc_long$d03_01_r <- elsoc_long$d03_01
elsoc_long$d03_01_r[ elsoc_long$d03_01_r < quantile(elsoc_long$d03_01_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d03_01_r > quantile(elsoc_long$d03_01_r, .999, na.rm = TRUE)[[1]]] <- NA

#3.- d04_01
elsoc_long$d04_01_r <- elsoc_long$d04_01
elsoc_long$d04_01_r[elsoc_long$d04_01_r < quantile(elsoc_long$d04_01_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d04_01_r > quantile(elsoc_long$d04_01_r, .99, na.rm = TRUE)[[1]]] <- NA


#----------------------------------------------------------------------------------------------------
#----ESCALAS----

### GENERALES
#1.- PROSOCIAL
elsoc_long$prosocial <-  c(as.numeric(elsoc_long$c07_01) + as.numeric(elsoc_long$c07_02) + as.numeric(elsoc_long$c07_03) +
                                as.numeric(elsoc_long$c07_04) + as.numeric(elsoc_long$c07_05) + as.numeric(elsoc_long$c07_06) +
                                as.numeric(elsoc_long$c07_07) + as.numeric(elsoc_long$c07_08)) / 8


###TERRITORIO

#I.- CONFLICTOS BARRIALES 
elsoc_long$confli.barrial <- c(as.numeric(elsoc_long$t11_01) + as.numeric(elsoc_long$t11_02) +
                                    as.numeric(elsoc_long$t11_03) + as.numeric(elsoc_long$t11_04)) / 4

elsoc_long$apoyo.soci <- c(as.numeric(elsoc_long$c07_01)+ as.numeric(elsoc_long$c07_03)) /2

### CAMBIO CONSTITUCIONAL ####

# Recodificar variables de conformidad y acuerdo de cambio constitucion
elsoc_long$conformidad_constitucion <- factor(with(elsoc_long, case_when(
    c26 == 'Muy disconforme' | c26 == 'Disconforme' ~ 1,
    c26 == 'Indiferente' ~ 2,
    c26 == 'Conforme' | c26 == 'Muy conforme' ~ 3)),
    labels = c('Disconforme o muy disconforme', 'Indiferente', 'Conforme o muy conforme'))

elsoc_long$acuerdo_cambio_constitucion <- factor(with(elsoc_long, case_when(
    c28 == 'Totalmente en desacuerdo' | c28 == 'En desacuerdo' ~ 1,
    c28 == 'Ni de acuerdo ni en desacuerdo' ~ 2,
    c28 == 'De acuerdo' | c28 == 'Totalmente de acuerdo' ~ 3)),
    labels = c('En desacuerdo o totalente en desacuerdo', 'Ni de acuerdo ni en desacuerdo', 'De acuerdo o totalmente de acuerdo'))

# Participación en procesos constituyentes:

table(elsoc_long$ola, elsoc_long$c31)

elsoc_long$participacion_2016 <- factor(with(elsoc_long, case_when(
    c31 == 'Si' & ola == 2016 ~ 'Si', 
    c31 == 'No' & ola == 2016 ~ 'No'),
    labels = c('Si', 'No')))

elsoc_long$participacion_2019 <- factor(with(elsoc_long, case_when(
    c31 == 'Si' & ola == 2019 ~ 'Si', 
    c31 == 'No' & ola == 2019 ~ 'No'),
    labels = c('Si', 'No')))


### PERCEPCION DE DESIGUALDAD Y MERITO
elsoc_long$merito <- c(as.numeric(elsoc_long$d05_01) + as.numeric(elsoc_long$d05_02) + 
                            as.numeric(elsoc_long$d05_03) + as.numeric(elsoc_long$d05_04))/4
# CONFIANZA EN ISNTITUCIONES
elsoc_long$confi.insti <- c(as.numeric(elsoc_long$c05_01) + as.numeric(elsoc_long$c05_02) + 
                            as.numeric(elsoc_long$c05_03) + as.numeric(elsoc_long$c05_04) +
                            as.numeric(elsoc_long$c05_05) + as.numeric(elsoc_long$c05_06) +
                            as.numeric(elsoc_long$c05_07) + as.numeric(elsoc_long$c05_08)) /8


# RECOMPENSA DEL MERITO (Concepto "Justicia distributiva y meritocracia")
elsoc_long$recompensa <- c(as.numeric(elsoc_long$c18_09) +
                                as.numeric(elsoc_long$c18_10)) /2 

# AUTOEFICACIA POLITICA
elsoc_long$autopolitica <- c(as.numeric(elsoc_long$c10_01) + as.numeric(elsoc_long$c10_02)+
                                  as.numeric(elsoc_long$c10_03)) /3

# TOLERANCIA 

elsoc_long$tolera <- c(as.numeric(elsoc_long$d26_01) + as.numeric(elsoc_long$d26_02)+
                            as.numeric(elsoc_long$d26_03)+as.numeric(elsoc_long$d26_04)) /4

#----SOCIABILIDAD BARRIAL----
elsoc_long$sociabili <-  c(as.numeric(elsoc_long$t03_01) + as.numeric(elsoc_long$t03_02) + as.numeric(elsoc_long$t03_03) +
                                as.numeric(elsoc_long$t03_04)) / 4

#------Con Pareja sin Pareja
elsoc_long$pareja <- factor(car::recode(elsoc_long$m36, "c(4,5,6,7,8)=0;c(1,2,3)= 1;else=NA", as.factor = T),
                             labels = c("Sin Pareja","Con Pareja"))

################################# Cambios en base WIDE #####################

#Edad
elsoc_wide$edad_w01 <- car::recode(elsoc_wide$m0_edad_w01, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_wide$edad_w02 <- car::recode(elsoc_wide$m0_edad_w02, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_wide$edad_w03 <- car::recode(elsoc_wide$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4")
elsoc_wide$edad_w04 <- car::recode(elsoc_wide$m0_edad_w04, "18:29=1;30:49=2;50:64=3;65:100=4")

#Educación Entrevistado
elsoc_wide$educ_w01 <- factor(car::recode(elsoc_wide$m01_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w02 <- factor(car::recode(elsoc_wide$m01_w02, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w03 <- factor(car::recode(elsoc_wide$m01_w03, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w04 <- factor(car::recode(elsoc_wide$m01_w04, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))

elsoc_wide$pos_id_w01  <- factor(car::recode(elsoc_wide$c15_w01,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w02  <- factor(car::recode(elsoc_wide$c15_w02,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w03  <- factor(car::recode(elsoc_wide$c15_w03,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w04  <- factor(car::recode(elsoc_wide$c15_w04,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

# Recodificar variables de conformidad y acuerdo de cambio constitucion
elsoc_wide$c26_w01_rec <- factor(with(elsoc_wide, case_when(
    c26_w01 == 1 | c26_w01 == 2 ~ 1,
    c26_w01 == 3 ~ 2,
    c26_w01 == 4 | c26_w01 == 5 ~ 3)))

elsoc_wide$c26_w04_rec <- factor(with(elsoc_wide, case_when(
    c26_w04 == 1 | c26_w04 == 2 ~ 1,
    c26_w04 == 3 ~ 2,
    c26_w04 == 4 | c26_w04 == 5 ~ 3)))

elsoc_wide$c28_w01_rec <- factor(with(elsoc_wide, case_when(
    c28_w01 == 1 | c28_w01 == 2 ~ 1,
    c28_w01 == 3 ~ 2,
    c28_w01 == 4 | c28_w01 == 5 ~ 3)))

elsoc_wide$c28_w04_rec <- factor(with(elsoc_wide, case_when(
    c28_w04 == 1 | c28_w04 == 2 ~ 1,
    c28_w04 == 3 ~ 2,
    c28_w04 == 4 | c28_w04 == 5 ~ 3)))

elsoc_wide$conformidad_constitucion <- factor(with(elsoc_wide, case_when(
    c26_w01_rec == 1 & c26_w04_rec == 1 ~ 1,
    c26_w01_rec == 2 & c26_w04_rec == 2 ~ 3,
    c26_w01_rec == 3 & c26_w04_rec == 3 ~ 5,
    c26_w01_rec != 1 & !is.na(c26_w01_rec) & c26_w04_rec == 1 ~ 2,
    c26_w01_rec != 2 & !is.na(c26_w01_rec) & c26_w04_rec == 2 ~ 4,
    c26_w01_rec != 3 & !is.na(c26_w01_rec) & c26_w04_rec == 3 ~ 6)),
    labels = c('Se mantiene disconforme',
               'Cambia a disconforme',
               'Se mantiene indiferente',
               'Cambia a indiferente',
               'Se mantiene conforme',
               'Cambia a conforme'))

elsoc_wide$acuerdo_cambio_constitucion <- factor(with(elsoc_wide, case_when(
    c28_w01_rec == 1 & c28_w04_rec == 1 ~ 2,
    c28_w01_rec == 2 & c28_w04_rec == 2 ~ 4,
    c28_w01_rec == 3 & c28_w04_rec == 3 ~ 6,
    c28_w01_rec != 1 & !is.na(c28_w01_rec) & c28_w04_rec == 1 ~ 1,
    c28_w01_rec != 2 & !is.na(c28_w01_rec) & c28_w04_rec == 2 ~ 3,
    c28_w01_rec != 3 & !is.na(c28_w01_rec) & c28_w04_rec == 3 ~ 5)),
    labels = c('Cambia a en desacuerdo',
               'Se mantiene en desacuerdo',
               'Cambia a indiferente',
               'Se mantiene indiferente',
               'Cambia a de acuerdo',
               'Se mantiene de acuerdo'))


#-----------------------------------
#Poner la clase S3 sitcky para que no se pierdan los atributos con subsets
elsoc_long <- lapply(elsoc_long, sticky::sticky) %>% data.frame()

#Separar las muestras
elsoc_panel <- elsoc_long %>% filter(tipo_atricion == 1 | tipo_atricion == 10)
elsoc_panel_m1 <- dplyr::filter(elsoc_long, muestra == 'Muestra Original' & tipo_atricion == 1)
elsoc_panel_m2 <- dplyr::filter(elsoc_long, muestra == 'Muestra Refresco' & tipo_atricion == 10 )

# Base sin pasar a factor:
# elsoc_panel_sf <- sjlabelled::set_na(elsoc_long_2016_2019, na = c(-888, -999))
# elsoc_panel_m1_sf <- dplyr::filter(elsoc_panel_sf, muestra == 1 & tipo_atricion == 1)
# elsoc_panel_m2_sf <- dplyr::filter(elsoc_panel_sf, muestra == 2 & tipo_atricion == 10)

# Bases wide por submuestra
elsoc_wide_m1 <- elsoc_wide %>% dplyr::filter(tipo_atricion == 1 & tipo_caso != 2)
elsoc_wide_m2 <- elsoc_wide %>% dplyr::filter(tipo_atricion == 10 & tipo_caso != 2)

rm(dir_carlos, dir_edgardo, var_labels, pasar_a_factor, elsoc_long_2016_2019, elsoc_wide_2016_2019)
save(list = ls(.GlobalEnv), 
     file = 'Presentacion/RCS2020 - Presentacion final/funciones_y_otros/datos_a_usar.RData')

# rm(list = ls())
