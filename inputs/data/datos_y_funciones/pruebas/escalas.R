

#ESCALAS
### GENERALES
#1.- PROSOCIAL
elsoc_panel$prosocial <-  c(as.numeric(elsoc_panel$c07_01) + as.numeric(elsoc_panel$c07_02) + as.numeric(elsoc_panel$c07_03) +
                            as.numeric(elsoc_panel$c07_04) + as.numeric(elsoc_panel$c07_05) + as.numeric(elsoc_panel$c07_06) +
                            as.numeric(elsoc_panel$c07_07) + as.numeric(elsoc_panel$c07_08)) / 8


###TERRITORIO

#I.- CONFLICTOS BARRIALES 
elsoc_panel$confli.barrial <- c(as.numeric(elsoc_panel$t11_01) + as.numeric(elsoc_panel$t11_02) +
                              as.numeric(elsoc_panel$t11_03) + as.numeric(elsoc_panel$t11_04)) / 4

elsoc_panel$apoyo.soci <- c(as.numeric(elsoc_panel$c07_01)+ as.numeric(elsoc_panel$c07_03)) /2


### PERCEPCION DE DESIGUALDAD Y MERITO

# MERITO (Concepto "Surgir en la vida")
elsoc_panel$merito <- c(as.numeric(elsoc_panel$c05_01) + as.numeric(elsoc_panel$c05_02) + 
                        as.numeric(elsoc_panel$c05_03) + as.numeric(elsoc_panel$c05_04))/4

# RECOMPENSA DEL MERITO (Concepto "Justicia distributiva y meritocracia")


elsoc_panel$recompensa <- c(as.numeric(elsoc_panel$c18_09) +
                            as.numeric(elsoc_panel$c18_10)) /2 

# AUTOEFICACIA POLITICA

elsoc_panel$autopolitica <- c(as.numeric(elsoc_panel$c10_01) + as.numeric(elsoc_panel$c10_02)+
                              as.numeric(elsoc_panel$c10_03)) /3


# DEPRIVA
elsoc_panel$depriva <- c(as.numeric(elsoc_panel$d27_01) + as.numeric(elsoc_panel$d27_02) + 
                           as.numeric(elsoc_panel$d27_03) + as.numeric(elsoc_panel$d27_04) +
                            as.numeric(elsoc_panel$d27_05)) /5




# TOLERANCIA 

elsoc_panel$tolera <- c(as.numeric(elsoc_panel$d26_01) + as.numeric(elsoc_panel$d26_02)+
                                as.numeric(elsoc_panel$d26_03)+as.numeric(elsoc_panel$d26_04)) /4



elsoc_panel$jusv.delincuencia <- c(as.numeric(elsoc_panel$f05_01)+as.numeric(elsoc_panel$f05_02)) /2
elsoc_panel$jusv.control <- c(as.numeric(elsoc_panel$f05_03)+as.numeric(elsoc_panel$f05_04)) /2
elsoc_panel$jusv.drasticas <- c(as.numeric(elsoc_panel$f05_09)+as.numeric(elsoc_panel$f05_10)+ as.numeric(elsoc_panel$f05_11))/3
elsoc_panel$jusv.cambio <- c(as.numeric(elsoc_panel$f05_06)+as.numeric(elsoc_panel$f05_07))/2

elsoc_panel$emo.rabia <- c(as.numeric(elsoc_panel$c41_01) + as.numeric(elsoc_panel$c41_02)+
                             as.numeric(elsoc_panel$c41_03) + as.numeric(elsoc_panel$c41_04)) /4

elsoc_panel$emo.miedo <- c(as.numeric(elsoc_panel$c42_01) + as.numeric(elsoc_panel$c42_02)+
                             as.numeric(elsoc_panel$c42_03) + as.numeric(elsoc_panel$c42_04)) /4


elsoc_panel$depriva <-   c(as.numeric(elsoc_panel$d27_01) + as.numeric(elsoc_panel$d27_02) + 
                             as.numeric(elsoc_panel$d27_03) + as.numeric(elsoc_panel$d27_04) + 
                             as.numeric(elsoc_panel$d27_05))/5

elsoc_panel$nada <- c(as.numeric(elsoc_panel$f07_01) + as.numeric(elsoc_panel$f07_02)) / 2

