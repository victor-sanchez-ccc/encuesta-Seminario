setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

#install.packages("dplyr") #instalamos la libreria dplyr

survey <- read.csv("survey_cleaned.csv", sep = ",", header = T)

#-----------------------------------columna de practica actualmente---------------------------------------------------

survey$practica_actualmente <- as.factor(survey$practica_actualmente)

table(survey$practica_actualmente) # muestra la frecuencia de los estudiantes

prop.table(table(survey$practica_actualmente))*100 #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$practica_actualmente))) # devuelve la info en forma de columna

df_perc_practica_actualmente <- as.data.frame(prop.table(table(survey$practica_actualmente))*100)#asignamos el resultado de la linea anterior a la variable df

library(dplyr) #carga la ibreria 

df_perc_practica_actualmente <- df_perc_practica_actualmente %>% arrange(-Freq) #ordena los datos en orden descendente con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_practica_actualmente$Freq)

hist(df_perc_practica_actualmente$Freq)

qqnorm(df_perc_practica_actualmente$Freq)

#-------------------------------------------columna de Practica en primera opcion---------------------------------------
survey$practica_primera_opcion <- as.factor(survey$practica_primera_opcion)

table(survey$practica_primera_opcion)

prop.table(table(survey$practica_primera_opcion))

as.data.frame(prop.table(table(survey$practica_primera_opcion)))

df_perc_primera_opcion <- as.data.frame(prop.table(table(survey$practica_primera_opcion))*100)

df_perc_primera_opcion <- df_perc_primera_opcion %>% arrange(-Freq)

boxplot(df_perc_primera_opcion$Freq)

hist(df_perc_primera_opcion$Freq)

qqnorm(df_perc_primera_opcion$Freq)
#-------------------------------------------columna practica cursando clases--------------------------------------------
survey$practica_cursando_clases <- as.factor(survey$practica_cursando_clases)

table(survey$practica_cursando_clases)

prop.table(table(survey$practica_cursando_clases))

as.data.frame(prop.table(table(survey$practica_cursando_clases)))

df_perc_cursa_clases <- as.data.frame(prop.table(table(survey$practica_cursando_clases))*100)

df_perc_cursa_clases <- df_perc_cursa_clases %>% arrange(-Freq)

boxplot(df_perc_cursa_clases$Freq)

hist(df_perc_cursa_clases$Freq)

qqnorm(df_perc_cursa_clases$Freq)
#----------------------------------------------columna Rango indice-----------------------------------------------
survey$rango_indice <- as.factor(survey$rango_indice)

table(survey$rango_indice)

prop.table(table(survey$rango_indice))

as.data.frame(prop.table(table(survey$rango_indice)))

df_perc_rango_indice <- as.data.frame(prop.table(table(survey$rango_indice))*100)

df_perc_rango_indice <- df_perc_rango_indice %>% arrange(-Freq)

boxplot(df_perc_rango_indice$Freq)

hist(df_perc_rango_indice$Freq)

qqnorm(df_perc_rango_indice$Freq)
#----------------------------------------------columna rendimiento academico-------------------------------------
survey$rendimiento_academico <- as.factor(survey$rendimiento_academico) 

table(survey$rendimiento_academico)

prop.table(table(survey$rendimiento_academico))

as.data.frame(prop.table(table(survey$rendimiento_academico)))

df_perc_rendimiento_academico <- as.data.frame(prop.table(table(survey$rendimiento_academico))*100)

df_perc_rendimiento_academico <- df_perc_rendimiento_academico %>% arrange(-Freq)

boxplot(df_perc_rendimiento_academico$Freq)

hist(df_perc_rendimiento_academico$Freq)

qqnorm(df_perc_rendimiento_academico$Freq)

#------------------------------------------------------------------------------------------------------------


