setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

#install.packages("dplyr") #instalamos la libreria dplyr

survey <- read.csv("survey_cleaned.csv", sep = ",", header = T)

survey_original <- read.csv("survey_cleaned.csv", sep = ",", header = T)

#-----------------------------------columna de practica actualmente---------------------------------------------------

survey$practica_actualmente <- as.factor(survey$practica_actualmente)

table(survey$practica_actualmente) # muestra la frecuencia de los estudiantes

prop.table(table(survey$practica_actualmente)) #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$practica_actualmente))) # devuelve la info en forma de columna

df_perc_practica_actualmente <- as.data.frame(prop.table(table(survey$practica_actualmente)))#asignamos el resultado 
                                          #de la linea anterior a la variable df

library(dplyr) #carga la ibreria 

df_perc_practica_actualmente <- df_perc_practica_actualmente %>% arrange(-Freq) #ordena los datos en orden descendente 
                                          #con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_practica_actualmente$Freq)

hist(df_perc_practica_actualmente$Freq)

qqnorm(df_perc_practica_actualmente$Freq)

#-------------------------------------------columna de Practica en primera opcion---------------------------------------

survey$practica_primera_opcion <- as.factor(survey$practica_primera_opcion)

table(survey$practica_primera_opcion)

prop.table(table(survey$practica_primera_opcion))

as.data.frame(prop.table(table(survey$practica_primera_opcion)))

df_perc_primera_opcion <- as.data.frame(prop.table(table(survey$practica_primera_opcion)))

df_perc_primera_opcion <- df_perc_primera_opcion %>% arrange(-Freq)


boxplot(df_perc_primera_opcion$Freq)
hist(df_perc_primera_opcion$Freq)
qqnorm(df_perc_primera_opcion$Freq)


df_perc_primera_opcion[df_perc_primera_opcion$Var1 %in% c("Si"), "estado_estudiante"] <- "Si hace practica en su primera opcion"
df_perc_primera_opcion[df_perc_primera_opcion$Var1 %in% c("No"), "estado_estudiante"] <- "No hace practica en su primera opcion"
df_perc_primera_opcion[is.na(df_perc_primera_opcion$"estado_estudiante"), "estado_estudiante"]<- "no hace practica"

df_perc_primera_opcion <- df_perc_primera_opcion %>% select(Var1,estado_estudiante)

survey <- left_join(survey,df_perc_primera_opcion,by=c("practica_primera_opcion"="Var1"))

survey <- survey[,!(names(survey) %in% c("practica_primera_opcion"))]

#-------------------------------------------columna practica cursando clases--------------------------------------------

survey$practica_cursando_clases <- as.factor(survey$practica_cursando_clases)

table(survey$practica_cursando_clases)

prop.table(table(survey$practica_cursando_clases))

as.data.frame(prop.table(table(survey$practica_cursando_clases)))

df_perc_cursa_clases <- as.data.frame(prop.table(table(survey$practica_cursando_clases)))

df_perc_cursa_clases <- df_perc_cursa_clases %>% arrange(-Freq)


boxplot(df_perc_cursa_clases$Freq)
hist(df_perc_cursa_clases$Freq)
qqnorm(df_perc_cursa_clases$Freq)


df_perc_cursa_clases[df_perc_cursa_clases$Var1 %in% c("Si"), "clases_durante_practica"] <- "Si cursa clases durante practica"
df_perc_cursa_clases[df_perc_cursa_clases$Var1 %in% c("No"), "clases_durante_practica"] <- "No cursa clases durante practica"
df_perc_cursa_clases[is.na(df_perc_cursa_clases$"clases_durante_practica"), "clases_durante_practica"]<- "no hace practica"

df_perc_cursa_clases <- df_perc_cursa_clases %>% select(Var1,clases_durante_practica)

survey <- left_join(survey,df_perc_cursa_clases,by=c("practica_cursando_clases"="Var1"))

survey <- survey[,!(names(survey) %in% c("practica_cursando_clases"))]


#----------------------------------------------columna Rango indice-----------------------------------------------

survey$rango_indice <- as.factor(survey$rango_indice)

table(survey$rango_indice)

prop.table(table(survey$rango_indice))

as.data.frame(prop.table(table(survey$rango_indice)))

df_perc_rango_indice <- as.data.frame(prop.table(table(survey$rango_indice)))

df_perc_rango_indice <- df_perc_rango_indice %>% arrange(-Freq)

boxplot(df_perc_rango_indice$Freq)

hist(df_perc_rango_indice$Freq)

qqnorm(df_perc_rango_indice$Freq)

#----------------------------------------------columna rendimiento academico-------------------------------------

survey$rendimiento_academico <- as.factor(survey$rendimiento_academico) 

table(survey$rendimiento_academico)

prop.table(table(survey$rendimiento_academico))

as.data.frame(prop.table(table(survey$rendimiento_academico)))

df_perc_rendimiento_academico <- as.data.frame(prop.table(table(survey$rendimiento_academico)))

df_perc_rendimiento_academico <- df_perc_rendimiento_academico %>% arrange(-Freq)

boxplot(df_perc_rendimiento_academico$Freq)

hist(df_perc_rendimiento_academico$Freq)

qqnorm(df_perc_rendimiento_academico$Freq)

#----------------------------------------------------------------------------------------------------------------

mis_variables_a_tratar <- survey %>% select(practica_actualmente, estado_estudiante, clases_durante_practica, rango_indice, rendimiento_academico)

#----------------------------------------------------------------------------------------------------------------

as.data.frame(prop.table(table(survey$practica_actualmente)) *100)

#---------------------grafica practica actualmente---------------------

grf1 <- barplot(prop.table(table(survey$practica_actualmente)), las=1, main = "Estudiantes haciendo practica")
text(grf1, c(0,0), round(prop.table(table(survey$practica_actualmente)), 3))

#---------------------grafica de su primera opcion---------------------

grf2 <- barplot(prop.table(table(survey$estado_estudiante)), las=1, main = "estudiantes haciendo practica en su primer opcion")
text(grf2, c(0,0), round(prop.table(table(survey$estado_estudiante)), 3))

#---------------------grafica cursando clases con practica-------------

grf3 <- barplot(prop.table(table(survey$clases_durante_practica)), las=1, main = "estudiantes cursando clases en practica")
text(grf3, c(0,0), round(prop.table(table(survey$clases_durante_practica)), 3))

#---------------------grafica rango de indice--------------------------

grf4 <- barplot(prop.table(table(survey$rango_indice)), las=1, main = "rangos de indice de los estudiantes")
text(grf4, c(0,0), round(prop.table(table(survey$rango_indice)), 3))

#---------------------grafica rendimiento academico--------------------

grf5 <- barplot(prop.table(table(survey$rendimiento_academico)), las=1, main = "rendimiento academico segun estudiantes")
text(grf5, c(0,0), round(prop.table(table(survey$rendimiento_academico)), 3))

#--------------------------------------------------------------