setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

#install.packages("dplyr") #instalamos la libreria dplyr

survey <- read.csv("survey_cleaned.csv", sep = ",", header = T)

#------------------------------------------------------------------------------------------------------------

survey$practica_actualmente <- as.factor(survey$practica_actualmente)

table(survey$practica_actualmente) # muestra la frecuencia de los estudiantes

prop.table(table(survey$practica_actualmente))*100 #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$practica_actualmente))) # devuelve la info en forma de columna

df_perc <- as.data.frame(prop.table(table(survey$practica_actualmente))*100)#asignamos el resultado de la linea anterior a la variable df

library(dplyr) #carga la ibreria 

df_perc <- df_perc %>% arrange(-Freq) #ordena los datos en orden descendente con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc$Freq)

hist(df_perc$Freq)

qqnorm(df_perc$Freq)

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------


