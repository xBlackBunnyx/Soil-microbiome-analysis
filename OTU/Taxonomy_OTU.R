#Codigo para hacer las graficas de OTUs

#Activamos las librerías que vamos a utilizar

library("ggplot2")
library("reshape2")
library("dplyr")

#Se importan los datos de la tabla indicando la ruta
tabla_OTU = read.csv("C:/Users/loren/Desktop/Cosas de R/OTU/GAIA_metagenomics_percentage.csv")
tabla_OTU$Taxonomic.lineage = NULL #Quitamos la ultima columna (redundante)

#Transformamos la tabla para ordenar los datos
colnames(tabla_OTU)[1] = "Domain" #Cambiamos el nombre de la primera columna
df_OTU = as.data.frame(t(tabla_OTU)) #Transponemos la tabla
df.OTU_merge = melt(tabla_OTU, id.vars = "Domain", value.name = "OTU", variable.name = "Sample")

#Realizacion de las graficas
ggplot(df.OTU_merge, aes(x="", y=OTU, fill=Domain)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)
