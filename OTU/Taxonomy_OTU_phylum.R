#Codigo para hacer las graficas de OTUs

#Activamos las librerías que vamos a utilizar

library("ggplot2")
library("reshape2")
library("dplyr")

#Se importan los datos de la tabla indicando la ruta
tabla_OTU = read.csv("C:/Users/loren/Desktop/Cosas de R/OTU/GAIA_metagenomics_phylum.csv")
tabla_OTU$Taxonomic.lineage = NULL #Quitamos la ultima columna (redundante)

#Transformamos la tabla para ordenar los datos
colnames(tabla_OTU)[1] = "Phylum" #Cambiamos el nombre de la primera columna
df_OTU = as.data.frame(t(tabla_OTU)) #Transponemos la tabla
df.OTU_merge = melt(tabla_OTU, id.vars = "Phylum", value.name = "OTU", variable.name = "Sample")

#Filtramos las muestras
df = df.OTU_merge %>% group_by(Sample) %>% slice_max(order_by = OTU, n=10)
df.top = df.OTU_merge %>% group_by(Sample) %>% arrange(OTU, .by_group = TRUE) %>% top_n(10)
#df.OTU_filtered = df %>% filter(OTU > 0)

#Realizacion de las graficas
ggplot(df.top, aes(x=OTU, y=Sample, fill=Phylum)) + geom_bar(stat = "identity", width = 1) 
