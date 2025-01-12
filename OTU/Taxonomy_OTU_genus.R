#Codigo para hacer las graficas de OTUs

#Activamos las librerías que vamos a utilizar
library("ggplot2")
library("reshape2")
library("dplyr")
library("data.table")

#Se importan los datos de la tabla indicando la ruta
tabla_OTU = read.csv("C:/Users/loren/Desktop/Cosas de R/OTU/GAIA_metagenomics_genus_percentage.csv")

#Transformamos la tabla para ordenar los datos
colnames(tabla_OTU)[1] = "Genus" #Cambiamos el nombre de la primera columna para que no haya confusiones

#Separamos la tabla segun el Dominio
df_Eukaryota = tabla_OTU %>% filter(grepl("Eukaryota", Taxonomic.lineage))
df_Bacteria = tabla_OTU %>% filter(grepl("Bacteria", Taxonomic.lineage))
df_Viruses = tabla_OTU %>% filter(grepl("Viruses", Taxonomic.lineage))
df_Archaea = tabla_OTU %>% filter(grepl("Archaea", Taxonomic.lineage))
df_Viroids = tabla_OTU %>% filter(grepl("Viroids", Taxonomic.lineage))

#Ponemos todas las dataframes en una lista para manipularlas simultáneamente
df.list = list(df_Eukaryota, df_Bacteria, df_Viruses, df_Archaea, df_Viroids)

#Quitamos la ultima columna de todas las dataframes
df.list = lapply(df.list, function(x) x[names(x) != "Taxonomic.lineage"])

#Transponemos todas las dataframes
df.list = lapply(df.list, function(x) as.data.frame(t(x)))

#Devolvemos los dataframes a modo individual 
names = paste0("mydata", 1:length(df.list))
list2env(setNames(df.list,names), .GlobalEnv)

#Ordenamos los datos para cada dataframe 
#Eucariota
names(mydata1) = mydata1[1,]
mydata1 = mydata1[-1,]
Eukaryota = tibble::rownames_to_column(mydata1, "Sample")
df.Eukaryota = Eukaryota %>% pivot_longer(cols = -Sample, names_to = "Genus", values_to = "OTU")
df.Eukaryota = transform(df.Eukaryota, OTU = as.numeric(OTU))
#Bacteria
names(mydata2) = mydata2[1,]
mydata2 = mydata2[-1,]
Bacteria = tibble::rownames_to_column(mydata2, "Sample")
df.Bacteria = Bacteria %>% pivot_longer(cols = -Sample, names_to = "Genus", values_to = "OTU")
df.Bacteria = transform(df.Bacteria, OTU = as.numeric(OTU))
#Virus
names(mydata3) = mydata3[1,]
mydata3 = mydata3[-1,]
Viruses = tibble::rownames_to_column(mydata3, "Sample")
df.Viruses = Viruses %>% pivot_longer(cols = -Sample, names_to = "Genus", values_to = "OTU")
df.Viruses = transform(df.Viruses, OTU = as.numeric(OTU))
#Arqueas
names(mydata4) = mydata4[1,]
mydata4 = mydata4[-1,]
Archaea = tibble::rownames_to_column(mydata4, "Sample")
df.Archaea= Archaea %>% pivot_longer(cols = -Sample, names_to = "Genus", values_to = "OTU")
df.Archaea = transform(df.Archaea, OTU = as.numeric(OTU))
#Viroides
Viroids = tibble::rownames_to_column(mydata5, "Sample")
names(Viroids) = Viroids[1,]
Viroids = Viroids[-1,]
colnames(Viroids)[1] = "Sample"
df.Viroids = Viroids %>% pivot_longer(cols = -Sample, names_to = "Genus", values_to = "OTU")
df.Viroids = transform(df.Viroids, OTU = as.numeric(OTU))

#Filtramos los datos para cada dataframe
#Eucariota
df_Eukaryota_filtered = filter(df.Eukaryota, OTU > 0.100)
#Bacteria
df_Bacteria_filtered = df.Bacteria %>% filter(OTU > 0.200)
#Virus
df_Virus_filtered = df.Viruses %>% filter(OTU > 0.001)
#Arqueas
df_Archaea_filtered = df.Archaea %>% filter(OTU > 0.001)
#Viroides
df_Viroids_filtered = df.Viroids %>% filter(OTU > 0)

#Realizacion de las graficas para cada dataframe
#Eucariota
ggplot(df_Eukaryota_filtered, aes(x="", y=OTU, fill=Genus)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)
#Bacteria
ggplot(df_Bacteria_filtered, aes(x="", y=OTU, fill=Genus)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)
#Virus
ggplot(df_Virus_filtered, aes(x="", y=OTU, fill=Genus)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)
#Arqueas
ggplot(df_Archaea_filtered, aes(x="", y=OTU, fill=Genus)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)
#Viroides
ggplot(df_Viroids_filtered, aes(x="", y=OTU, fill=Genus)) + geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) + facet_wrap(.~Sample)




