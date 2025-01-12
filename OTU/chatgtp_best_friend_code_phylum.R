#Codigo para hacer las graficas de OTUs

#Activamos las librerías que vamos a utilizar

library("ggplot2")
library("reshape2")
library("dplyr")
library("tidyr")

#Se importan los datos de la tabla indicando la ruta
tabla_OTU = read.csv("C:/Users/loren/Desktop/Cosas de R/OTU/GAIA_metagenomics_phylum.csv")
tabla_OTU$Taxonomic.lineage = NULL #Quitamos la ultima columna (redundante)

#Transformamos la tabla para ordenar los datos
colnames(tabla_OTU)[1] = "Phylum" #Cambiamos el nombre de la primera columna
df_OTU = as.data.frame(t(tabla_OTU)) #Transponemos la tabla
df.OTU_merge = melt(tabla_OTU, id.vars = "Phylum", value.name = "OTU", variable.name = "Sample")

#Filtramos las muestras para que no haya muestras con valor de 0
df.filtered = df.OTU_merge %>% filter(OTU > 0)

#Agrupamos por Phylum y Sample, sumamos los valores de los OTU
df2 <- df.filtered %>% group_by(Phylum, Sample) %>% summarize(total_OTU = sum(OTU))

#Nos quedamos con el top 10 de los filos por el total de OTUS de todas las muestras
top10_phyla <- df2 %>%
  group_by(Phylum) %>%
  summarize(total_OTU_all_samples = sum(total_OTU)) %>%
  top_n(10, total_OTU_all_samples) %>%
  arrange(desc(total_OTU_all_samples))

#Filtramos el dataframe original para solo incluir los 10 filos
df2_top10 <- df2 %>%
  filter(Phylum %in% top10_phyla$Phylum)

#Eliminamos los duplicados en la columna del Phylum
df2_top10$Phylum <- factor(df2_top10$Phylum, levels = unique(df2_top10$Phylum))

#Hacemos la grafica
ggplot(df2_top10, aes(x = total_OTU, y = Sample, fill = Phylum)) +
  geom_col() +
  labs(x = "Total OTU", y = "Sample", fill = "Phylum") +
  scale_fill_manual(values = rainbow(length(unique(df2_top10$Phylum)))) +
  theme_classic() +
  theme(legend.position = "bottom")
