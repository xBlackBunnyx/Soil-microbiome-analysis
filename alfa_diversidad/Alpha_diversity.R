#Codigo para hacer las graficas de alfa diversidad

#Activamos las librerías que vamos a utilizar

library("ggplot2")
library("phyloseq")
library("tidyr")
library("stringr")
library("dplyr")
library("gridExtra")

#Se importan los datos de la tabla indicando la ruta
tabla_alpha = read.csv("C:/Users/loren/Desktop/Cosas de R/alfa_diversidad/GAIA_alpha_diversity_genus.csv")
tabla_otu = read.delim("C:/Users/loren/Desktop/Cosas de R/alfa_diversidad/species.count.txt", sep = "\t")
metadata = read.delim("C:/Users/loren/Desktop/Cosas de R/alfa_diversidad/alpha_diversity_species_site.txt", sep = "\t")

#Conseguimos las tablas que hacen falta y formateamos los datos
#Arreglamos la tabla de los OTU
data_otu = tabla_otu[,-1]
rownames(data_otu) = tabla_otu[,1]
#Creamos la tabla de taxonomía
taxa_table = data.frame(tabla_otu$OTU)
taxa_table[c("Domain","Phylum", "Order", "Class", "Family", "Genus", "Species")] = str_split_fixed(taxa_table$tabla_otu.OTU,
                                                                                                   ";", 7)
#Creamos la tabla con los metadatos
metadata = metadata %>% select(-c(Observed, Chao1, Shannon, Simpson, Fisher))
metadatos = metadata[,-1]
rownames(metadatos) = metadata[,1]
#Arreglamos la tabla de taxonomia
TAXA = taxa_table[,-1]
rownames(TAXA) = taxa_table[,1]
#Arreglamos la tabla con los datos de la alfa diversidad obtenidos con anterioridad
data_alpha = tabla_alpha[,-1]
rownames(data_alpha) = tabla_alpha[,1]

#Preparamos los datos para que phyloseq los acepte
#Los ponemos en formato matrix
OTU = data.matrix(data_otu)
TAX = as.matrix(TAXA, rownames = TRUE)
#Aplicamos las funciones para introducirlo en phyloseq
OTU.data = otu_table(OTU,  taxa_are_rows = TRUE)
taxa = tax_table(TAX)
sampledata = sample_data(metadatos)
sampledata$SITE = as.factor(sampledata$SITE)

#Ejecutamos la funcion de phyloseq
physeq1 = phyloseq(OTU.data, taxa, sampledata)

#Calculo de Shannon si se quiere
richness = estimate_richness(physeq1, measures = "Shannon")

#Añadimos los valores del indice de Shannon y los observados para poder representarlos
sampledata$Shannon = data_alpha$Shannon
sampledata$Observed = data_alpha$Observed

# plot 1: observed richness
p1 <- ggplot(sampledata, aes(x = SITE, y = Observed, color = SITE)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Site") +
  stat_boxplot(geom = "errorbar", linetype = 1, width = 0.25) +
  ggtitle("Observed richness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# plot 2: Shannon index
p2 <- ggplot(sampledata, aes(x = SITE, y = Shannon, color = SITE)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Site") +
  stat_boxplot(geom = "errorbar", linetype = 1, width = 0.25) +
  ggtitle("Shannon index") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Juntamos los dos gráficos
grid.arrange(p1, p2, ncol = 2)

