#Analisis del perfil funcional

#Importamos las librerías
library("ggplot2")
library("dplyr")
library("tidyr")
library("ggrepel")

#Importamos las tablas
volcano_data = read.csv("C:/Users/loren/Desktop/Cosas de R/comparaciones/data_volcano.tsv", sep = "\t")

#Volcano plot
#Creamos una columna nueva que tenga la especie
volcano_data$Taxa = row.names(volcano_data) #Se crea una columna con el nombre de las filas
volcano_data$split = strsplit(volcano_data$Taxa, ";") #Separamos la info segun los ;
volcano_data$species = sapply(volcano_data$split, tail,1) #nos quedamos el ultimo elemento
#eliminamos las columnas transitorias
volcano_data$split = NULL
volcano_data$Taxa = NULL

#Añadimos una columna que tenga los valores de FDR
volcano_data = volcano_data %>% mutate(FDR = 10^-minuslog10FDR)

#Filtramos los valores muy grandes de FDR
volcano_data = volcano_data %>% filter(minuslog10FDR < 15)
volcano_data = volcano_data %>% filter(logFC > -8 & logFC < 8)

#Añadimos las columnas de Up, Down y Disexpressed
volcano_data$diffexpressed = "NO"
#Valores Up
volcano_data$diffexpressed[volcano_data$logFC > 0.5 & volcano_data$FDR <0.05] = "Up"
#Valores Down
volcano_data$diffexpressed[volcano_data$logFC < -0.5 & volcano_data$FDR <0.05] = "Down"

#Ponemos las etiquetas
volcano_data$delabel = "NA"
volcano_data$delabel[volcano_data$diffexpressed != "NO"] = volcano_data$species[volcano_data$diffexpressed != "NO"]

#Representacion
ggplot(volcano_data, aes(x=logFC, y=minuslog10FDR, col=diffexpressed, label=ifelse(delabel == "NA", NA, delabel))) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = 5) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.5, 0.5), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
