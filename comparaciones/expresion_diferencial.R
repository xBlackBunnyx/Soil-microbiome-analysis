#Expresion diferencial

#Importamos las librerias
library("readxl")
library("ggplot2")
library("dplyr")
library("tidyr")
library("ggrepel")

#Importamos las tablas
expressed_OTU = read_excel("C:/Users/loren/Desktop/Cosas de R/comparaciones/diff_exp_OTU.xlsx")
expressed_OTU_Hobbs = read_excel("C:/Users/loren/Desktop/Cosas de R/comparaciones/diff_exp_OTU_inverse.xlsx")
#perfil_funcional = read.delim("C:/Users/admin/OneDrive/Escritorio/Trabajo/comparaciones/FunctionalProfilingTMMnormalization.txt", 
 #                             header = TRUE, sep = "\t", dec = ".")

#Creamos una columna nueva que tenga la especie para RROSE
expressed_OTU$split = strsplit(expressed_OTU$OTUs, ";") #Separamos la info segun los ;
expressed_OTU$species = sapply(expressed_OTU$split, tail,1) #nos quedamos el ultimo elemento
#eliminamos las columnas transitorias
expressed_OTU$split = NULL

#Creamos una columna nueva que tenga la especie para HOBBS
expressed_OTU_Hobbs$split = strsplit(expressed_OTU_Hobbs$OTUs, ";") #Separamos la info segun los ;
expressed_OTU_Hobbs$species = sapply(expressed_OTU_Hobbs$split, tail,1) #nos quedamos el ultimo elemento
#eliminamos las columnas transitorias
expressed_OTU_Hobbs$split = NULL

#Filtramos para tener el top10 de RROSE
not_unique = expressed_OTU %>% filter(logFC.std != Inf)
top10RROSE = not_unique %>% arrange(desc(logFC.deseq2)) %>% slice(1:10)

#Filtramos para tener el top10 de HOBBS
not_unique_hobbs = expressed_OTU_Hobbs %>% filter(logFC.std != Inf)
top10HOBBS = not_unique_hobbs %>% arrange(desc(logFC.deseq2)) %>% slice(1:10)

#Modificamos el signo del logFC de Hobbs para que quede en la otra parte del grafico
top10HOBBS$logFC.deseq2[sapply(top10HOBBS$logFC.deseq2, is.numeric)] = top10HOBBS$logFC.deseq2[sapply(top10HOBBS$logFC.deseq2, is.numeric)] * -1

#Unificamos los datos en un solo dataframe
df1 = select(top10RROSE, logFC.deseq2, species)
df2 = select(top10HOBBS, logFC.deseq2, species)
RROSEvsHOBBS = rbind(df1,df2)
disposition = rep(c("UP", "DOWN"), each=10)
RROSEvsHOBBS$Regulation = disposition

#Hacemos el grafico
ggplot(RROSEvsHOBBS, aes(x=reorder(species, logFC.deseq2), y=logFC.deseq2, fill = Regulation)) +
  geom_bar(stat = "identity") + 
  ggtitle("RROSE vs HOBBS") +
  coord_flip()

