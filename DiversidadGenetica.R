#'Conectar la carpeta

setwd("pathway")

library(readxl)
library(mice)
library(vegan)
library(ggplot2)
library(ape)
library(dplyr)
library(ggrepel)

mat <- read_excel("hormigas.xlsx", sheet = "Hoja1")

# Extraer etiquetas
etiquetas <- mat[[1]]

# Eliminar columna de etiquetas
mat <- mat[, -1]

# Convertir a numérico
mat <- as.data.frame(lapply(mat, as.numeric))

# Convertir 9 → NA
mat[mat == 9] <- NA

#Imputación con MICE

meth <- rep("logreg", ncol(mat))
imp  <- mice(mat, method = meth, m = 1, maxit = 20, print = FALSE)
mat_imp <- complete(imp)

# Asegurar binario
mat_imp <- ifelse(mat_imp >= 0.5, 1, 0)

#Distancias

dist_jaccard <- vegdist(mat_imp, method = "jaccard", binary = TRUE, na.rm = TRUE)
hc <- hclust(dist_jaccard, method = "average")

plot(hc, labels = etiquetas,cex = 0.7, hang = -1, main = "Dendrograma UPGMA")

##Grafica de analisis de coordenas principales

#Distancias de jaccard

dist_jaccard <- vegdist(mat_imp, method = "jaccard", binary = TRUE, na.rm = TRUE)

#PCoA
pcoa_res <- pcoa(dist_jaccard)

coord <- as.data.frame(pcoa_res$vectors[, 1:2])
colnames(coord) <- c("PCoA1", "PCoA2")
coord$Etiqueta <- etiquetas

#Clustering 
hc <- hclust(dist_jaccard, method = "average")
coord$Grupo <- cutree(hc, k = 3)

#Grafica 

ggplot(coord, aes(x = PCoA1, y = PCoA2, color = factor(Grupo))) +
  geom_hline(yintercept = 0, linewidth = 0.7, color = "black") +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "black") +
  geom_point(size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = Etiqueta), vjust = -0.8, size = 4) +
  stat_ellipse(type = "t", linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  labs(
    title = "PCoA",
    x = "PCoA1",
    y = "PCoA2",
    color = "Grupo"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )