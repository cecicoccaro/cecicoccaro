library(xlsx)
library(normtest)
library(car)

#HISTOGRAMAS#
hist(Base_de_datos_postulantes$UNIDXPOND, main = "Histograma de frecuencias", # Frecuencia
     ylab = "Frecuencia")
hist(Base_de_datos_postulantes$FACTXPOND, main = "Histograma de frecuencias2", # Frecuencia
     ylab = "Frecuencia")

#BOXPLOT#
g_caja<-boxplot(Base_de_datos_postulantes$UNIDXPOND, col="skyblue", frame.plot=F)
g_caja$out
g_caja<-boxplot(Base_de_datos_postulantes$FACTXPOND, col="skyblue", frame.plot=F)
g_caja$out

#SUBCONJUNTOS PRE Y POST PANDEMIA#
pre_df<-subset(Base_de_datos_postulantes, Periodo == "Pre pandemia", select=UNIDXPOND:FACTXPOND)
post_df<-subset(Base_de_datos_postulantes, Periodo == "Post pandemia", select=UNIDXPOND:FACTXPOND)

#TEST NORMALIDAD#
shapiro.test(pre_df$UNIDXPOND)
shapiro.test(pre_df$FACTXPOND)
shapiro.test(post_df$UNIDXPOND)
shapiro.test(post_df$FACTXPOND)

#QQPLOTS#
qqnorm(pre_df$UNIDXPOND, 
          main = "Distribución de residuos para la variable discos vivos")
qqline(pre_df$UNIDXPOND, col = 2)
qqnorm(pre_df$FACTXPOND, 
       main = "Distribución de residuos para la variable discos vivos")
qqline(pre_df$FACTXPOND, col = 2)

qqnorm(post_df$UNIDXPOND, 
       main = "Distribución de residuos para la variable discos vivos")
qqline(pre_df$UNIDXPOND, col = 2)
qqnorm(post_df$FACTXPOND, 
       main = "Distribución de residuos para la variable discos vivos")
qqline(pre_df$FACTXPOND, col = 2)

#CALCULO DESVIOS STANDARD#
sd(pre_df$UNIDXPOND)
sd(post_df$UNIDXPOND)
sd(pre_df$FACTXPOND)
sd(post_df$FACTXPOND)

#TEST LEVENE GUALDAD VARIANCIAS#
leveneTest(y = Base_de_datos_postulantes$UNIDXPOND, group = Base_de_datos_postulantes$Periodo, center = "mean")
leveneTest(y = Base_de_datos_postulantes$FACTXPOND, group = Base_de_datos_postulantes$Periodo, center = "mean")

#COMPARACIONES DE MEDIAS PRE Y POST PANDEMIA#
t.test(x=pre_df$UNIDXPOND, y=post_df$UNIDXPOND, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=FALSE, conf.level=0.95)
kruskal.test(UNIDXPOND ~ Periodo, data = Base_de_datos_postulantes)
oneway.test(UNIDXPOND ~ Periodo, data = Base_de_datos_postulantes, var.equal = FALSE)

t.test(x=pre_df$FACTXPOND, y=post_df$FACTXPOND, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=FALSE, conf.level=0.95)