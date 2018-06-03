library(ggplot2)
head(Viajes)

#Analizis de las Variables Categoricas

#Frecuencia de las Rutas
test <- table(Viajes$Ruta) #591
test <- data.frame(Conteo = unclass(table(Viajes$Ruta)))
test$Nueva <- row.names(test)
ggplot(test, aes(x = test$Nueva,y = test$Conteo)) + geom_boxplot() 

#Frecuencia del Tipo de Flota
test <- levels(Viajes$Tipo.de.Flota) #12
test <- data.frame(Conteo = unclass(table(Viajes$Tipo.de.Flota)))
test$Nueva <- row.names(test)
ggplot(test, aes(x = test$Nueva,y = test$Conteo)) + geom_boxplot() 

test <- Viajes
ggplot(test, aes(x = Tipo.de.Flota)) + 
  geom_bar(fill = "chartreuse") + 
  theme(axis.text.x = element_text(angle = 90))

#Viajes de los transportistas
test <- levels(Viajes$Transportista) #87
test <- data.frame(Conteo = unclass(table(Viajes$Transportista)))
test$Nueva <- row.names(test)
ggplot(test, aes(x = test$Nueva,y = test$Conteo)) + geom_boxplot() 

#Origenes
test <- levels(Viajes$Origen) #7
test <- data.frame(Conteo = unclass(table(Viajes$Origen)))
test$Nueva <- row.names(test)
ggplot(test, aes(x = test$Nueva,y = test$Conteo)) + geom_boxplot() 

#Destinos
test <- levels(Viajes$Destino) #182
test <- data.frame(Conteo = unclass(table(Viajes$Destino)))
test$Nueva <- row.names(test)
ggplot(test, aes(x = test$Nueva,y = test$Conteo)) + geom_boxplot() 


  #Combinaciones de Variables Categoricas (Falta extender un poco mas el analisis para posibles insights)

test <- table(Viajes$Ruta, Viajes$Tipo.de.Flota)

test <- data.frame(unclass(table(Viajes$Ruta, Viajes$Transportista)))

test <- data.frame(unclass(table(Viajes$Origen, Viajes$Destino)))

test[test!=0] <- 1

test[8,] <- colSums(test)
test$Suma_Filas <- rowSums(test)

#Analizis de Variables Numericas
test <- Viajes

#Costos por Tipo de Flota
ggplot(test, aes(x = X.Tarifa.pagada.)) +
  geom_histogram() +
  facet_wrap(~ Tipo.de.Flota)

ggplot(test, aes(x = Tipo.de.Flota, y = X.Tarifa.pagada.)) +
  geom_boxplot()

summary(Viajes)

Suma <- aggregate(Viajes$X.Tarifa.pagada., by = list(Cate=Viajes$Tipo.de.Flota), FUN = sum)

#Densidad por tarifa pagada
test$Indice <- seq(1,131189)

test %>%
  ggplot(aes(X.Tarifa.pagada.)) +
  geom_histogram(binwidth = 50)

test %>%
  ggplot(aes(x = 1, y = X.Tarifa.pagada.)) +
  geom_boxplot()

test %>% 
  ggplot(aes(x = X.Tarifa.pagada.)) +
  geom_density()

summary(test$X.Tarifa.pagada.)

#Podemos ver que tenemos dos colas
quantile(test$X.Tarifa.pagada.) #Partiremos los costos apartir de los quantiles de manera homogenea
#Se decidio 100,000 por el numero de costos que componene esta parte
cola_1 <- subset(test, test$X.Tarifa.pagada. < 100000)

cola_1 %>% 
  ggplot(aes(x = cola_1$X.Tarifa.pagada.)) +
  geom_density()

cola_2 <- subset(test,test$X.Tarifa.pagada. >= 100000)

cola_2 %>%
  ggplot(aes(X.Tarifa.pagada.)) +
  geom_histogram(binwidth = 50)

cola_2 %>%
  ggplot(aes(x = 1, y = X.Tarifa.pagada.)) +
  geom_boxplot()

cola_2 %>% 
  ggplot(aes(x = cola_2$X.Tarifa.pagada.)) +
  geom_density()

summary(cola_2$X.Tarifa.pagada.)
quantile(cola_2$X.Tarifa.pagada.)

cola_2.1 <- subset(cola_2,cola_2$X.Tarifa.pagada. < 175000)

cola_2.1 %>% 
  ggplot(aes(x = cola_2.1$X.Tarifa.pagada.)) +
  geom_density()

cola_3 <- subset(cola_2,cola_2$X.Tarifa.pagada. >= 175000)

cola_3 %>% 
  ggplot(aes(x = cola_3$X.Tarifa.pagada.)) +
  geom_density()

#Aplicando una transformada para suavizar los datos

cola_1 %>% 
  ggplot(aes(x = log(cola_1$X.Tarifa.pagada.))) +
  geom_density()

cola_2 %>% 
  ggplot(aes(x = log(cola_2$X.Tarifa.pagada.))) +
  geom_density()

cola_2.1 %>% 
  ggplot(aes(x = log(cola_2.1$X.Tarifa.pagada.))) +
  geom_density()

cola_3 %>% 
  ggplot(aes(x = log(cola_3$X.Tarifa.pagada.))) +
  geom_density()

#Agregar el mes
#mes <- separate(Viajes, col = Fecha, into = c("m","d","y"), sep = "/")
serie <- select(Viajes, )


table(mes$y)




