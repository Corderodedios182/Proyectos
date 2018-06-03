rm(list=ls())
library(data.table) ; library(tidyverse) ; require(plyr); require(stringr)

#Importamos datos

Viajes <- fread("~/Hackaton/brewingdatacup-transporte-master/data/TransportChallenge-Viajes.csv")
Coordenadas <- fread("~/Hackaton/brewingdatacup-transporte-master/data/TransportChallenge-Coordenadas.csv")

tables() #Peso de archivos

Viajes <- data.frame(Viajes)
Coordenadas <- data.frame(Coordenadas)

Coordenadas <- unique(Coordenadas)
Viajes <- unique(Viajes)

str(Viajes)

any(is.na(Coordenadas))
any(is.na(Viajes))#FALSE(No contiene N/A)

Viajes[, c(1:dim(Viajes)[2])] <- colwise(str_trim)(Viajes[, c(1:dim(Viajes)[2])]) #Eliminar espacio en antes y despues, que afecten los datos.

Viajes$X.Tarifa.pagada. <- str_replace(Viajes$X.Tarifa.pagada.,"[$]","") #Tarifa en $ pesos Mexicanos.
Viajes$X.Tarifa.pagada. <- str_replace(Viajes$X.Tarifa.pagada.,",","")

#Estructura de los datos
convert.magic1 <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}

Viajes <- convert.magic1(Viajes, c('factor','factor','character','factor','factor','factor','numeric','factor','character'))
Coordenadas <- convert.magic1(Coordenadas, c('factor','numeric','numeric'))


#Cruzamos las bases
Viajes <- merge(Viajes,Coordenadas, by.x = "Destino", by.y = "Lugar")

Viajes <- merge(Viajes,Coordenadas, by.x = "Origen", by.y = "Lugar")

Viajes <- rename(Viajes, c("Latitud.x" = "Latitud_Destino", "Longitud.x" = "Longitud_Destino","Latitud.y" = "Latitud_Origen" , "Longitud.y" = "Longitud_Origen"))

any(is.na(Viajes)) #FALSE(No contiene N/A)

Viajes <- select(Viajes,"Folio", "Ruta", "Fecha", "Tipo.de.Flota","X.Tarifa.pagada.", "Transportista", "ID.Transportista","Origen", "Destino","Longitud_Origen", "Latitud_Origen", "Longitud_Destino", "Latitud_Destino")

#####Sacar las distancias

dist_list <- list()

for (i in 1:nrow(Viajes)) {
  
  dist_list[[i]] <- gdist(lon.1 = Viajes[i,10], 
                          lat.1 = Viajes[i,11], 
                          lon.2 = Viajes[i,12], 
                          lat.2 = Viajes[i,13], 
                          units="km")
  }

dist_mat <- data.frame(km = sapply(dist_list, unlist))

Viajes$Distancias <- dist_mat

rm(Coordenadas,dist_list,dist_mat,i,convert.magic1)