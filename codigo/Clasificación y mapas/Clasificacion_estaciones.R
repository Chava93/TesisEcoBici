# en este script se crean mapas y se etiqueta a todas las cicloestaciones para asignarles una estacion
# Meteorologica
### En este script tambiés se crea el archivo de estaciones_clasificadas.csv

### Mapas
library(leaflet)
library(ggmap)
setwd("C:/Users/adrian/Documents/ecobici")
## Base cocloestacoines
location = read.csv('datos/locations.csv',header = T,colClasses = c(rep('numeric',3)))
loc = location[,c(1:3)]
loc$class = 'Cicloestacion'
head(loc)
###########

## Base Estaciones meteorológicas
library(data.table)
met=fread('http://148.243.232.112:8080/opendata/catalogos/cat_estacion.csv',header = TRUE, skip=1)
met=met[obs_estac=='',.(id=cve_estac,lat=latitud,lon=longitud)]
#met = met[id  %in% c('MGH','MER','HGM')]
met = met[id  %in% c('SHA','IBM','MGH','COY','MER','HGM','MCM')]
met[,class:='Meteor']
#met_selec= met[id  %in% c('MGH','MER','HGM')]
met_selec  = met
head(met_selec)
###########
data = rbind(loc,met_selec)
data

pal = colorFactor(c('red','blue'),data$class)

data$class = as.factor(data$class)

qpal <- colorNumeric("RdYlBu", loc$trips, n = 3)

mex_loc <- geocode("ciuadd de Mexico,condesa")
p1 = leaflet() %>% addTiles()  %>% setView(lng = mex_loc$lon, lat = mex_loc$lat,zoom=13)%>%
        addCircleMarkers(data=loc,
                         lng=~lon,
                         lat=~lat,
                         stroke=F,
                         radius=~4,
                         fillOpacity = 1,
                         color='red')%>%
        addMarkers(data=met,
                   lng=~lon,
                   lat=~lat,
                   popup=~id)
p1


library(ggplot2)

ggplot(data,aes(lat,lon,color=class))+geom_point()

### Classification
library(class)
loc[,2:3] ## Valores por predecir
met[1:3] ## Training set
loc$cluster=knn(met[,2:3],loc[,2:3],cl=met$id,k = 1)
met$cluster=met$id
met_selec= met[id  %in% c('MGH','IBM','SHA','MGM','MCM','MER','COY','IZT','CAM','CCA','HGM')]
data = rbind(loc,met_selec)
data

ggplot(data,aes(x=lat,y=lon,color=cluster,size=class))+geom_point()+geom_point()

head(data)
write.csv(data,'datos/estaciones_clasificadas.csv')
