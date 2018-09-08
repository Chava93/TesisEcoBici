### Mapas
eco <- read.csv("C:/Users/adrian/Documents/ecobici/datos/hist_estaciones.csv", header = T)


location = read.csv('C:/Users/adrian/Documents/ecobici/datos/locations.csv',header = T,colClasses = c(rep('numeric',3)))
location$fase=with(location,ifelse(id<85,'Feb-2010',ifelse(id<90,'Feb-2011',ifelse(id<262,'Feb-2013',ifelse(id<275,'Feb-2014',ifelse(id<446,'Feb-2015','Feb-2016'))))))

library(leaflet)
library(ggmap)
library(RColorBrewer)
qpal <- colorFactor('Set1', location$fase, n = 6)
mex_loc <- geocode("ciuadd de Mexico,condesa")
p1 = leaflet(location) %>% addProviderTiles('OpenStreetMap')  %>% setView(lng = mex_loc$lon, lat = mex_loc$lat,zoom=13)%>%
        addCircleMarkers(lng=~lon,
                         lat=~lat,
                         stroke=F,
                         #label=~paste('id:',id,' Trips:',trips),
                         color=~qpal(fase),
                         radius=~5,
                         fillOpacity = 1)%>%
        addLegend("bottomright",
                  pal = qpal,
                  values = ~fase,
                  opacity = 1,
                  title='Nuevas Cicloestaciones')
       
p1


library(htmlwidgets)
saveWidget(p1, file="C:/Users/adrian/Documents/ecobici/plots/Nuevas_ciclo.html")

### Mapa estratificación
head(location)
location$col = 'red'
location[location$id %in% c(109,104,101,105,106,100,97,95,90,88,94,89,93,262,92),4]='green'
p1 = leaflet(location) %>% addProviderTiles('OpenStreetMap')  %>% setView(lng = mex_loc$lon, lat = mex_loc$lat,zoom=13)%>%
        addCircleMarkers(lng=~lon,
                         lat=~lat,
                         stroke=F,
                         #label=~paste('id:',id,' Trips:',trips),
                         color=~col,
                         radius=~5,
                         fillOpacity = 1)

p1

