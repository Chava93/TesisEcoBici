### Mapas
eco <- read.csv("datos/2016-06.csv", 
                colClasses = c("factor","integer","character","integer","character","character","integer","character","character"),header = T)
bicis = aggregate(rep(1,dim(eco)[1]),by=list(eco$Ciclo_Estacion_Retiro),sum)
names(bicis)= c('id','trips')
dim(bicis)
location = read.csv('datos/locations.csv',header = T,colClasses = c(rep('numeric',3)))
dim(location)
loc = merge(location,bicis)
dim(loc)
library(leaflet)
library(ggmap)
library(RColorBrewer)
qpal <- colorNumeric("RdYlBu", loc$trips, n = 3)
mex_loc <- geocode("ciuadd de Mexico,condesa")
p1 = leaflet(loc) %>% addTiles()  %>% setView(lng = mex_loc$lon, lat = mex_loc$lat,zoom=13)%>%
        addCircleMarkers(lng=~lon,
                   lat=~lat,
                   stroke=F,
                   label=~paste('id:',id,' Trips:',trips),
                   color=~qpal(trips),
                   radius=~5,
                   fillOpacity = 1)%>%
        addLegend("bottomright",
                  pal = qpal,
                  values = ~trips,
                  opacity = 1,
                  title='Outgoing trips')

p1

