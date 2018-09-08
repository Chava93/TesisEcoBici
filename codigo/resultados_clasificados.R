## Aqui se está trabajando la nueva base de datos del clima encontrada en aire.cdmx.gob.mx
## Este script utiliza 2 archivos: viajes_2016 y meteorología_2016 y se crea el archivo info.csv
## Si es necesario consultar una version aterior de este script está disponible en google drive
### Resultados 2

library(data.table)

wea = fread("C:/Users/adrian/Documents/ecobici/datos/meteorología_2016.csv")
wea= wea[id_station %in% c('SHA','IBM','MGH','COY','MER','HGM','MCM')]
wea= wea[id_parameter %in% c('PM10','WSP','TMP','RH')]

head(wea)
wea[,.(.N),by='id_station']

library(purrr)
wea$fecha=map_chr(wea$date,function(x){unlist(strsplit(x,' '))[1]})
wea$year=as.integer(map_chr(wea$fecha,function(x){(unlist(strsplit(x,'/'))[3])}))
wea$month=as.integer(map_chr(wea$fecha,function(x){unlist(strsplit(x,'/'))[2]}))
wea$day=as.integer(map_chr(wea$fecha,function(x){unlist(strsplit(x,'/'))[1]}))
wea$hour=as.integer(map_chr(wea$date,function(x){unlist(strsplit(unlist(strsplit(x,' '))[2],':'))[1]}))
head(wea)
wea=wea[,.(year,month,day,hour,id_station,id_parameter,value,unit)]
weap=reshape(wea,timevar ='id_parameter' ,idvar =c('year','month','day','hour','id_station'),direction='wide' )
head(weap)


## Cambiamos todas las horas = 24 por 0 para que esté en el mismo horario que ecobici
weap = as.data.table(weap)
weap[hour==24,]$hour=0

library(dplyr)
######
trips=fread("C:/Users/adrian/Documents/ecobici/datos/viajes_2016.csv")
trips$Ciclo_Estacion_Retiro = as.integer(trips$Ciclo_Estacion_Retiro)
trips=trips[,-c(1)]

trips[,.(.N),by=.(hour)]
## hay un registro de un viaje a la 1 hrs. no será tomado en cunata

trips = trips[hour!=1,]

#### Cargar base de est_clasificadas y hacer join con wea para despues hacer join con trips
## con ciclo_estacion_retiro
clasif=fread('C:/Users/adrian/Documents/ecobici/datos/estaciones_clasificadas.csv')
clasif=clasif[class=='Cicloestacion',-c(1,3:5)]
clasif$id = as.integer(clasif$id)
colnames(clasif)=c('id','id_station')
p1=left_join(trips,clasif,by=c('Ciclo_Estacion_Retiro'='id'))
sum(is.na(p1)) ## Todos los Na son cicloestacones que no están en el mapa
p1=p1[!is.na(p1$id_station),]
head(p1)


## Union
info = left_join(p1,weap)
info=as.data.table(info)
info[,sum(is.na(value.RH)),by='id_station']

sum(is.na(info$value.RH))/length(info$value.RH)

info = info[!is.na(info$value.RH),]

write.csv('data/info.csv')

