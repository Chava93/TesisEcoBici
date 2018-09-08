library(data.table)
library(dplyr)
rm(list = ls())
source('url_ind.R')
init = Sys.time()
nd=data.frame(dte=c(indicator(2015,3),indicator(2016,1),indicator(2017,1)[1:8]),
              reb_percentage=0,mov_prom=0)
j=1
for(dte in c(indicator(2015,3),indicator(2016,1),indicator(2017,1)[1:8])){
        url=paste('https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/',dte,'.csv',sep = '')
        eco =fread(url,
                   colClasses =c("factor","integer","character","integer","character","character","integer","character","character"),
                   header = T,na.strings = 'lab009' )
        print('################################')
        print(dte)
        eco=eco[,.(Bici,Ciclo_Estacion_Retiro,Ciclo_Estacion_Arribo)]
        un = unique(eco[,Bici])
        un=un[!is.na(un)]
        reb=vector()
        n=1
        for(bici in un){
                his=eco[Bici==bici]
                d=dim(his)[1]
                reb[n]=sum(!(his[2:d,Ciclo_Estacion_Retiro]==his[1:(d-1),Ciclo_Estacion_Arribo]))
                n=n+1
                
        }
        nd[j,3]=mean(reb,na.rm = T)
        nd[j,2]=sum(reb,na.rm = T)/dim(eco)[1]
        j=j+1

        
}
write.csv(nd,file = 'C:/Users/adrian/Documents/ecobici/datos/reb_data.csv')

 Sys.time() - init

#Mapa % reacomodos
library(data.table)
library(ggplot2)
 
reb=fread('C:/Users/adrian/Documents/ecobici/datos/reb_data.csv') 
reb
meses=c('ENE','FEB','MAR','ABR','MAY','JUN','JUL','AGO','SEP','OCT','NOV','DIC')
anios= 2015:2017
reb$mes=c(rep(meses,2),meses[1:8])

reb[,mes2:=paste(substr(dte,3,4),mes,sep='-')]
reb[,date:=paste(dte,'01',sep = '-')]
reb[,date:=as.Date(date)]

png(filename = 'C:/Users/adrian/Documents/ecobici/plots/Bicis_reacomodadas.png')
j= ggplot(reb,aes(date,reb_percentage,group=1))+geom_point(colour='red',size=2)+geom_line()+theme(axis.text = element_text(angle = 90))
j=j+scale_x_discrete(name='Fecha',labels=reb[,mes2])+scale_y_continuous(name = 'Porcentaje')
j= j+ggtitle('Bicis reacomodadas',subtitle = 'Como % de los viajes totales')+theme(plot.title = element_text(hjust = .5),plot.subtitle =element_text(hjust = .5) )
j
dev.off()
j+geom_vline()
ggplot(reb,aes(1:dim(reb)[1],reb_percentage))+geom_point()

with(nd, plot(dte,reb_percentage)) 
