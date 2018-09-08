## Aqui se crea la base para un an?lisis meteorol?gico Y NADA MAS
#  Este codigo descarga las bases de ecobici-cdmx-gob de todo el 2016
## ES NOMRAL QUE APAREZCA 'NAs introduced by coercion' PUES EN UNA PARTE DEL CODIGO ESTO SE UTILIZA PARA SABER EL FORMATO DE LAS FECHAS.
## Esta es la versión dos de base ecobici. Esta vez estratificando la poblacion por cicloestacion de retiiro tambien.
## De paso tomamos algunos registros de g?nero se los usuarios, edad y uso promedio de bicicletas
## 07/11/2017 Time difference of 30.89002 mins
### Este codigo genera el archivo viajes_2016.csv
### C O D I G O - S A G R A D O
library(data.table)
library(dplyr)
rm(list = ls())
source('url_ind.R')
init = Sys.time()
### Variables utilizadas para recopilar datos
gen=list('hombres'=vector(),'mujeres'=vector())
edad=data.table(edad=1:110)
prom=vector()
i=1
########

pickups = data.table()

for(dte in indicator(2016,1)){
        url=paste('https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/',dte,'.csv',sep = '')
        eco =fread(url,
                   colClasses =c("factor","integer","character","integer","character","character","integer","character","character"),
                   header = T,na.strings = 'lab009' )
        print('################################')
        print(dte)
        ### V A L I D A C I O N DE D A T O S #####
        if(!is.na(as.numeric(substr(eco[1,Fecha_Retiro],1,4)))){
                fmat='%Y-%m-%d'}else {fmat='%d/%m/%Y'}
        
        if(grepl('[mM]',eco[1,Hora_Retiro])){
                require(lubridate)
                eco[,Hora_Arribo:=parse_date_time(Hora_Arribo, '%I:%M:%S %p')]
                eco[,Hora_Retiro:=parse_date_time(Hora_Retiro, '%I:%M:%S %p')]
        }
        ###########################################
        
        ######R E C O P I L A C I O N#####
        
        ###Se toma el registro del genero
        a=eco[,.N,by=.(gen=Genero_Usuario)]
        gen$hombres[i]=a[1,N]
        gen$mujeres[i]=a[2,N]
        ##########
        
        ##Se toma el registro de la edad
        b=eco[,.N,by=.(edad=Edad_Usuario)]
        edad=left_join(edad,b,by=c('edad'))
        ###################################
        
        ### Se calcula el uso promedio por bicicleta al mes
        c=eco[,.N,by=.(Bici)]
        prom[i]= c[,mean(N)]
        i=i+1
        #######################
        ### Damos formato a fechas y horas
        eco[,Fecha_Retiro:=as.IDate(Fecha_Retiro,format=fmat)]
        eco[,Fecha_Arribo:=as.IDate(Fecha_Arribo,format=fmat)]
        eco[,Hora_Arribo:=as.ITime(Hora_Arribo)]
        eco[,Hora_Retiro:=as.ITime(Hora_Retiro)]
        
        ## Creamos Base
        base_eco = eco[,.(Fecha_Retiro,
                          Ciclo_Estacion_Retiro,
                          Hora_Retiro,
                          year= year(Fecha_Retiro),
                          month= month(Fecha_Retiro),
                          wday=wday(Fecha_Retiro),
                          day=mday(Fecha_Retiro),
                          hour= hour(Hora_Retiro))]
        print(head(base_eco))
        base_eco = base_eco[,.N,by=.(year,month,wday,day,hour,Ciclo_Estacion_Retiro)]
        
        pickups= bind_rows(pickups,base_eco) 
        
}
write.csv(pickups,file = "C:/Users/adrian/Documents/ecobici/datos/viajes_2016.csv")
write.csv(data.frame('fecha'=indicator(2016,1),'Hombres'=gen[1],'Mujeres'=gen[2],'uso_prom_bicis'=prom),file="C:/Users/adrian/Documents/ecobici/datos/datos_recavados.csv")
print(dim(pickups))
Sys.time() - init


