

'https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2016-06.csv'

indicator = function(year,n){
  ind=vector()
  n=1
  for(i in year:(year+n)){
    for(j in 1:12){
      if(j<10){
        ind[n]=paste(year,'-','0',j,sep = '')
        n=n+1
        }
      else {
      ind[n]=paste(year,'-',j,sep = '')
      n=n+1 }
    }
    return(ind)
  }
}

indicator(2016,1)
