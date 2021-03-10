#Tabla de frecuencias para cualitativas (tfc)
tfc<-function(variable){
  fi<-table(variable)
  hi<-fi/length(variable)
  pi<-hi*100
  atributos<-cbind(fi,hi,pi)
  totales<-c(sum(fi),sum(hi),sum(pi))
  pie(table((variable)))
  r<-rbind(atributos,totales)
  return(r)
}

#Tabla de frecuencias para discretas (tfd)
tfd<-function(variable){
  fi<-table(variable)
  hi<-fi/length(variable)
  pi<-hi*100
  atributos<-cbind(fi,hi,pi)
  totales<-c(sum(fi),sum(hi),sum(pi))
  barplot(table(variable))
  plot(table(variable))
  r<-rbind(atributos,totales)
  return(r)
}
