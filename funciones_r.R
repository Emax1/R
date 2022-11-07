#tabla de frecuencia para variables continuas
tfcont<-function(v,nombre){
r<-max(v)-min(v)
n<-length(v)
k<-round(1+3.32*log10(n),0)
w<-ceiling(r/k)
LS<-c()
  LI<-c()
  LS[1]=min(v)+w
  LI[1]=min(v)
  for (i in 2:k){ 
    LS[i]=LS[i-1]+w
    LI[i]=LI[i-1]+w
  }
  h<-hist(v,seq(min(v),max(LS),w),plot=F)
  fi<-h$counts
  hi<-fi/sum(fi)
  pi<-hi*100
  Fi<-cumsum(fi)
  Hi<-cumsum(hi)
  Pi<-cumsum(pi)
  hist(v,seq(min(v),max(LS),w),freq=FALSE,col='darkolivegreen1',plot=T,main = paste('Histograma de la Variable:',nombre))
  lines(density(v),col="red",lwd=2)
  abline(v=mean(v),col='red')
  abline(v=median(v),col='blue')
  curve(dnorm(x,mean=mean(v),sd=sd(v)), from=800,to=1600,add=TRUE, col="blue", lwd=2)
  K<-1:k
  atributos<-cbind(K,LI,LS,fi,hi,pi,Fi,Hi,Pi)
  totales<-c(' ',' ','TOTALES',sum(fi),sum(hi),sum(pi),' ',' ',' ')
  r<-rbind(atributos,totales)
   return (r)
   }

#tabla de frecuencia y grafico para varaibles cualitativas
tfgvc<-function(x){
  fi<-table(x)
  n<-length(x)
  hi<-fi/n
  phi<-hi*100
  pie(phi)
  Resultado<-cbind(fi,hi,phi)
  return(Resultado)
}
#tabla de frecuencia y grafico para varaibles Discretas
tfgvd<-function(x){
  fi<-table(x)
  n<-length(x)
  hi<-fi/n
  phi<-hi*100
  plot(phi)
  Resultado<-cbind(fi,hi,phi)
  return(Resultado)
}