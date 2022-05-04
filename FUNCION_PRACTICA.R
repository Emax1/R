#FUNCIoN DE MEDIDAS DE RESUMEN
GrafC<-function(x){
  #media
  me=mean(x)
  #mediana
  md=median(x)
  #Cuantiles
  q1=as.numeric(quantile(x,0.25))
  q2=as.numeric(quantile(x,0.50))
  q3=as.numeric(quantile(x,0.75))
  #MEDIDAS DE DISPERSIoN
  #rango
  r=max(x)-min(x)
  #Rango intercuartilico
  iqr=q3-q1
  #GRaFICO DE CAJAS Y BIGOTES
  #par(mfrow=c(1,2))
  boxplot(x,horizontal = TRUE)
  #plot(density(x, adjust = 5),col = 'black', lwd = 3)
  LI=q1-1.5*iqr
  LS=q3+1.5*iqr
  as=3*(mean(x)-median(x))/sd(x)
  d9<-as.numeric(quantile(x,0.9))
  d1<-as.numeric(quantile(x,0.1))
  k=(0.5*(q3-q1))/(d9-d1)
  ss=var(x)
  s=sd(x)
  cv=s/me*100
  numero=c(me,md,q1,q2,q3,r,iqr,LI,LS,round(as,3),k,ss,s,cv)
  nombre=c('Media','Mediana','Q1','Q2','Q3','r','iqr','LI','LS','as','k','var','sd','cv')
  return(cbind(nombre,numero))
}