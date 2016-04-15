###################################################################################
# A.
setwd ("/home/marvin/Escritorio/tarea")
icecube = read.table("ice.txt", header = T)
#######################################
# B.
icecube1 <- complete.cases(icecube) #Remover
icecube2 <- icecube[icecube1, ][1:53, ]#Tabla
####################################################################################
# C.
#2 Metodos para el Promedio
#######################################
#PRIMER METODO
coln <- icecube2[,8:9] # Para concentrarce en Med_Ang_Res_deg y Topology
data <- coln[order(coln$Topology),][1:53, ]# Ordenar en Track y Shower
s=subset(coln, Topology=='Track', select=Med_Ang_Res_deg)   #escoje solo los Track
Ta=subset(coln, Topology=='Shower', select=Med_Ang_Res_deg) #escoje solo los Shower
Shower <- mean(Ta[,"Med_Ang_Res_deg"])
Track <- mean(s[,"Med_Ang_Res_deg"])
Promedio2<-data.table(Shower,Track) #Agrupamos en una tabla y mostramos es Promedio
#######################################
#SEGUNDO METODO
library(data.table)
pollo =data.table(icecube2)
Promedio <- pollo[,list(Prom_MARd=mean(Med_Ang_Res_deg)), by = 'Topology']
####################################################################################
#D. MJD -> GMT
mjd2posix <- function(MJD)
{
  
  jd = MJD + 2400000.5
  l1 = jd + 68569
  n = (4*l1)%/%146097
  l2 = l1 - (146097*n+3)%/%4
  i  = (4000*(l2+1))%/%1461001
  l3 = l2 - (1461*i)%/%4 + 31
  j = (80*l3)%/%2447
  DAY = round(l3-(2447*j)%/%80,digits=0)
  l4 = j%/%11
  MONTH = j + 2 - (12*l4)
  YEAR = 100*(n-49) + i + l4
  
  date = paste(c(DAY,MONTH,YEAR),collapse = "-")
  date <- as.character(as.Date(date, format="%d-%m-%Y"))
  return(date)
}
################################################################
# E.
colnT <- icecube2[,5]
x1<-c(colnT)
m<-matrix(x1)
s23<-data.table("Time_GMT"=lapply(m,mjd2posix))
Nice <- data.frame(icecube2, s23)
#################################################################
# F. 
#Escribir una función (evt_month) que tome como argumentos la tabla de datos
#y un mes dado (numérico) y retorne el número de eventos en los diversos años
#ocurridos en dicho mes.
#fecha = Nice[,10]
#contador=0
#pollo <- NA
#for(i in fecha)
#{
#  p<-as.POSIXlt(i)
#  t<-p$mon
#}
#cols <- Nice[,10]
#x2<-c(cols)
#n<-matrix(x2)
#s26<-data.table("Num"=lapply(n,conv))
####
#Lq<-subset(s26, Num==0, select=Num)
#print(length(Lq))
colnT <- icecube2[,5]
x1<-c(colnT)
ww<-mapply(mjd2posix,x1)

conv<-function(f)
{
  p=as.POSIXlt(f)
  t=p$mon
  return(t)
}
Meses<-mapply(conv, ww)
##yy<-as.POSIXlt(ww)$mon
Num_Event_Mes<-table(Meses)
ee<-which.max(Num_Event_Mes)
pr<-months(ee)

####################################################################################################
#F.
x =c()
y =c()

for (X in 1:nrow(Nice))
{
  x = c(x,Nice[,6][X]) #Declinacion
  y = c(y,Nice[,7][X]) #Ascension
  
}
#plot(x,y,main="Distribución de los eventos in Equatorial Coordinate", xlab="ascensión", ylab = "declinación")

#uu<-rgl.spheres(y,x,main="distribución de los eventos", xlab="ascensión", ylab = "declinación",color=rainbow(10))
#spheres3d(y,x,main="distribución de los eventos", xlab="ascensión", ylab = "declinación")
#plot.ecdf(y,x,main="distribución de los eventos", xlab="ascensión", ylab = "declinación",color=rainbow(10))
#plot.apos(y,x,main="distribución de los eventos", xlab="ascensión", ylab = "declinación",color=rainbow(10))

########################################################################################
#H.Deposited EM-Equivalent Energy in Detector (TeV) VS Declination (degree)
ggplot(data=Nice, mapping = aes(x=Dep_Energy_TeV, y=Declination_deg))+geom_point()+geom_errorbar(aes(ymin=Declination_deg + Ene_Err_min, ymax=Declination_deg + Ene_Err_max))
####

