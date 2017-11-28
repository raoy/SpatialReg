library(rgdal)
library(raster)

jabar<-readOGR(dsn="D:\\Dept.STK\\Statistika Spasial (S2)\\Praktikum (RA)\\Data dari Bu Anik\\DATA JABAR\\Peta Jabar", layer="jawa_barat")
jabar1 <- jabar[jabar$PROVINSI=="JAWA BARAT", ]
plot(jabar1)
text(jabar1,'KABKOT',cex=0.5)

jabar2 <- jabar1[jabar1$KABKOT!="WADUK CIRATA", ]
plot(jabar2)
text(jabar2,'KABKOT',cex=0.5)


#writeOGR(jabar1, dsn = "D:/Dept.STK/Statistika Spasial (S2)/TA 2017-2018/Praktkum RA/Reference/petaJabar" , layer = "Jabar",driver="ESRI Shapefile")

#writeOGR(jabar2, dsn = "D:/Dept.STK/Statistika Spasial (S2)/TA 2017-2018/Praktkum RA/Reference/petaJabar2" , layer = "Jabar2",driver="ESRI Shapefile")

library(spdep)
data.jabar<-read.delim("clipboard") #copy data dari file  Jabar Data (gabung).xlsx
w<-poly2listw(jabar2)
ww<-nb2listw(w)
moran(jabar2$p.miskin16, ww, n=length(ww$neighbours), S0=Szero(ww))

moran(data.jabar$p.miskin16, ww, n=length(ww$neighbours), S0=Szero(ww))
moran.test(data.jabar$p.miskin16, ww,randomisation=T, alternative="greater")
moran.plot(data.jabar$p.miskin16, ww, labels=data.jabar$KABKOT)

#eksplorasi data
library(arules)
k=4
p.miskin.baru<-discretize(data.jabar$p.miskin16,method="interval",categories=k)
jabar2$miskin<-p.miskin.baru
palette(gray(seq(0.5,0.8,len = k)))
plot(jabar2, ,col=1:k)
legend("topright", levels(jabar2$miskin), pch=15,col=1:k)

#regresi klasik
reg.klasik<-lm(p.miskin16~EYS2016,data.jabar)
err.regklasik<-residuals(reg.klasik)

#uji asumsi regresi klasik
library(nortest)
library(car)
library(DescTools)
library(lmtest)
ad.test(err.regklasik) #menggunakan package "nortest"
hist(err.regklasik)
qqnorm(err.regklasik,datax=T)
qqline(rnorm(length(err.regklasik),mean(err.regklasik),sd(err.regklasik)),datax=T, col="red")
durbinWatsonTest(err.regklasik)
RunsTest(err.regklasik)
bptest(reg.klasik)

moran(err.regklasik, ww, n=length(ww$neighbours), S0=Szero(ww))
moran.test(err.regklasik, ww,randomisation=T, alternative="greater")

#LMtest
LM<-lm.LMtests(reg.klasik, nb2listw(w, style="W"),test=c("LMerr", "LMlag","RLMerr","RLMlag","SARMA"))

#pemodelan SAR
sar<-lagsarlm(p.miskin16~EYS2016,data=data.jabar,nb2listw(w))
err.sar<-residuals(sar)
ad.test(err.sar) #uji kenormalan,hrs menggunakan package "nortest"
bptest.sarlm(sar)    #uji kehomogenan residual
RunsTest(err.sar)

#pemodelan SEM
sem<-errorsarlm(p.miskin16~EYS2016,data=data.jabar,nb2listw(w))
err.sem<-residuals(sem)
ad.test(err.sem) #uji kenormalan,hrs menggunakan package "nortest"
bptest.sarlm(sem)    #uji kehomogenan residual
RunsTest(err.sem)

#pemodelan SEM
gsm<-sacsarlm(p.miskin16~EYS2016,data=data.jabar,nb2listw(w))
err.gsm<-residuals(gsm)
ad.test(err.gsm) #uji kenormalan,hrs menggunakan package "nortest"
bptest.sarlm(gsm)    #uji kehomogenan residual
RunsTest(err.gsm)
