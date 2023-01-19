
df4_pan<-pdata.frame(df4, index=c("district", "week"))

View(df4_pan)


fe6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
           , data =df4_pan, model = "within")
summary(fe6)


fe2201 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
              + hotspotnb + I(rate_viertimpf*hotspot)+ I(F.Anteil*lag(inzidenz,1))
              + I(F.Anteil*lag(inzidenz,2)) + I(A60.79.Anteil*rate_zweitimpf)
              , data =df4_pan, model = "within")
summary(fe2201)

fe2202 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + rate_drittimpf + A60.79.Anteil + rate_zweitimpf
              + hotspotnb + I(rate_viertimpf*hotspot)+ I(F.Anteil*lag(inzidenz,1))
              + I(F.Anteil*lag(inzidenz,2)) + I(A60.79.Anteil*rate_drittimpf)
              , data =df4_pan, model = "within")
summary(fe2202)

fe2202 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + rate_drittimpf + A60.79.Anteil + rate_zweitimpf
              + hotspotnb + I(rate_viertimpf*hotspot)+ I(F.Anteil*lag(inzidenz,1))
              + I(F.Anteil*lag(inzidenz,2)) + I(A60.79.Anteil*rate_drittimpf)
              + factor(week)
              , data =df4_pan, model = "within")
summary(fe2202)

fe2202 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + rate_drittimpf + A60.79.Anteil + rate_zweitimpf + rate_drittimpf
              + hotspotnb + I(rate_viertimpf*hotspot)+ I(F.Anteil*lag(inzidenz,1))
              + I(F.Anteil*lag(inzidenz,2)) + I(A60.79.Anteil*rate_drittimpf)
              + factor(week)
              , data =df4_pan, model = "within")
summary(fe2202)







fe2202 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
              + hotspotnb + I(rate_viertimpf*hotspot)+ I(M.Anteil*lag(inzidenz,1))
              + I(M.Anteil*lag(inzidenz,2))
              , data =df4_pan, model = "within")
summary(fe2202)


fe9 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
           + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + I(hotspot*rate_drittimpf)
           + factor(week)
           , data =df4_pan, model = "within")
summary(fe9)

pFtest(fe.step1,fe.step0)






fe.step13 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")

fe.actual <- fe.step13

df_pan2<-df4_pan[-(which(df4_pan$week==1)),]
df_pan2<-df_pan2[-(which(df_pan2$week==2)),]

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),
              c(I(df_pan2$rate_zweitimpf * df_pan2$hotspot)), 
              c(I(df_pan2$hotspot * lag(df_pan2$inzidenz, 1))) ,
              c(I(df_pan2$hotspotnb * lag(df_pan2$inzidenz, 1))))


re.step3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil 
                + factor(week)
                , data =df4_pan, model = "random")
s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),
              c(I(df_pan2$hotspot*lag(df_pan2$inzidenz,1))),
              c(I(df_pan2$hotspotnb*lag(df_pan2$weightednbinz,1))),
              c(I(df_pan2$rate_zweitimpf * df_pan2$hotspot)),
              c(df_pan2$A60.79.Anteil))


#t<-na.omit(s)

colnames(s)<-c("inzidenz1","weightednbinz1","inzidenz2","weightednbinz2",
               "A05.14.Anteil","A15.34.Anteil","density_inzidenz1","A60.79.Anteil",
               "rate_zweitimpf","rate_drittimpf","rate_viertimpf", "hotspot_inzidenz1", "hotspotnb_inzidenz1")


plot(formula = fe.actual$residuals ~ s$inzidenz2, xlab = "inzidenz", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe.actual$residuals ~ s$hotspot_inzidenz1, xlab = "hotspot_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe.actual$residuals ~ s$density_inzidenz1, xlab = "density_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe.actual$residuals ~ s$hotspotnb_inzidenz1, xlab = "hotspotnb_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe.actual$residuals ~s$weightednbinz2 , xlab = "weightednbinz2", ylab = "Residuen", cex.axis = 0.8)



plot(formula = fe7$residuals ~ s$rate_zweitimpf, xlab = "rate_zweitimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_drittimpf, xlab = "rate_drittimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_viertimpf, xlab = "rate_viertimpf", ylab = "Residuen", cex.axis = 0.8)


pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")



nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool0.step1<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
           + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
           +I(hotspotnb * lag(weightednbinz, 1)) 
           + A60.79.Anteil 
           + factor(week)
           , data =nullt_pan, model = "pooling")

summary(pool0.step1)

pool0.step2<-plm(inzidenz ~ lag(inzidenz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) 
                + A60.79.Anteil 
                + factor(week)
                , data =nullt_pan, model = "pooling")

pFtest(pool0.step1, pool0.step2)

pool0.step3<-plm(inzidenz ~ lag(inzidenz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
                + A60.79.Anteil 
                + factor(week)
                , data =nullt_pan, model = "pooling")

pFtest(pool0.step2,pool0.step3)

#pool0.step4<-plm(inzidenz ~ lag(inzidenz, 1) 
 #               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
  #              + factor(week)
   #             , data =nullt_pan, model = "pooling")

#pFtest(pool0.step3,pool0.step4)

pool0.step5<-plm(inzidenz ~ lag(inzidenz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                + A60.79.Anteil + A15.34.Anteil
                + factor(week)
                , data =nullt_pan, model = "pooling")

#pool0.step52<-plm(inzidenz ~ lag(inzidenz, 1) 
 #               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
 #               + A60.79.Anteil + A15.34.Anteil
 #               + factor(week)
 #               , data =nullt, model = "pooling",index=c("district","week"))


summary(pool0.step5)

pFtest(pool0.step5,pool0.step3)




erst_pan<-pdata.frame(erst,index=c("district","week"))

pool1.step1<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =erst_pan, model = "pooling")

summary(pool1.step1)



zweit_pan<-pdata.frame(zweit,index=c("district","week"))

pool2.step1<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =zweit_pan, model = "pooling")

summary(pool2.step1)

dritt_pan<-pdata.frame(dritt,index=c("district","week"))

pool3.step1<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =dritt_pan, model = "pooling")
summary(pool3.step1)

plot(formula = pool$residuals ~ s$inzidenz1, xlab = "inzidenz", ylab = "Residuen", 
     cex.axis = 0.8)+abline(h = 0, col = "black", lwd = 2,alpha=0.1)


ggplot(data=pool)+
  geom_point(mapping=aes(x=))


plot(formula = pool$residuals ~ s$inzidenz1, xlab = "inzidenz", ylab = "Residuen", 
     cex.axis = 0.8,pch=16,cex=0.5, col=alpha("black",0.3))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)


pool_bccg<-plm(formula=inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil 
               + factor(week),
               sigma.formula= ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil 
               + factor(week),
               nu.formula= ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil 
               + factor(week),
               family="BCCG",
               data =df4_pan, model = "pooling")

library(MASS)

pool_bccg<-boxcox(df4_pan$inzidenz ~ lag(df4$inzidenz, 1) + lag(df4$weightednbinz, 1) 
                  + I(log(df4_pan$density)*lag(df4_pan$inzidenz, 1)) + I(df4_pan$hotspot * lag(df4_pan$inzidenz, 1)) 
                  +I(df4_pan$hotspotnb * lag(df4_pan$weightednbinz, 1)) + I(df4_pan$rate_zweitimpf * df4_pan$hotspot) 
                  + df4_pan$A60.79.Anteil 
                  + factor(df4_pan$week))
library(geoR)

pool_bccg<-boxcoxfit(df4_pan$inzidenz ~ lag(df4$inzidenz, 1) + lag(df4$weightednbinz, 1) 
                     + I(log(df4_pan$density)*lag(df4_pan$inzidenz, 1)) + I(df4_pan$hotspot * lag(df4_pan$inzidenz, 1)) 
                     +I(df4_pan$hotspotnb * lag(df4_pan$weightednbinz, 1)) + I(df4_pan$rate_zweitimpf * df4_pan$hotspot) 
                     + df4_pan$A60.79.Anteil 
                     + factor(df4_pan$week))


df_pan2<-df4_pan

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),
              c(I(df_pan2$hotspot*lag(df_pan2$inzidenz,1))),
              c(I(df_pan2$hotspotnb*lag(df_pan2$weightednbinz,1))),
              c(I(df_pan2$rate_zweitimpf * df_pan2$hotspot)),
              c(df_pan2$A60.79.Anteil))


colnames(s)<-c("inzidenz1","weightednbinz1","density_inzidenz1",
               "hotspot_inzidenz1", "hotspotnb_wnbinzidenz1",
               "zweitimpf_hotspot","A60.79.Anteil","inzidenz")

s<-na.omit(s)

df4_2<-df4[-which(df4$week==1),]

s<-cbind(s,df4_2$inzidenz)

library(mgcv)

library(MASS)

library(gamlss)

library(gamlss.util)

pool_bccg<-gamlss(formula=inzidenz ~ inzidenz1 + weightednbinz1 
               + density_inzidenz1 + hotspot_inzidenz1 
               +hotspotnb_wnbinzidenz1 + zweitimpf_hotspot
               + A60.79.Anteil,
               sigma.formula= ~ inzidenz1 + weightednbinz1 
               + density_inzidenz1 + hotspot_inzidenz1 
               +hotspotnb_wnbinzidenz1 + zweitimpf_hotspot
               + A60.79.Anteil,
               nu.formula= ~ inzidenz1 + weightednbinz1 
               + density_inzidenz1 + hotspot_inzidenz1 
               +hotspotnb_wnbinzidenz1 + zweitimpf_hotspot
               + A60.79.Anteil,
               family=BCCG,data=s)


pool.sqrt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                 + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
                 + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
                 + A60.79.Anteil
                 + factor(week)
                 , data =df4_pan, model = "pooling")
plot(as.vector(fitted.values(pool.sqrt)), as.vector(residuals(pool.sqrt)))

plot(formula = pool$residuals ~ s$inzidenz1, xlab = "inzidenz",
     ylab = "Residuen", cex.axis = 0.8,pch=16,cex=0.5, 
     col=alpha("black",0.3))+abline(h = 0, col = adjustcolor("black",alpha=0.5),
                                    lwd = 2)
plot(formula=as.vector(residuals(pool.sqrt)) ~ as.vector(fitted.values(pool.sqrt),),
     xlab="Fitted values",ylab="Residuals",
     cex.axis=0.8,pch=16,cex=0.3,col=alpha("black",0.3))+abline(h=0,
                                                                col=adjustcolor("black",alpha=0.5),
                                                                lwd=2)










