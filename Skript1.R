
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
























