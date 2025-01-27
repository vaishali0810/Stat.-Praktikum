df_pan2<-df_pan[-(which(df_pan$week==1)),]
df_pan2<-df_pan2[-(which(df_pan2$week==2)),]

fe6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf + factor(week)
           , data =df_pan2, model = "within")
fd6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
           , data =df_pan2, model = "fd")
fe2200 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
              + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
              + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
              + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
              , data =df4_pan, model = "within")

#residual plots fe6

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)), c(lag(df_pan2$inzidenz,2))
           ,c(lag(df_pan2$weightednbinz, 2)),c(df_pan2$A05.14.Anteil),c(df_pan2$A15.34.Anteil),
           c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),c(df_pan2$A60.79.Anteil),
           c(df_pan2$rate_zweitimpf), c(df_pan2$rate_drittimpf), c(df_pan2$rate_viertimpf))

t<-na.omit(s)

colnames(t)<-c("inzidenz1","weightednbinz1","inzidenz2","weightednbinz2",
                        "A05.14.Anteil","A15.34.Anteil","density_inzidenz1","A60.79.Anteil",
                        "rate_zweitimpf","rate_drittimpf","rate_viertimpf")

plot(formula = fe6$residuals ~ t$inzidenz2, xlab = "Inzidenz", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Inzidenz")

plot(formula = fe6$residuals ~t$weightednbinz2 , xlab = "gewichtete Nachbar-Inzidenzen mit lag = 2", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für gewichtete Nachbar-Inzidenzen")

plot(formula = fe6$residuals ~t$A05.14.Anteil , xlab = "A05.14.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Altersanteile zwischen 05 & 14")

plot(formula = fe6$residuals ~ t$A15.34.Anteil, xlab = "A15.34.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Altersanteile zwischen 15 & 34")

plot(formula = fe6$residuals ~ t$density_inzidenz1, xlab = "Dichte_Inzidenz mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Dichte_Inzidenz1")

plot(formula = fe6$residuals ~ t$A60.79.Anteil, xlab = "A60.79.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Altersanteile zwischen 60 & 79")

plot(formula = fe6$residuals ~ t$rate_zweitimpf, xlab = "Zweitimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Zweitimpfungrate")

plot(formula = fe6$residuals ~ t$rate_drittimpf, xlab = "Drittimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Drittimpfungrate")

plot(formula = fe6$residuals ~ t$rate_viertimpf, xlab = "Viertimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Viertimpfungrate")

### #residual plots fe6

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)), c(lag(df_pan2$inzidenz,2))
              ,c(lag(df_pan2$weightednbinz, 2)),c(df_pan2$A05.14.Anteil),c(df_pan2$A15.34.Anteil),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),c(df_pan2$A60.79.Anteil),
              c(df_pan2$rate_zweitimpf), c(df_pan2$rate_drittimpf), c(df_pan2$rate_viertimpf))

t<-na.omit(s)

colnames(t)<-c("inzidenz1","weightednbinz1","inzidenz2","weightednbinz2",
               "A05.14.Anteil","A15.34.Anteil","density_inzidenz1","A60.79.Anteil",
               "rate_zweitimpf","rate_drittimpf","rate_viertimpf")

plot(formula = fe6$residuals ~ t$inzidenz2, xlab = "Inzidenz", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Inzidenz")

plot(formula = fe6$residuals ~t$weightednbinz2 , xlab = "gewichtete Nachbar-Inzidenzen mit lag = 2", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für gewichtete Nachbar-Inzidenzen")

plot(formula = fe6$residuals ~t$A05.14.Anteil , xlab = "A05.14.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Altersanteile zwischen 05 & 14")

plot(formula = fe6$residuals ~ t$A15.34.Anteil, xlab = "A15.34.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Altersanteile zwischen 15 & 34")

plot(formula = fe6$residuals ~ t$density_inzidenz1, xlab = "Dichte_Inzidenz mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Dichte_Inzidenz1")

plot(formula = fe6$residuals ~ t$A60.79.Anteil, xlab = "A60.79.Anteil", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Altersanteile zwischen 60 & 79")

plot(formula = fe6$residuals ~ t$rate_zweitimpf, xlab = "Zweitimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Zweitimpfungrate")

plot(formula = fe6$residuals ~ t$rate_drittimpf, xlab = "Drittimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Drittimpfungrate")

plot(formula = fe6$residuals ~ t$rate_viertimpf, xlab = "viertimpfungsrate", ylab = "Residuen", cex.axis = 0.8, cex = 0.7) + abline(h = 0, col = "red", lwd = 2)+ title("Residuen Plot für Viertimpfungrate")

