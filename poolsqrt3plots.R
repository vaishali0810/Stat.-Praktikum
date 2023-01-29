pool.sqrt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                 + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
                 + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
                 + A60.79.Anteil
                 + factor(week)
                 , data =df4_pan, model = "pooling")

pool.sqrt3 <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                  + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
                  + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
                  + sqrt(A60.79.Anteil)
                  + factor(week)
                  , data =df4_pan, model = "pooling")

pool.weighted <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data =df4_pan,weights = 1/(sqrt(inzidenz + 1)), model = "pooling")

pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")

pool.sqrt.actual <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                        + A60.79.Anteil
                        + factor(week)
                        , data =df4_pan, model = "pooling")

pool.sqrt.actual.lag <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                            + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                            + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                            + lag(A60.79.Anteil, 1)
                            + factor(week)
                            , data =df4_pan, model = "pooling")
coeftest(pool.sqrt.actual.lag, vcovHC(pool.sqrt.actual.lag, type = "HC0"))


##
df_pan2<-df4_pan[-(which(df4_pan$week==1)),]

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),
              c(I(df_pan2$hotspot*lag(df_pan2$inzidenz,1))),
              c(I(df_pan2$hotspotnb*lag(df_pan2$weightednbinz,1))),
              c(I(df_pan2$rate_zweitimpf * df_pan2$hotspot)),
              c(df_pan2$A60.79.Anteil))

colnames(s)<-c("inzidenz1","weightednbinz1","density_inzidenz1",
               "hotspot_inzidenz1", "hotspotnb_wnbinzidenz1",
               "zweitimpf_hotspot","A60.79.Anteil")

# pool.sqrt.actual.lag Plots

# pool.sqrt.actual.lag Fittes Value
par(mar=c(5,6,4,1)+.1, cex.lab = 2.2)
plot(as.vector(fitted.values(pool.sqrt.actual.lag)), as.vector(residuals(pool.sqrt.actual.lag)),
     xlab="Fitted values", ylab="Residuen", cex.lab = 2.2, cex.axis = 2) 
+ abline(h=0, col=adjustcolor("black",alpha=0.5), lwd=3, lty = "longdash")
+ title("Fitted Values vom Wurzel-Modell", cex.main = 2.4)

# Residuen
par(mar=c(5,6,4,1)+.1, mfrow = c(2, 2), cex.lab = 2.2) 
 p1 <- plot(formula = pool.sqrt.actual.lag$residuals ~ s$inzidenz1, xlab = "lag(Inz, 1)",
           ylab = "Residuen", cex.axis = 2,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.sqrt.actual.lag$residuals ~s$weightednbinz1 , xlab = "lag(NB.Inz, 1)",
           ylab = "Residuen", cex.axis = 2,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.sqrt.actual.lag$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inz, 1) ", 
           ylab = "Residuen", cex.axis = 2,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots vom Wurzel-Modell", side = 3, line = -2, outer = TRUE, font = 2, cex = 2.4)

par(mfrow = c(2, 2), cex.lab = 2.2)
p4 <- plot(formula = pool.sqrt.actual.lag$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inz, 1)", 
           ylab = "Residuen", cex.axis = 2,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p5 <- plot(formula = pool.sqrt.actual.lag$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "NB.Hotspot * lag(NB.Inz, 1)", 
           ylab = "Residuen", cex.axis = 2,pch=23, cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.sqrt.actual.lag$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfrate * Hotspot",
           ylab = "Residuen", cex.axis = 2,pch=23, cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <-  plot(formula=pool.sqrt.actual.lag$residuals~s$A60.79.Anteil, xlab="Anteil.A60.79", 
           ylab="Residuen", cex.axis = 2,pch=23, cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots vom Wurzel-Modell", side = 3, line = -2, outer = TRUE, font = 2, cex = 2.4)








###### pool.sqrts fitted Plots

## pool.sqrt Fitted Plot
plot(as.vector(fitted.values(pool.sqrt.actual)), as.vector(residuals(pool.sqrt)), 
     xlab="Fitted values", ylab="Residuals", cex.lab = 2.2, cex.axis = 2) + abline(h=0,
                                                                                       col=adjustcolor("black",alpha=0.5),
                                                                                       lwd=3, lty = "longdash")
## pool.sqrt2 Fitted Plot
plot(as.vector(fitted.values(pool.sqrt2)), as.vector(residuals(pool.sqrt2)), 
     xlab="Fitted values", ylab="Residuals", cex.lab = 1.5, cex.axis = 1.5) + abline(h=0,
                                                                                     col=adjustcolor("black",alpha=0.5),
                                                                                     lwd=3, lty = "longdash")
## pool.sqrt3 Fitted Plot
plot(as.vector(fitted.values(pool.sqrt3)), as.vector(residuals(pool.sqrt3)), 
     xlab="Fitted values", ylab="Residuen", cex.lab = 1.5, cex.axis = 1.3, pch=23, cex=0.5) + 
        abline(h=0,
               col=adjustcolor("black",alpha=0.5),
               lwd=3, lty = "longdash") +
        title("Fitted Values von pool.sqrt3", cex.main = 1.8)
        
 
                                                                                     
## pool.sqrt3 Residual-Plots
par(mfrow = c(2, 2), cex.lab = 1.3, cex.axis = 2)
p1 <- plot(formula = pool.sqrt3$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.sqrt3$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.sqrt3$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool.sqrt3$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.sqrt3", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)

par(mfrow = c(2, 2), cex.lab = 1.2, cex.axis = 2)
p5 <- plot(formula = pool.sqrt3$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.sqrt3$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
     ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <- plot(formula=pool.sqrt3$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
     ylab="Residuen", cex.axis=1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.sqrt3", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)


# pool.weighted

# Fitted Value
plot(as.vector(fitted.values(pool.weighted)), as.vector(residuals(pool.weighted)),
     xlab="Fitted values", ylab="Residuals", cex.lab = 1.3, cex.axis = 1.3) 
+ abline(h=0, col=adjustcolor("black",alpha=0.5), lwd=3, lty = "longdash")
+ title("Fitted Values von pool.weighted", cex.main = 1.8)

## Residuen 
par(mfrow = c(2, 2), cex.lab = 1.3)
p1 <- plot(formula = pool.weighted$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.weighted$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.weighted$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool.weighted$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.weighted", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)

par(mfrow = c(2, 2), cex.lab = 1.2)
p5 <- plot(formula = pool.weighted$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.weighted$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <- plot(formula=pool.weighted$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
           ylab="Residuen", cex.axis=1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.weighted", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)


# pool 

# Fitted Value
par(mar=c(5,6,4,1)+.1, cex.lab = 2.2)
plot(as.vector(fitted.values(pool)), as.vector(residuals(pool)),
     xlab="Fitted values", ylab="Residuen", cex.lab = 2.2, cex.axis = 2) 
+ abline(h=0, col=adjustcolor("black",alpha=0.5), lwd=3, lty = "longdash")
+ title("Fitted Values vom Pooling-Modell", cex.main = 2.4)

# Residuen
par(mfrow = c(2, 2), cex.lab = 1.3, cex.axis = 2)
p1 <- plot(formula = pool$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)


par(mfrow = c(2, 2), cex.lab = 1.2, cex.axis = 2)
p5 <- plot(formula = pool$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <- plot(formula=pool$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
           ylab="Residuen", cex.axis=1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)


## POOl.log
pool.log <- plm(log(inzidenz + 1) ~ lag(log(inzidenz +1), 1) + lag(log(weightednbinz +1), 1) 
                + log(I(log(density)*lag(inzidenz, 1)) + 1) + I(hotspot * lag(log(inzidenz+1), 1)) 
                + I(hotspotnb * lag(log(weightednbinz+1), 1)) 
                + factor(week)
                , data =df4_pan, model = "pooling")

par(mar=c(5,6,4,1)+.1, cex.lab = 2.2)
plot(as.vector(fitted.values(pool.log)), as.vector(residuals(pool.log)),
     xlab="Fitted values", ylab="Residuen", cex.lab = 2.2, cex.axis = 2) 
+ abline(h=0, col=adjustcolor("black",alpha=0.5), lwd=3, lty = "longdash")
+ title("Fitted Values von Log-Modell", cex.main = 2.4)


     
# Residuen
par(mfrow = c(2, 2), cex.lab = 1.3, cex.axis = 2)
p1 <- plot(formula = pool.log$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.log$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.log$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool.log$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.log", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)


par(mfrow = c(2, 2), cex.lab = 1.2, cex.axis = 2)
p5 <- plot(formula = pool.log$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.log$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
           ylab = "Residuen", cex.axis = 1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <- plot(formula=pool.log$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
           ylab="Residuen", cex.axis=1.3,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

mtext("Residuen Plots von pool.log", side = 3, line = -2, outer = TRUE, font = 2, cex = 2)

