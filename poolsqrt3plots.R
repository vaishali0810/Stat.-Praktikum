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



## pool.sqrt plot für A60.79.Anteil
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


plot(formula = pool.sqrt$residuals ~ s$A60.79.Anteil, xlab = "A60.79.Anteil", ylab = "Residuen", cex.axis = 1.5, pch=23, cex.lab= 1.5, cex=0.5,
     bg="black", col=alpha("black",0.2))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 3, lty = "longdash")

#
par(mfrow = c(3, 3), cex.lab = 1.5, cex.axis = 2)
p1 <- plot(formula = pool.sqrt$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.sqrt$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.sqrt$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool.sqrt$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p5 <- plot(formula = pool.sqrt$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.sqrt$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
           ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <-  plot(formula=pool.sqrt$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
           ylab="Residuen", cex.axis=1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")





## pool.sqrt Fitted Plot
plot(as.vector(fitted.values(pool.sqrt)), as.vector(residuals(pool.sqrt)), 
     xlab="Fitted values", ylab="Residuals", cex.lab = 1.5, cex.axis = 1.5) + abline(h=0,
                                                                                       col=adjustcolor("black",alpha=0.5),
                                                                                       lwd=3, lty = "longdash")
## pool.sqrt2 Fitted Plot
plot(as.vector(fitted.values(pool.sqrt2)), as.vector(residuals(pool.sqrt2)), 
     xlab="Fitted values", ylab="Residuals", cex.lab = 1.5, cex.axis = 1.5) + abline(h=0,
                                                                                     col=adjustcolor("black",alpha=0.5),
                                                                                     lwd=3, lty = "longdash")
## pool.sqrt3 Fitted Plot
plot(as.vector(fitted.values(pool.sqrt3)), as.vector(residuals(pool.sqrt3)), 
     xlab="Fitted values", ylab="Residuals", cex.lab = 1.5, cex.axis = 1.5) + abline(h=0,
                                                                                     col=adjustcolor("black",alpha=0.5),

                                                                                                                                                                          lwd=3, lty = "longdash")
## pool.sqrt3 Residual-Plots
par(mfrow = c(3, 3), cex.lab = 1.5, cex.axis = 2)
p1 <- plot(formula = pool.sqrt3$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)",
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p2 <- plot(formula = pool.sqrt3$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)",
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p3 <- plot(formula = pool.sqrt3$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", 
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5,col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p4 <- plot(formula = pool.sqrt3$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", 
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p5 <- plot(formula = pool.sqrt3$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", 
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8)) +abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p6 <- plot(formula = pool.sqrt3$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot",
     ylab = "Residuen", cex.axis = 1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")

p7 <- plot(formula=pool.sqrt3$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", 
     ylab="Residuen", cex.axis=1.5,pch=23,cex=0.5, col=alpha("black",0.8))+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2, lty = "longdash")


