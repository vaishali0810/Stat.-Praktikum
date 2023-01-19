plot(formula = pool$residuals ~ s$inzidenz1, xlab = "lag(Inzidenz, 1)", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+ abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula = pool$residuals ~s$weightednbinz1 , xlab = "lag(gewichtete Nachbar-Inzidenzen, 1)", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula = pool$residuals ~ s$density_inzidenz1, xlab = "log(Dichte) * lag(Inzidenz, 1) ", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5,col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula = pool$residuals ~ s$hotspot_inzidenz1, xlab = "Hotspot * lag(Inzidenz, 1)", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula = pool$residuals ~ s$hotspotnb_wnbinzidenz1, xlab = "Nachbar-Hotspot * lag(gewichtete Nachbar-Inzidenzen, 1)", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula = pool$residuals ~ s$zweitimpf_hotspot, xlab = "Zweitimpfungrate * Hotspot", ylab = "Residuen", cex.axis = 0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

plot(formula=pool$residuals~s$A60.79.Anteil, xlab="A60.79.Anteil", ylab="Residuen", cex.axis=0.8,pch=21,cex=0.5, col=alpha("black",0.3))
+abline(h = 0, col = adjustcolor("black",alpha=0.5), lwd = 2)

