fgls1 <- pggls(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil + factor(week),
               data = df4_pan, effect = "time", model = "pooling")
summary(fgls1)
plot(as.vector(fitted.values(fgls1)),as.vector(residuals(fgls1)))

fgls2<- pggls(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil + factor(week),
            sigma.formula = ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil + factor(week),
            nu.formula =  ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil + factor(week),
            family = BCCG,
            data = df4_pan, effect = "time", model = "pooling")
summary(fgls2)

plot(as.vector(fitted.values(fgls2)),as.vector(residuals(fgls2)))

