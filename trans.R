pool.sqrt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                 + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
                 + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
                 + A60.79.Anteil
                 + factor(week)
                 , data =df4_pan, model = "pooling")
plot(as.vector(fitted.values(pool.sqrt)), as.vector(residuals(pool.sqrt)))

summary(pool.sqrt)

pcdtest(pool.sqrt, test = c("lm"))
pcdtest(pool.sqrt, test = c("cd"))
pbgtest(pool.sqrt)

bptest(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
       + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
       + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
       + A60.79.Anteil
       + factor(week)
       + factor(district)
       , data =df4_pan,  studentize=F)

coeftest(pool.sqrt, vcovHC(pool.sqrt, type = "HC0"))

library(rcompanion)
pool.Tukey <- plm(transformTukey(inzidenz, plotit=FALSE) ~ transformTukey(lag(inzidenz, 1), plotit=FALSE) + transformTukey(lag(weightednbinz, 1), plotit=FALSE)
                 + transformTukey(I(log(density)*lag(inzidenz, 1)), plotit=FALSE) + transformTukey(I(hotspot * lag(inzidenz, 1)), plotit=FALSE) 
                 + transformTukey(I(hotspotnb * lag(weightednbinz, 1)), plotit=FALSE) + transformTukey(I(rate_zweitimpf * hotspot), plotit=FALSE) 
                 + A60.79.Anteil
                 + factor(week)
                 ,data = df4_pan, model = "pooling")

a<-(sign(lag(inzidenz, 1)) * (abs(lag(inzidenz, 1))^(1/3)))
pool.T_cub <- plm((sign(inzidenz) * abs(inzidenz)^(1/3)) ~ (sign(lag(inzidenz, 1)) * (abs(lag(inzidenz, 1))^(1/3))) 
                 +(sign(lag(weightednbinz, 1)) * (abs(lag(weightednbinz, 1))^(1/3)))
                 +(sign(I(log(density)*lag(inzidenz, 1))) * (abs(I(log(density)*lag(inzidenz, 1)))^(1/3)))
                 +(sign(I(hotspot * lag(inzidenz, 1))) * (abs(I(hotspot * lag(inzidenz, 1)))^(1/3))) 
                 +(sign(I(hotspotnb * lag(weightednbinz, 1))) * (abs(I(hotspotnb * lag(weightednbinz, 1)))^(1/3)))
                 +(sign(I(rate_zweitimpf * hotspot)) * (abs(I(rate_zweitimpf * hotspot))^(1/3))) 
                 + A60.79.Anteil
                 + factor(week)
                 , data =df4_pan, model = "pooling")

T_cub
(sign(I(rate_zweitimpf * hotspot)) * abs(I(rate_zweitimpf * hotspot))^(1/3))
