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
pool.weighted <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data =df4_pan,weights = 1/(sqrt(inzidenz + 1)), model = "pooling")
library(stargazer)
stargazer(pool,pool.sqrt.actual,pool.weighted)

a<-coeftest(pool, vcovHC(pool, type = "HC0"))
b<-coeftest(pool.sqrt.actual, vcovHC(pool.sqrt.actual, type = "HC0"))
c<-summary(pool.weighted)
stargazer(c$coefficients)

stargazer(p)
