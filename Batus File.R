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

pool.sqrt.actual.lag <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                            + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                            + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                            + lag(A60.79.Anteil, 1)
                            + factor(week)
                            , data =df4_pan, model = "pooling")

stargazer(pool.sqrt.actual.lag)

a<-coeftest(pool, vcovHC(pool, type = "HC0"))
b<-coeftest(pool.sqrt.actual, vcovHC(pool.sqrt.actual, type = "HC0"))
c<-summary(pool.weighted)
stargazer(b)

sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)

a<-list(sum.sqrt.nullt, 
sum.sqrt.erst,
sum.sqrt.zweit, 
sum.sqrt.dritt,
sum.sqrt.viert,
sum.sqrt.fuenft,
sum.sqrt.sechst,
sum.sqrt.siebt)

b<-list()
for (i in seq_along(a)){
  b[i]<-a[[i]]
}


