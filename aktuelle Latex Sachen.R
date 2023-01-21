
pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")
pool.sqrt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                 + sqrt(I(log(density)*lag(inzidenz, 1))) + sqrt(I(hotspot * lag(inzidenz, 1))) 
                 + sqrt(I(hotspotnb * lag(weightednbinz, 1))) + sqrt(I(rate_zweitimpf * hotspot)) 
                 + A60.79.Anteil
                 + factor(week)
                 , data =df4_pan, model = "pooling")
pool.weighted <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data =df4_pan,weights = 1/(sqrt(inzidenz) +1), model = "pooling")