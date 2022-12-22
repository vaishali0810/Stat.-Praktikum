

dfmunich_sk<-dfultimate[dfultimate$district!="SK München",]
dfmunich_lk<-dfultimate[dfultimate$district!="LK München",]
dfmunich_sklk <-dfultimate[dfultimate$district!="SK München" & dfultimate$district!="LK München",]

dfmunich_sk_pan<-pdata.frame(dfmunich_sk, index=c("district", "week"))
dfmunich_lk_pan<-pdata.frame(dfmunich_lk, index=c("district", "week"))
dfmunich_sklk_pan <- pdata.frame(dfmunich_sklk, index=c("district", "week"))
re110 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =dfmunich_sk_pan, model = "random")
summary(re110)

## density without sk munich -0.0060867 ** 


re109 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =df_pan, model = "random")
summary(re109)


##  with sk munich and LK munich -0.0047767 **

re111 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =dfmunich_lk_pan, model = "random")
summary(re111)


## without LK munich  --0.0047597 **



re112<- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =dfmunich_sklk_pan, model = "random")
summary(re112)

### wihtout SK munich and LK munich -0.0060446 * 






##### fixed effects 


fe109 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(density * lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =df_pan, model = "within")
summary(fe109)

## density I(density * lag(inzidenz, 1)) -2.4686e-05 ***


fe110 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(density * lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =dfmunich_sk_pan, model = "within")
summary(fe110)

### -3.4022e-05 ***

fe111 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(density * lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =dfmunich_lk_pan, model = "within")
summary(fe111)

### -2.4653e-05 ***


fe112 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(density * lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =dfmunich_sklk_pan, model = "within")
summary(fe112)

### -3.3911e-05 ***


### density stark durch SK munich beeinflusst


