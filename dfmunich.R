

dfmunich_sk<-dfultimate[dfultimate$district!="SK München",]
dfmunich_lk<-dfultimate[dfultimate$district!="LK München",]

dfmunich_sk_pan<-pdata.frame(dfmunich_sk, index=c("district", "week"))
dfmunich_lk_pan<-pdata.frame(dfmunich_lk, index=c("district", "week"))

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


##  with munich -0.0047767 **


