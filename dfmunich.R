

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




fe109 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
               A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(log(density) * lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
             , data =df_pan, model = "within")
summary(fe109)

## density I(log(density) * lag(inzidenz, 1)) -0.0184581 ***


fe110 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
               A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(log(density) * lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
             , data =dfmunich_sk_pan, model = "within")
summary(fe110)


### -0.0188575 ***


## einfach logarithmieren?





#### Models for Covid Waves 

nullt <- pdata.frame(nullt, index=c("district", "week"))

fe60<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
          , data =nullt, model = "within")
summary(fe60)

## Adj. R-Squared: -0.1109


erst <- pdata.frame(erst, index=c("district", "week"))

fe61<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
          , data =erst, model = "within")
summary(fe61)

## Adj. R-Squared: 0.64116


zweit<-pdata.frame(zweit, index=c("district", "week"))

fe62<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
          , data =zweit, model = "within")
summary(fe62)

## Adj. R-Squared: 0.38223


zweit_a<-pdata.frame(zweit_a, index=c("district", "week"))

fe62_a<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
            , data =zweit_a, model = "within")
summary(fe62_a)

## Adj. R-Squared: 0.019677


zweit_b<-pdata.frame(zweit_b, index=c("district", "week"))

fe62_b<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
            , data =zweit_b, model = "within")
summary(fe62_b)

## Adj. R-Squared: -0.069239



dritt<-pdata.frame(dritt, index=c("district", "week"))

fe63<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
          , data =dritt, model = "within")
summary(fe63)

## Adj. R-Squared: 0.60162



viert<-pdata.frame(viert, index=c("district", "week"))

fe64<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
          , data =viert, model = "within")
summary(fe64)

## Adj. R-Squared: 0.74854


fünft<-pdata.frame(fünft, index=c("district", "week"))

fe65<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil +I(log(density)*lag(inzidenz, 1))
           + lag(rate_zweitimpf,2) + lag(rate_drittimpf,2)+ A35.59.Anteil
          + A60.79.Anteil + weightednbinz +inzidenz 
          , data =fünft, model = "within")
summary(fe65)



## Adj. R-Squared: 0.011509



sechst<-pdata.frame(sechst, index=c("district", "week"))

fe66<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A35.59.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + lag(rate_zweitimpf,2) + lag(rate_drittimpf,2)
          + rate_zweitimpf + rate_drittimpf +lag(rate_zweitimpf,1) + lag(rate_drittimpf,1)
          , data =sechst, model = "within")
summary(fe66)

plot(residuals(fe66))

## Adj. R-Squared: 0.88393



sechst_a<-pdata.frame(sechst_a, index=c("district", "week"))

fe66_a<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A60.79.Anteil + rate_zweitimpf + rate_drittimpf
            , data =sechst_a, model = "within")
summary(fe66_a)

## Adj. R-Squared: 0.34228




sechst_b<-pdata.frame(sechst_b, index=c("district", "week"))

fe66_b<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A60.79.Anteil + rate_zweitimpf + rate_drittimpf 
            , data =sechst_b, model = "within")
summary(fe66_b)

## Adj. R-Squared: 0.76692



siebt<-pdata.frame(siebt,index=c("district","week"))

fe67<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
                  + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
                  + A60.79.Anteil + lag(rate_zweitimpf,2) + lag(rate_drittimpf,2)
          + lag(rate_viertimpf,2)
                  , data =siebt, model = "within")
summary(fe67)

## Adj. R-Squared: 0.8769

plot(residuals(fe67))

