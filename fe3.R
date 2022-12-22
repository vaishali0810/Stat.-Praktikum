nullt <- pdata.frame(nullt, index=c("district", "week"))
fe3_0 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil 
           , data =nullt, model = "within")
summary(fe3_0)
## R-Squared:      0.27493
## Adj. R-Squared: 0.0046549


erst<-pdata.frame(erst, index=c("district", "week"))
fe3_1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =erst, model = "within")
summary(fe3_1)
## R-Squared:      0.68426
## Adj. R-Squared: 0.641

zweit<-pdata.frame(zweit, index=c("district", "week"))
fe3_2 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =zweit, model = "within")
summary(fe3_2)
## R-Squared:      0.43071
## Adj. R-Squared: 0.39194

dritt<-pdata.frame(dritt, index=c("district", "week"))
fe3_3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =dritt, model = "within")
summary(fe3_3)
## R-Squared:      0.61566
## Adj. R-Squared: 0.5924

viert<-pdata.frame(viert, index=c("district", "week"))
fe3_4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =viert, model = "within")
summary(fe3_4)
## R-Squared:      0.75838
## Adj. R-Squared: 0.7364

fünft<-pdata.frame(fünft, index=c("district", "week"))
fe3_5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =fünft, model = "within")
summary(fe3_5)
## R-Squared:      0.23806
## Adj. R-Squared: 0.026748

sechst<-pdata.frame(sechst, index=c("district", "week"))
fe3_6<- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =sechst, model = "within")
summary(fe3_6)
## R-Squared:      0.89001
## Adj. R-Squared: 0.88335

siebt<-pdata.frame(siebt, index=c("district", "week"))
fe3_7<- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A35.59.Anteil + A60.79.Anteil 
            , data =siebt, model = "within")
summary(fe3_7)
## R-Squared:      0.88011
## Adj. R-Squared: 0.87722
