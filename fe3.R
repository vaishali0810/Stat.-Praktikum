nullt <- pdata.frame(nullt, index=c("district", "week"))
fe3_0 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil 
           , data =nullt, model = "within")
summary(fe3_0)

erst<-pdata.frame(erst, index=c("district", "week"))
fe3_1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =erst, model = "within")
summary(fe3_1)

zweit<-pdata.frame(zweit, index=c("district", "week"))
fe3_2 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =zweit, model = "within")
summary(fe3_2)

dritt<-pdata.frame(dritt, index=c("district", "week"))
fe3_3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =dritt, model = "within")
summary(fe3_3)

viert<-pdata.frame(viert, index=c("district", "week"))
fe3_4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =viert, model = "within")
summary(fe3_4)

fünft<-pdata.frame(fünft, index=c("district", "week"))
fe3_5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =fünft, model = "within")
summary(fe3_5)

sechst<-pdata.frame(sechst, index=c("district", "week"))
fe3_6<- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil 
             , data =sechst, model = "within")
summary(fe3_6)

siebt<-pdata.frame(siebt, index=c("district", "week"))
fe3_7<- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A35.59.Anteil + A60.79.Anteil 
            , data =siebt, model = "within")
summary(fe3_7)


p<-c(21:30)
zweit_a<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(31:39)
zweit_b<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+39))
sechst_a<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+40):(52+51))
sechst_b<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)


