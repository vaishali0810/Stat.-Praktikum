library(tidyverse) 
library(plm)       # wichtige package
library(car)       
library(tseries)   
library(lmtest)   




rm(list=ls())
dfultimate <- read.csv("dfultimate.csv", header = TRUE, sep = ",")
df <- dfultimate %>% select(district, week, bezirk, inzidenz, density, m_anteil, f_anteil, M.A00.04.Anteil, M.A05.14.Anteil,
                            M.A15.34.Anteil, M.A35.59.Anteil, M.A60.79.Anteil, M.A80.Anteil, M.Aunb.Anteil, F.A00.04.Anteil,
                            F.A05.14.Anteil, F.A15.34.Anteil, F.A35.59.Anteil, F.A60.79.Anteil, F.A80.Anteil, F.Aunb.Anteil,
                            A00.04.Anteil, A05.14.Anteil, A15.34.Anteil, A35.59.Anteil, A60.79.Anteil, A80.Anteil,
                            M.Anteil, F.Anteil, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf,
                            weightednbinz, unweightednbinz)


df_pan <- pdata.frame(df, index=c("district", "week"))
#dfultimate_pan <- pdata.frame(dfultimate, index=c("district", "week"))
#### https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2022/Ausgaben/10_22.pdf?__blob=publicationFile
## sum(Inzidenz * population) / gesamt population




## erste 'ernsthafte' Modelle Die anderen 

re12 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil
            + A35.59.Anteil + A60.79.Anteil + A80.Anteil,
            - 1, data =df_pan, model = "random")
summary(re12)

### lag(weightednbinz, 1)   0.408571 ***

re13 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil,
            - 1, data =df_pan, model = "random")
summary(re13)

### lag(weightednbinz, 1)   0.408930 ***

re14 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(unweightednbinz, 1) + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil
            + A35.59.Anteil + A60.79.Anteil + A80.Anteil,
            - 1, data =df_pan, model = "random")

summary(re14)

### lag(unweightednbinz, 1)   0.405013 ***

re15 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(unweightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil,
            - 1, data =df_pan, model = "random")
summary(re15)

### lag(unweightednbinz, 1)   0.405377 ***

#nur sehr geringer Unterschied zwischen weighted und nicht weighted inzidenz der Nachbarscharften







#### density transformieren Yes / NO ### bis Zeile 172, log(density) hat jedoch ziemlich gut funktioniert,
#### also kann auch easy geskippt werden.


######## Modelle ohne München

dfmunich_sk<-dfultimate[dfultimate$district!="SK München",]
dfmunich_lk<-dfultimate[dfultimate$district!="LK München",]
dfmunich_sklk <-dfultimate[dfultimate$district!="SK München" & dfultimate$district!="LK München",]

dfmunich_sk_pan<-pdata.frame(dfmunich_sk, index=c("district", "week"))
dfmunich_lk_pan<-pdata.frame(dfmunich_lk, index=c("district", "week"))
dfmunich_sklk_pan <- pdata.frame(dfmunich_sklk, index=c("district", "week"))


re109 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =df_pan, model = "random")
summary(re109)

##  with sk munich and LK munich -0.0047767 **

re110 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =dfmunich_sk_pan, model = "random")
summary(re110)

## density without sk munich -0.0060867 ** 



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


##### with fixed effects 


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









#### Hausmann test
fe1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =df_pan, model = "within")
summary(fe1)


re100 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
               A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(log(density) * lag(inzidenz, 1))
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
             - 1, data =df_pan, model = "random")
summary(re100)

phtest(re100, fe1)
#### p-value < 0.05 ---> fixed effects

### Vielleicht is das aber auch ein kompletter schmarn, weil durch den lag die Fehler korreliert sind und dadurch der
### Test immer abweist?


### Funktioniert nicht! Wir schauen gerade noch nach einer lösung

# stepAIC(object = re100, direction = "both", k = log(nrow(df_pan)), trace = FALSE)



### entnahme von nicht signifikanten Variablen ohne stepAIC
### zunächst ohne Impfungen

### "Model to beat"
fe5 <- plm(inzidenz ~ lag(inzidenz, 1), data =df_pan, model = "within")
summary(fe5)



fe2 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + I(density*lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil 
           , data =df_pan, model = "within")
summary(fe2)

fe3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil 
           , data =df_pan, model = "within")
summary(fe3)



### Kombination von A35.59 & A60.79?
fe4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + I(A35.59.Anteil + A60.79.Anteil)
           , data =df_pan, model = "within")
summary(fe4)

### vvmtl eher nicht --->  A35.59 raus


#### mit Impfungen 

fe6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
           , data =df_pan, model = "within")
summary(fe6)

fe7 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + lag(rate_zweitimpf,2) + lag(rate_drittimpf,2) + lag(rate_viertimpf,2)
           , data =df_pan, model = "within")
summary(fe7)


#### Wir vermuten dass Impfungen und Inzidenz beide mit einer drittvariablen korreliert ist, die wir nicht im Modell haben
#### Alternative captured das Modell hier Verhaltensänderungen von geimpften evtl.
#### oder die Querdenker hatten doch recht 



#### AIC funktioniert auch nicht mit 
AIC(fe6)


### Verschiedene Phasen von Corona separat schätzen

## data splits


dfultimate <- dfultimate %>% 
  mutate(Kalendarwoche=dfultimate$week+3)

p<-c(1:9)
nullt<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(10:20)
erst<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(21:39)
zweit<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(40:(52+8))
dritt<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+9):(52+23))
viert<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+24):(52+30))
fünft<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+51))
sechst<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+52):(52+151))
siebt<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(21:30)
zweit_a<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c(31:39)
zweit_b<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+39))
sechst_a<-subset(df,dfultimate$Kalendarwoche%in%p)

p<-c((52+40):(52+51))
sechst_b<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)



#### fe3 auf die verschiedenen Phasen

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



###### Covid Phases Models - fe6 als Grundlage

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
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
          , data =fünft, model = "within")
summary(fe65)

## Adj. R-Squared: 0.011509



sechst<-pdata.frame(sechst, index=c("district", "week"))

fe66<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
          + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf
          , data =sechst, model = "within")
summary(fe66)

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
          + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
          , data =siebt, model = "within")
summary(fe67)

## Adj. R-Squared: 0.8769





