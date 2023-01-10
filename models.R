# install.packages("lme4")
# library(lme4)
# 
library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
#library(ggplots2)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
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


dfultimate_pan <- pdata.frame(dfultimate, index=c("district", "week"))


re2 <- plm(inzidenz ~ lag(inzidenz, 1) + bezirk + density +  rate_zweitimpf + 
             rate_drittimpf - 1, data = dfultimate_pan, model = "random")
summary(re2)
#plot(residuals(re2))

re3 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil + A00.04.Anteil + A05.14.Anteil + A15.34.Anteil + A35.59.Anteil + A60.79.Anteil 
           + A80.Anteil - 1, data = dfultimate_pan, model = "random")

summary(re3)
#plot(residuals(re3))


#### https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2022/Ausgaben/10_22.pdf?__blob=publicationFile




## sum(Inzidenz * population) / gesamt population

re4 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil  + A00.04.Anteil + A05.14.Anteil + A15.34.Anteil + A35.59.Anteil + A60.79.Anteil 
           + A80.Anteil - 1, data = dfultimate_pan, model = "random")
summary(re4)
#plot(residuals(re4))

re5 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil*m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re5)
#plot(residuals(re5))

re6 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil*m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re6)
#plot(residuals(re6))

re7 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil + m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re7)

## gender spielt nur mit extremen phacking eine Rolle ---> raus

####

#plot(residuals(re3))







re10 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + rate_zweitimpf + rate_drittimpf
            - 1, data =df_pan, model = "random")

summary(re10)

re11 <-plm(inzidenz ~ lag(inzidenz, 1) 
           - 1, data =df_pan, model = "random")

summary(re11)

re12 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil
            + A35.59.Anteil + A60.79.Anteil + A80.Anteil,
            - 1, data =df_pan, model = "random")

summary(re12)

re13 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil,
            - 1, data =df_pan, model = "random")



summary(re13)


re14 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(unweightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil,
            - 1, data =df_pan, model = "random")
summary(re14)

#..
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

nullt <- pdata.frame(nullt, index=c("district", "week"))

re15 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil,
            - 1, data =nullt, model = "random")



summary(re15)

sechst <- pdata.frame(sechst, index=c("district", "week"))
re16 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil + rate_zweitimpf,
            - 1, data =sechst, model = "random")
summary(re16)



fünft <- pdata.frame(fünft, index=c("district", "week"))
re17 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil + rate_zweitimpf,
            - 1, data =fünft, model = "random")
summary(re17)




siebt<-pdata.frame(siebt, index=c("district", "week"))

re18 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil + rate_zweitimpf,
            - 1, data =siebt, model = "random")
summary(re18)

re19 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(unweightednbinz, 1)  + A05.14.Anteil+ A15.34.Anteil
            + A60.79.Anteil + rate_zweitimpf,
            - 1, data =siebt, model = "random")
summary(re19)





fe1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + I(density*lag(inzidenz, 1))
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil
           - 1, data =df_pan, model = "within")
summary(fe1)



re100 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil + density
            + A35.59.Anteil + A60.79.Anteil + A80.Anteil
            - 1, data =df_pan, model = "random")
summary(re100)


stepAIC(object = re100, direction = "both",
        k = log(nrow(df_pan)), trace = FALSE)


######## Modelle ohne München

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

df4_pan <- pdata.frame(df4,index=c("district", "week"))

fe2200 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
           , data =df4_pan, model = "within")
summary(fe2200)
plot(residuals(fe2200))
