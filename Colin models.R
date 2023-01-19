##models.R zuerst einlesen
fe1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
           A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =df_pan, model = "within")
summary(fe1)

# lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +

re100 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
             A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
             - 1, data =df_pan, model = "random")
summary(re100)


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


fe4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + I(A35.59.Anteil + A60.79.Anteil)
           , data =df_pan, model = "within")
summary(fe4)


fe5 <- plm(inzidenz ~ lag(inzidenz, 1), data =df_pan, model = "within")
summary(fe5)


fe6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
            + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf
           , data =df_pan, model = "within")
summary(fe6)

phtest(re100, fe1)
#### p-value < 0.05 ---> fixed effects




fe1.fixedweek <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2) +
                       A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
                     + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf + factor(week)
                     , data =df_pan, model = "within")
summary(fe1.fixedweek)
pFtest(fe1.fixedweek, fe1)
plmtest(fe1, c("time"), type=("bp"))

## fixed time effect week is needed, both clearly smaller than 0.05

pcdtest(fe1, test = c("lm"))
pcdtest(fe1, test = c("cd"))
pbgtest(fe1)

## cross sectional dependancy, is obvious given neighboring districts are part of the independent variables
## serial correlation at hand

library(tseries)
adf.test(df_pan$inzidenz, k=2)

## pvalue 0.01 --> stationary for lag order 2 
## This also holds for k=1, k=3, k=4
## k meaning biggest lag level

bptest(fe1.fixedweek, data = df_pan, studentize=F)

## heteroskedasticity at hand


t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fe1, type = x)))))

coeftest(fe1, vcovHC(fe1, method = "arellano"))

fe1.vcovHC <- vcovHC(fe1, method = "arellano")

acf(fe1$residuals, type = "correlation")
### link is both lines
### https://www.codingprof.com/3-easy-ways-to-test-for-autocorrelation-in-r-examples/#:~:
### text=In%20R%2C%20the%20easiest%20way%20to%20test%20for,perform%20the%20Durbin-Watson%20test%20or%20the%20Breusch-Godfrey%20test.
fe1.HC <- coeftest(fe1, vcovHC)
summary(fe1.HC)
#fe1.HC$residuals
#residuals.plm$residuals.plm

### ---> arellano
















##### test models



fe7 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf + factor(week)
           , data =df4_pan, model = "within")
summary(fe7)

df_pan2<-df4_pan[-(which(df4_pan$week==1)),]
df_pan2<-df_pan2[-(which(df_pan2$week==2)),]

#residual plots

s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)), c(lag(df_pan2$inzidenz,2))
              ,c(lag(df_pan2$weightednbinz, 2)),
              c(df_pan2$A05.14.Anteil),c(df_pan2$A15.34.Anteil),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),c(df_pan2$A60.79.Anteil),
              c(df_pan2$rate_zweitimpf), c(df_pan2$rate_drittimpf), c(df_pan2$rate_viertimpf), 
              c(I(df_pan2$hotspot * lag(df_pan2$inzidenz, 1))) , c(I(df_pan2$hotspotnb * lag(df_pan2$inzidenz, 1))))

#t<-na.omit(s)

colnames(s)<-c("inzidenz1","weightednbinz1","inzidenz2","weightednbinz2",
               "A05.14.Anteil","A15.34.Anteil","density_inzidenz1","A60.79.Anteil",
               "rate_zweitimpf","rate_drittimpf","rate_viertimpf", "hotspot_inzidenz1", "hotspotnb_inzidenz1")


plot(formula = fe7$residuals ~ s$inzidenz2, xlab = "inzidenz", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$hotspot_inzidenz1, xlab = "hotspot_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$hotspotnb_inzidenz1, xlab = "hotspotnb_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~s$weightednbinz2 , xlab = "weightednbinz2", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~s$A05.14.Anteil , xlab = "A05.14.Anteil", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$A15.34.Anteil, xlab = "A15.34.Anteil", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$density_inzidenz1, xlab = "density_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$A60.79.Anteil, xlab = "A60.79.Anteil", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_zweitimpf, xlab = "rate_zweitimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_drittimpf, xlab = "rate_drittimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_viertimpf, xlab = "rate_viertimpf", ylab = "Residuen", cex.axis = 0.8)






fe7nf <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf + rate_viertimpf 
           , data =df4_pan, model = "within")



pFtest(fe7, fe7nf)
## < 0.05
plmtest(fe7nf, c("time"), type=("bp"))
## <0.05
## fixed time effect week is needed, both clearly smaller than 0.05

pcdtest(fe7, test = c("lm"))
pcdtest(fe7, test = c("cd"))
pbgtest(fe7)

## cross sectional dependancy, is obvious given neighboring districts are part of the independent variables
## serial correlation at hand

library(tseries)
acf(fe7$residuals, type = "correlation")
adf.test(df4_pan$inzidenz, k=2)

## pvalue 0.01 --> stationary for lag order 2 
## This also holds for k=1, k=3, k=4
## k meaning biggest lag level

bptest(fe7, data = df4_pan, studentize=F)

## heteroskedasticity at hand


t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fe1, type = x)))))

coeftest(fe7, vcovHC(fe7, method = "arellano"))





### only 1 lag
fe8 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
           + I(hotspot * lag(inzidenz, 1)) + I(hotspotnb * lag(weightednbinz, 1))
           + A05.14.Anteil+ A15.34.Anteil + I(log(density)*lag(inzidenz, 1))
           + A60.79.Anteil + rate_zweitimpf + rate_drittimpf  + factor(week)
           , data =df4_pan, model = "within")
summary(fe8)

coeftest(fe8, vcovHC(fe8, method = "arellano"))





fe.step0 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1))
              + factor(week)
               , data =df4_pan, model = "within")

summary(fe.step0)

fe.step1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
                + factor(week)
                , data =df4_pan, model = "within")

#pwaldtest(fe.step1, plm(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
#                        + I(log(density)*lag(inzidenz, 1))
#                        + factor(week), data =df4_pan, model = "within"), param = "coef", vcov = NULL)

pFtest(fe.step1, fe.step0)

fe.step2 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1))
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step2, fe.step1)


fe.step3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A60.79.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step3, fe.step2)

## rejected


fe.step4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A35.59.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step4, fe.step2)

## rejected


fe.step5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A15.34.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step5, fe.step2)

## rejected


fe.step6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A05.14.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step6, fe.step2)

## rejected

fe.step7 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A00.04.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step7, fe.step2)

## REJECTED

fe.step8 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + A80.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step8, fe.step2)

## rejected

fe.step9 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + F.Anteil
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step9, fe.step2)

## rejected --- for M.Anteil as well


fe.step10 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + rate_zweitimpf
                + factor(week)
                , data =df4_pan, model = "within")

pFtest(fe.step10, fe.step2)


## rejected

fe.step11 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + rate_drittimpf
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step11, fe.step2)

## rejected

fe.step12 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + rate_viertimpf
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step12, fe.step2)

## rejected

fe.step13 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step13, fe.step2)

## scored

fe.step14 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(rate_drittimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step14, fe.step13)

pFtest(fe.step14, fe.step2)

### either zweitimpfung or drittimpfung

fe.step15 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(A60.79.Anteil * rate_drittimpf)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step15, fe.step13)

## soft rejection

fe.step16 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(A15.34.Anteil * rate_drittimpf)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step16, fe.step13)

## rejected

fe.step17 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(A15.34.Anteil * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step17, fe.step13)


## STRONG REJECTION

fe.step18 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(rate_drittimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")

pFtest(fe.step18, fe.step13)


## rejected


fe.step19 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + I(rate_drittimpf * hotspot) + F.A15.34.Anteil
                 + factor(week)
                 , data = df4_pan, model = "within")
pFtest(fe.step19, fe.step13)

## rejected


coeftest(fe.step15, vcovHC(fe.step15, method = "arellano"))

summary(fe.step15)

coeftest(fe.step13, vcovHC(fe.step13, method = "arellano"))

## fe.step13 is the winner, hooray

fe.actual <- fe.step13

df_pan2<-df4_pan[-(which(df4_pan$week==1)),]


s<-data.frame(c(lag(df_pan2$inzidenz, 1)),c(lag(df_pan2$weightednbinz, 1)),
              c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))),
              c(I(df_pan2$rate_zweitimpf * df_pan2$hotspot)), 
              c(I(df_pan2$hotspot * lag(df_pan2$inzidenz, 1))) ,
              c(I(df_pan2$hotspotnb * lag(df_pan2$inzidenz, 1))))

#s<-na.omit(s)

colnames(s)<-c("inzidenz1","weightednbinz1","density_inzidenz1",
               "zweitimpf_hotspot", "hotspot_inzidenz1", "hotspotnb_inzidenz1")


plot(formula = fe.actual$residuals ~ s$inzidenz1, xlab = "inzidenz", ylab = "Residuen", cex.axis = 0.8)
plot(formula = fe.actual$residuals ~s$weightednbinz1 , xlab = "weightednbinz1", ylab = "Residuen", cex.axis = 0.8)
plot(formula = fe.actual$residuals ~ s$density_inzidenz1, xlab = "density_inzidenz1", ylab = "Residuen", cex.axis = 0.8)
plot(formula = fe.actual$residuals ~ s$zweitimpf_hotspot, xlab = "zweitimpf_hotspot", ylab = "Residuen", cex.axis = 0.8)
plot(formula = fe.actual$residuals ~ s$hotspot_inzidenz1, xlab = "hotspot_inzidenz1", ylab = "Residuen", cex.axis = 0.8)
plot(formula = fe.actual$residuals ~ s$hotspotnb_inzidenz1, xlab = "hotspotnb_inzidenz1", ylab = "Residuen", cex.axis = 0.8)

plot(s$density_inzidenz1)


df4_pan <- pdata.frame(df4, index=c("district", "week"))


re.step0 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1))
                + factor(week)
                , data =df4_pan, model = "random")

summary(re.step0)

re.step1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step1, re.step0)

## include: hotspot*inzidenz

re.step1.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  + I(hotspotnb * lag(weightednbinz, 1))
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step1.5, re.step1)

## include: hotspotnb*inzidenz 

re.step2 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1))  
                + rate_zweitimpf
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step2, re.step1.5)

## reject rate_zweitimpf


re.step2.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step2.5, re.step1.5)

## include rate_zweitimpf * hotspot


re.step3 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil 
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step3, re.step2.5)

## include A60.79.Anteil


re.step4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + A35.59.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step4, re.step3)


re.step4.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A35.59.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step4.5, re.step2.5)


## reject A35.59.Anteil


re.step5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + A15.34.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step5, re.step3)

re.step5.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A15.34.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step5, re.step2.5)

## Entweder / oder, beide Variablen sind nicht gleichzeitig signifikant. 
## Wir nehmen zunächst A60.79.Anteil als Variable auf


re.step6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + A05.14.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step6, re.step3)

re.step6.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A05.14.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step6.5, re.step2.5)


## reject A05.14.Anteil

re.step7 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + A00.04.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step7, re.step3)

re.step7.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A00.04.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step7.5, re.step2.5)


## REJECT A00.04.Anteil

re.step8 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + A80.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step8, re.step3)

re.step8.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A80.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step8.5, re.step2.5)

## reject A80.Anteil


re.step9 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil + F.Anteil
                + factor(week)
                , data =df4_pan, model = "random")

pFtest(re.step9, re.step3)

re.step9.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + M.Anteil
                  + factor(week)
                  , data =df4_pan, model = "random")

pFtest(re.step9.5, re.step2.5)

## reject --- for F.Anteil, for M.Anteil as well


re.step10 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil + rate_zweitimpf
                 + factor(week)
                 , data =df4_pan, model = "random")

pFtest(re.step10, re.step3)

## reject rate_zweitimpf

re.step11 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil + rate_drittimpf
                 + factor(week)
                 , data =df4_pan, model = "random")
pFtest(re.step11, re.step3)

re.step11.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                   + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                   + I(hotspotnb * lag(weightednbinz, 1)) + rate_drittimpf
                   + factor(week)
                   , data =df4_pan, model = "random")

pFtest(re.step11.5, re.step1.5)

## reject rate_drittimpf

re.step12 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil + rate_viertimpf
                 + factor(week)
                 , data =df4_pan, model = "random")
pFtest(re.step12, re.step3)

re.step12.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                   + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                   + I(hotspotnb * lag(weightednbinz, 1)) + rate_viertimpf
                   + factor(week)
                   , data =df4_pan, model = "random")

pFtest(re.step12.5, re.step1.5)

## reject rate_viertimpf



re.step14 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + A60.79.Anteil + I(rate_drittimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "random")

pFtest(re.step14, re.step3)

re.step14.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                   + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                   + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_drittimpf * hotspot)
                   + factor(week)
                   , data =df4_pan, model = "random")

pFtest(re.step14.5, re.step1.5)

## Entweder / oder zweitimpfung*hotspot oder drittimpfung*hotspot; 
## beide ähnlich signifikant, drittimpf hatte einen kleineren p-value (0.01 zu 0.028)
## jedoch hat rate_drittimpf * hotspot extrem wenig Beobachtungen --> zweitimpfung*hotspot

re.step15 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + A60.79.Anteil + I(A60.79.Anteil * rate_drittimpf)
                 + factor(week)
                 , data =df4_pan, model = "random")

pFtest(re.step15, re.step3)

## soft reject A60.79.Anteil * rate_drittimpf; p value 0.08

re.step16 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + A60.79.Anteil
                 + I(A15.34.Anteil * rate_drittimpf)
                 + factor(week)
                 , data =df4_pan, model = "random")

pFtest(re.step16, re.step3)

## reject A15.34.Anteil * rate_drittimpf

re.step17 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + A60.79.Anteil
                 + I(A60.79.Anteil * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "random")

pFtest(re.step17, re.step3)


## STRONG REJECT of Age * hotspot (same conclussion for other age groups)

re.step18 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 , data =df4_pan, model = "random")

pFtest(re.step3, re.step18)

## factor(week) stays



re.actual <- re.step3
pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")
plmtest(pool, type=c("bp"))


pcdtest(pool, test = c("lm"))
pcdtest(pool, test = c("cd"))

# Plots
plot(as.vector(fitted.values(pool)), as.vector(residuals(pool)))


#df_pan2<-df4_pan[-(which(df4_pan$week==1)),]

#t<-data.frame(c(lag(df_pan2$inzidenz, 1)),
#              c(lag(df_pan2$weightednbinz, 1)),
 #             c(I(log(df_pan2$density)*lag(df_pan2$inzidenz, 1))), 
  #            c(I(log(df_pan2$hotspot)*lag(df_pan2$inzidenz, 1))),
   #           c(I(log(df_pan2$hotspotnb)*lag(df_pan2$weightednbinz, 1))),
    #          c(I(log(df_pan2$rate_zweitimpf)*lag(df_pan2$hotspot))),
     #         c(df_pan2$A60.79.Anteil))

#t<-na.omit(s)

#colnames(t)<-c("inzidenz1", "weightednbinz1", "density_inzidenz1", "hotspot_inzidenz1", "hotspotnb_weightednbinz1",
             #  "rateZweitimpf_hotspot", "A60.79.Anteil")

#plot(formula = pool$residuals ~ t$inzidenz1, xlab = "Inzidenz mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Inzidenz")

#plot(formula = pool$residuals ~t$weightednbinz1 , xlab = "gewichtete Nachbar-Inzidenzen mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für gewichtete Nachbar-Inzidenzen")

#plot(formula = pool$residuals ~t$density_inzidenz1 , xlab = "log(Dichte) * Inzidenz mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Dichte * Inzidenz mit lag = 1")

#plot(formula = pool$residuals ~t$hotspot_inzidenz1 , xlab = "log(Hotspot) * Inzidenz mit lag = 1", ylab = "Residuen", cex.axis = 0.8, cex = 0.4) + abline(h = 0, col = "red", lwd = 2) + title("Residuen Plot für Hotspot * Inzidenz mit lag = 1")
## cross sectional dependence 



pbgtest(pool)
## serial correlation

adf.test(df4_pan$inzidenz, k=1)
## assumed stationary


bptest(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
       + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
       +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
       + A60.79.Anteil 
       + factor(week)
       + factor(district)
       , data =df4_pan, studentize = F)

## assumed heteroskedasticity 

coeftest(pool, vcovHC(pool, method = "arellano"))
coeftest(pool, vcovHC(pool, type = "HC0"))



inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
+ I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
+I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
+ A60.79.Anteil 
+ factor(week)


fgls1 <- pggls(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil + factor(week),
                data = df4_pan, effect = "time", model = "pooling")
summary(fgls1)

plot(as.vector(fitted.values(fgls1)), as.vector(residuals(fgls1)))



fgls2 <- pggls(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil ,
               data = df4_pan, family = BCCG, c("individual", "time"), model = "pooling")

plot(as.vector(fitted.values(fgls2)), as.vector(residuals(fgls2)))



pbgtest(fgls1)
pcdtest(fgls1, test = c("lm"))
pcdtest(fgls1, test = c("cd"))


logLik.plm <- function(object){
  out <- -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  
  attr(out,"df") <- nobs(object) - object$df.residual
  attr(out,"nobs") <- plm::nobs(summary(object))
  return(out)
}


stats::logLik(pool)
stats::AIC(pool)
stats::BIC(pool)

LogLik.plm(re.actual)

stats::logLik(re.actual)
stats::AIC(re.actual)



df4 
df4 <- df4 %>% 
  mutate(Kalendarwoche = df4$week+3)


p<-c(1:9)
nullt<-subset(df4, df4$Kalendarwoche%in%p)

p<-c(9:20)
erst<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(20:39)
zweit<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(39:(52+8))
dritt<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+9-1):(52+23))
viert<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+24-1):(52+30))
fünft<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+31-1):(52+51))
sechst<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+52-1):(52+151))
siebt<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(20:30)
zweit_a<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(30:39)
zweit_b<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+31-1):(52+39))
sechst_a<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+40-1):(52+51))
sechst_b<-subset(df4,df4$Kalendarwoche%in%p)


df4_pan <- pdata.frame(df4, index = c("district", "week"))

### residual plots fill with alpha = 0.03


nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool.nullt<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =nullt_pan, model = "pooling")

summary(pool.nullt)

# pool0.step2<-plm(inzidenz ~ lag(inzidenz, 1) 
#                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
#                  +I(hotspotnb * lag(weightednbinz, 1)) 
#                  + A60.79.Anteil 
#                  + factor(week)
#                  , data =nullt_pan, model = "pooling")
# 
# #pFtest(pool0.step1, pool0.step2)
# 
# #pool0.step3<-plm(inzidenz ~ lag(inzidenz, 1) 
#                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
#                  + A60.79.Anteil 
#                  + factor(week)
#                  , data =nullt_pan, model = "pooling")
# 
# #pFtest(pool0.step2,pool0.step3)
# 
# #pool0.step4<-plm(inzidenz ~ lag(inzidenz, 1) 
# #               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
# #              + factor(week)
# #             , data =nullt_pan, model = "pooling")
# 
# #pFtest(pool0.step3,pool0.step4)
# 
pool.nullt.adj<-plm(inzidenz ~ lag(inzidenz, 1)
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
                 + A60.79.Anteil + A15.34.Anteil
                 + factor(week)
                 , data =nullt_pan, model = "pooling")
summary(pool.nullt.adj)
coeftest(pool.nullt.adj, vcovHC(pool.nullt.adj, type = "HC0"))

#pool0.step52<-plm(inzidenz ~ lag(inzidenz, 1) 
#               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
#               + A60.79.Anteil + A15.34.Anteil
#               + factor(week)
#               , data =nullt, model = "pooling",index=c("district","week"))


#summary(pool0.step5)

#pFtest(pool0.step5,pool0.step3)


nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool.nullt<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) 
                + A60.79.Anteil 
                + factor(week)
                , data =nullt_pan, model = "pooling")

summary(pool.nullt)


erst_pan<-pdata.frame(erst,index=c("district","week"))

pool.erst<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =erst_pan, model = "pooling")

summary(pool.erst)



zweit_pan<-pdata.frame(zweit,index=c("district","week"))

pool.zweit<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =zweit_pan, model = "pooling")

summary(pool.zweit)

dritt_pan <-pdata.frame(dritt,index=c("district","week"))

pool.dritt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                 + A60.79.Anteil 
                 + factor(week)
                 , data =dritt_pan, model = "pooling")
summary(pool.dritt)



pool.viert <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =viert, model = "pooling", index=c("district", "week"))
pool.fuenft <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =fünft, model = "pooling", index=c("district", "week"))
pool.sechst <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =sechst, model = "pooling", index=c("district", "week"))
pool.siebt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =siebt, model = "pooling", index=c("district", "week"))

pool.zweit.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                    +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                    + A60.79.Anteil 
                    + factor(week)
                    , data = zweit_a, model = "pooling", index = c("district", "week"))
stats::AIC(pool.zweit.a)

pool.zweit.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                    +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                    + A60.79.Anteil 
                    + factor(week)
                    , data = zweit_b, model = "pooling", index = c("district", "week"))
stats::AIC(pool.zweit.b)


pool.sechst.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                    +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                    + A60.79.Anteil 
                    + factor(week)
                    , data = sechst_a, model = "pooling", index = c("district", "week"))
stats::AIC(pool.sechst.a)


pool.sechst.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data = sechst_b, model = "pooling", index = c("district", "week"))
stats::AIC(pool.sechst.b)


sum.nullt <- summary(pool.nullt)
sum.erst <- summary(pool.erst)
sum.zweit <- summary(pool.zweit)
sum.dritt <-summary(pool.dritt)
sum.viert <-summary(pool.viert)
sum.fuenft <-summary(pool.fuenft)
sum.sechst <-summary(pool.sechst)
sum.siebt <-summary(pool.siebt)
sum.zweit.a <-summary(pool.zweit.a)
sum.zweit.b <-summary(pool.zweit.b)
sum.sechst.a <-summary(pool.sechst.a)
sum.sechst.b <-summary(pool.sechst.b)

sum.nullt$r.squared
sum.erst$r.squared
sum.zweit$r.squared
sum.dritt$r.squared
sum.viert$r.squared
sum.fuenft$r.squared
sum.sechst$r.squared
sum.siebt$r.squared
sum.zweit.a$r.squared
sum.zweit.b$r.squared
sum.sechst.a$r.squared
sum.sechst.b$r.squared


test.vcov <- vcovHC(
  pool,
  type = "HC0",
  cluster = c("group", "time"),
)

coefficients <- coeftest(pool, vcovHC(
  pool,
  type = "HC0",
  cluster = c("group", "time"),
))


df4 <- df4 %>% mutate(inzidenzsqrd = inzidenz * inzidenz)
df4_pan <- pdata.frame(df4, index = c("district", "week"))




pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")

pool.2 <- plm(inzidenz ~ 0 + lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + lag(exp(inzidenzsqrd), 1)
              + factor(week) * factor(district)
              , data =df4_pan, model = "pooling")
coeftest(pool.2, vcov = vcovHC(
  pool.2,
  type = "HC0",
  cluster = c("group", "time")))
summary(pool.2)
+ lag(weightednbinz, 1) 
+ I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
+I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
+ A60.79.Anteil 
+ factor(week)
plot(as.vector(fitted.values(pool.2)), as.vector(residuals(pool.2)))


summary(pool, vcov=vcovHC)

testc <- coefficients[,1]
pool.adj <- pool
pool.adj$coefficients <- testc

coeftest(pool, vcov = vcovHC(pool, type = "HC0"))
summary(re.step3)
coeftest(re.step3, vcov = vcovHC(pool, type = "HC0"))



pool.log <- plm(log(inzidenz + 1) ~ lag(log(inzidenz +1), 1) + lag(log(weightednbinz +1), 1) 
            + log(I(log(density)*lag(inzidenz, 1)) + 1) + I(hotspot * lag(log(inzidenz+1), 1)) 
            + I(hotspotnb * lag(log(weightednbinz+1), 1)) 
            + factor(week)
            , data =df4_pan, model = "pooling")

plot(as.vector(fitted.values(pool.log)), as.vector(residuals(pool.log)))


pool.log2 <- plm(log(inzidenz + 1, base = exp(0.5)) ~ lag(log(inzidenz + 1, base = exp(0.5)), 1) 
                + lag(log(weightednbinz + 1, base = exp(0.5)), 1) 
                + I(log(density)*lag(log(inzidenz + 1, base = exp(0.5)), 1) + 1) 
                + I(hotspot * lag(log(inzidenz + 1, base = exp(0.5)), 1)) 
                + I(hotspotnb * lag(log(weightednbinz + 1, base = exp(0.5)), 1)) 
                + factor(week)
                , data =df4_pan, model = "pooling")

plot(as.vector(fitted.values(pool.log2)), as.vector(residuals(pool.log2)))


pool.log3 <- plm(log(inzidenz + 1, base = exp(0.2)) ~ lag(log(inzidenz + 1, base = exp(0.2)), 1) 
                 + lag(log(weightednbinz + 1, base = exp(0.2)), 1) 
                 + I(log(density)*lag(log(inzidenz + 1, base = exp(0.2)), 1) + 1) 
                 + I(hotspot * lag(log(inzidenz + 1, base = exp(0.2)), 1)) 
                 + I(hotspotnb * lag(log(weightednbinz + 1, base = exp(0.2)), 1)) 
                 + factor(week)
                 , data =df4_pan, model = "pooling")

plot(as.vector(fitted.values(pool.log3)), as.vector(residuals(pool.log3)))

pool.log4 <- plm(log(inzidenz + 1, base = 1.1) ~ lag(log(inzidenz + 1, base = 1.1), 1) 
                 + lag(log(weightednbinz + 1, base = 1.1), 1) 
                 + I(log(density)*lag(log(inzidenz + 1, base = 1.1), 1) + 1) 
                 + I(hotspot * lag(log(inzidenz + 1, base = 1.1), 1)) 
                 + I(hotspotnb * lag(log(weightednbinz + 1, base = 1.1), 1)) 
                 + factor(week)
                 , data =df4_pan, model = "pooling")
plot(as.vector(fitted.values(pool.log4)), as.vector(residuals(pool.log4)))

plot(fitted.values(pool.log), pool.log$residuals)


pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")