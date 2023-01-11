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
fe1.HC$residuals
residuals.plm$residuals.plm

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

pwaldtest(fe.step1, plm(formula = inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                        + I(log(density)*lag(inzidenz, 1))
                        + factor(week), data =df4_pan, model = "within"), param = "coef", vcov = NULL)

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




plot(formula = fe7$residuals ~ s$rate_zweitimpf, xlab = "rate_zweitimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_drittimpf, xlab = "rate_drittimpf", ylab = "Residuen", cex.axis = 0.8)

plot(formula = fe7$residuals ~ s$rate_viertimpf, xlab = "rate_viertimpf", ylab = "Residuen", cex.axis = 0.8)



