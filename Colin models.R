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
