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
library(plm) 
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
plot(residuals(re2))

re3 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil + A00.04.Anteil + A05.14.Anteil + A15.34.Anteil + A35.59.Anteil + A60.79.Anteil 
           + A80.Anteil - 1, data = dfultimate_pan, model = "random")

summary(re3)
plot(residuals(re3))


#### https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2022/Ausgaben/10_22.pdf?__blob=publicationFile




## sum(Inzidenz * population) / gesamt population

re4 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil  + A00.04.Anteil + A05.14.Anteil + A15.34.Anteil + A35.59.Anteil + A60.79.Anteil 
           + A80.Anteil - 1, data = dfultimate_pan, model = "random")
summary(re4)
plot(residuals(re4))

re5 <- plm(inzidenz ~ lag(inzidenz, 1) + rate_zweitimpf + rate_drittimpf + density + 
             M.Anteil*m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re5)
plot(residuals(re5))

re6 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil*m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re6)
plot(residuals(re6))

re7 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil + m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re7)

## gender spielt nur mit extremen phacking eine Rolle ---> raus

####

plot(residuals(re3))







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
sechst_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

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








re100 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
            + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil +density
            + A35.59.Anteil + A60.79.Anteil + A80.Anteil,
            - 1, data =df_pan, model = "random")



stepAIC(object = re100, direction = "both",
        k = log(nrow(df_pan)), trace = FALSE)







####lambda bestimmen
model_lasso_cv <- glmsmurf(formula = inzidenz ~ p(lag(inzidenz, 1), pen = "lasso") + p(density,pen = "lasso")
                           + p(rate_zweitimpf, pen = "lasso")+ p(m_anteil, pen = "lasso"), family = gaussian(),
                           data = dfultimate_pan, lambda = "cv.mse")
plot_lambda(model_lasso_cv)


###Schätzung mit neuem lambda
lasso <- model_lasso_cv$lambda
model_lasso <- glmsmurf(formula = inzidenz ~ p(lag(inzidenz, 1), pen = "lasso") + p(density,pen = "lasso")
                           + p(rate_zweitimpf, pen = "lasso")+ p(m_anteil, pen = "lasso"), family = gaussian(),
                           data = dfultimate_pan, lambda = lasso)
summary(model_lasso)

###Variablenselektion https://stackoverflow.com/questions/48978179/r-plotting-lasso-beta-coefficients
library(reshape)

dfultimate=na.omit(dfultimate)
x=model.matrix(inzidenz ~ bezirk + density + m_anteil + rate_zweitimpf, dfultimate)[,-1]
y=as.matrix(dfultimate$inzidenz)
lasso.mod =glmnet(x,y, alpha =1)
beta=coef(lasso.mod)

tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso.mod$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

##plot
library(ggplot2)

ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(3,"lines"))

