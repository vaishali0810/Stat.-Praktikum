install.packages("lme4")
library(lme4)


dfcombined <- df

summary(lm(inzidenz~ bezirk + lag(inzidenz, 7) + gender + erstimpf_sum + zweitimpf_sum + drittimpf_sum,  data=dbayern5))
str(dbayern5)
#dbayern3$district <- as.factor(dbayern3$district)
# dbayern3$population <- as.numeric(dbayern3$population)
# dbayern3$male <- as.numeric(dbayern3$male)
# dbayern3$female <- as.numeric(dbayern3$female)
# dbayern3$density <- as.numeric(dbayern3$density)
# dbayern3$area <- as.numeric(dbayern3$area)
# str(dbayern3)

# glm(cases ~ district , family = negative.binomial(2), data=dbayern3)
# isfactor(dbayern3)

dbayern6 <- dbayern3[,-6]
dbayern7 <- pdata.frame(dbayern6, index=c("district", "date", "age_group"))




library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
#library(ggplots2)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)   
# dbayern4 <- pdata.frame(dfcombined, index=c("district", "gender", "age_group"))
# dbayern5 <- dbayern4[complete.cases(dbayern4),]
# #re1 <- plm(inzidenz~ bezirk + erstimpf_sum + zweitimpf_sum + drittimpf_sum + male_anteil, data=dbayern5, model = "random")
# summary(re1)

summary(glm(inzidenz ~ + I(zweitimpf_sum/population) + 
             I(drittimpf_sum/population) + lag(inzidenz, 7) + I(lag(inzidenz, 7) * log(density)) , data = dbayern3))


testpdf <- pdata.frame(dfcombined, index=c("district", "date"))
re1 <- plm(total_cases~lag(total_cases, 7), data= testpdf, model = "random")




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
plot(residuals(re6))

re6 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil*m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re6)

re7 <- plm(inzidenz ~ lag(inzidenz, 1) + M.Anteil + m_anteil - 1, data = dfultimate_pan, model = "random")
summary(re7)

## gender spielt nur mit extremen phacking eine Rolle ---> raus

####

plot(residuals(re3))























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

