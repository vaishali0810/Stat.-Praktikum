

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

