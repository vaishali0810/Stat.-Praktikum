dbayern3 <- merge(dbayern2, popbay2, by = c("district", "state", "bezirk"))
View(dbayern3)
dbayern3$date <- as.Date(dbayern3$date)

n <- nrow(dbayern3)
p <- ncol(dbayern3) - 1

set.seed(123)
ind_train <- sample(x = 1:n, size = ceiling(0.8*n))
data_train <- dbayern3[ind_train, ]
ind_test <- setdiff(1:n, ind_train)
data_test <- dbayern3[ind_test, ]
dbayern3<-dbayern3[,-2]
dbayern3<-dbayern3[,-5]
dbayern3<-dbayern3[,-13]
dbayern3<-dbayern3[,-7]
dbayern3$bezirk<-as.factor(dbayern3$bezirk)
dbayern3$gender<-as.factor(dbayern3$gender)
library(plm)
dbayern3 <- pdata.frame(dbayern3, index=c("district", "date"))
library(MASS)
model_boosting_default<- glmboost(cases ~ bezirk+age_group+gender+kr_erstimpf
                                  +kr_zweitimpf+kr_drittimpf+kr_viertimpf+population+
                                    density+area, data = data_train,
                                  control = boost_control(mstop = 1000,
                                                          nu = 0.1))
model_bezirk<- glmboost(cases ~ bezirk, data = data_train,
                                  control = boost_control(mstop = 1000,
                                                          nu = 0.1))
library(smurf)
model_lasso_cv <- glmsmurf(formula =  cases ~ p(bezirk, pen = "flasso",refcat = "Mittelfranken")+
                             p(area,pen = "lasso"),family = neg.bin(2), data = dbayern3, 
                           lambda = "cv.mse")
p(lage, pen = "flasso", refcat = "normal")

plot_lambda(model_lasso_cv)
par(mar = c(5.1, 4.1, 4.1, 9.1)) 
plot(x = model_boosting_default, main = "Koeffizientenpfade")
plot(x = model_bezirk, main = "Koeffizientenpfade",)
