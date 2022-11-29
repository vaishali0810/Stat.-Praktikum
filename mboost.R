dbayern3 <- merge(dbayern2, popbay, by = c("district", "state", "bezirk"))
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
dbayern3<-dbayern3[,-3]
dbayern3$cases<-as.factor(dbayern3$cases)
model_boosting_default<- glmboost(cases ~ ., data = data_train,
                                  control = boost_control(mstop = 100,
                                                          nu = 0.1))
plot(x = model_boosting_default, main = "Koeffizientenpfade")



popbay <- read.csv("popBay.csv", header = TRUE, sep = ";")
#View(popbay) 
popbay <- popbay %>% mutate(Kreis...Landkreise = recode(Kreis...Landkreise, "Kreisfreie Stadt" = "SK", "Landkreis" = "LK"))
popbay$district <- "NA"
popbay$district <- paste(popbay$Kreis...Landkreise, popbay$Kreisfreie.Stadt, sep=" ")
popbay <- popbay %>% select(state, bezirk, district, population, male, female, density, area)
colnames(popbay)

popbay2<-popbay

popbay2<-sapply(popbay2, gsub, pattern = ",", replacement= ".")
popbay2<-as.data.frame(popbay2)
a<-gsub(" ","",x=popbay2$area)
c<-as.numeric(a)
popbay2$area<-c

a<-gsub(" ","",x=popbay2$population)
b<-unlist(a)
c<-as.numeric(b)
popbay2$population<-c

a<-gsub(" ","",x=popbay2$male)
b<-unlist(a)
c<-as.numeric(b)
popbay2$male<-c

a<-gsub(" ","",x=popbay2$female)
b<-unlist(a)
c<-as.numeric(b)
popbay2$female<-c

a<-gsub(" ","",x=popbay2$density)
b<-unlist(a)
c<-as.numeric(b)
popbay2$density<-c

