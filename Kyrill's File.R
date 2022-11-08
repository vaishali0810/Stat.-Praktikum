`cases_GermanTemporal_2022-10-25` <- readRDS("~/Statistische Software/Stat.-Praktikum/cases_GermanTemporal_2022-10-25.rds")

data<-`cases_GermanTemporal_2022-10-25`

str(data)

View(data)

summary(data)

data[,6]<-as.Date(data[,6])

data[,7]<-as.Date(data[,7])

levels(data$state)

## Schleswig Holstein
schleswig_holstein<-data[data$state=="Schleswig-Holstein",]

summary(schleswig_holstein)


## Baden Württemberg
baden_wurttemberg<-data[data$state=="Baden-Württemberg",]



## Bayern
bayern<-data[data$state=="Bayern",]



## Berlin
berlin<-data[data$state=="Berlin",]























