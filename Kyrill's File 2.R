data<- readRDS("~/Statistische Software/Stat.-Praktikum/cases_GermanTemporal_2022-10-25.rds")

str(data)

View(data)

summary(data)

data[,6]<-as.Date(data[,6])

data[,7]<-as.Date(data[,7])

levels(data$state)

library(ggplot2)

factor2<-as.factor(data$new_fatality)

factor2


## Baden Württemberg
baden_wurttemberg<-data[data$state=="Baden-Württemberg",]
View(baden_wurttemberg)
str(baden_wurttemberg)
summary(baden_wurttemberg)
summary(baden_wurttemberg$district) # 56 districts haben keine Beobachtungen
lk_esslingen<-baden_wurttemberg[baden_wurttemberg$district=="LK Esslingen",]
levels(baden_wurttemberg$district)
vector1<-as.vector(summary(baden_wurttemberg$district))
vector1 # insgesamt 100 districts, allerdings nur 44 mit Beobachtungen
# districts sind schon der Größe nach sortiert



## Bayern
d_bayern<-data[data$state=="Bayern",]
View(d_bayern)
str(d_bayern)
summary(d_bayern)
summary(d_bayern$district)
vector2<-as.vector(summary(d_bayern$district))
vector2 # insgesamt 100 districts, 96 districts mit Beobachtungen, districts
# sind schon der Größe nach sortiert
bayern_cases<-ggplot(data=d_bayern,mapping=aes(x=d_bayern$date,y=d_bayern$cases))+geom_line()
bayern_cases

vector22<-c(summary(d_bayern$district)[1:96])
d<-as.table(vector22)
View(d)

## Berlin
berlin<-data[data$state=="Berlin",]
View(berlin)
str(berlin)
summary(berlin)
summary(berlin$district)
vector3<-as.vector(summary(berlin$district))
vector3 # insgesamt 100 districts, allerdings 88 OHNE Beobachtungen, nur 12 
# districts mit Beobachtungen



## Brandenburg
brandenburg<-data[data$state=="Brandenburg",]
View(brandenburg)
str(brandenbrug)
summary(brandenburg)
summary(brandenburg$district)
vector4<-as.vector(summary(brandenburg$district))
vector4 # wieder insgesamt 100 districts, allerdings nur 18 mit Beobachtungen


## Bremen
bremen<-data[data$state=="Bremen",]
View(bremen)
str(bremen)
summary(bremen)
summary(bremen$district)
vector5<-as.vector(summary(bremen$district))
vector5 # nur 2 districts mit Beobachtungen, und zwar SK Bremen und SK Bremerhaven


## Hamburg
hamburg<-data[data$state=="Hamburg",]
View(hamburg)
str(hamburg)
summary(hamburg)
summary(hamburg$district)
vector6<-as.vector(summary(hamburg$district))
vector6 # nur 1 district mit Beobachtungen, und zwar SK Hamburg


## Hessen
hessen<-data[data$state=="Hessen",]
View(hessen)
str(hessen)
summary(hessen)
summary(hessen$district)
vector7<-as.vector(summary(hessen$district))
vector7 # 26 districts mit Beobachtungen


## Mecklenburg-Vorpommern
mecklenburg_vorpommern<-data[data$state=="Mecklenburg-Vorpommern",]
View(mecklenburg_vorpommern)
str(mecklenburg_vorpommern)
summary(mecklenburg_vorpommern)
summary(mecklenburg_vorpommern$district)
vector8<-as.vector(summary(mecklenburg_vorpommern$district))
vector8 # 8 districts mit Beobachtungen


## Niedersachsen
niedersachsen<-data[data$state=="Niedersachsen",]
View(niedersachsen)
str(niedersachsen)
summary(niedersachsen)
summary(niedersachsen$district)
vector9<-as.vector(summary(niedersachsen$district))
vector9 # 45 districts mit Beobachtungen


## Nordrhein-Westfalen
nordrhein_westfalen<-data[data$state=="Nordrhein-Westfalen",]
View(nordrhein_westfalen)
str(nordrhein_westfalen)
summary(nordrhein_westfalen)
summary(nordrhein_westfalen$district)
vector10<-as.vector(summary(nordrhein_westfalen$district))
vector10 # 53 districts mit Beobachtungen


## Rheinland-Pfalz
rheinland_pfalz<-data[data$state=="Rheinland-Pfalz",]
View(rheinland_pfalz)
str(rheinland_pfalz)
summary(rheinland_pfalz)
summary(rheinland_pfalz$district)
vector11<-as.vector(summary(rheinland_pfalz$district))
vector11 # 36 districts mit BEobachtungen


## Saarland
saarland<-data[data$state=="Saarland",]
View(saarland)
str(saarland)
summary(saarland)
summary(saarland$district)
vector12<-as.vector(summary(saarland$district))
vector12 # 6 districts mit Beobachtungen


## Sachsen
sachsen<-data[data$state=="Sachsen",]
View(sachsen)
str(sachsen)
summary(sachsen)
summary(sachsen$district)
vector13<-as.vector(summary(sachsen$district))
vector13 #13 districts mit Beobachtungen


## Sachsen-Anhalt
sachsen_anhalt<-data[data$state=="Sachsen-Anhalt",]
View(sachsen_anhalt)
str(sachsen_anhalt)
summary(sachsen_anhalt)
summary(sachsen_anhalt$district)
vector14<-as.vector(summary(sachsen_anhalt$district))
vector14 # 14 districts mit Beobachtungen


## Schleswig Holstein
schleswig_holstein<-data[data$state=="Schleswig-Holstein",]
View(schleswig_holstein)
str(schleswig_holstein)
summary(schleswig_holstein)
summary(schleswig_holstein$district)
vector15<-as.vector(summary(schleswig_holstein$district))
vector15 # 15 districts mit Beobachtungen


## Thueringen
thueringen<-data[data$state=="Thüringen",]
View(thueringen)
str(thueringen)
summary(thueringen)
summary(thueringen$district)
vector16<-as.vector(summary(thueringen$district))
vector16 # 22 districts mit Beobachtungen






