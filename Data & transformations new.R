rm(list=ls())

library(tidyr)
library(dplyr)
data <- readRDS("cases_GermanTemporal_2022-11-28.rds")

data[,6]<-as.Date(data[,6])

data<-data[,-7]

data[,15]<-data[,2]

dbayern<-data[data$state=="Bayern",]

levels(dbayern[,15])<-c(levels(dbayern[,15])[1:411],"Schwaben","Oberbayern",
                        "Unterfranken","Oberpfalz","Oberfranken","Mittelfranken",
                        "Niederbayern")

dbayern[,15]<-as.vector(dbayern[,15])

names(dbayern)<-c(names(dbayern)[1:14],"bezirk")

## Schwaben
dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Aichach-Friedberg"="Schwaben","SK Augsburg"="Schwaben",
                                        "LK Augsburg"="Schwaben","LK Dillingen a.d.Donau"="Schwaben",
                                        "LK Donau-Ries"="Schwaben","LK Günzburg"="Schwaben","LK Lindau"="Schwaben",
                                        "LK Neu-Ulm"="Schwaben","LK Oberallgäu"="Schwaben","LK Ostallgäu"="Schwaben",
                                        "LK Unterallgäu"="Schwaben","SK Kaufbeuren"="Schwaben","SK Kempten"="Schwaben",
                                        "SK Memmingen"="Schwaben"))
## Oberfranken
dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"SK Bamberg"="Oberfranken","SK Bayreuth"="Oberfranken",
                                        "SK Coburg"="Oberfranken","SK Hof"="Oberfranken","LK Bamberg"="Oberfranken",
                                        "LK Bayreuth"="Oberfranken","LK Coburg"="Oberfranken","LK Forchheim"="Oberfranken",
                                        "LK Hof"="Oberfranken","LK Kronach"="Oberfranken","LK Kulmbach"="Oberfranken",
                                        "LK Lichtenfels"="Oberfranken","LK Wunsiedel i.Fichtelgebirge"="Oberfranken"))

## Oberbayern_Unterfranken_oberpfalz.R File einlesen
dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"SK München" = "Oberbayern", "SK Ingolstadt" = "Oberbayern", "SK Rosenheim" = "Oberbayern",
                                        "LK Altötting" = "Oberbayern", "LK Berchtesgadener Land" = "Oberbayern", 
                                        "LK Bad Tölz-Wolfratshausen" = "Oberbayern", "LK Dachau" = "Oberbayern",
                                        "LK Ebersberg" = "Oberbayern", "LK Eichstätt" = "Oberbayern", "LK Erding" = "Oberbayern",
                                        "LK Freising" = "Oberbayern", " LK Fürstenfeldbruck" = "Oberbayern",
                                        "LK Garmisch-Partenkirchen" = "Oberbayern", "LK Landsberg a.Lech" = "Oberbayern",
                                        "LK Miesbach" = "Oberbayern","LK Mühldorf a.Inn" = "Oberbayern", "LK München" = "Oberbayern",
                                        "LK Neuburg-Schrobenhausen" = "Oberbayern", 
                                        "LK Pfaffenhofen a.d.Ilm" = "Oberbayern", "LK Rosenheim" = "Oberbayern", "LK Starnberg" = "Oberbayern", 
                                        "LK Traunstein" = "Oberbayern", "LK Weilheim-Schongau" = "Oberbayern", 
                                        "LK Erding" = "Oberbayern", "LK Bad Reichenhall" = "Oberbayern","LK Fürstenfeldbruck"="Oberbayern",
))

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Aschaffenburg" = "Unterfranken", "LK Aschaffenburg" = "Unterfranken",
                                        "LK Haßberge" = "Unterfranken", "LK Kitzingen" = "Unterfranken",
                                        "LK Main-Spessart" = "Unterfranken", "LK Miltenberg" = "Unterfranken",
                                        "LK Rhön-Grabfeld" = "Unterfranken", "LK Schweinfurt" = "Unterfranken",
                                        "LK Würzburg" = "Unterfranken", "SK Würzburg" = "Unterfranken",
                                        "SK Aschaffenburg" = "Unterfranken", "SK Schweinfurt" = "Unterfranken","LK Bad Kissingen"="Unterfranken"))

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Amberg-Sulzbach" = "Oberpfalz", "LK Cham" = "Oberpfalz",
                                        "LK Neumarkt i.d.OPf."= "Oberpfalz", "LK Neustadt a.d.Waldnaab" = "Oberpfalz",
                                        "LK Regensburg" = "Oberpfalz", "LK Schwandorf" = "Oberpfalz", "LK Tirschenreuth" = "Oberpfalz",
                                        "SK Amberg" = "Oberpfalz", "SK Regensburg"  = "Oberpfalz",
                                        "SK Weiden i.d.OPf."  = "Oberpfalz"))


## Mittelfranken_Niederbayern.R File einlesen
dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Roth"="Mittelfranken","LK Nürnberger Land"="Mittelfranken",
                                        "LK Neustadt a.d.Aisch-Bad Windsheim"="Mittelfranken","LK Ansbach"="Mittelfranken",
                                        "SK Fürth"="Mittelfranken","SK Nürnberg"="Mittelfranken","LK Weißenburg-Gunzenhausen"="Mittelfranken",
                                        "LK Erlangen-Höchstadt"="Mittelfranken","LK Fürth"="Mittelfranken","SK Schwabach"="Mittelfranken",
                                        "SK Erlangen"="Mittelfranken","SK Erlangen"="Mittelfranken","SK Ansbach"="Mittelfranken"))

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Landshut"="Niederbayern","SK Landshut"="Niederbayern",
                                        "LK Dingolfing-Landau"="Niederbayern","LK Freyung-Grafenau"="Niederbayern",
                                        "LK Regen"="Niederbayern","LK Deggendorf"="Niederbayern",
                                        "LK Passau"="Niederbayern","SK Passau"="Niederbayern","LK Rottal-Inn"="Niederbayern",
                                        "SK Straubing"="Niederbayern","LK Straubing-Bogen"="Niederbayern","LK Kelheim"="Niederbayern"))









dimpf <- read.csv("impfdaten_regional.csv")
impfBayern <- dimpf[dimpf$bundesland == "Freistaat Bayern", ]
impfBayern <- impfBayern %>%
  mutate(kreis=recode(kreis, "München, Landeshauptstadt" = "SK München",
                      "Traunstein" = "LK Traunstein",
                      "München, Kreis" = "LK München",
                      "Augsburg, Stadt" = "SK Augsburg",
                      "Rosenheim, Kreis" = "LK Rosenheim",
                      "Augsburg, Kreis"= "LK Augsburg",
                      "Schwandorf" = "LK Schwandorf",
                      "Unterallgäu" = "LK Unterallgäu",
                      "Mühldorf a.Inn" = "LK Mühldorf a.Inn",
                      "Landshut, Kreis" = "LK Landshut",
                      "Freising" = "LK Freising",
                      "Ebersberg" = "LK Ebersberg",
                      "Miltenberg" = "LK Miltenberg",
                      "Aschaffenburg, Kreis" = "LK Aschaffenburg",
                      "Rottal-Inn" = "LK Rottal-Inn",
                      "Dachau" = "LK Dachau",
                      "Pfaffenhofen a.d.Ilm" = "LK Pfaffenhofen a.d.Ilm",
                      "Ingolstadt" = "SK Ingolstadt",
                      "Roth" = "LK Roth",
                      "Günzburg" = "LK Günzburg",
                      "Nürnberger Land" = "LK Nürnberger Land",
                      "Fürstenfeldbruck" = "LK Fürstenfeldbruck",
                      "Dillingen a.d.Donau" = "LK Dillingen a.d.Donau",
                      "Donau-Ries" = "LK Donau-Ries",
                      "Altötting" = "LK Altötting",
                      "Dingolfing-Landau" = "LK Dingolfing-Landau",
                      "Kelheim" = "LK Kelheim",
                      "Bamberg, Kreis" = "LK Bamberg",
                      "Neustadt a.d.Aisch-Bad Windsheim" = "LK Neustadt a.d.Aisch-Bad Windsheim",
                      "Regensburg, Kreis" = "LK Regensburg",
                      "Freyung-Grafenau" = "LK Freyung-Grafenau",
                      "Amberg-Sulzbach" = "LK Amberg-Sulzbach",
                      "Neu-Ulm" = "LK Neu-Ulm",
                      "Rhön-Grabfeld" = "LK Rhön-Grabfeld",
                      "Neumarkt i.d.OPf." = "LK Neumarkt i.d.OPf.",
                      "Berchtesgadener Land" = "LK Berchtesgadener Land",
                      "Passau, Kreis" = "LK Passau",
                      "Bayreuth, Kreis" = "LK Bayreuth",
                      "Regen" = "LK Regen",
                      "Bad Tölz-Wolfratshausen" = "LK Bad Tölz-Wolfratshausen",
                      "Aichach-Friedberg" = "LK Aichach-Friedberg",
                      "Schweinfurt, Kreis" = "LK Schweinfurt",
                      "Forchheim" = "LK Forchheim",
                      "Miesbach" = "LK Miesbach",
                      "Regensburg, Stadt" = "SK Regensburg",
                      "Main-Spessart" = "LK Main-Spessart",
                      "Ansbach, Kreis" = "LK Ansbach",
                      "Bayreuth, Stadt" = "SK Bayreuth",
                      "Cham" = "LK Cham",
                      "Kitzingen" = "LK Kitzingen",
                      "Tirschenreuth" = "LK Tirschenreuth",
                      "Eichstätt" = "LK Eichstätt",
                      "Landshut, Stadt" = "SK Landshut",
                      "Rosenheim, Stadt" = "SK Rosenheim",
                      "Oberallgäu" = "LK Oberallgäu",
                      "Fürth, Stadt" = "SK Fürth",
                      "Aschaffenburg, Stadt" = "SK Aschaffenburg",
                      "Coburg, Kreis" = "LK Coburg",
                      "Ostallgäu" = "LK Ostallgäu",
                      "Neustadt a.d.Waldnaab" = "LK Neustadt a.d.Waldnaab",
                      "Wunsiedel i.Fichtelgebirge" = "LK Wunsiedel i.Fichtelgebirge",
                      "Deggendorf" = "LK Deggendorf",
                      "Lichtenfels" = "LK Lichtenfels",
                      "Nürnberg" = "SK Nürnberg",
                      "Weißenburg-Gunzenhausen" = "LK Weißenburg-Gunzenhausen",
                      "Erlangen-Höchstadt" = "LK Erlangen-Höchstadt",
                      "Schweinfurt, Stadt" = "SK Schweinfurt",
                      "Kulmbach" = "LK Kulmbach",
                      "Würzburg, Kreis" = "LK Würzburg",
                      "Würzburg, Stadt" = "SK Würzburg",
                      "Fürth, Kreis" = "LK Fürth",
                      "Schwabach" = "SK Schwabach",
                      "Memmingen" = "SK Memmingen",
                      "Weilheim-Schongau" = "LK Weilheim-Schongau",
                      "Bad Kissingen" = "LK Bad Kissingen",
                      "Bamberg, Stadt" = "SK Bamberg",
                      "Straubing-Bogen" = "LK Straubing-Bogen",
                      "Hof, Kreis" = "LK Hof",
                      "Erding" = "LK Erding",
                      "Erlangen" = "SK Erlangen",
                      "Lindau (Bodensee)" = "LK Lindau",
                      "Amberg" = "SK Amberg",
                      "Starnberg"= "LK Starnberg",
                      "Neuburg-Schrobenhausen" = "LK Neuburg-Schrobenhausen",
                      "Landsberg am Lech" = "LK Landsberg a.Lech",
                      "Haßberge" = "LK Haßberge",
                      "Kempten (Allgäu)" = "SK Kempten",
                      "Coburg, Stadt" = "SK Coburg",
                      "Kronach" = "LK Kronach",
                      "Weiden i.d.OPf." = "SK Weiden i.d.OPf.",
                      "Garmisch-Partenkirchen" = "LK Garmisch-Partenkirchen",
                      "Passau, Stadt" = "SK Passau",
                      "Hof, Stadt" = "SK Hof",
                      "Straubing" = "SK Straubing",
                      "Kaufbeuren" = "SK Kaufbeuren",
                      "Ansbach, Stadt" = "SK Ansbach")) %>%
  as.data.frame()
colnames(impfBayern)[5] <- "district"
colnames(impfBayern)[6] <- "date"
impfBayern[,6] <- as.Date(impfBayern[,6])

#impfBayern$erstimpf <-cumsum(impfBayern$kr_erstimpf)
#impfBayern$zweitimpf <-cumsum(impfBayern$kr_zweitimpf)
#impfBayern$drittimpf <-cumsum(impfBayern$kr_drittimpf)
#impfBayern$viertimpf <-cumsum(impfBayern$kr_viertimpf)

# impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)
# dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"))

impfBayern$erstimpf_sum<-NA
impfBayern$zweitimpf_sum<-NA
impfBayern$drittimpf_sum<-NA
impfBayern$viertimpf_sum<-NA

impfbayern2<-impfBayern%>%group_by(district)%>%dplyr::mutate(erstimpf_sum=cumsum(kr_erstimpf),zweitimpf_sum=cumsum(kr_zweitimpf),drittimpf_sum=cumsum(kr_drittimpf),viertimpf_sum=cumsum(kr_viertimpf))

#View(impfbayern2)

#impfungentake <- impfbayern2 %>% select(district, date, erstimpf_sum, zweitimpf_sum, drittimpf_sum, viertimpf_sum)
impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)
#View(impfungentake)
dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"), all.x = TRUE, all.y = TRUE)
#View(dbayern2)



a <- dbayern2$kr_erstimpf 
index <- is.na(a) 
dbayern2[index, 16] <- 0
a <- dbayern2$kr_zweitimpf 
index <- is.na(a) 
dbayern2[index, 17] <- 0
a <- dbayern2$kr_drittimpf
index <- is.na(a) 
dbayern2[index, 18] <- 0
a <- dbayern2$kr_viertimpf 
index <- is.na(a) 
dbayern2[index, 19] <- 0


dbayern2.1<-dbayern2%>%group_by(district)%>%dplyr::mutate(erstimpf_sum=cumsum(kr_erstimpf),zweitimpf_sum=cumsum(kr_zweitimpf),drittimpf_sum=cumsum(kr_drittimpf),viertimpf_sum=cumsum(kr_viertimpf))
#View(dbayern2.1)

#dbayern2$erstimpf_sum <- a
#dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"))

trends <- read.csv("trends.csv", header=TRUE, sep = ",")
#View(trends)
# popkreise <- read.csv("04-kreise.csv", header = TRUE, sep= ";")
# View(popkreise)
# data from destatis.de ; density in km^2
popbay <- read.csv("popBay.csv", header = TRUE, sep = ";")
#View(popbay) 
popbay <- popbay %>% mutate(Kreis...Landkreise = recode(Kreis...Landkreise, "Kreisfreie Stadt" = "SK", "Landkreis" = "LK"))
popbay$district <- "NA"
popbay$district <- paste(popbay$Kreis...Landkreise, popbay$Kreisfreie.Stadt, sep=" ")
popbay <- popbay %>% select(state, bezirk, district, population, male, female, density, area)
colnames(popbay)
#popbay <- read.csv("popBay.csv", header = TRUE, sep = ";")
#View(popbay) 
#popbay <- popbay %>% mutate(Kreis...Landkreise = recode(Kreis...Landkreise, "Kreisfreie Stadt" = "SK", "Landkreis" = "LK"))
#popbay$district <- "NA"
#popbay$district <- paste(popbay$Kreis...Landkreise, popbay$Kreisfreie.Stadt, sep=" ")
#popbay <- popbay %>% select(state, bezirk, district, population, male, female, density, area)
#colnames(popbay)

popbay2<-popbay

### Code character change to valid numerics (no commas, no spaces etc.)

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

popbay2$district<-gsub("SK München. Landeshauptstadt","SK München",popbay2$district)

popbay2$district<-gsub("LK Lindau (Bodensee)","LK Lindau",popbay2$district)

popbay2$district<-gsub("SK Kempten (Allgäu)","SK Kempten",popbay2$district)

#popbay2$district

#View(popbay2)

identical(names(dbayern2.1$district),names(popbay2$district))

dbayern3 <- merge(dbayern2.1, popbay2, by = c("district", "state", "bezirk"))
#View(dbayern3)
# dbayern3$date <- as.Date(dbayern3$date)
# dbayern3$population <- as.numeric(dbayern3$population)
# dbayern3$male <- as.numeric(dbayern3$male)
# dbayern3$female <- as.numeric(dbayern3$female)
# dbayern3$density <- as.numeric(dbayern3$density)
# dbayern3$area <- as.numeric(dbayern3$area)
#dbayern3$age_group <- as.factor(dbayern3$age_group)
#dbayern3 <- dbayern3[, -6]
#dbayern3 <- dbayern3 %>% mutate(gender = recode(gender, "W" = "1", "M" = "0", "unbekannt" = "NA_integer_"))
#dbayern3[, 6] <- as.numeric(dbayern3[, 6])


dbayern3 <- dbayern3 %>% 
  arrange(date) %>%
  group_by(district) %>%
  mutate(inzidenz = ((lag(cases,6) + lag(cases,5) + lag(cases,4)+ 
                        lag(cases,3) +lag(cases,2) + lag(cases,1) + cases)
                     /population) * 100000)
dbayern3<-dbayern3%>%arrange(district)

dbayern3$male_anteil<-dbayern3$male/dbayern3$population
dbayern3$female_anteil<-dbayern3$female/dbayern3$population
View(dbayern3)



## Loop
# data_new und dbayern einlesen
vector23<-c(summary(dbayern$district)[1:96])
vector23names<-names(vector23)
Storage<-list()
for(i in 1:length(vector23names)){
  Storage[[i]]<-dbayern[dbayern$district==vector23names[i],]
}
# View(Storage)

## Nach Datum sortieren
#library(dplyr)
Storage2<-Storage
for(i in 1:length(vector23names)){
  Storage2[[i]]<-Storage2[[i]]%>%arrange(date)
}
# View(Storage2)

## Nach Gender sortieren
Storage3<-Storage2
for(i in 1:length(vector23names)){
  Storage3[[i]]<-Storage3[[i]]%>%arrange(gender)
}
# View (Storage3)

## Nach age group sortieren
Storage4<-Storage3
for(i in 1:length(vector23names)){
  Storage4[[i]]<-Storage4[[i]]%>%arrange(age_group)
}
#View(Storage4[[1]])

## Nach bezirk Kategorie
dbayern$bezirk<-as.factor(dbayern$bezirk)
summary(dbayern$bezirk)
bezirk_names<-c(names(summary(dbayern$bezirk)))
Storage5<-list()
for(i in 1:length(bezirk_names)){
  Storage5[[i]]<-dbayern[dbayern$bezirk==bezirk_names[i],]
}
Storage6<-Storage5
for(i in 1:length(bezirk_names)){
  Storage6[[i]]<-Storage6[[i]]%>%arrange(date)
}
Storage7<-Storage6
for(i in 1:length(bezirk_names)){
  Storage7[[i]]<-Storage7[[i]]%>%arrange(gender)
}
Storage8<-Storage7
for(i in 1:length(bezirk_names)){
  Storage8[[i]]<-Storage8[[i]]%>%arrange(age_group)
}
#View(Storage8[[1]])
Storage9<-Storage8
for(i in 1:length(bezirk_names)){
  Storage9[[i]]<-Storage9[[i]]%>%arrange(district)
}
#View(Storage9[[1]])

a<-min(Storage9[[1]]$date)
b<-max(Storage9[[1]]$date)
c<-seq(as.Date(a), as.Date(b), "days")
c<-as.data.frame(c)
colnames(c)[1] <- "date"
y<-merge(Storage9[[1]],c, by="date",
         all.x=TRUE, all.y=TRUE)
v<-y$cases
index<-is.na(v)
v[index]<-0
y$cases<-v

v<-y$gender
index<-is.na(v)
v[index]<-"egal"
y$gender<-v

ymal<-subset(y,gender=="M")
yfem<-subset(y,gender=="W")
yunk<-subset(y,gender=="unbekannt")

smal<-aggregate(x = ymal$cases,               
                by = list(ymal$date),              
                FUN = sum)
smal<-mutate(smal, gender ="M")

sfem<-aggregate(x = yfem$cases,               
                by = list(yfem$date),              
                FUN = sum)
sfem<-mutate(sfem, gender ="W")

sunk<-aggregate(x = yunk$cases,               
                by = list(yunk$date),              
                FUN = sum)
sunk<-mutate(sunk, gender ="U")

sall<-rbind(smal,sfem,sunk)

ggplot(sall, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)



# # remove age_group_2 & reference date
# data <- data[, -(c(4, 7))]
# # formate date as date
# data[, 5]<-as.Date(data[, 5])
# 
# #reduce data set to Bavaria
# dbayern <- data[data$state == "Bayern", ]

# daten erklärung https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/45258e51f57d43efb612f700a876ae8f_0/about

## git pull 
## git add --all    // or "filename"
## git commit -m"message"
## git push
## git status

Storage9[[1]] <- Storage9[[1]] %>% arrange(Storage9[[1]], district, date, age_group, cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         MA7cases=(lag1+lag2+lag3+lag4+lag5+lag6+cases)/7)
Storage9[[1]] <- Storage9[[1]] %>% arrange(Storage9[[1]], district, date, age_group, cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         MA15cases=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+cases)/15)
Storage9[[1]] <- Storage9[[1]] %>% arrange(Storage9[[1]], district, date, age_group, cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         lag15=lag(cases,15),
         lag16=lag(cases,16),
         lag17=lag(cases,17),
         lag18=lag(cases,18),
         lag19=lag(cases,19),
         lag20=lag(cases,20),
         lag21=lag(cases,21),
         lag22=lag(cases,22),
         lag23=lag(cases,23),
         lag24=lag(cases,24),
         lag25=lag(cases,25),
         lag26=lag(cases,26),
         lag27=lag(cases,27),
         lag28=lag(cases,28),
         MA29cases=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+cases
                    +lag15+lag16+lag17+lag18+lag19+lag20+lag21+lag22+lag23+lag24+lag25+lag26+lag27+lag28)/29)
##<<<<<<< HEAD

head(Storage9[[1]])
table(Storage9[[1]]$MA15cases)
View(Storage9[[1]])
library(ggplot2)
df91<-as.data.frame(Storage9[[1]], stringsAsFactors = FALSE) #Mittelfranken
ggplot(df91, aes(date,MA29cases,color = district,
                 linetype = gender)) +
  geom_point(stat="identity",size=0.1) + 
  theme()
ggplot(df91, aes(date,MA7cases,color = district,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)
colnames(df91)



#=======
#>>>>>>> 9f7e5bbaafaad5f226b3ae0d3e7068b08754da99
#>
#>
#>
#>

which_vector1<-c(which(dfultimate$Unb.A00.04!=0))

sum(dfultimate[which_vector1,12])
#[1] 29772
sum(dfultimate[which_vector1,19])
#[1] 28372
sum(dfultimate[which_vector1,26])
#[1] 1738
#1738/2
#869

####### Unbekannt aufteilen auf Mann und Frau

## Alter 0-4
which_vector1<-c(which(dfultimate$Unb.A00.04!=0))

sum(dfultimate[which_vector1,12])
#[1] 29772
sum(dfultimate[which_vector1,19])
#[1] 28372
sum(dfultimate[which_vector1,26])
#[1] 1738
#1738/2
#869


for(i in 0:(length(which_vector1)/2)){
  dfultimate[which_vector1[2*i],19]<-dfultimate[which_vector1[2*i],26]+dfultimate[which_vector1[2*i],19]
}

for(i in 0:((length(which_vector1)/2)-1)){
  dfultimate[which_vector1[2*i+1],12]<-dfultimate[which_vector1[2*i+1],26]+dfultimate[which_vector1[2*i+1],12]
}


## Alter 5-14
which_vector2<-c(which(dfultimate$Unb.A05.14!=0))
sum(dfultimate[which_vector2,20])
#[1] 200280
sum(dfultimate[which_vector2,13])
#[1] 216963
sum(dfultimate[which_vector2,27])
#[1] 7961

for(i in 0:(length(which_vector2)/2)){
  dfultimate[which_vector2[2*i],20]<-dfultimate[which_vector2[2*i],27]+dfultimate[which_vector2[2*i],20]
}

for(i in 0:((length(which_vector2)/2)-1)){
  dfultimate[which_vector2[2*i+1],13]<-dfultimate[which_vector2[2*i+1],27]+dfultimate[which_vector2[2*i+1],13]
}



## Alter 15-34
which_vector3<-c(which(dfultimate$Unb.A15.34!=0))
sum(dfultimate[which_vector3,21])
#[1] 462669
sum(dfultimate[which_vector3,14])
#[1] 436474
sum(dfultimate[which_vector3,28])
#[1] 7961


for(i in 0:(length(which_vector3)/2)){
  dfultimate[which_vector3[2*i],21]<-dfultimate[which_vector3[2*i],28]+dfultimate[which_vector3[2*i],21]
}

for(i in 0:((length(which_vector3)/2)-1)){
  dfultimate[which_vector3[2*i+1],14]<-dfultimate[which_vector3[2*i+1],28]+dfultimate[which_vector3[2*i+1],14]
}

## Alter 35-59
which_vector4<-c(which(dfultimate$Unb.A35.59!=0))
sum(dfultimate[which_vector4,22])
#[1] 580193
sum(dfultimate[which_vector4,15])
#[1] 520384
sum(dfultimate[which_vector4,29])
#[1] 7961


for(i in 0:(length(which_vector4)/2)){
  dfultimate[which_vector4[2*i],22]<-dfultimate[which_vector4[2*i],29]+dfultimate[which_vector4[2*i],22]
}

for(i in 0:((length(which_vector4)/2)-1)){
  dfultimate[which_vector4[2*i+1],15]<-dfultimate[which_vector4[2*i+1],29]+dfultimate[which_vector4[2*i+1],15]
}


## Alter 60-79
which_vector5<-c(which(dfultimate$Unb.A60.79!=0))
sum(dfultimate[which_vector5,23])
#[1] 166313
sum(dfultimate[which_vector5,16])
#[1] 159510
sum(dfultimate[which_vector5,30])
#[1] 7961

for(i in 0:(length(which_vector5)/2)){
  dfultimate[which_vector5[2*i],23]<-dfultimate[which_vector5[2*i],30]+dfultimate[which_vector5[2*i],23]
}

for(i in 0:((length(which_vector5)/2)-1)){
  dfultimate[which_vector5[2*i+1],16]<-dfultimate[which_vector5[2*i+1],30]+dfultimate[which_vector5[2*i+1],16]
}


## Alter 80+
which_vector6<-c(which(dfultimate$Unb.A80.!=0))
sum(dfultimate[which_vector6,24])
#[1] 54106
sum(dfultimate[which_vector6,17])
#[1] 34831
sum(dfultimate[which_vector6,31])
#[1] 7961


for(i in 0:(length(which_vector6)/2)){
  dfultimate[which_vector6[2*i],24]<-dfultimate[which_vector6[2*i],31]+dfultimate[which_vector6[2*i],24]
}

for(i in 0:((length(which_vector6)/2)-1)){
  dfultimate[which_vector6[2*i+1],17]<-dfultimate[which_vector6[2*i+1],31]+dfultimate[which_vector6[2*i+1],17]
}


## Alter unbekannt
which_vector7<-c(which(dfultimate$Unb.Aunb!=0))
sum(dfultimate[which_vector7,25])
#[1] 469
sum(dfultimate[which_vector7,18])
#[1] 442
sum(dfultimate[which_vector7,32])
#[1] 7961


for(i in 0:(length(which_vector7)/2)){
  dfultimate[which_vector7[2*i],25]<-dfultimate[which_vector7[2*i],32]+dfultimate[which_vector7[2*i],25]
}

for(i in 0:((length(which_vector7)/2)-1)){
  dfultimate[which_vector7[2*i+1],18]<-dfultimate[which_vector7[2*i+1],32]+dfultimate[which_vector7[2*i+1],18]
}


