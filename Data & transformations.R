rm(list=ls())
data <- readRDS("cases_GermanTemporal_2022-11-28.rds")

library(tidyr)
library(dplyr)
trends <- read.csv("trends.csv", header=TRUE, sep = ",")
View(trends)
# popkreise <- read.csv("04-kreise.csv", header = TRUE, sep= ";")
# View(popkreise)
# data from destatis.de ; density in km^2
popbay <- read.csv("popBay.csv", header = TRUE, sep = ";")
View(popbay) 
popbay <- popbay %>% mutate(Kreis...Landkreise = recode(Kreis...Landkreise, "Kreisfreie Stadt" = "SK", "Landkreis" = "LK"))
popbay$district <- "NA"
popbay$district <- paste(popbay$Kreis...Landkreise, popbay$Kreisfreie.Stadt, sep=" ")
popbay <- popbay %>% select(state, bezirk, district, population, male, female, density, area)
colnames(popbay)
dbayern3 <- merge(dbayern2, popbay, by = c("district", "state"))
View(dbayern3)
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

data[,6]<-as.Date(data[,6])

data_new<-data[,-7]

data_new[,15]<-data_new[,2]

dbayern<-data_new[data_new$state=="Bayern",]

levels(dbayern[,15])<-c(levels(dbayern[,15])[1:411],"Schwaben","Oberbayern",
                        "Unterfranken","Oberpfalz","Oberfranken","Mittelfranken",
                        "Niederbayern")

dbayern[,15]<-as.vector(dbayern[,15])

names(dbayern)<-c(names(dbayern)[1:14],"bezirk")

library(ggplot2)

library(dplyr)

# jetzt Bayern Abteil einlesen (Zeile 38-136)

# str(data)

# View(data)

# summary(data)

# levels(data$state)

# factor2<-as.factor(data$new_fatality) 

# factor2


## Bayern
# dbayern<-data_new[data_new$state=="Bayern",]
# View(dbayern)
# str(dbayern)
# summary(dbayern)
# summary(dbayern$district)
# vector2<-as.vector(summary(dbayern$district))
# vector2 # insgesamt 100 districts, 96 districts mit Beobachtungen, districts
# sind schon der Größe nach sortiert

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
library(dplyr)
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
View(Storage4[[1]])

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
View(Storage8[[1]])
Storage9<-Storage8
for(i in 1:length(bezirk_names)){
  Storage9[[i]]<-Storage9[[i]]%>%arrange(district)
}
View(Storage9[[1]])

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

########## Plots
# vectortop10<-c(summary(dbayern$district)[1:10])
# tbl1<-as.table(vectortop10)
# df1<-as.data.frame(tbl1)
## >>>>>>> d6e934ed6cd41746a4e5ec2d31b10c2d25a114b5
# View(df1)
# ggplot(data=df1,mapping=aes(x=Var1, y=Freq))+geom_bar(stat = "identity", position="dodge")
# vector23names<-names(vector23)
#vector23names[1:6]
#vector23names[46:51]
#vector23names[91:96]
#bayern_cases<-ggplot(data=dbayern,mapping=aes(x=dbayern$date,y=dbayern$cases))+geom_line()
#bayern_cases

df91<-as.data.frame(Storage9[[1]]) #Mittelfranken
ggplot(df91, aes(date,cases,color = gender,
                linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df92<-as.data.frame(Storage9[[2]]) #Niederbayern
ggplot(df92, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df93<-as.data.frame(Storage9[[3]]) #Oberbayern
ggplot(df93, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df94<-as.data.frame(Storage9[[4]]) #Oberfranken
ggplot(df94, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df95<-as.data.frame(Storage9[[5]]) #Oberpfalz
ggplot(df95, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df96<-as.data.frame(Storage9[[6]]) #Schwaben
ggplot(df96, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

df97<-as.data.frame(Storage9[[7]]) #Unterfranken
ggplot(df97, aes(date,cases,color = gender,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()


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

head(Storage9[[1]])
table(Storage9[[1]]$MA15cases)
View(Storage9[[1]])
library(ggplot2)
df91<-as.data.frame(Storage9[[1]], stringsAsFactors = FALSE) #Mittelfranken
ggplot(df91, aes(date,MA29cases,color = district,
                 linetype = gender)) +
  geom_point(stat="identity",size=0.1) + 
  theme
ggplot(df91, aes(date,MA7cases,color = district,
                 linetype = gender)) +
  geom_line(stat="identity",size=0.1)
colnames(df91)



