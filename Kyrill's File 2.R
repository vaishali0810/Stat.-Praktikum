data<- readRDS("~/Statistische Software/Stat.-Praktikum/cases_GermanTemporal_2022-10-25.rds")

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




dbayern$cases_on_date<-NA
## Loop
# data_new und dbayern einlesen
vector23<-c(summary(dbayern$district))
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

dates<-seq(as.Date("2020-01-28"),as.Date("2022-11-25"),by=1)

for(j in 1:length(Storage2)){
  for(i in dates){
    Storage2[[j]][Storage2[[j]]$date==i,16]<-sum(Storage2[[j]][Storage2[[j]]$date==i,11],na.rm=TRUE)
  }
}

View(Storage2[[1]])

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
#plot1<-ggplot(df91,aes(date,cases))+geom_line()
#plot1
#plot1<-ggplot(df91,aes(date,cases,colour=gender))+geom_point(stat="identity",size=0.1)
#plot1
#plot2<-ggplot(df91,aes(x=date,y=cases,color=gender,group=gender))+geom_point()+geom_line()+geom_smooth()
#plot2
plot1<-ggplot(df91,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Mittelfranken")
plot1

df92<-as.data.frame(Storage9[[2]]) #Niederbayern
plot2<-ggplot(df92,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Niederbayern")
plot2

df93<-as.data.frame(Storage9[[3]]) #Oberbayern
plot3<-ggplot(df93,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Oberbayern")
plot3

df94<-as.data.frame(Storage9[[4]]) #Oberfranken
plot4<-ggplot(df94,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Oberfranken")
plot4

df95<-as.data.frame(Storage9[[5]]) #Oberpfalz
plot5<-ggplot(df95,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Oberpfalz")
plot5

df96<-as.data.frame(Storage9[[6]]) #Schwaben
plot6<-ggplot(df96,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Schwaben")
plot6

df97<-as.data.frame(Storage9[[7]]) #Unterfranken
plot7<-ggplot(df97,aes(x=date,y=cases,colour=district,group=district))+geom_point()+geom_line()+geom_smooth()+labs(title="Unterfranken")
plot7


#dbayern3$male_anteil<-dbayern3$male/dbayern3$population
#dbayern3$female_anteil<-dbayern3$female/dbayern3$population
#View(dbayern3)

#dbayern3$kr_erstimpf_sum<-NA

## Storage2 einlesen

#View(Storage2[[1]])

#Storage2[[i]][j,26]

#Storage2[[1]][Storage2[[1]]$date=="2020-12-27",26]<-sum(Storage2[[1]][Storage2[[1]]$date=="2020-12-27",15])

#test1<-sum(Storage2[[1]][Storage2[[1]]$date=="2020-12-27",15])

#View(Storage2[[1]])


#for(i in 1:length(Storage2)){
 # for(j in 1:681){
  #  Storage2[[i]][Storage2[[i]]$date==2020-12-26+j,26]<-sum(Storage2[[i]][Storage2[[i]]$date==2020-12-26+j,15])+sum(Storage2[[i]][Storage2[[i]]$date==2020-12-26+(j-1),15])
  #}
#}

#View(Storage2[[1]])

#for(i in 1:length(Storage2)){
#  Storage2[[i]]$kr_erstimpf_sum<-cumsum(Storage2[[i]]$kr_erstimpf)
#}

#for(i in 1:length(Storage2)){
#  for(j in as.Date("2020-12-28"):as.Date("2022-11-08")){
#    Storage2[[i]][Storage2[[i]]$date==j,26]<-sum(Storage2[[i]][Storage2[[i]]$date==(j-1),15])+sum(Storage2[[i]][Storage2[[i]]$date==j,15])
#  }
#}


#impfBayern$erstimpf_sum<-NA

#impfbayern2<-impfBayern%>%group_by(district)%>%dplyr::mutate(erstimpf_sum=cumsum(kr_erstimpf))

#View(impfbayern2)

# Models
install.packages("lme4")
library(lme4)
install.packages("MASS")
library(MASS)
glm(cases~age_group+gender+kr_erstimpf+kr_zweitimpf+kr_drittimpf+kr_viertimpf,
    family=negative.binomial(10,link="logit"),data=dbayern3)

re1 <- plm(inzidenz~ bezirk + erstimpf_sum + zweitimpf_sum + drittimpf_sum + male_anteil, data=dbayern3, model = "random")
summary(re1)

#dbayern4<-dbayern3%>%arrange(district)
#for(i in 1:length(Storage2)){
#  for(j in dates){
#    Storage2[[i]]$cases_on_date<-sum(Storage2[[i]][Storage2[[i]]$date==j,11],na.rm=TRUE)
#  }
#}
#View(Storage2[[3]])

#for(i in dates){
#  Storage2[[1]][Storage2[[1]]$date==i,16]<-sum(Storage2[[1]][Storage2[[1]]$date==i,11],na.rm=TRUE)
#}

dates<-seq(as.Date("2020-01-28"),as.Date("2022-11-25"),by=1)

for(j in 1:length(Storage2)){
  for(i in dates){
    Storage2[[j]][Storage2[[j]]$date==i,16]<-sum(Storage2[[j]][Storage2[[j]]$date==i,11],na.rm=TRUE)
  }
}

View(Storage2[[1]])

dbayernshort<-dbayern%>%select(district, age_group, gender, date, cases, bezirk,cases_on_date)

Storageshort<-list()

vector23_2<-c(summary(dbayern$district))
vector23names_2<-names(vector23)
for(i in 1:length(vector23names_2)){
  Storageshort[[i]]<-dbayernshort[dbayernshort$district==vector23names_2[i],]
}

Storage2short<-Storageshort
for(i in 1:length(vector23names_2)){
  Storage2short[[i]]<-Storage2short[[i]]%>%arrange(date)
}

for(j in 1:length(Storage2short)){
  for(i in dates){
    Storage2short[[j]][Storage2short[[j]]$date==i,7]<-sum(Storage2short[[j]][Storage2short[[j]]$date==i,5],na.rm=TRUE)
  }
}

View(Storage2short[[1]])

Storage21filter<-Storage2[[1]]%>%filter(gender=="M")%>%filter(age_group=="A00-A04")

View(Storage21filter)

#dbayern4$anteil_man<-NA
#dbayern4$anteil_woman<-NA
#dbayern4$anteil_man<-ifelse(dbayern4$gender==0,1,0)
#dbayern4$anteil_woman<-ifelse(dbayern4$gender==1,1,0)
#View(dbayern5)

#dbayern4$erstimpf_sum<-as.factor(dbayern4$erstimpf_sum)
#dbayern4$zweitimpf_sum<-as.factor(dbayern4$zweitimpf_sum)
#dbayern4$drittimpf_sum<-as.factor(dbayern4$drittimpf_sum)
#dbayern4$male_anteil<-as.factor(dbayern4$male_anteil)
#dbayern4$inzidenz<-as.factor(dbayern4$inzidenz)



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






