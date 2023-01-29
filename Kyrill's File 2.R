`cases_GermanTemporal_2022-11-28` <- readRDS("~/Statistische Software/Stat.-Praktikum/cases_GermanTemporal_2022-11-28.rds")

data<-`cases_GermanTemporal_2022-11-28`

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
dbayern$inzidenz<-NA
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
#dbayern$inzidenz<-NULL
#dbayern2<-dbayern
#dbayern2 <- dbayern2 %>% 
#  arrange(date) %>%
#  mutate(inzidenz = ((lag(cases,6) + lag(cases,5) + lag(cases,4)+ 
#                        lag(cases,3) +lag(cases,2) + lag(cases,1) + cases)
#                     /population) * 100000)
#
#
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

#Storage62<-Storage6
#for(i in 1:length(bezirk_names)){
#  Storage62[[i]]<-Storage62[[i]]%>%mutate(inzidenz =  ((lag(cases,6) + lag(cases,5) + 
#                                                lag(cases,4)+ lag(cases,3) +
#                                                lag(cases,2) + lag(cases,1) + 
#                                                cases)/population) * 100000)
#  
#}
  
  
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


a1<-min(Storage9[[1]]$date)
b1<-max(Storage9[[1]]$date)
c1<-seq(as.Date(a), as.Date(b), "days")
c1<-as.data.frame(c)
colnames(c1)[1] <- "date"
y1<-merge(Storage9[[1]],c1, by="date",
         all.x=TRUE, all.y=TRUE)
v1<-y1$cases
index1<-is.na(v1)
v1[index]<-0
y1$cases<-v1

v1<-y1$gender
index1<-is.na(v1)
v1[index1]<-"egal"
y1$gender<-v1

ymal1<-subset(y1,gender=="M")
yfem1<-subset(y1,gender=="W")
yunk1<-subset(y1,gender=="unbekannt")

smal1<-aggregate(x = ymal1$cases,               
                by = list(ymal1$date),              
                FUN = sum)
smal1<-mutate(smal1, gender ="M")

sfem1<-aggregate(x = yfem1$cases,               
                by = list(yfem1$date),              
                FUN = sum)
sfem1<-mutate(sfem1, gender ="W")

sunk1<-aggregate(x = yunk1$cases,               
                by = list(yunk1$date),              
                FUN = sum)
sunk1<-mutate(sunk1, gender ="U")

sall1<-rbind(smal1,sfem1,sunk1)

ggplot(sall1, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Mittelfranken")



a2<-min(Storage9[[2]]$date)
b2<-max(Storage9[[2]]$date)
c2<-seq(as.Date(a2), as.Date(b2), "days")
c2<-as.data.frame(c2)
colnames(c2)[1] <- "date"
y2<-merge(Storage9[[2]],c2, by="date",
          all.x=TRUE, all.y=TRUE)
v2<-y2$cases
index2<-is.na(v2)
v2[index2]<-0
y2$cases<-v2

v2<-y2$gender
index2<-is.na(v2)
v2[index2]<-"egal"
y2$gender<-v2

ymal2<-subset(y2,gender=="M")
yfem2<-subset(y2,gender=="W")
yunk2<-subset(y2,gender=="unbekannt")

smal2<-aggregate(x = ymal2$cases,               
                 by = list(ymal2$date),              
                 FUN = sum)
smal2<-mutate(smal2, gender ="M")

sfem2<-aggregate(x = yfem2$cases,               
                by = list(yfem2$date),              
                 FUN = sum)
sfem2<-mutate(sfem2, gender ="W")

sunk2<-aggregate(x = yunk2$cases,               
                 by = list(yunk2$date),              
                 FUN = sum)
sunk2<-mutate(sunk2, gender ="U")

sall2<-rbind(smal2,sfem2,sunk2)

ggplot(sall2, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Niederbayern")



a3<-min(Storage9[[3]]$date)
b3<-max(Storage9[[3]]$date)
c3<-seq(as.Date(a3), as.Date(b3), "days")
c3<-as.data.frame(c3)
colnames(c3)[1] <- "date"
y3<-merge(Storage9[[3]],c3, by="date",
          all.x=TRUE, all.y=TRUE)
v3<-y3$cases
index3<-is.na(v3)
v3[index3]<-0
y3$cases<-v3

v3<-y3$gender
index3<-is.na(v3)
v3[index3]<-"egal"
y3$gender<-v3

ymal3<-subset(y3,gender=="M")
yfem3<-subset(y3,gender=="W")
yunk3<-subset(y3,gender=="unbekannt")

smal3<-aggregate(x = ymal3$cases,               
                 by = list(ymal3$date),              
                 FUN = sum)
smal3<-mutate(smal3, gender ="M")

sfem3<-aggregate(x = yfem3$cases,               
                 by = list(yfem3$date),              
                 FUN = sum)
sfem3<-mutate(sfem3, gender ="W")

sunk3<-aggregate(x = yunk3$cases,               
                 by = list(yunk3$date),              
                 FUN = sum)
sunk3<-mutate(sunk3, gender ="U")

sall3<-rbind(smal3,sfem3,sunk3)

ggplot(sall3, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberbayern")


a4<-min(Storage9[[4]]$date)
b4<-max(Storage9[[4]]$date)
c4<-seq(as.Date(a4), as.Date(b4), "days")
c4<-as.data.frame(c4)
colnames(c4)[1] <- "date"
y4<-merge(Storage9[[4]],c4, by="date",
          all.x=TRUE, all.y=TRUE)
v4<-y4$cases
index4<-is.na(v4)
v4[index4]<-0
y4$cases<-v4

v4<-y4$gender
index4<-is.na(v4)
v4[index4]<-"egal"
y4$gender<-v4

ymal4<-subset(y4,gender=="M")
yfem4<-subset(y4,gender=="W")
yunk4<-subset(y4,gender=="unbekannt")

smal4<-aggregate(x = ymal4$cases,               
                by = list(ymal4$date),              
                 FUN = sum)
smal4<-mutate(smal4, gender ="M")

sfem4<-aggregate(x = yfem4$cases,               
                 by = list(yfem4$date),              
                 FUN = sum)
sfem4<-mutate(sfem4, gender ="W")

sunk4<-aggregate(x = yunk4$cases,               
                 by = list(yunk4$date),              
                 FUN = sum)
sunk4<-mutate(sunk4, gender ="U")

sall4<-rbind(smal4,sfem4,sunk4)

ggplot(sall4, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberfranken")


a5<-min(Storage9[[5]]$date)
b5<-max(Storage9[[5]]$date)
c5<-seq(as.Date(a5), as.Date(b5), "days")
c5<-as.data.frame(c5)
colnames(c5)[1] <- "date"
y5<-merge(Storage9[[5]],c5, by="date",
          all.x=TRUE, all.y=TRUE)
v5<-y5$cases
index5<-is.na(v5)
v5[index5]<-0
y5$cases<-v5

v5<-y5$gender
index5<-is.na(v5)
v5[index5]<-"egal"
y5$gender<-v5

ymal5<-subset(y5,gender=="M")
yfem5<-subset(y5,gender=="W")
yunk5<-subset(y5,gender=="unbekannt")

smal5<-aggregate(x = ymal5$cases,               
                 by = list(ymal5$date),              
                 FUN = sum)
smal5<-mutate(smal5, gender ="M")

sfem5<-aggregate(x = yfem5$cases,               
                 by = list(yfem5$date),              
                 FUN = sum)
sfem5<-mutate(sfem5, gender ="W")

sunk5<-aggregate(x = yunk5$cases,               
                 by = list(yunk5$date),              
                 FUN = sum)
sunk5<-mutate(sunk5, gender ="U")

sall5<-rbind(smal5,sfem5,sunk5)

ggplot(sall5, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberpfalz")


a6<-min(Storage9[[6]]$date)
b6<-max(Storage9[[6]]$date)
c6<-seq(as.Date(a6), as.Date(b6), "days")
c6<-as.data.frame(c6)
colnames(c6)[1] <- "date"
y6<-merge(Storage9[[6]],c6, by="date",
          all.x=TRUE, all.y=TRUE)
v6<-y6$cases
index6<-is.na(v6)
v6[index6]<-0
y6$cases<-v6

v6<-y6$gender
index6<-is.na(v6)
v6[index6]<-"egal"
y6$gender<-v6

ymal6<-subset(y6,gender=="M")
yfem6<-subset(y6,gender=="W")
yunk6<-subset(y6,gender=="unbekannt")

smal6<-aggregate(x = ymal6$cases,               
                 by = list(ymal6$date),              
                 FUN = sum)
smal6<-mutate(smal6, gender ="M")

sfem6<-aggregate(x = yfem6$cases,               
                 by = list(yfem6$date),              
                 FUN = sum)
sfem6<-mutate(sfem6, gender ="W")

sunk6<-aggregate(x = yunk6$cases,               
                 by = list(yunk6$date),              
                 FUN = sum)
sunk6<-mutate(sunk6, gender ="U")

sall6<-rbind(smal6,sfem6,sunk6)

ggplot(sall6, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Schwaben")



a7<-min(Storage9[[7]]$date)
b7<-max(Storage9[[7]]$date)
c7<-seq(as.Date(a7), as.Date(b7), "days")
c7<-as.data.frame(c7)
colnames(c7)[1] <- "date"
y7<-merge(Storage9[[7]],c7, by="date",
          all.x=TRUE, all.y=TRUE)
v7<-y7$cases
index7<-is.na(v7)
v7[index7]<-0
y7$cases<-v7

v7<-y7$gender
index7<-is.na(v7)
v7[index7]<-"egal"
y7$gender<-v7

ymal7<-subset(y7,gender=="M")
yfem7<-subset(y7,gender=="W")
yunk7<-subset(y7,gender=="unbekannt")

smal7<-aggregate(x = ymal7$cases,               
                 by = list(ymal7$date),              
                 FUN = sum)
smal7<-mutate(smal7, gender ="M")

sfem7<-aggregate(x = yfem7$cases,               
                 by = list(yfem7$date),              
                 FUN = sum)
sfem7<-mutate(sfem7, gender ="W")

sunk7<-aggregate(x = yunk7$cases,               
                 by = list(yunk7$date),              
                 FUN = sum)
sunk7<-mutate(sunk7, gender ="U")

sall7<-rbind(smal7,sfem7,sunk7)

ggplot(sall7, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Unterfranken")


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

Storage2shortfiltered<-Storage2short

for(i in 1:length(Storage2short)){
  Storage2shortfiltered[[i]]<-Storage2short[[i]]%>%filter(gender=="M")%>%filter(age_group=="A00-A04")
}

View(Storage2shortfiltered[[1]])

Storage21filter<-Storage2[[1]]%>%filter(gender=="M")%>%filter(age_group=="A00-A04")

View(Storage21filter)

Storage2filtered<-Storage2

for(i in 1:length(Storage2filtered)){
  Storage2filtered[[i]]<-Storage2[[i]]%>%filter(gender=="M")%>%filter(age_group=="A00-A04")
}

View(Storage2filtered[[2]])

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

plot12<-NA
plot22<-NA
plot32<-NA
plot42<-NA
plot52<-NA
plot62<-NA
plot72<-NA

dbayern3$bezirk<-as.factor(dbayern3$bezirk)
summary(dbayern3$bezirk)
bezirk_names<-c(names(summary(dbayern3$bezirk)))
Storage52<-list()
for(i in 1:length(bezirk_names)){
  Storage52[[i]]<-dbayern3[dbayern3$bezirk==bezirk_names[i],]
}
Storage62<-Storage52
for(i in 1:length(bezirk_names)){
  Storage62[[i]]<-Storage62[[i]]%>%arrange(date)
}
Storage72<-Storage62
for(i in 1:length(bezirk_names)){
  Storage72[[i]]<-Storage72[[i]]%>%arrange(gender)
}
Storage82<-Storage72
for(i in 1:length(bezirk_names)){
  Storage82[[i]]<-Storage82[[i]]%>%arrange(age_group)
}
#View(Storage8[[1]])
Storage92<-Storage82
for(i in 1:length(bezirk_names)){
  Storage92[[i]]<-Storage92[[i]]%>%arrange(district)
}



for(i in 1:length(Storage92)){
  a1<-min(Storage92[[i]]$date)
  b1<-max(Storage92[[i]]$date)
  c1<-seq(as.Date(a1), as.Date(b1), "days")
  c1<-as.data.frame(c1)
  colnames(c1)[1] <- "date"
  y1<-merge(Storage92[[i]],c1, by="date",
            all.x=TRUE, all.y=TRUE)
  v1<-y1$inzidenz
  index1<-is.na(v1)
  v1[index1]<-0
  y1$cases<-v1
  v1<-y1$gender
  index1<-is.na(v1)
  v1[index1]<-"egal"
  y1$gender<-v1
  ymal1<-subset(y1,gender=="M")
  yfem1<-subset(y1,gender=="W")
  yunk1<-subset(y1,gender=="unbekannt")
  smal1<-aggregate(x = ymal1$cases,               
                   by = list(ymal1$date),              
                   FUN = sum)
  smal1<-mutate(smal1, gender ="M")
  sfem1<-aggregate(x = yfem1$cases,               
                   by = list(yfem1$date),              
                   FUN = sum)
  sfem1<-mutate(sfem1, gender ="W")
  sunk1<-aggregate(x = yunk1$cases,               
                   by = list(yunk1$date),              
                   FUN = sum)
  sunk1<-mutate(sunk1, gender ="U")
  sall1<-rbind(smal1,sfem1,sunk1)
  i<-ggplot(sall1, aes(Group.1,x,color = gender)) +
    geom_line(size=0.1)+labs(title="Mittelfranken")
  
  if(Storage92[[i]]$bezirk=="Mittelfranken"){
    plot12<-i
  }else if(Storage92[[i]]$bezirk=="Niederbayern"){
    plot22<-i
  }else if(Storage92[[i]]$bezirk=="Oberbayern"){
    plot32<-i
  }else if(Storage92[[i]]$bezirk=="Oberfranken"){
    plot42<-i
  }else if(Storage92[[i]]$bezirk=="Oberpfalz"){
    plot52<-i
  }else if(Storage92[[i]]$bezirk=="Schwaben"){
    plot62<-i
  }else if(Storage92[[i]]$bezirk=="Unterfranken"){
    plot72<-i
  }
  
}


for(i in 1:length(Storage92)){
  a1<-min(Storage92[[i]]$date)
  b1<-max(Storage92[[i]]$date)
  c1<-seq(as.Date(a1), as.Date(b1), "days")
  c1<-as.data.frame(c1)
  colnames(c1)[1] <- "date"
  y1<-merge(Storage92[[i]],c1, by="date",
            all.x=TRUE, all.y=TRUE)
  v1<-y1$inzidenz
  index1<-is.na(v1)
  v1[index1]<-0
  y1$cases<-v1
  v1<-y1$gender
  index1<-is.na(v1)
  v1[index1]<-"egal"
  y1$gender<-v1
  ymal1<-subset(y1,gender=="M")
  yfem1<-subset(y1,gender=="W")
  yunk1<-subset(y1,gender=="unbekannt")
  smal1<-aggregate(x = ymal1$cases,               
                   by = list(ymal1$date),              
                   FUN = sum)
  smal1<-mutate(smal1, gender ="M")
  sfem1<-aggregate(x = yfem1$cases,               
                   by = list(yfem1$date),              
                   FUN = sum)
  sfem1<-mutate(sfem1, gender ="W")
  sunk1<-aggregate(x = yunk1$cases,               
                   by = list(yunk1$date),              
                   FUN = sum)
  sunk1<-mutate(sunk1, gender ="U")
  sall1<-rbind(smal1,sfem1,sunk1)
  i<-ggplot(sall1, aes(Group.1,x,color = gender)) +
    geom_line(size=0.1)+labs(title="Mittelfranken")
  
  
}

##Impfbayern, popbay
weekly_cases <- dbayern3 %>%
  group_by(date = cut(date, "week"))  %>% summarise(case = sum(cases))

weekly_cases$date <- as.Date(weekly_cases$date)


vector33<-c(unique(dfcombined$district))
Storage01<-list()
for(i in 1:length(vector33)){
  Storage01[[i]]<-dfcombined[dfcombined$district==vector33[i],]
}
View(Storage01[[1]])

for(i in 1:length(Storage01))
Storage01[[i]] <- Storage01[[i]] %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

View(Storage01[[1]])

Storage012<-Storage_new

#Storage012[[1]]<-Storage01[[1]]%>%group_by(date=cut(date,"week"))%>%summarise(total_cases=sum(total_cases))

View(Storage012[[1]])

#library(lubridate)

#View(Storage01[[69]])
  
Storage012[[69]]<-Storage01[[69]]%>%select(date,`M.A00-04`,week)

Storage012[[69]]<-Storage01[[69]]%>%group_by(week)%>%summarise(`M.A00-04`=sum(`M.A00-04`),.groups="keep")

View(Storage012[[69]])





#Storage022<-Storage01
#for(i in 1:length(vector33)){
#  Storage022[[i]]<-Storage01[[i]]%>%group_by(date=unique(cut(dates,"week")))
#}

#View(Storage022[[1]])

#Storage02[[1]]<-unique(Storage02[[1]])

#View(Storage02[[1]])

#Storage03<-list()
#for(i in 1:length(vector33)){
#  Storage03[[i]]<-Storage01[[i]]%>%group_by(date=cut(date,"week"))%>%
#    summarise(M.A00-A04=sum(M.A00-A04),M.A05-A14=sum(M.A05-A14))
#}

dates<-seq(as.Date("2020-01-28"),as.Date("2022-11-25"),by=1)

Storage_new<-list()
for(i in 1:length(vector33)){
  Storage_new[[i]]<-data.frame(matrix(NA,ncol=25,nrow=148))
  colnames(Storage_new[[i]])<-colnames(Storage01[[1]])
}

View(Storage_new[[1]])

for(i in 1:length(vector33)){
  Storage_new[[i]][,2]<-Storage01[[i]][1,2]
  Storage_new[[i]][,3]<-Storage01[[i]][1,3]
}

subscript1<-unique(cut(dates,"week"))
subscript2<-levels(subscript1)


for(i in 1:length(vector33)){
  for(j in 1:nrow(Storage_new))
  Storage_new[[i]][j,4]<-Storage01[[i]][j,4]
}





bezirk_names02<-c(unique(dfcombined$bezirk))
Storage02<-list()
for(i in 1:length(bezirk_names02)){
  Storage02[[i]]<-dfcombined[dfcombined$bezirk==bezirk_names02[i],]
}
View(Storage02[[1]])

#Storage62<-Storage6
#for(i in 1:length(bezirk_names)){
#  Storage62[[i]]<-Storage62[[i]]%>%mutate(inzidenz =  ((lag(cases,6) + lag(cases,5) + 
#                                                lag(cases,4)+ lag(cases,3) +
#                                                lag(cases,2) + lag(cases,1) + 
#                                                cases)/population) * 100000)
#  
#}


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




df4$rate_zweitimpf<-df4$rate_zweitimpf*100
df4$rate_drittimpf<-df4$rate_drittimpf*100
df4$rate_viertimpf<-df4$rate_viertimpf*100
df4$rate_erstimpf<-df4$rate_erstimpf*100
max(df4$rate_zweitimpf)

df4$A00.04.Anteil<-df4$A00.04.Anteil*100
df4$A05.14.Anteil<-df4$A05.14.Anteil*100
df4$A15.34.Anteil<-df4$A15.34.Anteil*100
df4$A35.59.Anteil<-df4$A35.59.Anteil*100
df4$A60.79.Anteil<-df4$A60.79.Anteil*100
df4$A80.Anteil<-df4$A80.Anteil*100

summary(df4$A80.Anteil)





r_squares<-cbind(weighted=weighted_r_squared,squareroot=sqrt_r_squared,pooled=pooled_r_squared)

r_squares


weighted.inzidenz1<-c(sporadisch=2.7668096,erst=0.5550713,zweit=0.1654423,
                      dritt=0.6973943,viert=0.6789099,fuenft=-0.0253710,
                      sechst=0.7982215,siebt=0.5962823)

weighted.wnbinzidenz1<-c(sporadisch=0.1252372,erst=0.1604687,zweit=0.1295247,
                         dritt=0.2010791,viert=0.1239203,fuenft=0.1880349,
                         sechst=0.1696918,siebt=0.2394618)

weighted.densityInzidenz<-c(sporadisch=-0.4511571,erst=0.0048924,zweit=0.0521371,
                            dritt=-0.0010754,viert=0.0066245,fuenft=0.0648796,
                            sechst=-0.0103242,siebt=-0.0208797)

weighted.hotspotInzidenz<-c(sporadisch=7.4650833,erst=1.1136609,zweit=1.7558749,
                            dritt=0.6899014,viert=1.5667665,fuenft=1.8015844,
                            sechst=0.0523044,siebt=0.6132007)

weighted.hotspotnbWnbinzidenz<-c(sporadisch=0.9125414,erst=0.4589981,zweit=0.1744875,
                            dritt=0.2178449,viert=0.0587393,fuenft=0.2929069,
                            sechst=0.1808597,siebt=0.1405323)

weighted.zweitimpfHotspot<-c(sporadisch=NA,erst=NA,zweit=NA,dritt=NA,viert=-66.1471685,
                             fuenft=0.0376960,sechst=0.2212714,siebt=-2.3415206)

weighted.A60.79<-c(sporadisch=0.0156210,erst=0.0199370,zweit=0.0054493,
                   dritt=0.0090236,viert=-0.1665328,fuenft=-0.0023885,
                   sechst=-0.2483869,siebt=-4.2558274)

pooled.inzidenz1<-c(sporadisch=5.3474577,erst=0.6811960,zweit=0.4670898,
                    dritt=0.7174329,viert=0.7029977,fuenft=0.018152,
                    sechst=0.8247824,siebt=0.5815510)

pooled.wnbinzidenz1<-c(sporadisch=0.1286365,erst=0.1625975,zweit=0.0703312,
                       dritt=0.1976777,viert=0.1286938,fuenft=0.155424,
                       sechst=0.1609298,siebt=0.2777451)

pooled.densityInzidenz<-c(sporadisch=-0.9066297,erst=-0.0036743,zweit=0.0250933,
                          dritt=-0.0010176,viert=0.0072035,fuenft=0.065162,
                          sechst=-0.0141249,siebt=-0.0238211)

pooled.hotspotInzidenz<-c(sporadisch=10.1933767,erst=1.0282582,zweit=1.5258893,
                          dritt=0.6533205,viert=1.5249491,fuenft=1.539005,
                          sechst=-0.0372353,siebt=0.5708867)

pooled.hotspotnbWnbinzidenz<-c(sporadisch=0.8150946,erst=0.5087354,zweit=0.3034571,
                               dritt=0.2363805,viert=0.1554143,fuenft=0.277433,
                               sechst=0.1148830,siebt=0.1179980)

pooled.zweitimpfHotspot<-c(sporadisch=NA,erst=NA,zweit=NA,dritt=NA,viert=-69.4829645,
                           fuenft=-0.014969,sechst=0.0487976,siebt=-2.2045901)

pooled.A60.79<-c(sporadisch=0.0188747,erst=0.0156790,zweit=-0.0010253,dritt=0.0648375,
                 viert=-0.2973884,fuenft=-0.021424,sechst=-0.5230795,siebt=-6.0371608)


df.A60.79<-cbind(pooled.A60.79,weighted.A60.79,sqrt.A60.79)

df.zweitimpfHotspot<-cbind(pooled.zweitimpfHotspot,weighted.zweitimpfHotspot,sqrt.zweitimpfHotspot)

df.hotspotnbWnbinzidenz<-cbind(pooled.hotspotnbWnbinzidenz,weighted.hotspotnbWnbinzidenz,sqrt.hotspotnbWnbinzidenz)

df.hotspotInzidenz<-cbind(pooled.hotspotInzidenz,weighted.hotspotInzidenz,sqrt.hotspotInzidenz)

df.densityInzidenz<-cbind(pooled.densityInzidenz,weighted.densityInzidenz,sqrt.densityInzidenz)

df.wnbinzidenz1<-cbind(pooled.wnbinzidenz1,weighted.wnbinzidenz1,sqrt.wnbinzidenz1)
  
df.inzidenz1<-cbind(pooled.inzidenz1,weighted.inzidenz1,sqrt.inzidenz1)



df.A60.79<-cbind(pooled.A60.79,sqrt.A60.79)

df.zweitimpfHotspot<-cbind(pooled.zweitimpfHotspot,sqrt.zweitimpfHotspot)

df.hotspotnbWnbinzidenz<-cbind(pooled.hotspotnbWnbinzidenz,sqrt.hotspotnbWnbinzidenz)

df.hotspotInzidenz<-cbind(pooled.hotspotInzidenz,sqrt.hotspotInzidenz)

df.densityInzidenz<-cbind(pooled.densityInzidenz,sqrt.densityInzidenz)

df.wnbinzidenz1<-cbind(pooled.wnbinzidenz1,sqrt.wnbinzidenz1)

df.inzidenz1<-cbind(pooled.inzidenz1,sqrt.inzidenz1)



sqrt.inzidenz1<-c(model.value=0.64026677,sporadisch=1.7275431,ersteWelle=0.63730532,sommerplateau20=0.3267231,
                  zweiteWelle=0.710850,dritteWelle=0.7176103,sommerplateau21=0.1752151,
                  vierteWelle=0.7799930,fuenfteWelle=0.5799259)
sqrt.inzidenz1<-as.data.frame(sqrt.inzidenz1)
colnames(sqrt.inzidenz1)<-"lag(Inz,1)"

sqrt.wnbinzidenz1<-c(model.value=0.23171750,sporadisch=0.1453111,ersteWelle=0.24599765,sommerplateau20=0.1434222,
                     zweiteWelle=0.23873,dritteWelle=0.12683469,sommerplateau21=0.2693283,
                     vierteWelle=0.1877512,fuenfteWelle=0.2394195)
sqrt.wnbinzidenz1<-as.data.frame(sqrt.wnbinzidenz1)
colnames(sqrt.wnbinzidenz1)<-"sqrt(lag(NB.inz,1))"

sqrt.densityInzidenz<-c(model.value=-0.00584863,sporadisch=-0.2705284,ersteWelle=-0.00020297,sommerplateau20=0.0365050,
                        zweiteWelle=-0.0009883, dritteWelle=0.0042699,sommerplateau21=0.04543828,
                        vierteWelle=-0.0057843,fuenfteWelle=-0.0106490)

sqrt.densityInzidenz<-as.data.frame(sqrt.densityInzidenz)

colnames(sqrt.densityInzidenz)<-"ln(Dichte)*sqrt(lag(Inz,1))"

sqrt.hotspotInzidenz<-c(model.value=0.23092034,sporadisch=3.4252897,ersteWelle=0.34067481,sommerplateau20=0.5744111,
                        zweiteWelle=0.22086662,dritteWelle=0.6528536,sommerplateau21=0.58125082,
                        vierteWelle=0.0843292,fuenfteWelle=0.2095512)
sqrt.hotspotInzidenz<-as.data.frame(sqrt.hotspotInzidenz)
colnames(sqrt.hotspotInzidenz)<-"Hotspot*sqrt(lag(Inz,1))"

sqrt.hotspotnbWnbinzidenz<-c(model.value=0.06928206,sporadisch=0.6256243,ersteWelle=0.13501991,sommerplateau20=0.1689080,
                             zweiteWelle=0.08500171,dritteWelle=0.1263817,sommerplateau21=0.18297279,
                             vierteWelle=0.1053626,fuenfteWelle=0.0491935)
sqrt.hotspotnbWnbinzidenz<-as.data.frame(sqrt.hotspotnbWnbinzidenz)
colnames(sqrt.hotspotnbWnbinzidenz)<-"NB.hotspot*sqrt(lag(NB.inz,1))"

sqrt.zweitimpfHotspot<-c(model.value=-0.03427165,sporadisch=NA,ersteWelle=NA,sommerplateau20=NA,zweiteWelle=NA,dritteWelle=-2.60577324,
                         sommerplateau21= 0.0053421,vierteWelle=0.0120367,fuenfteWelle=-0.0447972)
sqrt.zweitimpfHotspot<-as.data.frame(sqrt.zweitimpfHotspot)
colnames(sqrt.zweitimpfHotspot)<-"Hotspot*Zweitimpfrate"

sqrt.A60.79<-c(model.value=-0.00373027,sporadisch=-0.0163743,ersteWelle=0.00174279,sommerplateau20=-0.0023077,zweiteWelle=0.0000930,
               dritteWelle=-0.00056633,sommerplateau21=-0.0022412,vierteWelle=-0.0067067,fuenfteWelle=-0.0666460)
sqrt.A60.79<-as.data.frame(sqrt.A60.79)
colnames(sqrt.A60.79)<-"Anteil.A60.79"



