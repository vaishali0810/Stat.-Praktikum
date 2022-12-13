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

Storage2<-Storage
for(i in 1:length(vector23names)){
  Storage2[[i]]<-Storage2[[i]]%>%arrange(date)
}

#dates<-seq(as.Date("2020-01-28"),as.Date("2022-11-25"),by=1)

for(j in 1:length(Storage2)){
  for(i in dates){
    Storage2[[j]][Storage2[[j]]$date==i,16]<-sum(Storage2[[j]][Storage2[[j]]$date==i,11],na.rm=TRUE)
  }
}




dbayern$gender <- as.factor(dbayern$gender)
dbayernshort <- dbayern %>% select(district, age_group, gender, date, cases, bezirk)
list <- list()
levelsgender <- levels(dbayernshort$gender)

for (i in seq_along(levelsgender)) {
  list[[i]] <- dbayernshort[dbayernshort$gender == levelsgender[i], ]
}

listm <- list[[1]]
listna <- list[[2]]
listw <- list[[3]]

list.m.age <- list()
list.na.age <- list()
list.w.age <- list()

levelsage <- levels(dbayernshort$age_group)

for (i in seq_along(levelsage)) {
  list.m.age[[i]] <- list[[1]][list[[1]]$age_group == levelsage[i], ]
}

for (i in seq_along(levelsage)) {
  list.na.age[[i]] <- list[[2]][list[[2]]$age_group == levelsage[i], ]
}

for (i in seq_along(levelsage)) {
  list.w.age[[i]] <- list[[3]][list[[3]]$age_group == levelsage[i], ]
}

#datevector <- c(min(dbayern$date):max(dbayern$date))



a<-min(dbayern$date)
b<-max(dbayern$date)
datev<-seq(as.Date(a), as.Date(b), "days")
datev<-as.Date(datev)
datev<-as.data.frame(datev)
head(datev)
length(datev)

#length(list.m.age)


vector23<-c(summary(dbayern$district)[1:96])
vector23names<-names(vector23)
Storage1000<-list()
for(i in 1:length(vector23names)){
  Storage1000[[i]]<-list.m.age[[1]][list.m.age[[1]]$district==vector23names[i],]
}

colnames(datev)[1] <- "date"
d<-merge(datev, Storage1000[[1]], by= "date", all.x = TRUE, all.y = TRUE)

is.data.frame(Storage1000[[1]])
is.data.frame(datev)
View(datev)


testdf <- data.frame(matrix(data=NA, nrow = nrow(datev), ncol = 6))
colnames(testdf) <- c("district", "age_group", "gender", "date", "cases", "bezirk")
testdf[,4] <- datev
testdf[,c(1:3,6)] <- Storage1000[[1]][1,c(1:3,6)]


# for(j in seq_along(testdf$date)) {
#   
# }


# for(j in 1:length(Storage2)){
#   for(i in dates){
#     Storage2[[j]][Storage2[[j]]$date==i,16]<-sum(Storage2[[j]][Storage2[[j]]$date==i,11],na.rm=TRUE)
#   }
# }

#View(Storage2[[1]])
dates <- as.vector(datev[,1])

f <- Storage2[[1]]%>%filter(gender=="W") %>% filter(age_group=="A00-A04")
for(j in 1:length(f)){
  for(i in dates){
    f[f$date==i,16]<-sum(f[f$date==i,11],na.rm=TRUE)
  }
}
f <- f %>% select(district, date, bezirk, cases_on_date, age_group, gender)
f <- unique(f)



## Split data based on gender and age group


m0.4 <- list()
m5.14 <- list()
m15.34 <- list()
m35.59 <- list()
m60.79 <- list()
m.80 <- list()
m.unb <- list()
for(i in seq_along(Storage2)) {
  m0.4[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A00-A04")
  for(j in 1:length(f)){
    for(j in dates){
      m0.4[[i]][m0.4[[i]]$date==j,16]<-sum(m0.4[[i]][m0.4[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m0.4[[i]] <- m0.4[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m0.4[[i]] <- unique(m0.4[[i]])
  
}
#View(m0.4)

for(i in seq_along(Storage2)) {
  m5.14[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m5.14[[i]][m5.14[[i]]$date==j,16]<-sum(m5.14[[i]][m5.14[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m5.14[[i]] <- m5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m5.14[[i]] <- unique(m5.14[[i]])
}
for(i in seq_along(Storage2)) {
  m15.34[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m15.34[[i]][m15.34[[i]]$date==j,16]<-sum(m15.34[[i]][m15.34[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m15.34[[i]] <- m15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m15.34[[i]] <- unique(m15.34[[i]])
}
for(i in seq_along(Storage2)) {
  m35.59[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m35.59[[i]][m35.59[[i]]$date==j,16]<-sum(m35.59[[i]][m35.59[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m35.59[[i]] <- m35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m35.59[[i]] <- unique(m35.59[[i]])
}
for(i in seq_along(Storage2)) {
  m60.79[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m60.79[[i]][m60.79[[i]]$date==j,16]<-sum(m60.79[[i]][m60.79[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m60.79[[i]] <- m60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m60.79[[i]] <- unique(m60.79[[i]])
}
for(i in seq_along(Storage2)) {
  m.80[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m.80[[i]][m.80[[i]]$date==j,16]<-sum(m.80[[i]][m.80[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m.80[[i]] <- m.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m.80[[i]] <- unique(m.80[[i]])
}
for(i in seq_along(Storage2)) {
  m.unb[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m.unb[[i]][m.unb[[i]]$date==j,16]<-sum(m.unb[[i]][m.unb[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  m.unb[[i]] <- m.unb[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m.unb[[i]] <- unique(m.unb[[i]])
}


f0.4 <- list()
f5.14 <- list()
f15.34 <- list()
f35.59 <- list()
f60.79 <- list()
f.80 <- list()
f.unb <- list()
for(i in seq_along(Storage2)) {
  f0.4[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A00-A04")
  for(j in 1:length(f)){
    for(j in dates){
      f0.4[[i]][f0.4[[i]]$date==j,16]<-sum(f0.4[[i]][f0.4[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f0.4[[i]] <- f0.4[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f0.4[[i]] <- unique(f0.4[[i]])
  
}
#View(f0.4)

for(i in seq_along(Storage2)) {
  f5.14[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      f5.14[[i]][f5.14[[i]]$date==j,16]<-sum(f5.14[[i]][f5.14[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f5.14[[i]] <- f5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f5.14[[i]] <- unique(f5.14[[i]])
}
for(i in seq_along(Storage2)) {
  f15.34[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A15-A34")
  for(j in 1:length(f)){
    for(j in dates){
      f15.34[[i]][f15.34[[i]]$date==j,16]<-sum(f15.34[[i]][f15.34[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f15.34[[i]] <- f15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f15.34[[i]] <- unique(f15.34[[i]])
}
for(i in seq_along(Storage2)) {
  f35.59[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A35-A59")
  for(j in 1:length(f)){
    for(j in dates){
      f35.59[[i]][f35.59[[i]]$date==j,16]<-sum(f35.59[[i]][f35.59[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f35.59[[i]] <- f35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f35.59[[i]] <- unique(f35.59[[i]])
}
for(i in seq_along(Storage2)) {
  f60.79[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A60-A79")
  for(j in 1:length(f)){
    for(j in dates){
      f60.79[[i]][f60.79[[i]]$date==j,16]<-sum(f60.79[[i]][f60.79[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f60.79[[i]] <- f60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f60.79[[i]] <- unique(f60.79[[i]])
}
for(i in seq_along(Storage2)) {
  f.80[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A80+")
  for(j in 1:length(f)){
    for(j in dates){
      f.80[[i]][f.80[[i]]$date==j,16]<-sum(f.80[[i]][f.80[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f.80[[i]] <- f.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f.80[[i]] <- unique(f.80[[i]])
}
for(i in seq_along(Storage2)) {
  f.unb[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="unbekannt")
  for(j in 1:length(f)){
    for(j in dates){
      f.unb[[i]][f.unb[[i]]$date==j,16]<-sum(f.unb[[i]][f.unb[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  f.unb[[i]] <- f.unb[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f.unb[[i]] <- unique(f.unb[[i]])
}

unb0.4 <- list()
unb5.14 <- list()
unb15.34 <- list()
unb35.59 <- list()
unb60.79 <- list()
unb.80 <- list()
unb.unb <- list()
for(i in seq_along(Storage2)) {
  unb0.4[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A00-A04")
  for(j in 1:length(f)){
    for(j in dates){
      unb0.4[[i]][unb0.4[[i]]$date==j,16]<-sum(unb0.4[[i]][unb0.4[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb0.4[[i]] <- unb0.4[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb0.4[[i]] <- unique(unb0.4[[i]])
  
}
#View(unb0.4)

for(i in seq_along(Storage2)) {
  unb5.14[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb5.14[[i]][unb5.14[[i]]$date==j,16]<-sum(unb5.14[[i]][unb5.14[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb5.14[[i]] <- unb5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb5.14[[i]] <- unique(unb5.14[[i]])
}
for(i in seq_along(Storage2)) {
  unb15.34[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb15.34[[i]][unb15.34[[i]]$date==j,16]<-sum(unb15.34[[i]][unb15.34[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb15.34[[i]] <- unb15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb15.34[[i]] <- unique(unb15.34[[i]])
}
for(i in seq_along(Storage2)) {
  unb35.59[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb35.59[[i]][unb35.59[[i]]$date==j,16]<-sum(unb35.59[[i]][unb35.59[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb35.59[[i]] <- unb35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb35.59[[i]] <- unique(unb35.59[[i]])
}
for(i in seq_along(Storage2)) {
  unb60.79[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb60.79[[i]][unb60.79[[i]]$date==j,16]<-sum(unb60.79[[i]][unb60.79[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb60.79[[i]] <- unb60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb60.79[[i]] <- unique(unb60.79[[i]])
}
for(i in seq_along(Storage2)) {
  unb.80[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb.80[[i]][unb.80[[i]]$date==j,16]<-sum(unb.80[[i]][unb.80[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb.80[[i]] <- unb.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb.80[[i]] <- unique(unb.80[[i]])
}
for(i in seq_along(Storage2)) {
  unb.unb[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb.unb[[i]][unb.unb[[i]]$date==j,16]<-sum(unb.unb[[i]][unb.unb[[i]]$date==j,11],na.rm=TRUE)
    }
  }
  unb.unb[[i]] <- unb.unb[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb.unb[[i]] <- unique(unb.unb[[i]])
}

View(m0.4[[1]])
# m0.4
# m5.14 
# m15.34
# m35.59 
# m60.79 
# m.80 
# m.unb 
# f0.4 
# f5.14 
# f15.34 
# f35.59 
# f60.79 
# f.80 
# f.unb 
# unb0.4 
# unb5.14 
# unb15.34 
# unb35.59
# unb60.79 
# unb.80 
# unb.unb 
# length(datev)
# 
# datev <- as.data.frame(datev)
# colnames(datev) <- "date"
# 
# 
# datev[,1] <- as.Date(datev[,1])
# test999 <- as.data.frame(m0.4[[1]])
# test999[,2] <- as.Date(test999[,2])
# 
# test1000 <- merge(y = test999, x = datev,  all.x = TRUE, all.y = TRUE)
# test1000[is.na(test1000)] <- 0
# test1000[,2] <- test999[1,1]
# test1000[,5] <- test999[1,5]
# test1000[,3] <- test999[1,3]
# test1000[,6] <- test999[1,6]



for(i in seq_along(m0.4)) {
  temp1 <- m0.4[[i]][1,1]
  temp2 <- m0.4[[i]][1,5]
  temp3 <- m0.4[[i]][1,3]
  temp4 <- m0.4[[i]][1,6]
  m0.4[[i]] <- merge(y = m0.4[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m0.4[[i]][is.na(m0.4[[i]])] <- 0
  m0.4[[i]][,2] <- temp1
  m0.4[[i]][,5] <- temp2
  m0.4[[i]][,3] <- temp3
  m0.4[[i]][,6] <- temp4
}

for(i in seq_along(m5.14)) {
  temp1 <- m5.14[[i]][1,1]
  temp2 <- m5.14[[i]][1,5]
  temp3 <- m5.14[[i]][1,3]
  temp4 <- m5.14[[i]][1,6]
  m5.14[[i]] <- merge(y = m5.14[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m5.14[[i]][is.na(m5.14[[i]])] <- 0
  m5.14[[i]][,2] <- temp1
  m5.14[[i]][,5] <- temp2
  m5.14[[i]][,3] <- temp3
  m5.14[[i]][,6] <- temp4
}

for(i in seq_along(m15.34)) {
  temp1 <- m15.34[[i]][1,1]
  temp2 <- m15.34[[i]][1,5]
  temp3 <- m15.34[[i]][1,3]
  temp4 <- m15.34[[i]][1,6]
  m15.34[[i]] <- merge(y = m15.34[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m15.34[[i]][is.na(m15.34[[i]])] <- 0
  m15.34[[i]][,2] <- temp1
  m15.34[[i]][,5] <- temp2
  m15.34[[i]][,3] <- temp3
  m15.34[[i]][,6] <- temp4
}


for(i in seq_along(m35.59)) {
  temp1 <- m35.59[[i]][1,1]
  temp2 <- m35.59[[i]][1,5]
  temp3 <- m35.59[[i]][1,3]
  temp4 <- m35.59[[i]][1,6]
  m35.59[[i]] <- merge(y = m35.59[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m35.59[[i]][is.na(m35.59[[i]])] <- 0
  m35.59[[i]][,2] <- temp1
  m35.59[[i]][,5] <- temp2
  m35.59[[i]][,3] <- temp3
  m35.59[[i]][,6] <- temp4
}

for(i in seq_along(m60.79)) {
  temp1 <- m60.79[[i]][1,1]
  temp2 <- m60.79[[i]][1,5]
  temp3 <- m60.79[[i]][1,3]
  temp4 <- m60.79[[i]][1,6]
  m60.79[[i]] <- merge(y = m60.79[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m60.79[[i]][is.na(m60.79[[i]])] <- 0
  m60.79[[i]][,2] <- temp1
  m60.79[[i]][,5] <- temp2
  m60.79[[i]][,3] <- temp3
  m60.79[[i]][,6] <- temp4
}


for(i in seq_along(m.80)) {
  temp1 <- m.80[[i]][1,1]
  temp2 <- m.80[[i]][1,5]
  temp3 <- m.80[[i]][1,3]
  temp4 <- m.80[[i]][1,6]
  m.80[[i]] <- merge(y = m.80[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m.80[[i]][is.na(m.80[[i]])] <- 0
  m.80[[i]][,2] <- temp1
  m.80[[i]][,5] <- temp2
  m.80[[i]][,3] <- temp3
  m.80[[i]][,6] <- temp4
}

for(i in seq_along(m.unb)) {
  temp1 <- m.unb[[i]][1,1]
  temp2 <- m.unb[[i]][1,5]
  temp3 <- m.unb[[i]][1,3]
  temp4 <- m.unb[[i]][1,6]
  m.unb[[i]] <- merge(y = m.unb[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  m.unb[[i]][is.na(m.unb[[i]])] <- 0
  m.unb[[i]][,2] <- temp1
  m.unb[[i]][,5] <- temp2
  m.unb[[i]][,3] <- temp3
  m.unb[[i]][,6] <- temp4
}

for(i in seq_along(f0.4)) {
  temp1 <- f0.4[[i]][1,1]
  temp2 <- f0.4[[i]][1,5]
  temp3 <- f0.4[[i]][1,3]
  temp4 <- f0.4[[i]][1,6]
  f0.4[[i]] <- merge(y = f0.4[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f0.4[[i]][is.na(f0.4[[i]])] <- 0
  f0.4[[i]][,2] <- temp1
  f0.4[[i]][,5] <- temp2
  f0.4[[i]][,3] <- temp3
  f0.4[[i]][,6] <- temp4
}

for(i in seq_along(f5.14)) {
  temp1 <- f5.14[[i]][1,1]
  temp2 <- f5.14[[i]][1,5]
  temp3 <- f5.14[[i]][1,3]
  temp4 <- f5.14[[i]][1,6]
  f5.14[[i]] <- merge(y = f5.14[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f5.14[[i]][is.na(f5.14[[i]])] <- 0
  f5.14[[i]][,2] <- temp1
  f5.14[[i]][,5] <- temp2
  f5.14[[i]][,3] <- temp3
  f5.14[[i]][,6] <- temp4
}

for(i in seq_along(f15.34)) {
  temp1 <- f15.34[[i]][1,1]
  temp2 <- f15.34[[i]][1,5]
  temp3 <- f15.34[[i]][1,3]
  temp4 <- f15.34[[i]][1,6]
  f15.34[[i]] <- merge(y = f15.34[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f15.34[[i]][is.na(f15.34[[i]])] <- 0
  f15.34[[i]][,2] <- temp1
  f15.34[[i]][,5] <- temp2
  f15.34[[i]][,3] <- temp3
  f15.34[[i]][,6] <- temp4
}


for(i in seq_along(f35.59)) {
  temp1 <- f35.59[[i]][1,1]
  temp2 <- f35.59[[i]][1,5]
  temp3 <- f35.59[[i]][1,3]
  temp4 <- f35.59[[i]][1,6]
  f35.59[[i]] <- merge(y = f35.59[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f35.59[[i]][is.na(f35.59[[i]])] <- 0
  f35.59[[i]][,2] <- temp1
  f35.59[[i]][,5] <- temp2
  f35.59[[i]][,3] <- temp3
  f35.59[[i]][,6] <- temp4
}

for(i in seq_along(f60.79)) {
  temp1 <- f60.79[[i]][1,1]
  temp2 <- f60.79[[i]][1,5]
  temp3 <- f60.79[[i]][1,3]
  temp4 <- f60.79[[i]][1,6]
  f60.79[[i]] <- merge(y = f60.79[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f60.79[[i]][is.na(f60.79[[i]])] <- 0
  f60.79[[i]][,2] <- temp1
  f60.79[[i]][,5] <- temp2
  f60.79[[i]][,3] <- temp3
  f60.79[[i]][,6] <- temp4
}


for(i in seq_along(f.80)) {
  temp1 <- f.80[[i]][1,1]
  temp2 <- f.80[[i]][1,5]
  temp3 <- f.80[[i]][1,3]
  temp4 <- f.80[[i]][1,6]
  f.80[[i]] <- merge(y = f.80[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f.80[[i]][is.na(f.80[[i]])] <- 0
  f.80[[i]][,2] <- temp1
  f.80[[i]][,5] <- temp2
  f.80[[i]][,3] <- temp3
  f.80[[i]][,6] <- temp4
}

for(i in seq_along(f.unb)) {
  temp1 <- f.unb[[i]][1,1]
  temp2 <- f.unb[[i]][1,5]
  temp3 <- f.unb[[i]][1,3]
  temp4 <- f.unb[[i]][1,6]
  f.unb[[i]] <- merge(y = f.unb[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  f.unb[[i]][is.na(f.unb[[i]])] <- 0
  f.unb[[i]][,2] <- temp1
  f.unb[[i]][,5] <- temp2
  f.unb[[i]][,3] <- temp3
  f.unb[[i]][,6] <- temp4
}
for(i in seq_along(unb0.4)) {
  temp1 <- unb0.4[[i]][1,1]
  temp2 <- unb0.4[[i]][1,5]
  temp3 <- unb0.4[[i]][1,3]
  temp4 <- unb0.4[[i]][1,6]
  unb0.4[[i]] <- merge(y = unb0.4[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb0.4[[i]][is.na(unb0.4[[i]])] <- 0
  unb0.4[[i]][,2] <- temp1
  unb0.4[[i]][,5] <- temp2
  unb0.4[[i]][,3] <- temp3
  unb0.4[[i]][,6] <- temp4
}

for(i in seq_along(unb5.14)) {
  temp1 <- unb5.14[[i]][1,1]
  temp2 <- unb5.14[[i]][1,5]
  temp3 <- unb5.14[[i]][1,3]
  temp4 <- unb5.14[[i]][1,6]
  unb5.14[[i]] <- merge(y = unb5.14[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb5.14[[i]][is.na(unb5.14[[i]])] <- 0
  unb5.14[[i]][,2] <- temp1
  unb5.14[[i]][,5] <- temp2
  unb5.14[[i]][,3] <- temp3
  unb5.14[[i]][,6] <- temp4
}

for(i in seq_along(unb15.34)) {
  temp1 <- unb15.34[[i]][1,1]
  temp2 <- unb15.34[[i]][1,5]
  temp3 <- unb15.34[[i]][1,3]
  temp4 <- unb15.34[[i]][1,6]
  unb15.34[[i]] <- merge(y = unb15.34[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb15.34[[i]][is.na(unb15.34[[i]])] <- 0
  unb15.34[[i]][,2] <- temp1
  unb15.34[[i]][,5] <- temp2
  unb15.34[[i]][,3] <- temp3
  unb15.34[[i]][,6] <- temp4
}


for(i in seq_along(unb35.59)) {
  temp1 <- unb35.59[[i]][1,1]
  temp2 <- unb35.59[[i]][1,5]
  temp3 <- unb35.59[[i]][1,3]
  temp4 <- unb35.59[[i]][1,6]
  unb35.59[[i]] <- merge(y = unb35.59[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb35.59[[i]][is.na(unb35.59[[i]])] <- 0
  unb35.59[[i]][,2] <- temp1
  unb35.59[[i]][,5] <- temp2
  unb35.59[[i]][,3] <- temp3
  unb35.59[[i]][,6] <- temp4
}

for(i in seq_along(unb60.79)) {
  temp1 <- unb60.79[[i]][1,1]
  temp2 <- unb60.79[[i]][1,5]
  temp3 <- unb60.79[[i]][1,3]
  temp4 <- unb60.79[[i]][1,6]
  unb60.79[[i]] <- merge(y = unb60.79[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb60.79[[i]][is.na(unb60.79[[i]])] <- 0
  unb60.79[[i]][,2] <- temp1
  unb60.79[[i]][,5] <- temp2
  unb60.79[[i]][,3] <- temp3
  unb60.79[[i]][,6] <- temp4
}


for(i in seq_along(unb.80)) {
  temp1 <- unb.80[[i]][1,1]
  temp2 <- unb.80[[i]][1,5]
  temp3 <- unb.80[[i]][1,3]
  temp4 <- unb.80[[i]][1,6]
  unb.80[[i]] <- merge(y = unb.80[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb.80[[i]][is.na(unb.80[[i]])] <- 0
  unb.80[[i]][,2] <- temp1
  unb.80[[i]][,5] <- temp2
  unb.80[[i]][,3] <- temp3
  unb.80[[i]][,6] <- temp4
}

for(i in seq_along(unb.unb)) {
  temp1 <- unb.unb[[i]][1,1]
  temp2 <- unb.unb[[i]][1,5]
  temp3 <- unb.unb[[i]][1,3]
  temp4 <- unb.unb[[i]][1,6]
  unb.unb[[i]] <- merge(y = unb.unb[[i]], x = datev,  all.x = TRUE, all.y = TRUE)
  unb.unb[[i]][is.na(unb.unb[[i]])] <- 0
  unb.unb[[i]][,2] <- temp1
  unb.unb[[i]][,5] <- temp2
  unb.unb[[i]][,3] <- temp3
  unb.unb[[i]][,6] <- temp4
}

length(m0.4)
m0.4 <- m0.4[c(1:96)]
m5.14 <- m5.14[c(1:96)]
m15.34 <- m15.34[c(1:96)]
m35.59 <- m35.59[c(1:96)]
m60.79 <- m60.79[c(1:96)]
m.80 <- m.80[c(1:96)]
m.unb <- m.unb[c(1:96)]
f0.4 <- f0.4[c(1:96)]
f5.14 <- f5.14[c(1:96)]
f15.34 <- f15.34[c(1:96)]
f35.59 <- f35.59[c(1:96)]
f60.79 <- f60.79[c(1:96)]
f.80 <- f.80[c(1:96)]
f.unb <- f.unb[c(1:96)]
unb0.4 <- unb0.4[c(1:96)]
unb5.14 <- unb5.14[c(1:96)]
unb15.34 <- unb15.34[c(1:96)]
unb35.59 <- unb35.59[c(1:96)]
unb60.79 <- unb60.79[c(1:96)]
unb.80 <- unb.80[c(1:96)]
unb.unb <- unb.unb[c(1:96)]

df <- as.data.frame(do.call(rbind, m0.4))
colnames(df)[4] <- "M.A00-04"
df <- df[,c(-5, -6)]
head(df)
dim(df)
dfnew <- as.data.frame(do.call(rbind, m5.14))
df$new <- dfnew[,4]
colnames(df)[5] <- "M.A05-14"
dfnew <- as.data.frame(do.call(rbind, m15.34))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[6] <- "M.A15-34"
dfnew <- as.data.frame(do.call(rbind, m35.59))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[7] <- "M.A35-59"
dfnew <- as.data.frame(do.call(rbind, m60.79))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[8] <- "M.A60-79"
dfnew <- as.data.frame(do.call(rbind, m.80))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[9] <- "M.A80+"
dfnew <- as.data.frame(do.call(rbind, m.unb))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[10] <- "M.Aunb"


dfnew <- as.data.frame(do.call(rbind, f0.4))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[11] <- "F.A00-04"
dfnew <- as.data.frame(do.call(rbind, f5.14))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[12] <- "F.A05.14"
dfnew <- as.data.frame(do.call(rbind, f15.34))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[13] <- "F.A15-34"
dfnew <- as.data.frame(do.call(rbind, f35.59))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[14] <- "F.A35-59"
dfnew <- as.data.frame(do.call(rbind, f60.79))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[15] <- "F.A60-79"
dfnew <- as.data.frame(do.call(rbind, f.80))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[16] <- "F.A80+"
dfnew <- as.data.frame(do.call(rbind, f.unb))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[17] <- "F.Aunb"

dfnew <- as.data.frame(do.call(rbind, unb0.4))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[18] <- "Unb.A00-04"
dfnew <- as.data.frame(do.call(rbind, unb5.14))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[19] <- "Unb.A05.14"
dfnew <- as.data.frame(do.call(rbind, unb15.34))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[20] <- "Unb.A15-34"
dfnew <- as.data.frame(do.call(rbind, unb35.59))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[21] <- "Unb.A35-59"
dfnew <- as.data.frame(do.call(rbind, unb60.79))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[22] <- "Unb.A60-79"
dfnew <- as.data.frame(do.call(rbind, unb.80))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[23] <- "Unb.A80+"
dfnew <- as.data.frame(do.call(rbind, unb.unb))
df <- df %>% mutate(new = dfnew[,4])
colnames(df)[24] <- "Unb.Aunb"
df$total_cases <- 0
df[,25] <- 0

for(i in 1:nrow(df)){
  df[i,25] <- sum(df[i,4:24])
}


write.csv(df, "/Users/colinlinke/Documents/ProgR/Stat.-Praktikum/dfcombined.csv", row.names=FALSE)


test100000 <- read.csv("dfcombined.csv", head=TRUE, sep=",")
