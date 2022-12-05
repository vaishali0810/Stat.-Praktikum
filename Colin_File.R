#data <- readRDS("cases_GermanTemporal_2022-10-25.rds")
head(data)
summary(data)


library(ggplot2)
library(tidyr)
library(dplyr)

theme <- theme_classic() +
  theme(text = element_text(size = 12), axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text.y = element_text(size = 12), legend.text.align = 0,
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))


length(data)
vectorstate <- data[, 1]
head(vectorstate)
truth_vector <- rep.int(FALSE, 6285344)

# truth_vector <- vapply(vectorstate, function(i) {data[i, 1] == "Bayern"}, TRUE)
# head(vectorstate)

#data_bayern <- data[truth_vector]
#data_bayern <- data[data$state == "Bayern", ]

getwd()


ggplot(dbayern, aes(y = cases, x = date, color= cases)) + #mapping = aes(x = new_cases)
  geom_line() +
  theme

# facet_wrap(facets = ~variable, scales = "free") + theme

ggplot(data, aes(y = cases, x = date)) + #mapping = aes(x = new_cases)
  geom_line(data$state) +
  theme

# ggplot(dbayern, aes(y = cases, x = date, color= district)) + #mapping = aes(x = new_cases)
#    geom_line() +
#    theme

ggplot(data = data, aes(x = date, y = cases)) +
  geom_line() +
  facet_wrap(facets = vars(state))

## only 4 districts levels are not in Bayern, 3 are not in Bavaria, one is other


test1<-as.vector(summary(dbayern$district))

test2 <- summary(dbayern$district)

# labels(test2)
# dim(test2)
# length(test2)
# names(test2)

test5 <- data.frame(matrix(NA, nrow=length(test2), ncol=2))
test5[,1] <- labels(test2)
test5[,2] <- as.vector(summary(dbayern$district))
test5 <- test5[ -c(97:100), ]
test5

colnames(test5) <- c("district", "Einträge")
test5
plot(test5$district[1:10,], test5$Einträge)

table(dbayern$district, dbayern$cases)













library(dplyr)
library(tidyselect)


dimpf <- read.csv("impfdaten_regional.csv")


table(dimpf$bundesland)
bdimpf1 <- dimpf[dimpf$bundesland == "Freistaat Bayern", ]
table(dimpf$kreis)
summary(bdimpf)
bdimpf

bdimpf <- dimpf %>% select(bundesland == "Freistaat Bayern")
head(bdimpf)
table(dimpf$kreis)

identical(bdimpf, dimpf)

impfBayern <- dimpf[dimpf$bundesland == "Freistaat Bayern", ]
table(impfBayern$kreis)
# impfBayern
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
table(impfBayern$kreis)

#table(dbayern$district)

View(impfBayern)

str(impfBayern)

colnames(impfBayern)[5] <- "district"
colnames(impfBayern)[6] <- "date"
impfBayern[,6] <- as.Date(impfBayern[,6])
colnames(impfBayern)
str(impfBayern)

impfBayern$erstimpftotal <- 0
impfBayern <- impfBayern %>% mutate(erstimpftotal = as.factor(kr))

#districtvector<-unique(impfBayern$district)[1:96]
#districtvector<-names(vector23)
storageimpf<-list()
districtvector <- unique(impfBayern$district)
for(i in 1:length(districtvector)){
  storageimpf[[i]]<-impfBayern[impfBayern$district==districtvector[i],]
}




# add_fun <- function(x) {
#   for (i in seq_along(x$date)) {
#     if (date == "2020-12-27") {
#       x$erstimpftotal[i] <- kr_erstimpf[i]
#     }
#     else {
#       x$erstimpftotal <- x$erstimpftotal[i-1] + x$kr_erstimpf[i]
#     }
#   }
# }

impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)
View(impfungentake)
View(dbayern)
# df3 = merge(df1, df2, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))
dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"))


dbayern <- dbayern %>% arrange(district)
impfBayern <- impfBayern %>% arrange(district)
head(dbayern)

library(tidyverse)
dbayern2 <- dbayern %>% right_join(impfBayern, by = "district")
dbayern <- as_tibble(dbayern)
impfBayern <- as_tibble(impfBayern)


trends <- read.csv("trends.csv", header=TRUE, sep = ",")
View(trends)
popkreise <- read.csv("04-kreise.csv", header = TRUE, sep= ";")
View(popkreise)

library(dplyr)

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
c<-as.data.frame(c)
c<-as.Date(c)
head(datev)
length(datev)

#length(list.m.age)


vector23<-c(summary(dbayern$district)[1:96])
vector23names<-names(vector23)
Storage1000<-list()
for(i in 1:length(vector23names)){
  Storage1000[[i]]<-list.m.age[[1]][list.m.age[[1]]$district==vector23names[i],]
}

colnames(c)[1] <- "date"
d<-merge(c, Storage1000[[1]], by = "date", all.x = TRUE, all.y = TRUE)

is.data.frame(Storage1000[[1]])
is.data.frame(c)
View(c)


testdf <- data.frame(matrix(data=NA, nrow = length(c), ncol = 6))
colnames(testdf) <- c("district", "age_group", "gender", "date", "cases", "bezirk")
testdf[,4] <- c
testdf[,c(1:3,6)] <- Storage1000[[1]][1,c(1:3,6)]
for(j in seq_along(testdf$date)) {
  
}


for(j in 1:length(Storage2)){
  for(i in dates){
    Storage2[[j]][Storage2[[j]]$date==i,16]<-sum(Storage2[[j]][Storage2[[j]]$date==i,11],na.rm=TRUE)
  }
}

View(Storage2[[1]])


f <- Storage2[[1]]%>%filter(gender=="W") %>% filter(age_group=="A00-A04")
for(j in 1:length(f)){
  for(i in dates){
    f[f$date==i,16]<-sum(f[f$date==i,10],na.rm=TRUE)
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
      m0.4[[i]][m0.4[[i]]$date==j,16]<-sum(m0.4[[i]][m0.4[[i]]$date==j,10],na.rm=TRUE)
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
      m5.14[[i]][m5.14[[i]]$date==j,16]<-sum(m5.14[[i]][m5.14[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  m5.14[[i]] <- m5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m5.14[[i]] <- unique(m5.14[[i]])
}
for(i in seq_along(Storage2)) {
  m15.34[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m15.34[[i]][m15.34[[i]]$date==j,16]<-sum(m15.34[[i]][m15.34[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  m15.34[[i]] <- m15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m15.34[[i]] <- unique(m15.34[[i]])
}
for(i in seq_along(Storage2)) {
  m35.59[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m35.59[[i]][m35.59[[i]]$date==j,16]<-sum(m35.59[[i]][m35.59[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  m35.59[[i]] <- m35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m35.59[[i]] <- unique(m35.59[[i]])
}
for(i in seq_along(Storage2)) {
  m60.79[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m60.79[[i]][m60.79[[i]]$date==j,16]<-sum(m60.79[[i]][m60.79[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  m60.79[[i]] <- m60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m60.79[[i]] <- unique(m60.79[[i]])
}
for(i in seq_along(Storage2)) {
  m.80[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m.80[[i]][m.80[[i]]$date==j,16]<-sum(m.80[[i]][m.80[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  m.80[[i]] <- m.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  m.80[[i]] <- unique(m.80[[i]])
}
for(i in seq_along(Storage2)) {
  m.unb[[i]] <- Storage2[[i]]%>%filter(gender=="M") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      m.unb[[i]][m.unb[[i]]$date==j,16]<-sum(m.unb[[i]][m.unb[[i]]$date==j,10],na.rm=TRUE)
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
      f0.4[[i]][f0.4[[i]]$date==j,16]<-sum(f0.4[[i]][f0.4[[i]]$date==j,10],na.rm=TRUE)
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
      f5.14[[i]][f5.14[[i]]$date==j,16]<-sum(f5.14[[i]][f5.14[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  f5.14[[i]] <- f5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f5.14[[i]] <- unique(f5.14[[i]])
}
for(i in seq_along(Storage2)) {
  f15.34[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A15-A34")
  for(j in 1:length(f)){
    for(j in dates){
      f15.34[[i]][f15.34[[i]]$date==j,16]<-sum(f15.34[[i]][f15.34[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  f15.34[[i]] <- f15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f15.34[[i]] <- unique(f15.34[[i]])
}
for(i in seq_along(Storage2)) {
  f35.59[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A35-A59")
  for(j in 1:length(f)){
    for(j in dates){
      f35.59[[i]][f35.59[[i]]$date==j,16]<-sum(f35.59[[i]][f35.59[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  f35.59[[i]] <- f35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f35.59[[i]] <- unique(f35.59[[i]])
}
for(i in seq_along(Storage2)) {
  f60.79[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A60-A79")
  for(j in 1:length(f)){
    for(j in dates){
      f60.79[[i]][f60.79[[i]]$date==j,16]<-sum(f60.79[[i]][f60.79[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  f60.79[[i]] <- f60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f60.79[[i]] <- unique(f60.79[[i]])
}
for(i in seq_along(Storage2)) {
  f.80[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="A80+")
  for(j in 1:length(f)){
    for(j in dates){
      f.80[[i]][f.80[[i]]$date==j,16]<-sum(f.80[[i]][f.80[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  f.80[[i]] <- f.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  f.80[[i]] <- unique(f.80[[i]])
}
for(i in seq_along(Storage2)) {
  f.unb[[i]] <- Storage2[[i]]%>%filter(gender=="W") %>% filter(age_group=="unbekannt")
  for(j in 1:length(f)){
    for(j in dates){
      f.unb[[i]][f.unb[[i]]$date==j,16]<-sum(f.unb[[i]][f.unb[[i]]$date==j,10],na.rm=TRUE)
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
      unb0.4[[i]][unb0.4[[i]]$date==j,16]<-sum(unb0.4[[i]][unb0.4[[i]]$date==j,10],na.rm=TRUE)
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
      unb5.14[[i]][unb5.14[[i]]$date==j,16]<-sum(unb5.14[[i]][unb5.14[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  unb5.14[[i]] <- unb5.14[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb5.14[[i]] <- unique(unb5.14[[i]])
}
for(i in seq_along(Storage2)) {
  unb15.34[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb15.34[[i]][unb15.34[[i]]$date==j,16]<-sum(unb15.34[[i]][unb15.34[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  unb15.34[[i]] <- unb15.34[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb15.34[[i]] <- unique(unb15.34[[i]])
}
for(i in seq_along(Storage2)) {
  unb35.59[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb35.59[[i]][unb35.59[[i]]$date==j,16]<-sum(unb35.59[[i]][unb35.59[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  unb35.59[[i]] <- unb35.59[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb35.59[[i]] <- unique(unb35.59[[i]])
}
for(i in seq_along(Storage2)) {
  unb60.79[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb60.79[[i]][unb60.79[[i]]$date==j,16]<-sum(unb60.79[[i]][unb60.79[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  unb60.79[[i]] <- unb60.79[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb60.79[[i]] <- unique(unb60.79[[i]])
}
for(i in seq_along(Storage2)) {
  unb.80[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb.80[[i]][unb.80[[i]]$date==j,16]<-sum(unb.80[[i]][unb.80[[i]]$date==j,10],na.rm=TRUE)
    }
  }
  unb.80[[i]] <- unb.80[[i]] %>% select(district, date, bezirk, cases_on_date, age_group, gender)
  unb.80[[i]] <- unique(unb.80[[i]])
}
for(i in seq_along(Storage2)) {
  unb.unb[[i]] <- Storage2[[i]]%>%filter(gender=="unbekannt") %>% filter(age_group=="A05-A14")
  for(j in 1:length(f)){
    for(j in dates){
      unb.unb[[i]][unb.unb[[i]]$date==j,16]<-sum(unb.unb[[i]][unb.unb[[i]]$date==j,10],na.rm=TRUE)
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

