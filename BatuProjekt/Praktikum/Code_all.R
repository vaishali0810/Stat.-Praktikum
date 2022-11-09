# Benötigte Packete: ggplot, dplyr, reshape2, tidyr, plyr, patchwork
# das Package "plyr" wird an benötigter Stelle eingeladen und danach wieder entladen,
# um im Großteil des Codes keine Spezifizierungen zwischen "dplyr" und "plyr" machen zu müssen
# falls Arbeitspeicherproblem beim Einladen bestimmter Variablen auftreten, kann das Global Environment 
# mit dem Befehl: "rm(list = ls(all.names = TRUE))" geleert werden.

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(patchwork)

### Datensatz einladen

data <- readRDS("210727_dat_ftf.rds")

levels(data$JS_HUR_Antrittsmonat)

### soziodemographische Variablen 

sozio <- select(data,
                id,
                travel_year,
                S_Geschlecht,
                S_Bundesland,
                S_Alter,
                S_FamStand,
                S_Bildung,
                S_Beruf,
                S_PKW,
                S_Kinder_0_bis_5_binaer,
                S_Kinder_6_bis_13_binaer,
                S_Kinder_14_bis_17_binaer,
                S_Haushaltsgroesse,
                S_Einkommen_HH,
                S_Einkommen,
                JS_HUR_Antrittsmonat)


## Jahreszeiten filtern
# Sommer
sozio_summer <- sozio %>% 
  filter(JS_HUR_Antrittsmonat == c("Juni","Juli","August","September"))

# Winter
sozio_winter <- sozio %>%
  filter(JS_HUR_Antrittsmonat == c("Dezember","Januar","Februar","Maerz"))


# Zeitrahmen filtern (2009-2018)

recent_summer <- sozio_summer %>% filter(travel_year > 2008)
recent_winter <- sozio_winter %>% filter(travel_year > 2008)

# Anzahl Anfangsmonate aufsummieren und in Winter- und Sommermonate einteilen

wintermonate <- data %>% filter(JS_HUR_Antrittsmonat == "Dezember"|
                                  JS_HUR_Antrittsmonat == "Januar"|
                                  JS_HUR_Antrittsmonat == "Februar"|
                                  JS_HUR_Antrittsmonat == "Maerz")
wintermonate <- wintermonate %>% 
  group_by(JS_HUR_Antrittsmonat) %>% 
  summarise(JS_HUR_Antrittsmonat,n = n())

wintermonate <- wintermonate %>% distinct(JS_HUR_Antrittsmonat, n, .keep_all = TRUE)
wintermonate <- wintermonate %>% mutate(Jahreszeit = "Winter")


sommermonate <- data %>% filter(JS_HUR_Antrittsmonat == "Juni"|
                                  JS_HUR_Antrittsmonat == "Juli"|
                                  JS_HUR_Antrittsmonat == "August"|
                                  JS_HUR_Antrittsmonat == "September")
sommermonate <- sommermonate %>% 
  group_by(JS_HUR_Antrittsmonat) %>% 
  summarise(JS_HUR_Antrittsmonat, n = n())

sommermonate <- sommermonate %>% distinct(JS_HUR_Antrittsmonat, n, .keep_all = TRUE)
sommermonate <- sommermonate %>% mutate(Jahreszeit = "Sommer")

sonstige <- data %>% filter(JS_HUR_Antrittsmonat == "April"|
                              JS_HUR_Antrittsmonat == "Mai"|
                              JS_HUR_Antrittsmonat == "Oktober"|
                              JS_HUR_Antrittsmonat == "November")
sonstige <- sonstige %>% 
  group_by(JS_HUR_Antrittsmonat) %>% 
  summarise(JS_HUR_Antrittsmonat, n = n())

sonstige <- sonstige %>% distinct(JS_HUR_Antrittsmonat, n, .keep_all = TRUE)
sonstige <- sonstige %>% mutate(Jahreszeit = "Sonstige")
all_monate <- rbind(sommermonate, wintermonate, sonstige)
all_monate <- all_monate[c(5,6,7,9,10,1,2,3,4,11,12,8),]

monate <- as.factor(c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
all_monate[,1] <- monate
all_monate$JS_HUR_Antrittsmonat <- factor(all_monate$JS_HUR_Antrittsmonat, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))

# Ergebniss plotten

ggplot(all_monate, aes(x = JS_HUR_Antrittsmonat, y = n, fill = Jahreszeit)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Antrittsmonat") +
  ylab("Anzahl") +
  scale_fill_manual(values = c("Sommer" = "#F8766D",
                               "Winter" = "#00BFC4",
                               "Sonstige" = "grey")) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.25, hjust = 0.8)) +
  annotate(geom = "text", x = 10, y = 40000, label = "n = 164.328", color ="#F8766D") +
  annotate(geom = "text", x = 2, y = 10000, label = "n = 18.427", color = "#00BFC4")
  


# "Geschlechter" nach Sommer und Winter trennen

genders_summer <- group_by(recent_summer, S_Geschlecht)
by_gender_summer <- summarise(genders_summer, count = n())

genders_winter <- group_by(recent_winter, S_Geschlecht)
by_gender_winter <- summarise(genders_winter, count = n())

by_gender_winter <- by_gender_winter %>% mutate(anteil = by_gender_winter$count/sum(by_gender_winter$count))
by_gender_summer <- by_gender_summer %>% mutate(anteil = by_gender_summer$count/sum(by_gender_summer$count))

by_gender_summer <- by_gender_summer %>% mutate(season = "Sommer")
by_gender_winter <- by_gender_winter %>% mutate(season = "Winter")

all_genders <- rbind(by_gender_summer, by_gender_winter)
char_genders <- c("männlich", "weiblich", "männlich", "weiblich")
all_genders[,1] <- char_genders

# Ergebniss plotten
ggplot(all_genders, aes(x = season, y = anteil, fill = S_Geschlecht)) + 
  geom_bar(stat = "identity", position = "dodge")

# da binäre Variablen -> nur Anteil Frauen abbilden
all_women <- all_genders %>% distinct(season, anteil, .keep_all = TRUE)

all_women <- subset(all_genders, S_Geschlecht != "männlich")

# Ergebniss plotten
ggplot(all_women, aes(x = season, y = anteil, fill = season)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("weiblicher Anteile in %") +
  guides(fill = guide_legend(title = "Jahreszeit")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6))+ 
  scale_fill_manual("Jahreszeit", values = c("Sommer" = "#F8766D", "Winter" = "#00BFC4"))


### "Bildung" nach Sommer und Winter trennen

edu_summer <- group_by(recent_summer, S_Bildung)
by_edu_summer <- summarise(edu_summer, count = n())

edu_winter <- group_by(recent_winter, S_Bildung)
by_edu_winter <- summarise(edu_winter, count = n())

by_edu_summer <- by_edu_summer %>% mutate(anteil = by_edu_summer$count/sum(by_edu_summer$count))
by_edu_winter <- by_edu_winter %>% mutate(anteil = by_edu_winter$count/sum(by_edu_winter$count))

by_edu_summer <- by_edu_summer %>% mutate(season = "Sommer")
by_edu_winter <- by_edu_winter %>% mutate(season = "Winter")

all_edus <- rbind(by_edu_summer, by_edu_winter)
edunames <- as_tibble(c("Hauptschule", "Realschule", "Abitur", "Universität","Hauptschule", "Realschule", "Abitur", "Universität" ))
all_edus[,1] <- edunames 

# Ergebniss plotten

ggplot(all_edus, aes(x = season, y = anteil, fill =  S_Bildung)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("Anteile in %") +
  guides(fill = guide_legend(title = "Bildungsabschluss")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 0.6)) +
  scale_fill_brewer(palette = "Purples")


### "Anzahl PKWs" nach Sommer und Winter trennen

cars_summer <- group_by(recent_summer, S_PKW)
by_cars_summer <- summarise(cars_summer, count = n())

cars_winter <- group_by(recent_winter, S_PKW)
by_cars_winter <- summarise(cars_winter, count = n())

by_cars_summer <- by_cars_summer %>% mutate(season = "Sommer")
by_cars_winter <- by_cars_winter %>% mutate(season = "Winter")

by_cars_summer <- by_cars_summer %>% drop_na(S_PKW)
by_cars_winter <- by_cars_winter %>% drop_na(S_PKW)

by_cars_summer <- by_cars_summer %>% mutate(anteil = by_cars_summer$count/sum(by_cars_summer$count))
by_cars_winter <- by_cars_winter %>% mutate(anteil = by_cars_winter$count/sum(by_cars_winter$count))

all_cars <- rbind(by_cars_summer,by_cars_winter)

# Ergebniss plotten:

ggplot(all_cars, aes(x = season, y = anteil, fill = S_PKW)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("Anteile in %") +
  guides(fill = guide_legend(title = "Anzahl PKWs")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_fill_brewer(palette = "Purples")


# "Familienstand" nach Sommer und Winter trennen

fam_stand_summer <- group_by(recent_summer, S_FamStand)
by_fam_summer <- summarise(fam_stand_summer, count = n())

fam_stand_winter <- group_by(recent_winter, S_FamStand)
by_fam_winter <- summarise(fam_stand_winter, count = n())

by_fam_summer <- by_fam_summer %>% mutate(season = "Sommer")
by_fam_winter <- by_fam_winter %>% mutate(season = "Winter")

by_fam_summer <- by_fam_summer %>% mutate(anteil = by_fam_summer$count/sum(by_fam_summer$count))
by_fam_winter <- by_fam_winter %>% mutate(anteil = by_fam_winter$count/sum(by_fam_winter$count))

all_fam <- rbind(by_fam_summer,by_fam_winter)


# Ergebniss plotten

ggplot(all_fam, aes(x = season, y = anteil, fill = S_FamStand)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("Anteile in %") +
  guides(fill = guide_legend(title = "Familienstand")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_fill_brewer(palette = "Purples")


# "Berufen" nach Sommer und Winter trennen

beruf_summer <- group_by(recent_summer, S_Beruf)
by_beruf_summer <- summarise(beruf_summer, count = n())

beruf_winter <- group_by(recent_winter, S_Beruf)
by_beruf_winter <- summarise(beruf_winter, count = n())

by_beruf_summer <- by_beruf_summer %>% mutate(season = "Sommer")
by_beruf_winter <- by_beruf_winter %>% mutate(season = "Winter")

by_beruf_summer <- by_beruf_summer %>% mutate(anteil = by_beruf_summer$count/sum(by_beruf_summer$count))
by_beruf_winter <- by_beruf_winter %>% mutate(anteil = by_beruf_winter$count/sum(by_beruf_winter$count))

all_berufe <- rbind(by_beruf_summer, by_beruf_winter)

berufnamen <- as_tibble(c("Arbeiter", "Angestellter", "Beamter", "Selbständiger", "ohne Beruf", "in Schulausbildung","Arbeiter", "Angestellter", "Beamter", "Selbständiger", "ohne Beruf", "in Schulausbildung"))

all_berufe[,1] <- berufnamen

# Ergebniss plotten:
ggplot(all_berufe, aes(x = season, y = anteil, fill = S_Beruf)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("Anteile in %") +
  guides(fill = guide_legend(title = "Berufe")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_fill_brewer(palette = "Purples")


# "Haushaltsgröße" nach Sommer und Winter trennen

hsize_summer <- group_by(recent_summer, S_Haushaltsgroesse)
by_size_summer <- summarise(hsize_summer, count = n())

hsize_winter <- group_by(recent_winter, S_Haushaltsgroesse)
by_size_winter <- summarise(hsize_winter, count = n())

by_size_summer <- by_size_summer %>% 
  mutate(anteil = by_size_summer$count/sum(by_size_summer$count))

by_size_winter <- by_size_winter %>% 
  mutate(anteil = by_size_winter$count/sum(by_size_winter$count))


by_size_summer <- by_size_summer %>% mutate(season = "Sommer")
by_size_winter <- by_size_winter %>% mutate(season = "Winter")

all_hsizes <- rbind(by_size_summer, by_size_winter)

# Ergebniss plotten
ggplot(all_hsizes, aes(x = season, y = anteil, fill = S_Haushaltsgroesse)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Jahreszeit") +
  ylab("Anteile in %") +
  guides(fill = guide_legend(title = "Haushaltsgrößen")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_fill_brewer(palette = "Purples")


# Altersgruppen erstellen und nach Sommer und Winter trennen,
# dann für das Vergleichen normieren

age_groupS <- sozio_summer %>% mutate(age_group = dplyr::case_when(
  S_Alter > 0 & S_Alter <= 20 ~ "(14 - 20]",
  S_Alter > 20 & S_Alter <= 30 ~ "(20 - 30]",
  S_Alter > 30 & S_Alter <= 40 ~ "(30 - 40]",
  S_Alter > 40 & S_Alter <= 50 ~ "(40 - 50]",
  S_Alter > 50 & S_Alter <= 60 ~ "(50 - 60]", 
  S_Alter > 60 & S_Alter <= 70 ~ "(60 - 70]",
  S_Alter > 70 & S_Alter <= 80 ~ "(70 - 80]",
  S_Alter > 80 ~ "(80+)"
))


age_groupW <- sozio_winter %>% mutate(age_group = dplyr::case_when(
  S_Alter > 0 & S_Alter <= 20 ~ "(14 - 20]",
  S_Alter > 20 & S_Alter <= 30 ~ "(20 - 30]",
  S_Alter > 30 & S_Alter <= 40 ~ "(30 - 40]",
  S_Alter > 40 & S_Alter <= 50 ~ "(40 - 50]",
  S_Alter > 50 & S_Alter <= 60 ~ "(50 - 60]", 
  S_Alter > 60 & S_Alter <= 70 ~ "(60 - 70]",
  S_Alter > 70 & S_Alter <= 80 ~ "(70 - 80]",
  S_Alter > 80 ~ "(80+)"
))


age_summer <- age_groupS %>% 
  group_by(age_group) %>% 
  summarise(age_group, n = n())

age_summer <- age_summer %>% mutate(Jahreszeit = "Sommer")

age_winter <- age_groupW %>% 
  group_by(age_group) %>% 
  summarise(age_group, n = n())

age_winter <- age_winter %>% mutate(Jahreszeit = "Winter")

age_winter123 <- age_winter %>% distinct(age_group, n, Jahreszeit, .keep_all = TRUE)

teilerW <- c(4655, 4655, 4655, 4655, 4655, 4655, 4655, 4655, 4655)
anteilW <- age_winter123$n/teilerW

age_winter123 <- cbind(age_winter123, anteilW)
colnames(age_winter123) <- c("Altersgruppe", "Anzahl", "Jahreszeit", "Anteil")

age_summer123 <- age_summer %>% distinct(age_group, n, Jahreszeit, .keep_all = TRUE)

teilerS <- c(41145,41145,41145,41145,41145,41145,41145,41145,41145)
anteilS <- age_summer123$n/teilerS

age_summer123 <- cbind(age_summer123, anteilS)
colnames(age_summer123) <- c("Altersgruppe", "Anzahl", "Jahreszeit", "Anteil")

all_ages <- rbind(age_summer123, age_winter123)

all_ages <- all_ages %>% drop_na(Altersgruppe)

# Ergebniss plotten
ggplot(all_ages, aes(x = Jahreszeit, y = Anteil, fill = Altersgruppe)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.25)) +
  scale_fill_brewer(palette = "RdYlGn")





##### PKW Anzahl per Haushaltsgroeße pro Jahr berechnen

psize_time <- sozio_summer %>% 
  group_by(travel_year,S_PKW) %>% 
  summarise(S_PKW, n=n())

psize_time <- psize_time[psize_time$S_PKW != "keine Angabe",]
psize_time_summer <- psize_time %>% drop_na(S_PKW)

############# In Sommer und Winter Haushalte aufteilen

psize_time <- sozio_winter %>% 
  group_by(travel_year,S_PKW) %>% 
  summarise(S_PKW, n=n())

psize_time <- psize_time[psize_time$S_PKW != "keine Angabe",]
psize_time_winter <- psize_time %>% drop_na(S_PKW)

psize_time_summer<-psize_time_summer%>%mutate(season="summer")
psize_time_winter<-psize_time_winter%>%mutate(season="winter")

psize_time_all<-rbind(psize_time_summer,psize_time_winter)


###### Anzahl der PKW pro Haushaltsgroeße für jedes Jahr berechnen

########### Anzahl der 0 PKW Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-psize_time_summer%>%filter(S_PKW=="keinen PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Ein<-rename(temp_a,summer=x)

temp_b<-psize_time_winter%>%filter(S_PKW=="keinen PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Ein<-cbind(Ein,winter)


##### normieren

temp_a<-Ein$summer/(Ein$summer+Ein$winter)
temp_b<-Ein$winter/(Ein$summer+Ein$winter)

Ein$summer<-temp_a
Ein$winter<-temp_b

############# Anzahl der 1 PKW Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-psize_time_summer%>%filter(S_PKW=="einen PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Zwei<-rename(temp_a,summer=x)

temp_b<-psize_time_winter%>%filter(S_PKW=="einen PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Zwei<-cbind(Zwei,winter)

##### normieren
temp_a<-Zwei$summer/(Zwei$summer+Zwei$winter)
temp_b<-Zwei$winter/(Zwei$summer+Zwei$winter)

Zwei$summer<-temp_a
Zwei$winter<-temp_b

############# Anzahl der 2 PKW Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-psize_time_summer%>%filter(S_PKW=="zwei PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Drei<-rename(temp_a,summer=x)

temp_b<-psize_time_winter%>%filter(S_PKW=="zwei PKW")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Drei<-cbind(Drei,winter)

###### normieren
temp_a<-Drei$summer/(Drei$summer+Drei$winter)
temp_b<-Drei$winter/(Drei$summer+Drei$winter)

Drei$summer<-temp_a
Drei$winter<-temp_b

############# Anzahl der 3 PKW Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-psize_time_summer%>%filter(S_PKW=="drei PKW und mehr")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Vier<-rename(temp_a,summer=x)

temp_b<-psize_time_winter%>%filter(S_PKW=="drei PKW und mehr")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)

###### Zeilen streichen wo nicht vergleichbar ist

Vier<-Vier[Vier$Group.1 !=1971,]
Vier<-Vier[Vier$Group.1 !=1972,]
Vier<-Vier[Vier$Group.1 !=1973,]
Vier<-Vier[Vier$Group.1 !=1975,]
Vier<-Vier[Vier$Group.1 !=1978,]
Vier<-Vier[Vier$Group.1 !=1979,]
Vier<-Vier[Vier$Group.1 !=1981,]
Vier<-Vier[Vier$Group.1 !=2015,]

winter<-temp_a$x
winter<-as.data.frame(winter)

######## normieren

Vier<-cbind(Vier,winter)

temp_a<-Vier$summer/(Vier$summer+Vier$winter)
temp_b<-Vier$winter/(Vier$summer+Vier$winter)

Vier$summer<-temp_a
Vier$winter<-temp_b


################### Alle Haushalte zusammenfassen

######### für sommer
temp_a<-Ein[,-c(3)]
temp_a<-temp_a%>%mutate(S_PKW="keinen PKW",season="summer")
temp_a<-rename(temp_a,n=summer)

temp_b<-Zwei[,-c(3)]
temp_b<-temp_b%>%mutate(S_PKW="einen PKW",season="summer")
temp_b<-rename(temp_b,n=summer)

temp_c<-Drei[,-c(3)]
temp_c<-temp_c%>%mutate(S_PKW="zwei PKW",season="summer")
temp_c<-rename(temp_c,n=summer)

temp_d<-Vier[,-c(3)]
temp_d<-temp_d%>%mutate(S_PKW="drei PKW und mehr",season="summer")
temp_d<-rename(temp_d,n=summer)

temp_f<-rbind(temp_a,temp_b,temp_c,temp_d)

############ für winter
temp_a<-Ein[,-c(2)]
temp_a<-temp_a%>%mutate(S_PKW="keinen PKW",season="winter")
temp_a<-rename(temp_a,n=winter)

temp_b<-Zwei[,-c(2)]
temp_b<-temp_b%>%mutate(S_PKW="einen PKW",season="winter")
temp_b<-rename(temp_b,n=winter)

temp_c<-Drei[,-c(2)]
temp_c<-temp_c%>%mutate(S_PKW="zwei PKW",season="winter")
temp_c<-rename(temp_c,n=winter)

temp_d<-Vier[,-c(2)]
temp_d<-temp_d%>%mutate(S_PKW="drei PKW und mehr",season="winter")
temp_d<-rename(temp_d,n=winter)


temp_g<-rbind(temp_a,temp_b,temp_c,temp_d)


##### winter und sommer zusammenführen

psize_time_all2<-rbind(temp_g,temp_f)

psize_time_all2<-rename(psize_time_all2,travel_year=Group.1)



######## Jahreszeiten bearbeiten
psize_time_all2<-psize_time_all2%>%filter(season=="winter")

psize_time_all2<-rename(psize_time_all2,Jahreszeit=season)


psize_time_all2$Jahreszeit<-as.factor(psize_time_all2$Jahreszeit)
levels(psize_time_all2$Jahreszeit)[levels(psize_time_all2$Jahreszeit)=="winter"]<-"Winter"

psize_time_all2$S_PKW<-factor(psize_time_all2$S_PKW,
                              levels=c("keinen PKW","einen PKW","zwei PKW","drei PKW und mehr"))

########### plotten
psize_time_all2%>%
  ggplot(aes(travel_year,n,colour=Jahreszeit))+
  geom_line(size=1)+
  facet_wrap(~S_PKW)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  ylab("Anteil")+
  xlab("Reisejahr")+
  scale_color_discrete(name="Jahreszeit",labels=c("sommer","winter"))+
  scale_color_manual(values = c("#00BDC4"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3))+
  geom_area(fill="lightblue")




########### Anzahl der Herkunft per Bundesl?ndern pro Jahr berechnen
bsize_time <- sozio_summer %>% 
  group_by(travel_year,S_Bundesland) %>% 
  summarise(S_Bundesland, n=n())

bsize_time <- bsize_time[bsize_time$S_Bundesland != "keine Angabe",]
bsize_time_summer <- bsize_time %>% drop_na(S_Bundesland)

bsize_time <- sozio_winter %>% 
  group_by(travel_year,S_Bundesland) %>% 
  summarise(S_Bundesland, n=n())

bsize_time <- bsize_time[bsize_time$S_Bundesland != "keine Angabe",]
bsize_time_winter <- bsize_time %>% drop_na(S_Bundesland)

bsize_time_summer<-bsize_time_summer%>%mutate(season="summer")
bsize_time_winter<-bsize_time_winter%>%mutate(season="winter")

bsize_time_all<-rbind(bsize_time_summer,bsize_time_winter)


######## In alte und neue Bundesl?nder aufteilen

alte_bund<-subset(bsize_time_all,
                  bsize_time_all$S_Bundesland=="SchleswigHolstein"|
                    bsize_time_all$S_Bundesland=="Hamburg"|
                    bsize_time_all$S_Bundesland=="Niedersachsen"|
                    bsize_time_all$S_Bundesland=="Bremen"|
                    bsize_time_all$S_Bundesland=="NordrheinWestfalen"|
                    bsize_time_all$S_Bundesland=="Hessen"|
                    bsize_time_all$S_Bundesland=="RheinlandPflaz"|
                    bsize_time_all$S_Bundesland=="BadenWuerttemberg"|
                    bsize_time_all$S_Bundesland=="Bayern"|
                    bsize_time_all$S_Bundesland=="Saarland"|
                    bsize_time_all$S_Bundesland=="Berlin")

alte_bund<-alte_bund%>%mutate(S_Bundesland="alt")


neue_bund<-subset(bsize_time_all,
                  bsize_time_all$S_Bundesland=="Brandenburg"|
                    bsize_time_all$S_Bundesland=="MeclenburgVorpommern"|
                    bsize_time_all$S_Bundesland=="Sachsen"|
                    bsize_time_all$S_Bundesland=="SachsenAnhalt"|
                    bsize_time_all$S_Bundesland=="Thueringen")

neue_bund<-neue_bund%>%mutate(S_Bundesland="neu")


######## Anzahl derjenigen die aus den alten Bundesl?nder kommen pro Jahr rechnen

alte_bund_s<-alte_bund%>%filter(season=="summer")
alte_bund_w<-alte_bund%>%filter(season=="winter")

alt_s<-aggregate(x=alte_bund_s$n,by=list(alte_bund_s$travel_year),FUN=mean)
alt_s<-rename(alt_s,summer=x)

alt_w<-aggregate(x=alte_bund_w$n,by=list(alte_bund_w$travel_year),FUN=mean)
alt_w<-rename(alt_w,winter=x)

winter<-alt_w$winter
winter<-as.data.frame(winter)

alt<-cbind(alt_s,winter)

###### normieren

temp_a<-alt$summer/(alt$summer+alt$winter)
temp_b<-alt$winter/(alt$summer+alt$winter)

alt$summer<-temp_a
alt$winter<-temp_b
################## Anzahl derjenigen die aus den neuen Bundesl?nder kommen pro Jahr rechnen

neue_bund_s<-neue_bund%>%filter(season=="summer")
neue_bund_w<-neue_bund%>%filter(season=="winter")

neu_s<-aggregate(x=neue_bund_s$n,by=list(neue_bund_s$travel_year),FUN=mean)
neu_s<-rename(neu_s,summer=x)

neu_w<-aggregate(x=neue_bund_w$n,by=list(neue_bund_w$travel_year),FUN=mean)
neu_w<-rename(neu_w,winter=x)

winter<-neu_w$winter
winter<-as.data.frame(winter)

neu<-cbind(neu_s,winter)

####### normieren

temp_a<-neu$summer/(neu$summer+neu$winter)
temp_b<-neu$winter/(neu$summer+neu$winter)

neu$summer<-temp_a
neu$winter<-temp_b


###### neue und alte Bundesl?nder im Sommer zusammenf?hren


temp_a<-alt[,-c(3)]
temp_a<-temp_a%>%mutate(S_Bundesland="alt",season="summer")
temp_a<-rename(temp_a,n=summer)

temp_b<-neu[,-c(3)]
temp_b<-temp_b%>%mutate(S_Bundesland="neu",season="summer")
temp_b<-rename(temp_b,n=summer)

temp_c<-rbind(temp_a,temp_b)

####### neue und alte Bundesl?nder im Winter zusammenf?hren

temp_a<-alt[,-c(2)]
temp_a<-temp_a%>%mutate(S_Bundesland="alt",season="winter")
temp_a<-rename(temp_a,n=winter)

temp_b<-neu[,-c(2)]
temp_b<-temp_b%>%mutate(S_Bundesland="neu",season="winter")
temp_b<-rename(temp_b,n=winter)

temp_d<-rbind(temp_a,temp_b)


######## Sommer und Winter zusammenf?hren

bsize_time_all2<-rbind(temp_c,temp_d)


######## Datensatz reinigen, Zeilennamen ?ndern, allgemeine Formfeler ?ndern 

bsize_time_all2<-rename(bsize_time_all2,travel_year=Group.1)

bsize_time_all3<-bsize_time_all2%>%filter(travel_year>1990)

bsize_time_all3<-bsize_time_all3%>%filter(season=="winter")

bsize_time_all3<-rename(bsize_time_all3,Jahreszeit=season)

bsize_time_all3$Jahreszeit<-as.factor(bsize_time_all3$Jahreszeit)
levels(bsize_time_all3$Jahreszeit)[levels(bsize_time_all3$Jahreszeit)=="winter"]<-"Winter"

######## plotten

bsize_time_all3%>%
  ggplot(aes(travel_year,n,colour=Jahreszeit))+
  geom_line(size=1)+
  facet_wrap(~S_Bundesland)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  ylab("Anteil")+
  xlab("Reisejahr")+
  scale_color_discrete(name="Jahreszeit",labels=c("winter"))+
  scale_color_manual(values = c("#00BDC4"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3))+
  geom_area(fill="lightblue")





###### Anzahl der Haushalte per Haushaltsgröße pro Jahr berechnen

hsize_time <- sozio_summer %>% 
  group_by(travel_year,S_Haushaltsgroesse) %>% 
  summarise(S_Haushaltsgroesse, n=n())
hsize_time <- hsize_time[hsize_time$S_Haushaltsgroesse != "keine Angabe",]
hsize_time_summer <- hsize_time %>% drop_na(S_Haushaltsgroesse)

hsize_time <- sozio_winter %>% 
  group_by(travel_year,S_Haushaltsgroesse) %>% 
  summarise(S_Haushaltsgroesse, n=n())
hsize_time <- hsize_time[hsize_time$S_Haushaltsgroesse != "keine Angabe",]
hsize_time_winter <- hsize_time %>% drop_na(S_Haushaltsgroesse)

hsize_time_summer<-hsize_time_summer%>%mutate(season="summer")
hsize_time_winter<-hsize_time_winter%>%mutate(season="winter")

hsize_time_all<-rbind(hsize_time_summer,hsize_time_winter)



########### Anzahl der 1 Personen Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-hsize_time_summer%>%filter(S_Haushaltsgroesse=="1 Person")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Ein<-rename(temp_a,summer=x)

temp_b<-hsize_time_winter%>%filter(S_Haushaltsgroesse=="1 Person")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Ein<-cbind(Ein,winter)

####### normieren

temp_a<-Ein$summer/(Ein$summer+Ein$winter)
temp_b<-Ein$winter/(Ein$summer+Ein$winter)

Ein$summer<-temp_a
Ein$winter<-temp_b

############# Anzahl der 2 Personen Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-hsize_time_summer%>%filter(S_Haushaltsgroesse=="2 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Zwei<-rename(temp_a,summer=x)

temp_b<-hsize_time_winter%>%filter(S_Haushaltsgroesse=="2 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Zwei<-cbind(Zwei,winter)

#### normieren

temp_a<-Zwei$summer/(Zwei$summer+Zwei$winter)
temp_b<-Zwei$winter/(Zwei$summer+Zwei$winter)

Zwei$summer<-temp_a
Zwei$winter<-temp_b

############# Anzahl der 3 Personen Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-hsize_time_summer%>%filter(S_Haushaltsgroesse=="3 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Drei<-rename(temp_a,summer=x)

temp_b<-hsize_time_winter%>%filter(S_Haushaltsgroesse=="3 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Drei<-cbind(Drei,winter)

###### normieren

temp_a<-Drei$summer/(Drei$summer+Drei$winter)
temp_b<-Drei$winter/(Drei$summer+Drei$winter)

Drei$summer<-temp_a
Drei$winter<-temp_b

############# Anzahl der 4 Personen Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-hsize_time_summer%>%filter(S_Haushaltsgroesse=="4 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Vier<-rename(temp_a,summer=x)

temp_b<-hsize_time_winter%>%filter(S_Haushaltsgroesse=="4 Personen")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)
Vier<-cbind(Vier,winter)

#####normieren

temp_a<-Vier$summer/(Vier$summer+Vier$winter)
temp_b<-Vier$winter/(Vier$summer+Vier$winter)

Vier$summer<-temp_a
Vier$winter<-temp_b

############# Anzahl der 5 Personen Haushalte pro Jahr aufgeteilt nach Sommer Winter
temp_b<-hsize_time_summer%>%filter(S_Haushaltsgroesse=="5 Personen und mehr")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
Funf<-rename(temp_a,summer=x)

temp_b<-hsize_time_winter%>%filter(S_Haushaltsgroesse=="5 Personen und mehr")
temp_a<-aggregate(x=temp_b$n,by=list(temp_b$travel_year),FUN=mean)
winter<-temp_a$x
winter<-as.data.frame(winter)

####### nicht vergleichbare Spalten auslassen

Funf<-Funf[Funf$Group.1 !=1977,]
Funf<-Funf[Funf$Group.1 !=1988,]
Funf<-Funf[Funf$Group.1 !=1979,]
Funf<-Funf[Funf$Group.1 !=2011,]

Funf<-cbind(Funf,winter)

###### normieren

temp_a<-Funf$summer/(Funf$summer+Funf$winter)
temp_b<-Funf$winter/(Funf$summer+Funf$winter)

Funf$summer<-temp_a
Funf$winter<-temp_b




################### großen Datensatz erstellen
######### sommer
temp_a<-Ein[,-c(3)]
temp_a<-temp_a%>%mutate(S_Haushaltsgroesse="1 Person",season="summer")
temp_a<-rename(temp_a,n=summer)

temp_b<-Zwei[,-c(3)]
temp_b<-temp_b%>%mutate(S_Haushaltsgroesse="2 Personen",season="summer")
temp_b<-rename(temp_b,n=summer)

temp_c<-Drei[,-c(3)]
temp_c<-temp_c%>%mutate(S_Haushaltsgroesse="3 Personen",season="summer")
temp_c<-rename(temp_c,n=summer)

temp_d<-Vier[,-c(3)]
temp_d<-temp_d%>%mutate(S_Haushaltsgroesse="4 Personen",season="summer")
temp_d<-rename(temp_d,n=summer)

temp_e<-Funf[,-c(3)]
temp_e<-temp_e%>%mutate(S_Haushaltsgroesse="5 Personen und mehr",season="summer")
temp_e<-rename(temp_e,n=summer)

temp_f<-rbind(temp_a,temp_b,temp_c,temp_d,temp_e)

############ winter
temp_a<-Ein[,-c(2)]
temp_a<-temp_a%>%mutate(S_Haushaltsgroesse="1 Person",season="winter")
temp_a<-rename(temp_a,n=winter)

temp_b<-Zwei[,-c(2)]
temp_b<-temp_b%>%mutate(S_Haushaltsgroesse="2 Personen",season="winter")
temp_b<-rename(temp_b,n=winter)

temp_c<-Drei[,-c(2)]
temp_c<-temp_c%>%mutate(S_Haushaltsgroesse="3 Personen",season="winter")
temp_c<-rename(temp_c,n=winter)

temp_d<-Vier[,-c(2)]
temp_d<-temp_d%>%mutate(S_Haushaltsgroesse="4 Personen",season="winter")
temp_d<-rename(temp_d,n=winter)

temp_e<-Funf[,-c(2)]
temp_e<-temp_e%>%mutate(S_Haushaltsgroesse="5 Personen und mehr",season="winter")
temp_e<-rename(temp_e,n=winter)

temp_g<-rbind(temp_a,temp_b,temp_c,temp_d,temp_e)



#####zusammenführen von Sommer und Winter

hsize_time_all2<-rbind(temp_g,temp_f)

hsize_time_all2<-rename(hsize_time_all2,travel_year=Group.1)

####### Jahreszeiten anpassen

hsize_time_all2<-hsize_time_all2%>%filter(season=="winter")

hsize_time_all2<-rename(hsize_time_all2,Jahreszeit=season)

hsize_time_all2$Jahreszeit<-as.factor(hsize_time_all2$Jahreszeit)
levels(hsize_time_all2$Jahreszeit)[levels(hsize_time_all2$Jahreszeit)=="winter"]<-"Winter"


########### plotten

hsize_time_all2%>%
  ggplot(aes(travel_year,n,colour=Jahreszeit))+
  geom_line(size=1)+
  facet_wrap(~S_Haushaltsgroesse)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  ylab("Anteil")+
  xlab("Reisejahr")+
  scale_color_discrete(name="Jahreszeit",labels=c("sommer","winter"))+
  scale_color_manual(values = c("#00BDC4"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3))+
  geom_area(fill="lightblue")





#Datensatz in Sommer und Winter einteilen:

f<-subset(data,data$JS_HUR_Antrittsmonat=="Dezember")
g<-subset(data,data$JS_HUR_Antrittsmonat=="Januar")
h<-subset(data,data$JS_HUR_Antrittsmonat=="Februar")
i<-subset(data,data$JS_HUR_Antrittsmonat=="Maerz")
winter<-rbind(f,g,h,i)

j<-subset(data,data$JS_HUR_Antrittsmonat=="Juni")
k<-subset(data,data$JS_HUR_Antrittsmonat=="Juli")
l<-subset(data,data$JS_HUR_Antrittsmonat=="August")
m<-subset(data,data$JS_HUR_Antrittsmonat=="September")
sommer<-rbind(j,k,l,m)



sommer_Varia<-select(sommer,travel_year,JS_HUR_Reiseziel,
                     RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                     RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                     RM_OB5_Familie_HUR,
                     RM_OB7_Aktiv_HUR)

winter_Varia<-select(winter,travel_year,JS_HUR_Reiseziel,
                     RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                     RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                     RM_OB5_Familie_HUR,
                     RM_OB7_Aktiv_HUR)

sommer_Varia<-sommer_Varia%>%filter(travel_year<1992)

winter_Varia<-winter_Varia%>%filter(travel_year<1992)


##### 10 häufigsten Ziele filtern

RZErf_sommer <- filter(sommer_Varia,JS_HUR_Reiseziel == "ESP" |
                         JS_HUR_Reiseziel == "ITA" |
                         JS_HUR_Reiseziel == "AUT" |
                         JS_HUR_Reiseziel == "FRA" |
                         JS_HUR_Reiseziel == "GRC" |
                         JS_HUR_Reiseziel == "CHE" |
                         JS_HUR_Reiseziel == "USA" |
                         JS_HUR_Reiseziel == "TUR" |
                         JS_HUR_Reiseziel == "SOAN" |
                         JS_HUR_Reiseziel == "KRIK")

RZErf_winter<- filter(winter_Varia,JS_HUR_Reiseziel == "ESP" |
                        JS_HUR_Reiseziel == "ITA" |
                        JS_HUR_Reiseziel == "AUT" |
                        JS_HUR_Reiseziel == "FRA" |
                        JS_HUR_Reiseziel == "GRC" |
                        JS_HUR_Reiseziel == "CHE" |
                        JS_HUR_Reiseziel == "USA" |
                        JS_HUR_Reiseziel == "TUR" |
                        JS_HUR_Reiseziel == "SOAN" |
                        JS_HUR_Reiseziel == "KRIK")


RZErf_sommer<-na.omit(RZErf_sommer)

RZErf_winter<-na.omit(RZErf_winter)


######## Durchschnitte der Sommer Motive berechnen

Lander_M1<-aggregate(x=RZErf_sommer$RM_OB1_Entspannen_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
Lander_M1<-rename(Lander_M1,RM1=x)


Lander_M2<-aggregate(x=RZErf_sommer$RM_OB2_Genuss_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
RM2<-Lander_M2$x
RM2<-as.data.frame(RM2)


Lander_M3<-aggregate(x=RZErf_sommer$RM_OB3_Neues_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
RM3<-Lander_M3$x
RM3<-as.data.frame(RM3)


Lander_M4<-aggregate(x=RZErf_sommer$RM_OB4_Natur_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
RM4<-Lander_M4$x
RM4<-as.data.frame(RM4)


Lander_M5<-aggregate(x=RZErf_sommer$RM_OB5_Familie_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
RM5<-Lander_M5$x
RM5<-as.data.frame(RM5)


Lander_M7<-aggregate(x=RZErf_sommer$RM_OB7_Aktiv_HUR,
                     by=list(RZErf_sommer$JS_HUR_Reiseziel),FUN=mean)
RM7<-Lander_M7$x
RM7<-as.data.frame(RM7)

s_RMRZ<-cbind(Lander_M1,RM2,RM3,RM4,RM5,RM7)

######## Durschschnitte der Winter Motive berechnen

Lander_M1<-aggregate(x=RZErf_winter$RM_OB1_Entspannen_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
Lander_M1<-rename(Lander_M1,RM1=x)


Lander_M2<-aggregate(x=RZErf_winter$RM_OB2_Genuss_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
RM2<-Lander_M2$x
RM2<-as.data.frame(RM2)


Lander_M3<-aggregate(x=RZErf_winter$RM_OB3_Neues_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
RM3<-Lander_M3$x
RM3<-as.data.frame(RM3)


Lander_M4<-aggregate(x=RZErf_winter$RM_OB4_Natur_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
RM4<-Lander_M4$x
RM4<-as.data.frame(RM4)


Lander_M5<-aggregate(x=RZErf_winter$RM_OB5_Familie_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
RM5<-Lander_M5$x
RM5<-as.data.frame(RM5)


Lander_M7<-aggregate(x=RZErf_winter$RM_OB7_Aktiv_HUR,
                     by=list(RZErf_winter$JS_HUR_Reiseziel),FUN=mean)
RM7<-Lander_M7$x
RM7<-as.data.frame(RM7)

w_RMRZ<-cbind(Lander_M1,RM2,RM3,RM4,RM5,RM7)


######## zusammenführen von Winter und Sommer

s_RMRZ<-s_RMRZ%>%mutate(season="summer")
w_RMRZ<-w_RMRZ%>%mutate(season="winter")

a_RMRZ<-rbind(s_RMRZ,w_RMRZ)

####### Zeilen verbessern

RM1<-a_RMRZ%>%select(Group.1,RM1,season)
RM1<-RM1%>%mutate(Motiv="Entspannen")
RM1<-rename(RM1,n=RM1)

RM2<-a_RMRZ%>%select(Group.1,RM2,season)
RM2<-RM2%>%mutate(Motiv="Genuss")
RM2<-rename(RM2,n=RM2)

RM3<-a_RMRZ%>%select(Group.1,RM3,season)
RM3<-RM3%>%mutate(Motiv="Neues")
RM3<-rename(RM3,n=RM3)

RM3<-a_RMRZ%>%select(Group.1,RM3,season)
RM3<-RM2%>%mutate(Motiv="Neues")
RM3<-rename(RM3,n=RM3)

RM4<-a_RMRZ%>%select(Group.1,RM4,season)
RM4<-RM4%>%mutate(Motiv="Natur")
RM4<-rename(RM4,n=RM4)

RM5<-a_RMRZ%>%select(Group.1,RM5,season)
RM5<-RM5%>%mutate(Motiv="Familie")
RM5<-rename(RM5,n=RM5)

RM7<-a_RMRZ%>%select(Group.1,RM7,season)
RM7<-RM7%>%mutate(Motiv="Aktiv")
RM7<-rename(RM7,n=RM7)

##### Reisemotive zusammenführen

a_RM<-rbind(RM1,RM2,RM3,RM4,RM5,RM7)

a_RM<-rename(a_RM,Ziel=Group.1)

###### Größten 4 Reiseziele filtern

a_RM_a<-a_RM%>%filter(Ziel=="AUT"|
                        Ziel=="ESP"|
                        Ziel=="TUR"|
                        Ziel=="ITA")

a_RM_a<-a_RM_a%>%arrange(Motiv)

####### Durchschnitte der Motive über Sommer und winter verteilt berechnen

Motive<-aggregate(x=a_RM$n,
                  by=list(a_RM$Motiv),FUN=mean)

temp_x<-Motive$x
temp_x1<-rep(temp_x,each=8)
temp_x1<-as.data.frame(temp_x1)
a_RM_a<-cbind(temp_x1,a_RM_a)

########## Abstand der Sommer/Winter Motive 
#######zu dem Durchschnitt der Motive über beide Saisonen berechnen 


a_RM_a<-a_RM_a%>%mutate(n=n-temp_x1)

###### Jahreszeiten anpasen

a_RM_a<-rename(a_RM_a,Jahreszeit=season)

a_RM_a$Jahreszeit<-as.factor(a_RM_a$Jahreszeit)

levels(a_RM_a$Jahreszeit)[levels(a_RM_a$Jahreszeit)=="summer"]<-"Sommer"
levels(a_RM_a$Jahreszeit)[levels(a_RM_a$Jahreszeit)=="winter"]<-"Winter"

##### Reiehenfolgen und Levels anpassen

a_RM_a$Motiv<-factor(a_RM_a$Motiv,
                     levels=c("Entspannen","Genuss","Neues",
                              "Natur","Familie","Aktiv"))

a_RM_a$Ziel<-as.factor(a_RM_a$Ziel)
levels(a_RM_a$Ziel)[levels(a_RM_a$Ziel)=="AUT"]<-"Österreich"

a_RM_a$Ziel<-as.factor(a_RM_a$Ziel)
levels(a_RM_a$Ziel)[levels(a_RM_a$Ziel)=="ESP"]<-"Spanien"

a_RM_a$Ziel<-as.factor(a_RM_a$Ziel)
levels(a_RM_a$Ziel)[levels(a_RM_a$Ziel)=="TUR"]<-"Türkei"

a_RM_a$Ziel<-as.factor(a_RM_a$Ziel)
levels(a_RM_a$Ziel)[levels(a_RM_a$Ziel)=="ITA"]<-"Italien"


######### plotten

a_RM_a%>%
  ggplot(aes(Motiv,n))+
  geom_bar(aes(fill=Jahreszeit),stat="identity",position="dodge")+
  facet_wrap(~Ziel)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  ylab("Abweichung vom Durchschnittsscore in Prozentpunkten")+
  xlab("Reisemotiv")+
  scale_color_discrete(name="Jahreszeit",labels=c("sommer","winter"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 0.1))











# temporäres einladen des Package "plyr"
library(plyr)

#Oberbereich Reisemotiv Begegnen (RM_OB6_Begegnen_HUR) weglassen weil dort mehr NA`s als bei den anderen Oberbereichen sind

#Datensatz mit unseren ausgewaehlten Variablen:

sommer_Varia<-select(sommer,travel_year,S_Geschlecht,
                     S_Bundesland,S_Alter,S_FamStand,
                     S_Bildung,S_Kinder_u18_binaer,
                     S_Haushaltsgroesse,S_Einkommen_HH,
                     RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                     RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                     RM_OB5_Familie_HUR,
                     RM_OB7_Aktiv_HUR)

winter_Varia<-select(winter,travel_year,S_Geschlecht,
                     S_Bundesland,S_Alter,S_FamStand,
                     S_Bildung,S_Kinder_u18_binaer,
                     S_Haushaltsgroesse,S_Einkommen_HH,
                     RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                     RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                     RM_OB5_Familie_HUR,
                     RM_OB7_Aktiv_HUR)

#Reisemotive Teildatensaetze:

sommerReisemotive <- select(sommer_Varia, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                            RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                            RM_OB5_Familie_HUR,
                            RM_OB7_Aktiv_HUR)

winterReisemotive <- select(winter_Varia, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                            RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                            RM_OB5_Familie_HUR,
                            RM_OB7_Aktiv_HUR)

#Listen fuer Reisemotive im Sommer und Winter mit Durchschnittswerten:

sommerReisemotiveDurchschnitte <- sapply(sommerReisemotive, mean, na.rm = T) 

winterReisemotiveDurchschnitte <- sapply(winterReisemotive, mean, na.rm = T) 

#Dataframe mit abgelesenen gerundeten Werten:

dfReisemotive <- data.frame(Jahreszeit=rep(c("Sommer","Winter"),each=6),
                            Motiv=rep(c("Entspannen", "Genuss", "Neues", "Natur", "Familie", "Aktiv"),times=2),
                            Durchschnitt=c(0.68, 0.58, 0.55, 0.63, 0.67, 0.45,0.64, 0.57, 0.55, 0.60, 0.62, 0.50))

#erstes Balkendiagramm zu Reisemotiven (erster Plot): 

ggplot(dfReisemotive, aes(fill =Jahreszeit, y=Durchschnitt, x=Motiv)) +
  geom_bar(position='dodge', stat='identity')

dfReisemotive <- data.frame(Jahreszeit=rep(c("Sommer","Winter"),each=6),
                            Motiv=rep(c("Entspannen", "Genuss", "Neues", "Natur", "Familie", "Aktiv"),times=2),
                            Durchschnitt=c(0.68, 0.58, 0.55, 0.63, 0.67, 0.45,0.64, 0.57, 0.55, 0.60, 0.62, 0.50))

ggplot(dfReisemotive, aes(fill =Jahreszeit, y=Durchschnitt, x=Motiv)) +
  geom_bar(position='dodge', stat='identity') + ylab("Durchschnitt-Score") + xlab("Reisemotiv") + ylim(0,1) + scale_x_discrete(limits=c("Entspannen", "Genuss", "Neues", "Natur", "Familie", "Aktiv")) + theme_bw()

#Reisemotive mit Geschlecht; nur HUR, deswegen bis 1991 (zweiter Plot):

#Sommer Geschlecht Teildatensatz: 

sommerGeschlecht <- filter(sommer, travel_year < 1992)

sommerReisemotiveGeschlecht <- select(sommerGeschlecht,S_Geschlecht, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                      RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                      RM_OB5_Familie_HUR,
                                      RM_OB7_Aktiv_HUR)
#Winter Geschlecht Teildatensatz: 

winterGeschlecht <- filter(winter, travel_year < 1992)

winterReisemotiveGeschlecht <- select(winterGeschlecht,S_Geschlecht, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                      RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                      RM_OB5_Familie_HUR,
                                      RM_OB7_Aktiv_HUR)
#Sommer dataframe:

Geschlecht <- sommerReisemotiveGeschlecht$S_Geschlecht
Entspannen <- sommerReisemotiveGeschlecht$RM_OB1_Entspannen_HUR
Genuss <- sommerReisemotiveGeschlecht$RM_OB2_Genuss_HUR
Neues <- sommerReisemotiveGeschlecht$RM_OB3_Neues_HUR
Natur<- sommerReisemotiveGeschlecht$RM_OB4_Natur_HUR
Familie <- sommerReisemotiveGeschlecht$RM_OB5_Familie_HUR
Aktiv <- sommerReisemotiveGeschlecht$RM_OB7_Aktiv_HUR

dataSommerHURGeschlecht <- data.frame(Geschlecht,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Winter dataframe:

Geschlecht <- winterReisemotiveGeschlecht$S_Geschlecht
Entspannen <- winterReisemotiveGeschlecht$RM_OB1_Entspannen_HUR
Genuss <- winterReisemotiveGeschlecht$RM_OB2_Genuss_HUR
Neues <- winterReisemotiveGeschlecht$RM_OB3_Neues_HUR
Natur <- winterReisemotiveGeschlecht$RM_OB4_Natur_HUR
Familie <- winterReisemotiveGeschlecht$RM_OB5_Familie_HUR
Aktiv <- winterReisemotiveGeschlecht$RM_OB7_Aktiv_HUR

dataWinterHURGeschlecht <- data.frame(Geschlecht,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Neuer Sommer Datensatz mit Geschlecht und Durchschnitte der Reisemotive:

Entspannendurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Entspannen,
                                    by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Genuss,
                                by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Neues,
                               by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Natur,
                               by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Familie,
                                 by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = dataSommerHURGeschlecht$Aktiv,
                               by = list(dataSommerHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)

EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')
FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')
AlleDurchschnitteSommerGeschlecht <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Neuer Winter Datensatz mit Geschlecht und Durchschnitte der Reisemotive:

Entspannendurchschnitt <- aggregate(x = dataWinterHURGeschlecht$Entspannen,
                                    by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = dataWinterHURGeschlecht$Genuss,
                                by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x =dataWinterHURGeschlecht$Neues,
                               by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = dataWinterHURGeschlecht$Natur,
                               by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = dataWinterHURGeschlecht$Familie,
                                 by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = dataWinterHURGeschlecht$Aktiv,
                               by = list(dataWinterHURGeschlecht$Geschlecht), FUN = mean, na.rm = T)

EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')
FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')
AlleDurchschnitteWinterGeschlecht <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Neue Spalte mit Jahreszeit Sommer/Winter: 

AlleDurchschnitteSommerGeschlecht<-AlleDurchschnitteSommerGeschlecht%>%mutate(Jahreszeit="Sommer")

AlleDurchschnitteWinterGeschlecht<-AlleDurchschnitteWinterGeschlecht%>%mutate(Jahreszeit="Winter")

#Sommer und Winter zusammen:

AlleDurchschnitteGeschlecht <-rbind.fill(AlleDurchschnitteSommerGeschlecht,AlleDurchschnitteWinterGeschlecht)

#"keine Angabe" entfernen:

AlleDurchschnitteGeschlecht <- filter(AlleDurchschnitteGeschlecht, Group.1!= "keine Angabe")

#Umlaute:

AlleDurchschnitteGeschlecht$Group.1 <- as.factor(AlleDurchschnitteGeschlecht$Group.1)
levels(AlleDurchschnitteGeschlecht$Group.1) [levels(AlleDurchschnitteGeschlecht$Group.1)=="maennlich"] <- "männlich"

#Plot:

plot19 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.x.x, x=Group.1)) +
  geom_bar(position='dodge', stat='identity') + xlab("Geschlecht") + ylab("Durchschnitt-Score") + ggtitle('Entspannen')+ ylim(0.0, 1)+ theme_bw()+theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                        axis.text.x=element_blank(),
                                                                                                                                                                                        axis.ticks.x=element_blank())
plot20 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.y.x, x=Group.1)) +
  geom_bar(position='dodge', stat='identity') + xlab("Geschlecht") + ylab("Durchschnitt-Score") + ggtitle('Genuss') + ylim(0.0, 1)+ theme_bw()+theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                     axis.text.x=element_blank(),
                                                                                                                                                                                     axis.ticks.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot21 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.x.y, x=Group.1)) +
  geom_bar(position='dodge', stat='identity') + xlab("Geschlecht") + ylab("Durchschnitt-Score") + ggtitle('Neues') + ylim(0.0, 1)+ theme_bw() + theme(axis.title.x=element_blank(),
                                                                                                                                                      axis.text.x=element_blank(),
                                                                                                                                                      axis.ticks.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot22 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.y.y, x=Group.1)) +
  geom_bar(position='dodge', stat='identity')  + ylab("Durchschnitt-Score") + ggtitle('Natur') + ylim(0.0, 1)+ theme_bw()+theme(legend.position="none")+ theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank())

plot23 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.x, x=Group.1)) +
  geom_bar(position='dodge', stat='identity') + xlab("Geschlecht") + ylab("Durchschnitt-Score") + ggtitle('Familie') + ylim(0.0, 1)+ theme_bw()+theme(legend.position="none") +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plot24 <- ggplot(AlleDurchschnitteGeschlecht, aes(fill =Jahreszeit, y=x.y, x=Group.1)) +
  geom_bar(position='dodge', stat='identity') + ylab("Durchschnitt-Score") + ggtitle('Aktiv') + ylim(0.0, 1)+ theme_bw()+theme(legend.position="none")+  theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plotsobenG <- plot19 + plot20 + plot21

plotsuntenG <- plot22 + plot23 + plot24

plotsobenG/plotsuntenG +
  theme_bw()

#Reisemotive mit Alter, nur HUR (dritter Plot):

#Sommer Alter Teildatensatz:

sommerAlter <- filter(sommer, travel_year < 1992)

sommerReisemotiveAlter <- select(sommerAlter,S_Alter, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                 RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                 RM_OB5_Familie_HUR,
                                 RM_OB7_Aktiv_HUR)

#Winter Alter Teildatensatz:     

winterAlter <- filter(winter, travel_year < 1992)

winterReisemotiveAlter <- select(winterAlter,S_Alter, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                 RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                 RM_OB5_Familie_HUR,
                                 RM_OB7_Aktiv_HUR)

#Sommer dataframe:

Alter <- sommerReisemotiveAlter$S_Alter
Entspannen <- sommerReisemotiveAlter$RM_OB1_Entspannen_HUR
Genuss <- sommerReisemotiveAlter$RM_OB2_Genuss_HUR
Neues <- sommerReisemotiveAlter$RM_OB3_Neues_HUR
Natur<- sommerReisemotiveAlter$RM_OB4_Natur_HUR
Familie <- sommerReisemotiveAlter$RM_OB5_Familie_HUR
Aktiv <- sommerReisemotiveAlter$RM_OB7_Aktiv_HUR

dataSommerHURAlter <- data.frame(Alter,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Winter dataframe:

Alter <- winterReisemotiveAlter$S_Alter
Entspannen <- winterReisemotiveAlter$RM_OB1_Entspannen_HUR
Genuss <- winterReisemotiveAlter$RM_OB2_Genuss_HUR
Neues <- winterReisemotiveAlter$RM_OB3_Neues_HUR
Natur <- winterReisemotiveAlter$RM_OB4_Natur_HUR
Familie <- winterReisemotiveAlter$RM_OB5_Familie_HUR
Aktiv <- winterReisemotiveAlter$RM_OB7_Aktiv_HUR

dataWinterHURAlter <- data.frame(Alter,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Sommer und Winter zu einem Datensatz:

datenzusammenHURAlter <- rbind(dataSommerHURAlter,dataWinterHURAlter)

#Neuer Sommer Datensatz mit Alter und Durchschnitte der Reisemotive:

Entspannendurchschnitt <- aggregate(x = dataSommerHURAlter$Entspannen,                
                                    by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = dataSommerHURAlter$Genuss,                
                                by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x = dataSommerHURAlter$Neues,                
                               by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = dataSommerHURAlter$Natur,                
                               by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = dataSommerHURAlter$Familie,                
                                 by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = dataSommerHURAlter$Aktiv,                
                               by = list(dataSommerHURAlter$Alter), FUN = mean, na.rm = T)

EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')
FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')

AlleDurchschnitteSommerAlter <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Neuer Winter Datensatz mit Alter und Durchschnitte der Reisemotive:

Entspannendurchschnitt <- aggregate(x = dataWinterHURAlter$Entspannen,                
                                    by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = dataWinterHURAlter$Genuss,                
                                by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x =dataWinterHURAlter$Neues,                
                               by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = dataWinterHURAlter$Natur,                
                               by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = dataWinterHURAlter$Familie,                
                                 by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = dataWinterHURAlter$Aktiv,                
                               by = list(dataWinterHURAlter$Alter), FUN = mean, na.rm = T)
EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')

FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')

AlleDurchschnitteWinterAlter <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Neue Spalte mit Jahreszeit Sommer/Winter: 

AlleDurchschnitteSommerAlter<-AlleDurchschnitteSommerAlter%>%mutate(Jahreszeit="Sommer")

AlleDurchschnitteWinterAlter<-AlleDurchschnitteWinterAlter%>%mutate(Jahreszeit="Winter")

AlleDurchschnitteAlter <-rbind.fill(AlleDurchschnitteSommerAlter,AlleDurchschnitteWinterAlter)

#plot:

plot13 <- ggplot(data =AlleDurchschnitteAlter, mapping = (aes(x = Group.1, y = x.x.x))) + geom_line(aes(color=Jahreszeit), size=1) + xlab("Alter") + ylab("Durchschnitt") + ggtitle('Entspannen') + ylim(0.3, 1)+ theme_bw()+ theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                    axis.ticks.x=element_blank()) + ylab("Durchschnitt-Score")

plot14 <- ggplot(data=AlleDurchschnitteAlter,mapping = (aes(x = Group.1, y = x.y.x))) + geom_line(aes(color=Jahreszeit), size=1) + xlab("Alter") + ylab("Durchschnitt")+ ggtitle('Genuss')+ ylim(0.3, 1)+ theme_bw()+theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                           axis.text.x=element_blank(),
                                                                                                                                                                                                                                                           axis.ticks.x=element_blank())+  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plot15 <- ggplot(data= AlleDurchschnitteAlter, mapping = (aes(x=Group.1, y= x.x.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Alter") + ylab("Durchschnitt") + ggtitle('Neues')+ ylim(0.3, 1)+ theme_bw()+ theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                          axis.text.x=element_blank(),
                                                                                                                                                                                                                          axis.ticks.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plot16 <- ggplot(data= AlleDurchschnitteAlter, mapping = (aes(x=Group.1, y=x.y.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Alter") + ylab("Durchschnitt") + ggtitle('Natur')+ ylim(0.3, 1)+ theme_bw()+theme(legend.position="none")+  theme(axis.title.x=element_blank())+ theme(axis.title.y=element_blank()) 

plot17 <- ggplot(data= AlleDurchschnitteAlter, mapping = (aes(x=Group.1, y= x.x))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Alter") + ylab("Durchschnitt") + ggtitle('Familie')+ ylim(0.3, 1)+ theme_bw()+theme(legend.position="none")  + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plot18 <- ggplot(data= AlleDurchschnitteAlter, mapping = (aes(x=Group.1, y= x.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Alter") + ylab("Durchschnitt") + ggtitle('Aktiv')+ ylim(0.3, 1)+ theme_bw() +theme(legend.position="none")+  theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

plotsoben <- plot13 + plot14 + plot15

plotsunten <- plot16 + plot17 + plot18

plotsoben/plotsunten +
  theme_bw()

#Linienplot Reisemotive mit Jahren fuer zeitlichen Verlauf (letzter Plot):

#Neuer Datensatz mit HUR bis 1991 und UR ab 1992:

sommerReisemotiveJahre <- select(sommer,travel_year, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                 RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                 RM_OB5_Familie_HUR,
                                 RM_OB7_Aktiv_HUR,
                                 RM_OB1_Entspannen_UR,RM_OB2_Genuss_UR,
                                 RM_OB3_Neues_UR,RM_OB4_Natur_UR,
                                 RM_OB5_Familie_UR,
                                 RM_OB7_Aktiv_UR)

winterReisemotiveJahre <- select(winter,travel_year, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                                 RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                                 RM_OB5_Familie_HUR,
                                 RM_OB7_Aktiv_HUR,
                                 RM_OB1_Entspannen_UR,RM_OB2_Genuss_UR,
                                 RM_OB3_Neues_UR,RM_OB4_Natur_UR,
                                 RM_OB5_Familie_UR,
                                 RM_OB7_Aktiv_UR)

#HUR:

HURSommer <- select(sommerReisemotiveJahre, travel_year, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                    RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                    RM_OB5_Familie_HUR,
                    RM_OB7_Aktiv_HUR)

HURWinter <- select(winterReisemotiveJahre, travel_year, RM_OB1_Entspannen_HUR,RM_OB2_Genuss_HUR,
                    RM_OB3_Neues_HUR,RM_OB4_Natur_HUR,
                    RM_OB5_Familie_HUR,
                    RM_OB7_Aktiv_HUR)
#UR:

URSommer <- select(sommerReisemotiveJahre, travel_year, RM_OB1_Entspannen_UR,RM_OB2_Genuss_UR,
                   RM_OB3_Neues_UR,RM_OB4_Natur_UR,
                   RM_OB5_Familie_UR,
                   RM_OB7_Aktiv_UR)

URWinter <- select(winterReisemotiveJahre, travel_year, RM_OB1_Entspannen_UR,RM_OB2_Genuss_UR,
                   RM_OB3_Neues_UR,RM_OB4_Natur_UR,
                   RM_OB5_Familie_UR,
                   RM_OB7_Aktiv_UR)

ReisemotiveHURbis1991Sommer <- filter(HURSommer, travel_year < 1992)

ReisemotiveHURbis1991Winter <- filter(HURWinter, travel_year < 1992)

ReisemotiveURab1992Sommer <- filter(URSommer, travel_year > 1991)

ReisemotiveURab1992Winter <- filter(URWinter, travel_year > 1991)

#Sommer HUR:

Jahre <- ReisemotiveHURbis1991Sommer$travel_year
Entspannen <- ReisemotiveHURbis1991Sommer$RM_OB1_Entspannen_HUR
Genuss <- ReisemotiveHURbis1991Sommer$RM_OB2_Genuss_HUR
Neues <- ReisemotiveHURbis1991Sommer$RM_OB3_Neues_HUR
Natur<- ReisemotiveHURbis1991Sommer$RM_OB4_Natur_HUR
Familie <- ReisemotiveHURbis1991Sommer$RM_OB5_Familie_HUR
Aktiv <- ReisemotiveHURbis1991Sommer$RM_OB7_Aktiv_HUR

dataSommerHUR <- data.frame(Jahre,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Winter HUR:

Jahre <- ReisemotiveHURbis1991Winter$travel_year
Entspannen <- ReisemotiveHURbis1991Winter$RM_OB1_Entspannen_HUR
Genuss <- ReisemotiveHURbis1991Winter$RM_OB2_Genuss_HUR
Neues <- ReisemotiveHURbis1991Winter$RM_OB3_Neues_HUR
Natur <- ReisemotiveHURbis1991Winter$RM_OB4_Natur_HUR
Familie <- ReisemotiveHURbis1991Winter$RM_OB5_Familie_HUR
Aktiv <- ReisemotiveHURbis1991Winter$RM_OB7_Aktiv_HUR

dataWinterHUR <- data.frame(Jahre,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

datenzusammenHUR <- rbind(dataSommerHUR,dataWinterHUR)

#Sommer UR:

Jahre <- ReisemotiveURab1992Sommer$travel_year
Entspannen <- ReisemotiveURab1992Sommer$RM_OB1_Entspannen_UR
Genuss <- ReisemotiveURab1992Sommer$RM_OB2_Genuss_UR
Neues <- ReisemotiveURab1992Sommer$RM_OB3_Neues_UR
Natur <- ReisemotiveURab1992Sommer$RM_OB4_Natur_UR
Familie <- ReisemotiveURab1992Sommer$RM_OB5_Familie_UR
Aktiv <- ReisemotiveURab1992Sommer$RM_OB7_Aktiv_UR

dataSommerUR <- data.frame(Jahre,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#Winter UR:

Jahre <- ReisemotiveURab1992Winter$travel_year
Entspannen <- ReisemotiveURab1992Winter$RM_OB1_Entspannen_UR
Genuss <- ReisemotiveURab1992Winter$RM_OB2_Genuss_UR
Neues <- ReisemotiveURab1992Winter$RM_OB3_Neues_UR
Natur <- ReisemotiveURab1992Winter$RM_OB4_Natur_UR
Familie <- ReisemotiveURab1992Winter$RM_OB5_Familie_UR
Aktiv <- ReisemotiveURab1992Winter$RM_OB7_Aktiv_UR

dataWinterUR <- data.frame(Jahre,Entspannen,Genuss,Neues,Natur,Familie,Aktiv)

#HUR und UR in Datensaetze fuer Sommer und Winter:

SommerHURUR <- rbind(dataSommerHUR,dataSommerUR)

WinterHURUR <- rbind(dataWinterHUR, dataWinterUR)

#Sommer Datensatz mit Durchschnitten pro Jahr:

Entspannendurchschnitt <- aggregate(x = SommerHURUR$Entspannen,                
                                    by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = SommerHURUR$Genuss,                
                                by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x = SommerHURUR$Neues,                
                               by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = SommerHURUR$Natur,                
                               by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = SommerHURUR$Familie,                
                                 by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = SommerHURUR$Aktiv,                
                               by = list(SommerHURUR$Jahre), FUN = mean, na.rm = T)

EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')
FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')

AlleDurchschnitteSommer <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Winter Datensatz mit Durchschnitten pro Jahr:


Entspannendurchschnitt <- aggregate(x = WinterHURUR$Entspannen,                
                                    by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)
Genussdurchschnitt <- aggregate(x = WinterHURUR$Genuss,                
                                by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)
Neuesdurchschnitt <- aggregate(x = WinterHURUR$Neues,                
                               by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)
Naturdurchschnitt <- aggregate(x = WinterHURUR$Natur,                
                               by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)
Familiedurchschnitt <- aggregate(x = WinterHURUR$Familie,                
                                 by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)
Aktivdurchschnitt <- aggregate(x = WinterHURUR$Aktiv,                
                               by = list(WinterHURUR$Jahre), FUN = mean, na.rm = T)

EGDurchschnitte <- merge(Entspannendurchschnitt, Genussdurchschnitt, by = 'Group.1')
NNDurchschnitte <- merge(Neuesdurchschnitt, Naturdurchschnitt, by = 'Group.1')
EGNNDurchschnitte <- merge(EGDurchschnitte, NNDurchschnitte, by = 'Group.1')
FADurchschnitte <- merge(Familiedurchschnitt, Aktivdurchschnitt, by = 'Group.1')

AlleDurchschnitteWinter <- merge(EGNNDurchschnitte, FADurchschnitte, by = 'Group.1' )

#Neue Spalte mit Jahreszeit Sommer/Winter: 

AlleDurchschnitteSommer<-AlleDurchschnitteSommer%>%mutate(Jahreszeit="Sommer")

AlleDurchschnitteWinter<-AlleDurchschnitteWinter%>%mutate(Jahreszeit="Winter")

AlleDurchschnitte2<-rbind(AlleDurchschnitteSommer,AlleDurchschnitteWinter)

#Plot: 

plot7 <- ggplot(data =AlleDurchschnitte2, mapping = (aes(x = Group.1, y = x.x.x))) + geom_line(aes(color=Jahreszeit), size=1) + xlab("Jahre") + ylab("Durchschnitt-Score") + ggtitle('Entspannen') + ylim(0.3, 1) + geom_vline(xintercept= 1992) +theme_bw()+ theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                                                    axis.ticks.x=element_blank()) 

plot8 <- ggplot(data=AlleDurchschnitte2,mapping = (aes(x = Group.1, y = x.y.x))) + geom_line(aes(color=Jahreszeit), size=1) + xlab("Jahre") + ylab("Durchschnitt-Score")+ ggtitle('Genuss')+ ylim(0.3,1)+ geom_vline(xintercept= 1992)+theme_bw()+theme(legend.position="none") + theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                                        axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                                        axis.ticks.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                                                                                               axis.ticks.y=element_blank())

plot9 <- ggplot(data= AlleDurchschnitte2, mapping = (aes(x=Group.1, y= x.x.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Jahre") + ylab("Durchschnitt-Score") + ggtitle('Neues')+ ylim(0.3, 1)+ geom_vline(xintercept= 1992)+theme_bw()+ theme(axis.title.x=element_blank(),
                                                                                                                                                                                                                                                        axis.text.x=element_blank(),
                                                                                                                                                                                                                                                        axis.ticks.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                                                               axis.ticks.y=element_blank())

plot10 <- ggplot(data= AlleDurchschnitte2, mapping = (aes(x=Group.1, y=x.y.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Jahre") + ylab("Durchschnitt-Score") + ggtitle('Natur')+ ylim(0.3, 1)+ geom_vline(xintercept= 1992)+theme_bw()+theme(legend.position="none")+  theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank())+ theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

plot11 <- ggplot(data= AlleDurchschnitte2, mapping = (aes(x=Group.1, y= x.x))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Jahre") + ylab("Durchschnitt-Score") + ggtitle('Familie')+ ylim(0.3, 1)+ geom_vline(xintercept= 1992)+theme_bw()+theme(legend.position="none") +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                                                         axis.ticks.y=element_blank()) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

plot12 <- ggplot(data= AlleDurchschnitte2, mapping = (aes(x=Group.1, y= x.y))) + geom_line(aes(color=Jahreszeit),size=1) + xlab("Jahre") + ylab("Durchschnitt-Score") + ggtitle('Aktiv')+ ylim(0.3, 1)+ geom_vline(xintercept= 1992)+theme_bw()+theme(legend.position="none")+  theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                                                                                                                                                                                                                                                                                                                             axis.ticks.y=element_blank())+ theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

plot789 <- plot7 + plot8 + plot9

plot101112 <- plot10 + plot11 + plot12

plot789/plot101112 +
  theme_bw()

# Package "reshape2" unloaden damit keine Probleme mit "dplyr" entstehen (keine Spezifizierung des Packages notwendig)
detach(package:plyr, unload = TRUE)
# Environment leeren um Memory limits zu umgehen
rm(list = ls(all.names = TRUE))
# Erstellen einer u14 Variable und anfügen an den Datensatz

APR_data <- readRDS("210727_dat_ftf.rds")

u14 <- APR_data%>%filter(APR_data$S_Kinder_0_bis_5_binaer == "Kinder dieser Altersstufe" |
                           APR_data$S_Kinder_6_bis_13_binaer == "Kinder dieser Altersstufe")
nu14 <- APR_data%>%filter(APR_data$S_Kinder_u18_binaer == "keine Kinder dieser Altersstufe" |
                            (APR_data$S_Kinder_14_bis_17_binaer == "Kinder dieser Altersstufe" &
                               APR_data$S_Kinder_0_bis_5_binaer == "keine Kinder dieser Altersstufe" &
                               APR_data$S_Kinder_6_bis_13_binaer == "keine Kinder dieser Altersstufe"))

u14 <- u14%>%mutate(u14, S_Kinder_u14_binaer = "Kinder dieser Altersstufe")
nu14 <- nu14%>%mutate(nu14, S_Kinder_u14_binaer = "keine Kinder dieser Altersstufe")

data <- rbind(u14, nu14)


#'data' entspricht APR_data mit u14 variable

f<-subset(data,data$JS_HUR_Antrittsmonat=="Dezember")
g<-subset(data,data$JS_HUR_Antrittsmonat=="Januar")
h<-subset(data,data$JS_HUR_Antrittsmonat=="Februar")
i<-subset(data,data$JS_HUR_Antrittsmonat=="Maerz")
winter<-rbind(f,g,h,i)

j<-subset(data,data$JS_HUR_Antrittsmonat=="Juni")
k<-subset(data,data$JS_HUR_Antrittsmonat=="Juli")
l<-subset(data,data$JS_HUR_Antrittsmonat=="August")
m<-subset(data,data$JS_HUR_Antrittsmonat=="September")
sommer<-rbind(j,k,l,m)



winter1 <- filter(winter, travel_year > 2008)
sommer1 <- filter(sommer, travel_year > 2008)
###Beschränkung auf die letzten 10 Jahre

###zuerst für den Sommer....
JSHURs <- group_by(sommer1, JS_HUR_Reiseziel)
by_JSHURs <- summarise(JSHURs, count = n())
by_JSHURs <- by_JSHURs%>%mutate(Jahreszeit = "Sommer")

bls <- filter(by_JSHURs, JS_HUR_Reiseziel == "BY" |
                JS_HUR_Reiseziel == "SH" |
                JS_HUR_Reiseziel == "BW" |
                JS_HUR_Reiseziel == "MV" |
                JS_HUR_Reiseziel == "NI" |
                JS_HUR_Reiseziel == "NW" |
                JS_HUR_Reiseziel == "BE" |
                JS_HUR_Reiseziel == "SN" | 
                JS_HUR_Reiseziel == "HE" |
                JS_HUR_Reiseziel == "TH" |
                JS_HUR_Reiseziel == "BB" |
                JS_HUR_Reiseziel == "ST" |
                JS_HUR_Reiseziel == "SL" |
                JS_HUR_Reiseziel == "HB" |
                JS_HUR_Reiseziel == "HH" |
                JS_HUR_Reiseziel == "RP")

bls <- bls%>%mutate(Anteil = bls$count/sum(bls$count))
DEs <- data.frame(JS_HUR_Reiseziel = "DE", count = sum(bls$count), Jahreszeit = "Sommer", Anteil=sum(bls$count)/length(sommer1$id))

bls <- filter(bls, JS_HUR_Reiseziel == "BY" |
                JS_HUR_Reiseziel == "SH" |
                JS_HUR_Reiseziel == "BW" |
                JS_HUR_Reiseziel == "MV" |
                JS_HUR_Reiseziel == "NI" |
                JS_HUR_Reiseziel == "NW" |
                JS_HUR_Reiseziel == "BE" |
                JS_HUR_Reiseziel == "SN" |
                JS_HUR_Reiseziel == "HE" |
                JS_HUR_Reiseziel == "TH")

###...dann für den Winter
JSHURw <- group_by(winter1, JS_HUR_Reiseziel)
by_JSHURw <- summarise(JSHURw, count = n())
by_JSHURw <- by_JSHURw%>%mutate(Jahreszeit = "Winter")

blw <- filter(by_JSHURw, JS_HUR_Reiseziel == "BY" |
                JS_HUR_Reiseziel == "SH" |
                JS_HUR_Reiseziel == "BW" |
                JS_HUR_Reiseziel == "NI" |
                JS_HUR_Reiseziel == "MV" |
                JS_HUR_Reiseziel == "BE" |
                JS_HUR_Reiseziel == "SN" |
                JS_HUR_Reiseziel == "NW" |
                JS_HUR_Reiseziel == "HE" |
                JS_HUR_Reiseziel == "TH" |
                JS_HUR_Reiseziel == "BB" |
                JS_HUR_Reiseziel == "ST" |
                JS_HUR_Reiseziel == "SL" |
                JS_HUR_Reiseziel == "HB" |
                JS_HUR_Reiseziel == "HH" |
                JS_HUR_Reiseziel == "RP")

blw <- blw%>%mutate(Anteil = blw$count/sum(blw$count))
DEw <- data.frame(JS_HUR_Reiseziel = "DE", count = sum(blw$count), Jahreszeit = "Winter", Anteil=sum(blw$count)/length(winter1$id))

blw <- filter(blw, JS_HUR_Reiseziel == "BY" |
                JS_HUR_Reiseziel == "SH" |
                JS_HUR_Reiseziel == "BW" |
                JS_HUR_Reiseziel == "NI" |
                JS_HUR_Reiseziel == "MV" |
                JS_HUR_Reiseziel == "BE" |
                JS_HUR_Reiseziel == "SN" |
                JS_HUR_Reiseziel == "NW" |
                JS_HUR_Reiseziel == "HE" |
                JS_HUR_Reiseziel == "TH")
### relative Häufigkeiten innerhalb DE, danach Herausfiltern der 10 "größten" Reiseziele

bld <- rbind(bls, blw)

DE<-rbind(DEs, DEw)
###die Daten von Deutschland werden für den Plot danach benötigt

bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "BY"] <- "Bayern"   
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "MV"] <- "Mecklenburg-Vorpommern"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "NI"] <- "Niedersachsen"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "SH"] <- "Schleswig-Holstein"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "BW"] <- "Baden-Württemberg"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "BE"] <- "Berlin"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "SN"] <- "Sachsen"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "NW"] <- "Nordrhein-Westfalen"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "TH"] <- "Thüringen"
bld$JS_HUR_Reiseziel[bld$JS_HUR_Reiseziel == "HE"] <- "Hessen"
### Umbenennung für Grafik

plotd <- ggplot(data = bld) +
  geom_bar(aes(reorder(JS_HUR_Reiseziel, -Anteil), Anteil, fill = Jahreszeit), stat = "identity", position = "dodge" ) +
  facet_grid() +
  xlab("Reiseziel")+
  ylab("Anteil an Winter-/Sommerurlaubern")+
  theme_bw()+
  theme(axis.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 14))+
  theme(axis.text.x = element_text(angle = 40, size = 9, hjust = 1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.33))

plotd



by_JSHURw2 <- by_JSHURw%>%mutate(Anteil = by_JSHURw$count/sum(by_JSHURw$count))

by_JSHURs2 <- by_JSHURs%>%mutate(Anteil = by_JSHURs$count/sum(by_JSHURs$count))
###relative Häufigkeiten weltweit

by2s <- filter(by_JSHURs2, JS_HUR_Reiseziel == "ESP" |
                 JS_HUR_Reiseziel == "ITA" |
                 JS_HUR_Reiseziel == "AUT" |
                 JS_HUR_Reiseziel == "FRA" |
                 JS_HUR_Reiseziel == "GRC" |
                 JS_HUR_Reiseziel == "HRV" |
                 JS_HUR_Reiseziel == "TUR" |
                 JS_HUR_Reiseziel == "SOAN" |
                 JS_HUR_Reiseziel == "KRIK")

by2w <- filter(by_JSHURw2, JS_HUR_Reiseziel == "ESP" |
                 JS_HUR_Reiseziel == "ITA" |
                 JS_HUR_Reiseziel == "AUT" |
                 JS_HUR_Reiseziel == "FRA" |
                 JS_HUR_Reiseziel == "GRC" |
                 JS_HUR_Reiseziel == "HRV" |
                 JS_HUR_Reiseziel == "TUR" |
                 JS_HUR_Reiseziel == "SOAN" |
                 JS_HUR_Reiseziel == "KRIK")
###Filtern der "größten" Reiseziele

by2 <- rbind(by2s, by2w)
by2 <- rbind(by2, DE)
###Hinzufügen von DE (aus dem vorherigen Plot)

by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "ESP"] <- "Spanien"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "AUT"] <- "Österreich"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "ITA"] <- "Italien"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "HRV"] <- "Kroatien"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "GRC"] <- "Griechenland"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "KRIK"] <- "Karibik"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "TUR"] <- "Türkei"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "FRA"] <- "Frankreich"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "SOAN"] <- "Südostasien"
by2$JS_HUR_Reiseziel[by2$JS_HUR_Reiseziel == "DE"] <- "Deutschland"
###Umbenennen für Grafik

plot_by2 <- ggplot(data = by2) +
  geom_bar(aes(reorder(JS_HUR_Reiseziel, -Anteil), Anteil, fill = Jahreszeit), stat = "identity", position = "dodge" ) +
  facet_grid() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, size = 11, hjust = 1)) +
  xlab("Reiseziel")+
  ylab("Anteil an Winter-/Sommerurlaubern")+
  theme(axis.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 14))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.33))

plot_by2



HUR <- data%>%select(JS_HUR_Antrittsmonat, travel_year, JS_HUR_Reiseziel)

HURs <- HUR%>%filter(JS_HUR_Antrittsmonat == "Juni"|
                       JS_HUR_Antrittsmonat == "Juli"|
                       JS_HUR_Antrittsmonat == "August"|
                       JS_HUR_Antrittsmonat == "September")
HURw <- HUR%>%filter(JS_HUR_Antrittsmonat == "Dezember"|
                       JS_HUR_Antrittsmonat == "Januar"|
                       JS_HUR_Antrittsmonat == "Februar"|
                       JS_HUR_Antrittsmonat == "Maerz")

###Darstellung von Reisezielen im zeitlichen Verlauf

sizetime <- HURs %>%
  group_by(travel_year, JS_HUR_Reiseziel) %>%
  summarise(JS_HUR_Reiseziel, n = n())
sizetime_summer <- sizetime %>% mutate(Jahreszeit = "Sommer") 
sizetime_summer <- transform(sizetime_summer, Anteil = 48*sizetime_summer$n/length(sizetime_summer$n))

sizetime <- HURw %>%
  group_by(travel_year, JS_HUR_Reiseziel) %>%
  summarise(JS_HUR_Reiseziel, n = n())
sizetime_winter <- sizetime %>% mutate(Jahreszeit = "Winter")
sizetime_winter <- transform(sizetime_winter, Anteil = 48*sizetime_winter$n/length(sizetime_winter$n))
###damit die relativen Häufigkeiten sich auf das jeweilige Jahr beziehen: multiplizieren mit Anzahl der Jahre (48)

sizetime_all <- rbind(sizetime_summer, sizetime_winter)
sizetime_all <- sizetime_all%>%filter(JS_HUR_Reiseziel == "ESP" |
                                        JS_HUR_Reiseziel == "AUT" |
                                        JS_HUR_Reiseziel == "ITA" |
                                        JS_HUR_Reiseziel == "FRA" |
                                        JS_HUR_Reiseziel == "GRC" |
                                        JS_HUR_Reiseziel == "HRV" |
                                        JS_HUR_Reiseziel == "TUR" |
                                        JS_HUR_Reiseziel == "KRIK" |
                                        JS_HUR_Reiseziel == "SOAN" |
                                        JS_HUR_Reiseziel == "SOAA")

sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "ESP"] <- "Spanien"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "AUT"] <- "Österreich"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "ITA"] <- "Italien"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "HRV"] <- "Kroatien"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "GRC"] <- "Griechenland"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "KRIK"] <- "Karibik"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "TUR"] <- "Türkei"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "FRA"] <- "Frankreich"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "SOAN"] <- "Südostasien"
sizetime_all$JS_HUR_Reiseziel[sizetime_all$JS_HUR_Reiseziel == "SOAA"] <- "Südostasien"
###SOAA und SOAN (alt und neu) werden hierfür vereinigt

abc <- sizetime_all%>%
  ggplot(aes(travel_year, Anteil, color = Jahreszeit))+
  geom_line(stat = "identity", size = 0.7)+
  facet_wrap(~reorder(JS_HUR_Reiseziel, -n))+
  ylab("Anteil an Winter-/Sommerurlaubern")+
  xlab("Reisejahr")+
  theme_bw()+
  theme(axis.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 14))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3))+
  theme(axis.text.x = element_text(angle = 60, size = 9, hjust = 1))

abc



