dfcombined <- read.csv("dfcombined.csv", header = TRUE, sep =",")
View(dfcombined)

dfcombined$date <- as.Date(dfcombined$date)

dfcombined <- dfcombined %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

dfcombined <- dfcombined %>% arrange(district, date)
View(dfcombined)


df_comb_week<-dfcombined%>%group_by(district,week)%>%summarise(M.A00.04 =sum(M.A00.04),
                                                                    M.A05.14 = sum(M.A05.14),
                                                                    M.A15.34 = sum(M.A15.34),    
                                                                    M.A35.59 = sum(M.A35.59),
                                                                    M.A60.79 = sum(M.A60.79), 
                                                                    M.A80. = sum(M.A80.),
                                                                    M.Aunb = sum(M.Aunb),
                                                                    `F.A00.04`=sum(`F.A00.04`),
                                                                    F.A05.14 = sum(F.A05.14),
                                                                    F.A15.34 = sum(F.A15.34),    
                                                                    F.A35.59 = sum(F.A35.59),
                                                                    F.A60.79 = sum(F.A60.79), 
                                                                    F.A80. = sum(F.A80.),
                                                                    F.Aunb = sum(F.Aunb),
                                                                    `Unb.A00.04`=sum(`Unb.A00.04`),
                                                                    Unb.A05.14 = sum(Unb.A05.14),
                                                                    Unb.A15.34 = sum(Unb.A15.34),    
                                                                    Unb.A35.59 = sum(Unb.A35.59),
                                                                    Unb.A60.79 = sum(Unb.A60.79), 
                                                                    Unb.A80. = sum(Unb.A80.),
                                                                    Unb.Aunb = sum(Unb.Aunb),
                                                                    total_cases = sum(total_cases),
                                                                    .groups="keep")
# identical(sum(dfcombined$total_cases), sum(df_comb_week$total_cases))
colnames(df_comb_week)


# reading vaccination data and replacing the district names with the district names from the RKI data

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



#colnames(impfbayern2)


# min(impfbayern2$date)
# [1] "2020-12-27"

# colnames(impfungentake)

impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)

impfungentake <- impfungentake %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(district, date)

impfungentake<-impfungentake%>%group_by(district,week)%>%summarise(kr_erstimpf =sum(kr_erstimpf),
                                                               kr_zweitimpf =sum(kr_zweitimpf),
                                                               kr_drittimpf =sum(kr_drittimpf),
                                                               kr_viertimpf =sum(kr_viertimpf),
                                                               .groups="keep")

# time difference 334 days ---> 47 weeks (days are Tuesday and Sunday --> round down) (relative to RKI data of course)
# correct week index enables the merge

impfungentake$week <- impfungentake$week + 47

df_comb_week_impf <- merge(df_comb_week, impfungentake, by = c("district", "week"), all.x = TRUE, all.y = TRUE)
# View(df_comb_week_impf)
# Merge induces NA for missing values --> replacement with 0
df_comb_week_impf[is.na(df_comb_week_impf)] <- 0

# any(is.na(df_comb_week_impf))
# identical(sum(df_comb_week_impf$kr_erstimpf, na.rm=TRUE), sum(impfungentake$kr_erstimpf, na.rm=TRUE))

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
popbay2 <- popbay2 %>% 
  mutate(district = recode(district, "LK Lindau (Bodensee)" = "LK Lindau", 
                           "LK Landsberg am Lech" = "LK Landsberg a.Lech", "SK Kempten (Allgäu)" = "SK Kempten"))
df_ultimate <- merge(df_comb_week_impf, popbay2, by = c("district"), all.x = TRUE, all.y = TRUE)

df_ultimate<-df_ultimate[,c(1,30,29,2,3:28,31:35)]


## calculating rates of vaccinations
df_ultimate<-df_ultimate%>%
  group_by(district)%>%
  dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / population,
                rate_zweitimpf=cumsum(kr_zweitimpf) / population,
                rate_drittimpf=cumsum(kr_drittimpf) / population,
                rate_viertimpf=cumsum(kr_viertimpf) / population)
## Fixxing issue of fewer first vaccinations than second vaccinations
vector10001 <- which(df_ultimate$rate_erstimpf < df_ultimate$rate_zweitimpf)
df_ultimate[vector10001,36] <- df_ultimate[vector10001,37]
## Some have more vaccinations than population, maybe Impfzenter in Altötting and the like are the explanation.

df_ultimate$m_anteil<-df_ultimate$male/df_ultimate$population

df_ultimate$f_anteil<-df_ultimate$female/df_ultimate$population

df_ultimate <- df_ultimate %>% mutate(inzidenz = (total_cases / population) * 100000)
df_ultimate<-df_ultimate[,c(1:4,42, 36:39, 34, 35, 5:33, 40:41)]

View(df_ultimate)


colnames(nachbarkreise) <- c("district", "neighboring districts", "bezirk")
dfultimate <- merge(dfultimate, nachbarkreise, by = c("district", "bezirk"), all.x = TRUE, all.y = TRUE)



dfultimate <- dfultimate %>% mutate(M.A00.04.Anteil = M.A00.04/total_cases,
                                    M.A05.14.Anteil = M.A05.14/total_cases,
                                    M.A15.34.Anteil = M.A15.34/total_cases,
                                    M.A35.59.Anteil = M.A35.59/total_cases,
                                    M.A60.79.Anteil = M.A60.79/total_cases,
                                    M.A80.Anteil = M.A80./total_cases,
                                    M.Aunb.Anteil = M.A80./total_cases,
                                    
                                    F.A00.04.Anteil =F.A00.04/total_cases,
                                    F.A05.14.Anteil =F.A05.14/total_cases,
                                    F.A15.34.Anteil =F.A15.34/total_cases,
                                    F.A35.59.Anteil =F.A35.59/total_cases,
                                    F.A60.79.Anteil =F.A60.79/total_cases,
                                    F.A80.Anteil =F.A80./total_cases,
                                    F.Aunb.Anteil =F.A80./total_cases,
                                    
                                    Unb.A00.04.Anteil =Unb.A00.04/total_cases,
                                    Unb.A05.14.Anteil =Unb.A05.14/total_cases,
                                    Unb.A15.34.Anteil =Unb.A15.34/total_cases,
                                    Unb.A35.59.Anteil =Unb.A35.59/total_cases,
                                    Unb.A60.79.Anteil =Unb.A60.79/total_cases,
                                    Unb.A80.Anteil =Unb.A80./total_cases,
                                    Unb.Aunb.Anteil =Unb.A80./total_cases)
dfultimate <- dfultimate %>% mutate_all(~replace(., is.nan(.), 0)) 



dfultimate <- dfultimate %>%
  group_by(district) %>%
  dplyr::mutate(A00.04.Anteil = M.A00.04.Anteil + F.A00.04.Anteil + Unb.A00.04.Anteil,
                A05.14.Anteil = M.A05.14.Anteil + F.A05.14.Anteil + Unb.A05.14.Anteil,
                A15.34.Anteil = M.A15.34.Anteil + F.A15.34.Anteil + Unb.A15.34.Anteil,
                A35.59.Anteil = M.A35.59.Anteil + F.A35.59.Anteil + Unb.A35.59.Anteil,
                A60.79.Anteil = M.A60.79.Anteil + F.A60.79.Anteil + Unb.A60.79.Anteil,
                A80.Anteil = M.A80.Anteil + F.A80.Anteil + Unb.A80.Anteil,
                Aunb.Anteil = M.Aunb.Anteil + F.Aunb.Anteil + Unb.Aunb.Anteil,
                M.Anteil = M.A00.04.Anteil + M.A05.14.Anteil +
                  M.A15.34.Anteil + M.A35.59.Anteil + M.A60.79.Anteil + M.Aunb.Anteil,
                F.Anteil = F.A00.04.Anteil + F.A05.14.Anteil +
                  F.A15.34.Anteil + F.A35.59.Anteil + F.A60.79.Anteil + F.Aunb.Anteil,
                Unb.Anteil = Unb.A00.04.Anteil + Unb.A05.14.Anteil +
                  Unb.A15.34.Anteil + Unb.A35.59.Anteil + Unb.A60.79.Anteil + Unb.Aunb.Anteil)




View(dfultimate)

dfultimate<-dfultimate[,c(1:11,38:73, 12:37)]





# write.csv(df_ultimate, "/Users/colinlinke/Documents/ProgR/Stat.-Praktikum/dfultimate.csv", row.names=FALSE)
