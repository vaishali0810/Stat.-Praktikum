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


# 2. Nachbarkreise von allen Bezirken

## Schwaben

# Aichach-Friedberg: "LK Donau-Ries", "SK Augsburg", "LK Augsburg", ("Neuburg-Schrobenhausen", "Pfaffenhofen a. d. Ilm", "Dachau, Fürstenfeldbruck, Landsberg am Lech)
# SK Augburg: "LK Aichach-Friedberg", "LK Augsburg"
# LK Augsburg: "LK Dillingen a.d.Donau",  "LK Donau-Ries", "LK Aichach-Friedberg", "sK Augsburg", "LK Günzburg", 
#              "LK Unterallgäu", "LK Ostallgäu" (LK Landsberg am Lech")
# LK Dillingen a.d.Donau : "LK Donau-Ries", "LK Augsburg", "LK Günzburg"
# "LK Donau-Ries": "LK Aichach-Friedberg", "LK Augsburg", "LK Dillingen a.d.Donau", (LK Ansbach, LK Weißenburg-Gunzenhausen, LK Eichstätt, LK Neuburg-Schrobenhausen")
# LK Günzburg: "LK Dillingen a.d.Donau", "LK Unterallgäu", "LK Augsburg", "LK Neu-Ulm"
# LK Lindau: "LK Oberallgäu"
# LK Neu-Ulm: "LK Günzburg", "LK Unterallgäu"
# "LK Oberallgäu": "LK Lindau", "LK Unterallgäu", "LK Ostallgäu", "SK Kempten"
# "LK Ostallgäu": "LK Oberallgäu", "LK Unterallgäu","LK Augsburg", "SK Kaufbeuren"(LK Landsberg am Lech, Weilheim-Schongau, Garmisch-Patenkirchen)
# "LK Unterallgäu": "LK Augsburg", "LK Neu-Ulm", "LK Günzburg", "LK Ostallgäu", "LK Oberallgäu", "SK Memmingen" (Memmingen) 
# "SK Kaufbeuren": "LK Ostallgäu"
# "SK Kempten": "LK Oberallgäu"
# "SK Memmingen": "LK Unterallgäu"

schwaben <- data.frame(bezirk = c("LK Aichach-Friedberg","SK Augsburg",
                                  "LK Augsburg","LK Dillingen a.d.Donau",
                                  "LK Donau-Ries","LK Günzburg","LK Lindau",
                                  "LK Neu-Ulm","LK Oberallgäu","LK Ostallgäu",
                                  "LK Unterallgäu","SK Kaufbeuren","SK Kempten",
                                  "SK Memmingen"),
                       
                       nachbarkreise = c("LK Donau-Ries, SK Augsburg, LK Augsburg, LK Neuburg-Schrobenhausen, LK Pfaffenhofen a.d.Ilm, LK Dachau, LK Fürstenfeldbruck, LK Landsberg a.Lech",
                                         "LK Dillingen a.d.Donau, LK Donau-Ries, LK Aichach-Friedberg, LK Günzburg, LK Unterallgäu, LK Ostallgäu, LK Landsberg a.Lech, LK Augsburg",
                                         "LK Dillingen a.d.Donau, LK Donau-Ries, LK Aichach-Friedberg, LK Günzburg, LK Unterallgäu, LK Ostallgäu, LK Landsberg a.Lech, SK Augsburg",
                                         "LK Donau-Ries, LK Günzburg, LK Augsburg",
                                         "LK Aichach-Friedberg, LK Augsburg, LK Dillingen a.d.Donau, LK Ansbach, LK Weißenburg-Gunzenhausen, LK Eichstätt, LK Neuburg-Schrobenhausen",
                                         "LK Dillingen a.d.Donau, LK Unterallgäu, LK Augsburg, LK Neu-Ulm",
                                         "LK Oberallgäu",
                                         "LK Günzburg, LK Unterallgäu",
                                         "LK Lindau, LK Unterallgäu, LK Ostallgäu, SK Kempten",
                                         "LK Oberallgäu, LK Unterallgäu, LK Augsburg, LK Landsberg a.Lech, LK Weilheim-Schongau, LK Garmisch-Partenkirchen, SK Kaufbeuren",
                                         "LK Augsburg, LK Neu-Ulm, LK Günzburg, LK Ostallgäu, LK Oberallgäu, SK Memmingen",
                                         "LK Oberallgäu, LK Unterallgäu, LK Augsburg, LK Landsberg a.Lech, LK Weilheim-Schongau, LK Garmisch-Partenkirchen, LK Ostallgäu",
                                         "LK Lindau, LK Unterallgäu, LK Ostallgäu, LK Oberallgäu",
                                         "LK Unterallgäu"),
                       
                       regierungsbezirk = rep("Schwaben", 14)
                       
)


#
#####
# Oberfranken
#
#"SK Bamberg": "LK Bamberg"
#"SK Bayreuth": "LK Bayreuth"
#"SK Coburg": "LK Coburg"
#"SK Hof": "LK Hof"
# "LK Bamberg": "LK Coburg", c,"LK Bayreuth","LK Forchheim", "SK Bamberg" (Erlangen, Neustadt an der Aisch-Bad Windsheim, Kitzingern, Schweinfurt, Haßberge)  
# "LK Bayreuth": "SK Bayreuth", "LK Lichtenfels", "LK Kulmbach", "LK Hof", "LK Forchheim", 
#                "LK Bamberg", "LK Wunsiedel i.Fichtelgebirge" (Tirschenreuth, neustadt an der Waldnaab, Amberg-Sulzbach, Nürnberger Land)
# "LK Coburg": "LK Kronach", "LK Lichtenfels", "LK Bamberg", "SK Coburg" (Haßberge)
# "LK Forchheim": "LK Bayreuth",  "LK Bamberg", (Nürnberger Land, erlangen-Höchstadt)
# "LK Hof": "SK Hof", "LK Kronach", "LK Kulmbach", "LK Bayreuth"
#"LK Kronach": "LK Hof", "LK Kulmbach", "LK Lichtenfels", "LK Coburg" (Hof)
#"LK Kulmbach": "LK Hof", "LK Kronach", "LK Bayreuth", "LK Lichtenfels"
#"LK Lichtenfels": "LK Coburg", "LK Kronach", "LK Kulmbach", "LK Bayreuth", "LK Bamberg"
#"LK Wunsiedel i.Fichtelgebirge":"LK Hof", "LK Bayreuth" (Tirschenreuth) 

oberfranken <- data.frame(bezirk = c("SK Bamberg", "SK Bayreuth",
                                     "SK Coburg", "SK Hof", "LK Bamberg",
                                     "LK Bayreuth", "LK Coburg", "LK Forchheim",
                                     "LK Hof", "LK Kronach", "LK Kulmbach",
                                     "LK Lichtenfels", "LK Wunsiedel i.Fichtelgebirge"),
                          
                          nachbarkreise = c("LK Coburg, LK Bayreuth, LK Forchheim, LK Erlangen-Höchstadt, LK Neustadt a.d.Aisch-Bad Windsheim, LK Kitzingen, LK Schweinfurt, LK Haßberge, LK Bamberg",
                                            "LK Lichtenfels, LK Kulmbach, LK Hof, LK Forchheim, LK Bamberg, LK Wunsiedel i.Fichtelgebirge, LK Tirschenreuth, LK Neustadt a.d.Waldnaab, LK Amberg-Sulzbach, LK Nürnberger Land, LK Bayreuth",
                                            "LK Kronach, LK Lichtenfels, LK Bamberg, LK Haßberge, LK Coburg",
                                            "LK Kronach, LK Kulmbach, LK Bayreuth, LK Hof",
                                            "LK Coburg, LK Bayreuth, LK Forchheim, LK Erlangen-Höchstadt, LK Neustadt a.d.Aisch-Bad Windsheim, LK Kitzingen, LK Schweinfurt, LK Haßberge, SK Bamberg",
                                            "LK Lichtenfels, LK Kulmbach, LK Hof, LK Forchheim, LK Bamberg, LK Wunsiedel i.Fichtelgebirge, LK Tirschenreuth, LK Neustadt a.d.Waldnaab, LK Amberg-Sulzbach, LK Nürnberger Land, SK Bayreuth",
                                            "LK Kronach, LK Lichtenfels, LK Bamberg, LK Haßberge, SK Coburg",
                                            "LK Bayreuth, LK Bamberg, LK Nürnberger Land, LK Erlangen-Höchstadt",
                                            "LK Kronach, LK Kulmbach, LK Bayreuth, SK Hof",
                                            "LK Hof, LK Kulmbach, LK Lichtenfels, LK Coburg, LK Hof",
                                            "LK Hof, LK Kronach, LK Bayreuth, LK Lichtenfels",
                                            "LK Coburg, LK Kronach, LK Kulmbach, LK Bayreuth, LK Bamberg",
                                            "LK Hof, LK Bayreuth, LK Tirschenreuth"),
                          
                          regierungsbezirk = rep("Oberfranken", 13)
                          
)
#####
# Oberbayern
#
#"SK München": "LK Dachau", "LK München", "LK Fürstenfeldbruck"
#"SK Ingolstadt":"LK Eichstätt", "LK Pfaffenhofen a.d.Ilm", "LK Neuburg-Schrobenhausen" 
#"SK Rosenheim":"LK Rosenheim"
#"LK Altötting":"LK Traunstein", "LK Mühldorf a.Inn", (Rottal-Inn)
#"LK Berchtesgadener Land": "LK Traunstein"
#"LK Bad Tölz-Wolfratshausen": "LK Starnberg", "LK München", "LK Miesbach", "LK Garmisch-Partenkirchen","LK Weilheim-Schongau" 
#"LK Dachau": "LK Pfaffenhofen a.d.Ilm", "LK Freising", "SK München", "LK München", "LK Fürstenfeldbruck" (LK Aichach-Friedberg)
#"LK Ebersberg": "LK Erding", "LK Mühldorf a.Inn", "LK Rosenheim", "LK München"
#"LK Eichstätt":"LK Pfaffenhofen a.d.Ilm", "SK Ingolstadt, "LK Neuburg-Schrobenhausen" (Roth, Neumarkt in der Oberpfalzt, Kelheim, Donau-Ries, Weißenburg-Gunzenhausen)
# "LK Erding":"LK Mühldorf a.Inn", "LK Ebersberg", "LK München", "LK Freising" (LK Landshut) 
# "LK Freising":"LK Erding", "LK München", "LK Dachau", "LK Pfaffenhofen a.d.Ilm" (Kelheim, Lndshut)
# "LK Fürstenfeldbruck":"SK München", "LK München", "LK Dachau","LK Starnberg","LK Landsberg a.Lech"  (LK Aichach-Friedberg)
#"LK Garmisch-Partenkirchen":"LK Weilheim-Schongau", "LK Bad Tölz-Wolfratshausen" (LK Ostallgäu)
#"LK Landsberg a.Lech":"LK Fürstenfeldbruck", "LK Starnberg", "LK Weilheim-Schongau"(LK Ostallgäu, LK Aichach-Friedberg, LK Augsburg)
#"LK Miesbach":"LK München", "LK Rosenheim", "LK Bad Tölz-Wolfratshausen"
#"LK Mühldorf a.Inn":"LK Altötting", "LK Traunstein", "LK Rosenheim", "LK Ebersberg", "LK Erding" (Landshut, Rottal-Inn)
#"LK München":"LK Dachau","LK Freising", "LK Erding", "LK Ebersberg", "LK Rosenheim", "LK Miesbach","LK Bad Tölz-Wolfratshausen", "LK Starnberg", "SK München"    
#"LK Neuburg-Schrobenhausen": "LK Eichstätt", "LK Pfaffenhofen a.d.Ilm","SK Ingolstadt", (LK Aichach-Friedberg, LK Donau-Ries)
#"LK Pfaffenhofen a.d.Ilm":"LK Eichstätt", "SK Ingolstadt", "LK Freising", "LK Dachau", "LK Neuburg-Schrobenhausen"(Kelheim,LK Aichach-Friedberg )
#"LK Rosenheim":"LK Traunstein", "LK Miesbach","LK München","LK Ebersberg" , "LK Mühldorf a.Inn"
#"LK Starnberg":"LK Fürstenfeldbruck", "LK München","LK Bad Tölz-Wolfratshausen" , "LK Weilheim-Schongau", "LK Landsberg a.Lech"
#"LK Traunstein":"LK Mühldorf a.Inn", "LK Altötting", "LK Berchtesgadener Land", "LK Rosenheim"
#"LK Weilheim-Schongau":"LK Landsberg a.Lech", "LK Starnberg", "LK Bad Tölz-Wolfratshausen", "LK Garmisch-Partenkirchen" (LK Ostallgäu)

oberbayern <- data.frame(bezirk = c("SK München" ,"SK Ingolstadt" ,"SK Rosenheim" ,
                                    "LK Altötting" ,"LK Berchtesgadener Land" , 
                                    "LK Bad Tölz-Wolfratshausen" ,"LK Dachau" ,
                                    "LK Ebersberg" ,"LK Eichstätt" ,"LK Erding" ,
                                    "LK Freising" ,"LK Fürstenfeldbruck" ,
                                    "LK Garmisch-Partenkirchen" ,"LK Landsberg a.Lech" ,
                                    "LK Miesbach" ,"LK Mühldorf a.Inn" ,"LK München" ,
                                    "LK Neuburg-Schrobenhausen" , 
                                    "LK Pfaffenhofen a.d.Ilm" ,"LK Rosenheim" ,"LK Starnberg" , 
                                    "LK Traunstein" ,"LK Weilheim-Schongau"),
                         
                         nachbarkreise = c("LK Dachau, LK Freising, LK Erding, LK Ebersberg, LK Rosenheim, LK Miesbach, LK Bad Tölz-Wolfratshausen, LK Starnberg, LK Fürstenfeldbruck, LK München",
                                           "LK Eichstätt, LK Pfaffenhofen a.d.Ilm, LK Neuburg-Schrobenhausen" ,
                                           "LK Traunstein, LK Miesbach, LK München, LK Ebersberg, LK Mühldorf a.Inn, LK Rosenheim",
                                           "LK Traunstein, LK Mühldorf a.Inn, LK Rottal-Inn",
                                           "LK Traunstein",
                                           "LK Starnberg, LK München, LK Miesbach, LK Garmisch-Partenkirchen, LK Weilheim-Schongau" ,
                                           "LK Pfaffenhofen a.d.Ilm, LK Freising, SK München, LK München, LK Fürstenfeldbruck, LK Aichach-Friedberg",
                                           "LK Erding, LK Mühldorf a.Inn, LK Rosenheim, LK München",
                                           "LK Pfaffenhofen a.d.Ilm, SK Ingolstadt, LK Neuburg-Schrobenhausen, LK Roth, LK Neumarkt i.d.OPf., LK Kelheim, LK Donau-Ries, LK Weißenburg-Gunzenhausen",
                                           "LK Mühldorf a.Inn, LK Ebersberg, LK München, LK Freising, LK Landshut",
                                           "LK Erding, LK München, LK Dachau, LK Pfaffenhofen a.d.Ilm, LK Landshut, LK Kelheim",
                                           "SK München, LK München, LK Dachau, LK Starnberg, LK Landsberg a.Lech, LK Aichach-Friedberg",
                                           "LK Weilheim-Schongau, LK Bad Tölz-Wolfratshausen, LK Ostallgäu",
                                           "LK Fürstenfeldbruck, LK Starnberg, LK Weilheim-Schongau, LK Ostallgäu, LK Aichach-Friedberg, LK Augsburg",
                                           "LK München, LK Rosenheim, LK Bad Tölz-Wolfratshausen",
                                           "LK Altötting, LK Traunstein, LK Rosenheim, LK Ebersberg, LK Erding, LK Landshut, LK Rottal-Inn",
                                           "LK Dachau, LK Freising, LK Erding, LK Ebersberg, LK Rosenheim, LK Miesbach, LK Bad Tölz-Wolfratshausen, LK Starnberg, LK Fürstenfeldbruck, SK München",   
                                           "LK Eichstätt, LK Pfaffenhofen a.d.Ilm, SK Ingolstadt, LK Aichach-Friedberg, LK Donau-Ries", 
                                           "LK Eichstätt, SK Ingolstadt, LK Freising, LK Dachau, LK Neuburg-Schrobenhausen, LK Kelheim, LK Aichach-Friedberg",
                                           "LK Traunstein, LK Miesbach, LK München, LK Ebersberg, LK Mühldorf a.Inn, SK Rosenheim",
                                           "LK Fürstenfeldbruck, LK München, LK Bad Tölz-Wolfratshausen, LK Weilheim-Schongau, LK Landsberg a.Lech",
                                           "LK Mühldorf a.Inn, LK Altötting, LK Berchtesgadener Land, LK Rosenheim",
                                           "LK Landsberg a.Lech, LK Starnberg, LK Bad Tölz-Wolfratshausen, LK Garmisch-Partenkirchen, LK Ostallgäu"),
                         
                         regierungsbezirk = rep("Oberbayern", 23)
                         
)

#####
## Unterfranken
#
#"LK Aschaffenburg": "LK Main-Spessart", "LK Miltenberg", "SK Aschaffenburg"
#"LK Haßberge":"LK Rhön-Grabfeld", "LK Schweinfurt" (LK Coburg, LK Bamberg)
#"LK Kitzingen":"LK Schweinfurt", "LK Würzburg", (LK Bamberg, LK Neustadt a. d. Aisch.Bad Windsheim)
#"LK Main-Spessart":"LK Bad Kissingen", "LK Schweinfurt", "LK Würzburg", "LK Miltenberg", "LK Aschaffenburg"
#"LK Miltenberg":"LK Aschaffenburg", "SK Aschaffenburg", "LK Main-Spessart"
#"LK Rhön-Grabfeld":"LK Bad Kissingen", "LK Schweinfurt", "LK Haßberge"
#"LK Schweinfurt":"LK Rhön-Grabfeld", "LK Bad Kissingen", "LK Haßberge", "LK Kitzingen". "LK Main-Spessart", "LK Würzburg" (LK Bamberg)
#"LK Würzburg":LK Main-Spessart", "LK Schweinfurt", "LK Kitzingen" ( LK Neustadt a. d. Aisch.Bad Windsheim))
#"SK Würzburg":"LK Würzburg"
#"SK Aschaffenburg":"LK Aschaffenburg", "LK Miltenberg"
#"SK Schweinfurt":"LK Schweinfurt"
#"LK Bad Kissingen":"LK Rhön-Grabfeld", "LK Schweinfurt", "LK Main-Spessart"

unterfranken <- data.frame(bezirk = c("LK Aschaffenburg",
                                      "LK Haßberge" , "LK Kitzingen" ,
                                      "LK Main-Spessart" , "LK Miltenberg" ,
                                      "LK Rhön-Grabfeld" , "LK Schweinfurt" ,
                                      "LK Würzburg" , "SK Würzburg" ,
                                      "SK Aschaffenburg" , "SK Schweinfurt","LK Bad Kissingen"),
                           
                           nachbarkreise = c("LK Main-Spessart, LK Miltenberg, SK Aschaffenburg",
                                             "LK Rhön-Grabfeld, LK Schweinfurt, LK Coburg, LK Bamberg",
                                             "LK Schweinfurt, LK Würzburg, LK Bamberg, LK Neustadt a.d.Aisch-Bad Windsheim",
                                             "LK Bad Kissingen, LK Schweinfurt, LK Würzburg, LK Miltenberg, LK Aschaffenburg",
                                             "LK Aschaffenburg, SK Aschaffenburg, LK Main-Spessart",
                                             "LK Bad Kissingen, LK Schweinfurt, LK Haßberge",
                                             "LK Rhön-Grabfeld, LK Bad Kissingen, LK Haßberge, LK Kitzingen, LK Main-Spessart, LK Würzburg, SK Schweinfurt",
                                             "LK Main-Spessart, LK Schweinfurt, LK Kitzingen, LK Neustadt a.d.Aisch-Bad Windsheim, SK Würzburg",
                                             "LK Main-Spessart, LK Schweinfurt, LK Kitzingen, LK Neustadt a.d.Aisch-Bad Windsheim, LK Würzburg",
                                             "LK Main-Spessart, LK Miltenberg, LK Aschaffenburg",
                                             "LK Rhön-Grabfeld, LK Bad Kissingen, LK Haßberge, LK Kitzingen, LK Main-Spessart, LK Würzburg, LK Schweinfurt",
                                             "LK Rhön-Grabfeld, LK Schweinfurt, LK Main-Spessart"),
                           
                           regierungsbezirk = rep("Unterfranken", 12)
                           
)

#####
## Oberpfalz
#
#"LK Amberg-Sulzbach": "LK Neustadt a.d.Waldnaab","LK Schwandorf", "LK Neumarkt i.d.OPf.", "SK Amberg" (LK Bayreuth, LK Nürnberger Land)  
#"LK Cham":"LK Regensburg", "LK Schwandorf"(LK Regen, LK Straubing-Bogen)
#"LK Neumarkt i.d.OPf.":"LK Amberg-Sulzbach", "LK Regensburg"(LK Nürnberger Land, LK Kelheim, LK Eichstätt, LK Roth) 
#"LK Neustadt a.d.Waldnaab":"SK Weiden i.d.OPf.", "LK Tirschenreuth","LK Schwandorf", "LK Amberg-Sulzbach" (LK Bayreuth)
#"LK Regensburg":"LK Cham", "SK Regensburg", "LK Schwandorf", "LK Neumarkt i.d.OPf." (LK Straubing-Bogen, LK Kelheim)
#"LK Schwandorf":"LK Neustadt a.d.Waldnaab", "LK Cham", "LK Regensburg", "LK Neumarkt i.d.OPf.", "LK Amberg-Sulzbach"
#"LK Tirschenreuth":"LK Neustadt a.d.Waldnaab" (LK Bayreuth, LK Wunsiedel i.Fichtelgebirge)
#"SK Amberg" :"LK Amberg-Sulzbach"
#"SK Regensburg":"LK Regensburg"
#"SK Weiden i.d.OPf.":"LK Neustadt a.d.Waldnaab"

oberpfalz <- data.frame(bezirk = c("LK Amberg-Sulzbach" , "LK Cham" ,
                                   "LK Neumarkt i.d.OPf.", "LK Neustadt a.d.Waldnaab" ,
                                   "LK Regensburg" , "LK Schwandorf" , "LK Tirschenreuth" ,
                                   "SK Amberg" , "SK Regensburg"  ,
                                   "SK Weiden i.d.OPf."),
                        
                        nachbarkreise = c("LK Neustadt a.d.Waldnaab, LK Schwandorf, LK Neumarkt i.d.OPf., LK Bayreuth, LK Nürnberger Land, SK Amberg",
                                          "LK Regensburg, LK Schwandorf, LK Regen, LK Straubing-Bogen",
                                          "LK Amberg-Sulzbach, LK Regensburg, LK Nürnberger Land, LK Kelheim, LK Eichstätt, LK Roth",
                                          "LK Tirschenreuth, LK Schwandorf, LK Amberg-Sulzbach, LK Bayreuth, SK Weiden i.d.OPf.",
                                          "LK Cham, LK Schwandorf, LK Neumarkt i.d.OPf., LK Straubing-Bogen, LK Kelheim, SK Regensburg",
                                          "LK Neustadt a.d.Waldnaab, LK Cham, LK Regensburg, LK Neumarkt i.d.OPf., LK Amberg-Sulzbach",
                                          "LK Neustadt a.d.Waldnaab, LK Bayreuth, LK Wunsiedel i.Fichtelgebirge",
                                          "LK Neustadt a.d.Waldnaab, LK Schwandorf, LK Neumarkt i.d.OPf., LK Bayreuth, LK Nürnberger Land, LK Amberg-Sulzbach",
                                          "LK Cham, LK Schwandorf, LK Neumarkt i.d.OPf., LK Straubing-Bogen, LK Kelheim, LK Regensburg",
                                          "LK Tirschenreuth, LK Schwandorf, LK Amberg-Sulzbach, LK Bayreuth, LK Neustadt a.d.Waldnaab"),
                        
                        regierungsbezirk = rep("Oberpfalz", 10)
                        
)

######
## Mittelfranken
#
#"LK Roth": "SK Fürth", "SK Schwabach", "SK Nürnberg", "LK Weißenburg-Gunzenhausen", "LK Ansbach" (LK Neumarkt i.d.OPf., LK Eichstätt)
#"LK Nürnberger Land": "LK Roth","SK Nürnberg", "LK Erlangen-Höchstadt"  (LK Forchheim, LK Bayreuth,LK Amberg-Sulzbach, LK Neumarkt i.d.OPf. )
#"LK Neustadt a.d.Aisch-Bad Windsheim": "LK Erlangen-Höchstadt", "LK Fürth", "LK Ansbach", (LK Kitzingen, LK Bamberg, LK Würzburg)
#"LK Ansbach":"LK Neustadt a.d.Aisch-Bad Windsheim", "LK Fürth", ,"LK Roth", "LK Weißenburg-Gunzenhausen" (LK Donau-Ries)
#"SK Fürth":"SK Erlangen", "LK Erlangen-Höchstadt", "SK Nürnberg", "LK Fürth"
#"SK Nürnberg":"SK Erlangen", "LK Erlangen-Höchstadt", "LK Nürnberger Land", "LK Roth", "SK Schwabach","SK Fürth", "LK Fürth" 
#"LK Weißenburg-Gunzenhausen": "LK Roth", "LK Ansbach" (LK Eichstätt, LK Donau-Ries)
#"LK Erlangen-Höchstadt":"LK Nürnberger Land", "SK Nürnberg", "SK Erlangen", "LK Fürth", "SK Fürth", "LK Neustadt a.d.Aisch-Bad Windsheim (LK Bamberg, LK Forchheim) 
#"LK Fürth":"LK Erlangen-Höchstadt", "SK Erlangen", "SK Fürth", "SK Nürnberg", "LK Roth", LK Ansbach", "LK Neustadt a.d.Aisch-Bad Windsheim"
#"SK Schwabach":"SK Nürnberg", "LK Roth"
#"SK Erlangen": "LK Erlangen-Höchstadt, "SK Fürth", "SK Nürnberg"
#"SK Ansbach": "LK Ansbach"

mittelfranken <- data.frame(bezirk = c("LK Roth","LK Nürnberger Land",
                                       "LK Neustadt a.d.Aisch-Bad Windsheim","LK Ansbach",
                                       "SK Fürth","SK Nürnberg","LK Weißenburg-Gunzenhausen",
                                       "LK Erlangen-Höchstadt","LK Fürth","SK Schwabach",
                                       "SK Erlangen","SK Ansbach"), 
                            
                            nachbarkreise = c("SK Fürth, SK Schwabach, SK Nürnberg, LK Weißenburg-Gunzenhausen, LK Ansbach, LK Neumarkt i.d.OPf., LK Eichstätt",
                                              "LK Roth, SK Nürnberg, LK Erlangen-Höchstadt, LK Forchheim, LK Bayreuth, LK Amberg-Sulzbach, LK Neumarkt i.d.OPf.", 
                                              "LK Erlangen-Höchstadt, LK Fürth, LK Ansbach, LK Kitzingen, LK Bamberg, LK Würzburg",
                                              "LK Neustadt a.d.Aisch-Bad Windsheim, LK Fürth, LK Roth, LK Weißenburg-Gunzenhausen, LK Donau-Ries, SK Ansbach",
                                              "SK Erlangen, LK Erlangen-Höchstadt, SK Nürnberg, LK Fürth",
                                              "SK Erlangen, LK Erlangen-Höchstadt, LK Nürnberger Land, LK Roth, SK Schwabach, SK Fürth, LK Fürth",
                                              "LK Roth, LK Ansbach, LK Eichstätt, LK Donau-Ries",
                                              "LK Nürnberger Land, SK Nürnberg, SK Erlangen, LK Fürth, SK Fürth, LK Neustadt a.d.Aisch-Bad Windsheim, LK Bamberg, LK Forchheim",
                                              "LK Erlangen-Höchstadt, SK Erlangen, SK Fürth, SK Nürnberg, LK Roth, LK Ansbach, LK Neustadt a.d.Aisch-Bad Windsheim",
                                              "SK Nürnberg, LK Roth",
                                              "LK Erlangen-Höchstadt, SK Fürth, SK Nürnberg",
                                              "LK Neustadt a.d.Aisch-Bad Windsheim, LK Fürth, LK Roth, LK Weißenburg-Gunzenhausen, LK Donau-Ries, LK Ansbach"),
                            regierungsbezirk = rep("Mittelfranken", 12) 
)

#######
## Niederbayern
#
#"LK Landshut":"LK Straubing-Bogen","LK Dingolfing-Landau", "LK Rottal-Inn", "LK Kelheim"(LK Regensburg, LK Mühldorf a.Inn, LK Erding, LK Freising, )
#"SK Landshut": "LK Landshut"
#"LK Dingolfing-Landau":"LK Straubing-Bogen", "LK Deggendorf", "LK Rottal-Inn", "LK Landshut"
#"LK Freyung-Grafenau":"LK Regen", "LK Landshut", "LK Deggendorf"
#"LK Regen": "LK Freyung-Grafenau", "LK Deggendorf", "LK Straubing-Bogen"(LK Cham)
#"LK Deggendorf":"LK Regen", "LK Freyung-Grafenau", "LK Passau", 3*"LK Rottal-Inn", "LK Dingolfing-Landau", "LK Straubing-Bogen" 
#"LK Passau":"LK Deggendorf", "LK Freyung-Grafenau", "LK Rottal-Inn" 
#"SK Passau":"LK Passau"
#"LK Rottal-Inn":"LK Dingolfing-Landau", "LK Deggendorf", "LK Passau", "LK Landshut" (LK Altötting, LK Mühldorf a. Inn)
#"SK Straubing":"LK Straubing-Bogen"
#"LK Straubing-Bogen":"LK Dingolfing-Landau", "LK Deggendorf", "LK Landshut" (LK Cham, LK Regensburg)
#"LK Kelheim": "LK Landshut", (LK Neumarkt i.d.OPf., LK Regensburg, LK Freising, LK Pfaffenhofen a.d.Ilm", LK Eichstätt)
niederbayern <- data.frame( bezirk = c("LK Landshut","SK Landshut",
                                       "LK Dingolfing-Landau","LK Freyung-Grafenau",
                                       "LK Regen","LK Deggendorf",
                                       "LK Passau","SK Passau","LK Rottal-Inn",
                                       "SK Straubing","LK Straubing-Bogen","LK Kelheim"),
                            nachbarkreise = c("LK Straubing-Bogen, LK Dingolfing-Landau, LK Rottal-Inn, LK Kelheim, LK Regensburg, LK Mühldorf a.Inn, LK Erding, LK Freising",
                                              "LK Landshut", 
                                              "LK Straubing-Bogen, LK Deggendorf, LK Rottal-Inn, LK Landshut",
                                              "LK Regen, LK Landshut, LK Deggendorf", 
                                              "LK Freyung-Grafenau, LK Deggendorf, LK Straubing-Bogen, LK Cham",
                                              "LK Regen, LK Freyung-Grafenau, LK Passau, LK Rottal-Inn, LK Dingolfing-Landau, LK Straubing-Bogen",
                                              "LK Deggendorf, LK Freyung-Grafenau, LK Rottal-Inn, SK Passau", 
                                              "LK Deggendorf, LK Freyung-Grafenau, LK Rottal-Inn, LK Passau", 
                                              "LK Dingolfing-Landau, LK Deggendorf, LK Passau, LK Landshut, LK Altötting, LK Mühldorf a.Inn",
                                              "LK Dingolfing-Landau, LK Deggendorf, LK Landshut, LK Cham, LK Regensburg, LK Straubing-Bogen", 
                                              "LK Dingolfing-Landau, LK Deggendorf, LK Landshut, LK Cham, LK Regensburg, SK Straubing", 
                                              "LK Landshut, LK Neumarkt i.d.OPf., LK Regensburg, LK Freising, LK Pfaffenhofen a.d.Ilm, LK Eichstätt"),
                            regierungsbezirk = rep("Niederbayern", 12)                  
)


# Alle zuammen:
nachbarkreise_list <- list(niederbayern, oberbayern, oberfranken, oberpfalz, mittelfranken, schwaben, unterfranken)
nachbarkreise <-Reduce(function(x, y) merge(x, y, all=TRUE), nachbarkreise_list, accumulate=FALSE)




###
# neue Spalte: Inzidenzen * Population

#dfultimate <- dfultimate %>% mutate(inz.pop = inzidenz * population)


colnames(nachbarkreise) <- c("district", "neighboring districts", "bezirk")
dfultimate <- merge(df_ultimate, nachbarkreise, by = c("district", "bezirk"), all.x = TRUE, all.y = TRUE)


####### Unbekannt aufteilen auf Mann und Frau

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
  dplyr::mutate(A00.04.Anteil = M.A00.04.Anteil + F.A00.04.Anteil,
                A05.14.Anteil = M.A05.14.Anteil + F.A05.14.Anteil,
                A15.34.Anteil = M.A15.34.Anteil + F.A15.34.Anteil,
                A35.59.Anteil = M.A35.59.Anteil + F.A35.59.Anteil,
                A60.79.Anteil = M.A60.79.Anteil + F.A60.79.Anteil,
                A80.Anteil = M.A80.Anteil + F.A80.Anteil,
                Aunb.Anteil = M.Aunb.Anteil + F.Aunb.Anteil,
                M.Anteil = M.A00.04.Anteil + M.A05.14.Anteil +
                  M.A15.34.Anteil + M.A35.59.Anteil + M.A60.79.Anteil + M.Aunb.Anteil,
                F.Anteil = F.A00.04.Anteil + F.A05.14.Anteil +
                  F.A15.34.Anteil + F.A35.59.Anteil + F.A60.79.Anteil + F.Aunb.Anteil,
  )
            



View(dfultimate)

dfultimate<-dfultimate[,c(1:11,38:42,44:73, 12:37,43)]



im_schwab <- dfultimate %>% filter(bezirk == "Schwaben")
im_Oberba <- dfultimate %>% filter(bezirk == "Oberbayern")
im_Oberpf <- dfultimate %>% filter(bezirk == "Oberpfalz")
im_MittelF <- dfultimate %>% filter(bezirk == "Mittelfranken")
im_UnterFr <- dfultimate %>% filter(bezirk == "Unterfranken")
im_OberFr <- dfultimate %>% filter(bezirk == "Oberfranken")
im_NiederB <- dfultimate %>% filter(bezirk == "Niederbayern")

im_schwab <- im_schwab %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))
im_Oberba <- im_Oberba %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))
im_Oberpf <- im_Oberpf %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))
im_MittelF <- im_MittelF %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))
im_UnterFr <- im_UnterFr %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))

im_OberFr <- im_OberFr %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))

im_NiederB <- im_NiederB %>% group_by(week) %>% summarise(kr_erstimpf = sum(kr_erstimpf), kr_zweitimpf = sum(kr_zweitimpf),
                                                        kr_drittimpf = sum(kr_drittimpf), kr_viertimpf = sum(kr_viertimpf))

schwabpop <- popbay2 %>% filter(bezirk == "Schwaben") 
schwabpop <- sum(schwabpop$population)
oberbbpop <- popbay2 %>% filter(bezirk == "Oberbayern") 
oberbbpop <- sum(oberbbpop$population)
niedbbpop <- popbay2 %>% filter(bezirk == "Niederbayern") 
niedbbpop <- sum(niedbbpop$population)
mittfbpop <- popbay2 %>% filter(bezirk == "Mittelfranken") 
mittfbpop <- sum(mittfbpop$population)
untefbpop <- popbay2 %>% filter(bezirk == "Unterfranken") 
untefbpop <- sum(untefbpop$population)
oberfbpop <- popbay2 %>% filter(bezirk == "Oberfranken") 
oberfbpop <- sum(oberfbpop$population)
oberpbpop <- popbay2 %>% filter(bezirk == "Oberpfalz") 
oberpbpop <- sum(oberpbpop$population)


im_schwab <- im_schwab %>% mutate(rate_erstimpf = cumsum(kr_erstimpf) / schwabpop,
                                  rate_zweitimpf=cumsum(kr_zweitimpf) / schwabpop,
                                  rate_drittimpf=cumsum(kr_drittimpf) / schwabpop,
                                  rate_viertimpf=cumsum(kr_viertimpf) / schwabpop)

im_Oberba <- im_Oberba %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / oberbbpop,
                                                            rate_zweitimpf=cumsum(kr_zweitimpf) / oberbbpop,
                                                            rate_drittimpf=cumsum(kr_drittimpf) / oberbbpop,
                                                            rate_viertimpf=cumsum(kr_viertimpf) / oberbbpop)

im_Oberpf <- im_Oberpf %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / oberpbpop,
                                                            rate_zweitimpf=cumsum(kr_zweitimpf) / oberpbpop,
                                                            rate_drittimpf=cumsum(kr_drittimpf) / oberpbpop,
                                                            rate_viertimpf=cumsum(kr_viertimpf) / oberpbpop)

im_MittelF <- im_MittelF %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / mittfbpop,
                                                              rate_zweitimpf=cumsum(kr_zweitimpf) / mittfbpop,
                                                              rate_drittimpf=cumsum(kr_drittimpf) / mittfbpop,
                                                              rate_viertimpf=cumsum(kr_viertimpf) / mittfbpop)

im_UnterFr <- im_UnterFr %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / untefbpop,
                                                              rate_zweitimpf=cumsum(kr_zweitimpf) / untefbpop,
                                                              rate_drittimpf=cumsum(kr_drittimpf) / untefbpop,
                                                              rate_viertimpf=cumsum(kr_viertimpf) / untefbpop)

im_OberFr <- im_OberFr %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / oberfbpop,
                                                            rate_zweitimpf=cumsum(kr_zweitimpf) / oberfbpop,
                                                            rate_drittimpf=cumsum(kr_drittimpf) / oberfbpop,
                                                            rate_viertimpf=cumsum(kr_viertimpf) / oberfbpop)

im_NiederB <- im_NiederB %>% group_by(week) %>% dplyr::mutate(rate_erstimpf = cumsum(kr_erstimpf) / niedbbpop,
                                                              rate_zweitimpf=cumsum(kr_zweitimpf) / niedbbpop,
                                                              rate_drittimpf=cumsum(kr_drittimpf) / niedbbpop,
                                                              rate_viertimpf=cumsum(kr_viertimpf) / niedbbpop)

im_schwab <- im_schwab %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Schwaben")

im_Oberba <- im_Oberba %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Oberbayern")

im_Oberpf <- im_Oberpf %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Oberpfalz")

im_MittelF <- im_MittelF %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Mittelfranken")

im_UnterFr <- im_UnterFr %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Unterfranken")

im_OberFr <- im_OberFr %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
  mutate(bezirk = "Oberfranken")

im_NiederB <- im_NiederB %>% select(week, rate_erstimpf, rate_zweitimpf, rate_drittimpf, rate_viertimpf) %>% 
mutate(bezirk = "Niederbayern")

dfultimate <- dfultimate[, -(6: 9)]

impfungen <- rbind(im_schwab, im_Oberba, im_OberFr, im_Oberpf, im_MittelF, im_UnterFr, im_NiederB)
dfultimate <- merge(dfultimate, impfungen, by = c("week", "bezirk"))


#### Kalendarwochen des Jahres sind anders als unsere week Variable
#### deswegen neue Zeile als Referenz
dfultimate <- dfultimate %>% 
  mutate(Kalendarwoche=dfultimate$week+3)

###############
### erste bis 9 Kalendarwoche ist Phase 0 der Pandemie
### Datensatz für Phase 0, 1, 2 etc.

p<-c(1:9)
nullt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(10:20)
erst<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(21:39)
zweit<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
##### Jahr 2020 hat 52 Wochen bei unserem erzeugten Kalendarwochen,
##### die basierend sind auf week ist diese neue Var durchgängig
##### aber kein Problem >>>Lösung ist 52 + Wochen neues Jahr
p<-c(40:(52+8))
dritt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+9):(52+23))
viert<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+24):(52+30))
fünft<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+51))
sechst<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+52):(52+151))
siebt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(21:30)
zweit_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(31:39)
zweit_b<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+39))
sechst_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+40):(52+51))
sechst_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)







## calculating the incidence of the neighboring districts, one weighted by population, one unweighted


districtnames <- sort(unique(dfultimate$district))
districtnames




### nachbarkreise[[1]] schon sortiert

nachbarkreise[[1]]
identical(districtnames, nachbarkreise[[1]])
neighboring <- nachbarkreise[[2]]

emptylistinz <- list()
emptylistpop <- list()

neighboring <- lapply(neighboring, function(x) {unlist(strsplit(x, ", "))})

shortdf <- dfultimate %>% select(district, week, population, inzidenz) %>% arrange(district, week)
View(shortdf)

shortdf %>% filter(district == districtnames[1])

View(nachbarkreise)

for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp1 <- unlist(temp1)
  temp2 <- 0
  temp3 <- 0
  #print(temp1)
  for (j in seq_along(temp1)) {
    tempdf <- shortdf %>% filter(district == temp1[j])
    tempdf <- as.data.frame(tempdf)
    #print(tempdf)
    temp2 <- (temp2 + tempdf[, 4]*tempdf[3, 3])
    temp3 <- (temp3 + tempdf[3, 3])
  }
  #print(temp2)
  #print(temp3)
  emptylistinz[[i]] <- temp2
  emptylistpop[[i]] <- temp3
}
#emptylistinz
#emptylistpop

### sum(populationLK * InzidenzLK) / sum( population LK)


for ( i in 1:96) {
  emptylistinz[[i]] <- emptylistinz[[i]] / emptylistpop [[i]]
}




vectorinz <- unlist(emptylistinz)

dfultimate <- dfultimate %>% arrange(district, week)
dfultimate$weightednbinz  <- vectorinz




unweightedlist <- list()
for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp1 <- unlist(temp1)
  temp2 <- length(temp1)
  temp3 <- 0
  #print(temp1)
  for (j in seq_along(temp1)) {
    tempdf <- shortdf %>% filter(district == temp1[j])
    tempdf <- as.data.frame(tempdf)
    temp3 <- (temp3 + tempdf[, 4])
  }
  unweightedlist[[i]] <- (temp3/temp2)
}

vectorinz2 <- unlist(unweightedlist)
dfultimate$unweightednbinz <- vectorinz2


View(dfultimate)


#write.csv(dfultimate, "C:/Users/kyril/OneDrive/Documents/Statistische Software/Stat.-Praktikum/dfultimate.csv", row.names=FALSE)
