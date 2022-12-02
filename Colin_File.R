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
