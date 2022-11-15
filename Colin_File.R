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
  mutate(kreis=recode(kreis, München, Landeshauptstadt"SK München"                          "LK Traunstein"                       "LK München"                         
                       "SK Augsburg"                         "LK Rosenheim"                        "LK Augsburg"                        
                      "LK Schwandorf"                       "LK Unterallgäu"                      "LK Mühldorf a.Inn"                  
                      "LK Landshut"                         "LK Freising"                         "LK Ebersberg"                       
                       "LK Miltenberg"                       "LK Aschaffenburg"                    "LK Rottal-Inn"                      
                       "LK Dachau"                           "LK Pfaffenhofen a.d.Ilm"             "SK Ingolstadt"                      
                       "LK Roth"                             "LK Günzburg"                         "LK Nürnberger Land"                 
                      "LK Fürstenfeldbruck"                 "LK Dillingen a.d.Donau"              "LK Donau-Ries"                      
                       "LK Altötting"                        "LK Dingolfing-Landau"                "LK Kelheim"                         
                       "LK Bamberg"                          "LK Neustadt a.d.Aisch-Bad Windsheim" "LK Regensburg"                      
                       "LK Freyung-Grafenau"                 "LK Amberg-Sulzbach"                  "LK Neu-Ulm"                         
                       "LK Rhön-Grabfeld"                    "LK Neumarkt i.d.OPf."                "LK Berchtesgadener Land"            
                       "LK Passau"                           "LK Bayreuth"                         "LK Regen"                           
                       "LK Bad Tölz-Wolfratshausen"          "LK Aichach-Friedberg"                "LK Schweinfurt"                     
                       "LK Forchheim"                        "LK Miesbach"                         "SK Regensburg"                      
                       "LK Main-Spessart"                    "LK Ansbach"                          "SK Bayreuth"                        
                       "LK Cham"                             "LK Kitzingen"                        "LK Tirschenreuth"                   
                       "LK Eichstätt"                        "SK Landshut"                         "SK Rosenheim"                       
                       "LK Oberallgäu"                       "SK Fürth"                            "SK Aschaffenburg"                   
                       "LK Coburg"                           "LK Ostallgäu"                        "LK Neustadt a.d.Waldnaab"           
                       "LK Wunsiedel i.Fichtelgebirge"       "LK Deggendorf"                       "LK Lichtenfels"                     
                     "SK Nürnberg"                         "LK Weißenburg-Gunzenhausen"          "LK Erlangen-Höchstadt"              
                       "SK Schweinfurt"                      "LK Kulmbach"                         "LK Würzburg"                        
                       "SK Würzburg"                         "LK Fürth"                            "SK Schwabach"                       
                       "SK Memmingen"                        "LK Weilheim-Schongau"                "LK Bad Kissingen"                   
                       "SK Bamberg"                          "LK Straubing-Bogen"                  "LK Hof"                             
                       "LK Erding"                           "SK Erlangen"                         "LK Lindau"                          
                       "SK Amberg"                           "LK Starnberg"                        "LK Neuburg-Schrobenhausen"          
                       "LK Landsberg a.Lech"                 "LK Haßberge"                         "SK Kempten"                         
                       "SK Coburg"                           "LK Kronach"                          "SK Weiden i.d.OPf."                 
                       "LK Garmisch-Partenkirchen"           "SK Passau"                           "SK Hof"                             
                       "SK Straubing"                        "SK Kaufbeuren"                       "SK Ansbach"  "Roth" = "Mittelfranken", "Starnberg" = "Oberbayern")) %>%
  as.data.frame()
table(impfBayern$kreis)

table(dbayern$district)
