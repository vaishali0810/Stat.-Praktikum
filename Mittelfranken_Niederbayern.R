kreise_mittelfranken<-c("LK Roth", "LK Nürnberger Land",
                       "LK Neustadt a.d.Aisch-Bad Windsheim",
                       "LK Ansbach","SK Fürth","SK Nürnberg",
                       "LK Weißenburg-Gunzenhausen",
                       "LK Erlangen-Höchstadt","LK Fürth",
                       "SK Schwabach","SK Erlangen","SK Ansbach")

kreise_niederbayern<-c("LK Landshut", "SK Landshut", "LK Dingolfing-Landau", 
                       "LK Freyung-Grafenau", "LK Passau", 
                       "LK Regen", "LK Deggendorf",
                       "SK Passau", "LK Rottal-Inn", "SK Straubing", 
                       "LK Straubing-Bogen")

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Roth"="Mittelfranken","LK Nürnberger Land"="Mittelfranken",
                                     "LK Neustadt a.d.Aisch-Bad Windsheim"="Mittelfranken","LK Ansbach"="Mittelfranken",
                                     "SK Fürth"="Mittelfranken","SK Nürnberg"="Mittelfranken","LK Weißenburg-Gunzenhausen"="Mittelfranken",
                                     "LK Erlangen-Höchstadt"="Mittelfranken","LK Fürth"="Mittelfranken","SK Schwabach"="Mittelfranken",
                                     "SK Erlangen"="Mittelfranken","SK Erlangen"="Mittelfranken","SK Ansbach"="Mittelfranken"))

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Landshut"="Niederbayern","SK Landshut"="Niederbayern",
                                        "LK Dingolfing-Landau"="Niederbayern","LK Freyung-Grafenau"="Niederbayern",
                                        "LK Regen"="Niederbayern","LK Deggendorf"="Niederbayern",
                                        "LK Passau"="Niederbayern","SK Passau"="Niederbayern","LK Rottal-Inn"="Niederbayern",
                                        "SK Straubing"="Niederbayern","LK Straubing-Bogen"="Niederbayern"))


#https://www.regierung.niederbayern.bayern.de/regierungsbezirk/landkreise/index.html