# https://www.regierung.oberbayern.bayern.de/regierungsbezirk/landkreise-Staedte/index.html
Oberbayern <- c("SK München", "SK Ingolstadt", "SK Rosenheim","LK Altötting", "LK Berchtesgadener Land", 
                "LK Bad Tölz-Wolfratshausen", "LK Dachau", "LK Ebersberg", 
                "LK Eichstätt", "LK Erding", "LK Freising", " LK Fürstenfeldbruck",
                "LK Garmisch-Partenkirchen", " LK Landsberg a.Lech", "LK Miesbach",
                "LK Mühldorf a.Inn", "LK München", "LK Neuburg-Schrobenhausen", 
                "LK Pfaffenhofen a.d.Ilm", "LK Rosenheim", "LK Starnberg", 
                "LK Traunstein", " LK Weilheim-Schongau", "LK Erding", "LK Bad Reichenhall")

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"SK München" = "Oberbayern", "SK Ingolstadt" = "Oberbayern", "SK Rosenheim" = "Oberbayern",
                                        "LK Altötting" = "Oberbayern", "LK Berchtesgadener Land" = "Oberbayern", 
                                        "LK Bad Tölz-Wolfratshausen" = "Oberbayern", "LK Dachau" = "Oberbayern",
                                        "LK Ebersberg" = "Oberbayern", "LK Eichstätt" = "Oberbayern", "LK Erding" = "Oberbayern",
                                        "LK Freising" = "Oberbayern", " LK Fürstenfeldbruck" = "Oberbayern",
                                        "LK Garmisch-Partenkirchen" = "Oberbayern", " LK Landsberg a.Lech" = "Oberbayern",
                                        "LK Miesbach" = "Oberbayern","LK Mühldorf a.Inn" = "Oberbayern", "LK München" = "Oberbayern",
                                        "LK Neuburg-Schrobenhausen" = "Oberbayern", 
                                        "LK Pfaffenhofen a.d.Ilm" = "Oberbayern", "LK Rosenheim" = "Oberbayern", "LK Starnberg" = "Oberbayern", 
                                        "LK Traunstein" = "Oberbayern", " LK Weilheim-Schongau" = "Oberbayern", 
                                        "LK Erding" = "Oberbayern", "LK Bad Reichenhall" = "Oberbayern"))

# https://www.regierung.unterfranken.bayern.de/regierungsbezirk/landkreise_staedte/index.html
unterfranken <- c("LK Aschaffenburg", "LK Aschaffenburg", "LK Haßberge", "LK Kitzingen",
                  "LK Main-Spessart", "LK Miltenberg", "LK Rhön-Grabfeld", "LK Schweinfurt",
                  "LK Würzburg", "SK Würzburg", "SK Aschaffenburg", "SK Schweinfurt")

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Aschaffenburg" = "Unterfranken", "LK Aschaffenburg" = "Unterfranken",
                                        "LK Haßberge" = "Unterfranken", "LK Kitzingen" = "Unterfranken",
                                        "LK Main-Spessart" = "Unterfranken", "LK Miltenberg" = "Unterfranken",
                                        "LK Rhön-Grabfeld" = "Unterfranken", "LK Schweinfurt" = "Unterfranken",
                                        "LK Würzburg" = "Unterfranken", "SK Würzburg" = "Unterfranken",
                                        "SK Aschaffenburg" = "Unterfranken", "SK Schweinfurt" = "Unterfranken"))

# https://www.regierung.oberpfalz.bayern.de/regierungsbezirk/landkreise_staedte_gemeinden/index.html
oberpfalz <- c("LK Amberg-Sulzbach", "LK Cham", "LK Neumarkt i.d.OPf.", "LK Neustadt a.d.Waldnaab",
               "LK Regensburg", "LK Schwandorf", "LK Tirschenreuth", "SK Amberg", "SK Regensburg",
               "SK Weiden i.d.OPf.")

dbayern<-dbayern%>%mutate(bezirk=recode(bezirk,"LK Amberg-Sulzbach" = "Oberpfalz", "LK Cham" = "Oberpfalz",
                                        "LK Neumarkt i.d.OPf."= "Oberpfalz", "LK Neustadt a.d.Waldnaab" = "Oberpfalz",
                                        "LK Regensburg" = "Oberpfalz", "LK Schwandorf" = "Oberpfalz", "LK Tirschenreuth" = "Oberpfalz",
                                        "SK Amberg" = "Oberpfalz", "SK Regensburg"  = "Oberpfalz",
                                        "SK Weiden i.d.OPf."  = "Oberpfalz"))

