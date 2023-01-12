
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
library(readr)
dfultimate <- read_csv("dfultimate.csv")

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

districtnames <- sort(unique(dfultimate$district))

neighboring <- nachbarkreise[[2]]
neighboring <- lapply(neighboring, function(x) {unlist(strsplit(x, ", "))})

View(df)

df2<-df[,c(1:4,34:35,5:33)]

View(df2)

df2$hotspot<-vector(mode="logical",length=nrow(df2))

View(df2)

df2<-df2[,c(1:6,36,7:35)]

districts<-unique(df2$district)

firstlist<-list()

for(i in 1:length(districts)){
  firstlist[[i]]<-df2[df2$district==districts[i],]
}

View(firstlist[[1]])

firstlist[[1]][2,4]>2*firstlist[[1]][1,4]&firstlist[[1]][2,5]>2*firstlist[[1]][1,5]

firstlist[[1]][24,4]>2*firstlist[[1]][23,4]&firstlist[[1]][24,5]>2*firstlist[[1]][23,5]

for(i in 1:length(firstlist)){
  for(j in 2:148){
    firstlist[[i]][j,7]<-firstlist[[i]][j,4]>2*firstlist[[i]][j-1,4]&firstlist[[i]][j,5]>2*firstlist[[i]][j-1,5]
  }
}

df3<-do.call(rbind.data.frame,firstlist)

View(df3)

df3$hotspotnb<-vector(mode="logical",length=nrow(df3))
#df3$neighboring <- neighboring
view(df3)
#df3 <- df3[,-38]

secondlist <- list()
df3 <- df3 %>% arrange(district)
for(i in 1:length(districts)){
  secondlist[[i]]<-df3[df3$district==districts[i],]
}

for (i in 1:96) {
  temp1 <- neighboring[[i]]
  temp1 <- unlist(temp1)
  temp2 <- rep.int(FALSE, 148)
  for (j in seq_along(temp1)) {
    tempdf <- df3 %>% filter(district == temp1[j])
    tempdf <- as.data.frame(tempdf)
    for (k in 1:148) {
      if(tempdf[k,7] == TRUE) {
        temp2[k] <- TRUE
      }
    }
  }
  secondlist[[i]][, 37] <- temp2
}

df4<-do.call(rbind.data.frame,secondlist)

df3short <- df3 %>% select(district, week, hotspot, hotspotnb)
view(df3short)
