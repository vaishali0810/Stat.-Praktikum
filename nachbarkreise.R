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
                                         "LK Aichach-Friedberg, LK Augsburg",
                                         "LK Dillingen a.d.Donau,  LK Donau-Ries, LK Aichach-Friedberg, sK Augsburg, LK Günzburg, LK Unterallgäu, LK Ostallgäu, LK Landsberg a.Lech",
                                         "LK Donau-Ries, LK Augsburg, LK Günzburg",
                                         "LK Aichach-Friedberg, LK Augsburg, LK Dillingen a.d.Donau,LK Ansbach, LK Weißenburg-Gunzenhausen, LK Eichstätt, LK Neuburg-Schrobenhausen",
                                         "LK Dillingen a.d.Donau, LK Unterallgäu, LK Augsburg, LK Neu-Ulm",
                                         "LK Oberallgäu",
                                         "LK Günzburg, LK Unterallgäu",
                                         "LK Lindau, LK Unterallgäu, LK Ostallgäu, SK Kempten",
                                         "LK Oberallgäu, LK Unterallgäu,LK Augsburg, SK Kaufbeuren, LK Landsberg a.Lech, LK Weilheim-Schongau, LK Garmisch-Patenkirchen",
                                         "LK Augsburg, LK Neu-Ulm, LK Günzburg, LK Ostallgäu, LK Oberallgäu, SK Memmingen",
                                         "LK Ostallgäu",
                                         "LK Oberallgäu",
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
# "LK Bamberg": "LK Coburg", c,"LK Bayreuth","LK Forchheim", "SK Bamberg" (Erlangen, Neustadt an der Aisch-Bad Windsheim, Kitzigern, Schweinfurt, Haßberge)  
# "LK Bayreuth": "SK Bayreuth", "LK Lichtenfels", "LK Kulmbach", "LK Hof", "LK Forchheim", 
#                "LK Bamberg", "LK Wunsiedel i.Fichtelgebirge" (Tirschenreuth, neustadt an der Waldnaab, Amberg-Sulzbach, Nürnberger Land)
# "LK Coburg": "LK Kronach", "LK Lichtenfels", "LK Bamberg", "SK Coburg" (Haßberge)
# "LK Forchheim": "LK Bayreuth",  "LK Bamberg", (Nürnberger Land, erlangen-Höchstadt)
# "LK Hof": "SK Hof", "LK Kronach", "LK Kulmbach", "LK Bayreuth"
#"LK Kronach": "LK Hof", "LK Kulmbach", "LK Lichtenfels", "LK Coburg" (Hof)
#"LK Kulmbach": "LK Hof", "LK Kronach", "LK Bayreuth", "LK Lichtenfels"
#"LK Lichtenfels": "LK Coburg", "LK Kronach", "LK Kulmbach", "LK Bayreuth", "LK Bamberg"
#"LK Wunsiedel i.Fichtelgebirge":"LK Hof", "LK Bayreuth" (Tirschenreuth) 

oberfranken <- data.frame(bezirk = c("SK Bamberg","SK Bayreuth",
                                     "SK Coburg","SK Hof","LK Bamberg",
                                     "LK Bayreuth","LK Coburg","LK Forchheim",
                                     "LK Hof","LK Kronach","LK Kulmbach",
                                     "LK Lichtenfels","LK Wunsiedel i.Fichtelgebirge"),
                          
                          nachbarkreise = c("LK Bamberg",
                                            "LK Bayreuth",
                                            "LK Coburg",
                                            "LK Hof",
                                            "LK Coburg,LK Bayreuth,LK Forchheim, SK Bamberg, LK Erlangen, LK Neustadt a.d.Aisch-Bad Windsheim, LK Kitzigen, LK Schweinfurt, LK Haßberge",
                                            "SK Bayreuth, LK Lichtenfels, LK Kulmbach, LK Hof, LK Forchheim, Bamberg, LK Wunsiedel i.Fichtelgebirge, LK Tirschenreuth, LK Neustadt a.d.Waldnaab, LK Amberg-Sulzbach, LK Nürnberger Land",
                                            "LK Kronach, LK Lichtenfels, LK Bamberg, SK Coburg, LK Haßberge",
                                            "LK Bayreuth,  LK Bamberg, LK Nürnberger Land, LK Erlangen-Höchstadt",
                                            "SK Hof, LK Kronach, LK Kulmbach, LK Bayreuth",
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

oberbayern <- data.frame(bezirk = c("SK München" , "SK Ingolstadt" , "SK Rosenheim" ,
                                    "LK Altötting" , "LK Berchtesgadener Land" , 
                                    "LK Bad Tölz-Wolfratshausen" , "LK Dachau" ,
                                    "LK Ebersberg" , "LK Eichstätt" , "LK Erding" ,
                                    "LK Freising" , "LK Fürstenfeldbruck" ,
                                    "LK Garmisch-Partenkirchen" , "LK Landsberg a.Lech" ,
                                    "LK Miesbach" ,"LK Mühldorf a.Inn" , "LK München" ,
                                    "LK Neuburg-Schrobenhausen" , 
                                    "LK Pfaffenhofen a.d.Ilm" , "LK Rosenheim" , "LK Starnberg" , 
                                    "LK Traunstein" , "LK Weilheim-Schongau"),
                         
                         nachbarkreise = c( "LK Dachau, LK München, LK Fürstenfeldbruck",
                                            "LK Eichstätt, LK Pfaffenhofen a.d.Ilm, LK Neuburg-Schrobenhausen" ,
                                            "LK Rosenheim",
                                            "LK Traunstein, LK Mühldorf a.Inn, LK Rottal-Inn",
                                            "LK Traunstein",
                                            "LK Starnberg, LK München, LK Miesbach, LK Garmisch-Partenkirchen, LK Weilheim-Schongau" ,
                                            "LK Pfaffenhofen a.d.Ilm, LK Freising, SK München, LK München, LK Fürstenfeldbruck, LK Aichach-Friedberg",
                                            "LK Erding, LK Mühldorf a.Inn, LK Rosenheim, LK München",
                                            "LK Pfaffenhofen a.d.Ilm, SK Ingolstadt, LK Neuburg-Schrobenhausen, LK Roth, LK Neumarkt i.d.Oberpfalz, LK Kelheim, LK Donau-Ries, LK Weißenburg-Gunzenhausen",
                                            "LK Mühldorf a.Inn, LK Ebersberg, LK München, LK Freising, LK Landshut",
                                            "LK Erding, LK München, LK Dachau, LK Pfaffenhofen a.d.Ilm, LK Landshut, LK Kelheim",
                                            "SK München, LK München, LK Dachau,LK Starnberg, LK Landsberg a.Lech, LK Aichach-Friedberg",
                                            "LK Weilheim-Schongau, LK Bad Tölz-Wolfratshausen, LK Ostallgäu",
                                            "LK Fürstenfeldbruck, LK Starnberg, LK Weilheim-Schongau, LK Ostallgäu, LK Aichach-Friedberg, LK Augsburg",
                                            "LK München, LK Rosenheim, LK Bad Tölz-Wolfratshausen",
                                            "LK Altötting, LK Traunstein, LK Rosenheim, LK Ebersberg, LK Erding, LK Landshut, LK Rottal-Inn",
                                            "LK Dachau,LK Freising, LK Erding, LK Ebersberg, LK Rosenheim, LK Miesbach, LK Bad Tölz-Wolfratshausen, LK Starnberg, SK München",   
                                            "LK Eichstätt, LK Pfaffenhofen a.d.Ilm, SK Ingolstadt, LK Aichach-Friedberg, LK Donau-Ries", 
                                            "LK Eichstätt, SK Ingolstadt, LK Freising, LK Dachau, LK Neuburg-Schrobenhausen, LK Kelheim, LK Aichach-Friedberg",
                                            "LK Traunstein, LK Miesbach,LK München,LK Ebersberg, LK Mühldorf a.Inn",
                                            "LK Fürstenfeldbruck, LK München,LK Bad Tölz-Wolfratshausen , LK Weilheim-Schongau, LK Landsberg a.Lech",
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
                                             "LK Rhön-Grabfeld, LK Bad Kissingen, LK Haßberge, LK Kitzingen, LK Main-Spessart, LK Würzburg",
                                             "LK Main-Spessart, LK Schweinfurt, LK Kitzingen, LK Neustadt a.d.Aisch-Bad Windsheim",
                                             "LK Würzburg",
                                             "LK Aschaffenburg, LK Miltenberg",
                                             "LK Schweinfurt",
                                             "LK Rhön-Grabfeld, LK Schweinfurt, LK Main-Spessart"),
                           
                           regierungsbezirk = rep("Unterfranken", 12)
                           
)

#####
## Oberpfalz
#
#"LK Amberg-Sulzbach": "LK Neustadt a.d.Waldnaab","LK Schwandorf", "LK Neumarkt i.d.OPf.", "SK Amberg" (LK Bayreuth, LK Nürnberger-Land)  
#"LK Cham":"LK Regensburg", "LK Schwandorf"(LK Regen, LK Straubing-Bogen)
#"LK Neumarkt i.d.OPf.":"LK Amberg-Sulzbach", "LK Regensburg"(LK Nürnberger-Land, LK Kelheim, LK Eichstätt, LK Roth) 
#"LK Neustadt a.d.Waldnaab":"SK Weiden i.d.OPf.", "LK Tirschenreuth","LK Schwandorf", "LK Amberg-Sulzbach" (LK Bayreuth)
#"LK Regensburg":"LK Cham", "SK Regensburg", "LK Schwandorf", "LK Neumarkt i.d.OPf." (LK Straubing-Bogen, LK Kelheim)
#"LK Schwandorf":"LK Neustadt a.d.Waldnaab", "LK Cham", "LK Regensburg", "LK Neumarkt i.d.OPf.", "LK Amberg-Sulzbach"
#"LK Tirschenreuth":"LK Neustadt a.d.Waldnaab" (LK Bayreuth, LK Wunsiedl)
#"SK Amberg" :"LK Amberg-Sulzbach"
#"SK Regensburg":"LK Regensburg"
#"SK Weiden i.d.OPf.":"LK Neustadt a.d.Waldnaab"

oberpfalz <- data.frame(bezirk = c("LK Amberg-Sulzbach" , "LK Cham" ,
                                   "LK Neumarkt i.d.OPf.", "LK Neustadt a.d.Waldnaab" ,
                                   "LK Regensburg" , "LK Schwandorf" , "LK Tirschenreuth" ,
                                   "SK Amberg" , "SK Regensburg"  ,
                                   "SK Weiden i.d.OPf."),
                        
                        nachbarkreise = c("LK Neustadt a.d.Waldnaab,LK Schwandorf, LK Neumarkt i.d.OPf., SK Amberg, LK Bayreuth, LK Nürnberger-Land",
                                          "LK Regensburg, LK Schwandorf, LK Regen, LK Straubing-Bogen",
                                          "LK Amberg-Sulzbach, LK Regensburg, LK Nürnberger-Land, LK Kelheim, LK Eichstätt, LK Roth",
                                          "SK Weiden i.d.OPf., LK Tirschenreuth,LK Schwandorf, LK Amberg-Sulzbach, LK Bayreuth",
                                          "LK Cham, SK Regensburg, LK Schwandorf, LK Neumarkt i.d.OPf., LK Straubing-Bogen, LK Kelheim",
                                          "LK Neustadt a.d.Waldnaab, LK Cham, LK Regensburg, LK Neumarkt i.d.OPf., LK Amberg-Sulzbach",
                                          "LK Neustadt a.d.Waldnaab, LK Bayreuth, LK Wunsiedl",
                                          "LK Amberg-Sulzbach",
                                          "LK Regensburg",
                                          "LK Neustadt a.d.Waldnaab"),
                        
                        regierungsbezirk = rep("Oberpfalz", 10)
                        
)

######
## Mittelfranken
#
#"LK Roth": "SK Fürth", "SK Schwabach", "SK Nürnberg", "LK Weißenburg-Gunzenhausen", "LK Ansbach" (LK Neumarkt i.d.OPf., LK Eichstätt)
#"LK Nürnberger Land": "LK Roth","SK Nürnberg", "LK Erlangen-Höchstadt"  (LK Forchheim, LK Bayreuth,LK Amberg-Sulzbach, LK Neumarkt i.d.OPf. )
#"LK Neustadt a.d.Aisch-Bad Windsheim": "LK Erlangen-Höchstadt", "LK Fürth", "LK Ansbach", (LK Kitzigen, LK Bamberg, LK Würzburg)
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
                                              "LK Erlangen-Höchstadt, LK Fürth, LK Ansbach, LK Kitzigen, LK Bamberg, LK Würzburg",
                                              "LK Neustadt a.d.Aisch-Bad Windsheim, LK Fürth, LK Roth, LK Weißenburg-Gunzenhausen, LK Donau-Ries",
                                              "SK Erlangen, LK Erlangen-Höchstadt, SK Nürnberg, LK Fürth",
                                              "SK Erlangen, LK Erlangen-Höchstadt, LK Nürnberger Land, LK Roth, SK Schwabach,SK Fürth, LK Fürth",
                                              "LK Roth, LK Ansbach, LK Eichstätt, LK Donau-Ries",
                                              "LK Nürnberger Land, SK Nürnberg, SK Erlangen, LK Fürth, SK Fürth, LK Neustadt a.d.Aisch-Bad Windsheim, LK Bamberg, LK Forchheim",
                                              "LK Erlangen-Höchstadt, SK Erlangen, SK Fürth, SK Nürnberg, LK Roth, LK Ansbach, LK Neustadt a.d.Aisch-Bad Windsheim",
                                              "SK Nürnberg, LK Roth",
                                              "LK Erlangen-Höchstadt, SK Fürth, SK Nürnberg",
                                              "LK Ansbach"),
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
#"LK Deggendorf":"LK Regen", "LK Freyung-Grafenau", "LK Passau","LK Rottal-Inn", "LK Dingolfing-Landau", "LK Straubing-Bogen" 
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
                                              "LK Regen, LK Freyung-Grafenau, LK Passau,LK Rottal-Inn, LK Dingolfing-Landau, LK Straubing-Bogen",
                                              "LK Deggendorf, LK Freyung-Grafenau, LK Rottal-Inn", 
                                              "LK Passau", 
                                              "LK Dingolfing-Landau, LK Deggendorf, LK Passau, LK Landshut, LK Altötting, LK Mühldorf a. Inn",
                                              "LK Straubing-Bogen", 
                                              "LK Dingolfing-Landau, LK Deggendorf, LK Landshut, LK Cham, LK Regensburg", 
                                              "LK Landshut, LK Neumarkt i.d.OPf., LK Regensburg, LK Freising, LK Pfaffenhofen a.d.Ilm, LK Eichstätt"),
                            regierungsbezirk = rep("Niederbayern", 12)                  
)


# Alle zuammen:
nachbarkreise_list <- list(niederbayern, oberbayern, oberfranken, oberpfalz, mittelfranken, schwaben, unterfranken)
nachbarkreise <-Reduce(function(x, y) merge(x, y, all=TRUE), nachbarkreise_list, accumulate=FALSE)



###
# neue Spalte: Inzidenzen * Population

dfultimate <- dfultimate %>% mutate(inz.pop = inzidenz * population)

# nachbarkreise Inzidenz:
list_temp <- list()


outputlist<-list()

temp1 <- nachbarkreise$nachbarkreise
list_temp1 <- list()
for(i in 1:96){
  list_temp1[[i]] <- temp1[i]
}

list_temp2 <- as.list(lapply(list_temp1, function(x) 
  { unlist(str_split(x, ", "))}))

list_temp3 <- list()


temppop<-rep.int(0, 96)
for(j in seq_along(list_temp2)){
  temp_pop<- 0
  temp_Pop <- sum(temp_pop, popbay2[popbay2$district==j, 4])
  print(popbay2[popbay2$district==j, 4])
  print(temp_pop)
  temppop[j] <- temp_pop
}
temppop


temp1<-temp1[1]
temp1 <-  unlist(str_split(temp1, ", "))
temp1<-as.list(temp1)
temp5 <- unlist(str_split(temp1, ", "))

temp2 <- dfultimate$inz.pop[1:148]
temp3 <- list()
for(i in temp5){
  temp3[[i]] <- popbay2[popbay2$district==i,4]
}

temp3<-sum(unlist(temp3))

temp4 <- list()
for(i in temp5){
  temp4[[i]] <- dfultimate[dfultimate$district==i,43]
}

temp7 <- list()
for( i in temp5){
 temp7[[i]] <- as.data.frame(c(temp4[[i]])) 
}

temp8 <- as.data.frame(do.call(cbind, temp7))
temp9<-rowSums(temp8)
temp10 <- temp9/temp3



