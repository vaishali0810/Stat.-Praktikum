kreise_mittelfranken<-c("LK Roth", "LK Nürnberger Land",
                       "LK Neustadt a.d.Aisch-Bad Windsheim",
                       "Ansbach","SK Fürth","SK Nürnberg",
                       "LK Weißenburg-Gunzenhausen",
                       "LK Erlangen-Höchstadt","LK Fürth",
                       "SK Schwabach","SK Erlangen","SK Ansbach")

kreise_niederbayern<-c("LK Landshut", "SK Landshut", "LK Dingolfing-Landau", 
                       "LK Freyung-Grafenau", "LK Passau", 
                       "LK Regen", "LK Deggendorf", "LK Passau",
                       "SK Passau", "LK Rottal-Inn", "SK Straubing", 
                       "LK Straubing-Bogen")

kreise_mittelfranken<-as.factor(kreise_mittelfranken)
h<-rep(0, 1488865)
h<-as.data.frame(h)
dbayern2<-cbind(dbayern,h)

for (i in levels(kreise_mittelfranken)){
if(dbayern2$district==i){
  h[i]<-"Mittelfranken"
  }
}  

#https://www.regierung.niederbayern.bayern.de/regierungsbezirk/landkreise/index.html