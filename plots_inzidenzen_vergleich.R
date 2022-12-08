## Plots

library(ggplot2)
library(lubridate)

## 1. Bayern Plot mit Cases

weekly_cases <- dbayern3 %>%
  group_by(date = cut(date, "week"))  %>% summarise(case = sum(cases))

weekly_cases$date <- as.Date(weekly_cases$date)


ggplot(data= weekly_cases, aes(x=date, y = case)) + geom_line(color = "black") +
  labs(x = "Datum", y = "Covid Cases", 
       title = "Wöchentliche Covid-Cases in Bayern") +
  geom_vline(xintercept = as.Date(c("2020-12-27", "2021-01-16", "2021-06-14")), color = "red")+
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )
             
          








## 2. Größte und Kleinste Population vergleich pro Bezirk

new <- dbayern3 %>%
  group_by(district, bezirk, date, male_anteil, female_anteil) #%>%
  summarize(inz = sum(inzidenz),
            case = sum(cases),
            m_teil = sum(male_anteil),
            f_teil = sum(female_anteil))

# Schwaben
augsburg <- subset(new, district =="SK Augsburg")
memmingen <- subset(new, district =="SK Memmingen")
 ggplot() + geom_line(data=augsburg, aes(x=date, y = inz), color = "red") + 
  geom_line(data=memmingen, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Covid-Infektionen pro 100.000 Einwohner in Augsburg & Memmingen (Schwaben)") +
   scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
   theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
   theme(axis.text.y = element_text(size = 10, face = "bold"))+
   theme(text = element_text(size = 8))+
   theme(panel.background = element_rect(fill = "white",
                                         colour = "white",
                                         size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "grey"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white"))
  

# Oberpfalz
regensburg <- subset(new, district =="LK Regensburg")
amberg <- subset(new, district =="SK Amberg")
ggplot() + geom_line(data=regensburg, aes(x=date, y = inz), color = "red") + 
  geom_line(data=amberg, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Regensburg und Amberg (Oberpfalz)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

#Niederbayern
passau <- subset(new, district =="LK Passau")
straubing <- subset(new, district =="SK Straubing")
ggplot() + geom_line(data=passau, aes(x=date, y = inz), color = "red") + 
  geom_line(data=straubing, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Passau und Straubing (Niederbayern)")+
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

# Unterfranken
aschaffenburg <- subset(new, district =="LK Aschaffenburg")
schweinfurt <- subset(new, district =="SK Schweinfurt")
ggplot() + geom_line(data=aschaffenburg, aes(x=date, y = inz), color = "red") + 
  geom_line(data=schweinfurt, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Aschaffenburg und Schweinfurt (Unterfranken)")+
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

# Oberfranken
bamberg <- subset(new, district =="LK Bamberg")
hof <- subset(new, district =="SK Hof")
ggplot() + geom_line(data=bamberg, aes(x=date, y = inz), color = "red") + 
  geom_line(data=hof, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Bamberg und Hof (Oberfranken)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

# Oberbayern
muenchen <- subset(new, district =="SK München")
rosenheim <- subset(new, district =="SK Rosenheim")
ggplot() + geom_line(data=muenchen, aes(x=date, y = inz), color = "red") + 
  geom_line(data=rosenheim, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner München und Rosenheim (Oberbayern)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

# Mittelfranken
nuernberg <- subset(new, district =="SK Nürnberg")
schwabach <- subset(new, district =="SK Schwabach")
ggplot() + geom_line(data=nuernberg, aes(x=date, y = inz), color = "red") + 
  geom_line(data=schwabach, aes(x=date,y=inz), color = "blue") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Covid-Infektionen pro 100.000 Einwohner Nürnberg & Schwabach (Mittelfranken)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 8))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

# 3. Anteil männlich - weiblich


# Schwaben
ggplot() + geom_line(data=augsburg, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=memmingen, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 8))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))



# Oberpfalz
ggplot() + geom_line(data=regensburg, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=amberg, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))
# Niederbayern
ggplot() + geom_line(data=passau, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=straubing, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

#Unterfranken
ggplot() + geom_line(data=aschaffenburg, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=schweinfurt, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

#Oberfranken
ggplot() + geom_line(data=bamberg, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=hof, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

#Oberbayern
ggplot() + geom_line(data=muenchen, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=rosenheim, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Tägliche Covid-Infektionen pro 100.000 Einwohner in Augsburg und Memmingen (Schwaben)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

#Mittelfranken
ggplot() + geom_line(data=nuernberg, aes(x=date, y = m_teil), color = "blue") + 
  geom_line(data=schwabach, aes(x=date,y=f_teil), color = "pink") +
  labs(x = "Datum", y = "Covid-Infektionen\npro 100.000 Einwohner", 
       title = "Covid-Infektionen pro 100.000 Einwohner in Nürnberg und Schwabach (Mittelfranken)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%d. %b %y") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 10, face = "bold"))+
  theme(text = element_text(size = 8))+
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))
