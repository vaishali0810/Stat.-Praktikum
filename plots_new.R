

#### Storage1 bis Storage9 einlesen

# Mittelfranken cases
a1<-min(Storage9[[1]]$date)
b1<-max(Storage9[[1]]$date)
c1<-seq(as.Date(a1), as.Date(b1), "days")
c1<-as.data.frame(c1)
colnames(c1)[1] <- "date"
y1<-merge(Storage9[[1]],c1, by="date",
          all.x=TRUE, all.y=TRUE)
v1<-y1$cases
index1<-is.na(v1)
v1[index1]<-0
y1$cases<-v1

v1<-y1$gender
index1<-is.na(v1)
v1[index1]<-"egal"
y1$gender<-v1

ymal1<-subset(y1,gender=="M")
yfem1<-subset(y1,gender=="W")
yunk1<-subset(y1,gender=="unbekannt")

smal1<-aggregate(x = ymal1$cases,               
                 by = list(ymal1$date),              
                 FUN = sum)
smal1<-mutate(smal1, gender ="M")

sfem1<-aggregate(x = yfem1$cases,               
                 by = list(yfem1$date),              
                 FUN = sum)
sfem1<-mutate(sfem1, gender ="W")

sunk1<-aggregate(x = yunk1$cases,               
                 by = list(yunk1$date),              
                 FUN = sum)
sunk1<-mutate(sunk1, gender ="U")

sall1<-rbind(smal1,sfem1,sunk1)

plot1<-ggplot(sall1, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Mittelfranken")

plot1



# Niederbayern cases
a2<-min(Storage9[[2]]$date)
b2<-max(Storage9[[2]]$date)
c2<-seq(as.Date(a2), as.Date(b2), "days")
c2<-as.data.frame(c2)
colnames(c2)[1] <- "date"
y2<-merge(Storage9[[2]],c2, by="date",
          all.x=TRUE, all.y=TRUE)
v2<-y2$cases
index2<-is.na(v2)
v2[index2]<-0
y2$cases<-v2

v2<-y2$gender
index2<-is.na(v2)
v2[index2]<-"egal"
y2$gender<-v2

ymal2<-subset(y2,gender=="M")
yfem2<-subset(y2,gender=="W")
yunk2<-subset(y2,gender=="unbekannt")

smal2<-aggregate(x = ymal2$cases,               
                 by = list(ymal2$date),              
                 FUN = sum)
smal2<-mutate(smal2, gender ="M")

sfem2<-aggregate(x = yfem2$cases,               
                 by = list(yfem2$date),              
                 FUN = sum)
sfem2<-mutate(sfem2, gender ="W")

sunk2<-aggregate(x = yunk2$cases,               
                 by = list(yunk2$date),              
                 FUN = sum)
sunk2<-mutate(sunk2, gender ="U")

sall2<-rbind(smal2,sfem2,sunk2)

plot2<-ggplot(sall2, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Niederbayern")

plot2


# Oberbayern cases
a3<-min(Storage9[[3]]$date)
b3<-max(Storage9[[3]]$date)
c3<-seq(as.Date(a3), as.Date(b3), "days")
c3<-as.data.frame(c3)
colnames(c3)[1] <- "date"
y3<-merge(Storage9[[3]],c3, by="date",
          all.x=TRUE, all.y=TRUE)
v3<-y3$cases
index3<-is.na(v3)
v3[index3]<-0
y3$cases<-v3

v3<-y3$gender
index3<-is.na(v3)
v3[index3]<-"egal"
y3$gender<-v3

ymal3<-subset(y3,gender=="M")
yfem3<-subset(y3,gender=="W")
yunk3<-subset(y3,gender=="unbekannt")

smal3<-aggregate(x = ymal3$cases,               
                 by = list(ymal3$date),              
                 FUN = sum)
smal3<-mutate(smal3, gender ="M")

sfem3<-aggregate(x = yfem3$cases,               
                 by = list(yfem3$date),              
                 FUN = sum)
sfem3<-mutate(sfem3, gender ="W")

sunk3<-aggregate(x = yunk3$cases,               
                 by = list(yunk3$date),              
                 FUN = sum)
sunk3<-mutate(sunk3, gender ="U")

sall3<-rbind(smal3,sfem3,sunk3)

plot3<-ggplot(sall3, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberbayern")

plot3



# Oberfranken cases
a4<-min(Storage9[[4]]$date)
b4<-max(Storage9[[4]]$date)
c4<-seq(as.Date(a4), as.Date(b4), "days")
c4<-as.data.frame(c4)
colnames(c4)[1] <- "date"
y4<-merge(Storage9[[4]],c4, by="date",
          all.x=TRUE, all.y=TRUE)
v4<-y4$cases
index4<-is.na(v4)
v4[index4]<-0
y4$cases<-v4

v4<-y4$gender
index4<-is.na(v4)
v4[index4]<-"egal"
y4$gender<-v4

ymal4<-subset(y4,gender=="M")
yfem4<-subset(y4,gender=="W")
yunk4<-subset(y4,gender=="unbekannt")

smal4<-aggregate(x = ymal4$cases,               
                 by = list(ymal4$date),              
                 FUN = sum)
smal4<-mutate(smal4, gender ="M")

sfem4<-aggregate(x = yfem4$cases,               
                 by = list(yfem4$date),              
                 FUN = sum)
sfem4<-mutate(sfem4, gender ="W")

sunk4<-aggregate(x = yunk4$cases,               
                 by = list(yunk4$date),              
                 FUN = sum)
sunk4<-mutate(sunk4, gender ="U")

sall4<-rbind(smal4,sfem4,sunk4)

plot4<-ggplot(sall4, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberfranken")

plot4


# Oberpfalz cases
a5<-min(Storage9[[5]]$date)
b5<-max(Storage9[[5]]$date)
c5<-seq(as.Date(a5), as.Date(b5), "days")
c5<-as.data.frame(c5)
colnames(c5)[1] <- "date"
y5<-merge(Storage9[[5]],c5, by="date",
          all.x=TRUE, all.y=TRUE)
v5<-y5$cases
index5<-is.na(v5)
v5[index5]<-0
y5$cases<-v5

v5<-y5$gender
index5<-is.na(v5)
v5[index5]<-"egal"
y5$gender<-v5

ymal5<-subset(y5,gender=="M")
yfem5<-subset(y5,gender=="W")
yunk5<-subset(y5,gender=="unbekannt")

smal5<-aggregate(x = ymal5$cases,               
                 by = list(ymal5$date),              
                 FUN = sum)
smal5<-mutate(smal5, gender ="M")

sfem5<-aggregate(x = yfem5$cases,               
                 by = list(yfem5$date),              
                 FUN = sum)
sfem5<-mutate(sfem5, gender ="W")

sunk5<-aggregate(x = yunk5$cases,               
                 by = list(yunk5$date),              
                 FUN = sum)
sunk5<-mutate(sunk5, gender ="U")

sall5<-rbind(smal5,sfem5,sunk5)

plot5<-ggplot(sall5, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Oberpfalz")

plot5


# Schwaben cases
a6<-min(Storage9[[6]]$date)
b6<-max(Storage9[[6]]$date)
c6<-seq(as.Date(a6), as.Date(b6), "days")
c6<-as.data.frame(c6)
colnames(c6)[1] <- "date"
y6<-merge(Storage9[[6]],c6, by="date",
          all.x=TRUE, all.y=TRUE)
v6<-y6$cases
index6<-is.na(v6)
v6[index6]<-0
y6$cases<-v6

v6<-y6$gender
index6<-is.na(v6)
v6[index6]<-"egal"
y6$gender<-v6

ymal6<-subset(y6,gender=="M")
yfem6<-subset(y6,gender=="W")
yunk6<-subset(y6,gender=="unbekannt")

smal6<-aggregate(x = ymal6$cases,               
                 by = list(ymal6$date),              
                 FUN = sum)
smal6<-mutate(smal6, gender ="M")

sfem6<-aggregate(x = yfem6$cases,               
                 by = list(yfem6$date),              
                 FUN = sum)
sfem6<-mutate(sfem6, gender ="W")

sunk6<-aggregate(x = yunk6$cases,               
                 by = list(yunk6$date),              
                 FUN = sum)
sunk6<-mutate(sunk6, gender ="U")

sall6<-rbind(smal6,sfem6,sunk6)

plot6<-ggplot(sall6, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)+labs(title="Schwaben")

plot6


# Unterfranken cases
a7<-min(Storage9[[7]]$date)
b7<-max(Storage9[[7]]$date)
c7<-seq(as.Date(a7), as.Date(b7), "days")
c7<-as.data.frame(c7)
colnames(c7)[1] <- "date"
y7<-merge(Storage9[[7]],c7, by="date",
          all.x=TRUE, all.y=TRUE)
v7<-y7$cases
index7<-is.na(v7)
v7[index7]<-0
y7$cases<-v7

v7<-y7$gender
index7<-is.na(v7)
v7[index7]<-"egal"
y7$gender<-v7

ymal7<-subset(y7,gender=="M")
yfem7<-subset(y7,gender=="W")
yunk7<-subset(y7,gender=="unbekannt")

smal7<-aggregate(x = ymal7$cases,               
                 by = list(ymal7$date),              
                 FUN = sum)
smal7<-mutate(smal7, gender ="M")

sfem7<-aggregate(x = yfem7$cases,               
                 by = list(yfem7$date),              
                 FUN = sum)
sfem7<-mutate(sfem7, gender ="W")

sunk7<-aggregate(x = yunk7$cases,               
                 by = list(yunk7$date),              
                 FUN = sum)
sunk7<-mutate(sunk7, gender ="U")

sall7<-rbind(smal7,sfem7,sunk7)

plot7<-ggplot(sall7, aes(Group.1,x,color = gender)) +geom_line(size=0.1)+
  labs(title="Unterfranken")

plot7



## Inzidenzen
Mittelfranken1<-new[new$bezirk=="Mittelfranken",]
Niederbayern1<-new[new$bezirk=="Niederbayern",]
Oberbayern1<-new[new$bezirk=="Oberbayern",]
Oberfranken1<-new[new$bezirk=="Oberfranken",]
Oberpfalz1<-new[new$bezirk=="Oberpfalz",]
Schwaben1<-new[new$bezirk=="Schwaben",]
Unterfranken1<-new[new$bezirk=="Unterfranken",]

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot() + geom_line(data=Mittelfranken1, aes(x=date, y =inz),color="#999999") +
  geom_line(data=Niederbayern1, aes(x=date,y=inz),color="#E69F00") +
  geom_line(data=Oberbayern1,aes(x=date,y=inz),color="#56B4E9")+
  geom_line(data=Oberfranken1,aes(x=date,y=inz),color="#009E73")+
  geom_line(data=Oberpfalz1,aes(x=date,y=inz),color="#F0E442")+
  geom_line(data=Schwaben1,aes(x=date,y=inz),color="#0072B2")+
  geom_line(data=Unterfranken1,aes(x=date,y=inz),color="#D55E00")+
   labs(x = "Datum", y = "Covid-Inzidenz", 
       title = "Covid-Inzidenz in Bayern") +
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

