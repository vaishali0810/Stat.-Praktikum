dbayern$bezirk<-as.factor(dbayern$bezirk)
vector2<-c(summary(dbayern$bezirk)[1:7])
vector2names<-names(vector2)
Stor<-list()
for(i in 1:length(vector2names)){
  Stor[[i]]<-dbayern[dbayern$bezirk==vector2names[i],]
}
# View(Storage)

## Nach Datum sortieren
library(dplyr)
Stor2<-Stor
for(i in 1:length(vector2names)){
  Stor2[[i]]<-Stor2[[i]]%>%arrange(date)
}
# View(Storage2)

## Nach Gender sortieren
Stor3<-Stor2
for(i in 1:length(vector2names)){
  Stor3[[i]]<-Stor3[[i]]%>%arrange(gender)
}
# View (Storage3)

## Nach age group sortieren
Stor4<-Stor3
for(i in 1:length(vector2names)){
  Stor4[[i]]<-Stor4[[i]]%>%arrange(age_group)
}
View(Stor4[[1]])

S41<-as.data.frame(Stor4[1])
S41%>%
  ggplot(aes(date, cases, colour = gender)) + 
  geom_line(stat = "identity")

ggplot(S41, aes(date,cases,color = gender,
                        linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

ggplot(S41, aes(date,cases,color = gender,
                linetype = gender)) +
  geom_point(size=0.1)+
  geom_smooth()

broom augument




a<-min(Storage9[[1]]$date)
b<-max(Storage9[[1]]$date)
c<-seq(as.Date(a), as.Date(b), "days")
c<-as.data.frame(c)
colnames(c)[1] <- "date"
y<-merge(Storage9[[1]],c, by="date",
         all.x=TRUE, all.y=TRUE)
v<-y$cases
index<-is.na(v)
v[index]<-0
y$cases<-v

v<-y$gender
index<-is.na(v)
v[index]<-"egal"
y$gender<-v


#####


ymal<-subset(y,gender=="M")
yfem<-subset(y,gender=="W")
yunk<-subset(y,gender=="unbekannt")

smal<-aggregate(x = ymal$cases,               
          by = list(ymal$date),              
          FUN = sum)
smal<-mutate(smal, gender ="M")

sfem<-aggregate(x = yfem$cases,               
                by = list(yfem$date),              
                FUN = sum)
sfem<-mutate(sfem, gender ="W")

sunk<-aggregate(x = yunk$cases,               
                by = list(yunk$date),              
                FUN = sum)
sunk<-mutate(sunk, gender ="U")

sall<-rbind(smal,sfem,sunk)

ggplot(sall, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)


y2 <- y2 %>% arrange(y2, district, date, age_group, cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         MA15cases=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+cases)/15)

df91<-as.data.frame(ymal, stringsAsFactors = FALSE)
ggplot(sall, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)

