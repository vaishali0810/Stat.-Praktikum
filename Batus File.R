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
y
ggplot(y, aes(date,cases,color = gender,
              linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()