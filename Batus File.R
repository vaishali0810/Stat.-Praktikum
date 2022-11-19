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



