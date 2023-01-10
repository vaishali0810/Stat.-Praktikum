



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

