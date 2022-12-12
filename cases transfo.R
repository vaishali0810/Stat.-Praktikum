dfcombined <- read.csv("dfcombined.csv", header = TRUE, sep =",")
View(dfcombined)



weekly_cases <- dbayern3 %>%
  group_by(date = cut(date, "week"))  %>% summarise(case = sum(cases))

weekly_cases$date <- as.Date(weekly_cases$date)


vector33<-c(unique(dfcombined$district))
Storage01<-list()
for(i in 1:length(vector33)){
  Storage01[[i]]<-dfcombined[dfcombined$district==vector33[i],]
}
View(Storage01[[1]])


library(lubridate)

Storage022<-Storage01
for(i in 1:length(vector33)){
  Storage022[[i]]<-Storage01[[i]]%>%group_by(date=unique(cut(dates,"week")))
}

View(Storage022[[1]])

Storage02[[1]]<-unique(Storage02[[1]])

View(Storage02[[1]])

# Storage03<-list()
# for(i in 1:length(vector33)){
#   Storage03[[i]]<-Storage01[[i]]%>%group_by(date=cut(date,"week"))%>%
#     summarise(M.A00-A04=sum(M.A00-A04),M.A05-A14=sum(M.A05-A14))
# }

Storage_new<-list()
for(i in 1:length(vector33)){
  Storage_new[[i]]<-data.frame(matrix(NA,ncol=25,nrow=148))
  colnames(Storage_new[[i]])<-colnames(Storage01[[1]])
}

View(Storage_new[[1]])

for(i in 1:length(vector33)){
  Storage_new[[i]][,2]<-Storage01[[i]][1,2]
  Storage_new[[i]][,3]<-Storage01[[i]][1,3]
}

Storage01[[69]] <- Storage01[[69]] %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Order_Date)
Storage01[[69]]$date <- as.Date(Storage01[[69]]$date)
str(Storage01[[69]])
