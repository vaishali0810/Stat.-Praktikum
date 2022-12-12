dfcombined <- read.csv("dfcombined.csv", header = TRUE, sep =",")
View(dfcombined)

# weekly_cases <- dbayern3 %>%
#   group_by(date = cut(date, "week"))  %>% summarise(case = sum(cases))
# 
# weekly_cases$date <- as.Date(weekly_cases$date)

dfcombined$date <- as.Date(dfcombined$date)

dfcombined <- dfcombined %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

dfcombined <- dfcombined %>% arrange(district, date)
View(dfcombined)

vector33<-c(unique(dfcombined$district))
Storage01<-list()
for(i in 1:length(vector33)){
  Storage01[[i]]<-dfcombined[dfcombined$district==vector33[i],]
}
View(Storage01[[1]])


library(lubridate)



Storage_new<-list()
for(i in 1:length(vector33)){
  Storage_new[[i]]<-data.frame(matrix(NA, ncol = 26, nrow = 148))
  colnames(Storage_new[[i]])<-colnames(Storage01[[1]])
}

View(Storage_new[[1]])

for(i in 1:length(vector33)){
  Storage_new[[i]][, 2] <- Storage01[[i]][1, 2]
  Storage_new[[i]][, 3] <- Storage01[[i]][1, 3]
  Storage_new[[i]][, 26] <- c(1:148)
}
View(Storage01[[69]])
