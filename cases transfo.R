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


Storage012<-Storage_new

View(Storage012[[1]])

Storage012[[69]]<-Storage01[[69]]%>%select(date,`M.A00-04`,week)

df_comb_week<-dfcombined%>%group_by(district,week)%>%summarise(M.A00.04 =sum(M.A00.04),
                                                                    M.A05.14 = sum(M.A05.14),
                                                                    M.A15.34 = sum(M.A15.34),    
                                                                    M.A35.59 = sum(M.A35.59),
                                                                    M.A60.79 = sum(M.A60.79), 
                                                                    M.A80. = sum(M.A80.),
                                                                    M.Aunb = sum(M.Aunb),
                                                                    `F.A00.04`=sum(`F.A00.04`),
                                                                    F.A05.14 = sum(F.A05.14),
                                                                    F.A15.34 = sum(F.A15.34),    
                                                                    F.A35.59 = sum(F.A35.59),
                                                                    F.A60.79 = sum(F.A60.79), 
                                                                    F.A80. = sum(F.A80.),
                                                                    F.Aunb = sum(F.Aunb),
                                                                    `Unb.A00.04`=sum(`Unb.A00.04`),
                                                                    Unb.A05.14 = sum(Unb.A05.14),
                                                                    Unb.A15.34 = sum(Unb.A15.34),    
                                                                    Unb.A35.59 = sum(Unb.A35.59),
                                                                    Unb.A60.79 = sum(Unb.A60.79), 
                                                                    Unb.A80. = sum(Unb.A80.),
                                                                    Unb.Aunb = sum(Unb.Aunb),
                                                                    total_cases = sum(total_cases),
                                                                    .groups="keep")
identical(sum(dfcombined$total_cases), sum(df_comb_week$total_cases))

View(Storage012[[69]])




