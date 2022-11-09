dbayern<-data[data$state=="Bayern",]

vector2<-as.vector(summary(dbayern$district))
vector2 # insgesamt 100 districts, 96 districts mit Beobachtungen, districts
# sind schon der Größe nach sortiert
bayern_cases<-ggplot(data=dbayern,mapping=aes(x=dbayern$date,y=dbayern$cases))+geom_line()
bayern_cases

vector22<-c(summary(dbayern$district)[1:10])
d<-as.data.frame(as.table(vector22))

View(d)
library(ggplot2)
ggplot(data=d,mapping=aes(x=d$Var1, y=d$Freq))+geom_bar(stat = "identity", position = "dodge")
