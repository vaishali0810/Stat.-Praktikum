dfultimate <- dfultimate %>% 
  mutate(Kalendarwoche=dfultimate$week+3)

###############


p<-c(1:9)
auftretten<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c(10:20)
erst<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c(21:39)
zweit<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c(40:(52+8))
dritt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c((52+9):(52+23))
viert<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c((52+24):(52+30))
fünft<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c((52+31):(52+51))
sechst<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
p<-c((52+52):(52+151))
sechst<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

