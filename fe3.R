dfultimate <- dfultimate %>% 
  mutate(Kalendarwoche=dfultimate$week+3)

###############


p<-c(1:9)
nullt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

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
siebt<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(21:30)
zweit_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c(31:39)
zweit_b<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+31):(52+39))
sechst_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)

p<-c((52+40):(52+51))
sechst_a<-subset(dfultimate,dfultimate$Kalendarwoche%in%p)
