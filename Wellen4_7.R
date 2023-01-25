pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling", index=c("district", "week"))

df4 <- df4 %>% 
  mutate(Kalendarwoche = df4$week+3)

p<-c(1:9)
nullt<-subset(df4, df4$Kalendarwoche%in%p)

p<-c(9:20)
erst<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(20:39)
zweit<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(39:(52+8))
dritt<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+9-1):(52+23))
viert<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+24-1):(52+30))
fünft<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+31-1):(52+51))
sechst<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+52-1):(52+151))
siebt<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(20:30)
zweit_a<-subset(df4,df4$Kalendarwoche%in%p)

p<-c(30:39)
zweit_b<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+31-1):(52+39))
sechst_a<-subset(df4,df4$Kalendarwoche%in%p)

p<-c((52+40-1):(52+51))
sechst_b<-subset(df4,df4$Kalendarwoche%in%p)


list_wellen <- list(nullt, erst, zweit, dritt, viert, fünft, sechst, siebt)

store_wellen <- list()
for(i in seq_along(list_wellen)) {
  store_wellen[[i]] <- table(list_wellen[[i]]$hotspot)
  
}

b <- as.data.frame(store_wellen)
view(b)

pool.4 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =viert, model = "pooling", index=c("district", "week"))
pool.5 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =fünft, model = "pooling", index=c("district", "week"))
pool.6 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =sechst, model = "pooling", index=c("district", "week"))
pool.7 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
              + A60.79.Anteil 
              + factor(week)
              , data =siebt, model = "pooling", index=c("district", "week"))


