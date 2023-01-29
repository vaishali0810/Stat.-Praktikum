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

o<-do.call(cbind,store_wellen)
p<-as.matrix(o)
p<-t(p)
rownames(p)<-c("nullt", "erst", "zweit", "dritt", "viert", "fünft", "sechst", "siebt")

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

## Wellen plot
df_wellen_plot <- aggregate(df4$inzidenz,
                            by = list(df4$Kalendarwoche),
                            FUN = sum)
colnames(df_wellen_plot) <- c("KW", "Inz")
as.data.frame(df_wellen_plot)
my_labels <- as.character(c(4:53, 1:52, 1:46))
df_wellen_plot

ggplot(df_wellen_plot, aes(x = KW, y = Inz/96)) + geom_bar(stat='identity') +
  xlab("Kalendarwoche") + ylab("Inzidenz") + ggtitle("Inzidenzen in Bayern") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") ) +
  theme(legend.text=element_text(size=15), legend.title=element_blank()) 

