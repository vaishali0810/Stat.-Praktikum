name <- c("sporadisch", "ersteWelle", "sommerplateau20", "zweiteWelle", "dritteWelle", "sommerplateau21", "vierteWelle", "fuenfteWelle")
sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)

##

#model.value=0.64026677
sqrt.inzidenz1<-c(sporadisch=1.7275431,
                  ersteWelle=0.63730532,
                  sommerplateau20=0.3267231,
                  zweiteWelle=0.710850,
                  
                  dritteWelle=0.7176103,
                  sommerplateau21=0.1752151,
                  vierteWelle=0.7799930,
                  fuenfteWelle=0.5799259)

st1 <- c(sporadisch=0.4917869,
          ersteWelle=0.04269120,
          sommerplateau20=0.0506541,
          zweiteWelle=0.023658,
          dritteWelle=0.02515458,
          sommerplateau21=0.0767015,
          vierteWelle=0.0183722,
         fuenfteWelle=0.0141360)

sqrt.inzidenz1<-data.frame(sqrt.inzidenz1, name)
colnames(sqrt.inzidenz1)<-c("a", "name")
sqrt.inzidenz1$name <- factor(sqrt.inzidenz1$name, levels = sqrt.inzidenz1$name)


ggplot(sqrt.inzidenz1, aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size=1) + ylab("lag(Inz,1)") + xlab("") +
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  geom_hline(yintercept=0.64026677, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=0.68, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st1, ymax=a+st1), width=0.25, color = "black", size = 1)


##
#model.value=0.23171750,
sqrt.wnbinzidenz1<-c(sporadisch=0.1453111,
                     ersteWelle=0.24599765,
                     sommerplateau20=0.1434222,
                     zweiteWelle=0.23873,
                     
                     dritteWelle=0.12683469,
                     sommerplateau21=0.2693283,
                     vierteWelle=0.1877512,
                     fuenfteWelle=0.2394195)

st2 <- c(sporadisch=0.0943975,
         ersteWelle=0.03904637,
         sommerplateau20=0.0353850,
         zweiteWelle=0.028114 ,
         
         dritteWelle=0.03044614,
         sommerplateau21=0.0658091,
         vierteWelle=0.0209524,
         fuenfteWelle=0.0194335)

sqrt.wnbinzidenz1<-data.frame(sqrt.wnbinzidenz1, name)
colnames(sqrt.wnbinzidenz1)<-c("a", "name")
sqrt.wnbinzidenz1$name <- factor(sqrt.wnbinzidenz1$name, levels = sqrt.wnbinzidenz1$name)


ggplot(sqrt.wnbinzidenz1, aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size = 1)+ ylab("sqrt(lag(NB.Inz,1))") + xlab("")+
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
geom_hline(yintercept=0.23171750, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sommerplateau20", y=0.237, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st2, ymax=a+st2), width=0.25, color = "black", size = 1)


##
#model.value=-0.00584863
sqrt.densityInzidenz<-c(sporadisch=-0.2705284,
                        ersteWelle=-0.00098574,
                        sommerplateau20=0.0365050,
                        zweiteWelle=-0.0009883, 
                        
                        dritteWelle=0.00557025,
                        sommerplateau21=0.04543828,
                        vierteWelle=-0.0052004,
                        fuenfteWelle=-0.0092886)

st3 <- c(sporadisch=0.0880139,
         ersteWelle=0.00665491,
         sommerplateau20=0.0077702,
         zweiteWelle=0.0028010,
         
         dritteWelle=0.00298210,
         sommerplateau21=0.0110276,
         vierteWelle=0.0022731,
         fuenfteWelle=0.0013343)

sqrt.densityInzidenz<-data.frame(sqrt.densityInzidenz, name)
colnames(sqrt.densityInzidenz)<-c("a", "name")
sqrt.densityInzidenz$name <- factor(sqrt.densityInzidenz$name, levels = sqrt.densityInzidenz$name)


ggplot(sqrt.densityInzidenz, aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size=1)+
  xlab("") + ylab("ln(Dichte)*sqrt(lag(Inz,1))") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") ) +
  geom_hline(yintercept=-0.00584863, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=0.004, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st3, ymax=a+st3), width=0.25, color = "black", size = 1)


##
# model.value=0.23092034,
sqrt.hotspotInzidenz<-c(sporadisch=3.4252897,
                        ersteWelle=0.34067481,
                        sommerplateau20=0.5744111,
                        zweiteWelle=0.22086662,
                        
                        dritteWelle= 0.55144379,
                        sommerplateau21=0.58125082,
                        vierteWelle=0.0843292,
                        fuenfteWelle= 0.2202815)

st4 <- c(sporadisch=0.2765762,
         ersteWelle=0.04540509,
         sommerplateau20=0.0766901,
         zweiteWelle=0.051372,
         
         dritteWelle=0.24589170,
         sommerplateau21=0.1909672,
         vierteWelle=0.0549345,
         fuenfteWelle=0.0407520)

sqrt.hotspotInzidenz<-data.frame(sqrt.hotspotInzidenz, name)
colnames(sqrt.hotspotInzidenz)<-c("a", "name")
sqrt.hotspotInzidenz$name <- factor(sqrt.hotspotInzidenz$name, levels = sqrt.hotspotInzidenz$name)


ggplot(sqrt.hotspotInzidenz, aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size = 1)+
  xlab("") + ylab("Hotspot*sqrt(lag(Inz,1))") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") ) +
  geom_hline(yintercept=0.23092034, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=0.32, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st4, ymax=a+st4), width=0.25, color = "black", size = 1)


##
#model.value=0.06928206
sqrt.hotspotnbWnbinzidenz<-c(sporadisch=0.6256243,
                             ersteWelle=0.13501991,
                             sommerplateau20=0.1689080,
                             zweiteWelle=0.0850171,
                             
                             dritteWelle=0.1263817,
                             sommerplateau21=0.18297279,
                             vierteWelle=0.1057920,
                             fuenfteWelle= 0.0569973)

st5 <- c(sporadisch=0.1993813,
         ersteWelle=0.05001543,
         sommerplateau20=0.0368755,
         zweiteWelle=0.039919,
         
         dritteWelle=0.12740579,
         sommerplateau21=0.0749478,
         vierteWelle=0.0295697,
         fuenfteWelle=0.0225224)

sqrt.hotspotnbWnbinzidenz<-data.frame(sqrt.hotspotnbWnbinzidenz, name)
colnames(sqrt.hotspotnbWnbinzidenz)<-c("a", "name")
sqrt.hotspotnbWnbinzidenz$name <- factor(sqrt.hotspotnbWnbinzidenz$name, levels = sqrt.hotspotnbWnbinzidenz$name)


ggplot(sqrt.hotspotnbWnbinzidenz,aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size = 1)+
  xlab("") + ylab("NB.Hotspot*sqrt(lag(NB.Inz,1))") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") ) +
  geom_hline(yintercept=0.069282064, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=0.09, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st5, ymax=a+st5), width=0.25, color = "black", size = 1)


##
#model.value=-0.03427165,
sqrt.zweitimpfHotspot<-c(sporadisch=NA,
                         ersteWelle=NA,
                         sommerplateau20=NA,
                         zweiteWelle=NA,
                         
                         dritteWelle=-2.60577324,
                         sommerplateau21= 0.0053421,
                         vierteWelle=0.0120367,
                         fuenfteWelle=-0.0447972)

st6 <- c(sporadisch=NA,
         ersteWelle=NA,
         sommerplateau20=NA,
         zweiteWelle=NA,
         
         dritteWelle=2.93776018,
         sommerplateau21=0.0277143,
         vierteWelle=0.0213790,
         fuenfteWelle=0.0211063)

sqrt.zweitimpfHotspot<-data.frame(sqrt.zweitimpfHotspot, name)
colnames(sqrt.zweitimpfHotspot)<-c("a","name")
sqrt.zweitimpfHotspot$name <- factor(sqrt.zweitimpfHotspot$name, levels = sqrt.zweitimpfHotspot$name)


ggplot(sqrt.zweitimpfHotspot,aes(x=name, y= a, group = 1)) + geom_point(size = 4)+ geom_line(size = 1)+
  xlab("") + ylab("Hotspot*Zweitimpfrate") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )  +
  geom_hline(yintercept=-0.03427165, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=0.09, label= "model value", color = "blue", size = 6)+
  geom_errorbar(aes(x=name, ymin=a-st6, ymax=a+st6), width=0.25, color = "black", size = 1)


###
#model.value=-0.00373027,
sqrt.A60.79<-c(sporadisch=-0.0163743,
               ersteWelle=0.00174279,
               sommerplateau20=-0.0023077,
               zweiteWelle=0.0000930,
               
               dritteWelle=-0.00056633,
               sommerplateau21=-0.0022412,
               vierteWelle=-0.0067067,
               fuenfteWelle=-0.0666460)

st7 <- c(sporadisch=0.0041254,
         ersteWelle=0.00307448,
         sommerplateau20=0.0013173,
         zweiteWelle=0.0057646,
         
         dritteWelle= 0.00555535,
         sommerplateau21=0.0029211,
         vierteWelle=0.0068866,
         fuenfteWelle=0.0147487)

sqrt.A60.79 <- data.frame(sqrt.A60.79, name)
colnames(sqrt.A60.79)<-c ("Anteil.A60.79", "name")
sqrt.A60.79$name <- factor(sqrt.A60.79$name, levels = sqrt.A60.79$name)

ggplot(sqrt.A60.79,aes(x=name, y=Anteil.A60.79, group = 1)) + geom_point(size = 4) + geom_line(size = 1)+
  xlab("") + ylab("Anteil.A60.79") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") ) +
  geom_hline(yintercept=-0.00373027, size = 1, linetype="dashed", color = "blue")+
  annotate("text", x="sporadisch", y=-0.002, label= "model value", color = "blue", size=6)+
  geom_errorbar(aes(x=name, ymin=Anteil.A60.79-st7, ymax=Anteil.A60.79+st7), width=0.25, color = "black", size = 1)



## adjusteed r squared
r2 <- c(sporadisch=0.4294543,ersteWelle=0.8235471,sommerplateau20=0.6332727,zweiteWelle=0.8206104,
dritteWelle=0.8787739,sommerplateau21=0.4086672,vierteWelle=0.9551718,fuenfteWelle=0.9485910)
r2 <- data.frame(r2, name)
colnames(r2)<-c ("a", "name")
r2$name <- factor(r2$name, levels = r2$name)

ggplot(r2,aes(x=name, y=a, group = 1)) + geom_point(size = 4) + geom_line(size = 1)+
  xlab("") + ylab("Adjusted R-Squared") + 
  theme(axis.text.x = element_text(size = 20, vjust = 1, hjust = 1, face = "bold", angle = 25)) +
  theme(axis.text.y = element_text(size = 20, face = "bold")) +
  theme(text = element_text(size = 22)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )

