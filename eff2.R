######### 1000
summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*1000)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz = 1000")

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*100)}
plot(a(h), xlab = "Density", ylab = "Effekte") + title("Effekt bei Inzidenz = 100")

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*10)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz = 10")

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-0.0284347)*sqrt(log(x)*1000)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz=1000 sqrt-Modell")

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-0.0284347)*sqrt(log(x)*100)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz = 100 sqrt-Modell")

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-0.0284347)*sqrt(log(x)*10)}
p6<-plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz = 10 sqrt-Modell")



summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*1000)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*1000)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 1000")

########## 100


summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*100)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 100")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*100)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 100")

########## 10


summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*10)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 10")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*10)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 10")



#Coefficients:
# Estimate
#(Intercept)                                -0.0089898
#sqrt(lag(inzidenz, 1))                      0.6744596
#sqrt(lag(weightednbinz, 1))                 0.2316856
#sqrt(I(log(density) * lag(inzidenz, 1)))   -0.0284347
#sqrt(I(hotspot * lag(inzidenz, 1)))         0.2275481
#sqrt(I(hotspotnb * lag(weightednbinz, 1)))  0.0682300
#sqrt(I(rate_zweitimpf * hotspot))          -1.8974017
#A60.79.Anteil                              -0.3125136


######### 1000 mit sqrt

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(0.2316856)*sqrt(log(x)*1000)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000 sqrt-Modell")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(0.2275481)*sqrt(log(x)*1000)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 1000 sqrt-Modell")

########## 100

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(0.2316856)*sqrt(log(x)*100)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 100 sqrt-Modell")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(0.2275481)*sqrt(log(x)*100)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 100 sqrt-Modell")

########## 10

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(0.2316856)*sqrt(log(x)*10)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 10 sqrt-Modell")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(0.2275481)*sqrt(log(x)*10)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 10 sqrt-Modell")

#