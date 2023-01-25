
fe.step13 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + factor(week)
                 , data =df4_pan, model = "within")
library(mgcv)
fe.step14 <- lm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + factor(week)
                 , data =df4_pan)#, model = "within")


plot(fe.step14, shade = TRUE, select = 4, main = "Glatter Effekt",
     xlab = "dens_inz", cex.axis = 0.8)
plot(fe.step14)

library(sjPlot)
plot_model(fe.step14)


plot(df4_paninzidenzdf4_pan$density)

plot1 <- plot(effect(term = "I(log(density)*lag(inzidenz, 1))", mod = fe.step14 ),
              main = "Linearer Effekt", xlab = "density",
              ylab = "inzidenz", ylim = c(4.5, 7.5))

term.plot(object = fe.step13, what = "mu", rug = TRUE,
          main = "Glatter Effekt bezüglich des Erwartungswertes",
          xlab = "Wohnfläche (in qm)", cex.main = 1)





library(plm)
library(fixest)
library(marginaleffects)
library(modelsummary)
data("EmplUK")

mod1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                 + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                 + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                 + factor(week), effect = "individual"
                 , data =df4_pan, model = "within")

mod2 <- feols(
  inzidenz ~ lag(inzidenz, 1) + I(log(density)*lag(inzidenz, 1))|(lag(weightednbinz, 1) 
  + I(hotspot * lag(inzidenz, 1)) 
  + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
  + factor(week)),
  se = "standard",
  data = df4_pan)

mod3 <- feols(
  inzidenz ~ 100 + I(log(density)*100)|(lag(weightednbinz, 1) + I(hotspot * 100) 
                                                                  + I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot)
                                                                  + factor(week)),
  se = "standard",
  data = df4_pan)





models <- list("PLM" = mod1, "FIXEST" = mod2)
modelsummary(models)



plot_cap(mod2, condition = "I(log(density)*lag(inzidenz, 1))")

plot_cme(mod2, effect = "I(log(density)*lag(inzidenz, 1))", condition = "week")

library(plm)
library(fixest)
library(marginaleffects)
library(modelsummary)

comparisons(mod1) %>% tidy()


#x<-c(10:200)

#fe.actual$coefficients*100

#a<-as.vector(df4_pan$density)
#b<-as.vector(df4_pan$inzidenz)
#plot(*b)

#Coefficients:
#  Estimate
#(Intercept)                          -9.0117e-04
#lag(inzidenz, 1)                      5.9661e-01
#lag(weightednbinz, 1)                 2.7351e-01
#I(log(density) * lag(inzidenz, 1))   -2.0009e-02
#I(hotspot * lag(inzidenz, 1))         5.6233e-01
#I(hotspotnb * lag(weightednbinz, 1))  1.3104e-01
#I(rate_zweitimpf * hotspot)          -1.6095e+02
#A60.79.Anteil                        -2.2597e+01

######### 1000
summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*1000)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 1000") +
  lines(x2, col = "red" ,type = 'b')

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*1000)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*1000)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 1000")

########## 100

summary(df4_pan$density)
h2<-c(66.0:4788.0)
a2<-function(x){(-2.0009e-02)*(log(x)*100)}
x2 <- a2(h2)
plot(a(h), xlab = "Density", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 100")

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*100)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 100")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*100)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 100")

########## 10

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*10)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 10")

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
summary(df4_pan$density)
h1_sqrt<-c(66.0:4788.0)
a1_sqrt<-function(x){(-0.0284347)*sqrt(log(x)*1000)}
# plot(a1_sqrt(h1_sqrt), xlab = "Dichte", ylab = "Effekte") + title("Effekt bei Inzidenz=1000 sqrt-Modell", pch = 1)

summary(df4_pan$weightednbinz)
h2_sqrt<-c(0.0:3302.4)
a2_sqrt<-function(x){(0.2316856)*sqrt(log(x)*1000)}
# plot(a2_sqrt(h2_sqrt), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000 sqrt-Modell")

summary(df4_pan$hotspot)
h3_sqrt<-c(0.0:3302.4)
a3_sqrt<-function(x){(0.2275481)*sqrt(log(x)*1000)}
# plot(a3_sqrt(h3_sqrt), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 1000 sqrt-Modell")

########## 100
summary(df4_pan$density)
h4_sqrt<-c(66.0:4788.0)
a4_sqrt<-function(x){(-0.0284347)*sqrt(log(x)*100)}
# plot(a4_sqrt(h4_sqrt), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 100 sqrt-Modell")

summary(df4_pan$weightednbinz)
h5_sqrt<-c(0.0:3302.4)
a5_sqrt<-function(x){(0.2316856)*sqrt(log(x)*100)}
# plot(a5_sqrt(h5_sqrt), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 100 sqrt-Modell")

summary(df4_pan$hotspot)
h6_sqrt<-c(0.0:3302.4)
a6_sqrt<-function(x){(0.2275481)*sqrt(log(x)*100)}
# plot(a6_sqrt(h6_sqrt), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 100 sqrt-Modell")

########## 10
summary(df4_pan$density)
h7_sqrt<-c(66.0:4788.0)
a7_sqrt<-function(x){(-0.0284347)*sqrt(log(x)*10)}
# plot(a7_sqrt(h7_sqrt), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 10 sqrt-Modell")

summary(df4_pan$weightednbinz)
h8_sqrt<-c(0.0:3302.4)
a8_sqrt<-function(x){(0.2316856)*sqrt(log(x)*10)}
# plot(a8_sqrt(h8_sqrt), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 10 sqrt-Modell")

summary(df4_pan$hotspot)
h9_sqrt<-c(0.0:3302.4)
a9_sqrt<-function(x){(0.2275481)*sqrt(log(x)*10)}
# plot(a9_sqrt(h9_sqrt), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 10 sqrt-Modell")

# Density SQRT Plots
d_density <- data.frame(h1_sqrt, a1_sqrt(h1_sqrt), h4_sqrt, a4_sqrt(h4_sqrt), h7_sqrt, h7_sqrt(h7_sqrt))
ggplot(d) + geom_point(aes(x = h1_sqrt, y = a1_sqrt(h1_sqrt)), size = 0.5) + geom_point(aes(x = h4_sqrt, y = a4_sqrt(h4_sqrt)), col = "darkgrey") +
  geom_point(aes(x = h7_sqrt, y = a7_sqrt(h7_sqrt)), col = "blue")
