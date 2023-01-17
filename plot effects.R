
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
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekten Plot für Dichte mit Inzidenz = 1000")

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*1000)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekten Plot für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*1000)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekten Plot für Hotspot mit Inzidenz = 1000")

########## 100

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*100)}
plot(a(h), xlab = "Density", ylab = "Effekte") + title("Effekten Plot für Dichte mit Inzidenz = 100")

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*100)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekten Plot für gewichtete Nachbar-Inzidenz mit Inzidenz = 100")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*100)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekten Plot für Hotspot mit Inzidenz = 100")

########## 10

summary(df4_pan$density)
h<-c(66.0:4788.0)
a<-function(x){(-2.0009e-02)*(log(x)*10)}
plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekten Plot für Dichte mit Inzidenz = 10")

summary(df4_pan$weightednbinz)
h<-c(0.0:3302.4)
a<-function(x){(2.7351e-01)*(log(x)*10)}
plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekten Plot für gewichtete Nachbar-Inzidenz mit Inzidenz = 10")

summary(df4_pan$hotspot)
h<-c(0.0:3302.4)
a<-function(x){(5.7362e-01)*(log(x)*10)}
plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekten Plot für Hotspot mit Inzidenz = 10")






