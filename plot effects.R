
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
h1<-c(66.0:4788.0)
a1<-function(x){(-2.0009e-02)*(log(x)*1000)}
# plot(a1(h1), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 1000") +

summary(df4_pan$weightednbinz)
h2<-c(0.0:3302.4)
a2<-function(x){(2.7351e-01)*(log(x)*1000)}
# plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 1000")

summary(df4_pan$hotspot)
h3<-c(0.0:3302.4)
a3<-function(x){(5.7362e-01)*(log(x)*1000)}
# plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 1000")

########## 100

summary(df4_pan$density)
h4<-c(66.0:4788.0)
a4<-function(x){(-2.0009e-02)*(log(x)*100)}
# plot(a(h), xlab = "Density", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 100")

summary(df4_pan$weightednbinz)
h5<-c(0.0:3302.4)
a5<-function(x){(2.7351e-01)*(log(x)*100)}
# plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 100")

summary(df4_pan$hotspot)
h6<-c(0.0:3302.4)
a6<-function(x){(5.7362e-01)*(log(x)*100)}
# plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 100")

########## 10

summary(df4_pan$density)
h7<-c(66.0:4788.0)
a7<-function(x){(-2.0009e-02)*(log(x)*10)}
# plot(a(h), xlab = "Dichte", ylab = "Effekte") + title("Effekt für Dichte mit Inzidenz = 10")

summary(df4_pan$weightednbinz)
h8<-c(0.0:3302.4)
a8<-function(x){(2.7351e-01)*(log(x)*10)}
# plot(a(h), xlab = "gewichtete Nachbar-Inzidenz", ylab = "Effekte") + title("Effekt für gewichtete Nachbar-Inzidenz mit Inzidenz = 10")

summary(df4_pan$hotspot)
h9<-c(0.0:3302.4)
a9<-function(x){(5.7362e-01)*(log(x)*10)}
# plot(a(h), xlab = "Hotspot", ylab = "Effekte") + title("Effekt für Hotspot mit Inzidenz = 10")

## Plots

# densityPlots
d_density <- data.frame(h1, a1(h1), h4, a4(h4), h7, a7(h7))
ggplot(d_density) + geom_point(aes(x = h1, y = a1(h1)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h4, y = a4(h4)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h7, y = a7(h7)), colour = "blue", show.legend = T) + 
  xlab("Dichte") + ylab("Effekte") +
  ggtitle("Effekt Plot für Dichte") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))


# weightednbinz
d_weighted <- data.frame(h2, a2(h2), h5, a5(h5), h8, a8(h8))
ggplot(d_weighted) + geom_point(aes(x = h2, y = a2(h2)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h5, y = a5(h5)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h8, y = a8(h8)), colour = "blue", show.legend = T) + 
  xlab(" gewichtete Nachbar-Inzidenz") + ylab("Effekte") +
  ggtitle("Effekt Plot für gewichtete Nachbar-Inzidenz") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))

# hotspot

d_hotspot <- data.frame(h3, a3(h3), h6, a6(h6), h9, a9(h9))
ggplot(d_hotspot) + geom_point(aes(x = h3, y = a3(h3)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h6, y = a6(h6)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h9, y = a9(h9)), colour = "blue", show.legend = T) + 
  xlab("Hotspot") + ylab("Effekte") +
  ggtitle("Effekt Plot für Hotspot") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))


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


## Sqrt Plots

# density_sqrt Plots
d_density_sqrt <- data.frame(h1_sqrt, a1_sqrt(h1_sqrt), h4_sqrt, a4_sqrt(h4_sqrt), h7_sqrt, a7_sqrt(h7_sqrt))
ggplot(d_density_sqrt) + geom_point(aes(x = h1_sqrt, y = a1_sqrt(h1_sqrt)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h4_sqrt, y = a4_sqrt(h4_sqrt)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h7_sqrt, y = a7_sqrt(h7_sqrt)), colour = "blue", show.legend = T) + 
  xlab("Dichte") + ylab("Effekte") +
  ggtitle("Effekt Plot für Dichte von sqrt-Modell") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))
  

# weightednbinz_sqrt
d_weighted_sqrt <- data.frame(h2_sqrt, a2_sqrt(h2_sqrt), h5_sqrt, a5_sqrt(h5_sqrt), h8_sqrt, a8_sqrt(h8_sqrt))
ggplot(d_weighted_sqrt) + geom_point(aes(x = h2_sqrt, y = a2_sqrt(h2_sqrt)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h5_sqrt, y = a5_sqrt(h5_sqrt)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h8_sqrt, y = a8_sqrt(h8_sqrt)), colour = "blue", show.legend = T) + 
  xlab(" gewichtete Nachbar-Inzidenz") + ylab("Effekte") +
  ggtitle("Effekt Plot für gewichtete Nachbar-Inzidenz von sqrt-Modell") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))

# hotspot_sqrt

d_hotspot_sqrt <- data.frame(h3_sqrt, a3_sqrt(h3_sqrt), h6_sqrt, a6_sqrt(h6_sqrt), h9_sqrt, a9_sqrt(h9_sqrt))
ggplot(d_hotspot_sqrt) + geom_point(aes(x = h3_sqrt, y = a3_sqrt(h3_sqrt)), colour = "black", show.legend = T) + 
  geom_point(aes(x = h6_sqrt, y = a6_sqrt(h6_sqrt)), colour = "darkgrey", show.legend = T) +
  geom_point(aes(x = h9_sqrt, y = a9_sqrt(h9_sqrt)), colour = "blue", show.legend = T) + 
  xlab("Hotspot") + ylab("Effekte") +
  ggtitle("Effekt Plot für Hotspot von sqrt-Modell") + 
  theme(axis.text.x = element_text(size = 16, vjust = 1, hjust = 1, face = "bold")) +
  theme(axis.text.y = element_text(size = 16, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white") )+
  scale_colour_manual(name = '', 
                      guide = 'legend',
                      values = c('black' = 'black',
                                 'darkgrey' = 'grey',
                                 'blue' = 'blue'), 
                      labels = c('Inzidenz = 1000',
                                 'Inzidenz = 100',
                                 'Inzidenz = 10'))
