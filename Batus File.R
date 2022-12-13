dbayern$bezirk<-as.factor(dbayern$bezirk)
vector2<-c(summary(dbayern$bezirk)[1:7])
vector2names<-names(vector2)
Stor<-list()
for(i in 1:length(vector2names)){
  Stor[[i]]<-dbayern[dbayern$bezirk==vector2names[i],]
}
# View(Storage)

## Nach Datum sortieren
library(dplyr)
Stor2<-Stor
for(i in 1:length(vector2names)){
  Stor2[[i]]<-Stor2[[i]]%>%arrange(date)
}
# View(Storage2)

## Nach Gender sortieren
Stor3<-Stor2
for(i in 1:length(vector2names)){
  Stor3[[i]]<-Stor3[[i]]%>%arrange(gender)
}
# View (Storage3)

## Nach age group sortieren
Stor4<-Stor3
for(i in 1:length(vector2names)){
  Stor4[[i]]<-Stor4[[i]]%>%arrange(age_group)
}
View(Stor4[[1]])

S41<-as.data.frame(Stor4[1])
S41%>%
  ggplot(aes(date, cases, colour = gender)) + 
  geom_line(stat = "identity")

ggplot(S41, aes(date,cases,color = gender,
                        linetype = gender)) +
  geom_line(stat="identity",size=0.1)+
  geom_smooth()

ggplot(S41, aes(date,cases,color = gender,
                linetype = gender)) +
  geom_point(size=0.1)+
  geom_smooth()

broom augument




a<-min(Storage9[[1]]$date)
b<-max(Storage9[[1]]$date)
c<-seq(as.Date(a), as.Date(b), "days")
c<-as.data.frame(c)
colnames(c)[1] <- "date"
y<-merge(Storage9[[1]],c, by="date",
         all.x=TRUE, all.y=TRUE)
v<-y$cases
index<-is.na(v)
v[index]<-0
y$cases<-v

v<-y$gender
index<-is.na(v)
v[index]<-"egal"
y$gender<-v


#####


ymal<-subset(y,gender=="M")
yfem<-subset(y,gender=="W")
yunk<-subset(y,gender=="unbekannt")

smal<-aggregate(x = ymal$cases,               
          by = list(ymal$date),              
          FUN = sum)
smal<-mutate(smal, gender ="M")

sfem<-aggregate(x = yfem$cases,               
                by = list(yfem$date),              
                FUN = sum)
sfem<-mutate(sfem, gender ="W")

sunk<-aggregate(x = yunk$cases,               
                by = list(yunk$date),              
                FUN = sum)
sunk<-mutate(sunk, gender ="U")

sall<-rbind(smal,sfem,sunk)

ggplot(sall, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)


y2 <- y2 %>% arrange(y2, district, date, age_group, cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         MA15cases=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+cases)/15)

df91<-as.data.frame(ymal, stringsAsFactors = FALSE)
ggplot(sall, aes(Group.1,x,color = gender)) +
  geom_line(size=0.1)



impfBayern <- dimpf[dimpf$bundesland == "Freistaat Bayern", ]
impfBayern <- impfBayern %>%
  mutate(kreis=recode(kreis, "München, Landeshauptstadt" = "SK München",
                      "Traunstein" = "LK Traunstein",
                      "München, Kreis" = "LK München",
                      "Augsburg, Stadt" = "SK Augsburg",
                      "Rosenheim, Kreis" = "LK Rosenheim",
                      "Augsburg, Kreis"= "LK Augsburg",
                      "Schwandorf" = "LK Schwandorf",
                      "Unterallgäu" = "LK Unterallgäu",
                      "Mühldorf a.Inn" = "LK Mühldorf a.Inn",
                      "Landshut, Kreis" = "LK Landshut",
                      "Freising" = "LK Freising",
                      "Ebersberg" = "LK Ebersberg",
                      "Miltenberg" = "LK Miltenberg",
                      "Aschaffenburg, Kreis" = "LK Aschaffenburg",
                      "Rottal-Inn" = "LK Rottal-Inn",
                      "Dachau" = "LK Dachau",
                      "Pfaffenhofen a.d.Ilm" = "LK Pfaffenhofen a.d.Ilm",
                      "Ingolstadt" = "SK Ingolstadt",
                      "Roth" = "LK Roth",
                      "Günzburg" = "LK Günzburg",
                      "Nürnberger Land" = "LK Nürnberger Land",
                      "Fürstenfeldbruck" = "LK Fürstenfeldbruck",
                      "Dillingen a.d.Donau" = "LK Dillingen a.d.Donau",
                      "Donau-Ries" = "LK Donau-Ries",
                      "Altötting" = "LK Altötting",
                      "Dingolfing-Landau" = "LK Dingolfing-Landau",
                      "Kelheim" = "LK Kelheim",
                      "Bamberg, Kreis" = "LK Bamberg",
                      "Neustadt a.d.Aisch-Bad Windsheim" = "LK Neustadt a.d.Aisch-Bad Windsheim",
                      "Regensburg, Kreis" = "LK Regensburg",
                      "Freyung-Grafenau" = "LK Freyung-Grafenau",
                      "Amberg-Sulzbach" = "LK Amberg-Sulzbach",
                      "Neu-Ulm" = "LK Neu-Ulm",
                      "Rhön-Grabfeld" = "LK Rhön-Grabfeld",
                      "Neumarkt i.d.OPf." = "LK Neumarkt i.d.OPf.",
                      "Berchtesgadener Land" = "LK Berchtesgadener Land",
                      "Passau, Kreis" = "LK Passau",
                      "Bayreuth, Kreis" = "LK Bayreuth",
                      "Regen" = "LK Regen",
                      "Bad Tölz-Wolfratshausen" = "LK Bad Tölz-Wolfratshausen",
                      "Aichach-Friedberg" = "LK Aichach-Friedberg",
                      "Schweinfurt, Kreis" = "LK Schweinfurt",
                      "Forchheim" = "LK Forchheim",
                      "Miesbach" = "LK Miesbach",
                      "Regensburg, Stadt" = "SK Regensburg",
                      "Main-Spessart" = "LK Main-Spessart",
                      "Ansbach, Kreis" = "LK Ansbach",
                      "Bayreuth, Stadt" = "SK Bayreuth",
                      "Cham" = "LK Cham",
                      "Kitzingen" = "LK Kitzingen",
                      "Tirschenreuth" = "LK Tirschenreuth",
                      "Eichstätt" = "LK Eichstätt",
                      "Landshut, Stadt" = "SK Landshut",
                      "Rosenheim, Stadt" = "SK Rosenheim",
                      "Oberallgäu" = "LK Oberallgäu",
                      "Fürth, Stadt" = "SK Fürth",
                      "Aschaffenburg, Stadt" = "SK Aschaffenburg",
                      "Coburg, Kreis" = "LK Coburg",
                      "Ostallgäu" = "LK Ostallgäu",
                      "Neustadt a.d.Waldnaab" = "LK Neustadt a.d.Waldnaab",
                      "Wunsiedel i.Fichtelgebirge" = "LK Wunsiedel i.Fichtelgebirge",
                      "Deggendorf" = "LK Deggendorf",
                      "Lichtenfels" = "LK Lichtenfels",
                      "Nürnberg" = "SK Nürnberg",
                      "Weißenburg-Gunzenhausen" = "LK Weißenburg-Gunzenhausen",
                      "Erlangen-Höchstadt" = "LK Erlangen-Höchstadt",
                      "Schweinfurt, Stadt" = "SK Schweinfurt",
                      "Kulmbach" = "LK Kulmbach",
                      "Würzburg, Kreis" = "LK Würzburg",
                      "Würzburg, Stadt" = "SK Würzburg",
                      "Fürth, Kreis" = "LK Fürth",
                      "Schwabach" = "SK Schwabach",
                      "Memmingen" = "SK Memmingen",
                      "Weilheim-Schongau" = "LK Weilheim-Schongau",
                      "Bad Kissingen" = "LK Bad Kissingen",
                      "Bamberg, Stadt" = "SK Bamberg",
                      "Straubing-Bogen" = "LK Straubing-Bogen",
                      "Hof, Kreis" = "LK Hof",
                      "Erding" = "LK Erding",
                      "Erlangen" = "SK Erlangen",
                      "Lindau (Bodensee)" = "LK Lindau",
                      "Amberg" = "SK Amberg",
                      "Starnberg"= "LK Starnberg",
                      "Neuburg-Schrobenhausen" = "LK Neuburg-Schrobenhausen",
                      "Landsberg am Lech" = "LK Landsberg a.Lech",
                      "Haßberge" = "LK Haßberge",
                      "Kempten (Allgäu)" = "SK Kempten",
                      "Coburg, Stadt" = "SK Coburg",
                      "Kronach" = "LK Kronach",
                      "Weiden i.d.OPf." = "SK Weiden i.d.OPf.",
                      "Garmisch-Partenkirchen" = "LK Garmisch-Partenkirchen",
                      "Passau, Stadt" = "SK Passau",
                      "Hof, Stadt" = "SK Hof",
                      "Straubing" = "SK Straubing",
                      "Kaufbeuren" = "SK Kaufbeuren",
                      "Ansbach, Stadt" = "SK Ansbach")) %>%
  as.data.frame()
colnames(impfBayern)[5] <- "district"
colnames(impfBayern)[6] <- "date"
impfBayern[,6] <- as.Date(impfBayern[,6])

#impfBayern$erstimpf <-cumsum(impfBayern$kr_erstimpf)
#impfBayern$zweitimpf <-cumsum(impfBayern$kr_zweitimpf)
#impfBayern$drittimpf <-cumsum(impfBayern$kr_drittimpf)
#impfBayern$viertimpf <-cumsum(impfBayern$kr_viertimpf)

# impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)
# dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"))

impfBayern$erstimpf_sum<-NA
impfBayern$zweitimpf_sum<-NA
impfBayern$drittimpf_sum<-NA
impfBayern$viertimpf_sum<-NA

impfbayern2<-impfBayern%>%group_by(district)%>%dplyr::mutate(erstimpf_sum=cumsum(kr_erstimpf),zweitimpf_sum=cumsum(kr_zweitimpf),drittimpf_sum=cumsum(kr_drittimpf),viertimpf_sum=cumsum(kr_viertimpf))

#View(impfbayern2)

#impfungentake <- impfbayern2 %>% select(district, date, erstimpf_sum, zweitimpf_sum, drittimpf_sum, viertimpf_sum)
impfungentake <- impfBayern %>% select(district, date, kr_erstimpf, kr_zweitimpf, kr_drittimpf, kr_viertimpf)
#View(impfungentake)
dbayern2 <- merge(dbayern, impfungentake, by = c("district", "date"), all.x = TRUE, all.y = TRUE)
#View(dbayern2)


impf<- read.csv("impfdaten_regional.csv", head=TRUE, sep=",")

dfcombined<-test100000

dfcombined$date<-as.Date(dfcombined$date)
impfungentake$date<-as.Date(impfungentake$date)

dfcomb_impf <- merge(dfcombined, impfungentake, by = c("district", "date"), 
           all.x = TRUE, all.y = TRUE)

a <- dfcomb_impf$kr_erstimpf 
index <- is.na(a) 
dfcomb_impf[index, 26] <- 0
a <- dfcomb_impf$kr_zweitimpf 
index <- is.na(a) 
dfcomb_impf[index, 27] <- 0
a <- dfcomb_impf$kr_drittimpf
index <- is.na(a) 
dfcomb_impf[index, 28] <- 0
a <- dfcomb_impf$kr_viertimpf 
index <- is.na(a) 
dfcomb_impf[index, 29] <- 0

dfcomb_impf_pop2<-merge(dfcomb_impf, popbay2, by=c("district","bezirk"))

colnames(dfcomb_impf)
colnames(popbay2)



dfcomb_impf_pop2 <- dfcomb_impf_pop2 %>% 
  arrange(date) %>%
  group_by(district) %>%
  mutate(inzidenz = ((lag(total_cases,6) + lag(total_cases,5) + lag(total_cases,4)+ 
                        lag(total_cases,3) +lag(total_cases,2) + lag(total_cases,1) + total_cases)
                     /population) * 100000)
dfcomb_impf_pop2<-dfcomb_impf_pop2%>%arrange(district)

dfcomb_impf_pop2$male_anteil<-dfcomb_impf_pop2$male/dfcomb_impf_pop2$population
dfcomb_impf_pop2$female_anteil<-dfcomb_impf_pop2$female/dfcomb_impf_pop2$population
View(dfcomb_impf_pop2)

model_boosting_default <- glmboost(medv ~ ., data = data_train,
                                   control = boost_control(mstop = 100,
                                                           nu = 0.1))

data<-dfultimate
testpdf <- pdata.frame(dfultimate, index=c("district", "week"))
re1 <- plm(inzidenz~lag(inzidenz,7), data= testpdf, model = "random")

form_lasso <- nm ~ p(wfl, pen = "lasso") + p(rooms, pen = "lasso") +
  p(bj, pen = "lasso") + p(lage, pen = "lasso")

model_lasso_cv <- glmsmurf(formula = form_lasso, family = gaussian(),
                           data = miete, lambda = "cv.mse")


dfultimate_pan <- pdata.frame(dfultimate, index=c("district", "week"))

form_lasso <- inzidenz ~ p(lag(inzidenz, 1), pen = "lasso") + p(density,pen = "lasso")
+ p(rate_zweitimpf, pen = "lasso")+ p(m_anteil, pen = "lasso")

model_lasso_cv <- glmsmurf(formula = inzidenz ~ p(lag(inzidenz, 1), pen = "lasso") + p(density,pen = "lasso")
                           + p(rate_zweitimpf, pen = "lasso")+ p(m_anteil, pen = "lasso"), family = gaussian(),
                           data = dfultimate_pan, lambda = "cv.mse")
plot_lambda(model_lasso_cv)

lambda<-4.5



###Variablenselektion
library(reshape)

dfultimate=na.omit(dfultimate)
x=model.matrix(inzidenz ~ bezirk + density + m_anteil + rate_zweitimpf, dfultimate)[,-1]
y=as.matrix(dfultimate$inzidenz)
lasso.mod =glmnet(x,y, alpha =1)
beta=coef(lasso.mod)

tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso.mod$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm

##plot
library(ggplot2)

ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(3,"lines"))