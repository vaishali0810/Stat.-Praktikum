pool <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
            + A60.79.Anteil 
            + factor(week)
            , data =df4_pan, model = "pooling")
pool.sqrt.actual <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                        + A60.79.Anteil
                        + factor(week)
                        , data =df4_pan, model = "pooling")
pool.weighted <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data =df4_pan,weights = 1/(sqrt(inzidenz + 1)), model = "pooling")
library(stargazer)

pool.sqrt.actual.lag <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                            + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                            + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                            + lag(A60.79.Anteil, 1)
                            + factor(week)
                            , data =df4_pan, model = "pooling")

stargazer(pool.sqrt.actual.lag)

a<-coeftest(pool, vcovHC(pool, type = "HC0"))
b<-coeftest(pool.sqrt.actual, vcovHC(pool.sqrt.actual, type = "HC0"))
c<-summary(pool.weighted)
stargazer(b)

pool.sqrt.actual.lag <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                            + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                            + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                            + lag(A60.79.Anteil, 1)
                            + factor(week)
                            , data =df4_pan, model = "pooling")



pool.sqrt.nullt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                       + lag(A60.79.Anteil, 1)
                       + factor(week)
                       , data =nullt, model = "pooling", index = c("district", "week"))

pool.sqrt.erst <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                      + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                      + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                      + lag(A60.79.Anteil, 1)
                      + factor(week)
                      , data =erst, model = "pooling", index = c("district", "week"))
pool.sqrt.zweit <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                       + lag(A60.79.Anteil, 1)
                       + factor(week)
                       , data =zweit, model = "pooling", index = c("district", "week"))
pool.sqrt.dritt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                       + lag(A60.79.Anteil, 1)
                       + factor(week)
                       , data =dritt, model = "pooling", index = c("district", "week"))
pool.sqrt.viert <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                       + lag(A60.79.Anteil, 1)
                       + factor(week)
                       , data =viert, model = "pooling", index = c("district", "week"))
pool.sqrt.fuenft <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                        + lag(A60.79.Anteil, 1)
                        + factor(week)
                        , data =fünft, model = "pooling", index = c("district", "week"))
pool.sqrt.sechst <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                        + lag(A60.79.Anteil, 1)
                        + factor(week)
                        , data =sechst, model = "pooling", index = c("district", "week"))
pool.sqrt.siebt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(lag(rate_zweitimpf,1) * hotspot)
                       + lag(A60.79.Anteil, 1)
                       + factor(week)
                       , data =siebt, model = "pooling", index = c("district", "week"))

sum.sqrt.actual.lag<-summary(pool.sqrt.actual.lag)
sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)

a<-list(sum.sqrt.actual.lag,sum.sqrt.nullt, 
sum.sqrt.erst,
sum.sqrt.zweit, 
sum.sqrt.dritt,
sum.sqrt.viert,
sum.sqrt.fuenft,
sum.sqrt.sechst,
sum.sqrt.siebt)

b<-list()
for (i in seq_along(a)){
  b[i]<-a[[i]]
}
c<-list()
for (i in seq_along(b)){
  c[i]<-as.data.frame(b[[i]])
}

hotspot_inzidenz_coeff<-list()
for (i in seq_along(c)){
  hotspot_inzidenz_coeff[i]<-c[[i]][5]
}
hotspot_inzidenz_coeff<-as.data.frame(unlist(hotspot_inzidenz_coeff))

Wellen<-as.data.frame(c("model.value",
"sporadisch",
"ersteWelle",
"sommerplateau20",
"zweiteWelle",
"dritteWelle",
"sommerplateau21",
"vierteWelle",
"fuenfteWelle"))

hotspot_inzidenz_t<-cbind(Wellen, hotspot_inzidenz_coeff)
library(kableExtra)
hotspot_inzidenz_t %>%kable(format = 'latex', booktabs = TRUE)

#########

sum.sqrt.actual.lag<-summary(pool.sqrt.actual.lag)
sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)

a<-list(sum.sqrt.actual.lag,
        sum.sqrt.nullt, 
        sum.sqrt.erst,
        sum.sqrt.zweit, 
        sum.sqrt.dritt,
        sum.sqrt.viert,
        sum.sqrt.fuenft,
        sum.sqrt.sechst,
        sum.sqrt.siebt)

b<-list()
for (i in seq_along(a)){
  b[i]<-a[[i]]
}
c<-list()
for (i in seq_along(b)){
  c[i]<-as.data.frame(b[[i]])
}

den_inzidenz_coeff<-list()
for (i in seq_along(c)){
  den_inzidenz_coeff[i]<-c[[i]][4]
}
den_inzidenz_coeff<-as.data.frame(unlist(den_inzidenz_coeff))

Wellen<-as.data.frame(c("model.value",
                        "sporadisch",
                        "ersteWelle",
                        "sommerplateau20",
                        "zweiteWelle",
                        "dritteWelle",
                        "sommerplateau21",
                        "vierteWelle",
                        "fuenfteWelle"))

den_inzidenz_t<-cbind(Wellen, den_inzidenz_coeff)
library(kableExtra)
den_inzidenz_t %>%kable(format = 'latex', booktabs = TRUE)

##########################

sum.sqrt.actual.lag<-summary(pool.sqrt.actual.lag)
sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)

a<-list(sum.sqrt.actual.lag,
        sum.sqrt.nullt, 
        sum.sqrt.erst,
        sum.sqrt.zweit, 
        sum.sqrt.dritt,
        sum.sqrt.viert,
        sum.sqrt.fuenft,
        sum.sqrt.sechst,
        sum.sqrt.siebt)

b<-list()
for (i in seq_along(a)){
  b[i]<-a[[i]]
}
c<-list()
for (i in seq_along(b)){
  c[i]<-as.data.frame(b[[i]])
}

den_inzidenz_coeff<-list()
for (i in seq_along(c)){
  den_inzidenz_coeff[i]<-c[[i]][6]
}
den_inzidenz_coeff<-as.data.frame(unlist(den_inzidenz_coeff))

Wellen<-as.data.frame(c("model.value",
                        "sporadisch",
                        "ersteWelle",
                        "sommerplateau20",
                        "zweiteWelle",
                        "dritteWelle",
                        "sommerplateau21",
                        "vierteWelle",
                        "fuenfteWelle"))

Nb_hot_NB_inzidenz_t<-cbind(Wellen, den_inzidenz_coeff)
library(kableExtra)
Nb_hot_NB_inzidenz_t %>%kable(format = 'latex', booktabs = TRUE)