df4 <- df4 %>% 
  mutate(Kalendarwoche = df4$week+3)

# df4$rate_zweitimpf<-df4$rate_zweitimpf*100
# df4$rate_drittimpf<-df4$rate_drittimpf*100
# df4$rate_viertimpf<-df4$rate_viertimpf*100
# df4$rate_erstimpf<-df4$rate_erstimpf*100
# max(df4$rate_zweitimpf)
# 
# df4$A00.04.Anteil<-df4$A00.04.Anteil*100
# df4$A05.14.Anteil<-df4$A05.14.Anteil*100
# df4$A15.34.Anteil<-df4$A15.34.Anteil*100
# df4$A35.59.Anteil<-df4$A35.59.Anteil*100
# df4$A60.79.Anteil<-df4$A60.79.Anteil*100
# df4$A80.Anteil<-df4$A80.Anteil*100

summary(df4$A80.Anteil)


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


df4_pan <- pdata.frame(df4, index = c("district", "week"))

### residual plots fill with alpha = 0.03


nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool.nullt<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) 
                + A60.79.Anteil 
                + factor(week)
                , data =nullt_pan, model = "pooling")



pool.nullt.adj<-plm(inzidenz ~ lag(inzidenz, 1)
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1))
                    + A60.79.Anteil + A15.34.Anteil
                    + factor(week)
                    , data =nullt_pan, model = "pooling")
# summary(pool.nullt.adj)
# coeftest(pool.nullt.adj, vcovHC(pool.nullt.adj, type = "HC0"))


nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool.nullt<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) 
                + A60.79.Anteil 
                + factor(week)
                , data =nullt_pan, model = "pooling")

summary(pool.nullt)


erst_pan<-pdata.frame(erst,index=c("district","week"))

pool.erst<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
               + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
               +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
               + A60.79.Anteil 
               + factor(week)
               , data =erst_pan, model = "pooling")




zweit_pan<-pdata.frame(zweit,index=c("district","week"))

pool.zweit<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                + A60.79.Anteil 
                + factor(week)
                , data =zweit_pan, model = "pooling")


dritt_pan <-pdata.frame(dritt,index=c("district","week"))

pool.dritt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A60.79.Anteil 
                  + factor(week)
                  , data =dritt_pan, model = "pooling")

pool.viert <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A60.79.Anteil 
                  + factor(week)
                  , data =viert, model = "pooling", index=c("district", "week"))

pool.fuenft <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                   + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                   +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                   + A60.79.Anteil 
                   + factor(week)
                   , data =fünft, model = "pooling", index=c("district", "week"))

pool.sechst <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                   + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                   +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                   + A60.79.Anteil 
                   + factor(week)
                   , data =sechst, model = "pooling", index=c("district", "week"))

pool.siebt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                  + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                  +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                  + A60.79.Anteil 
                  + factor(week)
                  , data =siebt, model = "pooling", index=c("district", "week"))

pool.zweit.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                    +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                    + A60.79.Anteil 
                    + factor(week)
                    , data = zweit_a, model = "pooling", index = c("district", "week"))


pool.zweit.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                    + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                    +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                    + A60.79.Anteil 
                    + factor(week)
                    , data = zweit_b, model = "pooling", index = c("district", "week"))


pool.sechst.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data = sechst_a, model = "pooling", index = c("district", "week"))
pool.sechst.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                     + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                     +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                     + A60.79.Anteil 
                     + factor(week)
                     , data = sechst_b, model = "pooling", index = c("district", "week"))



sum.nullt <- summary(pool.nullt)
sum.erst <- summary(pool.erst)
sum.zweit <- summary(pool.zweit)
sum.dritt <-summary(pool.dritt)
sum.viert <-summary(pool.viert)
sum.fuenft <-summary(pool.fuenft)
sum.sechst <-summary(pool.sechst)
sum.siebt <-summary(pool.siebt)
sum.zweit.a <-summary(pool.zweit.a)
sum.zweit.b <-summary(pool.zweit.b)
sum.sechst.a <-summary(pool.sechst.a)
sum.sechst.b <-summary(pool.sechst.b)

sum.nullt$r.squared
sum.erst$r.squared
sum.zweit$r.squared
sum.dritt$r.squared
sum.viert$r.squared
sum.fuenft$r.squared
sum.sechst$r.squared
sum.siebt$r.squared
sum.zweit.a$r.squared
sum.zweit.b$r.squared
sum.sechst.a$r.squared
sum.sechst.b$r.squared


pool.sqrt.actual <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                        + A60.79.Anteil
                        + factor(week)
                        , data =df4_pan, model = "pooling")



pool.sqrt.nullt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                       + A60.79.Anteil
                       + factor(week)
                  , data =nullt, model = "pooling", index = c("district", "week"))

pool.sqrt.erst <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                      + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                      + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                      + A60.79.Anteil
                      + factor(week)
                       , data =erst, model = "pooling", index = c("district", "week"))
pool.sqrt.zweit <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                       + A60.79.Anteil
                       + factor(week)
                       , data =zweit, model = "pooling", index = c("district", "week"))
pool.sqrt.dritt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                       + A60.79.Anteil
                       + factor(week)
                       , data =dritt, model = "pooling", index = c("district", "week"))
pool.sqrt.viert <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                       + A60.79.Anteil
                       + factor(week)
                       , data =viert, model = "pooling", index = c("district", "week"))
pool.sqrt.fuenft <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                        + A60.79.Anteil
                        + factor(week)
                       , data =fünft, model = "pooling", index = c("district", "week"))
pool.sqrt.sechst <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                        + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                        + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                        + A60.79.Anteil
                        + factor(week)
                       , data =sechst, model = "pooling", index = c("district", "week"))
pool.sqrt.siebt <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                       + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                       + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                       + A60.79.Anteil
                       + factor(week)
                       , data =siebt, model = "pooling", index = c("district", "week"))
pool.sqrt.zweit.a <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                         + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                         + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                         + A60.79.Anteil
                         + factor(week)
                       , data =zweit_a, model = "pooling", index = c("district", "week"))
pool.sqrt.zweit.b <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                         + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                         + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                         + A60.79.Anteil
                         + factor(week)
                       , data = zweit_b, model = "pooling", index = c("district", "week"))
pool.sqrt.sechst.a <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                          + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                          + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                          + A60.79.Anteil
                          + factor(week)
                       , data =sechst_a, model = "pooling", index = c("district", "week"))
pool.sqrt.sechst.b <- plm(sqrt(inzidenz) ~ sqrt(lag(inzidenz, 1)) + sqrt(lag(weightednbinz, 1))
                          + I(log(density)*sqrt(lag(inzidenz, 1))) + I(hotspot * sqrt(lag(inzidenz, 1))) 
                          + I(hotspotnb * sqrt(lag(weightednbinz, 1))) + I(rate_zweitimpf * hotspot)
                          + A60.79.Anteil
                          + factor(week)
                       , data =sechst_b, model = "pooling", index = c("district", "week"))



sum.sqrt.nullt <- summary(pool.sqrt.nullt)
sum.sqrt.erst <- summary(pool.sqrt.erst)
sum.sqrt.zweit <- summary(pool.sqrt.zweit)
sum.sqrt.dritt <-summary(pool.sqrt.dritt)
sum.sqrt.viert <-summary(pool.sqrt.viert)
sum.sqrt.fuenft <-summary(pool.sqrt.fuenft)
sum.sqrt.sechst <-summary(pool.sqrt.sechst)
sum.sqrt.siebt <-summary(pool.sqrt.siebt)
sum.sqrt.zweit.a <-summary(pool.sqrt.zweit.a)
sum.sqrt.zweit.b <-summary(pool.sqrt.zweit.b)
sum.sqrt.sechst.a <-summary(pool.sqrt.sechst.a)
sum.sqrt.sechst.b <-summary(pool.sqrt.sechst.b)

sum.sqrt.nullt$r.squared
sum.sqrt.erst$r.squared
sum.sqrt.zweit$r.squared
sum.sqrt.dritt$r.squared
sum.sqrt.viert$r.squared
sum.sqrt.fuenft$r.squared
sum.sqrt.sechst$r.squared
sum.sqrt.siebt$r.squared
sum.sqrt.zweit.a$r.squared
sum.sqrt.zweit.b$r.squared
sum.sqrt.sechst.a$r.squared
sum.sqrt.sechst.b$r.squared



nullt_pan<-pdata.frame(nullt,index=c("district","week"))

pool.weighted.nullt<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                         + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                         +I(hotspotnb * lag(weightednbinz, 1)) 
                         + A60.79.Anteil 
                         + factor(week)
                         , data =nullt_pan, model = "pooling", weights = 1/sqrt(inzidenz + 1))


erst_pan<-pdata.frame(erst,index=c("district","week"))

pool.weighted.erst<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                        + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                        +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                        + A60.79.Anteil 
                        + factor(week)
                        , data =erst_pan, model = "pooling", weights = 1/sqrt(inzidenz + 1))




zweit_pan<-pdata.frame(zweit,index=c("district","week"))

pool.weighted.zweit<-plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                         + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                         +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                         + A60.79.Anteil 
                         + factor(week)
                         , data =zweit_pan, model = "pooling", weights = 1/sqrt(inzidenz + 1))


dritt_pan <-pdata.frame(dritt,index=c("district","week"))

pool.weighted.dritt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                           + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                           +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                           + A60.79.Anteil 
                           + factor(week)
                           , data =dritt_pan, model = "pooling", weights = 1/sqrt(inzidenz + 1),)

pool.weighted.viert <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                           + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                           +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                           + A60.79.Anteil 
                           + factor(week)
                           , data =viert, model = "pooling", weights = 1/sqrt(inzidenz + 1), index=c("district", "week"))

pool.weighted.fuenft <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                            + A60.79.Anteil 
                            + factor(week)
                            , data =fünft, model = "pooling", weights = 1/sqrt(inzidenz + 1), index=c("district", "week"))

pool.weighted.sechst <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                            + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                            +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                            + A60.79.Anteil 
                            + factor(week)
                            , data =sechst, model = "pooling", weights = 1/sqrt(inzidenz + 1), index=c("district", "week"))

pool.weighted.siebt <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                           + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                           +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                           + A60.79.Anteil 
                           + factor(week)
                           , data =siebt, model = "pooling", weights = 1/sqrt(inzidenz + 1), index=c("district", "week"))

pool.weighted.zweit.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                             + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                             +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                             + A60.79.Anteil 
                             + factor(week)
                             , data = zweit_a, model = "pooling", weights = 1/sqrt(inzidenz + 1), index = c("district", "week"))


pool.weighted.zweit.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                             + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                             +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                             + A60.79.Anteil 
                             + factor(week)
                             , data = zweit_b, model = "pooling", weights = 1/sqrt(inzidenz + 1), index = c("district", "week"))


pool.weighted.sechst.a <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                              + A60.79.Anteil 
                              + factor(week)
                              , data = sechst_a, model = "pooling", weights = 1/sqrt(inzidenz + 1), index = c("district", "week"))

pool.weighted.sechst.b <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) 
                              + I(log(density)*lag(inzidenz, 1)) + I(hotspot * lag(inzidenz, 1)) 
                              +I(hotspotnb * lag(weightednbinz, 1)) + I(rate_zweitimpf * hotspot) 
                              + A60.79.Anteil 
                              + factor(week)
                              , data = sechst_b, model = "pooling", weights = 1/sqrt(inzidenz + 1), index = c("district", "week"))


sum.weighted.nullt <- summary(pool.weighted.nullt)
sum.weighted.erst <- summary(pool.weighted.erst)
sum.weighted.zweit <- summary(pool.weighted.zweit)
sum.weighted.dritt <-summary(pool.weighted.dritt)
sum.weighted.viert <-summary(pool.weighted.viert)
sum.weighted.fuenft <-summary(pool.weighted.fuenft)
sum.weighted.sechst <-summary(pool.weighted.sechst)
sum.weighted.siebt <-summary(pool.weighted.siebt)
sum.weighted.zweit.a <-summary(pool.weighted.zweit.a)
sum.weighted.zweit.b <-summary(pool.weighted.zweit.b)
sum.weighted.sechst.a <-summary(pool.weighted.sechst.a)
sum.weighted.sechst.b <-summary(pool.weighted.sechst.b)

sum.weighted.nullt$r.squared
sum.weighted.erst$r.squared
sum.weighted.zweit$r.squared
sum.weighted.dritt$r.squared
sum.weighted.viert$r.squared
sum.weighted.fuenft$r.squared
sum.weighted.sechst$r.squared
sum.weighted.siebt$r.squared
sum.weighted.zweit.a$r.squared
sum.weighted.zweit.b$r.squared
sum.weighted.sechst.a$r.squared
sum.weighted.sechst.b$r.squared


summary(pool.nullt)
coeftest(pool.nullt, vcovHC(pool.nullt, type = "HC0"))
summary(pool.erst)
summary(pool.zweit)
summary(pool.dritt)
summary(pool.viert)
summary(pool.fuenft)
summary(pool.sechst)
summary(pool.siebt)









