

dfmunich_sk<-dfultimate[dfultimate$district=="SK München",]
dfmunich_lk<-dfultimate[dfultimate$district=="LK München",]

dfmunich_sk_pan<-pdata.frame(dfmunich_sk, index=c("district", "week"))
dfmunich_lk_pan<-pdata.frame(dfmunich_lk, index=c("district", "week"))





