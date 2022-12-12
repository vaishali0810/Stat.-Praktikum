dfcombined <- read.csv("dfcombined.csv", header = TRUE, sep =",")
View(dfcombined)

dfcombined$date <- as.Date(dfcombined$date)

dfcombined <- dfcombined %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

dfcombined <- dfcombined %>% arrange(district, date)
View(dfcombined)


df_comb_week<-dfcombined%>%group_by(district,week)%>%summarise(M.A00.04 =sum(M.A00.04),
                                                                    M.A05.14 = sum(M.A05.14),
                                                                    M.A15.34 = sum(M.A15.34),    
                                                                    M.A35.59 = sum(M.A35.59),
                                                                    M.A60.79 = sum(M.A60.79), 
                                                                    M.A80. = sum(M.A80.),
                                                                    M.Aunb = sum(M.Aunb),
                                                                    `F.A00.04`=sum(`F.A00.04`),
                                                                    F.A05.14 = sum(F.A05.14),
                                                                    F.A15.34 = sum(F.A15.34),    
                                                                    F.A35.59 = sum(F.A35.59),
                                                                    F.A60.79 = sum(F.A60.79), 
                                                                    F.A80. = sum(F.A80.),
                                                                    F.Aunb = sum(F.Aunb),
                                                                    `Unb.A00.04`=sum(`Unb.A00.04`),
                                                                    Unb.A05.14 = sum(Unb.A05.14),
                                                                    Unb.A15.34 = sum(Unb.A15.34),    
                                                                    Unb.A35.59 = sum(Unb.A35.59),
                                                                    Unb.A60.79 = sum(Unb.A60.79), 
                                                                    Unb.A80. = sum(Unb.A80.),
                                                                    Unb.Aunb = sum(Unb.Aunb),
                                                                    total_cases = sum(total_cases),
                                                                    .groups="keep")
# identical(sum(dfcombined$total_cases), sum(df_comb_week$total_cases))
colnames(df_comb_week)

colnames(impfbayern2)


# min(impfbayern2$date)
# [1] "2020-12-27"

colnames(impfungentake)

impfungentake <- impfungentake %>% 
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(district, date)

impfungentake<-impfungentake%>%group_by(district,week)%>%summarise(kr_erstimpf =sum(kr_erstimpf),
                                                               kr_zweitimpf =sum(kr_zweitimpf),
                                                               kr_drittimpf =sum(kr_drittimpf),
                                                               kr_viertimpf =sum(kr_viertimpf),
                                                               .groups="keep")

#time difference 334 days ---> 47 weeks (days are Tuesday and Sunday --> round down)

impfungentake$week <- impfungentake$week + 47

df_comb_week_impf <- merge(df_comb_week, impfungentake, by = c("district", "week"), all.x = TRUE, all.y = TRUE)
# View(df_comb_week_impf)
df_comb_week_impf[is.na(df_comb_week_impf)] <- 0

# any(is.na(df_comb_week_impf))
# identical(sum(df_comb_week_impf$kr_erstimpf, na.rm=TRUE), sum(impfungentake$kr_erstimpf, na.rm=TRUE))

popbay <- read.csv("popBay.csv", header = TRUE, sep = ";")
#View(popbay) 
popbay <- popbay %>% mutate(Kreis...Landkreise = recode(Kreis...Landkreise, "Kreisfreie Stadt" = "SK", "Landkreis" = "LK"))

df_ultimate <- merge(df_comb_week_impf, popbay2, by = c("district"), all.x = TRUE, all.y = TRUE)
