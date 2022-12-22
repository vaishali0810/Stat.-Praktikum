fe1 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
           + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
           + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
           , data =df_pan, model = "within")
summary(fe1)



re100 <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil 
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil + rate_zweitimpf
             - 1, data =df_pan, model = "random")
summary(re100)


phtest(fe1, re100)




