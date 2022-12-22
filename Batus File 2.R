library(lmtest)

library(lmtest)
bptest(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
       + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil +density
       + A35.59.Anteil + A60.79.Anteil + A80.Anteil
       - 1, data = df_pan, studentize=F)

model <- plm(inzidenz ~ lag(inzidenz, 1) + lag(weightednbinz, 1) + lag(inzidenz,2) + lag(weightednbinz, 2)
             + A00.04.Anteil + A05.14.Anteil+ A15.34.Anteil + M.Anteil +density
             + A35.59.Anteil + A60.79.Anteil + A80.Anteil
             - 1, data =df_pan, model = "random")
lmtest::bptest(model)

# Presence of heteroskedasticity da p kleiner als 0.05 kein pls sondern glm