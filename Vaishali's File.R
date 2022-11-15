# Vaishali's Code

trends <- read.csv(unzip("trends.zip"))
trends

trends$datum <- as.Date(trends$datum)
trendsBayern <- trends[trends$bundesland == "Freistaat Bayern", ]

# Impfungen
impf <- read.csv("impfdaten_regional.csv")
impf

impf$datum <- as.Date(impf$datum)
impfBayern <- impf[impf$bundesland == "Freistaat Bayern", ]
impfBayern

## Plots
# München Impfung
impfMuc <- impfBayern[impfBayern$kreis == "MÃ¼nchen, Kreis", ]
impfMuc
ggplot(impfMuc, aes(y = kr_erstimpf, x = datum)) + geom_line(stat = "identity")

# Traunstein Impfung
impfTraunstein <- impfBayern[impfBayern$kreis == "Traunstein", ]
impfTraunstein
ggplot(impfTraunstein, aes(y = kr_erstimpf, x = datum)) + geom_line(stat = "identity")

