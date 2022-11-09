# Vaishali's Code

trends <- read.csv(unzip("trends.zip"))
trends

trendsBayern <- trends[trends$bundesland == "Freistaat Bayern", ]


impf <- read.csv("impfdaten_regional.csv")
impf


impfBayern <- impf[impf$bundesland == "Freistaat Bayern", ]
impfBayern


## Plots
