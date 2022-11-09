# Vaishali's Code

trends <- read.csv(unzip("trends.zip"))
trends

trendsBayern <- trends[trends$bundesland == "Freistaat Bayern", ]
