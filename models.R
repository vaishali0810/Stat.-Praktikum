install.packages("lme4")
library(lme4)


dfcombined <- df

summary(lm(inzidenz~ bezirk + lag(inzidenz, 7) + gender + erstimpf_sum + zweitimpf_sum + drittimpf_sum,  data=dbayern5))
str(dbayern5)
#dbayern3$district <- as.factor(dbayern3$district)
# dbayern3$population <- as.numeric(dbayern3$population)
# dbayern3$male <- as.numeric(dbayern3$male)
# dbayern3$female <- as.numeric(dbayern3$female)
# dbayern3$density <- as.numeric(dbayern3$density)
# dbayern3$area <- as.numeric(dbayern3$area)
# str(dbayern3)

# glm(cases ~ district , family = negative.binomial(2), data=dbayern3)
# isfactor(dbayern3)

dbayern6 <- dbayern3[,-6]
dbayern7 <- pdata.frame(dbayern6, index=c("district", "date", "age_group"))




library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
#library(ggplots2)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)   
# dbayern4 <- pdata.frame(dfcombined, index=c("district", "gender", "age_group"))
# dbayern5 <- dbayern4[complete.cases(dbayern4),]
# #re1 <- plm(inzidenz~ bezirk + erstimpf_sum + zweitimpf_sum + drittimpf_sum + male_anteil, data=dbayern5, model = "random")
# summary(re1)

summary(glm(inzidenz ~ + I(zweitimpf_sum/population) + 
             I(drittimpf_sum/population) + lag(inzidenz, 7) + I(lag(inzidenz, 7) * log(density)) , data = dbayern3))


testpdf <- pdata.frame(dfcombined, index=c("district", "date"))
re1 <- plm(total_cases~lag(total_cases, 7), data= testpdf, model = "random")




dfultimate_pan <- pdata.frame(dfultimate, index=c("district", "week"))


re2 <- plm(inzidenz ~ lag(inzidenz, 1) + density + rate_zweitimpf + m_anteil, data = dfultimate_pan, model = "random")

