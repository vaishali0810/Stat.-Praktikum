install.packages("lme4")
library(lme4)

lm(cases ~ state + kr_erstimpf+ kr_zweitimpf + kr_drittimpf,  data=dbayern3)

#dbayern3$district <- as.factor(dbayern3$district)
# dbayern3$population <- as.numeric(dbayern3$population)
# dbayern3$male <- as.numeric(dbayern3$male)
# dbayern3$female <- as.numeric(dbayern3$female)
# dbayern3$density <- as.numeric(dbayern3$density)
# dbayern3$area <- as.numeric(dbayern3$area)
# str(dbayern3)

glm(cases ~ district , family = negative.binomial(2), data=dbayern3)
isfactor(dbayern3)



library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)   
dbayern3 <- plm.data(dbayern3, index=c("district", "date"))
plm(cases~district + kr_erstimpf+ kr_zweitimpf + kr_drittimpf, data=dbayern3, model = "random")
