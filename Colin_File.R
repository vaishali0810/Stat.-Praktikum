#data <- readRDS("cases_GermanTemporal_2022-10-25.rds")
head(data)
summary(data)


library(ggplot2)
library(tidyr)
library(dplyr)

theme <- theme_classic() +
  theme(text = element_text(size = 12), axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text.y = element_text(size = 12), legend.text.align = 0,
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))


length(data)
vectorstate <- data[, 1]
head(vectorstate)
truth_vector <- rep.int(FALSE, 6285344)

# truth_vector <- vapply(vectorstate, function(i) {data[i, 1] == "Bayern"}, TRUE)
# head(vectorstate)

#data_bayern <- data[truth_vector]
#data_bayern <- data[data$state == "Bayern", ]

getwd()


ggplot(dbayern, aes(y = cases, x = date, color= cases)) + #mapping = aes(x = new_cases)
  geom_line() +
  theme

# facet_wrap(facets = ~variable, scales = "free") + theme

ggplot(data, aes(y = cases, x = date)) + #mapping = aes(x = new_cases)
  geom_line(data$state) +
  theme

# ggplot(dbayern, aes(y = cases, x = date, color= district)) + #mapping = aes(x = new_cases)
#    geom_line() +
#    theme

ggplot(data = data, aes(x = date, y = cases)) +
  geom_line() +
  facet_wrap(facets = vars(state))

## only 4 districts levels are not in Bayern, 3 are not in Bavaria, one is other


test1<-as.vector(summary(dbayern$district))

test2 <- summary(dbayern$district)

# labels(test2)
# dim(test2)
# length(test2)
# names(test2)

test5 <- data.frame(matrix(NA, nrow=length(test2), ncol=2))
test5[,1] <- labels(test2)
test5[,2] <- as.vector(summary(dbayern$district))
test5 <- test5[ -c(97:100), ]
test5

colnames(test5) <- c("district", "Einträge")
test5
plot(test5$district[1:10,], test5$Einträge)

table(dbayern$district, dbayern$cases)
