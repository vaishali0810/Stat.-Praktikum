library(plotly)
library(ggpubr)

plot_ly(
  data = df91,
  x = ~date, 
  y = ~cases,
  type= "scatter",
  mode = "lines",
  color = ~gender
)

ggplot(data=df, aes(x= date, y=cases)) + 
  geom_line(aes(color = source_clean)) + 
  scale_x_date(date_breaks = "months" , date_labels = "%d-%b-%y")

qplot(date, cases, data=df91,
      geom="line", color =gender)

ggdensity(df91,"date",fill="gender",add="mean")

