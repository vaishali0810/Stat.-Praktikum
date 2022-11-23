library(plotly)

plot_ly(
  data = df91,
  x = ~date, 
  y = ~cases,
  type= "scatter",
  mode = "lines",
  color = ~gender,
)
qplot(date, cases, data=df91,
      geom="line", color =gender)

ggdensity(df91,"date",fill="gender",add="mean")

px.line(df91, x=date , y=cases,
        color = ~gender)