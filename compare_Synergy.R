library(data.table)
library(ggplot2)

MA <- fread("MA Consumption history.csv")
setnames(MA, c("date", "days", "daily", "total", "amount", "export", "CO2"))

MA[, date := as.Date(MA$date, "%d %b %Y")]
comp <- ggplot(daily[date >= as.Date('2018-02-15')-65, ]) + 
  aes(x=date, y = sold) + geom_line() + 
  geom_line(data = MA, aes(x = date, y = export/days), colour = "red")
print(comp)
