## Consumption data from google docs file

library(data.table)
library(ggplot2)

####################################################
g.doc <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ0dz7rlUoZDa4z1nwXtQFwwVli9flGD8ZsgaNgSTjG5YrD09q0iqaZbSMe04aWMkFhCSboSxwz3Fxc/pub?gid=0&single=true&output=csv"

#meter.readings <- fread("meter.readings.csv")
meter.readings <- fread(g.doc)

# make usable dates, based on the google sheets date format
meter.readings[, date:=as.Date(date, "%d/%m/%y")]

# work out how much we actually used (we sell some of what we make and use all that we buy)
meter.readings[, used:=(made-sold)+bought]

# calculate the incremental change (the difference from previous reading)
cols <- c("water","gas","made","bought","sold", "used")
daily <- meter.readings[, lapply(.SD, diff), .SDcols=cols]
# work out how many days between readings
daily[, span:=as.integer(diff(meter.readings$date))]
# divide change by number of days to calcluate the daily increment
daily[, (cols) := lapply(.SD, "/", span), .SDcols=cols]

# now add back the date of the reading and the number of people in the house for that week
# drop the first row so that they line up with the diff'ed readings
cols2 <- c("date","people")
daily[, (cols2) := meter.readings[, tail(.SD, -1),  .SDcols=cols2]]


# censor the water consumption for 13/04/2013 as we had a leak
daily[date=="2013-04-13", water:=NA]

# put into long format for ggplotting
daily.l <- melt(daily, id.vars="date")

# plot the measured qunatities (i.e.not span or number of people) monthly
all.01 <- ggplot(daily.l[!(variable %in% c("span","people")), ]) + 
  aes(x=month(date), y=value, colour=year(date)) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~variable, scales="free")
print(all.01)

# really need to do proper monthly averaging
monthly <- daily[, lapply(.SD, mean), by=.(year(date),month(date))]
# melt for plotting
monthly.l <- melt(monthly, id.vars=c("year","month"))
# again, don't care about who was here or how long between readings
month.p <- ggplot(monthly.l[!(variable %in% c("span","people")), ]) + 
  aes(x=month, y=value, colour=year) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~variable, scales="free")
print(month.p)

