library(data.table)
library(ggplot2)
#library(stringr)
library(lubridate)

# data from https://pvwatts.nrel.gov/pvwatts.php for a 2.8 kW system pointing North, 35° elevation, in Perth
# using default efficiency etc. settings
pvwatts <- fread("pvwatts_monthly.csv", skip=17) # rows 1:16 are metadata
pvwatts <- pvwatts[Month!="Total", ] # only want the monthly values
setnames(pvwatts, c("month","AC.out","solar.irrad","plane.power", "DC.out", "value"))
pvwatts[, month:=as.numeric(month)] # comes in as char, need real numbers

MJ.to.kwhr = 1000/3600

rad <- function(angle) {angle*(pi/180)}
deg <- function(angle) {angle*(180/pi)}

# our installation
panels = 16
rows = 2
width = 0.8 # m
length = 1.6 # m
angle = 35 # degrees from horizontal

area = (length*rows)*cos(rad(angle)) * (panels/rows)*width

# insolation data for Fremantle from BOM in MJ/m*m
# http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=203&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=009192

# get http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=monthlyZippedDataFile&p_stn_num=009192&p_c=-17068615&p_nccObsCode=203&p_startYear=
# unzip & extract IDCJAC0003_009192_Data12.csv

insolation.BOM <- fread("IDCJAC0003_009192_Data12.csv", na.strings="null")

insolation <- melt(insolation.BOM[, .(Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)], 
                   id.vars="Year", variable.name = "Month", value.name = "MJ")

# use all lowercase convention for year & month to match other data tables
setnames(insolation, c("year","month","MJ"))

# actual generation is MJ x panel area x efficiency.
# work out efficiency from area and actual output
insolation[, MJ:=MJ*area]
# convert to kw.hr
insolation[, kw.hr := MJ*MJ.to.kwhr]

# insolation plots
i.plot.01 <- ggplot(insolation) + aes(x=month, y=kw.hr)
i.plot.02 <- i.plot.01 + geom_line(aes(group=year), alpha=0.5)
i.plot.03 <- i.plot.02 + geom_boxplot(aes(group=month), width=0.5)
## print(i.plot.03)

i.plot.05 <- ggplot(insolation) + aes(x=month, y=kw.hr) + 
             geom_point(aes(colour=year), alpha=0.5) + geom_line(aes(group=year), alpha=0.1) + 
             geom_smooth(aes(x=as.numeric(month)), span=0.4)
## print(i.plot.05)

i.plot.06 <- ggplot(insolation) + aes(x=month, y=kw.hr) + 
  geom_point(aes(colour=as.factor(year)), alpha=0.5) + geom_line(aes(group=year, colour=as.factor(year)), alpha=0.2) + 
  geom_smooth(aes(x=as.numeric(month)), span=0.4) + guides(colour=FALSE)
## print(i.plot.06)

# add collected power to insolation plot

i.plot.07 <- i.plot.06 + geom_point(data=daily,  aes(x=month(date), y=made)) + 
                         geom_smooth(data=daily, aes(x=month(date), y=made), span=0.4)

####################################################
# data can be from local file or from google docs sheet
data.source <- list(local = "meter.readings.csv",
                    g.doc = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ0dz7rlUoZDa4z1nwXtQFwwVli9flGD8ZsgaNgSTjG5YrD09q0iqaZbSMe04aWMkFhCSboSxwz3Fxc/pub?gid=0&single=true&output=csv"
                    )


use.source <- "g.doc"

meter.readings <- fread(data.source[[use.source]])

# fix any odd date formats.
# google docs occasionally gives two digit years, but always in d m y order for my data
meter.readings$date <-as.Date(parse_date_time(x = meter.readings$date, orders = c("d m y")), tz = "UTC")

# work out how much we actually used (we sell some of what we make and use all that we buy)
meter.readings[, used:=(made-sold)+bought]

# find where the new water meter was installed (read on day before change then at the end of the next week)
change <- meter.readings[,which(shift(water)>water)]
# so the last reading before the change needs to be added to all subsequent readings to get the cumulative reading
last.old <- meter.readings[change-1, water]
meter.readings[,water:=c(water[1:change-1],water[change:.N]+last.old)]

# calculate the incremental change (the difference from previous reading)
measure.cols <- c("water","gas","made","bought","sold", "used")
daily <- meter.readings[, lapply(.SD, diff), .SDcols=measure.cols]

# water meter changed on 2018-06-19, starts again from 0000
# look for discontinuity in diff() values and substitute actual amount (i.e. from zero)
daily[water<0, water:=meter.readings[c(FALSE, diff(water)<0),water] ]

# work out how many days between readings
daily[, span:=as.integer(diff(meter.readings$date))]
# divide change by number of days to calcluate the daily increment
daily[, (measure.cols) := lapply(.SD, "/", span), .SDcols=measure.cols]

# add back the date of the reading, the number of people in the house for that week and any notes
# drop the first row so that they line up with the diff'ed readings
info.cols <- c("date","people", "notes")
daily[, (info.cols) := meter.readings[, tail(.SD, -1),  .SDcols=info.cols]]

# add month and year variables for later ease
daily[, c("year", "month") := list(year(date), month(date))]

# censor the water consumption for known problems
daily[date=="2013-04-13", water:=NA] # had a leak
daily[date=="2010-11-07", water:=NA] # broken tap timer
daily[date=="2017-12-23", water:=NA] # tap left on trees overnight

# quantities that make sense to plot monthly put into long format for ggplotting
daily.l <- melt(daily, id.vars="date", measure.vars=measure.cols)
# add month & year for plotting
daily.l[, c("year", "month") := list(year(date), month(date))]

# plot the measured quantities (i.e.not span or number of people or notes) monthly
# reorder the variables to get the plot we want (consumption in top row, power made, in & out in the bottom)
daily.l[, variable:= factor(variable, levels=c("water","gas","used","made","sold","bought"))]

# plot all the data just over time
raw <- ggplot(daily.l) + 
                aes(x=date, y=value) + 
                geom_line(colour="grey") + geom_point() + 
                facet_wrap(~variable, scales="free") +
                labs(title="weekly readings", x="time", y="value per day")
print(raw)

# plot by month to show the seasonal variation
seasonal <- ggplot(daily.l) + 
                  aes(x=month(date, label=TRUE, abbr = TRUE), y=value, colour=year) + 
                  geom_point() + geom_smooth(aes(x=month(date)), span=0.4) +
                  facet_wrap(~variable, scales="free") + 
                  labs(title="seasonal variation", x="month", y="value")
print(seasonal)

# calculate monthly averages to stabilise the variance a bit
# average date to put average measurement at reasonable mid point for each month
monthly.l <- daily.l[, .(monthly=mean(value, na.rm=TRUE), date=mean(date)), by="variable,month,year"]

# to get the anomaly we need the average by month across all years
# na.rm because we have some missing data (particularly pre-solar power figures)
monthly.means <- monthly.l[, .(mean=mean(monthly, na.rm = TRUE)), by="variable,month"]

# now the anomaly is each monthly average - the average for that month over the whole data set
# use a right join to bring in the matching monthly mean data for each variable
anomaly.l <- monthly.l[monthly.means, .(variable, year, month, date, monthly, mean=mean, anomaly=monthly-mean), on=c("variable","month"), ]

trends <- ggplot(anomaly.l[variable %in% c("water","gas","made","used"),]) + 
                  aes(x=date, y=anomaly) + 
                  geom_point() + geom_smooth() +
                  facet_wrap(~variable, scales="free") + 
                  labs(title="long term trends", x="year", y="anomaly")
print(trends)
