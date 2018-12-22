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

# zipped file is at
# http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=monthlyZippedDataFile&p_stn_num=009192&p_c=-17068615&p_nccObsCode=203&p_startYear=


# # unzip & extract IDCJAC0003_009192_Data12.csv
# 
# insolation.BOM <- fread("IDCJAC0003_009192_Data12.csv", na.strings="null")
# 
# insolation <- melt(insolation.BOM[, .(Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)], 
#                    id.vars="Year", variable.name = "Month", value.name = "MJ")
# 
# # use all lowercase convention for year & month to match other data tables
# setnames(insolation, c("year","month","MJ"))
# 
# # actual generation is MJ x panel area x efficiency.
# # work out efficiency from area and actual output
# insolation[, MJ:=MJ*area]
# # convert to kw.hr
# insolation[, kw.hr := MJ*MJ.to.kwhr]

### 20180924 RED  changed to BOM monthly data product. easier to manipulate, so less chance of errors

insolation.BOM <- fread("IDCJAC0003_009192_Data1.csv", na.strings="null")
# use all lowercase convention for year & month to match other data tables
setnames(insolation.BOM, c("product","station","year","month", "MJ"))
insolation.BOM[, kw.hr := MJ*MJ.to.kwhr]
# and use month names rather than integers, like the other tables
insolation.BOM[, month:=month.abb[month]]

insolation <- insolation.BOM

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


# if the monthly data is not already loaded, create is
if (!exists("monthly")) source("consumption_08.r")

# look at output vs insolation to estimate the efficiency of our system
comp <- monthly[insolation, on=c("year","month")]

# start from when solar panels were installed (NA before that)
comp <- comp[complete.cases(comp)]

## plot by month & year
#comp[, ym:=paste(year, month)]
#ggplot(comp)+aes(x=kw.hr, y=made, colour=ym)+geom_point()+theme(legend.position="none")

# show a trend line - it's obviusly non-linear
ggplot(comp)+aes(x=kw.hr, y=made)+geom_point()+geom_smooth(span=1)

# make a segmented plot
library(segmented)
linear.model <- lm(made~kw.hr, data=comp)
segmented.model <- segmented(linear.model, seg.Z=~kw.hr)

# now use the segmented model to make some data points to plot
comp[, fit:= predict(segmented.model)]
comp[, group:=segmented.model$id.group]
seg.plot <- ggplot(comp)+aes(x=kw.hr, y=made)+geom_point()+geom_smooth(span=1)+geom_line(aes(y=fit, group=group), colour="red")+theme(legend.position="none")
#print(seg.plot)

# extract the breakpoint(s) and 95% CI(s)
breakp <- data.frame(confint(segmented.model))
setnames(breakp, c("est","lwr","upr"))

# plot the intercept & 95% CI
# linetype 1 is solid, 2 is dashed
print(seg.plot + geom_vline(xintercept=unlist(breakp), linetype=c(1,2,2)) + coord_fixed(ratio=1) )
