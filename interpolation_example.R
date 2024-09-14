# Tests of interpolation methods in time series

# From StackOverflow
# https://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr

# See also 
# https://stackoverflow.com/questions/33186316/linear-interpolate-missing-values-in-time-series
# and maybe
# https://otexts.com/fpp2/missing-outliers.html
# https://business-science.github.io/timetk/index.html


# Make test sata
event.date <- c("2010-05-25", "2010-09-10", "2011-05-13", "2012-03-28", "2013-03-07",
"2014-02-13", "2010-06-11", "2010-09-10", "2011-05-13", "2012-03-28",
"2013-03-07", "2014-02-13")
variable   <- c("neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd", "neck.bmd",
"wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd", "wbody.bmd")
value      <- c(0.7490, 0.7615, 0.7900, 0.7730, NA, 0.7420, 1.0520, 1.0665, 1.0760,
1.0870, NA, 1.0550)
## Bind into a data frame
df <- data.frame(event.date, variable, value)
rm(event.date, variable, value)
## Convert date
df$event.date <- as.Date(df$event.date)

## Load libraries
library(magrittr)
library(xts)
library(zoo)
library(dplyr)

# Use xts::xts() to make a tiem series and zoo::na.approx() to fill it in

## group and then arrange the data (to ensure dates are correct)
df %>%
  group_by(variable) %>%
  arrange(variable, event.date) %>%
  xts(x = .$value, order.by = .$event.date) %>%
  na.approx()

# DON'T need xts, and can mutate a column to keep the data together
# DO need to provide a sequence to simulate a time series
df %>%
  group_by(variable) %>%
  arrange(variable, event.date) %>%
  mutate(
    ip.value = na.approx(value, maxgap = 4, rule = 2),
    time = seq(1, n()),
    ap.value = approx(time, y = value, time)$y
  )

# but don't need to make the sequence an explicit column
df %>%
  group_by(variable) %>%
  arrange(variable, event.date) %>%
  mutate(
    ip.value = zoo::na.approx(value, maxgap = 4, rule = 2),
    ap.value = approx(seq(1, n()), y = value, n = n())$y
  )
