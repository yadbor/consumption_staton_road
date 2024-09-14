library(data.table)
library(ggplot2)
library(lubridate)
library(googlesheets4)
library(dplyr)
#library(stringr)


# sheet_url <- "https://docs.google.com/spreadsheets/d/1YSv-EKJq7lm0wUCAOsnJ9y7OCkmaF2-gNEx6crz9diA/"
# sheet <- googlesheets4::read_sheet(sheet_url)


# the data source has n rows of data and a header row with the names
# date, water, gas, made, bought, sold, people, notes
# date  date of reading.
# water reading from water meter. reset when new meter installed.
# gas   reading from gas meter.
# made  Etotal reading from solar power inverter.
# bought  power read from meter (electricity from network to house).
# sold  ALT1 reading from meter (electricity from house to network).
# people  (roughly) the number of people staying in the house over the period.
# notes any additional text notes about the weather, faults etc.
sheet_id <- "1YSv-EKJq7lm0wUCAOsnJ9y7OCkmaF2-gNEx6crz9diA"
readings <- googlesheets4::read_sheet(sheet_id, sheet = "Sheet1")

# Drop the "chk..." columns used to validate data entry

readings <- readings %>% select(!starts_with("chk_"))

# There have been changes to both the water and gas meters, each of which
# restarted the reading on that meter. Fix these by finding the last reading on
# the old meter and adding to the first reading on the new meter.

# Fix gaps in a vector of readings that are expected to increase monotonically.
# Take the difference of the sequence, and when it goes backwards (diff < 0)
# replace those values with the current meter reading, which is the diff from
# the new meter starting at zero to the first reading of the new meter.
# Then add up all of the fixed readings to give the correct monotonic sequence.
fix_gaps <- function(v) {
  d = c(v[1], diff(v))
  res <- cumsum(if_else(d<0, v, d))
  return(res)
}

# The new fix_gaps procedure will not change an existing monotonic sequence, so
# could be applied to any sequence of readings, but only water & gas changed.

readings <- readings |>
  mutate(wate = fix_gaps(water), gas = fix_gaps(gas))



# Calculate the total electricity used (solar used on site plus bought from grid)
readings <- readings %>% 
  mutate(used = (made - sold) + bought)

# Remove readings with known problems, identified in the notes column
readings <- readings %>% 
  filter(!stringr::str_detect(notes, "PROBLEM"))
  # Did not record electricity until after installing solar panels, so there 
  # is about a year with only gas & water readings.
  # For clarity in plotting only use data where we have all meters.  
readings <- readings %>% tidyr::drop_na(water, gas, made, bought, sold, people)

# Find the number of days between readings and calculate daily amounts
daily <- readings %>% 
  reframe(days = as.integer(diff(readings$date) / ddays(1)), 
          across(c(water, gas, made, bought, sold, used), ~ diff(.x)/days),
          # Drop the first row of non-meter columns to align with the diffs
          date = date[-1],
          people = people[-1],
          notes = notes[-1]
  )

# Remove readings with known problems, identified in the notes column
daily <- daily %>% filter(!stringr::str_detect(notes, "PROBLEM"))

# Did not record electricity until after installing solar panels, so there 
# is about a year with only gas & water readings.
# For clarity in plotting only use data where we have all meters.
NonNAindex <- which(!is.na(readings$made))
first_solar <- min(NonNAindex)

daily <- daily %>% slice(-(1:first_solar))

# Rearrange into long form for plotting
daily_long <- daily %>% 
  tidyr::pivot_longer(cols = c(water, gas, made, bought, sold, used),
                      names_to = "meter",
                      values_to = "reading"
)

all_plot <- daily_long %>% ggplot() + 
                aes(x = date, y = reading, group = meter, colour = meter) +
                geom_point() + geom_line() +
                facet_wrap(vars(meter), ncol = 2, scales = "free_y") +
                labs(title = "all data")
print(all_plot)


# Solar performance is seasonal, so plot electricity readings by month
electricity <- daily_long %>% 
  filter(meter %in% c("bought", "made", "sold", "used"))

# There are a couple of odd outliers in the bought. & used plots.
# They may be due to 

season_plot <- electricity %>% ggplot() + 
  aes(x = month(date, label = TRUE, abbr = TRUE), 
      y = reading
      ) + 
  geom_point(aes(colour = year(date))) + 
  geom_smooth(aes(x = month(date)), span = 0.4) +
  facet_wrap(~meter) + 
  labs(title = "seasonal variation", x = "month", y = "value")
print(season_plot)


# Annual averages for electricity, to work out how much we could save on 
# electricity costs by using all solar generation on site (ie with a battery).
averages <- electricity %>% 
  group_by(meter, yr = year(date)) %>% 
  summarise(average = mean(reading, na.rm = TRUE))

print(averages %>% filter(meter == "sold"))

# How are the averages varying over time?
average_plot <- averages %>% ggplot() + 
  aes(x=yr, y = average, colour = meter, group = meter) + 
  geom_point() + geom_line() + 
  geom_smooth(method = "lm") +
  labs(title = "electricity annual average", x = "year")
print(average_plot)

# Costs from Synergy 2023-07-02
A1_tariff <- 27.3277
natural_power_charge <- 4.9680
buyback <- 7.1350

cost_per_unit <- A1_tariff + natural_power_charge - buyback
averages %>% mutate(annual = 365.25 * average, 
                    cost = annual * (cost_per_unit/100)) %>%
  filter(meter == "sold") %>%
  print()
