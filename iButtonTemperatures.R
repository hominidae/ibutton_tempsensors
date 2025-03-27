# Load temperature data from iButton
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)

# Load first data file
cf1h <- read_csv("data/Coldframe_01_High.csv", skip = 18)
cf1l <- read_csv("data/Coldframe_01_Low.csv", skip =18)
cf2h <- read_csv("data/Coldframe_02_High.csv", skip=18)
cf2l <- read_csv("data/Coldframe_02_Low.csv", skip=18)
cf3h <- read_csv("data/Coldframe_03_High.csv", skip=18)
cf3l <- read_csv("data/Coldframe_03_Low.csv", skip=18)
ghh <- read_csv("data/Greenhouse_High.csv", skip=18)
ghm <- read_csv("data/Greenhouse_Mid.csv", skip=18)
ghl <- read_csv("data/Greenhouse_Low.csv", skip=18)
# ambient <- read_csv("data/Ambient.csv, skip=18)

# Fix the csv data a bit. We're changing dmyhms to ymdhms as that is the correct format literally everywhere!
# Why is America so broken when it comes to common sense?
cf1h <- cf1h %>%
  mutate(
    date = dmy_hms(cf1h$`Date/Time`),
    temp = cf1h$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Repeat for cf1l
cf1l <- cf1l %>%
  mutate(
    date = dmy_hms(cf1l$`Date/Time`),
    temp = cf1l$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for cf2h
cf2h <- cf2h %>%
  mutate(
    date = dmy_hms(cf2h$`Date/Time`),
    temp = cf2h$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for cf2l
cf2l <- cf2l %>%
  mutate(
    date = dmy_hms(cf2l$`Date/Time`),
    temp = cf2l$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for cf3h
cf3h <- cf3h %>%
  mutate(
    date = dmy_hms(cf3h$`Date/Time`),
    temp = cf3h$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for cf3l
cf3l <- cf3l %>%
  mutate(
    date = dmy_hms(cf3l$`Date/Time`),
    temp = cf3l$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for ghh
ghh <- ghh %>%
  mutate(
    date = dmy_hms(ghh$`Date/Time`),
    temp = ghh$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for ghm
ghm <- ghm %>%
  mutate(
    date = dmy_hms(ghm$`Date/Time`),
    temp = ghm$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for ghl
ghl <- ghl %>%
  mutate(
    date = dmy_hms(ghl$`Date/Time`),
    temp = ghl$Value
  ) %>%
  mutate(
    ymd = format(date, "%Y-%m-%d"),
    time = format(date, "%H:%M:%S")
  ) %>%
  select(
    date,
    ymd,
    time,
    temp
  )

# Do the same for ambient
#ambient <- ambient %>%
#  mutate(
#    date = dmy_hms(ambient$`Date/Time`),
#    temp = ambient$Value
#  ) %>%
#  mutate(
#    ymd = format(date, "%Y-%m-%d"),
#    time = format(date, "%H:%M:%S")
#  ) %>%
#  select(
#    date,
#    ymd,
#    time,
#    temp
#  )

# Generate a plot from that data for CF 1 high
ggplot(data = cf1h, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 1 High")

# Generate a plot for CF 1 low
ggplot(data = cf1l, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 1 Low")

# Coldframe 2 High
ggplot(data = cf2h, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 2 High")

# Coldframe 2 Low
ggplot(data = cf2l, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 2 Low")

# Coldframe 3 High
ggplot(data = cf3h, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 3 High")

# Coldframe 3 Low
ggplot(data = cf3l, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Coldframe 3 Low")

# Greenhouse High
ggplot(data = ghh, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Greenhouse High")

# Greenhouse Mid
ggplot(data = ghm, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Greenhouse Mid")

# Greenhouse Low
ggplot(data = ghl, mapping = aes(x = date, y = temp)) +
  geom_point(color = "black") +
  geom_line(color = "blue") +
  geom_smooth(method = "loess") +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("6 hour"),
                   date_labels = "%D") +
  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart - Greenhouse Low")

# Ambient
# Greenhouse Low
#ggplot(data = ambient, mapping = aes(x = date, y = temp)) +
#  geom_point(color = "black") +
#  geom_line(color = "blue") +
#  geom_smooth(method = "loess") +
#  scale_x_datetime(breaks = date_breaks("1 day"),
#                   minor_breaks = date_breaks("6 hour"),
#                   date_labels = "%D") +
#  scale_y_continuous(breaks = as.integer(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))) +
#  labs(x = "Date", y = "Temperature", title="Temperature Chart - Ambient")
