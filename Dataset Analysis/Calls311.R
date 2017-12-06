library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(ggmap)
library(ggthemes)

calls = read.csv("Data/311_calls_w_CTs20171102134828.csv")
calls = calls %>%
  mutate(CREATEDDATE = mdy_hm(as.character(CREATEDDATE))) %>%
  mutate(Month = month(CREATEDDATE), 
         Day = day(CREATEDDATE),
         Hour = hour(CREATEDDATE),
         Min = minute(CREATEDDATE),
         Time = paste(Hour, Min))

# number of calls per month
calls %>%
  group_by(Month) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Month, y = count)) +
  geom_line(color = "brown3")+
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  ggtitle("Number of Calls per Month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# number of calls per hour
calls %>%
  group_by(Hour) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Hour, y = count)) +
  geom_line(color = "brown3")+
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  ggtitle("Number of Calls per Hour") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# number of calls per month, by REQUESTSOURCE
calls %>%
  group_by(Month, REQUESTSOURCE) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Month, y = count, color = REQUESTSOURCE)) +
  geom_line()+
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  ggtitle("Number of Calls per Month by Request Source") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# number of calls per hour, by REQUESTSOURCE
calls %>%
  group_by(Hour, REQUESTSOURCE) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Hour, y = count, color = REQUESTSOURCE)) +
  geom_line()+
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  ggtitle("Number of Calls per Hour by Request Source") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

# number of requests by REQUESTSOURCE
calls %>%
  group_by(REQUESTSOURCE) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(REQUESTSOURCE, count), y = count)) +
  geom_bar(stat = "identity", fill = "brown3") +
  coord_flip() +
  ggtitle("Number of Requests by Requests Source") +
  xlab("Request Source") +
  theme_bw()

# number of requests by POLICEPRECINCT
calls %>%
  group_by(POLICEPRECINCT) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(POLICEPRECINCT, count), y = count)) +
  geom_bar(stat = "identity", fill = "brown3") +
  coord_flip() +
  ggtitle("Number of Requests by Police Precinct") +
  xlab("Police Precinct") +
  theme_bw()
  

# requests on map, dot plot
la_map = get_map("Los Angeles County", zoom = 10)
ggmap(la_map) + 
  geom_point(data = calls, 
             aes(x = LONGITUDE, y = LATITUDE),
             color = "slateblue3",
             size = 0.2) +
  theme_bw()

# requests on map, dot plot, by police precinct
la_map = get_map("Los Angeles County", zoom = 10)
ggmap(la_map) + 
  geom_point(data = calls, 
             aes(x = LONGITUDE, y = LATITUDE, color = POLICEPRECINCT),
             size = 0.1)+
  ggtitle("311 Calls by Police Precinct") +
  theme_bw()

# requests on map, density
ggmap(la_map) + 
  stat_density2d(data = calls,
                 aes(x = LONGITUDE, y = LATITUDE,
                     fill = ..level..,
                     alpha = ..level..),
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high = "red")

# requests on map, density, by month
ggmap(la_map) + 
  stat_density2d(data = calls,
                 aes(x = LONGITUDE, y = LATITUDE,
                     fill = ..level..,
                     alpha = ..level..),
                 geom = "polygon") +
  scale_fill_gradient(low = "white", high = "red") +
  facet_wrap(~Month, nrow = 3)
