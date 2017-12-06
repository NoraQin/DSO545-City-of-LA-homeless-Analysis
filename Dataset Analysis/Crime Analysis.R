library(ggplot2)
library(dplyr)
library(ggthemes)
library(stringr)
library(lubridate)

crime2 = read.csv("crime_clean.csv")

### regarding to time (day/month/week of the day etc.)
# crime occurances
ggplot(crime2, aes(x = TIME.OCCURRED, y = ..density..)) +
  geom_histogram(color = "blanchedalmond", fill = "bisque2") +
  geom_line(stat = "density", adjust = 0.35,color = "firebrick2", size = 0.8) +
  #scale_y_continuous(name = "Density") +
  #scale_x_continuous(name = "Time Crime Occurred (miltary time)",
  scale_x_continuous(breaks = seq(0, 2400, 400),
                     labels = seq(0, 2400, 400)) +
  ggtitle("Crime happens most frequently around noon and after 8pm") +
  #theme_fivethirtyeight() +
  # theme_bw()
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text()) + 
  ylab("Density") +
  xlab("Time Crime Occurred (Miltary Time)")

## DASHBOARD: change time by hour 
## DASHBOARD: need to keep all crime types and keep the order and scale same cross graphs 
## (code below are for around noon)
crime2 %>%
  filter(TIME.OCCURRED >= 1130 & TIME.OCCURRED <= 1330) %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(crime.type.record,-count,sum), y = count, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("VICTIM.SEX", values = c("F" = "brown3", "M" = "grey50", "X" = "bisque2")) +
    xlab("Crime type") +
    ggtitle("Crime type distribution around noon") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
   
# top crime are assault related, most of victimes are male

## (code below are for after 8pm)
crime2 %>%
  filter(TIME.OCCURRED >= 2000 & crime.type.record != "NA") %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(crime.type.record,-count,sum), y = count, fill = VICTIM.SEX)) +
  geom_bar(stat = "identity") +
  scale_fill_manual("VICTIM.SEX", values = c("F" = "brown3", "M" = "grey50")) +
  xlab("Crime type") +
  ggtitle("Crime type distribution after 8pm") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## distribution by month
# has all months (double counted August)
crime2 %>%
  group_by(month) %>%
  #summarise(count = n()) %>%
  ggplot(aes(x = month)) +
  geom_bar() +
  theme_bw() +
  ggtitle("Crime occurance by month")

# filter out 2016 august
no201608 = read.csv("crime_no201608.csv")
no201608 %>%
  group_by(month, VICTIM.SEX) %>%
  #summarize(count = n()) %>%
  ggplot(aes(x = month, fill = VICTIM.SEX)) +
  geom_bar() +
  theme_bw() +
  ggtitle("Crime occurance by month (Sep 2016 - Aug 2017)")
# filter out 2017 august
no201708 = read.csv("crime_no201708.csv")
no201708 %>%
  group_by(month, VICTIM.SEX) %>%
  #summarize(count = n()) %>%
  ggplot(aes(x = month, fill = VICTIM.SEX)) +
  geom_bar() +
  theme_bw() +
  ggtitle("Crime occurance by month (Aug 2016 - Jul 2017)")
# --> peak a little during summer months, but not much different
# --> gender has not much differences

## --> top crime type in summer months

## distribution by day of week

# ERROR: crime2$day_of_week = levels(birthwt$smoke, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
## Need to rearrange levels
crime2 %>%
  group_by(day_of_week, VICTIM.SEX) %>%
  ggplot(aes(x = day_of_week, fill = VICTIM.SEX)) +
  geom_bar() +
  theme_bw() +
  ggtitle("Crime occurance by day of week") +
  scale_fill_manual("VICTIM.SEX", values = c("F" = "brown3", "M" = "grey50", "X" = "bisque2")) +
  xlab("Day of Week") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

# --> no much differences either

## compare different year
# crime type
crime2 %>%
  group_by(CRIME.CODE.DESCRIPTION, year) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x = reorder(CRIME.CODE.DESCRIPTION, -count), y = count, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual("as.factor(year)", values = c("2017" = "brown3", "2016" = "grey50")) +
    ggtitle("Year comparision by crime type") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
# not meaningful: crime occurance significantly increase --> due to 2017 has an additional month

crime2 %>%
  filter(VICTIM.SEX != "X") %>%
  group_by(month, year, VICTIM.SEX) %>%
  summarise(count = n(),
            average = mean(count)) %>%
ggplot(aes(x = as.factor(year), fill = VICTIM.SEX)) +
  geom_bar(position = "dodge")

#( point 
# not helpful
crime2 %>%
  filter(VICTIM.SEX != "X") %>%
  ggplot(aes(x = VICTIM.AGE, y = month, color = VICTIM.SEX)) +
  geom_point()

crime2 %>%
  filter(VICTIM.SEX != "X") %>%
  ggplot(aes(x = VICTIM.AGE, y = day, color = VICTIM.SEX)) +
  geom_point()
#)

### Gender breakdown
## top crime type by gender
crime2 %>%
  filter(crime.type.record != "NA") %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarize(count = n()) %>%
  #mutate(percent = count) %>%
  arrange(desc(count)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), 
             y = count,
             fill = VICTIM.SEX)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("VICTIM.SEX", values = c("F" = "brown3", "M" = "grey50", "X" = "bisque2")) +
    theme_bw() +
    ggtitle("Top 10 crime") +
    xlab("Crime type") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

## top crime type with female victims
crime2 %>%
  filter(VICTIM.SEX == "F" & crime.type.record != "NA") %>%
  group_by(crime.type.record) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(crime.type.record, count), y = count, fill = "bisque2")) +
    geom_bar(stat = "identity") +
    ggtitle("Top 10 crime with female victims") +
    scale_fill_manual("crime.type.record", values = "darkred") +
    xlab("Crime type") +
    ylab("Nunber of crime") +
    coord_flip() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(hjust = 1, size =10),
          axis.text.y = element_text(size =10),
          legend.position="none")

crime2 %>%
  filter(VICTIM.SEX == "M" & crime.type.record != "NA") %>%
  group_by(crime.type.record) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(crime.type.record, count), y = count, fill = "bisque2")) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 crime with male victims") +
  scale_fill_manual("crime.type.record", values = "darkred") +
  xlab("Crime type") +
  ylab("Nunber of crime") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1, size =10),
        axis.text.y = element_text(size =10),
        legend.position="none")

# x = reporting days, color = crime type
crime2 %>%
  filter(duration < 40 & duration >10) %>%
  group_by(duration, crime.type.record) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = duration, y = crime.type.record)) +
    geom_point()

######## !!!!! here is to use the new grouped delay days dataset ################
crime3 = read.csv("crime_clean2.csv")

crime3 %>%
  filter(VICTIM.SEX == "F" & crime.type.record != "NA") %>%
  group_by(crime.type.record, duration_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = duration_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Reporting delay for female victims (days)") +
  xlab("Crime type") +
  ylab("Number of Crimes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

crime3 %>%
  filter(VICTIM.SEX == "M" & crime.type.record != "NA") %>%
  group_by(crime.type.record, duration_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = duration_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Reporting delay for male victims (days)") +
  xlab("Crime type") +
  ylab("Number of Crimes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

########## ！！！！End of using updated dataset #################

# fill by dates report after crime happens
crime2 %>%
  filter(VICTIM.SEX == "F" & crime.type.record != "NA") %>%
  group_by(crime.type.record, duration_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = duration_group)) +
    geom_bar(stat = "identity") +
    ggtitle("Reporting delay for female victims (days)") +
    xlab("Crime type") +
    ylab("Number of Crimes") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

# fill by dates report after crime happens
crime2 %>%
  filter(VICTIM.SEX == "M" & crime.type.record != "NA") %>%
  group_by(crime.type.record, duration_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = duration_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Reporting delay for male victims (days)") +
  xlab("Crime type") +
  ylab("Number of Crimes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# filter out crimes reported within 10 days
crime2 %>%
  filter(VICTIM.SEX == "F" & crime.type.record != "NA" & duration > 10) %>%
  group_by(crime.type.record, duration_group) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, count, sum), y = count, fill = duration_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Crimes reported after 10 days (female victims)") +
  labs(x = "Crime type", 
       y = "Number of Crimes",
       Fill = "Reproting delay (days)") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# calculate percentages - top 10+ crimes
crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX != "X" &
           crime.type.record %in% 
           c("Sexual Assault/Lewd", "Rape", "Theft", "Threats", 
             "Robbery", "Battery", "Homicide", "Vandalism",
             "Intimate partner assault", "Assault")) %>% # selecting top 10 crime among both female and male
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(ave_duration = mean(duration),
            count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -ave_duration, sum), y = ave_duration, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Crime type",
         y = "Average reporting delay (days)",
         fill = "Gender") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Average reporting time of top 10 crime")

# take out the male
crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX == "F" &
           crime.type.record %in% 
           c("Sexual Assault/Lewd", "Rape", "Theft", "Threats", 
             "Robbery", "Battery", "Homicide", "Vandalism",
             "Intimate partner assault", "Assault",
             "Violation/Contempt of court order")) %>% # selecting top 10 crime among both female and male
  group_by(crime.type.record) %>%
  summarise(ave_duration = mean(duration),
            count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -ave_duration, sum), y = ave_duration)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Crime type",
       y = "Average reporting delay (days)",
       fill = "Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggtitle("Average reporting time for top 10 crime (Female)")

# calculate percentages - top 10+ crimes
crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX != "X") %>% #&
           #crime.type.record %in% 
           #c("Sexual Assault/Lewd", "Rape", "Theft", "Threats", 
            # "Robbery", "Battery", "Homicide", "Vandalism",
             #"Intimate partner assault", "Assault")) %>% # selecting top 10 crime among both female and male
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(ave_duration = mean(duration),
            count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, ave_duration, sum), y = ave_duration, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Crime type",
         y = "Average reporting delay (days)",
         fill = "Gender") +
    coord_flip() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10)) +
    ggtitle("Crimes with the longest average reporting time")

# calculate percentages - include total number of crime
crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX != "X" &
           crime.type.record %in% 
           c("Sexual Assault/Lewd", "Rape", "Theft", "Threats", 
             "Robbery", "Battery",
             "Intimate partner assault", "Assault")) %>% # selecting top 10 crime among both female and male
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(ave_duration = mean(duration),
            count = n()/10) %>%
  ggplot(aes(x = reorder(crime.type.record, -ave_duration, sum), y = ave_duration, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
    labs(x = "Crime type",
         y = "Average reporting delay (days)",
         fill = "Gender") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
  ## add a second layer graph from here
    geom_line(aes(y = count, group = VICTIM.SEX, color = VICTIM.SEX)) +
    scale_y_continuous(sec.axis = sec_axis(~.*2, name = "1/5 of total crime occurance")) +
    ggtitle("Which type of crime has the longerest reporting time?")

# more percentages
ave_duration_female = crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX == "F") %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(ave_duration = mean(duration)) %>%
  arrange(crime.type.record, ave_duration)

ave_duration_male = crime2 %>%
  filter(crime.type.record != "NA" & VICTIM.SEX == "M") %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(ave_duration = mean(duration)) %>%
  arrange(crime.type.record, ave_duration)

         
## top crime type with male victims
crime2 %>%
  filter(VICTIM.SEX == "M" & crime.type.record != "NA") %>%
  group_by(crime.type.record) %>%
  summarize(count = n()) %>%
  #mutate(percent = count) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(crime.type.record, -count), y = count, fill = "bisque2")) +
    geom_bar(stat = "identity") +
    ggtitle("Top 10 crime with male victims") +
    scale_fill_manual("crime.type.record", values = "darkred") +
    xlab("Crime type") +
    ylab("Number of crime") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="none") 

##crime victim distribution/histogram/count, highlight female
## What happens to female and male around the ages?

ggplot(crime2, aes(x = VICTIM.AGE, y = ..density.., fill = VICTIM.SEX, color = VICTIM.SEX)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  ggtitle("Crime victim distribution by gender") +
  xlab("Age") +
  ylab("Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# look into female: 18-40, male: 50+

crime2 %>%
  filter(VICTIM.AGE >= 18 & VICTIM.AGE <= 40 & VICTIM.SEX == "F") %>%
  group_by(crime.type.record) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x = reorder(crime.type.record, -count), y = count, fill = "firebrick")) +
  geom_bar(stat = "identity") +
  scale_fill_manual("crime.type.record", values = "firebrick") +
  ggtitle("Female victim age 18-40 by crime type (top 10)") +
  xlab("Crime type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

crime2 %>%
  filter(VICTIM.AGE >= 50 & VICTIM.SEX == "M") %>%
  group_by(crime.type.record) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x = reorder(crime.type.record, -count), y = count, fill = "firebrick")) +
    geom_bar(stat = "identity") +
    scale_fill_manual("crime.type.record", values = "firebrick") +
    ggtitle("Male victim age 50+ by crime type (top 10)") +
    xlab("Crime type") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="none") 

# females and males are victims of different crimes
# follow the same patten when segnmented by age (due to discripency)

### overlook of age and crime types
# --> still need to group crime types
ggplot(crime2, aes(x = VICTIM.AGE, y = CRIME.CODE.DESCRIPTION)) +
  geom_boxplot()

# crime type and mode age
crime2 %>%
  group_by(VICTIM.AGE, CRIME.CODE.DESCRIPTION) %>%
  summarise(mode = mode(VICTIM.AGE))
# why does not work?

### crime type breakdown by area
## central has highest number of crime
crime2 %>%
  filter(VICTIM.SEX != "X") %>%
  group_by(AREA.NAME, VICTIM.SEX) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(AREA.NAME, count), y = count, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    xlab("Area") +
    ylab("Number of Crime") +
    scale_fill_manual(values=c("firebrick2","grey50")) +
    scale_color_discrete(name  ="Gender") +
    ggtitle("Crime area and victim gender") +
    theme(plot.title = element_text(hjust = 0.5))

## crime type in central area breandown
crime2 %>%
  filter(AREA.NAME != "Central" & crime.type.record != "NA") %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, count), y = count, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    xlab("Crime type") +
    ylab("Number of Crime") +
    scale_fill_manual(values=c("firebrick2","grey50")) +
    scale_color_discrete(name  ="Gender") +
    ggtitle("Crime type breakdown in central area") +
    theme(plot.title = element_text(hjust = 0.5))

table(unique(crime2$CRIME.CODE.DESCRIPTION))

## line graph x = time, y = count, line color is by crime type
crime2 =  mutate(crime2, time.occurred.hour = TIME.OCCURRED%/%100)

## DASHBOARD - still
crime2 %>%
  group_by(time.occurred.hour) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = time.occurred.hour, y = count, color = "firebrick2")) +
    geom_line() +
    theme_bw() +
    xlab("Time") +
    ylab("Number of Crime") +
    ggtitle("Crime Occurance by Hour") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="none")
    
## DASHBOARD - all crime types included
crime2 %>%
  filter(crime.type.record != "NA") %>%
  ggplot(aes(x = TIME.OCCURRED, color = crime.type.record)) +
    geom_freqpoly() +
    xlab("Time") +
    ylab("Number of Crime") +
    ggtitle("Crime Occurance by Hour") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
    scale_x_continuous(breaks = seq(0,24,4),
                       labels = seq(0,24,4))

crime2 %>%
  filter(crime.type.record != "NA") %>%
  ggplot(aes(x = time.occurred.hour, color = crime.type.record)) +
  geom_freqpoly() +
  xlab("Time") +
  ylab("Number of Crime") +
  ggtitle("Crime Occurance by Hour") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## DASHBOARD - selected crime types: recommend not to select more then 3 crime types
crime2 %>%
  filter(crime.type.record == c("Assault", 
                                #"Battery", 
                                #"Intimate partner assault",
                                "Theft",
                                "Robbery",
                                #"Threats",
                                #"Rape",
                                #"Sexual Assault/Lewd"
                                "Vandalism"
                                )) %>%
  ggplot(aes(x = time.occurred.hour, color = crime.type.record)) +
    geom_freqpoly()

crime2 %>%
  filter(VICTIM.SEX != "X") %>%
  group_by(time.occurred.hour, VICTIM.SEX) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = time.occurred.hour, y = count, color = VICTIM.SEX)) +
    geom_line() +
    xlab("Time") +
    ylab("Number of Crime") +
    ggtitle("Crime occurance by gender") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))


## time between report and occurred, and how does it differ by type of crime, by gender?
# 1. compute duration
crime2$DATE.OCCURRED = mdy(crime2$DATE.OCCURRED)
crime2$DATE.REPORTED = mdy(crime2$DATE.REPORTED)
crime2 = mutate(crime2, duration = DATE.REPORTED-DATE.OCCURRED)

write.csv(crime2, "crime_clean.csv")

ave_duration = mean(crime2$duration)
ave_duration  # 1.69 days

duration_table = crime2%>%
  group_by(duration) %>%
  summarise(count = n())
duration_table

# list of crimes reporting after 10, 20, 30, and 60 days
## DASHBOARD content
morethan10days = filter(crime2, duration >=10)
morethan10days = morethan10days %>%
  arrange(desc(duration))

morethan10days %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = VICTIM.SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("firebrick2","grey50")) +
  ggtitle("Reporting after 10 days of crime occurance") +
  labs(x = "Count", y = "Crime type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

morethan20days = filter(crime2, duration >=20)
morethan20days = morethan20days %>%
  arrange(desc(duration))

morethan20days %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = VICTIM.SEX)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("firebrick2","grey50")) +
    ggtitle("Reporting after 20 days of crime occurance") +
    labs(x = "Count", y = "Crime type") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))

morethan30days = filter(crime2, duration >=30)
morethan30days = morethan30days %>%
  arrange(desc(duration))

morethan30days %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = VICTIM.SEX)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick2","grey50")) +
  ggtitle("Reporting after 30 days of crime occurance") +
  labs(x = "Count", y = "Crime type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

morethan60days = filter(crime2, duration >=60)
morethan60days = morethan60days %>%
  arrange(desc(duration))

morethan60days %>%
  group_by(crime.type.record, VICTIM.SEX) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(crime.type.record, -count, sum), y = count, fill = VICTIM.SEX)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick2","grey50")) +
  ggtitle("Reporting after 60 days of crime occurance") +
  labs(x = "Count", y = "Crime type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## percentage: compute among all the rape, sexual assault/lewd crime --> 
## what is the percentage of them reporting after 20 days and 30 days/ between 20-30 days

# average reporting time by crime type

# crime map
library(maps)
crime_map = get_map("Los Angeles County", zoom = 10)
ggmap(crime_map) +
  geom_point(data = crime2, 
             aes(x = LONGITUDE, y = LATITUDE, color = VICTIM.SEX)) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Crime occurance")

# crime location by gender by Intimate partner assault
library(maps)
crime_map = get_map("Los Angeles County", zoom = 10)
crime_subset = crime2 %>%
  filter(crime.type.record == "Intimate partner assault"
         & VICTIM.SEX != "X")
  # , "Battery", "Robbery"
ggmap(crime_map) +
  geom_point(data = crime_subset, 
             aes(x = LONGITUDE, 
                 y = LATITUDE, 
                 color = VICTIM.SEX)) +
  facet_wrap(~VICTIM.SEX) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Intimate partner assault occurance")

# crime location by gender by Assault
library(maps)
crime_map = get_map("Los Angeles County", zoom = 10)
crime_subset = crime2 %>%
  filter(crime.type.record == "Assault"
         & VICTIM.SEX != "X")
ggmap(crime_map) +
  geom_point(data = crime_subset, 
             aes(x = LONGITUDE, 
                 y = LATITUDE, 
                 color = VICTIM.SEX)) +
  facet_wrap(~VICTIM.SEX) +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Assault occurance")

## compute some percentages
# percentage assault crimes, among these how many percent are aggravated
# percentage sexual assault, among these how many percent are intimit partner assault  
# 2016 and 2017 percentage youth victim comparsion
# 2016 and 2017 each crime type percentage comparision



