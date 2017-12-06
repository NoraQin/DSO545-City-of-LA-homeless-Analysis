library(ggplot2)
library(dplyr)
library(ggmap)
library(gplots)
library(ggthemes)
library(scales)
library(reshape2)
library(RColorBrewer)



homeless = read.csv("homeless2017community.csv")

homelessTopTotPeople = hc17 %>% 
  arrange(-totPeople) %>% 
  mutate(Perc = totUnsheltPeople/totPeople)

top= head(homelessTopTotPeople,10)

ggplot(top, aes(x = reorder(Community, -totPeople), y = totPeople))+
  geom_bar(stat = "identity", fill = "blanchedalmond", width = 0.4) +
  geom_bar(aes(x = reorder(Community, -totPeople), y = totUnsheltPeople), 
           stat = "identity", fill = "brown3", width = 0.4) +
  xlab("Community")+
  ylab("Number of Homeless People") +
  ggtitle("Top 10 Communities with Highest Homeless Count, 2017")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))


#### Shelter spread for youth 18 to 24 in 2017

homelessTopTotPeople = hc17 %>% 
  arrange(-totPeople) %>% 
  mutate(Perc = totUnsheltPeople/totPeople)

top= head(homelessTopTotPeople,10)
homelessyouth = top %>% 
  select(Community, totESYouthSingYouth, totTHYouthSingYouth, totSHYouthSingYouth, totPeople) 

mhomelessyouth = melt(homelessyouth, id.vars = "Community")
  
  ggplot(mhomelessyouth, aes(x = reorder(Community,-value), y = value, fill=variable)) +
    geom_bar(stat='identity', width = 0.4)+
    scale_fill_manual("Total Youth Count in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                      labels=c("Emergency Shelter", "Transitional Housing", "Safe Havens","Unsheltered Homeless Persons"))+
    xlab("Community")+
    ylab("Number of Homeless People") +
    ggtitle("Top 10 Homeless Communities with Single Adult Youth 18 to 24, Shelter Distribution in 2017")+
      theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15))

#### Shelter spread for minors in 2017
  
  homelessminor = top %>% 
    select(Community, totESYouthUnaccYouth, totTHYouthUnaccYouth, totSHYouthUnaccYouth, totPeople) 
  
  mhomelessminor = melt(homelessminor, id.vars = "Community")
  
  ggplot(mhomelessminor, aes(x = reorder(Community,-value), y = value, fill=variable)) +
    geom_bar(stat='identity', width = 0.4)+
    scale_fill_manual("Total Unaccompanied Minors in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                      labels=c("Emergency Shelter", 
                               "Transitional Housing", 
                               "Safe Havens",
                               "Unsheltered Homeless Persons"))+
    xlab("Community")+
    ylab("Number of Homeless People") +
    ggtitle("Top 10 Homeless Communities with Unaccompanied Minors, Shelter Distribution in 2017")+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15))
  
  
## Shelter Spread for family HH head, 18-24 , in 2017
  
  homelessyouthFH = top %>% 
    select(Community, totESYouthFamHH, totTHYouthFamHH, totSHYouthFamHH, totPeople) 
  
  mhomelessyouthFH = melt(homelessyouthFH, id.vars = "Community")
  
  ggplot(mhomelessyouthFH, aes(x = reorder(Community,-value), y = value, fill=variable)) +
    geom_bar(stat='identity', width = 0.4)+
    scale_fill_manual("Total Families with Head of Household 18 to 24, Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                      labels=c("Emergency Shelter", 
                               "Transitional Housing", 
                               "Safe Havens",
                               "Unsheltered Homeless Persons"))+
    xlab("Community")+
    ylab("Number of Homeless People") +
    ggtitle("Top 10 Homeless Communities with Family Head of Household 18 to 24, Shelter Distribution in 2017")+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15))

  
##########2016 Analysis 
  
  
## Homeless Youth 
  
homeless16 = read.csv("homeless2016tract.csv")
  

homelessTop = homeless16 %>% 
  arrange(-totPeople) %>% 
  mutate(Perc = totUnsheltPeople/totPeople)

top10 = head(homelessTop,10)

ggplot(top10, aes(x = reorder(CommunityName, -totPeople), y = totPeople))+
  geom_bar(stat = "identity", fill = "blanchedalmond", width = 0.4) +
  geom_bar(aes(x = reorder(CommunityName, -totPeople), y = totUnsheltPeople), 
           stat = "identity", fill = "brown3", width = 0.4) +
  xlab("Community")+
  ylab("Number of Homeless People") +
  ggtitle("Top 10 Communities with Highest Homeless Count, 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))
  
  
  

#### Shelter spread for youth 18 to 24 in 2016
homelessyouth16 = top10 %>% 
  select(CommunityName, totEsYouthSingYouth, totThYouthSingYouth, totShYouthSingYouth, totPeople) 

mhomelessyouth16 = melt(homelessyouth16, id.vars = "CommunityName")

ggplot(mhomelessyouth16, aes(x = reorder(CommunityName,-value), y = value, fill=variable)) +
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_manual("Total Youth Count in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                    labels=c("Emergency Shelter", "Transitional Housing", "Safe Havens","Unsheltered Homeless Persons"))+
  xlab("Community")+
  ylab("Number of Homeless People") +
  ggtitle("Top 10 Homeless Communities with Single Adult Youth 18 to 24, Shelter Distribution in 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))


####  

#### Shelter spread for minors in 2016
homelessTop = homeless16 %>% 
  arrange(-totPeople) %>% 
  mutate(Perc = totUnsheltPeople/totPeople)

top10 = head(homelessTop,10)

homelessminor16 = top10 %>% 
  select(CommunityName, totEsYouthUnaccYouth, totThYouthUnaccYouth, totShYouthUnaccYouth, totPeople) 

mhomelessminor16 = melt(homelessminor16, id.vars = "CommunityName")

ggplot(mhomelessminor16, aes(x = reorder(CommunityName,-value), y = value, fill=variable)) +
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_manual("Total Unaccompanied Minors in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                    labels=c("Emergency Shelter", 
                             "Transitional Housing", 
                             "Safe Havens",
                             "Unsheltered Homeless Persons"))+
  xlab("Community")+
  ylab("Number of Homeless People") +
  ggtitle("Top 10 Homeless Communities with Unaccompanied Minors, Shelter Distribution in 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))



## Shelter Spread for family HH head, 18-24 , in 2016

homelessyouthFH = top10 %>% 
  select(CommunityName, totESYouthFamHH, totTHYouthFamHH, totSHYouthFamHH, totPeople) 

mhomelessyouthFH = melt(homelessyouthFH, id.vars = "CommunityName")

ggplot(mhomelessyouthFH, aes(x = reorder(CommunityName,-value), y = value, fill=variable)) +
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_manual("Total Families with Head of Household 18 to 24, Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                    labels=c("Emergency Shelter", 
                             "Transitional Housing", 
                             "Safe Havens",
                             "Unsheltered Homeless Persons"))+
  xlab("Community")+
  ylab("Number of Homeless People") +
  ggtitle("Top 10 Homeless Communities with Family Head of Household 18 to 24, Shelter Distribution in 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))

  
    

#### Shelter spread for youth 18 to 24 in 2016 out of total youth
  
homelessyouth116 = top10 %>% 
  select(CommunityName, totEsYouthSingYouth, totThYouthSingYouth, totShYouthSingYouth, totYouthIndiv) 


mhomelessyouth116 = melt(homelessyouth116, id.vars = "CommunityName")

ggplot(mhomelessyouth116, aes(x = reorder(CommunityName,-value), y = value, fill=variable)) +
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_manual("Total Youth Count in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                    labels=c("Emergency Shelter", "Transitional Housing", "Safe Havens","Unsheltered Single Youth Count 18-24"))+
  xlab("Community")+
  ylab("Number of Single Adult Youth") +
  ggtitle("Top 10 Homeless Communities with Single Adult Youth 18 to 24, Shelter Distribution in 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))
  
###Minor count out of only minors in top 10 

homelessminor116 = top10 %>% 
  select(CommunityName, totEsYouthUnaccYouth, totThYouthUnaccYouth, totShYouthUnaccYouth, totYouthUnaccKid) 

mhomelessminor116 = melt(homelessminor116, id.vars = "CommunityName")

ggplot(mhomelessminor116, aes(x = reorder(CommunityName,-value), y = value, fill=variable)) +
  geom_bar(stat='identity', width = 0.4)+
  scale_fill_manual("Total Unaccompanied Minors in Shelter Type", values = c("rosybrown","lavenderblush3","mistyrose1","papayawhip"),
                    labels=c("Emergency Shelter", 
                             "Transitional Housing", 
                             "Safe Havens",
                             "Unsheltered Unaccompanied Minors"))+
  xlab("Community")+
  ylab("Number of Unaccompanied Minors") +
  ggtitle("Top 10 Homeless Communities with Unaccompanied Minors, Shelter Distribution in 2016")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size =15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size=15))
  
  
