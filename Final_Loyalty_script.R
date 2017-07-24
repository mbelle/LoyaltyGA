install.packages("xlsx")
install.packages("scales")


library(ggplot2)
library(dplyr)
library(xlsx)
library(tidyr)
library(scales)

##This code will load station level monthly data for a quarter from a user level file and from segment/session files
##Create a monthly station summary of traffic sources and behaviour 
##Create a monthly station summary of loyal users v total users
## attempt to find a relationship between traffic segments/behaviours and the loyal user levels


setwd('/Users/mbellettiere/3s')

##load loyalty data
filename="3plusmonthly.xlsx"
loyaldf<-read.xlsx(file=filename, header=TRUE, stringsAsFactors=FALSE, sheetIndex = 1) 


#creates columns for loyal users and all users
loyaldf<-spread(loyaldf, Segment, Users)
#rename columns
names(loyaldf)<-c("Station","Month","Date","LoyalUsers","AllUsers")
summary(loyaldf)
head(loyaldf)

#drop column for date
userdf<-loyaldf[,(-3)]

#mutate to calculate % loyal
userdf<-userdf %>%
  mutate(loyalPct = LoyalUsers/AllUsers)

#need to remove invalid stations with limited data
userdf<-userdf %>%
  filter(AllUsers > 100)

##summary(userdf)


#load additional visit based segement data
filename="segmentMonthly.xlsx"
segdf<-read.xlsx(file=filename, header=TRUE, stringsAsFactors=FALSE, sheetIndex = 1) 

#creates columns for direct, social, reading, streaming all users
segdf<-spread(segdf, Segment, Sessions)

names(segdf)<-c("Station","Month","allUserSessions","directSessions","readerSessions","socialSessions","streamSessions")
#mutate to calculate % visits
segdf<-segdf %>%
  mutate(directPct = directSessions/allUserSessions, readerPct = readerSessions/allUserSessions, socialPct =socialSessions/allUserSessions, streamPct =streamSessions/allUserSessions)

hist(segdf$allUserSessions)

#need to remove invalid stations with limited data
segdf<-segdf %>%
  filter(allUserSessions > 100)

##merge user dataframe with session dataframe

alldf<-merge(segdf,userdf, by=c("Station", "Month"))

##clean data

##regressions of monthly data points
lm1<-lm(alldf$loyalPct~alldf$allUserSessions)
summary(lm1)

lm1<-lm(fdf$totalLoyalPct~fdf$totalDirectPct+fdf$totalSessions)
summary(lm1)

lm2<-lm(fdf$totalLoyalPct~fdf$totalSocialPct+fdf$totalSessions)
summary(lm2)

lm3<-lm(fdf$totalLoyalPct~fdf$totalSocialPct+fdf$totalSessions+fdf$totalReadPct)
summary(lm3)


##plots of monthly data points
ggplot(alldf, aes(loyalPct,directPct))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Percent Direct Traffic")+
  geom_smooth(method=lm,color="black")+
  scale_y_continuous(labels = scales::percent)+ 
  scale_x_continuous(labels = scales::percent)

ggplot(alldf, aes(log(loyalPct),log(socialPct)))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Percent Social Traffic")+
  geom_smooth(method=lm,color="black")+
  scale_y_continuous(labels = scales::percent)+ 
  scale_x_continuous(labels = scales::percent)

ggplot(alldf, aes(LoyalUsers,socialSessions))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Social Traffic")+
  geom_smooth(method=lm,color="black")
  #scale_x_continuous(labels = scales::percent)

#social and loyalty
ggplot(alldf, aes(log(LoyalUsers),log(socialSessions)))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Social Traffic")+
  geom_smooth(method=lm,color="black")

  

ggplot(alldf, aes((LoyalUsers),(socialSessions)))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Social Traffic")+
  geom_smooth(method=lm,color="black")


ggplot(alldf%>%filter(streamPct>0.01), aes(log(LoyalUsers),log(streamSessions)))+ geom_point(color="darkBlue") +
  labs(x="Log(Loyal Users)", y = "Log(Listening Sessions)")+
  geom_smooth(method=lm,color="black")

ggplot(alldf%>%filter(streamPct>0.02), aes((loyalPct),(streamPct)))+ geom_point(color="darkBlue") +
  labs(x="Loyal Users", y = "Listening Sessions")+
  #geom_smooth(method=lm,color="black")+
  scale_y_continuous(labels = scales::percent)+ 
  scale_x_continuous(labels = scales::percent)



####Quarterly data using stations with 3 months of data
#remove stations without 3 months of data and summarize
tSegdf<-segdf %>%
  group_by(Station) %>%
  summarise(totalSocial =sum(socialSessions),totalDirect= sum(directSessions),totalRead = sum(readerSessions), totalListen = sum(streamSessions), totalSessions = sum(allUserSessions),Months=n_distinct(Month))

tSegdf<-tSegdf %>%
  filter(Months==3)

#add total pct columns
totalSegdf<-totalSegdf %>%
  mutate(totalSocialPct =totalSocial/totalSessions, totalDirectPct =totalDirect/totalSessions, totalReadPct = totalRead/totalSessions, totalListenPct = totalListen/totalSessions)

#remove stations without 3 months of data and summarize
totalUserdf<-userdf %>%
  group_by(Station) %>%
  summarise(totalLoyal=sum(LoyalUsers),totalUsers=sum(AllUsers), Months=n_distinct(Month))

totalUserdf<-totalUserdf %>%
  filter(Months==3)

#calculate ave pctloyal for quarter

totalUserdf<-totalUserdf %>%
  mutate(totalLoyalPct = totalLoyal/totalUsers )

ggplot(totalUserdf, aes(totalLoyalPct))+ geom_histogram(color="gray", fill="lightblue", bins = 10) +
  labs(title="Stations by Percent Loyal Users",x="Percent Loyal Users", y = "Count of Stations")


#top Loyalty 
alldfSort<-alldf[order(-alldf$loyalPct),]
head(alldfSort,10)

totalUserdfTop<-totalUserdf[order(-totalUserdf$totalLoyalPct),]
head(totalUserdfTop,10)

#create a theme for charts
my_theme <- function(){
  theme_light() +
    theme(text = element_text(family = "sans"),  
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 9, color = "gray10"))
}
#bar chat of top loyal users % station 

dflist<-head(totalUserdfTop,10)

ggplot(dflist, aes(totalLoyalPct, Station))+
  geom_point()

ggplot(dflist, aes( x=reorder(Station,totalLoyalPct),y=totalLoyalPct)) +
  geom_bar(stat = "identity", 
           color ="lightblue", 
           fill="lightblue",width = 0.75) +
  geom_text(aes(label = percent(totalLoyalPct)),
            #family = "Sans",
            size = 3.5,                 
            hjust = 0) + 
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(dflist$totalLoyalPct) * 1.3)) +  # Make the x-axis 30% longer than the max
  my_theme()
  
