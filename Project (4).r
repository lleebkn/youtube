rm(list = ls())

setwd("~/OneDrive/Fall 2019/Economics_Data_Science/Project/Data")

library(tidyverse)
library(gmodels)
library(foreign)
library(AER)
library(nycflights13)
library(GGally)
library(car)
library(readxl) 
library(tidyquant)
library(zoo)
library(ggthemes)
library(gridExtra)
library(rjson)
library(psych)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gmodels)
USvideos <- read.csv("USvideos.csv")
CAvideos <- read.csv("CAvideos.csv")
KRvideos <- read.csv("KRvideos.csv")


#replace category_id with category name
USvideos$category_id <- ifelse(USvideos$category_id==1, "Film & Animation", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==2, "Autos & Vehicles", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==10, "Music", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==15, "Pets & Animals", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==17, "Sports", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==18, "Short Movies", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==19, "Travel & Events", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==20, "Gaming", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==21, "Videoblogging", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==22, "People & Blogs", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==23, "Comedy", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==24, "Entertainment", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==25, "News & Politics", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==26, "Howto & Style", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==27, "Education", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==28, "Science & Technology", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==29, "Nonprofits & Activism", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==30, "Movies", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==31, "Anime/Animation", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==32, "Action/Adventure", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==33, "Classics", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==34, "Comedy", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==35, "Documentary", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==36, "Drama", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==37, "Family", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==38, "Foreign", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==39, "Horror", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==40, "Sci-Fi/Fantasy", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==41, "Thriller", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==42, "Shorts", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==43, "Shows", USvideos$category_id)
USvideos$category_id <- ifelse(USvideos$category_id==44, "Trailers", USvideos$category_id)


#rename "category_id" column 
colnames(USvideos)[colnames((USvideos)) =="category_id"] <- "category"


# Sort published time into morning, afternoon, night, and midnight
USvideos$publish_Date <- as.Date(USvideos$publish_time)
USvideos$publish_time <- ymd_hms(USvideos$publish_time)
USvideos$time_of_day <- format(USvideos$publish_time, "%H%M%S") # remove date and only leave time of the day.
as.numeric(USvideos$time_of_day)

USvideos$time_of_day <- ifelse(USvideos$time_of_day>=060000 & USvideos$time_of_day<120000, "Morning", USvideos$time_of_day)
USvideos$time_of_day <- ifelse(USvideos$time_of_day>=120000 & USvideos$time_of_day<180000, "Afternoon", USvideos$time_of_day)
USvideos$time_of_day <- ifelse(USvideos$time_of_day>=180000 & USvideos$time_of_day<240000, "Night", USvideos$time_of_day)
USvideos$time_of_day <- ifelse(USvideos$time_of_day>=000000 & USvideos$time_of_day<060000, "Midnight", USvideos$time_of_day)

#check how many videos have disabled comment area/disabled ratings/been removed
nrow(subset(USvideos,comments_disabled == "TRUE" ))
nrow(subset(USvideos,ratings_disabled == "TRUE"))
nrow(subset(USvideos,video_error_or_removed == "TRUE"))
# all returns zero


#regression of views on likes, dislikes, and the number of comments
lm2 <- lm(views ~ likes + dislikes +comment_count,data = USvideos)
summary(lm2)


# Test Correlations between explanatory variables
cor1 <- cor(USvideos$likes,USvideos$comment_count)
cor1
cor2 <- cor(USvideos$dislikes,USvideos$comment_count)
cor2


#regression of views on the number of comments
lm11 <- lm(views ~ comment_count,data = USvideos)
summary(lm11)



# Regression on views ~ likes + dislikes + comment_count + category + time_of_day
lm3 <- lm(views ~ likes + dislikes +comment_count + category + time_of_day, data = USvideos)
summary(lm3)


lm14 <- lm(views ~ likes + dislikes +comment_count, data = USvideos)
summary(lm14)


# create a subset including videos published during the first three months
USvideos$publish_year <- as.numeric(format(USvideos$publish_Date,"%Y"))
USvideos$pubish_month <- month(USvideos$publish_Date)
firstThreeMonths2018 <- subset(USvideos,publish_year == 2018 & USvideos$pubish_month <=3)
lm10 <- lm(views ~ likes + dislikes +comment_count, data = firstThreeMonths2018)
summary(lm10)


lm12 <- lm(views ~ comment_count, data = firstThreeMonths2018)
summary(lm12)


# top 5 most-viewed channels
mostViewedChannels <- USvideos %>%
  group_by(channel_title) %>%
  summarise(views = sum(as.numeric(views)),
            category = first(category)) %>%
  arrange(desc(views))
FiveMostViewedChannels <- head(mostViewedChannels,5)


# Visualization top 5 most-viewed channels
FiveMostViewedChannels$views <-  FiveMostViewedChannels$views/1000
Bar_FiveMostViewedChannels <- ggplot(data = FiveMostViewedChannels,
                                     aes(x = reorder(channel_title, -views),y = views, fill = category)) +
  geom_bar(stat = "identity",width = 0.6)+
  theme_clean() +
  ylab("Views in Thousands") +
  xlab("Channel Title") +
  labs(title = "Top 5 most-viewed YouTube Channels") +
  scale_fill_manual(values=c("firebrick3", "brown4", "chocolate3"))
Bar_FiveMostViewedChannels

# top 10 most-viewed categories
mostViewedCategories <- USvideos %>%
  group_by(category) %>%
  summarise(views = sum(as.numeric(views))) %>%
  arrange(desc(views))
TenMostViewedCategories <- head(mostViewedCategories,10)
TenMostViewedCategories


# top 5 most-viewed channels
FiveMostViewedCategories<- head(mostViewedCategories,5)
FiveMostViewedCategories


# Visualization top 5 most-viewed categories
FiveMostViewedCategories$views <- FiveMostViewedCategories$views/1000
Bar_FiveMostViewedCategories <- ggplot(data = FiveMostViewedCategories, aes(x = reorder(category, -views),y = views)) +
  geom_bar(stat = "identity",width = 0.6,
           fill="firebrick3")+
  theme_clean() +
  ylab("Views in Thousands") +
  xlab("Category") +
  labs(title = "Top 5 most-viewed YouTube Categories") 
Bar_FiveMostViewedCategories

# Visualization top 10 most-viewed categories
TenMostViewedCategories$views <- TenMostViewedCategories$views/1000
Bar_TenMostViewedCategories <- ggplot(TenMostViewedCategories, aes(x = reorder(category, -views),y = views)) +
  geom_bar(stat = "identity",width = 0.6,
           fill="firebrick3")+
  theme_clean() +
  ylab("Views in Thousands") +
  xlab("Category") +
  labs(title = "Top 10 most-viewed YouTube Categories") 
  
Bar_TenMostViewedCategories

#Regression of views of top 1 category on likes + dislikes + time of the day
music <-subset(USvideos, category == "Music")
lm4 <- lm(views ~likes + dislikes + time_of_day, data = music)
summary(lm4)

#Regression of views of the second most-viewed category on likes + dislikes + time of the day
Entertainment <-subset(USvideos, category == "Entertainment")
lm5 <- lm(views ~likes + dislikes + time_of_day, data = Entertainment)
summary(lm5)


#Regression of views of the third most-viewed category on likes + dislikes + time of the day
FilmAnimation <-subset(USvideos, category == "Film & Animation")
lm6 <- lm(views ~likes + dislikes + time_of_day, data = FilmAnimation)
summary(lm6)



#Regression of views of the fourth most-viewed category on likes + dislikes + time of the day
Comedy <-subset(USvideos, category == "Comedy")
lm10 <- lm(views ~likes + dislikes + time_of_day, data = Comedy)
summary(lm10)


#Regression of views of the fifth most-viewed category on likes + dislikes + time of the day
PeopleBlogs <-subset(USvideos, category == "People & Blogs")
lm13 <- lm(views ~likes + dislikes + time_of_day, data = PeopleBlogs)
summary(lm13)



# Extract data based on seasons (Summer & Winter)

## Summer data analysis
summerdata <- subset(USvideos,pubish_month >= 5 | pubish_month <= 9)
mean(summerdata$views)
numVideosSummer <- nrow(summerdata)
numVideosSummer

## Winter data analysis
winterdata <- subset(USvideos,pubish_month >= 11 | pubish_month <= 3)
mean(winterdata$views)
numVideosWinter <- nrow(winterdata)
numVideosWinter
ViewsgapPercent <- (mean(summerdata$views) - mean(winterdata$views))/mean(summerdata$views)
ViewsgapPercent
NumVidGapPercent <- (numVideosSummer - numVideosWinter)/numVideosSummer
NumVidGapPercent

# Comparing across countries

## Canada
#replace category_id with category name
CAvideos1 <- subset(CAvideos, category_id == 10)
CAvideos1$category_id <- ifelse(CAvideos1$category_id==10, "Music", CAvideos1$category_id)

#rename "category_id" column 
colnames(CAvideos1)[colnames((CAvideos1)) =="category_id"] <- "category"

CAvideos1$publish_Date <- as.Date(CAvideos1$publish_time)
CAvideos1$publish_time <- ymd_hms(CAvideos1$publish_time)
CAvideos1$time_of_day <- format(CAvideos1$publish_time, "%H%M%S") # remove date and only leave time of the day.
as.numeric(CAvideos1$time_of_day)

CAvideos1$time_of_day <- ifelse(CAvideos1$time_of_day>=060000 & CAvideos1$time_of_day<120000, "Morning", CAvideos1$time_of_day)
CAvideos1$time_of_day <- ifelse(CAvideos1$time_of_day>=120000 & CAvideos1$time_of_day<180000, "Afternoon", CAvideos1$time_of_day)
CAvideos1$time_of_day <- ifelse(CAvideos1$time_of_day>=180000 & CAvideos1$time_of_day<240000, "Night", CAvideos1$time_of_day)
CAvideos1$time_of_day <- ifelse(CAvideos1$time_of_day>=000000 & CAvideos1$time_of_day<060000, "Midnight", CAvideos1$time_of_day)

lmCA1 <- lm(views ~ likes + dislikes + time_of_day, data = CAvideos1)
summary(lmCA1)
plot(lmCA1)
residualPlots(lmCA1)
qqPlot(lmCA1)

gg <- ggplot(data=CAvideos1, mapping=aes(x=likes, y=views)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Impact of likes on views for Music category in Canada", caption="Source: CAvideos dataset", y="Views", x="Likes") +
  scale_colour_brewer(palette="BrBG") + theme_bw()

gg + theme(plot.title=element_text(size=18,face="bold", color="firebrick",hjust=0.5,lineheight=1.5), # modifying title's theme component
           plot.caption=element_text(size=12),
           panel.background = element_rect(fill="white"),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Korea
#replace category_id with category name
KRvideos1 <- subset(KRvideos, category_id == 10)
KRvideos1$category_id <- ifelse(KRvideos1$category_id==10, "Music", KRvideos1$category_id)

#rename "category_id" column 
colnames(KRvideos1)[colnames((KRvideos1)) =="category_id"] <- "category"

KRvideos1$publish_Date <- as.Date(KRvideos1$publish_time)
KRvideos1$publish_time <- ymd_hms(KRvideos1$publish_time)
KRvideos1$time_of_day <- format(KRvideos1$publish_time, "%H%M%S") # remove date and only leave time of the day.
as.numeric(KRvideos1$time_of_day)

KRvideos1$time_of_day <- ifelse(KRvideos1$time_of_day>=060000 & KRvideos1$time_of_day<120000, "Morning", KRvideos1$time_of_day)
KRvideos1$time_of_day <- ifelse(KRvideos1$time_of_day>=120000 & KRvideos1$time_of_day<180000, "Afternoon", KRvideos1$time_of_day)
KRvideos1$time_of_day <- ifelse(KRvideos1$time_of_day>=180000 & KRvideos1$time_of_day<240000, "Night", KRvideos1$time_of_day)
KRvideos1$time_of_day <- ifelse(KRvideos1$time_of_day>=000000 & KRvideos1$time_of_day<060000, "Midnight", KRvideos1$time_of_day)

lmKR1 <- lm(views ~ likes + dislikes + time_of_day, data = KRvideos1)
summary(lmKR1)
plot(lmKR1)
residualPlots(lmKR1)
qqPlot(lmKR1)
gg2 <- ggplot(data=KRvideos1, mapping=aes(x=likes, y=views)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Impact of likes on views for Music category in Korea", caption="Source: KRvideos dataset", y="Views", x="Likes") +
  scale_colour_brewer(palette="BrBG") + theme_bw()

gg2 + theme(plot.title=element_text(size=18,face="bold", color="firebrick",hjust=0.5,lineheight=1.5), # modifying title's theme component
            plot.caption=element_text(size=12),
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## US
data2 <- subset(USvideos, category == "Music") # subsetting Music category in the US
lmUS <- lm(views ~likes + dislikes + time_of_day, data = data2)
summary(lmUS)
plot(lmUS)
residualPlots(lmUS)
qqPlot(lmUS)
gg3 <- ggplot(data=data2, mapping=aes(x=likes, y=views)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title="Impact of likes on views for Music category in the US", caption="Source: USvideos dataset", y="Views", x="Likes") +
  scale_colour_brewer(palette="BrBG") + theme_bw()

gg3 + theme(plot.title=element_text(size=18,face="bold", color="firebrick",hjust=0.5,lineheight=1.5), # modifying title's theme component
            plot.caption=element_text(size=12),
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
