#install the necessary library 
install.packages("hms")
install.packages("gridExtra")
install.packages("lubridate")

#read the data 
data_2019_dec <- read.csv("/Users/arielliwenxi/Downloads/426888_1015349_bundle_archive/2019-Dec.csv")
data_2019_nov <- read.csv("/Users/arielliwenxi/Downloads/426888_1015349_bundle_archive/2019-Nov.csv")
data_2019_oct <- read.csv("/Users/arielliwenxi/Downloads/426888_1015349_bundle_archive/2019-Oct.csv")
data_2020_feb <- read.csv("/Users/arielliwenxi/Downloads/426888_1015349_bundle_archive/2020-Feb.csv")
data_2020_jan <- read.csv("/Users/arielliwenxi/Downloads/426888_1015349_bundle_archive/2020-Jan.csv")

#look into the structure of the data 
str(data_2019_oct)
cat("\n")
str(data_2019_nov)
cat("\n")
str(data_2019_dec)
cat("\n")
str(data_2020_jan)
cat("\n")
str(data_2020_feb)

#remove the unwanted columns and fixing outliers of negative prices 
sample_2019_oct <- data_2019_oct[c(-3,-4,-5)]
s1 <- sum(sample_2019_oct$price<0)
sample_2019_oct <- sample_2019_oct[(sample_2019_oct$price>0),]
t1 <- nrow(sample_2019_oct)

sample_2019_nov <- data_2019_nov[c(-3,-4,-5)]
s2 <- sum(sample_2019_nov$price<0)
sample_2019_nov <- sample_2019_nov[(sample_2019_nov$price>0),]
t2 <- nrow(sample_2019_nov)

sample_2019_dec <- data_2019_dec[c(-3,-4,-5)]
s3 <- sum(sample_2019_dec$price<0)
sample_2019_dec <- sample_2019_dec[(sample_2019_dec$price>0),]
t3 <- nrow(sample_2019_dec)

sample_2020_jan <- data_2020_jan[c(-3,-4,-5)]
s4 <- sum(sample_2020_jan$price<0)
sample_2020_jan <- sample_2020_jan[(sample_2020_jan$price>0),]
t4 <- nrow(sample_2020_jan)

sample_2020_feb <- data_2020_feb[c(-3,-4,-5)]
s5 <- sum(sample_2020_feb$price<0)
sample_2020_feb <- sample_2020_feb[(sample_2020_feb$price>0),]
t5 <- nrow(sample_2020_feb)

#the percentage of outliers in the combined dataset that was removed 
total_per_outliers_removed <- (sum(s1,s2,s3,s4,s5)/sum(t1,t2,t3,t4,t5)) * 100
total_per_outliers_removed

#remove unwanted attributed from workspace
rm(total_per_outliers_removed, t5,t4,t3,t2,t1,s1,s2,s3,s4,s5, data_2019_oct, data_2019_nov, data_2019_dec, data_2020_jan, data_2020_feb)

#missing values detection if any

sapply(sample_2019_oct, function(x) sum(is.na(x)))
sapply(sample_2019_nov, function(x) sum(is.na(x)))
sapply(sample_2019_dec, function(x) sum(is.na(x)))
sapply(sample_2020_jan, function(x) sum(is.na(x)))
sapply(sample_2020_feb, function(x) sum(is.na(x)))

#date time split 

library(hms)

parsed1 <- strptime(sample_2019_oct$event_time, "%Y-%m-%d %H:%M:%S")
event_time<- format(parsed1,"%H:%M:%S")
event_date<- format(parsed1,"%Y-%m-%d")
sample_2019_oct$event_time<- as_hms(event_time)
sample_2019_oct$event_date<- as.Date(event_date)

parsed2 <- strptime(sample_2019_nov$event_time, "%Y-%m-%d %H:%M:%S")
event_time<- format(parsed2,"%H:%M:%S")
event_date<- format(parsed2,"%Y-%m-%d")
sample_2019_nov$event_time<- as_hms(event_time)
sample_2019_nov$event_date_ <- as.Date(event_date)

parsed3 <- strptime(sample_2019_dec$event_time, "%Y-%m-%d %H:%M:%S")
event_time<- format(parsed3,"%H:%M:%S")
event_date<- format(parsed3,"%Y-%m-%d")
sample_2019_dec$event_time <- as_hms(event_time)
sample_2019_dec$event_date <- as.Date(event_date)

parsed4 <- strptime(sample_2020_jan$event_time, "%Y-%m-%d %H:%M:%S")
event_time<- format(parsed4,"%H:%M:%S")
event_date<- format(parsed4,"%Y-%m-%d")
sample_2020_jan$event_time <- as_hms(event_time)
sample_2020_jan$event_date <- as.Date(event_date)

parsed5 <- strptime(sample_2020_feb$event_time, "%Y-%m-%d %H:%M:%S")
event_time<- format(parsed5,"%H:%M:%S")
event_date<- format(parsed5,"%Y-%m-%d")
sample_2020_feb$event_time <- as_hms(event_time)
sample_2020_feb$event_date <- as.Date(event_date)

#check to see if it worked 
str(sample_2019_oct)
cat("\n")
str(sample_2019_nov)
cat("\n")
str(sample_2019_dec)
cat("\n")
str(sample_2020_jan)
cat("\n")
str(sample_2020_feb)

#Distribution of event history across each month. 

library(ggplot2)
library(gridExtra)

options(repr.plot.width = 12, repr.plot.height = 10)
plot1 <- ggplot(data = sample_2019_oct) + geom_bar(mapping = aes(x = event_type, fill = event_type), show.legend = FALSE) + theme_linedraw() + ggtitle("Distribution of event type - Oct 2019") + xlab("Event type") + ylab("Count")
plot2 <- ggplot(data = sample_2019_nov) + geom_bar(mapping = aes(x = event_type, fill = event_type), show.legend = FALSE) + theme_linedraw() + ggtitle("Distribution of event type - Nov 2019") + xlab("Event type") + ylab("Count")
plot3 <- ggplot(data = sample_2019_dec) + geom_bar(mapping = aes(x = event_type, fill = event_type), show.legend = FALSE) + theme_linedraw() + ggtitle("Distribution of event type - Dec 2019") + xlab("Event type") + ylab("Count")
plot4 <- ggplot(data = sample_2020_jan) + geom_bar(mapping = aes(x = event_type, fill = event_type), show.legend = FALSE) + theme_linedraw() + ggtitle("Distribution of event type - Jan 2020") + xlab("Event type") + ylab("Count")
plot5 <- ggplot(data = sample_2020_feb) + geom_bar(mapping = aes(x = event_type, fill = event_type)) + theme_linedraw() + ggtitle("Distribution of event type - Feb 2020") + xlab("Event type") + ylab("Count")
grid.arrange(plot1,plot2,plot3,plot4,plot5)

#remove unwanted attributes from the workspace
rm(plot1,plot2,plot3,plot4,plot5)

#trend line for event history 

tab1 <- as.data.frame(table(sample_2019_oct$event_type))
names(tab1) <- c("Event_type", "Freq")
tab1$Month <- "2019-10"
tab2 <- as.data.frame(table(sample_2019_nov$event_type))
names(tab2) <- c("Event_type", "Freq")
tab2$Month <- "2019-11"
tab3 <- as.data.frame(table(sample_2019_dec$event_type))
names(tab3) <- c("Event_type", "Freq")
tab3$Month <- "2019-12"
tab4 <- as.data.frame(table(sample_2020_jan$event_type))
names(tab4) <- c("Event_type", "Freq")
tab4$Month <- "2020-01"
tab5 <- as.data.frame(table(sample_2020_feb$event_type))
names(tab5) <- c("Event_type", "Freq")
tab5$Month <- "2020-02"
trend <- rbind(tab1, tab2, tab3, tab4, tab5)

options(repr.plot.width = 8, repr.plot.height = 5)
ggplot(data = trend, mapping = aes(x = Month, y = Freq/100000)) + geom_line(mapping = aes(color = Event_type, group = Event_type), lwd = 1.5) + geom_point(mapping = aes(color = Event_type, size = 0.5), show.legend = FALSE) + theme_linedraw() + ggtitle("Event history trend over months") + xlab("Time period") + ylab("Total Count (in 100 Thousands)")

#remove the unwanted attributes from the workspace 

rm(tab1,tab2,tab3,tab4,tab5)

#Daily sales 

#assuming selling price is same as price listed 

library(lubridate)
library(tidyverse)
library(dplyr)

s1 <- sample_2019_oct %>% filter(event_type == "purchase") %>% select(price, event_date)
s1 <- aggregate(price ~ event_date, s1, sum)
s2 <- sample_2019_nov %>% filter(event_type == "purchase") %>% select(price, event_date_)
s2 <- aggregate(price ~ event_date_, s2, sum)
names(s2) <- c("event_date", "price")
s3 <- sample_2019_dec %>% filter(event_type == "purchase") %>% select(price, event_date)
s3 <- aggregate(price ~ event_date, s3, sum)
s4 <- sample_2020_jan %>% filter(event_type == "purchase") %>% select(price, event_date)
s4 <- aggregate(price ~ event_date, s4, sum)
s5 <- sample_2020_feb %>% filter(event_type == "purchase") %>% select(price, event_date)
s5 <- aggregate(price ~ event_date, s5, sum)

sales <- rbind(s1,s2,s3,s4,s5)
sales$period <- month(sales$event_date)

sales[(sales$price == (max(sales$price))),]
sales[(sales$price == (min(sales$price))),]

options(repr.plot.width = 8, repr.plot.height = 8)
ggplot(data = sales, mapping = aes(x = event_date, y = price)) + geom_line(lwd = 0.7) + geom_smooth( se = TRUE, alpha = 0.5) + scale_y_continuous(breaks = c(6900,25000,50000,75000,102000)) + geom_hline(mapping = aes(yintercept = min(price)), linetype = "dashed", color = "red") +  geom_hline(mapping = aes(yintercept = max(price)), linetype = "dashed", color = "red") +  xlab("Time period") + ylab("Total Sales") + theme_linedraw() + ggtitle("Total Sales across time period", subtitle = "Max Sale - Nov22,2019 \nMin Sale - Dec31,2019")

#cart abandonment rate and conversion rate (conversion goal is purchases made)

cart <- trend %>% filter(Event_type == "cart")
purchase <- trend %>% filter(Event_type == "purchase")
removed <- trend %>% filter(Event_type == "remove_from_cart")
view <- trend %>% filter(Event_type == "view")

#cart abandonment rate 
ab_rate <- (1- (purchase$Freq/cart$Freq)) * 100 
#conversion rate for purchases made 
conv_rate <- (purchase$Freq/(view$Freq + cart$Freq + removed$Freq + purchase$Freq)) * 100 
month <- c("2019-10", "2019-11", "2019-12", "2020-01", "2020-02")
rate <- data.frame(ab_rate, conv_rate, month)
rate

options(repr.plot.width = 13, repr.plot.height = 5)
p1 <- ggplot(data = rate, mapping = aes(x = month)) + geom_line(mapping = aes(y = ab_rate, group = 1), lwd = 1.15) + geom_point(mapping = aes(y = ab_rate, color = month, size = 1), show.legend = FALSE) + theme_linedraw() +  ggtitle("Cart abandonment rate per month ") + xlab("Time period") + ylab("Abandonment rate (in %)")
options(repr.plot.width = 13, repr.plot.height = 5)
p2 <- ggplot(data = rate, mapping = aes(x = month)) + geom_line(mapping = aes(y = conv_rate, group = 1), lwd = 1.15) + geom_point(mapping = aes(y = conv_rate, color = month, size = 1), show.legend = FALSE) + theme_linedraw() +  ggtitle("Conversion rate per month ") + xlab("Time period") + ylab("Conversion rate (in %)")

grid.arrange(p1,p2, nrow = 1)

#who has brought in highest revenue 

t1 <- sample_2019_oct %>% select(user_id, event_type, price)
t2 <- sample_2019_nov %>% select(user_id, event_type, price)
t3 <- sample_2019_dec %>% select(user_id, event_type, price)
t4 <- sample_2020_jan %>% select(user_id, event_type, price)
t5 <- sample_2020_feb %>% select(user_id, event_type, price)
t6 <- rbind(t1,t2,t3,t4,t5)

#remove unwanted attributes from the workspace 
rm(t1,t2,t3,t4,t5)

loyalty <- t6 %>% filter(event_type == "purchase") 
loyal <- aggregate(price ~ user_id, loyalty, sum)
loyal <- loyal[(loyal$price>0),] #remove the observations where prices are below 0; total 16
loyal <- loyal %>% arrange(desc(price)) 
top_10 <- head(loyal, 10)
top_10$key <- factor(c("A","B","C","D", "E", "F", "G", "H", "I", "J"))
top_10
worst_10 <- tail(loyal, 10)

options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(data = top_10, mapping = aes(x = key, y = price)) + geom_bar(stat = "identity", mapping = aes(fill = key)) + coord_flip() +  scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500,3000,3500, 4000)) + theme_linedraw() +  ggtitle("Revenue brought in by most loyal customers") + xlab("User (in key)") + ylab("Revenue (in dollars)")

# Price vs hour categorized by event type 

t1 <- sample_2019_oct %>% select(price, event_time, event_type)
t1$hour <- hour(t1$event_time)
t2 <- sample_2019_nov %>% select(price, event_time, event_type)
t2$hour <- hour(t2$event_time)
t3 <- sample_2019_dec %>% select(price, event_time, event_type)
t3$hour <- hour(t3$event_time)
t4 <- sample_2020_jan %>% select(price, event_time, event_type)
t4$hour <- hour(t4$event_time)
t5 <- sample_2020_feb %>% select(price, event_time, event_type)
t5$hour <- hour(t5$event_time)
time <- rbind(t1,t2,t3,t4,t5)
time_hour <- aggregate(price ~ event_type + hour, time, sum)
head(time_hour)

options(repr.plot.width = 10, repr.plot.height = 6)
ggplot(data = time_hour, mapping = aes(x = hour, y = scale(price))) + geom_line(mapping = aes(color = event_type), lwd = 1.2) + theme_linedraw() + ggtitle("Relationship between price and hour of the day based on event type") + xlab("Time (in Hours)") + ylab("Scaled Price") +  geom_vline(mapping = aes(xintercept = 19), linetype = "dashed", color = "black") +  geom_vline(mapping = aes(xintercept = 11), linetype = "dashed", color = "black") +  geom_text(mapping = aes(x = 10, y = -0.5), label = "Max - Purchase") + geom_text(mapping = aes(x = 19, y = 2.7), label = "Max - View, Cart, Remove") + scale_x_continuous(breaks = c(0:24))


# number of clicks in session
sample_2020_feb %>%
  group_by(user_session) %>%
  summarise(clicks_in_session = n(), purchase_in_session = n(event_type[event_type = 'purchase']))%>%
  ggplot(aes(clicks_in_session))+geom_histogram(breaks = seq(0,100,by = 10))

#
