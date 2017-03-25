
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)
library(maps)
library(rmarkdown)
library(knitr)
library(viridis)
library(gridExtra)
setwd("/Users/chengzhang/Documents/BAData")
getwd()
load(file = "/Users/chengzhang/Desktop/calldata.rda")
load(file = "/Users/chengzhang/Desktop/calldata_2.rda")
load(file = "/Users/chengzhang/Desktop/zipcode_map.rda")


## Yearly Time Distribution 2011-2015

k <- call1 %>% 
  filter(!is.na(yr)) %>% 
  group_by(yr, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Time Distribution 2011-2015", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~yr)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 6))

ggsave("k.png",path = "/Users/chengzhang/Desktop/project _results")


## Monthly Time Distribution 2011-2015

l <- call1 %>% 
  filter(!is.na(yr)) %>% 
  group_by(mth, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Montly Call Time Distribution 2011-2015", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~mth)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 4))
ggsave("l.png",path = "/Users/chengzhang/Desktop/project _results")



## Yearly Spatial Distribution 2011-2015


la <- qmap("Mid City LA", maptype = "road", zoom = 11, color = "bw")

data1 <- call1 %>% 
  filter(!is.na(Zip.Code),
         !Zip.Code %in% c(99999),
         Service.Name != "") %>% 
  group_by(yr, mth, Zip.Code, Service.Name) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

joindata1 <- left_join(lazip, data1, by = c("id" = "Zip.Code"))

m <- la+
  geom_polygon(data = joindata1, 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Frequency by Zip Area 2011-2015", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~yr)

ggsave("m.png",path = "/Users/chengzhang/Desktop/project _results")



## Monthly Spatial Distribution 2011-2015

n <- la+
  geom_polygon(data = joindata1, 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Frequency by Zip Area 2011-2015", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~mth)

ggsave("n.png",path = "/Users/chengzhang/Desktop/project _results")



## Yearly Time Distribution 2016```{r}
newdata$CreatedDate <- mdy_hms(newdata$CreatedDate)
newdata$UpdatedDate <- mdy_hms(newdata$UpdatedDate)
newdata <- newdata %>% 
  mutate(wd = wday(CreatedDate, label = T),
         mth = month(CreatedDate, label = T),
         hr = hour(CreatedDate),
         yr = year(CreatedDate))

str(newdata)
o <- newdata %>% 
  filter(!is.na(yr),
         yr == 2016) %>% 
  group_by(yr, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Yearly Call Time Distribution 2016", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~yr)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave("o.png",path = "/Users/chengzhang/Desktop/project _results")



## Monthly Time Distribution 2016

p <- newdata %>% 
  filter(!is.na(yr),
         yr == 2016) %>% 
  group_by(mth, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Time Distribution 2016", y = "Hour", x = "Weekday", fill = "Frequency")+
  theme_classic()+
  facet_wrap(~mth)+
  theme(panel.background = element_rect(color = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave("p.png",path = "/Users/chengzhang/Desktop/project _results")



## Yearly Spatial Distribution 2016

data2 <- newdata %>% 
  group_by(yr, mth, ZipCode, RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

str(newdata)
str(lazip)

newdata$ZipCode <- as.numeric(as.character(newdata$ZipCode))
lazip$id <- as.numeric(lazip$id)

joindata2 <- left_join(lazip, data2, by = c("id" = "ZipCode"))

q <- la+
  geom_polygon(data = filter(joindata2, yr == 2016), 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Call Frequency by Zip Area 2016", fill =  "Frequency", alpha = "Transparency")

ggsave("q.png",path = "/Users/chengzhang/Desktop/project _results")



## Monthly Spatial Distribution 2016

r <- la+
  geom_polygon(data = filter(joindata2, yr == 2016), 
               aes(x = long, y = lat, group = group, fill = count, alpha = count),
               color = "darkgrey")+
  theme_void()+
  scale_fill_viridis()+
  labs(title = "Monthly Call Frequency by Zip Area 2016", fill =  "Frequency", alpha = "Transparency")+
  facet_wrap(~mth)
ggsave("r.png",path = "/Users/chengzhang/Desktop/project _results")



## Efficiency

newdata <- newdata %>% 
  mutate(proc_time = UpdatedDate - CreatedDate)
newdata$proc_time <- round(newdata$proc_time/(60*60*24), digits = 0)

newdata %>% 
  group_by(RequestType) %>% 
  summarise(Average = mean(proc_time)) %>% 
  arrange(-Average)


## Average efficiency by request type

a <- newdata %>% 
  group_by(RequestType) %>% 
  summarise(Average = mean(proc_time)) %>% 
  ggplot(aes(x = reorder(RequestType,Average), y = Average, fill = Average))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  labs(title = "Procedure Time", x = NULL, y = "Avg. Days")+
  scale_fill_viridis()+
  theme_classic()


## Total number of request of each type

levels(newdata$RequestType)
newdata$RequestType = factor(newdata$RequestType, 
                             levels = c("Report Water Waste",
                                        "Dead Animal Removal",
                                        "Graffiti Removal",
                                        "Metal/Household Appliances",
                                        "Electronic Waste",
                                        "Bulky Items",
                                        "Illegal Dumping Pickup",
                                        "Single Streetlight Issue",
                                        "Other",
                                        "Feedback",
                                        "Multiple Streetlight Issue",
                                        "Homeless Encampment"))


b <- newdata %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = RequestType, y = count/1000, fill = RequestType))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  labs(title = "Calling Number Count", x = NULL, y = "Count (in Thousands)")+
  theme_classic()+
  theme(axis.text.y = element_blank())+
  scale_fill_viridis(discrete = T)


## Comparison Chart

s <- grid.arrange(a, b, nrow = 1)
ggsave("s.png",path = "/Users/chengzhang/Desktop/project _results")




t <- newdata %>% 
  group_by(RequestType) %>% 
  summarise(count = n(),
            sumtime = sum(proc_time),
            eff = count/sumtime) %>% 
  filter(!RequestType %in% c("Report Water Waste", "Other")) %>%
  arrange(-eff) %>% 
  ggplot(aes(x = reorder(RequestType, -eff), y = eff, fill = eff))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  scale_fill_viridis(direction = 1)+
  labs(title = "Efficiency Rating", x = "Request Type", y = "Efficiency Ratio")+
  theme_classic()

ggsave("t.png",path = "/Users/chengzhang/Desktop/project _results")



## low eff - homeless encampment

u <- newdata %>% 
  filter(RequestType == "Homeless Encampment") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/14497, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Homeless Encampment", x = "Request Sourece", y = "Percentage")+
  theme_classic()

ggsave("u.png",path = "/Users/chengzhang/Desktop/project _results")




v <- newdata %>% 
  filter(RequestType == "Homeless Encampment") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Homeless Encampment",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

ggsave("v.png",path = "/Users/chengzhang/Desktop/project _results")


## low eff - multiple street light issue

w <- newdata %>% 
  filter(RequestType == "Multiple Streetlight Issue") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/4989, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Multiple Streetlight Issue", x = "Request Sourece", y = "Percentage")+
  theme_classic()
ggsave("w.png",path = "/Users/chengzhang/Desktop/project _results")




x <- newdata %>% 
  filter(RequestType == "Multiple Streetlight Issue") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Multiple Streetlight Issue",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))
ggsave("x.png",path = "/Users/chengzhang/Desktop/project _results")




## large request number - bulky item

y <- newdata %>% 
  filter(RequestType == "Bulky Items") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/542372, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Bulky Items", x = "Request Sourece", y = "Percentage")+
  theme_classic()
ggsave("y.png",path = "/Users/chengzhang/Desktop/project _results")



z <- newdata %>% 
  filter(RequestType == "Bulky Items") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Bulky Items",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

ggsave("z.png",path = "/Users/chengzhang/Desktop/project _results")


## large request number - graffiti removal

aa <- newdata %>% 
  filter(RequestType == "Graffiti Removal") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/279723, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Graffiti Removal", x = "Request Sourece", y = "Percentage")+
  theme_classic()

ggsave("aa.png",path = "/Users/chengzhang/Desktop/project _results")




bb <- newdata %>% 
  filter(RequestType == "Graffiti Removal") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Graffiti Removal",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))
ggsave("bb.png",path = "/Users/chengzhang/Desktop/project _results")


## high eff - dead animal removal


cc <- newdata %>% 
  filter(RequestType == "Dead Animal Removal") %>% 
  group_by(RequestSource) %>% 
  summarise(pct = round(n()/31697, digit = 2)) %>% 
  arrange(-pct) %>% 
  slice(1:3) %>% 
  ggplot(aes(x = RequestSource, y = pct, fill = RequestSource))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis(discrete = T)+
  labs(title = "Source of Dead Animal Removal", x = "Request Sourece", y = "Percentage")+
  theme_classic()

ggsave("cc.png",path = "/Users/chengzhang/Desktop/project _results")




dd <- newdata %>% 
  filter(RequestType == "Dead Animal Removal") %>% 
  group_by(RequestSource, wd, hr) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = wd, y = factor(hr), fill = count))+
  geom_tile()+
  scale_fill_viridis()+
  facet_wrap(~RequestSource)+
  labs(title = "Contact Method of Dead Animal Removal",
       x = NULL, 
       y = NULL)+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45))

ggsave("dd.png",path = "/Users/chengzhang/Desktop/project _results")






s <- newdata %>% 
  group_by(RequestType, wd, hr, RequestSource) %>% 
  summarise(avg = mean(proc_time)) %>% 
  ggplot(aes(x = factor(hr), y = avg, fill = RequestSource))+
  geom_bar(stat = "identity", position = "dodge", show.legend = F)+
  facet_wrap(~RequestSource)+
  theme_minimal()+
  labs(title = "Avg. Procedure Time via Each Request Source in 24 Hours", x = "Hour in a Day", y = "Procedure Days")+
  theme(axis.text.y = element_text(size = 4))+
  scale_fill_viridis(discrete = T)+
  coord_flip()

ggsave("s.png",path = "/Users/chengzhang/Desktop/project _results")




ll <- newdata %>% 
  filter(RequestSource %in% c("Mobile App")) %>% 
  group_by(RequestType) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ggplot(aes(x = reorder(RequestType, count), y = count, fill = count))+
  geom_bar(stat = "identity", show.legend = F)+
  scale_fill_viridis()+
  coord_flip()+
  theme_classic()+
  labs(title = "Ranking of Request Type via Mobile App", x = "Request Type", y = "Count")

ggsave("ll.png",path = "/Users/chengzhang/Desktop/project _results")










