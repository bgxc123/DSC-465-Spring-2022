#Problem 1
df = read.table("FoodSrvcByCounty.txt", header = TRUE,sep = "\t")
codes = read.csv("state codes.csv") #map state to each county recorded
df = left_join(df,codes,by="State")
df = df %>% filter(!is.na(df$region))
head(df)
library(ggplot2)
library(tidyverse)
#a.)
stateData = df %>%
  select(State, FoodServices.97, FoodServices.2002, FoodServices.2007, region) %>%
  rowwise() %>%
  mutate(FoodServices.AVG = sum(FoodServices.97, FoodServices.2002, FoodServices.2007) /3) %>%
  group_by(region) %>%
  summarise(ttl = sum(FoodServices.AVG))
stateData = stateData %>% 
  aggregate(by = list(stateData$region), FUN = sum)

head(stateData)
mapData = map_data("state")

stateData = left_join(mapData,stateData, by="region")

map1 = ggplot(stateData, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = ttl), color = "black")
map1
map2 = map1 + scale_fill_gradient(name = "Food Services",low = "red",
                                  high = "green", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Average Food Services by State 1997 - 2007") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))
map2
#b.)
us_counties = map_data("county")
countyData = df %>%
  select(County, region, FoodServices.97, FoodServices.2002, FoodServices.2007) %>%
  rowwise %>%
  mutate(FoodServices.AVG = sum(FoodServices.97, FoodServices.2002, FoodServices.2007) /3)

countyData$subregion = tolower(countyData$County)
countyData = left_join(us_counties, countyData, by = "subregion")
map3 = ggplot(countyData, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = FoodServices.AVG), color = "black")
map3
map4 = map3 +scale_fill_gradient(name = "Food Services",low = "red",
                                 high = "yellow", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Average Food Services by County 1997 - 2007") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 18))
map4 #outlier of los angeles county

#c.)
long = stateData$long
lat = stateData$lat
coords = cbind(long,lat)
head(coords)
p = Polygon(coords)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(sps, data)
spdf
install.packages("cartogram")
library(cartogram)
state_cartogram = cartogram_cont(spdf, itermax=10)

#Problem #2
#a.)
chiData = map_data("county")
chiData = chiData %>%
  filter(subregion == "cook" & region == "illinois")
df = read.csv("chicago_crashes.csv")
df = df %>% #get rid of 0,0
  filter(LATITUDE != 0 & LONGITUDE != 0)
df$counter = 1
df$long = df$LONGITUDE
df$lat = df$LATITUDE
chiData = full_join(chiData, df, by = c("long","lat"))

p = ggplot(NULL, aes(x=long, y=lat)) +
  geom_polygon(data=chiData,colour = "black", fill ="light pink") +
  geom_point(data=df, alpha=.05) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Car Crashes by location in Chicago - June 2019") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 18))
p

#b.)
df$TIME = str_sub(df$CRASH_DATE,-5)
df$TIME_NUM = as.integer(substr(df$TIME,1,2)) *60 + as.integer(str_sub(df$TIME,-2))

#binning
df = df %>%
  mutate(TIME_OF_DAY = cut(TIME_NUM,breaks=c(0,360,720,1080,1440),
                           labels=c("Early Morning","Morning","Afternoon","Night")))

#graph
chiData$TIME_OF_DAY = ""
p = ggplot(NULL, aes(x=long, y=lat, colour=TIME_OF_DAY)) +
  geom_polygon(data=chiData,colour = "black", fill ="light grey") +
  geom_point(data=df, alpha=.5,size = .35) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Car Crashes by Location & \n Time of Day in Chicago - June 2019") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 18))
p

p = ggplot(NULL, aes(x=long, y=lat, colour=TIME_OF_DAY)) +
  geom_point(data=df, alpha=.5,size = .45) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Car Crashes by Location & \n Time of Day in Chicago - June 2019") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 18))
p

sum(df$TIME_OF_DAY == "Early Morning",na.rm = TRUE)
sum(df$TIME_OF_DAY == "Morning",na.rm = TRUE)
sum(df$TIME_OF_DAY == "Afternoon",na.rm = TRUE)
sum(df$TIME_OF_DAY == "Night",na.rm = TRUE)

#Problem #3
#a.)
df = read.csv("PortlandWaterLevel2003.csv")
head(df)
df$row = seq(1,nrow(df))
df = df %>%
  mutate(timeRange = cut(row,breaks=12, labels=c(seq(1,12))))
waterLevels = df %>%
  group_by(timeRange) %>%
  summarise(AVG = mean(WL))

p = ggplot(waterLevels, aes(x = timeRange,y = AVG, group=1)) +
  geom_line(colour="blue") + 
  geom_point() +
  labs(title = "Portland Month MA Water Levels - 2003",
       x = "Month",y = "Water Level (Month MA)") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 15),
        rect = element_blank())
p
#b.)
df = read.csv("PortlandWaterLevel2003.csv")
head(df)
df$row = seq(1,nrow(df))
df = df %>%
  mutate(timeRange = cut(row,breaks=365, labels=c(seq(1,365))))
waterLevels = df %>%
  group_by(Time) %>%
  summarise(AVG = mean(WL))
waterLevels$Time2 = l
l = c(0,1,10,11,12,13,14,15,16,17,18,19,2,20,21,22,23,3,4,5,6,7,8,9)
p = ggplot(waterLevels, aes(x = Time2,y = AVG, group=1)) +
  geom_line(colour="blue") + 
  geom_point() +
  labs(title = "Portland Hourly MA Water Levels - 2003",
       x = "Hour of Day",y = "Water Level (Year MA)") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 15),
        rect = element_blank())
p

#Problem #4
sdAVG = sd(waterLevels$AVG)
sdAVG
minAVG = min(waterLevels$AVG)
minAVG
meanAVG = mean(waterLevels$AVG)
meanAVG
maxAVG = max(waterLevels$AVG)
maxAVG
p = ggplot(waterLevels, aes(x = Time2,y = AVG, group=1, 
                            colour = cut(AVG,c(.95,1.1,1.19,1.34,2.962)))) +
  geom_point() +
  scale_color_manual(name = "Water Level",
                     values = c("red","orange","blue","green"),
                     labels = c("Very Low WL","Low WL", "High WL","Very High WL")) +
  labs(title = "Portland Hourly MA Water Levels - 2003",
       x = "Hour of Day",y = "Water Level (Year MA)") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 15),
        rect = element_blank())
p
