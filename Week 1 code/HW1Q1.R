df = read.csv("Intel-1998.csv")
head(df)
library(ggplot2)

#a) line graph
ggplot(data=df, aes(x=Date, y=Close,group=1)) + 
  geom_line(colour="red", size=1.15) + 
  labs(y="Closing Price") +
  ggtitle("IBM Stock Price 1998") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

#b) bar graph volume & date
ggplot(data=df, aes(x=Date,y=Volume)) + 
  geom_bar(stat="identity",colour="blue") +
  ggtitle("IBM Stock Volume 1998") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

#c) scatter plot volume and price range
df$PriceRange = df$High - df$Low
head(df)  

ggplot(data=df, aes(x=Volume, y=PriceRange)) +
  geom_point(shape = 1,colour="brown") +
  ggtitle("IBM Volume vs Daily Range 1998") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))
