library(ggplot2)
cellPlans = data.frame(
  c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
    "Verizon", "ATT", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "ATT", "ATT", "Sprint"),
  c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 12, 16, 16,
    24, 24, 25, 30, 40),
  c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 80, 60,
    90, 90, 110, 80, 110, 135, 100))
names(cellPlans) = c("Company", "DataGB", "Price")

head(cellPlans)
#scatterplot
ggplot(cellPlans, aes(x=DataGB,y=Price,color=Company)) + 
  geom_point() +
  scale_colour_hue(l=50) +
  ggtitle("Phone Data Plans by Price & Company") +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

#bar chart
ggplot(cellPlans, aes(x=Company,y=Price,group=DataGB,fill = factor(DataGB))) + 
  geom_bar(stat="identity", position = "dodge",colour="black") +
  ggtitle("Phone Data Plans by Price & Company") +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = .5, face = "bold",size = 18))

  