library(tidyverse)
library(ggplot2)

#problem #1
df = read.csv("PerceptionExperiment.csv")
df = df %>%
  mutate(error = Response - TrueValue)

#a.)
ggplot(df, aes(x=Test, y=error, color = Test)) +
  geom_boxplot() +
  theme_classic() +
  ggtitle("Error of Test Categories by Box Plot") +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .1, face = "bold",size = 15)) +
  geom_hline(yintercept=0, color="red") +
  scale_color_grey()

#b.)
df = df %>%
  mutate(abError = abs(error))

#strip chart
ggplot(df, aes(x=Test, y= abError, color = Test)) +
  geom_point() +
  theme_classic() +
  ggtitle("Strip Chart - Absolute Error by Test") +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .1, face = "bold",size = 15)) +
  scale_color_viridis_d()
#strip chart w/ jitter
ggplot(df, aes(x=Test, y= abError, color = Test)) +
  geom_jitter() +
  theme_classic() +
  ggtitle("Strip Chart w/ Jitter - Absolute Error by Test") +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .1, face = "bold",size = 15)) +
  scale_color_viridis_d()

#c.)
df_filtered = df %>%
  filter(Subject %in% (56:73))
library(ggforce)
#sina/violin of displays
ggplot(df_filtered, aes(x=factor(Display), y= error, color=factor(Display))) +
  geom_violin() +
  geom_sina() +
  theme_classic() +
  xlab("Display") +
  ggtitle("Violin/Sina Plot - Error by Display") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        legend.position = "none") +
  scale_color_grey()

#d.)
df_d = df %>%
  filter(Test == "Vertical Distance, Non-Aligned") %>%
  filter(Display == 1)
#56 - 73
#scatterplot
ggplot(df_d, aes(x = Subject, y = Response)) +
  geom_point() +
  geom_jitter() +
  theme_classic() +
  ggtitle("Scatter Plot - Display 1, Vertical Distance, Non-Aligned") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        legend.position = "none") +
  geom_hline(yintercept=1, color="red", linetype = 2, size = .3)

#Problem #2
#a.)
df = read.csv("MessierData.csv")
ggplot(df, aes(x= Messier.., y=log(Distance..LY.))) +
  geom_point() +
  theme_classic()
ggplot(df, aes(x= Messier.., y=year)) +
  geom_point() +
  theme_classic()

#b.)
install.packages("ggbeeswarm")
library(ggbeeswarm)
ggplot(df, aes(x=Kind, y= log(Distance..LY.), color=Kind)) +
  geom_violin() +
  geom_sina() +
  theme_classic() +
  ylab("Log Distance") +
  ggtitle("Violin/Sina Plot - Log Distance by Kind") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.text.x = element_blank())

#c.)
ggplot(df, aes(x=Apparent.Magnitude, y= log(Distance..LY.))) +
  geom_point() +
  theme_classic() +
  ylab("Log Distance") +
  xlab("Magnitude (Higher = Fainter)") +
  ggtitle("Scatterplot - Magnitude vs Distance") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))

#d.)
ggplot(df, aes(x=Apparent.Magnitude, y= log(Distance..LY.), size = Size.....)) +
  geom_point() +
  theme_classic() +
  ylab("Log Distance") +
  xlab("Magnitude (Higher = Fainter)") +
  ggtitle("Scatterplot - Magnitude vs Distance") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))

#Problem #3
library(scale)
df = read.csv("MontanaPopulationData.csv")
#a.)
ggplot(df, aes(x = Year,y = Population)) +
  geom_line() +
  ylab("Log Population") +
  scale_y_continuous(
    trans = "log2",
    breaks = trans_breaks("log2",function(x) 2^x),
    labels = trans_format("log2",math_format(2^.x))
  ) +
  ggtitle("Log_2 Scale - Montana Population") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))

#b.)
ggplot(df, aes(x = Year,y = Population)) +
  geom_line() +
  ylab("LN Population") +
  scale_y_continuous(
    trans = log_trans(),
    breaks = trans_breaks("log",function(x) exp(x)),
    labels = trans_format("log",math_format(e^.x))
  ) +
  ggtitle("Natural Log Scale - Montana Population") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))
#c.)
df$test = log(df$Population, base = .15)
ggplot(df, aes(x = Year,y = test)) +
  geom_line() +
  ylab("LN Population") +
  ggtitle("Natural Log Scale - Montana Population") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15))

#Problem #4
data = airquality
print(data)

#a.)
df = read.csv("AirQuality.csv")
head(df)
ggplot(df, aes(x = Wind,y = Solar.R)) +
  geom_point() +
  ggtitle("Scatter Plot - Wind vs Solar") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15)) +
  geom_smooth(method=lm, se = FALSE)

#b.)
library(psych)
pairs.panels(df[2:3])

#c.)
install.packages("reshape")
library(reshape)
df_test = melt(df, id=c("Day"))
ggplot(df_test, aes(x=variable, y= value, color=variable)) +
  geom_violin() +
  geom_sina() +
  theme_classic() +
  ggtitle("Violin/Sina Plot - Air Quality") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.text.x = element_blank())
#d.)
library(magrittr)
library(scales)
df = na.omit(df)
df.qq = df %$%
  data.frame(wind=rescale(Wind, to=c(0,1)) %>% sort,
             solar=rescale(Solar.R, to=c(0,1)) %>% sort)

df.qq = df.qq %>%
  mutate(wind=(wind-min(wind))/(max(wind)-min(wind))) %>%
  mutate(solar=(solar-min(solar))/(max(solar)-min(solar)))

ggplot(df.qq, aes(x=wind,y=solar)) +
  geom_point() +
  ggtitle("QQ Plot - Wind vs Solar") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15)) +
  geom_abline(slope=1, intercept=0)

  