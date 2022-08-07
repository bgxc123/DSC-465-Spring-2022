library(tidyverse)
library(dplyr)
library(ggplot2)
#read in file
df = read.csv("energy-usage-2010.csv")
head(df)
summary(df)
df_test = na.omit(df)
summary(df_test)

#aggregate data properly
df_waff = df_test %>%
  select(BUILDING_SUBTYPE, TOTAL.KWH, TOTAL.THERMS) %>%
  group_by(BUILDING_SUBTYPE) %>%
  summarise(TTLKWH = sum(TOTAL.KWH), TTLTHERM = sum(TOTAL.THERMS))

#sort
df_waff = df_waff[order(-df_waff$TTLKWH),]

#Waffle Chart
remotes::install_github("hrbrmstr/waffle",force=TRUE)
library(waffle)
#KWH
ggplot(df_waff, aes(fill=BUILDING_SUBTYPE, values = TTLKWH)) +
  geom_waffle(flip = TRUE,make_proportional = TRUE, color = "white",
              size = 3) +
  ggtitle("Total KWH by Building Subtype") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
#making facet chart for therm
df_waff = df_waff %>%
  rowwise() %>%
  mutate(PCT.THERMS = TTLTHERM/sum(df_waff$TTLTHERM)*100)
df_waff = df_waff %>%
  mutate_at(vars(PCT.THERMS), funs(round(., 0)))


ggplot(df_waff[1:4,], aes(fill=BUILDING_SUBTYPE, values = PCT.THERMS)) +
  geom_waffle(flip = TRUE, n_rows = 5, color = "white",
              show.legend = FALSE) +
  facet_wrap(~BUILDING_SUBTYPE) +
  ggtitle("Total THERMS by Building Subtype") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        panel.spacing.x = unit(0,"npc"),
        strip.text.x = element_text(hjust=.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())

#circular radial chart
df_rad = df_test %>%
  select(BUILDING_SUBTYPE:KWH.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(MED = median(MED.AMOUNT))
df_rad$MONTH = str_replace(df_rad$MONTH,"KWH.","")
df_rad$MONTH = str_replace(df_rad$MONTH,".2010","")
df_rad = df_rad %>%
  mutate(MONTH = factor(MONTH,levels = 
                          c("JANUARY","FEBRUARY","MARCH","APRIL",
                            "MAY","JUNE","JULY","AUGUST","SEPTEMBER",
                            "OCTOBER","NOVEMBER","DECEMBER")))

ggplot(df_rad, aes(MONTH,MED, fill = BUILDING_SUBTYPE)) +
  geom_bar(stat='identity', color = "black",position = position_dodge(),
           width = .75, size = .3) +
  scale_y_continuous(trans = "log2") +
  coord_polar(theta = "x", start = 0) +
  scale_fill_brewer(palette = 4) +
  ylab("COMMUNITY") +
  xlab("MONTH") +
  ggtitle("KWH Log Scale Radial Chart MoM") +
  theme(axis.text.x = element_text(size = 6),
        plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())

df_rad2 = df_test %>%
  select(BUILDING_SUBTYPE,THERM.JANUARY.2010:THERM.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(MED = median(MED.AMOUNT))
df_rad2$MONTH = str_replace(df_rad2$MONTH,"TERM.","")
df_rad2$MONTH = str_replace(df_rad2$MONTH,"THERM.","")
df_rad2$MONTH = str_replace(df_rad2$MONTH,".2010","")
df_rad2 = df_rad2 %>%
  mutate(MONTH = factor(MONTH,levels = 
                          c("JANUARY","FEBRUARY","MARCH","APRIL",
                            "MAY","JUNE","JULY","AUGUST","SEPTEMBER",
                            "OCTOBER","NOVEMBER","DECEMBER")))

ggplot(df_rad2, aes(MONTH,MED, fill = BUILDING_SUBTYPE)) +
  geom_bar(stat='identity', color = "black",position = position_dodge(),
           width = .75, size = .3) +
  scale_y_continuous(trans = "log2") +
  coord_polar(theta = "x", start = 0) +
  scale_fill_brewer(palette = 7) +
  ylab("COMMUNITY") +
  xlab("MONTH") +
  ggtitle("THERM Log Scale Radial Chart MoM") +
  theme(axis.text.x = element_text(size = 6),
        plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
