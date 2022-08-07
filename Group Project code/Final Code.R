df = read.csv("energy-usage-2010.csv")
head(df)
summary(df)
df_test = df %>%
  drop_na(CENSUS.BLOCK) %>%
  select(BUILDING_SUBTYPE:KWH.DECEMBER.2010, THERM.JANUARY.2010:THERM.DECEMBER.2010)
df_test = na.omit(df_test) #60k observations, still maintaining 90% of data
summary(df_test)


#transform variables, compare correlation plots
df_nums = df_test[c(5:17,20:32,64:69)]
library(psych)
pairs.panels(df_nums[c(13,26:32)])
df_nums  = df_nums %>%
  select(TOTAL.KWH, TOTAL.THERMS : OCCUPIED.UNITS) %>%
  mutate(total.kwh_log = log2(TOTAL.KWH)) %>%
  mutate(total.therm_log = log2(TOTAL.THERMS))
pairs.panels(df_nums[3:10])

#median line charts
df_kwh = df_test %>%
  select(BUILDING_SUBTYPE:KWH.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(MED = median(MED.AMOUNT))

df_kwh$m = str_replace(df_kwh$MONTH,"KWH.","")
df_kwh$m = str_replace(df_kwh$m,".2010","")
months = c("JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE",
           "JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
period = c(1,2,3,4,5,6,7,8,9,10,11,12)
months = list(months,period)
months = as.data.frame(months)
colnames(months) = c("m","p")
df_kwh = inner_join(df_kwh,months)
df_kwh$p = as.factor(df_kwh$p)

#line chart - kwh
lineK = ggplot(df_kwh, aes(x = p, y = MED, group=BUILDING_SUBTYPE, colour = BUILDING_SUBTYPE)) +
  geom_line(size = .8) +
  xlab("Month") +
  labs(color = "Building Subtype") +
  scale_y_continuous(trans = "log2") +
  ylab("Median KWH") +
  theme_classic() +
  ggtitle("Median KWH by Building Subtype MoM") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))
lineK

df_therm = df_test %>%
  select(BUILDING_SUBTYPE, THERM.JANUARY.2010: THERM.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(MED = median(MED.AMOUNT))
df_therm$m = str_replace(df_kwh$MONTH,"KWH.","")
df_therm$m = str_replace(df_kwh$m,".2010","")
df_therm = inner_join(df_therm,months)
df_therm$p = as.factor(df_therm$p)

#line chart - therm
lineT = ggplot(df_therm, aes(x = p, y = MED, group=BUILDING_SUBTYPE, colour = BUILDING_SUBTYPE)) +
  geom_line(size = .8) +
  xlab("Month") +
  labs(color = "Building Subtype") +
  scale_y_continuous(trans = "log2") +
  ylab("Median Therm") +
  theme_classic() +
  ggtitle("Median Therm by Building Subtype MoM") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))
lineT
#sum kwh/therm
df_kwh2 = df_test %>%
  select(BUILDING_SUBTYPE:KWH.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "SUM.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(SUM = sum(SUM.AMOUNT))

df_kwh2$m = str_replace(df_kwh2$MONTH,"KWH.","")
df_kwh2$m = str_replace(df_kwh2$m,".2010","")
months = c("JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE",
           "JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
period = c(1,2,3,4,5,6,7,8,9,10,11,12)
months = list(months,period)
months = as.data.frame(months)
colnames(months) = c("m","p")
df_kwh2 = inner_join(df_kwh2,months)
df_kwh2$p = as.factor(df_kwh2$p)

#area chart - total kwh
areaK = ggplot(df_kwh2, aes(x = p, y = SUM, group=BUILDING_SUBTYPE, fill = BUILDING_SUBTYPE)) +
  geom_area() +
  xlab("Month") +
  ylab("Total KWH") +
  theme_classic() +
  labs(fill = "Building Subtype") +
  ggtitle("Total KWH by Building Subtype MoM") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8)) +
  scale_color_viridis_c()
areaK
df_therm2 = df_test %>%
  select(BUILDING_SUBTYPE, THERM.JANUARY.2010: THERM.DECEMBER.2010) %>%
  pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "SUM.AMOUNT") %>%
  group_by(BUILDING_SUBTYPE,MONTH) %>%
  summarise(SUM = sum(SUM.AMOUNT))
df_therm2$m = str_replace(df_therm2$MONTH,"THERM.","")
df_therm2$m = str_replace(df_therm2$m,"TERM.","")
df_therm2$m = str_replace(df_therm2$m,".2010","")
df_therm2 = inner_join(df_therm2,months)
df_therm2$p = as.factor(df_therm2$p)

#area chart - total therm
areaT = ggplot(df_therm2, aes(x = p, y = SUM, group=BUILDING_SUBTYPE, fill = BUILDING_SUBTYPE)) +
  geom_area() +
  xlab("Month") +
  ylab("Total Therm") +
  labs(fill = "Building Subtype") +
  theme_classic() +
  ggtitle("Total Therm by Building Subtype MoM") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8)) +
  scale_color_viridis_c()
areaT

#2x2 arrange
library(ggpubr)
ggarrange(areaK, areaT,lineK, lineT,
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

#table of counts
df_ct = df %>%
  select(BUILDING_SUBTYPE) %>%
  group_by(BUILDING_SUBTYPE) %>%
  summarise(CT = n())

#sort
df_ct = df_ct[order(-df_ct$CT),]
df_ct[nrow(df_ct)+1,] = list("Municipal/Industrial",401)
df_ct$BUILDING_SUBTYPE = factor(df_ct$BUILDING_SUBTYPE, levels = c("Municipal/Industrial",
                                                                   "Multi 7+","Commercial",
                                                                   "Single Family","Multi < 7"))
#waffle chart
library(waffle)
ggplot(df_ct, aes(fill=BUILDING_SUBTYPE, values = CT)) +
  geom_waffle(flip = TRUE,make_proportional = TRUE, color = "white",
              size = 3) +
  ggtitle("Frequencies by Building Subtype") +
  labs(fill = "Building Subtype") +
  theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

#waff animated - DID NOT USE
# install.packages("animation")
# library(animation)
# saveGIF({
#   for (i in df_ct$BUILDING_SUBTYPE) {
#     d1 = df_ct %>% filter(BUILDING_SUBTYPE==i)
#     
#     gg1 = d1 %>%
#       ggplot(aes(fill = BUILDING_SUBTYPE, values = CT)) +
#       geom_waffle(flip = TRUE, color = "white",
#                   size = .75) +
#       labs(title=i) +
#       
#       theme(plot.title = element_text(hjust = .1, face = "bold",size = 15),
#             axis.text.x = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks = element_blank(),
#             axis.title.y = element_blank(),
#             rect = element_blank())
#     print(gg1)
#       
#   }
# }, movie.name = "test.gif", ani.height = 500, ani.width = 500*3)

#heatmap in R - DID NOT USE
#therm
# df_zip = df_test %>%
#   select(BUILDING_SUBTYPE,THERM.JANUARY.2010:THERM.DECEMBER.2010) %>%
#   pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
#   group_by(BUILDING_SUBTYPE,MONTH) %>%
#   summarise(MED = median(MED.AMOUNT))
# df_zip$m = str_replace(df_zip$MONTH,"THERM.","")
# df_zip$m = str_replace(df_zip$m,"TERM.","")
# df_zip$m = str_replace(df_zip$m,".2010","")
# df_zip = df_zip %>%
#   mutate(m = factor(m,levels = 
#                           c("JANUARY","FEBRUARY","MARCH","APRIL",
#                             "MAY","JUNE","JULY","AUGUST","SEPTEMBER",
#                             "OCTOBER","NOVEMBER","DECEMBER")))
# 
# #with loop therm log scale
# ggplot(df_zip, aes(m,BUILDING_SUBTYPE, fill = MED)) +
#   geom_tile() +
#   scale_fill_gradient(low="white", high = "red",name="THERM", trans="log",
#                       breaks = c(1000,10000)) +
#   ylab("Building Subtype") +
#   xlab("Month") +
#   ggtitle("Median Therm by Building Subtype MoM") +
#   theme(axis.text.x = element_text(size = 8,angle = 90,vjust = .5, hjust = 1),
#         plot.title = element_text(hjust = .1, face = "bold",size = 15))
# 
# #kwh
# df_zip = df_test %>%
#   select(BUILDING_SUBTYPE,KWH.JANUARY.2010:KWH.DECEMBER.2010) %>%
#   pivot_longer(!BUILDING_SUBTYPE, names_to = "MONTH", values_to = "MED.AMOUNT") %>%
#   group_by(BUILDING_SUBTYPE,MONTH) %>%
#   summarise(MED = median(MED.AMOUNT))
# df_zip$m = str_replace(df_zip$MONTH,"KWH.","")
# df_zip$m = str_replace(df_zip$m,".2010","")
# df_zip = df_zip %>%
#   mutate(m = factor(m,levels = 
#                       c("JANUARY","FEBRUARY","MARCH","APRIL",
#                         "MAY","JUNE","JULY","AUGUST","SEPTEMBER",
#                         "OCTOBER","NOVEMBER","DECEMBER")))
# 
# #with loop kwh log scale
# ggplot(df_zip, aes(m,BUILDING_SUBTYPE, fill = MED)) +
#   geom_tile() +
#   scale_fill_gradient(low="white", high = "blue",name="KWH", trans="log",
#                       breaks = c(7000,33500,60000)) +
#   ylab("Building Subtype") +
#   xlab("Month") +
#   ggtitle("Median KWH by Building Subtype MoM") +
#   theme(axis.text.x = element_text(size = 8,angle = 90,vjust = .5, hjust = 1),
#         plot.title = element_text(hjust = .1, face = "bold",size = 15))


# #animate energy use by month - DID NOT USE
# install.packages("gganimate")
# library(gganimate)
# 
# df_rad2$MED2 = df_rad2$MED
# df_rad2 = df_rad2 %>%
#   select(BUILDING_SUBTYPE,MONTH,MED2)
# 
# df_ani = inner_join(df_rad,df_rad2)
# df_ani$KWH = df_ani$MED
# df_ani$THERM = df_ani$MED2
# df_ani = df_ani %>%
#   select(BUILDING_SUBTYPE,MONTH,KWH,THERM)
# 
# ani = ggplot(df_ani, aes(THERM,KWH,color = BUILDING_SUBTYPE)) +
#   geom_point(alpha=.7, size = 10) +
#   scale_color_viridis_d() +
#   scale_y_continuous(trans="log2") +
#   scale_x_continuous(trans = "log2") +
#   ylab("KWH") +
#   xlab("THERM") +
#   scale_size(range = c(2,12)) +
#   ggtitle("Median Therm by Building Subtype MoM") +
#   theme(axis.text.x = element_text(size = 7,angle = 90,vjust = .5, hjust = 1),
#         plot.title = element_text(hjust = .1, face = "bold",size = 15),
#         panel.background = element_rect(fill="white", color="grey"),
#         panel.grid.major = element_line(color="grey"))
# ani
# ani + transition_manual(MONTH) +
#   labs(title = "Month: {current_frame}")
