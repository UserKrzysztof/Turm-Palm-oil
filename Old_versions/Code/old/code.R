library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
library(patchwork)
options(scipen = 12)

df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")
str(df)

df %>% 
  filter((Area.Code < 300) & 
           stri_detect_regex(Item, "Palm oil")) %>% 
  group_by(Area) %>% 
  summarise(sum_2020 = Y2020/1000000) %>% 
  filter(sum_2020!=0)-> df3_1

df %>% 
  filter((Area.Code < 300) & 
           stri_detect_regex(Item, "Palm oil")) %>% 
  group_by(Area) %>% 
  summarise(sum_2020 = Y2020/1000000) %>% 
  filter(sum_2020>0) %>% 
  top_frac(0.25,sum_2020)-> df3

#########

mapa<- map_data("world")

map2 <- merge.data.frame(mapa,df3, by.x = "region", by.y = "Area", all.x = T)

subset(df3, !(Area %in% map2$region)) -> excluded

df3 %>% mutate(
  Area = case_when(
    stri_detect_regex(Area, "China") ~ "China",
    stri_detect_regex(Area, "Russia") ~ "Russia",
    Area == "Congo" ~ "Republic of Congo",
    stri_detect_regex(Area, "Venezuela") ~ "Venezuela",
    stri_detect_regex(Area, "Tanzania") ~ "Tanzania",
    stri_detect_regex(Area, "e d'Ivoire") ~ "Ivory Coast",
    TRUE ~ Area
  )
) -> df_final

map2 <- merge.data.frame(mapa,df_final, by.x = "region", by.y = "Area", all.x = T)
subset(df_final, !(Area %in% map2$region)) -> excluded

map2 %>% 
  filter(!long > 180, ! lat < -60) %>% arrange(order)-> map2

write.csv(map2, "map2.csv")#save data for further plot drawing

###########
map2 <- read.csv("map2.csv")

tre1 = 1
tre2 = 10
bh = 5

map2 %>% filter(is.na(sum_2020)) %>% 
  mutate(sum_2020 = "No data or\nlow production") %>% 
  ggplot( aes(long, lat, map_id = region)) +
  geom_map(
    map = map2,
    aes(fill = sum_2020),
    color = "#404040",
    linewidth = 0.1,
  ) +
  scale_fill_manual(values ="#202020")+
  coord_map("moll") +
  theme(
    text = element_text(colour = "white"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "black")
  ) +
  guides(fill = guide_legend(""))+
  labs(
    title = element_text("",color = "black"),
    subtitle = element_text("",color = "black")
  ) -> m0

map2 %>% filter(sum_2020 < tre1 | is.na(sum_2020)) -> data1
minmax = c(min(data1$sum_2020, na.rm = T), max(data1$sum_2020, na.rm = T))
mid = mean(minmax)

data1 %>% 
ggplot( aes(long, lat, map_id = region)) +
  geom_map(
    map = map2,
    aes(fill = sum_2020),
    color = "#505050",
    linewidth = 0.1,
  ) +
  scale_fill_gradient2(
    name = "Production(tonnes)",
    high= "#2c041c",
    mid = "#e39ff6",
    low = "white",
    midpoint = mid,
    na.value = "#202020",
    limits = c(minmax[1],minmax[2]),
    breaks = round(seq(minmax[1],minmax[2],length.out = 4),3)
  ) +
  coord_map("moll") +
  theme(
    text = element_text(colour = "white"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "black")
  ) +
  guides(
    fill = guide_colorbar(
      title = element_blank(),
      frame.colour = "black",
      title.vjust = 0.8,
      barwidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      barheight = bh,
    ))+
  labs(
    title = element_text("",color = "black"),
    subtitle = element_text("",color = "black")
  ) -> m1

map2 %>% filter((sum_2020 >= tre1 & sum_2020 < tre2) | is.na(sum_2020)) -> data2
minmax = c(min(data2$sum_2020, na.rm = T), max(data2$sum_2020, na.rm = T))

data2 %>% 
  ggplot( aes(long, lat, map_id = region)) +
  geom_map(
    map = map2,
    aes(fill = sum_2020),
    color = "gray60",
    linewidth = 0.1,
  ) +
  scale_fill_gradient(
    name = "Production(tonnes)",
    high = "#354a21",
    low = "#aef359",
    na.value = "#303030",
    breaks = round(seq(minmax[1],minmax[2],length.out = 4),3)
  ) +
  coord_map("moll") +
  theme(
    text = element_text(colour = "white"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "black")
  ) +
  guides(
    fill = guide_colorbar(
      title = element_blank(),
      frame.colour = "black",
      title.vjust = 0.8,
      barwidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      barheight = bh,
    )
  ) +
  labs(
    title = element_text("",color = "black"),
    subtitle = element_text("",color = "black")
  ) ->m2

map2 %>% filter(sum_2020 >= tre2 | is.na(sum_2020)) -> data3
minmax = c(min(data3$sum_2020, na.rm = T), max(data3$sum_2020, na.rm = T))

data3 %>% 
  ggplot( aes(long, lat, map_id = region)) +
  geom_map(
    map = map2,
    aes(fill = sum_2020),
    color = "#404040",
    linewidth = 0.1,
  ) +
  scale_fill_continuous(
    name = "Production(tonnes)",
    low = "#f9e076",
    high = "#fc6a03",
    na.value = "#101010",
    breaks = round(seq(minmax[1],minmax[2], length.out = 3),3)
  ) +
  coord_map("moll") +
  theme(
    text = element_text(colour = "white"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "black")
  ) +
  guides(
    fill = guide_colorbar(
      title = "Unit:\n1M tonnes",
      frame.colour = "black",
      title.vjust = 0.8,
      barwidth = 1,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      barheight = bh
    )) +
  labs(
    title = "Top 25% palm oil producers in 2020",
    subtitle = "They produced over 96% of total world palm oil production"
  )->m3


m3 + theme(plot.background = element_rect(fill = "black", color = "black"),
         panel.background = element_rect(fill= "black", color = "black"),
         ) +
  inset_element(m2, left = 0, bottom = 0, right = 1, top= 1, align_to = "full") -> m12

m12 +inset_element(m1, left = 0, bottom = 0, right = 1, top= 1, align_to = "full") ->m123

m123 + inset_element(m0, left = 0, bottom = 0, right = 1, top= 1, align_to = "full") -> m1230
  
m1230 +
  guide_area()+ 
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill= "black", color = "black"),
        plot.margin = margin(r=0.3,unit = "cm")) + 
  plot_spacer()+
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill= "black", color = "black"),
        plot.margin = margin(r=0.3,unit = "cm"))+
  plot_layout(design= 
                "
              111111112
              111111112
              111111112
              111111112
              111111112
              111111112
              111111113
            ",
              guides = "collect",
              )

c(sum(df3$sum_2020),sum(df3_1$sum_2020,na.rm = T))
# sum for considered countries = 73.09732 M tonnes
# total 75.87555 75.87555
  
