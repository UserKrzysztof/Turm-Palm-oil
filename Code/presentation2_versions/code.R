library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
options(scipen = 12)


df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")
str(df)


df %>% 
  filter((Area.Code < 300) & 
           stri_detect_regex(Item, "Palm oil")) %>% 
  group_by(Area) %>% 
  summarise(sum_2020 = sum(Y2020)) %>% 
  top_frac(.2,sum_2020)-> df3

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

map2 %>% arrange(order)-> map2

write.csv(map2, "map2.csv")#save data for further plot drawing

###########
map2 <- read.csv("map2.csv")

ggplot(map2, aes(long, lat, map_id = region)) +
  geom_map(
    map = map2,
    aes(fill = sum_2020),
    color = "#f5f5f5",
    linewidth = 0.1
  ) +
  scale_fill_continuous(
    name = "Production(tonnes)",
    high = "#491b00",
    low = "#F04a00",
    limits = c(min(map2$sum_2020, na.rm = T), max(map2$sum_2020, na.rm = T)),
    na.value = "gray50",
    expand = c(0, 0)
  ) +
  labs(title = "Palm oil producing countries, top 50%",
       subtitle = "2020, unit: tonnes") +
  theme(
    text = element_text(colour = "white"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black")
  ) +
  guides(
    fill = guide_colorbar(
      title = element_blank(),
      frame.colour = "black",
      title.vjust = 0.8,
      barwidth = 20,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  )


