library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
options(scipen = 12)


df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")

df %>%
  filter(Area.Code < 300) %>%
  filter(
    stri_detect_regex(Item, "oil") &
      !stri_detect_regex(Item, "seeds") &
      !stri_detect_regex(Item, "Saff")
  ) %>%
  group_by(Item, Element, Unit) %>%
  summarise(across(Y1961:Y2020, ~ sum(.x, na.rm = TRUE))) -> df2

df2 %>% 
  filter(Element == "Production") %>% 
  pivot_longer(matches("Y[0-9]{4}"), names_to = "year") %>% 
  mutate(year = as.double(substr(year,2,length(year))),
         value = value/1000)-> df3
  
  
ggplot(df3, aes(
  x = year,
  y = value,
  color = Item,
  shape = Item
)) +
  geom_point() +
  geom_line() +
  scale_shape_manual(values = c(0, 1, 2, 3, 16, 7, 8, 9, 10)) +
  scale_color_manual(
    values = c(
      "#777b7e",
      "#777b7e",
      "#777b7e",
      "#777b7e",
      "#ff3300",
      "#777b7e",
      "#777b7e",
      "#777b7e"
    )
  ) +
  theme(
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.line = element_line(colour = "#303030"),
    plot.background = element_rect(fill = "black"),
    panel.grid.minor  = element_blank(),
    panel.grid.major = element_line(color = "#303030"),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black")
  ) +
  xlab("Year") +
  ylab("Production (1000 tonnes)")
  

