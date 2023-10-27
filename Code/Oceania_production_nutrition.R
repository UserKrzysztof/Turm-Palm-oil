library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)
options(scipen = 12)

df <- read.csv("Production_Crops_Livestock_E_All_Data_NOFLAG.csv")
str(df)
unique(df$Element)

df %>% 
  filter(grepl("oil", Item)) %>% 
  distinct(Item) 

years <- colnames(df)[stri_detect_regex(colnames(df), "Y[0-9]{4}")]

df %>% 
  filter(grepl("oil", Item)) %>% 
  group_by(Item,Element, Unit) %>% 
  summarise_at(years, sum, na.rm =TRUE) -> oil_production_oceania

unique(oil_production_oceania$Element)

oil_production_oceania %>% 
  filter(Element == "Production") %>% 
  pivot_longer(matches("Y[0-9]{4}"), names_to = "year") %>% 
  mutate(year = as.double(substr(year,2,length(year)))) %>%  
  ggplot(aes(year, value, color =Item, fill = Item)) +
  geom_area(alpha = 0.25) +
  labs(title = "Production of oils per year",
       subtitle = "World, unit: tonnes")
df <- read.csv("Food_Security_Data_E_All_Data_(Normalized).csv")
str(df)
idx <- stri_detect_regex(unique(df$Item), "fat|oil|oils")
interesting <- unique(df$Item)[idx]

df %>% 
  filter(Item == interesting & !(Area.Code>300) )%>% 
  group_by(Area, Unit) %>% 
  summarise(mean_value = mean(as.double(Value), na.rm = T)) %>% ungroup() %>% 
  slice_max(mean_value, prop = 0.1) ->df1

df %>% 
  filter(Item == interesting & !(Area.Code>300) )%>% 
  group_by(Area, Unit) %>% 
  summarise(mean_value = mean(as.double(Value), na.rm = T)) %>% ungroup() %>% 
  slice_min(mean_value, prop = 0.1) ->df2

dfr <- rbind(df1,df2)

df %>% 
  filter(Item == interesting )%>% 
  group_by(Area, Unit) %>% 
  summarise(mean_value = mean(as.double(Value), na.rm = T)) %>% ungroup() ->dfw

dfw %>% 
  ggplot(aes(mean_value)) +
  geom_rect(aes(xmin = 44, xmax = 78, ymin = 0, ymax = Inf, fill = "red"), alpha = 0.1)+
  geom_density() +
  guides(fill = "none")
  


