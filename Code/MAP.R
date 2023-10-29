library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(stringi)

map <- map_data("world")


str(map)

df <- read.csv("/Users/krzysztof/visualisation_tech/Projekt1repo/local/map/mean_fat_intake.csv")

df <- df %>% select(-X)

map %>% filter(region == "USA") %>% View()

map2 <- merge.data.frame(map,df, by.x = "region", by.y = "Area")

subset(df2, !(Area %in% map2$region)) -> excluded

df %>% mutate(Area = case_when(stri_detect_regex(Area, "China") ~ "China",
                               stri_detect_regex(Area, "Russia") ~ "Russia",
                               stri_detect_regex(Area, "United States") ~ "USA",
                               stri_detect_regex(Area, "Czech") ~ "Czech Republic",
                               stri_detect_regex(Area, "rkiye") ~ "Turkey",
                               stri_detect_regex(Area, "Moldova") ~ "Moldova",
                               stri_detect_regex(Area, "Great Britain") ~ "UK",
                               stri_detect_regex(Area, "Iran") ~ "Iran",
                               stri_detect_regex(Area, "Lao") ~ "Laos",
                               stri_detect_regex(Area, "Syria") ~ "Syria",
                               stri_detect_regex(Area, "Venezuela") ~ "Venezuela",
                               stri_detect_regex(Area, "Bolivia") ~ "Bolivia",
                               stri_detect_regex(Area, "Brunei") ~ "Brunei",
                               stri_detect_regex(Area, "Nether") ~ "Netherlands",
                               stri_detect_regex(Area, "Tanzania") ~ "Tanzania",
                               stri_detect_regex(Area, "Democratic People's Republic of Korea") ~ "North Korea",
                               stri_detect_regex(Area, "Trinidad") ~ "Trinidad",
                               stri_detect_regex(Area, "Congo") ~ "Democratic Republic of the Congo",
                               stri_detect_regex(Area, "Viet Nam") ~ "Vietnam",
                               stri_detect_regex(Area, "Republic of Korea") ~ "South Korea",
                               stri_detect_regex(Area, "Verde") ~ "Cape Verde",
                               stri_detect_regex(Area, "Kitts") ~ "Saint Kitts",
                               stri_detect_regex(Area, "Vincent") ~ "Saint Vincent",
                               stri_detect_regex(Area, "Antigua") ~ "Antigua",
                               stri_detect_regex(Area, "e d'Ivoire") ~ "Ivory Coast",
                               TRUE ~ Area)) ->df2

map2 <- merge.data.frame(map,df2, by.x = "region", by.y = "Area", all.x = T)

map2 %>% arrange(order)-> map2

ggplot(map2, aes(long, lat, map_id=region)) +
  geom_map(map = map2, aes(fill = mean_value)) +
  labs(title = "Fat intake",
       subtitle = "Unit: g/cap/day")

