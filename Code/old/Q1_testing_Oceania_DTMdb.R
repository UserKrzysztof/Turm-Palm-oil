library(dplyr)
library(tidyr)
library(ggplot2)

options(scipen = 12)

#Exploring database
df <- read.csv("Trade_DetailedTradeMatrix_E_Oceania.csv")
str(df)
View(head(df,10))

df %>% 
  filter(Item == "Palm oil") %>% 
  head(10) %>% 
  View()

df %>% 
  filter(Element == "Import Value" & Item == "Palm oil") -> tmp
unique(tmp$Unit)

df %>% 
  filter(Element == "Export Quantity" & Item == "Palm oil") -> tmp
unique(tmp$Unit)

df %>% 
  filter(Item == "Palm oil" & 
           ((Reporter.Countries == "Australia" & Partner.Countries == "Bahrain") |
              (Reporter.Countries == "Bahrain" & Partner.Countries == "Australia")) ) %>% 
  head(10) %>% 
  View()

df %>% 
  filter(Item == "Palm oil" & 
           ((Reporter.Countries == "Australia" & Partner.Countries == "Bahrain") |
              (Reporter.Countries == "Bahrain" & Partner.Countries == "Australia")) ) %>% 
  head(10) %>% 
  View()
  

#Preparing data
df %>% 
  filter(Item == "Palm oil") %>% 
  select(!(starts_with("Y") & ends_with("F")) & !ends_with("Code")) %>% 
  relocate(where(is.numeric), .after = where(is.character)) %>% 
  group_by(Reporter.Countries,Element) %>% 
  summarise_if(is.numeric, sum,  na.rm = T) -> gr_by_rc_df

gr_by_rc_df %>% 
  pivot_longer(Y1986:Y2021, names_to = "year") %>% 
  group_by(year, Element) %>% 
  summarise(total = sum(value, na.rm =T)) %>% 
  pivot_wider(names_from = Element, values_from = total, values_fill = 0) -> year_ie_df

gr_by_rc_df %>% 
  pivot_longer(Y1986:Y2021, names_to = "year") %>% 
  group_by(year, Element) %>% 
  summarise(total = sum(value, na.rm =T)) %>% 
  mutate(continent = "Ocenania") -> longer_year_ie_df

year_ie_df %>% 
  ggplot(aes(x = as.factor(year), y = `Import Quantity`)) +
  geom_point()

longer_year_ie_df %>% 
  filter(Element == "Import Quantity" | Element == "Export Quantity") %>% 
  ggplot(aes(year, total, color = Element)) +
  geom_point() +
  coord_flip() +
  labs(title = "Quantity of imported and exported palm oil over the years",
       subtitle = "Oceania, unit: tonnes")

