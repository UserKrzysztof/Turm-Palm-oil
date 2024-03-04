library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen = 12)

files <- c("Trade_DetailedTradeMatrix_E_Africa.csv",
           "Trade_DetailedTradeMatrix_E_Europe.csv",
           "Trade_DetailedTradeMatrix_E_Asia.csv",
           "Trade_DetailedTradeMatrix_E_Americas.csv",
           "Trade_DetailedTradeMatrix_E_Oceania.csv")
continents <- c("Africa", "Europe", "Asia", "Americas", "Oceania")

for(i in 1:length(continents)){
  df <- read.csv(files[i])
  
  df %>% 
    filter(Item == "Palm oil") %>% 
    select(!(starts_with("Y") & ends_with("F")) & !ends_with("Code")) %>% 
    relocate(where(is.numeric), .after = where(is.character)) %>% 
    group_by(Reporter.Countries,Element) %>% 
    summarise_if(is.numeric, sum,  na.rm = T) %>% 
    pivot_longer(Y1986:Y2021, names_to = "year") %>% 
    group_by(year, Element) %>% 
    summarise(total = sum(value, na.rm =T)) %>% 
    mutate(continent = continents[i]) -> df
  
  assign(continents[i], df)
  
}

World_data <- tibble(rbind(Africa,Asia,Americas,Europe,Oceania))
View(tail(ungroup(World_data),100))

#write.csv(World_data ,file = "Word_data.csv")
World_data <- read.csv("Word_data.csv")

World_data %>% 
  filter(Element == "Import Quantity" | Element == "Export Quantity") %>% 
  ungroup() %>% 
  ggplot(aes(year, total,
             color= continent, 
             group = interaction(continent,Element, sep = ":"),
             linetype = Element)) +
  geom_point()+
  geom_line()+
  coord_flip() +
  labs(title = "Quantity of imported and exported palm oil over the years",
       subtitle = "World, unit: tonnes")


#one plot for each continent
World_data %>% 
  filter(Element == "Import Quantity" | Element == "Export Quantity") %>% 
  mutate(year = substr(year,2,length(year))) %>% 
  ungroup() %>% 
  ggplot(aes(year, total,
             color= continent, 
             group = interaction(continent,Element, sep = ":"),
             linetype = Element)) +
  facet_wrap(~continent, ncol =1, scales = "free")+
  geom_point()+
  geom_line()+
  labs(title = "Quantity of imported and exported palm oil over the years",
       subtitle = "World, unit: 1000 USD",
       )+
  theme(axis.text.x = element_text(angle=90)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")), 
         color = FALSE)

