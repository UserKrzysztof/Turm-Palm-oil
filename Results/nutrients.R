library(ggplot2)
library(dplyr)
library(tidyr)

nutrients <- read.csv("C:\\Users\\Ola\\Desktop\\Studia PW\\Semestr 3\\Techniki wizualizacji danych\\nutrients_in_oils1.csv")

nutrients %>% 
  select(Oil.type, Monounsaturated.fatty.acids) %>%
  ggplot(aes(x = reorder(Oil.type, Monounsaturated.fatty.acids), y = Monounsaturated.fatty.acids)) +
  geom_bar(fill = "orange", stat = "identity", alpha = 0.6) +
  labs(title = "Monounsaturated fatty acid content",
       subtitle = "(g/100 g)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_polar()

nutrients %>% 
  select(Oil.type, Polyunsaturated.fatty.acids) %>%
  ggplot(aes(x = reorder(Oil.type, Polyunsaturated.fatty.acids), y = Polyunsaturated.fatty.acids)) +
  geom_bar(fill = "orange", stat = "identity", alpha = 0.6) +
  labs(title = "Polyunsaturated fatty acid content",
       subtitle = "(g/100 g)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_polar()

nutrients %>% 
  select(Oil.type, Saturated.fatty.acids) %>%
  ggplot(aes(x = reorder(Oil.type, Saturated.fatty.acids), y = Saturated.fatty.acids)) +
  geom_bar(fill = "orange", stat = "identity", alpha = 0.6) +
  labs(title = "Saturated fatty acid content",
       subtitle = "(g/100 g)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_polar()
