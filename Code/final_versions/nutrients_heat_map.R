library(tidyverse)
library(hrbrthemes)

nutrients <- read.csv("C:\\Users\\Ola\\Desktop\\Studia PW\\Semestr 3\\Techniki wizualizacji danych\\Projekty\\Projekt 1\\Dane\\nutrients_in_oils2.csv")

nutrients_long <- nutrients %>% 
  rename("Monounsaturated fatty acids" = Monounsaturated.fatty.acids, "Polyunsaturated fatty acids" = Polyunsaturated.fatty.acids, "Saturated fatty acids" = Saturated.fatty.acids, "Omega-3 acids" = Omega.3.acids, "Vitamin E" = Vitamin.E) %>% 
  pivot_longer(cols = -Oil.type, names_to = "Nutrients", values_to = "grams")

ggplot(nutrients_long, aes(x = Nutrients, y = Oil.type, fill=grams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.ticks = element_line(color = "white"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")) +
  labs(fill = "g/100g") +
  scale_fill_gradient2(low="#603909", high="yellow", mid = "#ff7b00", limits = c(0, 89), midpoint = 44.5)

