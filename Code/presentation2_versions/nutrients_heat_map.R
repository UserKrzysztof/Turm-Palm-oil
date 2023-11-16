library(dplyr)
library(tidyr)
library(ggplot2)

nutrients <- read.csv("nutrients_in_oils2.csv")

nutrients_long <- nutrients %>% 
  rename("Monounsaturated\nfatty acids" = Monounsaturated.fatty.acids, "Polyunsaturated\nfatty acids" = Polyunsaturated.fatty.acids, "Saturated\nfatty acids" = Saturated.fatty.acids, "Omega-3 acids" = Omega.3.acids, "Vitamin E" = Vitamin.E) %>% 
  pivot_longer(cols = -Oil.type, names_to = "Nutrients", values_to = "grams")


### Utworzenie oddzielnych ramek danych dla kwasów tłuszczowych, kwasów omega-3 i witaminy E ###

fatty_acids <- nutrients_long %>% 
  filter(Nutrients != "Omega-3 acids" & Nutrients != "Vitamin E")

omega_3 <- nutrients_long %>% 
  filter(Nutrients == "Omega-3 acids")

vitamin_e <- nutrients_long %>% 
  filter(Nutrients == "Vitamin E") %>% 
  mutate(grams = grams*1000) %>% 
  rename("milligrams" = grams)


### Ustawienie kolejności typów oleju na wykresach ###
# Poprawna kolejność: "Coconut oil", "Sunflower oil", "Olive oil", "Rapeseed oil", "Palm oil (refined)", "Palm oil (hydrogenated)"

fatty_acids$Oil.type <- as.factor(fatty_acids$Oil.type)
levels(fatty_acids$Oil.type)
fatty_acids$Oil.type <- factor(fatty_acids$Oil.type, levels = levels(fatty_acids$Oil.type)[c(1, 6, 2, 5, 4, 3)])

omega_3$Oil.type <- as.factor(omega_3$Oil.type)
levels(omega_3$Oil.type)
omega_3$Oil.type <- factor(omega_3$Oil.type, levels = levels(omega_3$Oil.type)[c(1, 6, 2, 5, 4, 3)])

vitamin_e$Oil.type <- as.factor(vitamin_e$Oil.type)
levels(vitamin_e$Oil.type)
vitamin_e$Oil.type <- factor(vitamin_e$Oil.type, levels = levels(vitamin_e$Oil.type)[c(1, 6, 2, 5, 4, 3)])


### WYKRESY ###

# Heat mapa: zawartość kwasów tłuszczowych (3 kolumny) w g/100g
w1 <- ggplot(fatty_acids, aes(x = Nutrients, y = Oil.type, fill=grams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(color = "black", fill = "black", linewidth = 0),
        legend.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white", size = 15),
        axis.ticks = element_line(color = "white", linewidth = 1.5),
        legend.title = element_text(color = "white", size = 12, vjust = 0.9),
        legend.text = element_text(color = "white", size = 13),
        legend.position = "top",
        legend.direction = "horizontal",
        strip.clip = "on",
        plot.margin = margin(t = 1, r = -1, b = 1, l = 1, unit = "cm")) +
  labs(fill = "g/100g") +
  scale_fill_gradientn(colours = c("white", "#FFF064", "#FF4D00", "#330000"), limits = c(0,89), values = c(0, 0.12, 0.4, 1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
w1

# Heat mapa: zawartość kwasów omega-3 (1 kolumna) w g/100g
w2 <- ggplot(omega_3, aes(x = Nutrients, y = Oil.type, fill=grams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(color = "black", fill = "black", linewidth = 0),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white", size = 15),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "white", linewidth = 1.5),
        axis.ticks.y = element_blank(),
        legend.title = element_text(color = "white", size = 12, vjust = 0.9),
        legend.text = element_text(color = "white", size = 13),
        legend.position = "top",
        legend.direction = "horizontal",
        strip.clip = "on",
        plot.margin = margin(t = 1, r = -1, b = 1, l = -1, unit = "cm")) +
  labs(fill = "g/100g") +
  scale_fill_gradientn(colours = c("white", "#6AE474", "#003E05"), limits = c(0, 7.45), values = c(0, 0.12, 1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
w2

# Heat mapa: zawartość witaminy E (1 kolumna) w mg/100g
w3 <- ggplot(vitamin_e, aes(x = Nutrients, y = Oil.type, fill=milligrams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(color = "black", fill = "black", linewidth = 0),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white", size = 15),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "white", linewidth = 1.5),
        axis.ticks.y = element_blank(),
        legend.title = element_text(color = "white", size = 12, vjust = 0.9),
        legend.text = element_text(color = "white", size = 13),
        legend.position = "top",
        legend.direction = "horizontal",
        strip.clip = "on",
        plot.margin = margin(t = 1, r = 1, b = 1, l = -1, unit = "cm")) +
  labs(fill = "mg/100g") +
  scale_fill_gradientn(colours = c("white", "#F599FF", "#AC3EB8", "#250029"), limits = c(0, 41.10), values = c(0, 0.13, 0.42, 1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
w3

# Połączenie trzech wykresów w jeden
final_plot <- (w1 + w2 + w3) + patchwork::plot_layout(widths = c(0.7, 0.22, 0.22)) +
  labs(title = "Nutrients content by oil type") +
  theme(plot.title = element_text(hjust = 12, vjust = 6, color = "white", size = 17))
final_plot

