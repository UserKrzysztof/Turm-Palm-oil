library(dplyr)
library(tidyr)
library(ggplot2)

nutrients <- read.csv("C:\\Users\\Ola\\Desktop\\Studia PW\\Semestr 3\\Techniki wizualizacji danych\\Projekty\\Projekt 1\\Dane\\nutrients_in_oils2.csv")

nutrients_long <- nutrients %>% 
  rename("Monounsaturated\nfatty acids" = Monounsaturated.fatty.acids, "Polyunsaturated\nfatty acids" = Polyunsaturated.fatty.acids, "Saturated\nfatty acids" = Saturated.fatty.acids, "Omega-3 acids" = Omega.3.acids, "Vitamin E" = Vitamin.E) %>% 
  pivot_longer(cols = -Oil.type, names_to = "Nutrients", values_to = "grams")

fatty_acids <- nutrients_long %>% 
  filter(Nutrients != "Omega-3 acids" & Nutrients != "Vitamin E")

omega_3 <- nutrients_long %>% 
  filter(Nutrients == "Omega-3 acids")

vitamin_e <- nutrients_long %>% 
  filter(Nutrients == "Vitamin E") %>% 
  mutate(grams = grams*1000) %>% 
  rename("milligrams" = grams)

fatty_acids$Oil.type <- as.factor(fatty_acids$Oil.type)
levels(fatty_acids$Oil.type)
fatty_acids$Oil.type <- factor(fatty_acids$Oil.type, levels = levels(fatty_acids$Oil.type)[c(1, 5, 4, 2, 6, 3)])

omega_3$Oil.type <- as.factor(omega_3$Oil.type)
levels(omega_3$Oil.type)
omega_3$Oil.type <- factor(omega_3$Oil.type, levels = levels(omega_3$Oil.type)[c(1, 3, 5, 6, 4, 2)])

vitamin_e$Oil.type <- as.factor(vitamin_e$Oil.type)
levels(vitamin_e$Oil.type)
vitamin_e$Oil.type <- factor(vitamin_e$Oil.type, levels = levels(vitamin_e$Oil.type)[c(1, 3, 5, 6, 4, 2)])

mean(omega_3$grams)
# median = 17.155
# mean = 31.67556

# fatty_acids$Nutrients <- as.factor(fatty_acids$Nutrients)
# levels(fatty_acids$Nutrients)
# fatty_acids$Nutrients <- factor(fatty_acids$Nutrients, levels = levels(fatty_acids$Nutrients)[c(1, 3, 4, 2, 5)])

p1 <- ggplot(fatty_acids, aes(x = Nutrients, y = Oil.type, fill=grams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white", size = 15),
        axis.ticks = element_line(color = "white", linewidth = 1.5),
        legend.title = element_text(color = "white", size = 15),
        legend.text = element_text(color = "white", size = 15)) +
  labs(fill = "g/100g") +
  #  scale_fill_gradient2(low="#FFF064", high="#330000", mid = "#FF4D00", limits = c(0, 89), midpoint = 35) +
  scale_fill_gradientn(colours = c("white", "#FFF064", "#FF4D00", "#330000"), limits = c(0,89), values = c(0, 0.12, 0.4, 1)) +
  scale_x_discrete(expand = c(0.174,0.174)) +
  scale_y_discrete(expand = c(0.0895,0.0895))
p1

p2 <- ggplot(omega_3, aes(x = Nutrients, y = Oil.type, fill=grams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white", size = 15),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "white", linewidth = 1.5),
        axis.ticks.y = element_blank(),
        legend.title = element_text(color = "white", size = 15),
        legend.text = element_text(color = "white", size = 15)) +
  labs(fill = "g/100g") +
  scale_fill_gradientn(colours = c("white", "#33FF33", "#006600"), limits = c(0, 7.45), values = c(0, 0.1, 1)) +
  scale_x_discrete(expand = c(0.26,0.26)) +
  scale_y_discrete(expand = c(0.0895,0.0895))
p2

p3 <- ggplot(vitamin_e, aes(x = Nutrients, y = Oil.type, fill=milligrams)) + 
  geom_tile() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white", size = 15),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "white", linewidth = 1.5),
        axis.ticks.y = element_blank(),
        legend.title = element_text(color = "white", size = 15),
        legend.text = element_text(color = "white", size = 15)) +
  labs(fill = "mg/100g") +
  scale_fill_gradientn(colours = c("white", "#FF00D5", "#660033"), limits = c(0, 41.10), values = c(0, 0.34, 1)) +
  scale_x_discrete(expand = c(0.26,0.26)) +
  scale_y_discrete(expand = c(0.0895,0.0895))
p3

final_plot <- (p1 + p2 + p3) + patchwork::plot_layout(widths = c(0.7, 0.2, 0.2)) +
  labs(title = "Nutrients content by oil type") +
  theme(plot.title = element_text(hjust = 29.5, size = 17))
final_plot
