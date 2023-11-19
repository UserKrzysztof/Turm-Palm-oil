library(tidyverse)
library(Cairo)

Cairo::CairoPNG("nutrients_circular_final1.png", width = 1400, height = 1200, bg = "transparent")

nutrients <- read.csv("C:\\Users\\Ola\\Desktop\\Studia PW\\Semestr 3\\Techniki wizualizacji danych\\Projekty\\Projekt 1\\Dane\\nutrients_in_oils2.csv")
nutrients2 <- nutrients %>% 
  select(Oil.type, Monounsaturated.fatty.acids, Polyunsaturated.fatty.acids, Saturated.fatty.acids)
nutrients2$Oil.type <- c("Palm oil\n(refined)", "Palm oil\n(hydrogenated)", "Rapeseed\noil", "Olive\noil", "Sunflower\noil", "Coconut\noil")

nutrients_long2 <- nutrients2 %>% 
  rename("Monounsaturated" = Monounsaturated.fatty.acids, "Polyunsaturated" = Polyunsaturated.fatty.acids, "Saturated" = Saturated.fatty.acids) %>% 
  pivot_longer(cols = -Oil.type, names_to = "Fatty.acids", values_to = "grams")

list <- nutrients_long2 %>%
  group_by(Oil.type) %>%
  group_split()

for (i in 1:length(list)) {
  df_name <- paste("df", i, sep = "")
  assign(df_name, list[[i]])
}

empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar, ncol(nutrients_long2)))
colnames(to_add) <- colnames(nutrients_long2)
nutrients_with_na <- rbind(df4, to_add, df3, to_add, df5, to_add, df2, to_add, df6, to_add, df1, to_add)
nutrients_with_na$id <- seq(1, nrow(nutrients_with_na))

label_data <- nutrients_with_na
angle <-  90 - 360 * (label_data$id-0.5) / nrow(label_data)
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

base_data <- nutrients_with_na %>%
  group_by(Oil.type) %>% 
  summarize(start=min(id)-0.5, end=start+3) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>% 
  filter(!is.na(Oil.type))

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


### WYKRES ###

ggplot(nutrients_with_na, aes(x=as.factor(id), y=grams, fill = Fatty.acids)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), linetype = "dashed", colour = "#606060", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), linetype = "dashed", colour = "#606060", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), linetype = "dashed", colour = "#606060", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), linetype = "dashed", colour = "#606060", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(nutrients_with_na$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=5 , angle=0, fontface="bold", hjust=0.3) +
  geom_bar(stat="identity") +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    text = element_text(color = "white"),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color = NA),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(2, 4), "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, family = "bold")
  ) +
  coord_polar() +
  geom_text(aes(x=id, y=grams+14, label=grams), 
            color="white", fontface="bold", size=6, 
            inherit.aes = FALSE) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "white", alpha=0.9, size=0.8 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Oil.type), hjust = c(0.15,0.4,0.9,0.68,0.7,0.17), vjust = c(0.7,0.3,0.5,0.9,0.25,0.5), colour = "white", alpha= 1.2, size=5.5, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#FF9830", "#FFFC30", "#ff3300")) +         # coco (-p)(-g), olive, hydro, ref, rape, sun
  labs(fill = "Fatty acids:\n  (g/100g)") +
  theme(plot.title = element_text(size = 25, hjust = 0.51, family = "bold"))

dev.off()
