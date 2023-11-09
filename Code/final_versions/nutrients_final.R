library(tidyverse)

nutrients <- read.csv("C:\\Users\\Ola\\Desktop\\Studia PW\\Semestr 3\\Techniki wizualizacji danych\\nutrients_in_oils1.csv")

nutrients_long <- nutrients %>% 
  rename("Monounsaturated" = Monounsaturated.fatty.acids, "Polyunsaturated" = Polyunsaturated.fatty.acids, "Saturated" = Saturated.fatty.acids) %>% 
  pivot_longer(cols = -Oil.type, names_to = "Fatty.acids", values_to = "grams")

data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

list <- nutrients_long %>%
  group_by(Oil.type) %>%
  group_split()

for (i in 1:length(list)) {
  df_name <- paste("df", i, sep = "")
  assign(df_name, list[[i]])
}

empty_bar <- 3
to_add <- data.frame(matrix(NA, empty_bar, ncol(nutrients_long)))
colnames(to_add) <- colnames(nutrients_long)
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


### WYKRESY ###

# Wykres p1: legenda, wartości nad słupkami obrócone, skala do 80
p1 <- ggplot(nutrients_with_na, aes(x=as.factor(id), y=grams, fill = Fatty.acids)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(nutrients_with_na$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=grams+5, label=grams, hjust = hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.6, angle= label_data$angle, 
            inherit.aes = FALSE) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Oil.type), hjust = c(0.2,0.4,0.92,0.8,0.85,0.15), colour = "black", alpha=0.8, size=3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#F4A460", "brown", "red")) +
  labs(title = "The content of fatty acids by oil type (g/100g)",
       fill = "Fatty acids") +
  theme(plot.title = element_text(vjust = -20, hjust = 0.51))
p1

# Wykres p2: legenda, brak etykiet nad słupkami, skala do 80
p2 <- ggplot(nutrients_with_na, aes(x=as.factor(id), y=grams, fill = Fatty.acids)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(nutrients_with_na$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Oil.type), hjust = c(0.2,0.4,0.92,0.8,0.85,0.15), colour = "black", alpha=0.8, size=3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#F4A460", "brown", "red")) +
  labs(title = "The content of fatty acids by oil type (g/100g)",
       fill = "Fatty acids") +
  theme(plot.title = element_text(vjust = -20, hjust = 0.51))
p2

# Wykres p3: brak legendy, etykiety nad słupkami - nazwy kwasów, skala do 100
p3 <- ggplot(nutrients_with_na, aes(x=as.factor(id), y=grams, fill = Fatty.acids)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(nutrients_with_na$id),5), y = c(20, 40, 60, 80, 100), label = c("20", "40", "60", "80", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=grams+5, label=Fatty.acids, hjust = hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, 
            inherit.aes = FALSE) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Oil.type), hjust = c(0.2,0.4,0.92,0.8,0.85,0.15), colour = "black", alpha=0.8, size=3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#F4A460", "brown", "red")) +
  labs(title = "The content of fatty acids by oil type (g/100g)") +
  theme(plot.title = element_text(size = 13, vjust = -16, hjust = 0.51))
p3

# Wykres p4: legenda, wartości nad słupkami w poziomie, skala do 80
p4 <- ggplot(nutrients_with_na, aes(x=as.factor(id), y=grams, fill = Fatty.acids)) +
  geom_bar(stat="identity", alpha=0.8) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(nutrients_with_na$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(aes(x=id, y=grams+12, label=grams), 
            color="black", fontface="bold",alpha=0.6, size=2.6, 
            inherit.aes = FALSE) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=Oil.type), hjust = c(0.2,0.4,0.92,0.8,0.85,0.15), colour = "black", alpha=0.8, size=3, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("#F4A460", "brown", "red")) +
  labs(title = "The content of fatty acids by oil type (g/100g)",
       fill = "Fatty acids") +
  theme(plot.title = element_text(vjust = -20, hjust = 0.51))
p4
