library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(stringi)
library(forcats)
library(extrafont)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(maps)
library(patchwork)

loadfonts(device = "win")



deforestarion <- read.csv("Total forest area replaced by oil palm globally (directly and indirectly), 2001-2015.csv") %>%
  mutate(Year = as.character(Year))

deforestarion %>% 
  mutate(sum = (Nonprimary.forest + Primary.forest) / 1000) -> first_graph

first_graph$Year <- factor(first_graph$Year, levels = unique(first_graph$Year))


ggplot(first_graph, aes(y = Year,
                  x = sum)) +
  geom_hline(yintercept = seq(0,11,0.5),linetype = "dotted", color = "#606060", linewidth = 0.8)+
  geom_col(fill = "#b30000", color = "#b30000") +
  scale_y_discrete(guide = guide_axis(title = "")) +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10),
                     limits = c(0, 11.2),
                     expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent'),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#606060", linewidth = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(angle = 60,
                               size = 25),
    axis.text.x = element_text(size = 18),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 1),
    axis.line = element_line(color = "#606060", linewidth = 1),
    axis.text = element_text(color = "white",
                             family = "Arial Narrow"),
    axis.title.x = element_text(size = 22),
    text = element_text(family = "Arial Narrow",
                        color = "white")) +
  labs(x = "Deforestation (million hectares)")


####################################################################################################



deforestarion %>% 
  mutate(sum = Nonprimary.forest + Primary.forest) %>% 
  summarise(sum = sum(sum) / 1000) -> second_graph

second_graph$Year = "TOTAL"

second_graph %>% 
  add_row(Year = "AREA OF\nHUNGARY", sum = 9.303) -> second_graph

second_graph$Year <- factor(second_graph$Year, levels = unique(second_graph$Year))

second_graph$Highlight <- second_graph$Year == "AREA OF\nHUNGARY"

  

ggplot(second_graph, aes(x = Year,
                  y = sum,
                  fill = Highlight)) +
  geom_hline(yintercept = seq(0,11,0.5),linetype = "dotted", color = "#303030", linewidth = 0.8)+
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "#800000", "TRUE" = "#004d1a")) +
  scale_x_discrete(guide = guide_axis(title = "",
                                      angle = 60)) +
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 10),
                     limits = c(0, 11.2),
                     expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#303030", linewidth = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 60,
                               size = 25),
    axis.text.y = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(linewidth = 1),
    axis.line = element_line(color = "#303030", linewidth = 1),
    axis.text = element_text(color = "white",
                             family = "Arial Narrow"),
    axis.title = element_blank(),
    text = element_text(family = "Arial Narrow",
                        color = "white")) -> second


#######################################################################################




(first + second) + plot_layout(widths = c(17,3)) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent")
  ))
