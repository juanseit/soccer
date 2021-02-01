library(tidyverse)
library(ggimage)
library(zoo)
library(ggtext)

df <- data.frame(rodadas = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33),
                 xG = c(2.4,2.19,1.46,0.77,0.82,0.92,1.18,1.19,2.82,1.01,1.24,1.13,0.66,2.52,1.18,1.27,0.29,0.65,2.35,2.01,0.65,1.61,0.7,0.44,1.21,0.64,1.98,1.98,0.04,1.24,0.67,1.89,1.28),
                 xGA = c(0.53,1.79,2.04,0.58,0.82,1.23,1.34,1.04,2.19,0.1,1.7,2.32,0.6,2.14,1.54,1.27,1.07,1.77,1.44,0.69,0.88,0.54,3.56,4.11,1.4,1.33,1.23,2.8,0.44,0.6,0.67,1.1,0.68))

df %>%
  group_by(rodadas) %>%
  summarize(media_xg = cumsum(xG)/length(rodadas), media_xga = cumsum(xGA)/length(rodadas)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = rodadas, y = media_xg), color = "#00CCFF") +
  geom_point(mapping = aes(x = rodadas, y = media_xg), color = "#00CCFF") +
  geom_line(mapping = aes(x = rodadas, y = media_xga), color = "#FF00FF") +
  geom_point(mapping = aes(x = rodadas, y = media_xga), color = "#FF00FF") +
  labs(title = "Médias de xG e xGA do Athletico ao longo do Brasileirão 2020/21.",
       subtitle = "Médias móveis de 3 rodadas.",
       x = "Rodadas.",
       y = "Médias de xG e xGA por jogo.",
       caption = "Gráfico: @juanseit_ | Data by Infogol") +
  theme_minimal() +
  theme(legend.position = "right", 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid',
                                        colour = "gray30"),
        axis.title.x = element_text(size = 13, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Century Gothic"),
        axis.title.y = element_text(size = 13, 
                                    face = "bold", 
                                    colour = "white", 
                                    family = "Century Gothic"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(face = "bold", 
                                  colour = "white", 
                                  size = 14, 
                                  family = "Century Gothic"),
        plot.subtitle = element_text(colour = "white", 
                                     family = "URWGeometricW03-Light", 
                                     size = 10),
        plot.caption = element_text(colour = "white", 
                                    family = "URWGeometricW03-Light", 
                                    size = 10),
        plot.caption.position = "plot",
        legend.title = element_text(colour = "white", 
                                    family = "URWGeometricW03-Light", 
                                    size = 14),
        legend.text = element_text(colour = "white", 
                                   family = "URWGeometricW03-Light", 
                                   size = 14))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 32))