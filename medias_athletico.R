library(tidyverse)
library(ggtext)
library(readxl)
library(slider)

df_cap <- read_xlsx("C:/Users/User/Documents/cap.xlsx")


df_cap %>%
  mutate(mov_xg = slide_dbl(media_xg, mean, .before = 10, complete = T), mov_xga = slide_dbl(media_xga, mean, .before = 10, complete = T)) %>%
  ggplot() +
  geom_vline(xintercept = 6, linetype = "dashed", color = "white") +
  geom_vline(xintercept = 17, linetype = "dashed", color = "white") +
  geom_line(mapping = aes(x = rodadas, y = media_xg), color = "#00CCFF") +
  geom_point(mapping = aes(x = rodadas, y = media_xg), color = "#00CCFF") +
  geom_line(mapping = aes(x = rodadas, y = media_xga), color = "red") +
  geom_point(mapping = aes(x = rodadas, y = media_xga), color = "red") +
  geom_line(mapping = aes(x = rodadas, y = mov_xg), color = "#00CCFF", linetype = "dashed") +
  geom_line(mapping = aes(x = rodadas, y = mov_xga), color = "red", linetype = "dashed") +
  geom_text(x=2.5, y=1.4, label="Dorival Jr.", color = "white", family = "Century Gothic") +
  geom_text(x=11.5, y=0.75, label="Eduardo Barros", color = "white", family = "Century Gothic") +
  geom_text(x=22.5, y=0.75, label="Paulo Autuori", color = "white", family = "Century Gothic") +
  labs(title = "Médias de <span style = 'color:#00CCFF;'>xG</span> e <span style = 'color:red;'>xGA</span> do Athletico.",
       subtitle = "Brasileirão 2020/21. Médias móveis de 10 jogos.",
       x = "Rodadas.",
       y = "Médias de xG e xGA por jogo.",
       caption = "Gráfico: @juanseit_ | Data by Infogol and Fivethirtyeight") +
  theme_minimal() +
  theme(legend.position = "right", 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#141622"),
        panel.background = element_rect(fill = "#141622", 
                                        colour = "#141622",
                                        size = 2, 
                                        linetype = "dashed"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'dashed',
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
        plot.title = element_markdown(face = "bold", 
                                      colour = "white", 
                                      size = 15, 
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
                                   size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



