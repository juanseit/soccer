library(tidyverse)
library(ggrepel)
library(ggimage)
library(readxl)

le_stat <- read_xlsx("C:/Users/User/Documents/le_bra.xlsx")

medias_le <- le_stat %>%
  group_by(Nome) %>%
  summarize(media_xa = mean(xA_90), media_desarmes = mean(p_desarmes)) %>%
  ungroup()

le_stat %>%
  ggplot(mapping = aes(x = p_desarmes, y = xA_90)) +
  geom_hline(yintercept = mean(medias_le$media_xa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(medias_le$media_desarmes), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = le_stat$Cor_1, cex=le_stat$Minutos / 300, alpha = .6) +
  geom_text_repel(aes(label=Nome)) +
  labs(x = "Percentual de desarmes bem sucedidos.",
       y = "Assistências esperadas por 90 min (xA/90).",
       title = "Eficiência dos laterais esquerdos do Brasileirão 2020/21.",
       subtitle = "Mínimo 1500 minutos para qualificar.",
       caption = "Gráfico: @juanseit_ | Data by Infogol.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))