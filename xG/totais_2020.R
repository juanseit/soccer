library(readxl)
library(tidyverse)
library(ggtext)
library(ggrepel)

df_538 <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv")

mandante <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team1) %>%
  summarize(xg_casa = sum(xg1, na.rm = T), xga_casa = sum(xg2, na.rm = T)) %>%
  ungroup()

visitante <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team2) %>%
  summarize(xg_fora = sum(xg2, na.rm = T), xga_fora = sum(xg1, na.rm = T)) %>%
  ungroup()

mandante %>%
  inner_join(visitante, by = c("team1" = "team2")) %>%
  group_by(team1) %>%
  summarize(total_xg = (xg_casa + xg_fora)/38, total_xga = (xga_casa + xga_fora)/38) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = total_xg, y = total_xga)) +
  geom_point(color = "white", fill = "red", pch = 21, size = 6) +
  geom_text_repel(aes(label = team1), color = "white") +
  labs(x = "xG/Partida.",
       y = "xGA/Partida.",
       title = "Desempenho esperado do Brasileirão 2020/21.",
       subtitle = "Como defesas e ataques performaram durante o campeonato.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("total_xg.png", width = 11, height = 9, dpi = 300)

mandante_nsxg <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team1) %>%
  summarize(xg_casa = sum(nsxg1, na.rm = T), xga_casa = sum(nsxg2, na.rm = T)) %>%
  ungroup()

visitante_nsxg <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team2) %>%
  summarize(xg_fora = sum(nsxg2, na.rm = T), xga_fora = sum(nsxg1, na.rm = T)) %>%
  ungroup()

mandante_nsxg %>%
  inner_join(visitante_nsxg, by = c("team1" = "team2")) %>%
  group_by(team1) %>%
  summarize(total_nsxg = (xg_casa + xg_fora)/38, total_nsxga = (xga_casa + xga_fora)/38) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = total_nsxg, y = total_nsxga)) +
  geom_point(color = "white", fill = "red", pch = 21, size = 6) +
  geom_text_repel(aes(label = team1), color = "white") +
  labs(x = "NSxG/Partida.",
       y = "NSxGA/Partida.",
       title = "Desempenho esperado do Brasileirão 2020/21.",
       subtitle = "Non-Shot xG inclui todo valor gerado menos finalizações.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("total_nsxg.png", width = 11, height = 9, dpi = 300)
