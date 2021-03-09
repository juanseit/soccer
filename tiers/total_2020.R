library(readxl)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(ggimage)

df_538 <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv")
team_logo_brasileiro <- read_xlsx("C:/Users/User/Documents/team_logo_brasileiro.xlsx")

mandante <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team1) %>%
  summarize(xg_casa = sum(xg1, na.rm = T), xga_casa = sum(xg2, na.rm = T)) %>%
  ungroup() %>%
  left_join(team_logo_brasileiro, by = c("team1"))

visitante <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team2) %>%
  summarize(xg_fora = sum(xg2, na.rm = T), xga_fora = sum(xg1, na.rm = T)) %>%
  ungroup() %>%
  left_join(team_logo_brasileiro, by = c("team2"))

geral_xg <- mandante %>%
  inner_join(visitante, by = c("team1" = "team2")) %>%
  group_by(team1) %>%
  summarize(total_xg = (xg_casa + xg_fora)/38, total_xga = (xga_casa + xga_fora)/38) %>%
  ungroup()
  
geral_xg %>%
  ggplot(mapping = aes(x = total_xg, y = total_xga)) +
  geom_hline(yintercept = mean(geral_xg$total_xg), color = "white", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(geral_xg$total_xga), color = "white", linetype = "dashed", size = 1) +
  geom_image(aes(image = mandante$team_logo_wikipedia), asp = 16 / 11) +
  labs(x = "xG/Partida.",
       y = "xGA/Partida.",
       title = "Desempenho esperado do Brasileirão 2020/21.",
       subtitle = "Como defesas e ataques performaram durante o campeonato.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("total_xg_logo.png", width = 12, height = 9, dpi = 300)

mandante_nsxg <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team1) %>%
  summarize(xg_casa = sum(nsxg1, na.rm = T), xga_casa = sum(nsxg2, na.rm = T)) %>%
  ungroup() %>%
  left_join(team_logo_brasileiro, by = c("team1"))

visitante_nsxg <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1)) %>%
  group_by(team2) %>%
  summarize(xg_fora = sum(nsxg2, na.rm = T), xga_fora = sum(nsxg1, na.rm = T)) %>%
  ungroup() %>%
  left_join(team_logo_brasileiro, by = c("team2"))


geral_nsxg <- mandante_nsxg %>%
  inner_join(visitante_nsxg, by = c("team1" = "team2")) %>%
  group_by(team1) %>%
  summarize(total_nsxg = (xg_casa + xg_fora)/38, total_nsxga = (xga_casa + xga_fora)/38) %>%
  ungroup()

geral_nsxg %>%
  ggplot(mapping = aes(x = total_nsxg, y = total_nsxga)) +
  geom_hline(yintercept = mean(geral_nsxg$total_nsxg), color = "white", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(geral_nsxg$total_nsxga), color = "white", linetype = "dashed", size = 1) +
  geom_image(aes(image = mandante_nsxg$team_logo_wikipedia), asp = 16 / 11) +
  labs(x = "NSxG/Partida.",
       y = "NSxGA/Partida.",
       title = "Desempenho esperado do Brasileirão 2020/21.",
       subtitle = "Non-Shot xG inclui todo valor gerado menos finalizações.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("total_nsxg_logo.png", width = 12, height = 9, dpi = 300)
