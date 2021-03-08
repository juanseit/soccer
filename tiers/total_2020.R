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
  ungroup()

mandante %>%
  inner_join(visitante, by = c("team1" = "team2")) %>%
  group_by(team1) %>%
  summarize(total_xg = (xg_casa + xg_fora)/38, total_xga = (xga_casa + xga_fora)/38) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = total_xg, y = total_xga)) +
  geom_image(aes(image = mandante$team_logo_wikipedia), asp = 16 / 11) +
  geom_abline(slope = -1.5, intercept = c(0,.5,1, 1.5,2,2.5,3), alpha = .4, color = "white") +
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
  ungroup() %>%
  left_join(team_logo_brasileiro, by = c("team1"))

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
  geom_image(aes(image = mandante_nsxg$team_logo_wikipedia), asp = 16 / 11) +
  geom_abline(slope = -1.5, intercept = c(0.25,0.75,1.25,1.75), alpha = .4, color = "white") +
  labs(x = "NSxG/Partida.",
       y = "NSxGA/Partida.",
       title = "Desempenho esperado do Brasileirão 2020/21.",
       subtitle = "Non-Shot xG inclui todo valor gerado menos finalizações.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("total_nsxg.png", width = 11, height = 9, dpi = 300)
