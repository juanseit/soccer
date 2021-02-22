library(readxl)
library(tidyverse)
library(ggimage)
library(ggtext)
library(ggrepel)
library(ggalt)

df_538 <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv")
df_spi <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")

home <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1), !is.na(nsxg1), !is.na(nsxg2)) %>%
  group_by(team1) %>%
  summarize(xga = mean(xg2), xgf = mean(xg1), nsxga = mean(nsxg2), nsxgf = mean(nsxg1), diff = xgf - xga, jogos = n(), xgf_t = sum(xg1), gt = sum(score1), xga_t = sum(xg2), gat = sum(score2)) %>%
  ungroup() %>%
  arrange(-diff)

away <- df_538 %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1), !is.na(nsxg1), !is.na(nsxg2)) %>%
  group_by(team2) %>%
  summarize(xga = mean(xg1), xgf = mean(xg2), nsxga = mean(nsxg1), nsxgf = mean(nsxg2), diff = xgf - xga, jogos = n(), xga_t = sum(xg1), ga_t = sum(score1),xgf_t = sum(xg2), gt = sum(score2)) %>%
  ungroup() %>%
  arrange(-diff)

spi_index <- df_spi %>%
  filter(league == "Brasileiro Série A", !is.na(off), !is.na(def)) %>%
  group_by(name) %>%
  summarize(media_off = mean(off), media_def = mean(def), spi = spi) %>%
  ungroup() %>%
  arrange(-spi)

#melhores mandantes

home %>%
  ggplot(mapping = aes(x = xgf, y = xga)) +
  geom_point(color = "white", fill = "red", size = 5, shape = 21) +
  geom_hline(yintercept = mean(home$xga), color = "white", linetype = "dashed") +
  geom_vline(xintercept = mean(home$xgf), color = "white", linetype = "dashed") +
  geom_text_repel(aes(label=team1),colour = "white", 
                  family = "URWGeometricW03-Light") +
  labs(x = "Gols esperados a favor por jogo.",
       y = "Gols esperados contra por jogo.",
       title = "Os mandantes mais eficientes do Brasileirão 2020/21.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.",
       subtitle = "Como ataques e defesas tem performado em termos esperados.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("mandante.png",  width = 7, height = 5)

#melhores visitantes

away %>%
  ggplot(mapping = aes(x = xgf, y = xga)) +
  geom_point(color = "white", fill = "red", size = 5, shape = 21) +
  geom_hline(yintercept = mean(home$xga), color = "white", linetype = "dashed") +
  geom_vline(xintercept = mean(home$xgf), color = "white", linetype = "dashed") +
  geom_text_repel(aes(label=team2),colour = "white", 
                  family = "URWGeometricW03-Light") +
  labs(x = "Gols esperados a favor por jogo.",
       y = "Gols esperados contra por jogo.",
       title = "Os visitantes mais eficientes do Brasileirão 2020/21.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.",
       subtitle = "Como ataques e defesas tem performado em termos esperados.") +
  theme_theathletic() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  
ggsave("visitante.png", width = 7, height = 5)

home %>%
  ggplot(mapping =  aes(y=team1, x=xgf_t, xend=gt)) +
  geom_dumbbell(size=3, color="gray",
                colour_x = "#00CCFF", colour_xend = "red",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x = "Diferença na soma de Gols e xG.",
       y = "Times.",
       title = "Sorte e azar: Diferença entre <span style = 'color:#00CCFF;'>xG</span> e <span style = 'color:red;'>gols marcados</span> para os mandantes.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.",
       subtitle = "Como ataques tem performado em relação ao esperado.") +
  theme_theathletic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("diff_mandante.png", height = 12, width = 11, units = "in", dpi = 100)
  
away %>%
  ggplot(mapping =  aes(y=team2, x=xgf_t, xend=gt)) +
  geom_dumbbell(size=3, color="gray",
                colour_x = "#00CCFF", colour_xend = "red",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x = "Diferença na soma de Gols e xG.",
       y = "Times.",
       title = "Sorte e azar: Diferença entre <span style = 'color:#00CCFF;'>xG</span> e <span style = 'color:red;'>gols marcados</span> para os visitantes.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.",
       subtitle = "Como ataques tem performado em relação ao esperado.") +
  theme_theathletic()  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("diff_visitante.png", height = 12, width = 11, units = "in", dpi = 100)

spi_index %>%
  ggplot(mapping = aes(x = media_off, y = media_def)) +
  geom_point(color = "white", fill = "red", pch = 21, cex = spi_index$spi/ 5) +
  geom_text_repel(aes(label = name), color = "white") +
  geom_hline(yintercept = mean(spi_index$media_def), linetype = "dashed", color = "white") +
  geom_vline(xintercept = mean(spi_index$media_off), linetype = "dashed", color = "white") +
  labs(title = "Soccer Power Index no Brasileirão 2020/21.",
       subtitle = "Tamanho do ponto corresponde a nota global.",
       x = "Index ofensivo.",
       y = "Index defensivo.",
       caption = "Gráfico: @juanseit_ | Data by FiveThirtyEight.") +
  theme_theathletic()+
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("spi_br.png", width = 11, height = 9, dpi = 300)


