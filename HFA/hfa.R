library(tidyverse)
library(readxl)
library(lme4)
library(ggimage)
library(ggtext)
options(scipen = 9999)

df_hfa <- read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")

home_2020 <- df_hfa %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1), !is.na(nsxg1), !is.na(nsxg2),!is.na(score1),!is.na(score2)) %>%
  group_by(team1) %>%
  summarize(nsxga_home = sum(nsxg2), nsxgf_home = sum(nsxg1), xgf_home = sum(xg1), gt_home = sum(score1), xga_home = sum(xg2), gat_home = sum(score2),
            diff_score = gt_home - gat_home) %>%
  ungroup()

away_2020 <- df_hfa %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(xg2), !is.na(xg1), !is.na(nsxg1), !is.na(nsxg2),!is.na(score1),!is.na(score2)) %>%
  group_by(team2) %>%
  summarize(nsxga_away = sum(nsxg1), nsxgf_away = sum(nsxg2), xgf_away = sum(xg2), gt_away = sum(score2), xga_away = sum(xg1), gat_way = sum(score1)) %>%
  ungroup()

total_2020 <- home_2020 %>%
  inner_join(away_2020, by=c("team1"="team2")) %>%
  mutate(total_xg=xgf_home+xgf_away, total_xga=xga_home+xga_away, diff_xg_medio=total_xg-total_xga, total_nsxgf=nsxgf_home+nsxgf_away,total_nsxga=nsxga_home+nsxga_away,
         diff_nsxg_medio = total_nsxgf-total_nsxga)

jogos_2020 <- df_hfa %>%
  filter(season == 2020, league == "Brasileiro Série A", !is.na(date)) %>%
  mutate(game_id = row_number())%>%
  group_by(game_id,team1,team2) %>%
  summarize(diff = score1 - score2, spi_mandante = spi1, spi_visitante = spi2) %>%
  ungroup() %>%
  write.xlsx(file = "jogos_2020.xlsx",
             sheetName = "jogos_2020", append = FALSE)

#remontando os dados no excel

df_2020 <- read_xlsx("C:/Users/User/Desktop/Arquivos R/Futebol/jogos_2020.xlsx")

reg_hfa <- df_2020 %>%
  lm(diff ~ spi_1 + spi_2 + home, data=.)

#tabela

library(stargazer)

stargazer(reg_hfa,
          type = "html",
          out = "reg_hfa.doc",
          notes = "Feito por Juan Iturvide.",
          dep.var.labels = c("Diferença entre gols marcados e sofridos."),
          covariate.labels = c("SPI do time.",
                               "SPI do adversário.",
                               "Mando de campo.",
                               "Constante."),
          header = FALSE)


reg_lme <- df_2020 %>%
  lmer(diff ~ spi_1 + spi_2 + home + (0+home|team1), data = .)

ha1 <- ranef(reg_lme, condVar=TRUE)

logos <- read_xlsx("C:/Users/User/Documents/team_logo_brasileiro.xlsx")

df <- as.data.frame(ha1)

plot <- df %>% left_join(logos, by = c("grp" = "team1"))
view(plot)

plot %>%
  ggplot(mapping = aes(x = reorder(grp,condval),y=condval)) +
  geom_linerange(size=.75,color='gray30', aes(ymin=(condval - 0.6603464 * 0.110800),
                                              ymax=(condval + 0.6603464 * 0.110800)))+
  geom_image(aes(image=team_logo_wikipedia),size=.035, asp=1) +
  coord_flip() +
  labs(title = "Os melhores mandantes do campeonato brasileiro em 2020.",
       caption = "Gráfico: Juan Iturvide | Data by FiveThirtyEight.",
       y = "Vantagem de mando de campo.") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5,size=10,color='gray20',face = "bold"),
        plot.subtitle = element_text(hjust=0.5,size=8,color='gray20'),
        axis.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =  element_text(hjust=0.5,size=7,color='gray20'),
        plot.caption = element_text(size=7,color='gray20'),
        legend.position = 'right',
        legend.title = element_text(size=7,color='gray20'),
        legend.text = element_text(size=7,color='gray20'))

ggsave("mando_br20.png", dpi = 300)
