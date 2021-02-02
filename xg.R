library(tidyverse)
library(readxl)

dados <- read_xlsx("C:/Users/User/Downloads/prem1.xlsx")
gmae <- read_xlsx("C:/Users/User/Documents/gmae.xlsx")

fit <- lm(GP ~ xG, data = dados)
summary(fit)
fit_2 <- lm(GP ~ xGt, data = dados)
summary(fit_2)
fit_3 <- lm(xG ~ xGt, data = dados)
summary(fit_3)

#Descritivo

dados %>%
ggplot(mapping = aes(x = xG, y = GP)) +
  geom_point() +
  geom_smooth() +
  geom_text(mapping = aes(x = xG, y = GP, label = Equipe, vjust = -0.5, nudge_y = 0.5)) +
  geom_abline(slope = 1.244, intercept = -10.713) +
  labs( x = "Expected Goals.",
        y = "Goals Marcados.",
        title = "Relação entre gols marcados e esperados em 19/20.",
        caption = "Data by Statsbomb and fbref.com") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#Preditivo

dados %>%
  ggplot(mapping = aes(x = xGt, y = GP)) +
  geom_point() +
  geom_smooth() +
  geom_text(mapping = aes(x = xGt, y = GP, label = Equipe, vjust = -0.5, nudge_y = 0.5)) +
  geom_abline(slope = 1.396, intercept = -18.781) +
  labs( x = "Expected Goals em 2018/2019.",
        y = "Goals Marcados em 2019/2020.",
        title = "Relação entre os gols esperados em 18/19 e os marcados em 19/20.",
        caption = "Data by Statsbomb and fbref.com") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#Reliable

dados %>%
  ggplot(mapping = aes(x = xGt, y = xG)) +
  geom_point() +
  geom_smooth() +
  geom_text(mapping = aes(x = xGt, y = xG, label = Equipe, vjust = -0.5, nudge_y = 0.5)) +
  geom_abline(slope = 1.064, intercept = -3.916) +
  labs( x = "Expected Goals em 2018/2019.",
        y = "Expected Goals em 2019/2020.",
        title = "Relação entre os gols esperados em 18/19 e os em 19/20.",
        caption = "Data by Statsbomb and fbref.com") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#gmae

gmae %>%
  ggplot(mapping = aes(x = Sh, y = Pgmae)) +
  geom_point(aes(size = Gols), alpha = .6) +
  geom_text(aes(label = Nome, vjust = -1)) +
  labs( x = "Finalizações.",
        y = "Porcentagem de gols marcados acima do esperado.",
        title = "Eficiência de finalizações dos jogadores do Brasileirão 2020.",
        subtitle = "Mínimo 25 chutes para ser qualificado.",
        caption = "Data by Infogol.com and Globoesporte.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

reg1 <- gmae %>%
  lm(Gols ~ Pgmae, data = .) %>%
  summary()

gmae %>%
  ggplot(mapping = aes(x = Pgmae, y = Gols)) +
  geom_point(alpha = .6) +
  geom_abline(slope = 0.34049, intercept = 4.87250) +
  geom_text(aes(label = Nome, vjust = -1)) +
  labs( x = "Gols.",
        y = "Porcentagem de gols marcados acima do esperado.",
        title = "Teste de R quadrado.",
        subtitle = "Mínimo 25 chutes para ser qualificado.",
        caption = "Data by Infogol.com and Globoesporte.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#o que importa mais, ataque ou defesa?

totals <- read_xlsx("C:/Users/User/Downloads/prem2.xlsx")

ataque <- lm(Ptt ~ xGt, data = totals)
summary(ataque)
defesa <- lm(Ptt ~ xGAt, data = totals)
summary(defesa)

totals %>% #ataque
  ggplot(mapping = aes(x = xGt, y = Ptt)) +
  geom_smooth() +
  geom_point(alpha = .6) +
  geom_abline(slope =  1.18898, intercept = -13.40044) +
  geom_text(aes(label = Equipe, vjust = -1)) +
  labs( x = "Gols esperados marcados no total.",
        y = "Pontos totais conquistados.",
        title = "Pontos e Gols esperados acumulados na Premier League (2017/18 a 2019/20).",
        caption = "Gráfico: @juanseit_ | Data by fbref.com and Statsbomb.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

totals %>% #ataque
  ggplot(mapping = aes(x = xGAt, y = Ptt)) +
  geom_smooth() +
  geom_point(alpha = .6) +
  geom_abline(slope =  0.8229, intercept = 28.1172) +
  geom_text(aes(label = Equipe, vjust = -1)) +
  labs( x = "Gols esperados sofridos no total.",
        y = "Pontos totais conquistados.",
        title = "Pontos e Gols esperados sofridos acumulados na Premier League (2017/18 a 2019/20).",
        caption = "Gráfico: @juanseit_ | Data by fbref.com and Statsbomb.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

defesa_sem_6 <- totals %>%
  filter(!Equipe %in% c("Liverpool", "Manchester City", "Manchester Utd", "Tottenham", "Chelsea", "Arsenal")) %>%
  lm(Ptt ~ xGAt, data = .) %>%
  summary()


  


  