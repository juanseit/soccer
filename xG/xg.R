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


  


  
