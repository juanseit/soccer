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
  geom_point(fill = "#8b0000", color = "white", size =  4, pch = 21) +
  geom_text_repel(aes(label= Equipe), colour = "white") +
  stat_smooth(method = "lm", color = "white", se = FALSE) +
  labs( x = "Expected Goals.",
        y = "Goals Marcados.",
        title = "Relação entre gols marcados e esperados em 19/20.",
        subtitle = "Utilizando times da Premier League.",
        caption = "Gráfico: @juanseit_ | Data by Statsbomb and fbref.com") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("xg_1.png", width = 11, height = 8)

#Preditivo

dados %>%
  ggplot(mapping = aes(x = xGt, y = GP)) +
  geom_point(fill = "#8b0000", color = "white", size =  4, pch = 21) +
  geom_text_repel(aes(label= Equipe), colour = "white") +
  stat_smooth(method = "lm", color = "white", se = FALSE) +
  labs( x = "Expected Goals em 2018/2019.",
        y = "Goals Marcados em 2019/2020.",
        title = "Relação entre os gols esperados em 18/19 e os marcados em 19/20.",
        subtitle = "Utilizando times da Premier League.",
        caption = "Gráfico: @juanseit_ | Data by Statsbomb and fbref.com") +
  theme_theathletic()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("xg_2.png", width = 11, height = 8)

#Reliable

dados %>%
  ggplot(mapping = aes(x = xGt, y = xG)) +
   geom_point(fill = "#8b0000", color = "white", size =  4, pch = 21) +
  geom_text_repel(aes(label= Equipe), colour = "white") +
  stat_smooth(method = "lm", color = "white", se = FALSE) +
  labs( x = "Expected Goals em 2018/2019.",
        y = "Expected Goals em 2019/2020.",
        title = "Relação entre os gols esperados em 18/19 e os em 19/20.",
        subtitle = "Utilizando times da Premier League.",
        caption = "Gráfico: @juanseit_ | Data by Statsbomb and fbref.com") +
  theme_theathletic()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("xg_3.png", width = 11, height = 8)

#gmae

reg1 <- gmae %>%
  lm(Gols ~ Pgmae, data = .) %>%
  summary()

gmae %>%
  ggplot(mapping = aes(x = Sh, y = Pgmae)) +
  geom_point(aes(size = Gols), fill = "#8b0000", color = "white", size =  4, pch = 21) +
  geom_text_repel(aes(label = Nome), colour = "white") +
  labs( x = "Finalizações.",
        y = "Porcentagem de gols marcados acima do esperado.",
        title = "Eficiência de finalizações dos jogadores do Brasileirão 2020.",
        subtitle = "Mínimo 25 chutes para ser qualificado.",
        caption = "Gráfico: @juanseit_ | Data by Infogol.com and Globoesporte.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("pmgae.png", width = 11, height = 8)

reg1 <- gmae %>%
  lm(Gols ~ Pgmae, data = .) %>%
  summary()

gmae %>%
  ggplot(mapping = aes(x = Pgmae, y = Gols)) +
  geom_point(geom_point(aes(size = Gols), fill = "#8b0000", color = "white", size =  4, pch = 21) +
  geom_text_repel(aes(label = Nome), colour = "white") + 
  geom_abline(slope = c(.5,0.75,1,1.25,1.5), intercept = 0, linetype = "dashed", color = "white") +
  labs( x = "Gols.",
        y = "Porcentagem de gols marcados acima do esperado.",
        title = "Em quanto os melhores finalizadores sobreperformaram os gols marcados.",
        subtitle = "Mínimo 25 chutes para ser qualificado.",
        caption = "Data by Infogol.com and Globoesporte.") +
  theme_theathletic(
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlim(0,15)

ggsave("pmgae_2.png",  width = 11, height = 8)
