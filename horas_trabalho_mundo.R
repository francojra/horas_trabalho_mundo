
# Horas de trabalho no mundo ---------------------------------------------------------------------------------------------------------------
# Autora do script: Jeanne Franco ----------------------------------------------------------------------------------------------------------
# Data: 25/08/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/air-pollution -------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### No mundo de agitação e confusão que vivemos, é fácil assumir que estamos
### trabalhando mais que nunca. Mas isso é verdade?

### Como poderemos ver nos dados em breve, isso não é o que está acontecendo.

### Os dados apresentam que no século 19 pessoas de todo o mundo trabalhavam
### por mais tempo, mas nos pultimos 150 anos o tempo de horas trabalhadas
### reduziu substancialmente. Particularmente, em países mais ricos.

# Pacotes necessários para as análises -----------------------------------------------------------------------------------------------------

library(tidyverse)
library(pals)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

ht <- read.csv("annual-working-hours-per-worker.csv")
view(ht)
names(ht)

# Manipulação de dados ---------------------------------------------------------------------------------------------------------------------

ht1 <- ht %>%
  select(-Code) %>%
  rename(horas = Average.annual.working.hours.per.worker) %>%
  filter(Entity %in% c("Angola", "Brazil", "China", "Russsia",
                       "United States", "France", "Germany",
                       "Ghana", "India", "Haiti", "Hungary", 
                       "Iceland", "Israel", "Portugal", "Italy")) %>%
  group_by(Entity) %>%
  summarise(media = mean(horas),
            sd = sd(horas), n = n(),
            se = sd/sqrt(n)) %>%
  view()

ht2 <- ht %>%
  select(-Code) %>%
  rename(horas = Average.annual.working.hours.per.worker) %>%
  filter(Entity %in% c("United States", "France", "Germany",
                       "Italy")) %>%
  view()
glimpse(ht2)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

ggplot(ht1, aes(x = fct_reorder(Entity, media), y = media, 
                fill = Entity)) +
  geom_col() +
  geom_errorbar(aes(x = Entity, y = media,
                    ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.3) +
  scale_fill_manual(values = as.vector(alphabet(11))) +
  coord_flip() +
  labs(x = "Países", y = "Tempo médio de trabalho (horas)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

ggplot(ht2, aes(x = Year, y = horas, group = Entity, col = Entity)) +
  geom_line(size = 1.4) +
  scale_color_manual(values = as.vector(alphabet(4))) +
  labs(x = "Anos", y = "Tempo médio de trabalho (horas)",
       color = "Países") +
  theme_bw(base_size = 14) 
