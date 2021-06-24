library(tidyverse)
library(lme4)

D <- read_csv2("Bylot_DeadMaterials.csv")

D %>% mutate(White_prop = Pixel_white/Pixel_area,
             Traitement = factor(Traitement)) %>%
  # group_by(Parcelle, Traitement, Exclos) %>%
  # summarise(White_prop = mean(White_prop)) %>%
  ggplot(aes(x = Traitement, y = White_prop, color = Exclos)) +
  geom_boxplot()
