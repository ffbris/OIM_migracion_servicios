## This code is just a test
library(tidyverse)
prueba3 <- readRDS("1_data/raw_data/16-1.rds")

# INEGI dice que para pob ocupada, en UNIVERSO: Para el uso de las variables se debe aplicar el siguiente criterio: R_DEF= 00 y C;RES=1 o 3 y EDAD 15 a 98 AÑOS. https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/doc/fd_c_bas_amp_15ymas.pdf (p. 18)

prueba3 %>% filter(clase2 == 1,
                  r_def == 0,
                  (c_res == 1 | c_res == 3),
                  eda >= 15,
                  eda <= 98,
                  rama_est2 == 3
                  ) %>%
  mutate(migrante = ifelse(l_nac_c > 33, 1,0)) %>%
  filter(migrante == 1) %>%
  summarise(total = sum(fac)) %>% pull()