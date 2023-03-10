lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

function.migrante.esc <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO) %>% 
    summarise(Media = weighted.mean(anios_esc, fac), 
              Mediana = weighted.median(anios_esc, fac, na.rm = TRUE),
              Cuartil_1 = weighted.quantile(anios_esc, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(anios_esc, fac, prob = .75, na.rm= TRUE))
}

lista.pob.ocup.mig.esc <- lapply(lista.df, function.migrante.esc)

bd.ocupacion.esc <- Reduce(full_join,lista.pob.ocup.mig.esc) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.esc  %>% saveRDS("1_data/processed_data/bd_ocupacion_escolaridad.rds")

bd.ocupacion.esc %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  filter(fecha < 2022)  %>% 
  ggplot(aes(x=fecha)) +
  geom_line(aes(y=Media)) + 
  geom_line(alpha=.5, aes(y=Mediana), linetype = "dotted") + 
  geom_ribbon(aes(ymin = Cuartil_1, ymax =Cuartil_3), alpha = .2) +
  labs(y="Años de escolaridad", 
       x = "Fecha", 
       caption = "Área sombreada = rango entre cuartiles. Línea punteada = mediana. Línea sólida = media.\nTodos los estadísticos son ponderados por los factores de expansión de la ENOE.") + ylim(NA, 15000) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))



ggsave("6_output/01_plot.pob_migserv_escolaridad.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

bd.ocupacion.esc %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  filter(fecha < 2022)  %>% summarise(Media = mean(Media), Mediana = mean(Mediana)) 

bd.ocupacion.esc %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  filter(fecha == 2021)  
