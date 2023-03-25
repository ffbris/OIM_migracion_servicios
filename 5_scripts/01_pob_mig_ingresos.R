lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

function.migrante.ing <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4 ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO) %>% 
    summarise(Media = weighted.mean(ingocup, fac), 
              Mediana = weighted.median(ingocup, fac, na.rm = TRUE),
              Cuartil_1 = weighted.quantile(ingocup, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(ingocup, fac, prob = .75, na.rm= TRUE))
}

lista.pob.ocup.mig.ing <- lapply(lista.df, function.migrante.ing)

bd.ocupacion.ing <- Reduce(full_join,lista.pob.ocup.mig.ing) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.ing  %>% saveRDS("1_data/processed_data/bd_ocupacion_ingreso.rds")

bd.ocupacion.ing %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  filter(fecha < 2022)  %>% 
  ggplot(aes(x=fecha)) +
  geom_line(aes(y=Media)) + 
  geom_line(alpha=.5, aes(y=Mediana), linetype = "dotted") + 
  geom_ribbon(aes(ymin = Cuartil_1, ymax =Cuartil_3), alpha = .2) +
  
  # ggplot(aes(x=fecha, y=Total, fill = Edades)) +
  #                       geom_bar(position="stack", stat="identity") + 
  #                       xlab("") + 
  labs(y="Ingreso mensual", 
       x = "Fecha", 
       # title = "Ingreso mensual de la población migrante internacional ocupada \nen la manufactura",
       # subtitle = "2005-2022 Trimestral",
       caption = "Fuente: INEGI ENOE.\nÁrea sombreada = rango entre cuartiles. Línea punteada = mediana. Línea sólida = media.\nTodos los estadísticos son ponderados por los factores de expansión de la ENOE.") + ylim(NA, 15000) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))


ggsave("6_output/01_plot.pob_migserv_ingreso.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

# Calculos solicitados
bd.ocupacion.ing %>% #mutate(eda7c = as.factor(eda7c)) %>%
  filter(fecha < 2022)  %>% summarise(Media = mean(Media), Mediana = mean(Mediana))