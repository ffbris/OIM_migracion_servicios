lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

function.migrante.eda <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(migrante, eda7c, TIEMPO) %>% filter(migrante ==TRUE) %>% 
    summarise(total.ocupada.eda = sum(fac)) %>% ungroup()
}



function.migrante.eda2 <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO) %>% 
    summarise(Media = weighted.mean(eda, fac), 
              Mediana = weighted.median(eda, fac),
              Cuartil_1 = weighted.quantile(eda, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(eda, fac, prob = .75, na.rm= TRUE))
}

lista.pob.ocup.mig.eda <- lapply(lista.df, function.migrante.eda)

bd.ocupacion.eda <- Reduce(full_join,lista.pob.ocup.mig.eda) %>% mutate(fecha = as.yearqtr(TIEMPO))

edades <- c("De 15 a 19 años",
            "De 20 a 29 años",
            "De 30 a 39 años",
            "De 40 a 49 años",
            "De 50 a 59 años",
            "De 60 años y más",
            "Edad no especificado")

df_edades <- data.frame(edades, eda7c = c(1,2,3,4,5,6,7))

bd.ocupacion.eda  %>% saveRDS("1_data/processed_data/bd_ocupacion_eda.rds")

bd.ocupacion.eda %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  filter(fecha < 2022)  %>% 
  group_by(fecha, eda7c) %>% 
  summarise(Total = sum(total.ocupada.eda))%>%
  left_join(df_edades, by= "eda7c") %>%
  mutate(edad =as.factor(eda7c), Edades = edades)%>%
  ggplot(aes(x=fecha, y=Total, group= Edades, colour = Edades)) +
  geom_smooth(alpha=.3, fill = "gray") + 
  # ggplot(aes(x=fecha, y=Total, fill = Edades)) +
  #                       geom_bar(position="stack", stat="identity") + 
  #                       xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Población migrante internacional ocupada por grupo de edad",
       # subtitle = "2005-2022 Trimestral",
       caption = "Nota: Suavización Loess."
  ) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_edad.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)






lista.pob.ocup.mig.eda2 <- lapply(lista.df, function.migrante.eda2)

bd.ocupacion.eda2 <- Reduce(full_join,lista.pob.ocup.mig.eda2) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.eda2  %>% saveRDS("1_data/processed_data/bd_ocupacion_eda2.rds")

bd.ocupacion.eda2 %>%  
  filter(fecha < 2022)  %>% 
  ggplot(aes(x=fecha)) +  geom_line(aes(y=Media)) + 
  geom_line(alpha=.5, aes(y=Mediana), linetype = "dotted") + 
  geom_ribbon(aes(ymin = Cuartil_1, ymax =Cuartil_3), alpha = .2) +
  labs(y="Edad", 
       x = "Fecha", 
       caption = "Fuente: INEGI ENOE.\nÁrea sombreada = rango entre cuartiles. Línea punteada = mediana. Línea sólida = media.\nTodos los estadísticos son ponderados por los factores de expansión de la ENOE.") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_edad2.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)
