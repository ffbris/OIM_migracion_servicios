lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

function.migrante.sex <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(migrante, sex, TIEMPO) %>%
    summarise(total.ocupada.sex = sum(fac)) %>% ungroup()
}

lista.pob.ocup.mig.sex <- lapply(lista.df, function.migrante.sex)

bd.ocupacion.sex <- Reduce(full_join,lista.pob.ocup.mig.sex) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.sex  %>% saveRDS("1_data/processed_data/bd_ocupacion_sex.rds")

graf_pmom_sexo <- bd.ocupacion.sex %>% filter(migrante == TRUE) %>% mutate(sexo = as.factor(sex))%>%
  group_by(fecha, sexo) %>% 
  summarise(Total = sum(total.ocupada.sex))%>%
  mutate(sexo = ifelse(sexo ==1, "Hombre", "Mujer"))%>%
  ggplot(aes(x=fecha, y=Total, group = sexo, colour = sexo)) +
  geom_line() + 
  geom_smooth(fill = "light gray") +
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Población migrante ocupada en la manufactura (por sexo)",
       # subtitle = "2005-2022 Trimestral",
       # caption = "Fuente: INEGI ENOE."
  ) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_sexo.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)