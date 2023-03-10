lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")


sectores_MH <- c(
  "Trabajadores subordinados y remunerados Asalariados INFORMALES",
  "Trabajadores subordinados y remunerados Asalariados FORMALES",
  "Trabajadores subordinados y remunerados Con percepciones no salariales INFORMALES",
  "Trabajadores subordinados y remunerados con percepciones no salariales FORMALES",
  "Empleadores INFORMAL",
  "Empleadores FORMAL",
  "Trabajadores por cuenta propia INFORMAL",
  "Trabajadores por cuenta propia FORMAL",
  "Trabajadores no Remunerados", 
  "Trabajadores no Remunerados FORMAL"
)

sectores_MH_resum <- c(
  "Trabajadores subordinados y remunerados \nAsalariados INFORMALES",
  "Trabajadores subordinados y remunerados \nAsalariados FORMALES",
  "Trabajadores subordinados y remunerados \ncon percepciones no salariales INFORMALES",
  "Trabajadores subordinados y remunerados \ncon percepciones no salariales FORMALES",
  "Empleadores",
  "Empleadores",
  "Trabajadores por cuenta propia",
  "Trabajadores por cuenta propia",
  "Trabajadores no Remunerados", 
  "Trabajadores no Remunerados"
)


formal_informal <- c("Informales",
                     "Formales",
                     "Informales",
                     "Formales",
                     "Informales",
                     "Formales",
                     "Informales",
                     "Formales",
                     "Informales",
                     "Formales")

df_MH <- data.frame(sectores = sectores_MH, sectores_MH_resum, formal_informal, mh_col = c(1,2,3,4,5,6,7,8,9,10))


function.mh <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante == 1) %>%
    group_by(mh_col, migrante, TIEMPO) %>%
    summarise(total.ocupada = sum(fac)) %>% ungroup()
}

lista.pob.ocup.mh <- lapply(lista.df, function.mh)

bd.ocupacion.mh <- Reduce(full_join,lista.pob.ocup.mh) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.mh  %>% saveRDS("1_data/processed_data/bd_ocupacion_mh.rds")

bd.ocupacion.mh %>% filter(fecha < 2022)  %>% 
  group_by(mh_col, fecha) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Formal = formal_informal)%>%
  ggplot(aes(x=fecha, y=Total, fill = Formal)) +
  geom_bar(position="stack", stat="identity") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_formalinformal.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

bd.ocupacion.mh %>% filter(fecha < 2022)  %>% 
  group_by(mh_col, fecha) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Formal = formal_informal)%>% 
  group_by(fecha, Formal) %>% summarise(Total =sum(Total)) %>%
  spread(Formal, Total) %>%
  mutate(Prop = Informales / (Formales + Informales))  %>% 
  ggplot(aes(x=fecha, y=Prop)) +
  geom_line() + 
  geom_smooth(fill = "Gray") +
  xlab("") + 
  labs(y="Población ocupada informal como % del total", 
       x = "Fecha", 
       # title = "Población ocupada migrante internacional en manufactura \nformal vs informal",
       # subtitle = "2005-2022 Trimestral",
       # caption = "Fuente: INEGI ENOE."
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migserv_formalinformal_porcentaje.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)


bd.ocupacion.mh %>% filter(fecha < 2022)  %>% 
  group_by(mh_col, fecha) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Clasificacion = fct_reorder(sectores_MH_resum, Total, .desc = FALSE))%>%
  ggplot(aes(x=fecha, y=Total, fill = Clasificacion)) +
  geom_bar(position="fill", stat="identity", alpha = 0.75) + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Población ocupada migrante internacional en manufactura \npor sectores de matriz de Hussmans",
       # subtitle = "2005-2022 Trimestral",
       # caption = "Fuente: INEGI ENOE."
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book")) +
  theme(legend.position="bottom")

ggsave("6_output/01_plot.pob_migserv_tipoempleo.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)