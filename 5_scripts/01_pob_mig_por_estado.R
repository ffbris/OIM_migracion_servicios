lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")



function.migrante.estado <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)  ) %>% 
    group_by(TIEMPO, ent, migrante) %>%
    summarise(total.ocupada.estado = sum(fac)) %>% ungroup()
}


lista.pob.ocup.estado <- lapply(lista.df, function.migrante.estado)

bd.ocupacion.estado <- Reduce(full_join,lista.pob.ocup.estado) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.estado  %>% saveRDS("1_data/processed_data/bd_ocupacion_estado.rds")


bd.ocupacion.estado  <- readRDS("1_data/processed_data/bd_ocupacion_estado.rds")

df_estado <- as.data.frame(cbind(seq(1,32), 
                                 c("Ags.","BC","BCS","Camp.","Coah.","Col.","Chis.","Chih.","CDMX","Dgo.","Gto.","Gro.","Hgo.","Jal.","Mex.","Mich.","Mor.","Nay.","NL","Oax.","Pue.","Qro.","Q. Roo","SLP","Sin.","Son.","Tab.","Tamps.","Tlax.","Ver.","Yuc.","Zac."),
                                 c("Centro-norte","Noreste","Noreste","Peninsula","Norte","Occidente","Sur","Noreste","Centro","Noreste","Centro-norte","Sur","Golfo","Occidente","Centro","Occidente","Centro","Occidente","Norte","Sur","Golfo","Centro-norte","Peninsula","Centro-norte","Noreste","Noreste","Peninsula","Norte","Golfo","Golfo","Peninsula","Centro-norte"
                                 )) )

colnames(df_estado) <- c("ent", "entidad", "region")
df_estado$ent <- as.integer(df_estado$ent)

bd.ocupacion.estado %>% filter(migrante == TRUE)  %>% 
  group_by(ent, fecha) %>% 
  summarise(Total = sum(total.ocupada.estado)) %>% 
  left_join(df_estado, by= "ent") %>%
  mutate(ent =as.factor(ent)) %>%
  ggplot(aes(x=fecha, y=Total)) +
  geom_line() + 
  xlab("") + 
  labs(y="Población migrante internacional ocupada", 
       x = "Fecha") +
  facet_wrap("entidad", ncol(4)) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".", suffix = "mil", scale = 1e-3)) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_mig_internacional_por_estado.svg", 
       device = "svg",
       height = 25,
       width = 24,
       units = "cm",
       dpi = 300)

bd.ocupacion.estado %>% filter(migrante == TRUE)  %>% 
  group_by(ent, fecha) %>% 
  summarise(Total = sum(total.ocupada.estado)) %>% 
  left_join(df_estado, by= "ent") %>%
  mutate(ent =as.factor(ent)) %>% 
  filter(fecha == "2022 Q4") %>%
  ungroup() %>%
  mutate(Total_perc = Total/sum(Total))


bd.ocupacion.estado %>% filter(fecha < 2022, migrante == TRUE)  %>% 
  group_by(ent, fecha) %>% 
  summarise(Total = sum(total.ocupada.estado)) %>% 
  left_join(df_estado, by= "ent") %>%
  group_by(region, fecha) %>% 
  summarise(Total = sum(Total)) %>% 
  ggplot(aes(x=fecha, y=Total)) +
  geom_line() + 
  xlab("") + 
  labs(y="Población migrante internacional ocupada", 
       x = "Fecha", 
       caption = "Fuente: INEGI ENOE. Notas: 2005-2022 Trimestral") +
  facet_wrap("region") +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".", suffix = "mil", scale = 1e-3)) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book")) 

ggsave("6_output/01_plot.pob_mig_internacional_por_region.svg", 
       device = "svg",
       height = 20,
       width = 20,
       units = "cm",
       dpi = 300)