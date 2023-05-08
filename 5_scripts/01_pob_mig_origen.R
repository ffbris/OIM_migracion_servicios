lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")


function.migrante.origen <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    filter(migrante == TRUE) %>%
    mutate(origen = l_nac_c) %>%
    group_by(origen, TIEMPO) %>%
    summarise(total.ocupada.origen = sum(fac)) %>% ungroup()
}

lista.pob.ocup.mig.orig <- lapply(lista.df, function.migrante.origen)

bd.ocupacion.orig <- Reduce(full_join,lista.pob.ocup.mig.orig) %>% mutate(fecha = as.yearqtr(TIEMPO))

guia <- as.data.frame(cbind(origen.n = as.character(c(221, 225, 200, 400, 300, 500, 100, 415)), nombre = c("EUA", "Guatemala", "Otros países América", "Otros países Europa", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "España")))

bd.ocupacion.orig <-   bd.ocupacion.orig %>% 
  mutate(origen.n= ifelse((fecha < "2012 Q2" & origen == 201), 221, as.character(origen)))  %>% arrange(desc(total.ocupada.origen)) %>% left_join(guia, by= "origen.n") 

bd.ocupacion.orig <- bd.ocupacion.orig %>% mutate(nombre = ifelse((is.na(nombre) & substr(origen.n,1,1)==2), "Otros países América", ifelse( (is.na(nombre) & substr(origen.n,1,1)==4),"Otros países Europa",ifelse( (is.na(nombre) & (substr(origen.n,1,1)== 3 | substr(origen.n,1,1)== 5 | substr(origen.n,1,1)== 1)),"Otros países África, Oceanía y Asia",nombre) ))) 

bd.ocupacion.orig  %>% saveRDS("1_data/processed_data/bd_ocupacion_orig.rds")

bd.ocupacion.orig %>% 
  mutate(origen = as.factor(origen.n)) %>% 
  filter(!is.na(nombre)) %>% 
  group_by(fecha, nombre) %>% 
  summarise(Total = sum(total.ocupada.origen))    %>% 
  mutate(Región = nombre) %>%
  ggplot(aes(x=fecha, y=Total, group = Región, colour = Región)) +
  geom_smooth(alpha=.5, fill = "gray") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Población migrante ocupada en la manufactura (por origen)",
       # subtitle = "2005-2022 Trimestral",
       caption = "Nota: Series con suavización loess."
  ) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

ggsave("6_output/01_plot.pob_migmanuf_origen.svg", 
       device = "svg",
       height = 15,
       width = 24,
       units = "cm",
       dpi = 300)

# library(RColorBrewer)
# pal <- colorRampPalette(brewer.pal(12, "Set3"))
# pal(40)
# ggthemr('fresh')
# set_swatch(pal(40))

bd.ocupacion.orig %>% mutate(origen = as.factor(origen)) %>% filter(fecha > 2014) %>%
  group_by(origen) %>% 
  summarise(Total = median(total.ocupada.origen)) %>% arrange(desc(Total))