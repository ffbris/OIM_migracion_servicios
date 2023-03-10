lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

function.migrante.niv.inst <- function(df) {
  df %>% filter(clase1 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    filter(l_nac_c > 33) %>%
    mutate( origen = l_nac_c )%>%
    group_by(TIEMPO, niv_ins, origen) %>%
    summarise(total.ocupada.estado = sum(fac)) %>% ungroup()
}

lista.pob.ocup.mig.niv.inst <- lapply(lista.df, function.migrante.niv.inst)

bd.ocupacion.niv.inst <- Reduce(full_join,lista.pob.ocup.mig.niv.inst) %>% mutate(fecha = as.yearqtr(TIEMPO))

guia <- as.data.frame(cbind(origen.n = as.character(c(221, 225, 200, 400, 300, 500, 100, 415)), nombre = c("EUA", "Guatemala", "Otros países América", "Otros países Europa", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "España")))

bd.ocupacion.niv.inst <-   bd.ocupacion.niv.inst  %>% 
  mutate(origen.n= ifelse((fecha < "2012 Q2" & origen == 201), 221, as.character(origen)))  %>% arrange(desc(total.ocupada.estado)) %>% left_join(guia, by= "origen.n") 

bd.ocupacion.niv.inst <- bd.ocupacion.niv.inst %>% mutate(nombre = ifelse((is.na(nombre) & substr(origen.n,1,1)==2), "Otros países América", ifelse( (is.na(nombre) & substr(origen.n,1,1)==4),"Otros países Europa",ifelse( (is.na(nombre) & (substr(origen.n,1,1)== 3 | substr(origen.n,1,1)== 5 | substr(origen.n,1,1)== 1)),"Otros países África, Oceanía y Asia",nombre) ))) 


bd.ocupacion.niv.inst  %>% 
  group_by(niv_ins, fecha) %>%   mutate(EUA = ifelse(nombre == "EUA","TRUE","FALSE")) %>% filter(EUA == TRUE) %>%
  mutate(Niv_inst = as.factor(niv_ins), Total = total.ocupada.estado)%>%
  ggplot(aes(x=fecha, y=Total, fill= Niv_inst)) +
  geom_bar(position="fill", stat="identity") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Proporción entre población migrante internacional y población \ntotal ocupada por sector",
       # subtitle = "2005-2022 Trimestral"
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          theme(text=element_text(size=10,  family="Gill Sans Nova Book"))

bd.ocupacion.niv.inst  %>% 
  group_by(niv_ins, fecha) %>%   mutate(EUA = ifelse(nombre == "EUA","TRUE","FALSE")) %>% filter(EUA == FALSE) %>%
  mutate(Niv_inst = as.factor(niv_ins), Total = total.ocupada.estado)%>%
  ggplot(aes(x=fecha, y=Total, fill= Niv_inst)) +
  geom_bar(position="fill", stat="identity") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha", 
       # title = "Proporción entre población migrante internacional y población \ntotal ocupada por sector",
       # subtitle = "2005-2022 Trimestral"
  ) +
  # theme_bw() + 
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          theme(text=element_text(size=10,  family="Gill Sans Nova Book"))