lista.df <- readRDS("~/Desktop/23 Migración ASI servicios/OIM_migracion_servicios/1_data/processed_data/lista_df.rds")

sectores_rama <- c("Agropecuario",
                   "Construcción",
                   "Industria manufacturera",
                   "Comercio",
                   "Servicio",
                   "Otros",
                   "No especificado")

df_rama <- data.frame(sectores = sectores_rama, rama = c(6,1,2,3,4,5,7))

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



function.rama.formalidad <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
  ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>%
    group_by(rama, migrante, TIEMPO, mh_col, rama_est2) %>%
    summarise(total.ocupada = sum(fac)) %>% ungroup()
}


lista.rama.formalidad <- lapply(lista.df, function.rama.formalidad)

bd.ocupacion.rama.formalidad <- Reduce(full_join, lista.rama.formalidad) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.rama.formalidad  %>% saveRDS("1_data/processed_data/bd_ocupacion_rama_formalidad.rds")

bd.ocupacion.rama.formalidad %>% 
  filter(migrante == TRUE) %>%
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores, Total = total.ocupada)%>%
  group_by(mh_col, fecha, Sectores) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Formal = formal_informal)%>% 
  group_by(fecha, Formal, Sectores) %>% summarise(Total =sum(Total)) %>%
  spread(Formal, Total) %>%
  mutate(Prop = Informales / (Formales + Informales)) %>%
  ggplot(aes(x=fecha, y=Prop, color = Sectores)) +
  geom_smooth(alpha = .2, fill = "gray") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))


clasificacion <- c("Restaurantes y servicios \nde alojamiento", "Transportes, comunicaciones,\n correo y almacenamiento", "Servicios profesionales, \nfinancieros y corporativos", 
                   "Servicios sociales", "Servicios diversos", "Gobierno y organismos \ninternacionales")

clasif.df <- data.frame(rama_est2 = c(6,7,8,9,10,11), clasificacion)



bd.ocupacion.rama.formalidad %>% 
  filter(migrante == TRUE) %>%
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores, Total = total.ocupada)%>%
  group_by(mh_col, fecha, Sectores) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Formal = formal_informal)%>% 
  group_by(fecha, Formal, Sectores) %>% summarise(Total =sum(Total)) %>%
  spread(Formal, Total) %>%
  mutate(Prop = Informales / (Formales + Informales)) %>% filter(fecha == "2022 Q4")


bd.ocupacion.rama.formalidad %>% 
  left_join(clasif.df, by= "rama_est2") %>%
  mutate(rama =as.factor(rama), Subsectores = clasificacion, Total = total.ocupada)%>%
  filter(rama == 4) %>% 
  group_by(mh_col, fecha, Subsectores) %>% 
  summarise(Total = sum(total.ocupada)) %>% 
  left_join(df_MH, by= "mh_col") %>%
  mutate(Formal = formal_informal)%>% 
  group_by(fecha, Formal, Subsectores) %>% summarise(Total =sum(Total)) %>%
  spread(Formal, Total) %>%
  mutate(Prop = Informales / (Formales + Informales)) %>%
  ggplot(aes(x=fecha, y=Prop, color = Subsectores)) +
  geom_line(linetype = "solid") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))





bd.ocupacion.total  %>% filter(migrante == TRUE) %>%
  group_by(rama, fecha) %>%   
  left_join(df_rama, by= "rama") %>%
  mutate(rama =as.factor(rama), Sectores = sectores, Total = total.ocupada)%>%
  filter(rama != 7)  %>%
  ggplot(aes(x=fecha, y=Total, fill= Sectores)) +
  geom_bar(position="stack", stat="identity") + 
  xlab("") + 
  labs(y="Población ocupada", 
       x = "Fecha") +
  scale_x_yearqtr(format = "%YT%q", n = (2022-2005)*2-3) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +          
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))



# Escolaridad vs subsector

function.migrante.esc2 <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    group_by(TIEMPO, rama_est2) %>% 
    summarise(Media = weighted.mean(anios_esc, fac), 
              Mediana = weighted.median(anios_esc, fac, na.rm = TRUE),
              Cuartil_1 = weighted.quantile(anios_esc, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(anios_esc, fac, prob = .75, na.rm= TRUE))
}

lista.pob.ocup.mig.esc2 <- lapply(lista.df, function.migrante.esc2)

bd.ocupacion.esc2 <- Reduce(full_join,lista.pob.ocup.mig.esc2) %>% mutate(fecha = as.yearqtr(TIEMPO))

bd.ocupacion.esc2  %>% saveRDS("1_data/processed_data/bd_ocupacion_escolaridad_2.rds")

clasificacion <- c("Restaurantes y servicios \nde alojamiento", "Transportes, comunicaciones,\n correo y almacenamiento", "Servicios profesionales, \nfinancieros y corporativos", 
                   "Servicios sociales", "Servicios diversos", "Gobierno y organismos \ninternacionales")

clasif.df <- data.frame(rama_est2 = c(6,7,8,9,10,11), clasificacion)


bd.ocupacion.esc2 %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  left_join(clasif.df, by= "rama_est2") %>%
  ggplot(aes(x=fecha)) +
#  geom_line(aes(y=Media)) + 
  geom_smooth(alpha=.5, aes(y=Media, group = clasificacion, color = clasificacion), fill = "gray") + 
#  geom_ribbon(aes(ymin = Cuartil_1, ymax =Cuartil_3), alpha = .2) +
  labs(y="Años de escolaridad", 
       x = "Fecha", 
       caption = "Área sombreada = rango entre cuartiles. Línea punteada = mediana. Línea sólida = media.\nTodos los estadísticos son ponderados por los factores de expansión de la ENOE.") + ylim(NA, 15000) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  theme(text=element_text(size=10,  family="Gill Sans Nova Book"))


bd.ocupacion.esc2 %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  left_join(clasif.df, by= "rama_est2") %>% filter(fecha == "2022 Q4")



### INGRESOS



function.migrante.ing2 <- function(df) {
  df %>% filter(clase2 == 1, 
                r_def == 0, 
                (c_res == 1 | c_res == 3), 
                eda >= 15, 
                eda <= 98,
                rama == 4 ) %>% 
    mutate(migrante = ifelse(l_nac_c > 33, TRUE,FALSE)) %>% filter(migrante ==TRUE) %>% 
    mutate(origen = l_nac_c) %>%
    group_by(TIEMPO, origen) %>% 
    summarise(Media = weighted.mean(ingocup, fac), 
              Mediana = weighted.median(ingocup, fac, na.rm = TRUE),
              Cuartil_1 = weighted.quantile(ingocup, fac, prob = .25, na.rm= TRUE),
              Cuartil_3 = weighted.quantile(ingocup, fac, prob = .75, na.rm= TRUE),
              num_mig = sum(fac),
              n_obs = n())
}

lista.pob.ocup.mig.ing2 <- lapply(lista.df, function.migrante.ing2)

bd.ocupacion.ing2 <- Reduce(full_join,lista.pob.ocup.mig.ing2) %>% mutate(fecha = as.yearqtr(TIEMPO))

guia <- as.data.frame(cbind(origen.n = as.character(c(221, 225, 200, 400, 300, 500, 100, 415)), nombre = c("EUA", "Guatemala", "Otros países América", "Otros países Europa", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "Otros países África, Oceanía y Asia", "España")))


bd.ocupacion.ing2 <- bd.ocupacion.ing2 %>% 
  mutate(origen.n= ifelse((fecha < "2012 Q2" & origen == 201), 221, as.character(origen)))  %>% left_join(guia, by= "origen.n") 

bd.ocupacion.ing2 <- bd.ocupacion.ing2 %>% mutate(nombre = ifelse((is.na(nombre) & substr(origen.n,1,1)==2), "Otros países América", ifelse( (is.na(nombre) & substr(origen.n,1,1)==4),"Otros países Europa",ifelse( (is.na(nombre) & (substr(origen.n,1,1)== 3 | substr(origen.n,1,1)== 5 | substr(origen.n,1,1)== 1)),"Otros países África, Oceanía y Asia",nombre) ))) 


bd.ocupacion.ing2  %>% saveRDS("1_data/processed_data/bd_ocupacion_ingreso2.rds")

library(clipr)

bd.ocupacion.ing2 %>% #mutate(eda7c = as.factor(eda7c)) %>% 
  mutate(origen = as.factor(origen.n)) %>% 
  filter(!is.na(nombre)) %>%filter(fecha == "2022 Q4") %>%as.data.frame %>% write_clip()