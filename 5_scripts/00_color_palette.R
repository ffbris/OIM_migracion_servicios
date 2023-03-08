# This code establishes the palette for the ggraph graphs that will be produced in this project 


ggthemr_reset()


set.seed(12345)
paleta_OIM_base <- c("#0033A0", 
                              "#5B92E5", 
                              "#FFB81C",
                              "#5CB8B2",
                              "#ff671f",
                              "#d22630")
                              
paleta_OIM <- c(paleta_OIM_base,
                brightness(paleta_OIM_base, 0.75),
                brightness(paleta_OIM_base, 0.5),
                brightness(paleta_OIM_base, 0.3))

OIM <- define_palette(
  swatch = paleta_OIM,
  gradient = c(lower = "#5B92E5", upper = "#0033A0"),
  background = "#ffffff",
  text = c("#444444", "#444444"),
  line = c("#696969", "#696969"),
  gridline = "#F5F5F5"
)

ggthemr(OIM)


# Ver https://github.com/wch/extrafont/issues/32
# remotes::install_version("Rttf2pt1", version = "1.3.8")

font_import(paths = "7_misc/fonts/", prompt = FALSE)
font.add("Gill_Sans_Nova", "7_misc/fonts/GillSansNova-Book.otf")
