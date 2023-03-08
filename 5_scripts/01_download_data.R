
# Download databases from INEGI

x <- character() 
for (i in 5:22){
  for (j in 1:4){
    year <- 2000 + i
    texto <- cat(sprintf("\ncurl -o %dtrim%d_csv.zip https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/%dtrim%d_csv.zip",year,j,year,j))
    x <- paste(x,texto)
  }
}
print(x)


# Una vez que tenemos los CSV con el nombre homogeneizado (había que abrir los zip y poner en minúsculas), nos encargamos de pasarlo a RDS, que es más eficiente.

fnames <- list.files()
files <- length(fnames)

for (i in 1:files){
  name <- fnames[i]
  name_RDS <- paste(stri_sub(name,-6,-5),
                    "-",
                    stri_sub(name,-7,-7),
                              ".rds", sep = "")
  file <- read.csv(name)
  saveRDS(file,name_RDS)
}


