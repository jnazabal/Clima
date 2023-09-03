
library(tidyverse)
library(here)
here()

headers <- c(
  "Fecha" = "Fecha-hora",
  "Tmax" = "Temperatura máxima ºC",
  "Tmed" = "Temperatura media ºC",
  "Tmin" = "Temperatura mínima ºC",
  "Hmed" = "Humedad relativa med. %",
  "Hmax" = "Humedad relativa máx. %",
  "Hmin" = "Humedad relativa mín. %",
  "Prec" = "Precipitación acumulada l/m²",
  "Viento_speed" = "Velocidad media viento 10 m m/s",
  "Viento_dir" = "Dirección viento 10 m (MODA) sector",
  "Racha_max" = "Velocidad racha máx 10 m m/s",
  "Racha_dir" = "Dirección racha máx 10 m º",
  "Rad" = "Radiación global W/m²",
  "Sun" = "Insolación total s"
)

folders <- list.dirs(path = here("data"))
folders <- folders[-1] #removes the first folder of the list (the "data" folder)
folder_names <- basename(folders)

for(folder in folders){
  files <- list.files(path = here(folder), pattern = ".csv")
  #file_names <- substr(files, 1, nchar(files) - 4)
  setwd(here(folder))
  
  ndf <- data.frame(matrix(ncol = length(headers), nrow = 0))
  colnames(ndf) <- names(headers)
  
  for(i in files){
    df <- read_delim(i,
                     delim = ";", 
                     col_select = contains(c("a","o")),
                     #col_select = !last_col(),
                     locale = locale(decimal_mark = ".", encoding = "iso-8859-1")
    )
    for(j in 1:length(headers)){
      if(headers[j] %in% colnames(df)){
        df <- rename(df, headers[j])
      } else {
        df <- add_column(df, !!names(headers[j]) := NA, .after = j-1)
      }
    }
    ndf <- rbind(ndf, df)
  }
  ndf <- ndf %>% mutate(
    Fecha = dmy_hms(Fecha),
    Sun = seconds_to_period(as.numeric(Sun)),
    Tmax = as.numeric(Tmax),
    Tmed = as.numeric(Tmed),
    Tmin = as.numeric(Tmin),
    Hmax = as.numeric(Hmax),
    Hmin = as.numeric(Hmin),
    Hmed = as.numeric(Hmed),
    Prec = as.numeric(Prec)
  )
  assign(basename(folder),ndf)
}


  # df <- mutate(df, 
  #              "Fecha-hora" = dmy_hms(`Fecha-hora`), 
  #              "Insolación total h" = seconds_to_period(as.numeric(`Insolación total s`)), 
  #              "Insolación total s" = NULL
  # )

ggplot(data = get(folder_names[1])) + 
#ggplot(data = ndf) + 
  geom_line(mapping = aes(x = Fecha, y = Tmax), colour = "red", na.rm = TRUE) +
  geom_line(mapping = aes(x = Fecha, y = Tmin), colour = "blue", na.rm = TRUE) +
  #geom_line(mapping = aes(x = `Fecha-hora`, y = dif_temp), linewidth = 1, colour = "black", na.rm = TRUE) +
  geom_smooth(mapping = aes(x = Fecha, y = Tmax), colour = "red", method = "loess", na.rm = TRUE) +
  geom_smooth(mapping = aes(x = Fecha, y = Tmax), colour = "purple", method = "gam", na.rm = TRUE) +
  geom_smooth(mapping = aes(x = Fecha, y = Tmin), colour = "blue", na.rm = TRUE) 
  #geom_smooth(mapping = aes(x = `Fecha-hora`, y = dif_temp), linewidth = 1, colour = "black") 

# ggplot(data = df) + 
#   geom_line(mapping = aes(x = Fecha, y = Tmax), colour = "red", na.rm = TRUE) +
#   geom_line(mapping = aes(x = Fecha, y = Tmin), colour = "blue", na.rm = TRUE)

ggplot(data = get(folder_names[1])) + 
  geom_point(mapping = aes(x = Fecha, y = Prec), colour = "red", na.rm = TRUE)
