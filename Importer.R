
library(tidyverse)
library(here)
here()

files <- list.files(path = here("data"), pattern = ".csv")
file_names <- substr(files,1,23)

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

try(rename(df, any_of(headers)))

setwd(here("data"))

for(i in file_names){
  df <- read_delim(paste(i,".csv", sep=""),
                      delim = ";", 
                      col_select = contains(c("a","o")),
                      locale = locale(decimal_mark = ".", encoding = "iso-8859-1")
                      )
  for(j in 1:length(headers)){
      if(try(rename(df, headers[j]))){
        df <- mutate(df, headers[j])
      }
    
  }
  
        
  df <- mutate(df, 
         "Fecha-hora" = dmy_hms(`Fecha-hora`), 
         "Insolación total h" = seconds_to_period(as.numeric(`Insolación total s`)), 
         "Insolación total s" = NULL
  )
  }
  



potato2 <- mutate(potato, 
                  "Fecha-hora" = dmy_hms(`Fecha-hora`), 
                  "Insolación total h" = seconds_to_period(as.numeric(`Insolación total s`)), 
                  "Insolación total s" = NULL,
                  "dif_temp" = `Temperatura máxima ºC` - `Temperatura mínima ºC`
                  )

ggplot(data = potato2) + 
  geom_line(mapping = aes(x = `Fecha-hora`, y = `Temperatura máxima ºC`), colour = "red") +
  geom_line(mapping = aes(x = `Fecha-hora`, y = `Temperatura mínima ºC`), colour = "blue") +
  geom_line(mapping = aes(x = `Fecha-hora`, y = dif_temp), linewidth = 1, colour = "black") +
  geom_smooth(mapping = aes(x = `Fecha-hora`, y = `Temperatura máxima ºC`), colour = "red", method = "loess") +
  geom_smooth(mapping = aes(x = `Fecha-hora`, y = `Temperatura máxima ºC`), colour = "purple", method = "gam") +
  geom_smooth(mapping = aes(x = `Fecha-hora`, y = `Temperatura mínima ºC`), colour = "blue") +
  geom_smooth(mapping = aes(x = `Fecha-hora`, y = dif_temp), linewidth = 1, colour = "black") 

print(c("Media: ", mean(potato2$dif_temp), " - Max: ", max(potato2$dif_temp)," - Min: ", min(potato2$dif_temp)))

