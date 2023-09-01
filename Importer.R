setwd("~/R_exercises/Course 7/Week 3")
library(tidyverse)

potato <- read_delim("etxarri-aranatz gn_2022.csv", 
                     delim = ";", 
                     col_select = contains(c("a","o")),
                     locale = locale(decimal_mark = ".", encoding = "iso-8859-1")
                     )

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

