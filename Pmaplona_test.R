rain <- altsasu %>% filter(month(Fecha) == 12, Prec > 0) %>% group_by(year = year(Fecha)) %>%
  summarize(Preci = sum(Prec), n = n())

rain <- mutate(rain, ratio = Preci/n)

ggplot(data = Pamplona) + geom_line(mapping = aes(x = Fecha, y = Prec), colour = "red", na.rm = TRUE) #+
  #geom_line(mapping = aes(x = year, y = ratio), colour = "blue", na.rm = TRUE)

max(rain$Preci)

rain2 <- Pamplona %>% group_by(year = year(Fecha)) %>% filter(month(Fecha) == 8, sum(Prec) > 100)
