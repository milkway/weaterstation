#library("devtools")
#install_github("Ram-N/weatherData")
#library(weatherData)

library(rwunderground)
library(tidyverse)
# imaturia2 <- getSummarizedWeather("IMATURIA2", "2021-01-01",
#                      end_date="2021-07-10",
#                      station_type="id")
#days <- seq(from=as.Date('2021-01-01'), to = as.Date('2021-07-10'), by = 1)


set_api_key("2db930edbb8041e3b930edbb80d1e324")     #API key in weather underground page
pico <-"IMATURIA3"              #The uni of cyprus weather station id
jabre <- "IMATURIA2"
santoantonio <- "IMATUR2"

# # Pico do Jabre
# imaturia3 <- NULL
# for (d in 1:length(days)){
#   cat(format(days[d], '%Y%m%d'),'\n')
#   temp <- jsonlite::read_json(paste("https://api.weather.com/v2/pws/all/1day?stationId=IMATURIA3&format=json&units=m&date=",format(days[d], '%Y%m%d'),"&apiKey=2db930edbb8041e3b930edbb80d1e324", sep = ""), simplifyVector = TRUE)
#   imaturia3 <- bind_rows(imaturia3, temp$observations)
# }
#
# # Sítio Jabre
# imaturia2 <- NULL
# for (d in 1:length(days)){
#   cat(format(days[d], '%Y%m%d'),'\n')
#   temp <- jsonlite::read_json(paste("https://api.weather.com/v2/pws/history/all?stationId=IMATURIA2&format=json&units=m&date=",format(days[d], '%Y%m%d'),"&apiKey=2db930edbb8041e3b930edbb80d1e324", sep = ""), simplifyVector = TRUE)
#   imaturia2 <- bind_rows(imaturia2, temp$observations)
# }
#
#
# # Sítio Santo Antônio
# #IMATUR2
# imatur2 <- NULL
# for (d in 1:length(days)){
#   cat(format(days[d], '%Y%m%d'),'\n')
#   temp <- jsonlite::read_json(paste("https://api.weather.com/v2/pws/history/all?stationId=IMATUR2&format=json&units=m&date=",format(days[d], '%Y%m%d'),"&apiKey=2db930edbb8041e3b930edbb80d1e324", sep = ""), simplifyVector = TRUE)
#   imatur2 <- bind_rows(imatur2, temp$observations)
# }

#
# jsonlite::read_json(paste("https://api.weather.com/v2/pws/history/all?stationId=IMATUR2&format=json&units=m&date=",format(days[1], '%Y%m%d'),"&apiKey=2db930edbb8041e3b930edbb80d1e324", sep = ""))
#
# jsonlite::read_json(paste("https://api.weather.com/v2/pws/history/all?stationId=IMATURIA2&format=json&units=m&date=",format(days[d], '%Y%m%d'),"&apiKey=2db930edbb8041e3b930edbb80d1e324", sep = ""))
#
# imaturia2 = jsonlite::read_json("https://api.weather.com/v2/pws/observations/all/3day?apiKey=6532d6454b8aa370768e63d6ba5a832e&stationId=IMATURIA2&numericPrecision=decimal&format=json&units=m")
# imatur2 = jsonlite::read_json("https://api.weather.com/v2/pws/observations/all/3day?apiKey=6532d6454b8aa370768e63d6ba5a832e&stationId=IMATUR2&numericPrecision=decimal&format=json&units=m")
#
sta <- jsonlite::fromJSON("https://api.weather.com/v2/pws/observations/all/1day?apiKey=6532d6454b8aa370768e63d6ba5a832e&stationId=IMATUR2&numericPrecision=decimal&format=json&units=m", simplifyDataFrame = TRUE)
jbr <- jsonlite::fromJSON("https://api.weather.com/v2/pws/observations/all/1day?apiKey=6532d6454b8aa370768e63d6ba5a832e&stationId=IMATURIA2&numericPrecision=decimal&format=json&units=m", simplifyDataFrame = TRUE)
pco <- jsonlite::fromJSON("https://api.weather.com/v2/pws/observations/all/1day?apiKey=6532d6454b8aa370768e63d6ba5a832e&stationId=IMATURIA3&numericPrecision=decimal&format=json&units=m", simplifyDataFrame = TRUE)

jsonlite::fromJSON('https://api.weather.com/v2/pws/observations/all/1day?stationId=IMATUR2&format=json&units=e&apiKey=2db930edbb8041e3b930edbb80d1e324', simplifyDataFrame = TRUE)


jsonlite::fromJSON('https://api.weather.com/v2/pws/observations/all/1day?stationId=IMATURIA2&format=json&units=e&apiKey=2db930edbb8041e3b930edbb80d1e324', simplifyDataFrame = TRUE)


base <- bind_rows(sta$observations, jbr$observations) %>%
  bind_rows(pco$observations)

base %>%
  as_tibble() %>%
  mutate(Data = as.POSIXct(epoch, origin="1970-01-01"),
         Estação = if_else(stationID == 'IMATURIA3', 'Pico do Jabre',
                           if_else(stationID == 'IMATURIA2', 'Sítio Jabre',
                                   if_else(stationID == 'IMATUR2', 'Santo Antônio', NA_character_)))) %>%
  ggplot(aes(x = Data, y = winddirAvg, color = Estação)) + geom_point() +
  #geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.3) +
  ylab("Direção do Vento") +
  theme(legend.position="bottom")




base %>%
  as_tibble() %>%
  bind_cols(base$metric  %>% as_tibble()) %>%
  mutate(Data = as.POSIXct(epoch, origin="1970-01-01"),
         Estação = if_else(stationID == 'IMATURIA3', 'Pico do Jabre',
                           if_else(stationID == 'IMATURIA2', 'Sítio Jabre',
                                   if_else(stationID == 'IMATUR2', 'Santo Antônio', NA_character_)))) %>%
  ggplot(aes(x = Data, y = windspeedAvg, color = Estação)) + geom_point() +
  #geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.3) +
  ylab("Velocidade Média do Vento") +
  theme(legend.position="bottom")


base %>%
  as_tibble() %>%
  bind_cols(base$metric  %>% as_tibble()) %>%
  mutate(Data = as.POSIXct(epoch, origin="1970-01-01"),
         Estação = if_else(stationID == 'IMATURIA3', 'Pico do Jabre',
                           if_else(stationID == 'IMATURIA2', 'Sítio Jabre',
                                   if_else(stationID == 'IMATUR2', 'Santo Antônio', NA_character_)))) %>%
  ggplot(aes(x = Data, y = tempAvg, color = Estação)) + geom_point() +
  #geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.3) +
  ylab("Temperatura Média") +
  theme(legend.position="bottom")


base %>%
  as_tibble() %>%
  bind_cols(base$metric  %>% as_tibble()) %>%
  mutate(Data = as.POSIXct(epoch, origin="1970-01-01"),
         Estação = if_else(stationID == 'IMATURIA3', 'Pico do Jabre',
                           if_else(stationID == 'IMATURIA2', 'Sítio Jabre',
                                   if_else(stationID == 'IMATUR2', 'Santo Antônio', NA_character_)))) %>%
  ggplot(aes(x = Data, y = precipTotal, color = Estação)) + geom_point() +
  #geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.3) +
  ylab("Precipitação Total") +
  theme(legend.position="bottom")
