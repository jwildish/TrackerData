
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringi)
library(data.table)
library(tidyverse)
library(purrr)
library(plyr)
library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyverse)
library(data.table)
library(scales)
library(DT)
library(tools)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(formattable)
library(DT)
library(scales)
library(leaflet)
library(sf)
library(httr)
library(jsonlite)
library(latticeExtra)
library(reactable)
library(stringr)
library(plyr)
#pm25
citiespm25 <- "https://api.openaq.org/v1/cities"

citiespm25 <- GET(url = citiespm25, parameter="pm25",
                  query = list(limit = 10000))
citiespm25 <- httr::content(citiespm25, as = "text", encoding = "UTF-8")
citiespm25 <- fromJSON(citiespm25, flatten = TRUE) %>% 
  data.frame()
citiespm25$alleng <- stri_enc_isascii(citiespm25$results.city)
citiespm25 <- subset(citiespm25, alleng  == "TRUE")

citiespm25 <- subset(citiespm25, results.count >= 10000 & results.locations >=1)

citiespm25 <- citiespm25$results.city


no2 <- function(cityname) {(
  
  tryCatch({ path <- "https://api.openaq.org/v1/measurements"
  
  request <- GET(url = path, 
                 query = list(city= cityname, 
                              parameter = "no2",
                              limit = 1000))
  
  
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  
  path <- "C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/no2/"
  
  write.table(df, paste0(path,cityname, "_no2", ".csv"), append = FALSE, sep = ",", row.names = FALSE)}, error=function(e) NULL) 
)
}

lapply(citiespm25, no2)



filenames <- list.files(path="C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/no2/",pattern="*.csv")
fullpath=file.path("C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/no2/",filenames)
dataset <- do.call("rbind.fill",lapply(fullpath,FUN=function(files){read.csv(files)}))
dataset <- subset(dataset, results.value > 0)
dataset <- dataset %>% separate(results.date.local, c("Day", "Time"), sep = "T")
glimpse(dataset)
dataset$units <- ifelse(dataset$results.unit == "ppm", "ppm", "micrograms")
dataset$results.value <- ifelse(dataset$unit =="ppm", dataset$results.value * 1.88*1000, dataset$results.value)
dataset$units <- "micrograms per cubic meter"
datasetcountry <- dataset %>% dplyr::group_by(results.parameter, results.country, units) %>% dplyr::summarise(valuemax = max(results.value), valuemean = mean(results.value))
dataset2 <- dataset %>% dplyr::group_by(results.parameter,results.city, results.country, results.coordinates.latitude, results.coordinates.longitude, units) %>% dplyr::summarise(valuemax = max(results.value), valuemean = mean(results.value))


write.csv(dataset2, "./dataset2no2test.csv")



pm25 <- function(cityname) {(
  
  tryCatch({ 
    path <- "https://api.openaq.org/v1/measurements"
  
  request <- GET(url = path, 
                 query = list(city= cityname, 
                              parameter = "pm25", limit = 1000))
  
  
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  
  path <- "C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/pm25/"
  
  write.table(df, paste0(path,cityname, "_pm25", ".csv"), append = FALSE, sep = ",", row.names = FALSE)
  }, error=function(e) NULL) 
)
}


lapply(citiespm25, pm25)
getwd()
filenames <- list.files(path="C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/pm25/",pattern="*.csv")
fullpath=file.path("C:/Users/Jordan/OneDrive - Earth Economics/Documents/GlobalAirQuality/Data/pm25/",filenames)
dataset <- do.call("rbind.fill",lapply(fullpath,FUN=function(files){read.csv(files)}))
dataset <- subset(dataset, results.value > 0)
table(dataset$results.unit)
dataset <- dataset %>% separate(results.date.local, c("Day", "Time"), sep = "T")
dataset$units <- ifelse(dataset$results.unit == "ppm", "ppm", "micrograms")
dataset$results.value <- ifelse(dataset$unit =="ppm", dataset$results.value * 1.88*1000, dataset$results.value)
dataset$units <- "micrograms per cubic meter"
datasetcountry <- dataset %>% dplyr::group_by(results.parameter, results.country, units) %>% dplyr::summarise(valuemax = max(results.value), valuemean = mean(results.value))
dataset2 <- dataset %>% dplyr::group_by(results.parameter,results.city, results.country, results.coordinates.latitude, results.coordinates.longitude, units) %>%
  dplyr::summarise(valuemax = max(results.value), valuemean = mean(results.value), tally = dplyr::n())
dataset2 <- (subset(dataset2, tally > 400))

dataset2$results.city <- str_replace_all(dataset2$results.city, "[[:punct:]]", " ")
dataset2$results.city <- str_replace_all(dataset2$results.city, "-", " ")

write.csv(dataset2, "./dataset2pm25test.csv")

