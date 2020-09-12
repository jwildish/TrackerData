library(EIAdata)
library(zoo)
library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(httr)
library(git2r)

Location <- "US48"
key <- "30e49be2693ea1f6197f79bfa50d3ad8"

dataus<-getEIA(ID=paste0("EBA.",Location,"-ALL.D.H"), key = key)
dataus <- as.data.frame(dataus)

data2 <- cbind(rownames(dataus), data.frame(dataus, row.names=NULL))
data3 <- data2 %>% separate(`rownames(dataus)`, c("X", "Data"), sep = "X")
data3$Data<- lubridate::ymd_hms(data3$Data)
data3$Year <- year(data3$Data)
data3$day <- day(data3$Data)
data3$month <- month(data3$Data)
data3$hour <- hour(data3$Data)
data3$wday <- week(data3$Data)
data3$value <- data3$EBA.US48.ALL.D.H

write.csv(data3, paste0(Location,"TotalPower.csv"))

Location <- "US48"
key <- "30e49be2693ea1f6197f79bfa50d3ad8"
#coal
try( datauscoal<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.COL.H"), key = key))
datauscoal <- as.data.frame(datauscoal)

datauscoal2 <- cbind(rownames(datauscoal), data.frame(datauscoal, row.names=NULL))
datauscoal3 <- datauscoal2 %>% separate(`rownames(datauscoal)`, c("X", "datauscoal"), sep = "X")
datauscoal3$Data<- lubridate::ymd_hms(datauscoal3$datauscoal)
datauscoal3$Year <- year(datauscoal3$Data)
datauscoal3$day <- day(datauscoal3$Data)
datauscoal3$month <- month(datauscoal3$Data)
datauscoal3$hour <- hour(datauscoal3$Data)

datauscoal3$wday <- week(datauscoal3$Data)


dataustotal <- data3 %>% select(Data, value)
glimpse(datauscoal)
datauscoal3 <- merge(datauscoal3, dataustotal, by = "Data", all.x = TRUE)

#other
try(datausother<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.OTH.H"), key = key), silent = TRUE)
datausother <- as.data.frame(datausother)

datausother2 <- cbind(rownames(datausother), data.frame(datausother, row.names=NULL))
datausother3 <- datausother2 %>% separate(`rownames(datausother)`, c("X", "datausother"), sep = "X")
datausother3$Data<- lubridate::ymd_hms(datausother3$datausother)
datausother3$Year <- year(datausother3$Data)
datausother3$day <- day(datausother3$Data)
datausother3$month <- month(datausother3$Data)
datausother3$hour <- hour(datausother3$Data)

datausother3$wday <- week(datausother3$Data)

dataustotal <- data3 %>% select(Data, value)
datausother3 <- merge(datausother3, dataustotal, by = "Data", all.x = TRUE)


#Hydro
try(dataushydro<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.WAT.H"), key = key), silent = TRUE)
dataushydro <- as.data.frame(dataushydro)

dataushydro2 <- cbind(rownames(dataushydro), data.frame(dataushydro, row.names=NULL))
dataushydro3 <- dataushydro2 %>% separate(`rownames(dataushydro)`, c("X", "dataushydro"), sep = "X")
dataushydro3$Data<- lubridate::ymd_hms(dataushydro3$dataushydro)
dataushydro3$Year <- year(dataushydro3$Data)
dataushydro3$day <- day(dataushydro3$Data)
dataushydro3$month <- month(dataushydro3$Data)
dataushydro3$hour <- hour(dataushydro3$Data)

dataushydro3$wday <- week(dataushydro3$Data)


dataustotal <- data3 %>% select(Data, value)
glimpse(dataushydro)
dataushydro3 <- merge(dataushydro3, dataustotal, by = "Data", all.x = TRUE)

#Nat gas
try(datausnatgas<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.NG.H"), key = key), silent = TRUE)
datausnatgas <- as.data.frame(datausnatgas)

datausnatgas2 <- cbind(rownames(datausnatgas), data.frame(datausnatgas, row.names=NULL))
datausnatgas3 <- datausnatgas2 %>% separate(`rownames(datausnatgas)`, c("X", "datausnatgas"), sep = "X")
datausnatgas3$Data<- lubridate::ymd_hms(datausnatgas3$datausnatgas)
datausnatgas3$Year <- year(datausnatgas3$Data)
datausnatgas3$day <- day(datausnatgas3$Data)
datausnatgas3$month <- month(datausnatgas3$Data)
datausnatgas3$hour <- hour(datausnatgas3$Data)

datausnatgas3$wday <- week(datausnatgas3$Data)


dataustotal <- data3 %>% select(Data, value)
glimpse(datausnatgas)
datausnatgas3 <- merge(datausnatgas3, dataustotal, by = "Data", all.x = TRUE)

#Nuclear
try(datausnuclear<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.NUC.H"), key = key), silent = TRUE)
datausnuclear <- as.data.frame(datausnuclear)

datausnuclear2 <- cbind(rownames(datausnuclear), data.frame(datausnuclear, row.names=NULL))
datausnuclear3 <- datausnuclear2 %>% separate(`rownames(datausnuclear)`, c("X", "datausnuclear"), sep = "X")
datausnuclear3$Data<- lubridate::ymd_hms(datausnuclear3$datausnuclear)
datausnuclear3$Year <- year(datausnuclear3$Data)
datausnuclear3$day <- day(datausnuclear3$Data)
datausnuclear3$month <- month(datausnuclear3$Data)
datausnuclear3$hour <- hour(datausnuclear3$Data)

datausnuclear3$wday <- week(datausnuclear3$Data)


dataustotal <- data3 %>% select(Data, value)
glimpse(datausnuclear)
datausnuclear3 <- merge(datausnuclear3, dataustotal, by = "Data", all.x = TRUE)

#Oil
try(datausoil<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.OIL.H"), key = key),silent=TRUE)
datausoil <- as.data.frame(datausoil)

datausoil2 <- cbind(rownames(datausoil), data.frame(datausoil, row.names=NULL))
datausoil3 <- datausoil2 %>% separate(`rownames(datausoil)`, c("X", "datausoil"), sep = "X")
datausoil3$Data<- lubridate::ymd_hms(datausoil3$datausoil)
datausoil3$Year <- year(datausoil3$Data)
datausoil3$day <- day(datausoil3$Data)
datausoil3$month <- month(datausoil3$Data)
datausoil3$hour <- hour(datausoil3$Data)

datausoil3$wday <- week(datausoil3$Data)


dataustotal <- data3 %>% select(Data, value)
datausoil3 <- merge(datausoil3, dataustotal, by = "Data", all.x = TRUE)

#Solar
try(dataussolar<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.SUN.H"), key = key), silent = TRUE)
dataussolar <- as.data.frame(dataussolar)

dataussolar2 <- cbind(rownames(dataussolar), data.frame(dataussolar, row.names=NULL))
dataussolar3 <- dataussolar2 %>% separate(`rownames(dataussolar)`, c("X", "dataussolar"), sep = "X")
dataussolar3$Data<- lubridate::ymd_hms(dataussolar3$dataussolar)
dataussolar3$Year <- year(dataussolar3$Data)
dataussolar3$day <- day(dataussolar3$Data)
dataussolar3$month <- month(dataussolar3$Data)
dataussolar3$hour <- hour(dataussolar3$Data)

dataussolar3$wday <- week(dataussolar3$Data)

dataustotal <- data3 %>% select(Data, value)
dataussolar3 <- merge(dataussolar3, dataustotal, by = "Data", all.x = TRUE)

#Wind
try(datauswind<-getEIA(ID=paste0("EBA.",Location,"-ALL.NG.WND.H"), key = key), silent = TRUE)
datauswind <- as.data.frame(datauswind)

datauswind2 <- cbind(rownames(datauswind), data.frame(datauswind, row.names=NULL))
datauswind3 <- datauswind2 %>% separate(`rownames(datauswind)`, c("X", "datauswind"), sep = "X")
datauswind3$Data<- lubridate::ymd_hms(datauswind3$datauswind)
datauswind3$Year <- year(datauswind3$Data)
datauswind3$day <- day(datauswind3$Data)
datauswind3$month <- month(datauswind3$Data)
datauswind3$hour <- hour(datauswind3$Data)

datauswind3$wday <- week(datauswind3$Data)

dataustotal <- data3 %>% select(Data, value)
glimpse(datauswind)
datauswind3 <- merge(datauswind3, dataustotal, by = "Data", all.x = TRUE)


datauscoal3$Type <- "coal"
datauscoal3$datauscoal <- NULL
datauscoal3$sourceprod <- datauscoal3[[3]]
datauscoal3$EBA.US48.ALL.NG.COL.H <- NULL
datauscoal3$emissions <- 1001

glimpse(datauscoal3)

dataushydro3$Type <- "hydro"
dataushydro3$dataushydro <- NULL
dataushydro3$sourceprod <- dataushydro3[[3]]

dataushydro3$EBA.US48.ALL.NG.WAT.H <- NULL
dataushydro3$emissions <- 0
datausother3$Type <- "other"
datausother3$datausother <- NULL
datausother3$sourceprod <- datausother3[[3]]

datausother3$EBA.US48.ALL.NG.OTH.H <- NULL
datausother3$emissions <- 0

glimpse(dataushydro3)
datausnatgas3$Type <- "natgas"
datausnatgas3$datausnatgas <- NULL
datausnatgas3$sourceprod <- datausnatgas3[[3]]
datausnatgas3$EBA.US48.ALL.NG.NG.H <- NULL
datausnatgas3$emissions <- 429


datausnuclear3$Type <- "nuclear"
datausnuclear3$datausnuclear <- NULL
datausnuclear3$sourceprod <- datausnuclear3[[3]]

datausnuclear3$EBA.US48.ALL.NG.NUC.H <- NULL
datausnuclear3$emissions <- 0



datausoil3$Type <- "oil"
datausoil3$datausoil <- NULL
datausoil3$sourceprod <- datausoil3[[3]]

datausoil3$EBA.US48.ALL.NG.OIL.H <- NULL
datausoil3$emissions <- 902



dataussolar3$Type <- "solar"
dataussolar3$dataussolar <- NULL
dataussolar3$sourceprod <- dataussolar3[[3]]

dataussolar3$EBA.US48.ALL.NG.SUN.H <- NULL
dataussolar3$emissions <- 0



datauswind3$Type <- "wind"
datauswind3$datauswind <- NULL
datauswind3$sourceprod <- datauswind3[[3]]

datauswind3$EBA.US48.ALL.NG.WND.H <- NULL
datauswind3$emissions <- 0



df <-list(datauscoal3, dataushydro3, datausnuclear3, datausnatgas3, datausoil3, dataussolar3, datauswind3)
combined<- rbindlist(df, fill = TRUE)



write.csv(combined, paste0(Location,"PowerbySource.csv"))



#Daily plot
datasource <- read.csv("./US48PowerbySource.csv")
combined <- datasource

write.csv(combined, "./combined.csv")
combined$pctprod <- combined$sourceprod / combined$value
combined$day<-yday(combined$Data)
#combined <- subset(combined, day < as.numeric(yday(Sys.Date())))

combined <- combined %>% group_by(Year, day, month, hour, Type) %>% summarise(value = mean(value),  pctprod = mean(pctprod), emissions = mean(emissions))

combined <- subset(combined, !is.na(value))

combined$carbonemissions <- combined$value * combined$pctprod * combined$emissions * .001

combinedday <- combined %>% group_by(Year, day, month, Type) %>% summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions))

combinedday$Year <- as.character(combinedday$Year)
combinedday <- subset(combinedday, day < as.numeric(yday(Sys.Date())))

combineddaytpt <- combinedday %>% dplyr::group_by(Year, day, month) %>% dplyr::summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions, na.rm=TRUE))
combineddaytpt$carbonemissions <-combineddaytpt$carbonemissions/1000000

write.csv(combineddaytpt, "combineddaytpt.csv")

combineddaytpt2 <- combineddaytpt %>% group_by(Year) %>% summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions))



#Historic Emissions
#emissionsadjusted to date
datasource <- read.csv("./US48PowerbySource.csv")
datasource -> combined
combined$pctprod <- combined$sourceprod / combined$value
combined$day<-yday(combined$Data)
combined <- subset(combined, day < as.numeric(yday(Sys.Date())))
combined <- combined %>% group_by(Year, day, month, hour, Type) %>% dplyr::summarise(value = mean(value),  pctprod = mean(pctprod), emissions = mean(emissions))
combined <- subset(combined, !is.na(Year))
combined <- subset(combined, !is.na(value))

combined$carbonemissions <- combined$value * combined$pctprod * combined$emissions * .001
combinedday <- combined %>% group_by(Year, day, month, Type) %>% summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions))
combineddayyear <- combinedday %>% group_by(Year) %>% summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions))

combineddayyear <- subset(combineddayyear, Year >2018)

dif2020<- max(combineddayyear$carbonemissions,na.rm=TRUE) - min(combineddayyear$carbonemissions, na.rm = TRUE)

dif2020 <- dif2020/1000000
combineddayyear <- subset(combineddayyear, !is.na(Year))

historic <- read.csv("./data.csv")
historic$Year <- historic$Electricity.Generation.Sector
historic$carbonemissions <- historic$Fossil.fuel.combustion..carbon.dioxide
historic <- historic %>% select(Year, carbonemissions)
combineddayyear <- combineddayyear %>% select(Year, carbonemissions)
combineddayyear <- subset(combineddayyear, Year > 2018)
combineddayyear$carbonemissions <- combineddayyear$carbonemissions/1000000
historiccombined <- rbind(historic, combineddayyear)
historiccombined$carbonemissions[30]= 1582.69

pred2020 <- 1582.6982 - (dif2020)
historiccombined$carbonemissions[31]= pred2020

historiccombined$proj <- "NA"
historiccombined$proj <- ifelse(historiccombined$Year == 2020, "Projected", "Historic")

write.csv(historiccombined, "historiccombined.csv")

#energy production source

datasource <- read.csv("./US48PowerbySource.csv")
datasource -> combined
combined$pctprod <- combined$sourceprod / combined$value
combined$day<-yday(combined$Data)
combined <- subset(combined, !is.na(combined))

combined <- combined %>% group_by(Year, day, month, hour, Type) %>% dplyr::summarise(value = mean(value),  pctprod = mean(pctprod), emissions = mean(emissions), sourceprod = mean(sourceprod))

combined$carbonemissions <- combined$value * combined$pctprod * combined$emissions * .001

combinedday <- combined %>% group_by(Year, day, month, Type) %>% dplyr::summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions), sourceprod = sum(sourceprod))

combinedday$daysince18 <- ifelse(combinedday$Year == 2019, combinedday$day + 365,
                                 ifelse(combinedday$Year == 2020, combinedday$day + 365 + 365,
                                        combinedday$day))

combinedday$Year <- as.character(combinedday$Year)
combineddaytpt <- combinedday %>% group_by(Year, day, month, daysince18, Type) %>% dplyr::summarise(value = sum(value),  pctprod = mean(pctprod), carbonemissions = sum(carbonemissions), sourceprod = sum(sourceprod))

combineddaytpt$typedes <- ifelse(combineddaytpt$Type == "coal" | combineddaytpt$Type == "natgas" | combineddaytpt$Type == "oil", "Fossil Fuels", "Renewables")
write.csv(combineddaytpt, "combinedsource.csv")


combinedsource <- read.csv("./combined.csv")
combinedsource$Year <- as.character(combinedsource$Year)
combinedsource$pctprod <- combinedsource$sourceprod / combinedsource$value
combinedsource$day<-yday(combinedsource$Data)
combinedsource <- combinedsource %>% group_by(day, Type, Year) %>% dplyr::summarise(sourceprod = sum(sourceprod), pctprod = mean(pctprod))
glimpse(combinedsource)
combinedsource <- subset(combinedsource, !is.na(pctprod))
combinedsource <- subset(combinedsource, pctprod > 0)

combinedsource$daysince18 <- ifelse(combinedsource$Year == 2019, combinedsource$day + 365,
                                    ifelse(combinedsource$Year == 2020, combinedsource$day + 365 + 365,
                                           combinedsource$day))
combinedsource <- subset(combinedsource, daysince18 < max(daysince18-4))

write.csv(combinedsource, "combinedsource.csv")


