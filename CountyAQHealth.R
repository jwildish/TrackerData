library(tigris)
library(sf)
counties <- counties(state = NULL, cb = FALSE, resolution = "500k")
dataset2<- read.csv("https://raw.githubusercontent.com/jwildish/TrackerData/master/dataset2pm25test.csv")

dataset2 <- subset(dataset2, !is.na(results.coordinates.latitude))
dataset2 <- subset(dataset2, !is.na(results.coordinates.longitude))

my.sf.point <- st_as_sf(x = dataset2, 
                        coords = c("results.coordinates.longitude", "results.coordinates.latitude"),
                        crs = "+proj=longlat +datum=WGS84")
countries <- read.csv("./countries.csv")
my.sf.point <- merge(my.sf.point, countries, by = "results.country")
my.sf.pointpm25 <- subset(my.sf.point, results.parameter =="pm25")

counties <- st_as_sf(counties)
my.sf.pointpm25 <- sf::st_transform(my.sf.pointpm25, sf::st_crs(counties))

intersection <- st_intersection(counties, my.sf.pointpm25)
countypop <- read.csv("./countypop.csv")

countypop <- countypop %>% select(REGION, STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2019)

intersection$STATEFP <- as.numeric(intersection$STATEFP)
countypop$STATEFP <- countypop$STATE
countypop$NAMELSAD <- countypop$CTYNAME
mergedf <- merge(intersection, countypop, by = c("STATEFP", "NAMELSAD"))
mergedf$geometry <- NULL
mergedf <- mergedf %>% dplyr::group_by(NAMELSAD, POPESTIMATE2019, STATEFP, NAMELSAD) %>% dplyr::summarise(valuemax = max(valuemax), valuemean= mean(valuemean))

counties <- counties %>% select(STATEFP, NAMELSAD)
counties$STATEFP <- as.numeric(counties$STATEFP)
mergedf <- merge(mergedf, counties, by = c("STATEFP", "NAMELSAD"))
mergedf <- st_as_sf(mergedf)
mergedf <- st_sf(mergedf)

library(leaflet)
#mergedf <- as(mergedf, 'Spatial')
risk.bins <-c(0, 6, 12, 35, 55,155)
pal <- colorBin(palette = "RdYlBu", domain = mergedf$valuemean, reverse = TRUE, bins = risk.bins)
risk.bins2 <-c(0, 100, 250, 500, 1000)
pal2 <- colorBin(palette = "Reds", domain = mergedf$valuemax, bins = risk.bins2)#Set the color palette for the map
glimpse(mergedf)
mergedf <- subset(mergedf, STATEFP != 2)
mergedf <- subset(mergedf, STATEFP != 15)
mergedf <- st_cast(mergedf)
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4748718/
#2.4 counterfactual - https://www.pnas.org/content/115/38/9592
mergedf$mortality <- (mergedf$valuemean-2.4 * 2.03)/100
mergedf$mortality <- ifelse(mergedf$mortality < 0, 0, mergedf$mortality)
mergedf$mortality <- mergedf$mortality * mergedf$POPESTIMATE2019 * 863/100000
sum(mergedf$mortality)
sum(mergedf$POPESTIMATE2019)
st_write(mergedf, "CountyAQRisk.shp", driver="ESRI Shapefile") 

leaflet(data = mergedf) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = mergedf,
              stroke = TRUE, weight =1, opacity = mergedf$valuemax/100 , color= ~pal2(mergedf$valuemax),
              fillColor = ~pal(mergedf$valuemean),
              fillOpacity = .8) %>%
  addLegend(data = mergedf, "bottomright", 
            pal = pal, 
            values = ~(mergedf$valuemean),
            title = "Average (Fill Color)",
            opacity = 1) %>%
  addLegend(data = mergedf, "bottomright", 
            pal = pal2, 
            values = ~(mergedf$valuemax),
            title = "1 Hour Max (Outline)",
            opacity = 1) 


st_write(mergedf, "CountyAQRisk.shp", driver="ESRI Shapefile") 

glimpse(mergedf)
glimpse(countypop)
