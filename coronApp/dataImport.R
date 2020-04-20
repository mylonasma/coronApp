## Data import from official data resources to local directory
library(stringr)
library(dplyr)
library(rgdal)
library(rgeos)
## Read and Load the countries shapeFile
worldSlow <- readOGR("data/shapeFiles/countries_100m/ne_110m_admin_0_countries.shp", 'ne_110m_admin_0_countries', encoding='UTF-8')
world_shp <- gSimplify(worldSlow, tol = 0.01, topologyPreserve = TRUE)
world <- SpatialPolygonsDataFrame(world_shp, data = worldSlow@data)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
data <- data[!(data$countriesAndTerritories == "Cases_on_an_international_conveyance_Japan"),]
data$countriesAndTerritories <- gsub("_", " ", data$countriesAndTerritories)
dataCountries <- (data %>% distinct(data$countriesAndTerritories, .keep_all = T))$countriesAndTerritories
fatalRate = new(Class = "array")
currentCases = new(Class = "array")
currentDeaths = new(Class = "array")
currentCasesPer1m = new(Class = "array")
currentDeathsPer1m = new(Class = "array")
currentCasesPer1mColor = new(Class = "array")
currentDeathsPer1mColor = new(Class = "array")
source("nameCorrections.R")
for (i in 1:length(world$ADMIN)){
  if (!is.na(match(world$ADMIN[i], dataCountries))){
    fatalRate[i] <- round(sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])/sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])*100,digits = 1)
    currentCases[i] <- sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])
    currentDeaths[i] <- sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])
    currentCasesPer1m[i] <- round((sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])*1000000)/(data[data$countriesAndTerritories == world$ADMIN[i],"popData2018"][1]), digits = 0)
    currentDeathsPer1m[i] <- round((sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])*1000000)/(data[data$countriesAndTerritories == world$ADMIN[i],"popData2018"][1]), digits = 0)
    if (!is.na(currentCasesPer1m[i])) {
      if(currentCasesPer1m[i]>=0 & currentCasesPer1m[i] <2){
        currentCasesPer1mColor[i] <- '#F7FBFF'
      }else if(currentCasesPer1m[i]>=2 & currentCasesPer1m[i] <10){
        currentCasesPer1mColor[i] <- '#F7FBFF'
      }else if(currentCasesPer1m[i]>=10 & currentCasesPer1m[i] <50){
        currentCasesPer1mColor[i] <- '#F7FBFF'
      }else if(currentCasesPer1m[i]>=50 & currentCasesPer1m[i] <100){
        currentCasesPer1mColor[i] <- '#F6FAFF'
      }else if(currentCasesPer1m[i]>=100 & currentCasesPer1m[i] <500){
        currentCasesPer1mColor[i] <- '#F5FAFE'
      }else if(currentCasesPer1m[i]>=500 & currentCasesPer1m[i] <1000){
        currentCasesPer1mColor[i] <- '#EDF5FC'
      }else if(currentCasesPer1m[i]>=1000 & currentCasesPer1m[i] <5000){
        currentCasesPer1mColor[i] <- '#E3EEF9'
      }else if(currentCasesPer1m[i]>=5000){
        currentCasesPer1mColor[i] <- '#08306B'
      } 
    }else{
      currentCasesPer1mColor[i] <- '#F7FBFF'
    }
    if (!is.na(currentDeathsPer1m[i])) {
      if(currentDeathsPer1m[i]>=0 & currentDeathsPer1m[i] <2){
        currentDeathsPer1mColor[i] <- '#FFF5F0'
      }else if(currentDeathsPer1m[i]>=2 & currentDeathsPer1m[i] <10){
        currentDeathsPer1mColor[i] <- '#FFF5F0'
      }else if(currentDeathsPer1m[i]>=10 & currentDeathsPer1m[i] <50){
        currentDeathsPer1mColor[i] <- '#FFF3EE'
      }else if(currentDeathsPer1m[i]>=50 & currentDeathsPer1m[i] <100){
        currentDeathsPer1mColor[i] <- '#FFEDE4'
      }else if(currentDeathsPer1m[i]>=100 & currentDeathsPer1m[i] <500){
        currentDeathsPer1mColor[i] <- '#FEE4D8'
      }else if(currentDeathsPer1m[i]>=500){
        currentDeathsPer1mColor[i] <- '#67000D'
      } 
    }else{
      currentDeathsPer1mColor[i] <- '#FFF5F0'
    }
  }else{
    fatalRate[i] <- NA
    currentCases[i] <- NA
    currentDeaths[i] <- NA
    currentCasesPer1m[i] <- NA
    currentDeathsPer1m[i] <- NA
    currentCasesPer1mColor[i] <- '#ffffff'
    currentDeathsPer1mColor[i] <- '#ffffff'
  }
}
currentState <- data.frame(country=world$ADMIN, cases = currentCases, 
                           deaths = currentDeaths, fatalRate = fatalRate, 
                           currentCasesPer1m = currentCasesPer1m, 
                           currentDeathsPer1m = currentDeathsPer1m, 
                           currentCasesPer1mColor = currentCasesPer1mColor,
                           currentDeathsPer1mColor = currentDeathsPer1mColor)

## Create the color map

