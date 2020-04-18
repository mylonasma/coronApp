## Data import from official data resources to local directory
library(stringr)
library(dplyr)

## Read and Load the countries shapeFile
worldSlow <- readOGR("../data/shapeFiles/countries_50m/ne_50m_admin_0_countries.shp", 'ne_50m_admin_0_countries', encoding='UTF-8')
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
    write.csv(data[data$countriesAndTerritories == world$ADMIN[i],],file = paste("../data/countries/",world$ADMIN[i],".csv",sep = ""))
    fatalRate[i] <- round(sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])/sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])*100,digits = 2)
    currentCases[i] <- sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])
    currentDeaths[i] <- sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])
    currentCasesPer1m[i] <- round((sum(data[data$countriesAndTerritories == world$ADMIN[i],"cases"])*1000000)/(data[data$countriesAndTerritories == world$ADMIN[i],"popData2018"][1]), digits = 0)
    currentDeathsPer1m[i] <- round((sum(data[data$countriesAndTerritories == world$ADMIN[i],"deaths"])*1000000)/(data[data$countriesAndTerritories == world$ADMIN[i],"popData2018"][1]), digits = 0)
    if (!is.na(currentCasesPer1m[i])) {
      if(currentCasesPer1m[i]>=0 & currentCasesPer1m[i] <2){
        currentCasesPer1mColor[i] <- '#d1d8dc'
      }else if(currentCasesPer1m[i]>=2 & currentCasesPer1m[i] <10){
        currentCasesPer1mColor[i] <- '#a3b2b9'
      }else if(currentCasesPer1m[i]>=10 & currentCasesPer1m[i] <50){
        currentCasesPer1mColor[i] <- '#758c97'
      }else if(currentCasesPer1m[i]>=50 & currentCasesPer1m[i] <100){
        currentCasesPer1mColor[i] <- '#466674'
      }else if(currentCasesPer1m[i]>=100 & currentCasesPer1m[i] <500){
        currentCasesPer1mColor[i] <- '#2f5363'
      }else if(currentCasesPer1m[i]>=500 & currentCasesPer1m[i] <1000){
        currentCasesPer1mColor[i] <- '#194052'
      }else if(currentCasesPer1m[i]>=1000 & currentCasesPer1m[i] <5000){
        currentCasesPer1mColor[i] <- '#143341'
      }else if(currentCasesPer1m[i]>=5000 & currentCasesPer1m[i] <10000){
        currentCasesPer1mColor[i] <- '#112c39'
      }else if(currentCasesPer1m[i]>=10000){
        currentCasesPer1mColor[i] <- '#0f2631'
      }      
    }else{
      currentCasesPer1mColor[i] <- '#d1d8dc'
    }
    if (!is.na(currentDeathsPer1m[i])) {
      if(currentDeathsPer1m[i]>=0 & currentDeathsPer1m[i] <2){
        currentDeathsPer1mColor[i] <- '#e68585'
      }else if(currentDeathsPer1m[i]>=2 & currentDeathsPer1m[i] <10){
        currentDeathsPer1mColor[i] <- '#de5d5d'
      }else if(currentDeathsPer1m[i]>=10 & currentDeathsPer1m[i] <50){
        currentDeathsPer1mColor[i] <- '#d63535'
      }else if(currentDeathsPer1m[i]>=50 & currentDeathsPer1m[i] <100){
        currentDeathsPer1mColor[i] <- '#c02f2f'
      }else if(currentDeathsPer1m[i]>=100 & currentDeathsPer1m[i] <500){
        currentDeathsPer1mColor[i] <- '#ab2a2a'
      }else if(currentDeathsPer1m[i]>=500 & currentDeathsPer1m[i] <1000){
        currentDeathsPer1mColor[i] <- '#952525'
      }else if(currentDeathsPer1m[i]>=1000){
        currentDeathsPer1mColor[i] <- '#6b1a1a'
      }      
    }else{
      currentDeathsPer1mColor[i] <- '#e68585'
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
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  # if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}
options("scipen"=100, "digits"=4)
minCases <- min(na.omit(currentState$currentCasesPer1m))
maxCases <- max(na.omit(currentState$currentCasesPer1m))
step <- (maxCases-minCases)/5
# caseLevels <- roundUpNice(seq(minCases, maxCases,step))



## Calculate the total numbers for each country
