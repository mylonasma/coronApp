## Data import from official data resources to local directory
library(stringr)
library(dplyr)
data <- read.csv("../data/coronaData.csv")
dataCountries <- (data %>% distinct(data$countriesAndTerritories, .keep_all = T))$countriesAndTerritories
for (i in 1:length(dataCountries)){
  write.csv(data[data$countriesAndTerritories == dataCountries[i],],file = paste("../data/countries/",dataCountries[i],".csv",sep = ""))
}
## Read and Load the countries shapeFile
world <- readOGR("../data/shapeFiles/countries_50m/ne_50m_admin_0_countries.shp", 'ne_50m_admin_0_countries', encoding='UTF-8')
