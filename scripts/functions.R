# 1 - Country, 2 - Macroregion
# 3 - Region, 4 - Subregion
getByRegions <- function(m_data, choice = 1) {
  
  regions <- m_data[nchar(m_data$RegionCode) == choice + 1,]
  
  return(regions)
  
}

weeklyDeaths <- function(m_data, year) {
  
  m_data <- getByRegions(m_data)
  
  weeklyData <- colSums(m_data[4:length(m_data)])
  weeklyData <- data.frame(Week = names(weeklyData), 
                           Deaths = weeklyData, 
                           row.names = NULL)
  weeklyData <- cbind(Year = year, weeklyData)
  
  weeklyData
  weeklyData$Year <- as.factor(weeklyData$Year)
  weeklyData$Week <- factor(weeklyData$Week, levels = weeklyData$Week)       
  
  return(weeklyData)
  
}

weeklyDeathsByGroups <- function(m_data) {
  
  m_data <- getByRegions(m_data)
  
  groupsData <- m_data[c(1, 4:length(m_data))]
  groupsData$Age <- factor(groupsData$Age, levels = groupsData$Age)
  
  return(groupsData)
  
}

yearlyDeathsByGroups <- function(m_data) {
  
  m_data <- getByRegions(m_data)
  
  yearlyData <- m_data[1]
  yearlyData <- cbind(yearlyData, "R" = rowSums(m_data[4:length(m_data)]))
  yearlyData$Age <- factor(yearlyData$Age, levels = yearlyData$Age)
  
  return(yearlyData)
  
}

# komentarz