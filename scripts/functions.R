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
  yearlyData <- cbind(yearlyData, "R" = rowSums(m_data[4:length(m_data)],
                                                na.rm = T))
  
  yearlyData$Age <- factor(yearlyData$Age, levels = yearlyData$Age)
  
  return(yearlyData)
  
}

yearlyDeathsByRegions <- function(m_data) {
  
  m_data <- getByRegions(m_data,3)
  
  yearlyData <- m_data[3]
  yearlyData <- cbind(yearlyData, "R" = rowSums(m_data[4:length(m_data)],
                                                na.rm = T))
  yearlyData <- yearlyData[yearlyData$Region != "Warszawski stołeczny",]
  yearlyData <- setNames(aggregate(yearlyData$R, 
                                   by = list(yearlyData$Region), 
                                   FUN = sum),
                         c("Region", "Deaths"))
  
  yearlyData$Region <- factor(yearlyData$Region, 
                              levels = yearlyData$Region)
  
  return(yearlyData)
  
}


gendExp <- function(x){
  
  x <- split(x,x$Sex)
  feE <- as.data.frame(x[2])
  colnames(feE) <- c("Sex","Age","Number of survivors",
                        "Probability of death","Number of dead",
                        "Stationary population",
                        "Cumulated stationary population",
                        "Average life expectancy")
  
  mE <- as.data.frame(x[1])
  colnames(mE) <- c("Sex","Age","Number of survivors",
                        "Probability of death","Number of dead",
                        "Stationary population",
                        "Cumulated stationary population",
                        "Average life expectancy")
  
  assign("maleExp", mE, envir = globalenv())
  message("maleExp"," loaded!")
  assign("femaleExp", feE, envir = globalenv())
  message("femaleExp"," loaded!")
}



