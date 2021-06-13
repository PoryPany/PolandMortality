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


dataForMap <- function(year) {
  
  data <- yearlyDeathsByRegions(get(paste("A", year, sep = '')))
  population <- getPopulationData(year)
  data[1] <- population[1]
  
  data <- merge(data, population)
  
  # Necessary to properly show mortality data on map
  dataOrder <- c(12,6,9,1,8,10,14,4,16,3,11,7,5,2,15,13)
  
  return(data[dataOrder,])
  
}


getPopulationData <- function(year) {
  
  # This function loads data from file 'roczna_ludnosc' prepared
  # by Albert Gawin. This file contains population data 
  # separated by years from 2004 to 2020
  
  require("readxl")
  
  population <- data.frame(
    read_excel(
      path = "data/roczna_ludnosc.xlsx"
    )
  )
  
  colnames(population) <- c("Region", paste("Population_", 2004:2020, sep = ''))
  
  
  return(population[c("Region", paste("Population_", year, sep = ''))])
  
}


gendExp <- function(x){
  
  # This function takes Life Expectancy data frame as an argument and splits it
  # into 2 dataframes by sex
  
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

avgWeeklyDeaths <- function(yrs = 2000:2021){
  
    # This function takes data from weeklyDeaths() function and
    # calculates average weekly deaths for every year in our data and
    # creates data frame containing years and average weekly deaths for this years
   
    year <- 2000:2021
    AWD <- c()
    for (yr in yrs){
      AWD[yr-1999] <- mean(weeklyDeaths(get(paste("A",yr,sep="")),
                     yr)$Deaths[!is.na(weeklyDeaths(get(paste("A",
                                         yr,sep="")),yr)$Deaths)])
    }
    awd <- data.frame(year,AWD)
    n <- vapply(awd, is.numeric, FUN.VALUE = logical(1))
    awd[,n] <- round(awd[,n], digits = 0)
    awd[,-2] <- round(awd[,-2],0)
    colnames(awd) <- c("Year","Average weekly deaths")
    assign("aWD",awd,envir = globalenv())
  
  
}
  

