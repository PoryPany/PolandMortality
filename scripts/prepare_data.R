loadData <- function(genders = "A", years = 2000:2021) {
  
  # This function is loading data from files in 'data' folder.
  # You can choose gender (All, Male, Female) and years 
  # from 2000 to 2021
  
  require("readxl")
  
  for (year in years) {
    for (gender in genders) {
      
      # A - all, M - male, F - female
      if (gender == "A")      sheetPage <- 1
      else if (gender == "M") sheetPage <- 2
      else if (gender == "W") sheetPage <- 3
      
      # From 105-106th row, data is more specific
      mData <- data.frame(
        read_excel(
          path = paste("data/Zgony według tygodni w Polsce_", 
                       year, ".xlsx", sep = ''), 
          sheet = sheetPage, 
          skip = if (year > 2019) 105 else 106,
          .name_repair = "minimal"
        )
      )
      
      # Setting each column length to 56 for plots
      columns <- dim(mData)[2]
      if (columns <= 55) {
        for (week in (columns+1):56)
          mData <- cbind(mData, tempT = NA)
      }
        
      # Setting column names
      names(mData) <- c("Age", "RegionCode", "Region", 
                        paste("T", 1:(length(mData) - 3), sep = ''))
      
      varName <- paste(gender, year, sep = '')
      rows <- dim(mData)[1]
      
      # Error checking
      if (rows == 1862) {
        assign(varName, mData, envir = globalenv())
        message(varName, " loaded!")
      } else {
        warning(1862 - rows, " rows are missing in ", varName, " variable!")
      }
      
    }
  }
  
}

loadLifeExpect <- function(yrs = 1990:2019){
  
  require("readxl")
  
  for (yr in yrs){
    leData <- data.frame(
      read_excel(
        path = "data/tablice_trwania_zycia_1990-2019.xls", 
        sheet = yr-1989, 
        skip = 3, 
      )
    )
    colnames(leData) <- c("Sex","Age","Number of survivors",
                         "Probability of death","Number of dead",
                         "Stationary population",
                         "Cumulated stationary population",
                         "Average life expectancy")
    
    name <- paste("LE",yr,sep= '')
    
    assign(name, leData, envir = globalenv())
    message(name, " loaded!")
  }
  
}
