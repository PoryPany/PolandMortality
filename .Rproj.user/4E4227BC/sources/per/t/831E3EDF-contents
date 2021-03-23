loadData <- function(genders = "A", years = 2000:2021) {
  
  require("readxl")
  
  for (year in years) {
    for (gender in genders) {
      
      if (gender == "A")      sheetPage <- 1
      else if (gender == "M") sheetPage <- 2
      else if (gender == "W") sheetPage <- 3
      
      # From 105-106th row, data is more specific
      mData <- data.frame(
        read_excel(
          path = paste("data/", year, ".xlsx", sep = ''), 
          sheet = sheetPage, 
          skip = if (year > 2019) 105 else 106, 
          .name_repair = "minimal"
        )
      )
      
      columns <- dim(mData)[2]
      if (columns <= 55) {
        for (week in (columns+1):56)
          mData <- cbind(mData, tempT = NA)
      }
        
      names(mData) <- c("Age", "RegionCode", "Region", 
                        paste("T", 1:(length(mData) - 3), sep = ''))
      
      varName <- paste(gender, year, sep = '')
      rows <- dim(mData)[1]
      
      if (rows == 1862) {
        assign(varName, mData, envir = globalenv())
        message(varName, " loaded!")
      } else {
        warning(1862 - rows, " rows are missing in ", varName, " variable!")
      }
      
    }
  }
  
}
