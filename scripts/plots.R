require("ggplot2")

# Plot#1 showing mortality by age groups
yearlyData <- yearlyDeathsByGroups(A2020)
ggplot(yearlyData, aes(Age, R)) + geom_bar(stat = "identity") + 
  labs(title = "2020 Deaths", x = "Age group", y = "Deaths")


# Plot#2 showing weekly mortality for specific year
test <- data.frame()
for (year in 2019:2021) {
  
  varName <- paste("A", year, sep = '')
  test <- rbind(test, weeklyDeaths(get(varName), year))
  
}

ggplot(test, aes(x = Week, y = Deaths, 
                 group = Year, color = Year)) +
  geom_line(size = 3) + ylim(0, max(test$Deaths))
