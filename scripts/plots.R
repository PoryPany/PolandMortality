require("ggplot2")

# Plot#1, showing mortality by age groups
yearlyData <- yearlyDeathsByGroups(A2021)
ggplot(yearlyData, aes(Age, R)) + geom_bar(stat = "identity") + 
  labs(title = "2020 Deaths", x = "Age group", y = "Deaths") +
  ggsave("plots/plot#1.png", width = 13, height = 7)


# Plot#2, showing weekly mortality for specific years
test <- data.frame()
for (year in 2019:2021) {
  
  varName <- paste("A", year, sep = '')
  test <- rbind(test, weeklyDeaths(get(varName), year))
  
}

Weeks <- factor(test$Week, labels = 1:53)
ggplot(test, aes(x = Weeks, y = Deaths, 
                 group = Year, color = Year)) +
  geom_line(size = 3) + ylim(0, max(test$Deaths)) +
  ggsave("plots/plot#2.png", width = 13, height = 7)


# Plot#3, map
data <- yearlyDeathsByRegions(A2021)




# mapa testy
#install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
                   #"googleway", "ggrepel", "libwgeom", "rnaturalearthhires"))
#devtools::install_github("ropensci/rnaturalearthhires")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")

#install.packages("devtools")
library("devtools")

theme_set(theme_bw())

PolandMapData <- ne_countries(scale = "medium", country = "Poland", 
                              returnclass = "sf")

States <- ne_states(country = "Poland", returnclass = "sf")

#link 
#https://ggplot2.tidyverse.org/reference/scale_viridis.html
#https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
ggplot(data = PolandMapData) +
  geom_sf() +
  geom_sf(data = States, aes(fill = data$Deaths)) +
  scale_fill_viridis_c(alpha = 1, begin = 0, end = 1, direction = -1,
                       option = "D", values = NULL, space = "Lab",
                       guide = "colourbar", aesthetics = "colour") +
  coord_sf(xlim = c(13,25), ylim = c(48.75, 55), expand = F) +
  ggsave("plots/plot#3.png", width = 7, height = 7)


#install.packages("gganimate")
library(gganimate)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(tidyverse)
#install.packages("gifski")
library(gifski)
#install.packages("gif")
library(gif)  

anim <- function(x){
  m <- as.factor(x$Sex)
  levels(m) <- c("Male","Female")
p_LE <- x%>% 
  ggplot(aes(x=Age,y=`Probability of death`,group=Sex,color=m)) +
  geom_line(size=1.5) + ylim(0, max(test$Deaths)) +
  ggtitle("Probability of death animation") +
  scale_color_viridis_d("Sex") +
  theme_ipsum() +
  ggsave("animation/LE_plot.png", width = 7, height = 7)

  p_LE.animation = p_LE+
  transition_reveal(Age)+
  view_follow(fixed_y=T)

  #saving animation
  animate(p_LE.animation, duration = 3, fps = 8, width = 800, height = 700,
          renderer = gifski_renderer("animation/LE_animation.gif",loop=T))
  
} 



