# We use dplyr library, which help manipulate data
library(dplyr)
# We use ggplot2 to make plots
library(ggplot2)
# Library used to put several plot in a grid
library(gridExtra)

# We make a copy of the data set
copyAccidents <- nassCDS

#-------------------- Number of registered accidents regarding the age of drivers
# We group our dataset by the age of each people involved in an accident
# We keep only the drivers
# We summarize with a column counting each people involved in an accident depending his age
ageGroup <- copyAccidents %>%
  group_by(ageOFocc, sex) %>%
  filter(occRole == "driver") %>%
  summarise(count = n())
ageGroup

# We define and print a plot correponding to the above variable
agePlot <- ggplot(data = ageGroup, aes(x = ageOFocc, y = count, group = 1, colour = sex)) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95) +
  xlab("Age of the driver") +
  ylab("Number of accidents")
print(agePlot)
#-------------------- 

#-------------------- Number of accident per year
accGroup <- copyAccidents %>%
  group_by(yearacc, occRole) %>%
  summarise(count = n())
accGroup

# We define and print a plot correponding to the above variable
accPlot <- ggplot(data = accGroup, aes(x = yearacc, y = count, colour = occRole, shape = occRole)) %>% 
  + geom_point(size = 4) %>%
  + geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95)
print(accPlot)
#-------------------- 

#-------------------- Relation between speed and number of accident
speedGroup <- copyAccidents %>%
  # We group by speed and death
  group_by(dvcat, dead) %>%
  # We keep only the deadly accidents
  filter(dead == "dead") %>%
  # We count the number of every group made by group_by
  summarise(count = n())
# We tranform the counting made into percentages. So to every speed will be associated the percentage of deadly accidents
speedGroup$density = speedGroup$count/sum(speedGroup$count)*100
# We create a new column with rounded densities
speedGroup$densityRounded = signif(speedGroup$density, digits = 2)
# We use the previously defined rounded density to create a label for every part of the plot and adding "%" to them
speedGroup$densityString <- paste(speedGroup$densityRounded, "%")
speedGroup

# We create the plot
speedPlot <- ggplot(data = speedGroup, aes(x = "", y = density, fill = dvcat)) + 
  geom_bar(stat='identity') +
  # We use coor_polar to transform the bar plot into piechart
  coord_polar("y", start = 0) +
  # We add labels to every part of the plot
  geom_text(aes(label = speedGroup$densityString), position = position_stack(vjust = 0.5), size = 4)
  theme_void() 
print(speedPlot)
#--------------------

#-------------------- Relation between airbag deployment and injuries level
airbagGroup <- copyAccidents %>%
  # We group by speed and injury severity
  group_by(abcat, injSeverity) %>%
  # We throw away the 5 and 6 levels corresponding to unknow levels
  filter(injSeverity < 5) %>%
  summarise(count = n())
airbagGroup

# We create the bar plot
airbagPlot <- ggplot(data = airbagGroup, aes(x = injSeverity, y = count, fill = abcat)) + 
  geom_bar(stat='identity') +
  # We add a x title for the plot
  xlab("Injury level : from no injury to death") +
  # We add a y title for the plot
  ylab("Number of accidents")
print(airbagPlot)
#--------------------

#-------------------- Relation between frontal chocs and injury levels
beltdeadGroup <- copyAccidents %>%
  # We group injury severity and by presence or not of the seatbelt
  group_by(injSeverity, seatbelt) %>%
  # We keep only the deadly accidents
  filter(dead == "dead" & injSeverity < 5) %>%
  summarise(count = n())
# We want a percentage
beltdeadGroup$density = beltdeadGroup$count/sum(beltdeadGroup$count)*100
beltdeadGroup

beltdeadPlot <- ggplot(data = beltdeadGroup, aes(x = injSeverity, y = density, fill = seatbelt)) + 
  geom_bar(stat='identity') +
  xlab("Injury level : from no injury to death") +
  ylab("Percentage of accidents")

frontalGroup <- copyAccidents %>%
  # We group by injury level and by if the choc was frontal or not
  group_by(injSeverity, frontal) %>%
  # We keep only the dealdy accidents
  filter(dead == "dead" & injSeverity < 5) %>%
  summarise(count = n())
frontalGroup$density = frontalGroup$count/sum(frontalGroup$count)*100

# We define a translation vector because the frontal choc column is represented by a 0 or 1 
trans <- c("frontal","non-frontal")
names(trans) <- c(1,0)
# We translate the 0 and 1 of the frontal column by "frontal" or "non-frontal" this is more convenient for the plot legend
frontalGroup$frontalString <- trans[ as.character(frontalGroup$frontal) ]
frontalGroup

frontalPlot <- ggplot(data = frontalGroup, aes(x = injSeverity, y = density, fill = frontalString)) + 
  geom_bar(stat='identity') +
  xlab("Injury level : from no injury to death") +
  ylab("Percentage of accidents")

# We plot the two upper plts side by side using grid.arrange
grid.arrange(beltdeadPlot, frontalPlot, ncol=2)
#--------------------


