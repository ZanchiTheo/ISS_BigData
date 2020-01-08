# We use dplyr library, which help manipulate data
library(dplyr)
# We use ggplot2 to make plots
library(ggplot2)

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
  geom_point(size=3) +
  geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95)
print(agePlot)
#-------------------- 

#-------------------- Number of registered accidents regarding the age and sex of drivers
# We group our dataset by the age of each people involved in an accident
# We keep only the drivers
# We summarize with a column counting each people involved in an accident depending his age
ageGroup <- copyAccidents %>%
  group_by(ageOFocc) %>%
  filter(occRole == "driver") %>%
  summarise(count = n())
ageGroup

# We define and print a plot correponding to the above variable
agePlot <- ggplot(data = ageGroup, aes(x = ageOFocc, y = count, group = 1)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  geom_smooth(method = "loess", se = TRUE, fullrange = FALSE, level = 0.95)
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

#-------------------- Evolution of belted people between 1997 and 2002 involved in an accident
carAgeGroup <- copyAccidents %>%
  group_by(yearacc, seatbelt) %>%
  summarise(count = n())
carAgeGroup

carAgePlot <- ggplot(data = carAgeGroup, aes(x = yearacc, y = count, fill = seatbelt)) + geom_area()

print(carAgePlot)
#-------------------- 

#-------------------- Comparison of belted and not belted people for each injury level
beltGroup <- copyAccidents %>%
  group_by(injSeverity, seatbelt) %>%
  summarise(count = n())
beltGroup$density = beltGroup$count/sum(beltGroup$count)*100
beltGroup

beltPlot <- ggplot(data = beltGroup, aes(x = injSeverity, y = density, fill = seatbelt)) + 
  geom_bar(stat='identity')

print(beltPlot)
#--------------------

#-------------------- Comparison of belted and not belted people for each injury level
beltdeadGroup <- copyAccidents %>%
  group_by(injSeverity, seatbelt) %>%
  filter(dead == "dead") %>%
  summarise(count = n())
beltdeadGroup$density = beltdeadGroup$count/sum(beltdeadGroup$count)*100
beltdeadGroup

beltdeadPlot <- ggplot(data = beltdeadGroup, aes(x = injSeverity, y = density, fill = seatbelt)) + 
  geom_bar(stat='identity')

print(beltdeadPlot)
#--------------------

#-------------------- Relation between speed and number of accident
speedGroup <- copyAccidents %>%
  group_by(dvcat, dead) %>%
  summarise(count = n())
speedGroup$density = speedGroup$count/sum(speedGroup$count)*100
speedGroup

speedPlot <- ggplot(data = speedGroup, aes(x = dvcat, y = density, fill = dead)) + 
  geom_bar(stat='identity')

print(speedPlot)
#--------------------

#-------------------- Relation between airbag deployment and injuries level
airbagGroup <- copyAccidents %>%
  group_by(abcat, injSeverity) %>%
  summarise(count = n())
airbagGroup$density = airbagGroup$count/sum(airbagGroup$count)*100
airbagGroup

airbagPlot <- ggplot(data = airbagGroup, aes(x = injSeverity, y = count, fill = abcat)) + 
  geom_bar(stat='identity')

print(airbagPlot)
#--------------------

#-------------------- Relation between frontal chocs and injury levels
frontalGroup <- copyAccidents %>%
  group_by(injSeverity, frontal) %>%
  filter(dead == "dead") %>%
  mutate(frontalGroup = replace(frontal, frontal==0, "non-frontal")) %>%
  mutate(frontalGroup = replace(frontal, frontal==0, "frontal")) %>%
  summarise(count = n())
frontalGroup$density = frontalGroup$count/sum(frontalGroup$count)*100
frontalGroup

frontalPlot <- ggplot(data = frontalGroup, aes(x = injSeverity, y = density, fill = frontal)) + 
  geom_bar(stat='identity')

print(frontalPlot)
#--------------------


