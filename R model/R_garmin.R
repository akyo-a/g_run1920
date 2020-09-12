##marathon data
library(tidyverse)
library(ggplot2)


#read the csv file
marathon <- read.csv('/Users/aliciakyo/Desktop/Python_Notebooks/garmin_running/running_cals_eaten.csv')

#does dataset filter with times?
index <- marathon$DISTANCE <= 3.00
marathon$DISTANCE[index]

#using only brackets will print out the COLUMN

#How to count number of CM runs
incm <- marathon$LOCATION == 'Costa Mesa Running'
sum(incm)
###more efficient way
incm <- marathon %>% filter(LOCATION =='Costa Mesa Running' ) %>% summarise(max(DISTANCE), freq_run = n()) %>% .$freq_run

#How to print out dataset using tidyverse library
library(tidyverse)
cr <- filter(marathon, LOCATION =='Costa Mesa Running')
#select specific columns from cr
cr_select <- select(cr, LOCATION, DISTANCE, OVERALL)
hbr_select <- marathon %>% select(LOCATION, DISTANCE, OVERALL) %>% filter(LOCATION == 'Huntington Beach Running')
m_select <- marathon %>% select(LOCATION, DISTANCE, OVERALL)
#now, determine if there is a linear relationship
plot(cr_select$OVERALL, cr_select$DISTANCE, col = 'red', xlab = 'Time spent running', ylab = 'Distance')
points(hbr_select$OVERALL, hbr_select$DISTANCE, col = 'cyan')
points(marathon$OVERALL, 0.1068220*marathon$OVERALL)

###the plots show that there is a mostly linear relationship b/t time and distance regardless of distance
####or location. Pace calc by FiveThirtyEight mostly uses race time and race distance, no gender
#based on the visual below, the linear model 


#now for the linear model of marathon df based on distance and time
fit <- lm(marathon$DISTANCE ~ marathon$OVERALL)
summary(fit)
#predicted time from fit for marathon is 243.9513 (4 hours and 4 minutes). 

#making a vector based on the Runner's world (Riegel) formula
marathon$PREDICTED <- marathon$OVERALL*1.06*(26.2/marathon$DISTANCE)
#making a plot to compare the predicted marathon times
plot(marathon$DISTANCE, marathon$PREDICTED, xlab = 'Distance', ylab = 'Predicted Marathon Time')

marathon %>% ggplot(aes(x = reorder(LOCATION, DISTANCE, FUN = mean), y=DISTANCE, color = LOCATION)) +
  geom_boxplot()+geom_point()


