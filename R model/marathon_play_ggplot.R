##marathon data
library(tidyverse)
library(ggplot2)


#read the csv file
marathon <- read.csv('/Users/aliciakyo/Desktop/Python_Notebooks/garmin_running/running_cals_eaten.csv')

#attempting to filter for 2019 
##STEP 1: mutate DATE column to type date. It was previously type factor
##STEP 2: filter to only dates in 2019
tw19 <- marathon %>% mutate(DATE = as.Date(DATE)) %>%
  filter(DATE > as.Date('2019-10-31') & DATE < as.Date('2020-01-01')) 

tw19 %>% ggplot(aes(x = DATE, y = DISTANCE)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") #creates a scale on x axis by week


#time series of the whole df with a vert line at Surf City
m_all <- marathon %>% mutate(DATE = as.Date(DATE))

m_dist<- m_all %>%
  ggplot(aes(x = DATE, y = DISTANCE)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  geom_vline(aes(xintercept = as.Date('2020-02-02'), col ='red')) + #fixed by using the function as.Date for filter
  geom_text(aes(x=as.Date('2020-02-02'),y=13,label = 'Surf City Half(Start new cycle)'))
  

#pace
m_pace <- m_all %>%
  ggplot(aes(x = DATE, y = AVG_PACEMIN)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(8,11)) + 
  geom_vline(aes(xintercept = as.Date('2020-02-02'), col ='red')) + #fixed by using the function as.Date for filter
  geom_text(aes(x=as.Date('2020-02-02'),y=10,label = 'Surf City Half(Start new cycle)'))

m_pace



#pace
m_pace <- m_all %>%
  ggplot(aes(x = DATE, y = AVG_PACEMIN)) +
  geom_point() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(8,11)) + 
  geom_vline(aes(xintercept = as.Date('2020-02-02'), col ='red')) + #fixed by using the function as.Date for filter
  geom_text(aes(x=as.Date('2020-02-02'),y=10,label = 'Surf City Half(Start new cycle)')) + 
  geom_hline(aes(yintercept = avg_pace, col ='cyan'))

m_pace



#NOTES: need to decide on how to round the times, if any

#attempting to overlay before and after
##option1 : create two separate dfs (one for before and one for after) then overlay with color


##option 2: create new column and then group with dyplr
m_all<- m_all %>% 
  mutate( SCM = ifelse(as.Date(DATE) > '2020-02-02', 'after', 'before'))

m_all %>%
  ggplot(aes(x = DATE, y = AVG_PACEMIN, colour = SCM)) +
  geom_point() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(limits = c(8,11)) + 
  geom_vline(aes(xintercept = as.Date('2020-02-02'), col ='red')) + #fixed by using the function as.Date for filter
  geom_text(aes(x=as.Date('2020-02-02'),y=10,label = 'Surf City Half(Start new cycle)')) + 
  geom_hline(aes(yintercept = avg_pace, col ='cyan'))
