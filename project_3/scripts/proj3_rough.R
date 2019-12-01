library(tidyverse)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)

install.packages('zoo')
#Load csv file created in Pandas
umd<-read_csv('data/proj3_umd.csv')

view(umd)

ee<-read_csv('data/proj3_ee.csv')

view(ee)

ee.count

ggplot(ee, aes(x=Duration))+
  geom_histogram(aes(fill=`Reason for Leaving`))

#Plot information about clients
#Recent months homeless by gender
ggplot(umd, aes(x=Recent_Months_Homeless))+geom_bar(aes(fill=Gender))

#Recent months homeless by race
ggplot(umd, aes(x=Recent_Months_Homeless))+geom_bar(aes(fill=Race))

#Histogram of entry age and gender
ggplot(umd, aes(x=Entry_Age))+
  geom_histogram(aes(fill=Gender))+
  labs(x='Entry Age',y='Number of Clients')

#Horizontal bar of previous living situation by previous times homeless
ggplot(umd, aes(x=Prior_Living)) +
  geom_bar(aes(fill=Recent_Times_Homeless))+
  coord_flip()+
  labs(y='Number of Clients',x='Prior Living Situation',fill='Recent Times Homeless')

#Look more closely at most common previous living situations
umd_living <- umd %>%
  filter(Prior_Living =='Place not meant for habitation' | Prior_Living =='Emergency shelter' | Prior_Living =='Family Home' | Prior_Living =='Friend Home')

ggplot(umd_living, aes(x=Prior_Living))+
  geom_bar(aes(fill=Prior_Living_Time))+
  labs(x='Prior Living Situation',y='Number of Clients',fill='Recent Duration Homeless')

dis<-read_csv('data/proj3_dis.csv')

view(dis)

view(umd_living_head)

?theme

ggplot(dis, aes(x=Disability_Type))+
  geom_bar()+
  labs(x='Disability Type',y='Number of Clients')+
  coord_flip()

#These are plots provide some basic information about clients

#I hope to add some statistical analysis to track some trends between variables

#Also, I had an issue with pandas giving me a Memory Error when trying to merge too many files
#Hopefully I will find a way to merge files from my dis_ben file in pandas
