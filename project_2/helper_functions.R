#helper functions

library(tidyverse)
library(shiny)
library(shinydashboard)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)

#Read data from original tsv file
umd<-read_tsv("UMD_Services_Provided_20190719.tsv")


#convert date to standard format
umd<-umd %>%
  mutate(Date=as.Date(Date,"%m/%d/%Y"))

#found typo in client's file
#most 'food pounds' entries were 45, but one was 45121
umd$`Food Pounds`[68024]<-45

#clear out typos

#it appears to be another typo: 5303 instead of 53
umd<-umd %>%
  arrange(-`Food Pounds`)
umd$`Food Pounds`[1]<-53

umd$`Food Pounds`[2]<-37.5

umd$`Food Pounds`[3]<-26.5

umd$`Food Pounds`[4]<-26.5

umd$`Food Pounds`[5]<-26.5

umd$`Food Pounds`[6]<-26.5

umd$`Food Pounds`[7]<-24.5

umd$`Food Pounds`[8]<-24.5

umd$`Food Pounds`[9]<-24.5

umd$`Food Pounds`[10]<-24.5

umd<-umd %>%
  filter(`Client File Number`!=1180)

umd<-umd %>%
  arrange(-`Diapers`)
umd$`Diapers`[1]<-53

#limit analysis to post-2000
umd.2000to2019<-umd %>%
  filter(Date>"2000-01-01" & Date<"2019-10-21")

umd.2000to2019


#Create dataset with summary variables
umd.2000to2019.dur<-umd.2000to2019 %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Visits=n(),
            Frequency=as.numeric(Duration/Visits),
            `Food Pounds Per Visit`=sum(`Food Pounds`,na.rm=TRUE)/Visits,
            `Food Provided for Per Visit`=sum(`Food Provided for`,na.rm=TRUE)/Visits)

#re-join to original dataset
umd.2000to2019<-left_join(umd.2000to2019,umd.2000to2019.dur)

x<-tibble(umd.2000to2019$Duration)

g<-"initialize"
for(i in 1:nrow(x)){
  if(x[i,1]==0){
    g[i]<-paste0("1 Day")
  } else if(x[i,1]>1 & x[i,1]<=183){
    g[i]<-paste0("2 Days-6 Months")
  } else if(x[i,1]>183 & x[i,1]<=365){
    g[i]<-paste0("6-12 Months")
  }else if(x[i,1]>365 & x[i,1]<=730){
    g[i]<-paste0("1-2 Years")
  }else if(x[i,1]>730 & x[i,1]<=1825){
    g[i]<-paste0("2-5 Years")
  }else if(x[i,1]>1825){
    g[i]<-paste0("5+ Years")
  }
  else{
    NULL
  }
}


#create datasets for different time periods
umd.2005to2019<-umd.2000to2019 %>%
  filter(Date>"2005-01-01")

umd.2010to2019<-umd.2005to2019 %>%
  filter(Date>"2010-01-01")

umd.2015to2019<-umd.2010to2019 %>%
  filter(Date>"2015-01-01")

umd.2000to2004<-anti_join(umd.2000to2019,umd.2005to2019)

umd.2005to2009<-anti_join(umd.2005to2019,umd.2010to2019)

umd.2010to2014<-anti_join(umd.2010to2019,umd.2015to2019)

#mark time periods with new variable
umd.2000to2004<-umd.2000to2004 %>%
  mutate(Period="2000-2004")

umd.2005to2009<-umd.2005to2009 %>%
  mutate(Period="2005-2009")

umd.2010to2014<-umd.2010to2014 %>%
  mutate(Period="2010-2014")

umd.2015to2019<-umd.2015to2019 %>%
  mutate(Period="2015-2019")

#re-join datasets with time period marked
umd.2000to2019<-umd.2000to2004 %>%
  full_join(umd.2005to2009) %>%
  full_join(umd.2010to2014) %>%
  full_join(umd.2015to2019)

#remove unused variables
umd.2000to2019 <- umd.2000to2019 %>%
  select(-`Client File Merge`,-`Field1`,-`Field2`,-`Field3`,
  )

umd.2000to2019 <- umd.2000to2019 %>%
  rename(Food_Pounds=`Food Pounds`,
         Clothing_Items=`Clothing Items`,
         Hygiene_Kits=`Hygiene Kits`,
School_Kits=`School Kits`)


varplot <- function(v) {
  if(v==1){
    ggplot(umd.2000to2019, aes_string(x="Date", y="Clothing_Items")) + geom_point()
  }
  else if(v==2){
    ggplot(umd.2000to2019, aes_string(x="Date", y="Diapers")) + geom_point()
  }
  else if(v==3){
    ggplot(umd.2000to2019, aes_string(x="Date", y="Food_Pounds")) + geom_point()
  }
  else if(v==4){
    ggplot(umd.2000to2019, aes_string(x="Date", y="Hygiene_Kits")) + geom_point()
  }
  else if(v==5){
    ggplot(umd.2000to2019, aes_string(x="Date", y="School_Kits")) + geom_point()
  }
}
