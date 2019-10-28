#helper functions
library(rsconnect)
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

#Some typos are obvious: 5303 in one cell when all others are 53.
umd<-umd %>%
  arrange(-`Food Pounds`)
umd$`Food Pounds`[1]<-53

#Other typos appear to be two values entered into one cell: 3738
#Fix by entering in the average of the two.
umd$`Food Pounds`[2]<-37.5

umd$`Food Pounds`[3]<-26.5

umd$`Food Pounds`[4]<-26.5

umd$`Food Pounds`[5]<-26.5

umd$`Food Pounds`[6]<-26.5

umd$`Food Pounds`[7]<-24.5

umd$`Food Pounds`[8]<-24.5

umd$`Food Pounds`[9]<-24.5

umd$`Food Pounds`[10]<-24.5

#One client number has only improbable values for all variables
#Fix by removing that client file.
umd<-umd %>%
  filter(`Client File Number`!=1180)

#Similar straightforward typo in diapers of 5303 instead of 53.
umd<-umd %>%
  arrange(-`Diapers`)
umd$`Diapers`[1]<-53

#limit analysis to post-2000
umd.2000to2019<-umd %>%
  filter(Date>"2000-01-01" & Date<"2019-10-21")



#Create dataset with summary variables
umd.2000to2019.dur<-umd.2000to2019 %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date))

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
umd.2000to2019<-umd.2000to2019 %>%
  mutate(Timeassisted=g)

dur.group.order<-c("1 Day","2 Days-6 Months","6-12 Months","1-2 Years","2-5 Years","5+ Years")

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

umd.final <- umd.2000to2019 %>%
  rename(
          id=`Client File Number`,
          Food=`Food Pounds`,
          Clothing=`Clothing Items`,
          Hygienekits=`Hygiene Kits`,
          Schoolkits=`School Kits`,
          Financial=`Financial Support`,
          Bus=`Bus Tickets (Number of)`)


plot1a <- function(data,service,year) {
    
  serviceperyear= data %>%
      select(Date,service) %>%
      drop_na() %>%
      separate(Date, sep = "-", into = c("Year","Month", "Day"))%>%
      filter(Year>=year[1]&Year<=year[2])%>%
      group_by(Year) %>%
      summarise(sum=n())
    
    p1=ggplot(serviceperyear, aes(x=as.numeric(Year),y=sum)) + 
      geom_point()+
      geom_text(aes(label=sum),vjust=-0.5,size=3)+
      geom_line(size=1,color="lightskyblue")+
      scale_x_continuous(breaks=seq(year[1],year[2],2))+
  labs(x="Year",y= paste('Amount of',service,'Service Provided'),
       title = paste('Total',service,'Service Provided Annually from',min(year),'to',max(year)))
      
    return(p1)
}

plot1b <-function(data,service,year) {
  
  serviceperyear= data %>%
    select(Date,service) %>%
    drop_na() %>%
    separate(Date, sep = "-", into = c("Year","Month", "Day"),remove=FALSE)%>%
    filter(Year>=year[1]&Year<=year[2])
  
  p2=ggplot(serviceperyear, aes_string(x='Date',y=service)) + 
    geom_point(color="navy")+
    labs(x="Year",y= paste('Amount of',service,'Service Provided'),
         title = paste('All Counts of',service,'Service Provided from',min(year),'to',max(year)))
  
  return(p2)
}



plot2<-function(data,points,service,year) {

  serviceperyear= data %>%
    select(id,Date,service,'Timeassisted') %>%
    drop_na() %>%
    separate(Date, sep = "-", into = c("Year","Month", "Day"),remove=FALSE)%>%
    filter(Year>=year[1]&Year<=year[2])
  
  selectedData <- reactive({
    dat <- brushedPoints(serviceperyear, points)
    if (nrow(dat) == 0)
      dat <- serviceperyear
    dat})
  
  ggplot(selectedData(), aes_string(x='Timeassisted'))+
    scale_x_discrete(limit=dur.group.order)+
    scale_fill_discrete(limit=dur.group.order)+
    geom_bar(aes_string(fill='Timeassisted')) +
    theme(axis.text.x=element_blank())+
    labs(x= "Service Duration",y="Number of Clients",
         title="Clients by Service Duration",
         fill="Service Duration")
}


tab<-function(data,points,service,year){
 
   serviceperyear= data %>%
    select(id,Date,service) %>%
    drop_na() %>%
    separate(Date, sep = "-", into = c("Year","Month", "Day"),remove=FALSE)%>%
    filter(Year>=year[1]&Year<=year[2])
  
  selectedData2 <- reactive({
    dat2 <- brushedPoints(serviceperyear, points)
    if (nrow(dat2) == 0)
      dat2 <- serviceperyear
    dat2})
  
  table1<-selectedData2() %>%
    select(id)%>%
    drop_na()%>%
    group_by('idnum'=as.integer(id)) %>%
    summarise(`Service Count`=n())
  
  table2<-data %>%
    mutate('idnum'=as.integer(id))
  
  out_table<-left_join(table1,table2,by='idnum')%>%
    select(`Client ID`='idnum',`Service Count`,`Service Duration`='Timeassisted')%>%
    subset(!duplicated(`Client ID`))%>%
    arrange(-`Service Count`)%>%
    head(n=20)
  
  out_table
}

data=umd.final
