library(tidyverse)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(lubridate)

UMDsvc<-read_tsv("UMD_Services_Provided_20190719.tsv")

#found typo in client's file
#most 'food pounds' entries were 45, but one was 45121
UMDsvc$`Food Pounds`[68024]<-45

#convert "Date" column from characters to dates
UMDsvc<-UMDsvc %>%
  mutate(Date=as.Date(Date,"%m/%d/%Y"))

#clear out typos

UMDsvc %>%
  filter(`Client File Number`==15000) %>%
  select(Date,`Food Pounds`) 

#it appears to be another typo: 5303 instead of 53
UMDsvc<-UMDsvc %>%
  arrange(-`Food Pounds`)
UMDsvc$`Food Pounds`[1]<-53

UMDsvc %>%
  filter(`Client File Number`==14580) %>%
  select(`Date`,`Food Pounds`)

#It appears '3738' was meant to be 37 or 38. 
#We replace with average, 37.5
UMDsvc$`Food Pounds`[2]<-37.5

UMDsvc %>%
  filter(`Client File Number`==12719) %>%
  select(`Date`,`Food Pounds`)

#again, we see a similar problem.
#We replace '1934' with the average, (19+34)/2= 26.5

UMDsvc$`Food Pounds`[3]<-26.5

UMDsvc %>%
  filter(`Client File Number`==13749) %>%
  select(`Date`,`Food Pounds`)

#again, we see a similar problem.
#We replace '1934' with the average, (19+34)/2= 26.5
UMDsvc$`Food Pounds`[4]<-26.5

UMDsvc %>%
  filter(`Client File Number`==16043) %>%
  select(`Date`,`Food Pounds`)

UMDsvc %>%
  filter(`Client File Number`==16393) %>%
  select(`Date`,`Food Pounds`)
#these observations are different.
#the Client File Numbers 16043 and 16393
#only have 1 entry for `Food Pounds`
UMDsvc %>%
  filter(`Client File Number`==1370) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`)
#this observation also only has one non-NA entry for `Food Pounds`

UMDsvc %>%
  filter(`Client File Number`==10576) %>%
  select(`Date`,`Food Pounds`)
#this is more similar to the previous ones
#It seems reasonable to replace 1831 with the average again
#(18+31)/2=24.5
UMDsvc$`Food Pounds`[8]<-24.5

UMDsvc %>%
  filter(`Client File Number`==14151) %>%
  select(`Date`,`Food Pounds`)
#this is more similar to the previous ones
#It seems reasonable to replace 1831 with the average again
#(18+31)/2=24.5
UMDsvc$`Food Pounds`[9]<-24.5

UMDsvc %>%
  filter(`Client File Number`==14318) %>%
  select(`Date`,`Food Pounds`)
#This only has two entries but still likely a typo.
#Will replace 1831 with average of 24.5
UMDsvc$`Food Pounds`[10]<-24.5
#Client File Number 1180 has a large value for Food Pounds=1700
#And also large number for Food Provided for=200
UMDsvc %>%
  filter(`Client File Number`==1180) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`)

UMDsvc %>%
  filter(`Client File Merge`==21180) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`) 
#Will come back to this one.

#After looking at Food Provided for, can justify
#adjusting three previous examples, as they are likely typos

UMDsvc %>%
  filter(`Client File Number`==16043) %>%
  select(`Date`,`Food Pounds`)

UMDsvc$`Food Pounds`[5]<-26.5

UMDsvc %>%
  filter(`Client File Number`==16393) %>%
  select(`Date`,`Food Pounds`)

UMDsvc$`Food Pounds`[6]<-26.5

UMDsvc %>%
  filter(`Client File Number`==1370) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`)

UMDsvc$`Food Pounds`[7]<-24.5

#Dates appear to be continuous going back until 1991.
#Then they begin to "jump" by 10 year intervals, all the way to 1931!
#Let's limit our analysis to the past 20 years.
#They will likely be more relevant anyways.

UMD.20yr<-UMDsvc %>%
  filter(Date>"2000-01-01" & Date<"2019-09-25")

#We can also split our data into different time periods for comparison

UMD.15yr<-UMD.20yr %>%
  filter(Date>"2005-01-01")

UMD.10yr<-UMD.15yr %>%
  filter(Date>"2010-01-01")

UMD.5yr<-UMD.10yr %>%
  filter(Date>"2015-01-01")

UMD.16.20yr<-anti_join(UMD.20yr,UMD.15yr)

UMD.11.15yr<-anti_join(UMD.15yr,UMD.10yr)

UMD.6.10yr<-anti_join(UMD.10yr,UMD.5yr)

UMD.16.20yr<-UMD.16.20yr %>%
  mutate(Period="2000-2004")

UMD.11.15yr<-UMD.11.15yr %>%
  mutate(Period="2005-2009")

UMD.6.10yr<-UMD.6.10yr %>%
  mutate(Period="2010-2014")

UMD.5yr<-UMD.5yr %>%
  mutate(Period="2015-2019")

UMD.20yr<-UMD.16.20yr %>%
  full_join(UMD.11.15yr) %>%
  full_join(UMD.6.10yr) %>%
  full_join(UMD.5yr)
#Food appears to be the variable with most complete data for analysis
#remove variables we will not use in this analysis
UMD.20yr <- UMD.20yr %>%
  select(-`Client File Merge`,-`Bus Tickets (Number of)`,
         -`Notes of Service`,-Referrals,-`Type of Bill Paid`,
         -`Payer of Support`,-`Field1`,-`Field2`,-`Field3`,
         -Diapers,-`School Kits`,-`Hygiene Kits`,-`Financial Support`
  )

UMD.20yr.dur<-UMD.20yr %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Visits=n(),
            Frequency=as.numeric(Duration/Visits),
            `Food Pounds Per Visit`=sum(`Food Pounds`,na.rm=TRUE)/Visits,
            `Food Provided for Per Visit`=sum(`Food Provided for`,na.rm=TRUE)/Visits)


UMD.20yr<-left_join(UMD.20yr,UMD.20yr.dur)

ggplot(filter(UMD.20yr,Duration>0))+
  geom_histogram(mapping=aes(x=as.numeric(Duration),fill=Period),
                 binwidth=60)+
  facet_wrap(vars(Period))+
  labs(title="Number of Clients by Duration Assisted", x="Duration",y="Number of Clients")

ggplot(filter(UMD.20yr,Frequency>0 & Frequency<500))+
  geom_histogram(aes(x=Frequency,fill=Period),
                 binwidth=7)+
  labs(title="Frequency of Assistance for Repeat Clients", x="Days per Visit",y="Number of Clients") +
  facet_wrap(vars(Period))

ggplot(filter(UMD.20yr,Duration==0),color=Period)+
  geom_bar(aes(x=Period,fill=Period))+
  theme(legend.position="none")+
  labs(y="Clients",title="Number of Single-Visit Clients by Period")

ggplot(filter(UMD.20yr,Duration==0))+
  geom_bar(aes(x=month(Date,label=TRUE),fill=month(Date)))+
  theme(legend.position="none")+
  labs(x="Month",y="Visits",title="Visits per Month, 2000-2019")

ggplot(filter(UMD.20yr,Duration==0))+
  geom_bar(aes(x=month(Date,label=TRUE),fill=month(Date)))+
  theme(legend.position="none")+
  labs(x="Month",y="Visits",title="Visits per Month by Period")+
  facet_wrap(vars(Period))

#Let's split the Duration into several categories
#The histogram suggested the duration of assistance 
#Could be split into a few relatively similar groups

x<-tibble(UMD.20yr$Duration)

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

UMD.20yr<-UMD.20yr %>%
  mutate(`Time Assisted for`=g)

UMD.15yr<-UMD.20yr %>%
  filter(Date>"2005-01-01") 

UMD.10yr<-UMD.15yr %>%
  filter(Date>"2010-01-01") 

UMD.5yr<-UMD.10yr %>%
  filter(Date>"2015-01-01")

UMD.16.20yr<-anti_join(UMD.20yr,UMD.15yr)

UMD.11.15yr<-anti_join(UMD.15yr,UMD.10yr)

UMD.6.10yr<-anti_join(UMD.10yr,UMD.5yr)

dur.group.order<-c("1 Day","2 Days-6 Months","6-12 Months","1-2 Years","2-5 Years","5+ Years")

ggplot(data=UMD.20yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  scale_fill_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`)) +
  facet_wrap(vars(Period)) +
  theme(axis.text.x=element_blank()) +
  labs(x="Time Assisted For",y="Number of Clients",
       title= "Clients' Duration of Assistance, 2000-2019")