library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tibble)
library(lubridate)

#Let's read the data into two files:
#One to keep as an original, and one to adjust
#as needed to aid our analysis
UMDsvc.orig<-read_tsv("UMD_Services_Provided_20190719.tsv")
UMDsvc<-read_tsv("UMD_Services_Provided_20190719.tsv")

#Let's have a look at some of the variables
#To see if anything stands out
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_food=sum(`Food Pounds`)) %>%
  drop_na() %>%
  arrange(-total_food)
c12943<-filter(UMDsvc,`Client File Number`==12943)
view(c12943)
view(arrange(UMDsvc,`Client File Number`))

#found typo in client's file
#most 'food pounds' entries were 45, but one was 45121
UMDsvc$`Food Pounds`[68024]<-45
#check for Food Provided for
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_food_provided=sum(`Food Provided for`)) %>%
  drop_na() %>%
  arrange(-total_food_provided)
#no unusual outliers

#check for Clothing Items
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_clothing=sum(`Clothing Items`)) %>%
  drop_na() %>%
  arrange(-total_clothing)
#no unusual outliers

#check for Diapers
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_diapers=sum(`Diapers`)) %>%
  drop_na() %>%
  arrange(-total_diapers)
#no unusual outliers

#check for school kits
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_school_kits =sum(`School Kits`)) %>%
  drop_na() %>%
  arrange(-total_school_kits)
#no unusual outliers

#check for hygiene kits
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_hygiene_kits =sum(`Hygiene Kits`)) %>%
  drop_na() %>%
  arrange(-total_hygiene_kits)
#no unusual outliers

#check for financial support
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_financial_support =sum(`Financial Support`)) %>%
  drop_na() %>%
  arrange(-total_financial_support)
#no unusual outliers

#check for bus tickets
UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(total_bus_tickets =sum(`Bus Tickets (Number of)`)) %>%
  drop_na() %>%
  arrange(-total_bus_tickets)
#no unusual outliers

#how many unique clients?
client_unique <-unique(UMDsvc$`Client File Number`)
length(client_unique)

#how many uniqe client file merge?
merge_unique <-unique(UMDsvc$`Client File Merge`)
length(merge_unique)

#convert "Date" column from characters to dates
UMDsvc<-UMDsvc %>%
  mutate(Date=as.Date(Date,"%m/%d/%Y"))

ggplot(UMDsvc,aes(x=Date,y=`Food Pounds`))+geom_point()
  geom_text(aes(label=Date))
#outliers at 1/3/2018,1/26/2018,1/30/2018,12/12/2017,8/21/2018
ggplot(UMDsvc,aes(x=Date,y=`Food Provided for`))+geom_point()+
  geom_text(aes(label=Date))
#outliers on 11/23/2009, 3/2/2016,5/8/2012,7/15/2010
#let's see what's going on in January, 2018
UMDsvc.jan01.16.18<-filter(UMDsvc,Date=="2018-01-26")
select(UMDsvc.jan01.16.18,`Client File Number`,`Food Pounds`)
#Client File Number '15000' received 5303 lbs food on 2018-01-26

UMDsvc %>%
filter(`Client File Number`==15000) %>%
  select(Date,`Food Pounds`) %>%
  view()
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
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==12719) %>%
  select(`Date`,`Food Pounds`)

#again, we see a similar problem.
#We replace '1934' with the average, (19+34)/2= 26.5

UMDsvc$`Food Pounds`[3]<-26.5
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==13749) %>%
  select(`Date`,`Food Pounds`)

#again, we see a similar problem.
#We replace '1934' with the average, (19+34)/2= 26.5
UMDsvc$`Food Pounds`[4]<-26.5
view(UMDsvc)

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
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==14151) %>%
  select(`Date`,`Food Pounds`)
#this is more similar to the previous ones
#It seems reasonable to replace 1831 with the average again
#(18+31)/2=24.5
UMDsvc$`Food Pounds`[9]<-24.5
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==14318) %>%
  select(`Date`,`Food Pounds`)
#This only has two entries but still likely a typo.
#Will replace 1831 with average of 24.5
UMDsvc$`Food Pounds`[10]<-24.5
view(UMDsvc)
#Client File Number 1180 has a large value for Food Pounds=1700
#And also large number for Food Provided for=200
UMDsvc %>%
  filter(`Client File Number`==1180) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`)

UMDsvc %>%
  filter(`Client File Merge`==21180) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`) %>%
  view()
#Will come back to this one.

#After looking at Food Provided for, can justify
#adjusting three previous examples, as they are likely typos

UMDsvc %>%
  filter(`Client File Number`==16043) %>%
  select(`Date`,`Food Pounds`)

UMDsvc$`Food Pounds`[5]<-26.5
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==16393) %>%
  select(`Date`,`Food Pounds`)

UMDsvc$`Food Pounds`[6]<-26.5
view(UMDsvc)

UMDsvc %>%
  filter(`Client File Number`==1370) %>%
  select(`Date`,`Food Provided for`,`Food Pounds`)

UMDsvc$`Food Pounds`[7]<-24.5
view(UMDsvc)

#for now, I think we have delt with obvious typos and can proceed
#have a look at client duration of assistance
client.duration<-UMDsvc %>%
  group_by(`Client File Number`) %>%
  summarize(Duration=difftime(max(Date),min(Date)))

arrange(client.duration,-Duration)
ggplot(data=client.duration)+geom_histogram(mapping=aes(x=Duration),binwidth=500)

#There appear to be some impossibly long durations.
client.duration %>%
  arrange(-Duration) %>%
view()
#How many of these early dates are there?

UMDsvc %>%
  arrange(Date) %>%
  view()
#Dates appear to be continuous going back until 1991.
#Then they begin to "jump" by 10 year intervals, all the way to 1931!
#Let's limit our analysis to the past 20 years.
#They will likely be more relevant anyways.

UMD.20yr<-UMD.mod %>%
  filter(Date>"2000-01-01" & Date<"2019-09-25")
view(UMD.20yr)

#We can also split our data into different time periods for comparison

UMD.15yr<-UMD.20yr %>%
  filter(Date>"2005-01-01") %>%
  view()

UMD.10yr<-UMD.15yr %>%
  filter(Date>"2010-01-01") %>%
  view()

UMD.5yr<-UMD.10yr %>%
  filter(Date>"2015-01-01") %>%
  view()

UMD.16.20yr<-anti_join(UMD.20yr,UMD.15yr)

UMD.11.15yr<-anti_join(UMD.15yr,UMD.10yr)

UMD.6.10yr<-anti_join(UMD.10yr,UMD.5yr)

#Let's see if there are any patterns in Duration
#binwidth=100 days

ggplot(data=UMD.20yr)+
  geom_histogram(mapping=aes(x=Duration),
                 binwidth=100,color="blue",fill="green")

#There appears to be a huge spike near 0.

#Let's split our data into 2 groups:
#1). Clients who received assistance only 1 time
#2). Clients who received assistance multiple times
client.20yr.sing<-UMD.20yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.20yr.sing<-semi_join(UMD.20yr,client.20yr.sing)
UMD.20yr.mult<-anti_join(UMD.20yr,client.20yr.sing)

client.16.20yr.sing<-UMD.16.20yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.16.20yr.sing<-semi_join(UMD.16.20yr,client.16.20yr.sing)
UMD.16.20yr.mult<-anti_join(UMD.16.20yr,client.16.20yr.sing)

client.15yr.sing<-UMD.15yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.15yr.sing<-semi_join(UMD.15yr,client.15yr.sing)
UMD.15yr.mult<-anti_join(UMD.10yr,client.15yr.sing)

client.11.15yr.sing<-UMD.11.15yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.11.15yr.sing<-semi_join(UMD.11.15yr,client.11.15yr.sing)
UMD.11.15yr.mult<-anti_join(UMD.11.15yr,client.11.15yr.sing)

client.10yr.sing<-UMD.10yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.10yr.sing<-semi_join(UMD.10yr,client.10yr.sing)
UMD.10yr.mult<-anti_join(UMD.10yr,client.10yr.sing)

client.6.10yr.sing<-UMD.6.10yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.6.10yr.sing<-semi_join(UMD.6.10yr,client.6.10yr.sing)
UMD.6.10yr.mult<-anti_join(UMD.6.10yr,client.6.10yr.sing)

client.5yr.sing<-UMD.5yr %>%
  group_by(`Client File Number`) %>%
  summarize(First=min(Date),Last=max(Date)) %>%
  filter(First==Last)

UMD.5yr.sing<-semi_join(UMD.5yr,client.5yr.sing)
UMD.5yr.mult<-anti_join(UMD.5yr,client.5yr.sing)

UMD.20yr.mult.dur<-UMD.20yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

UMD.20yr.mult.dur %>%
  arrange(Frequency) %>%
  view()

UMD.16.20yr.mult.dur<-UMD.16.20yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

UMD.16.20yr.mult.dur

UMD.15yr.mult.dur<-UMD.15yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))



UMD.11.15yr.mult.dur<-UMD.11.15yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

UMD.10yr.mult.dur<-UMD.10yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

UMD.6.10yr.mult.dur<-UMD.6.10yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

UMD.5yr.mult.dur<-UMD.5yr.mult %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Times=n(),
            Freq=format(Duration/Times,digits=3))

ggplot(data=UMD.20yr.mult.dur,aes(x=Duration)) 

#Let's have a look at some histograms of duration of assistance.
#Binwidth of 30 days gives us about 1 bar per month.


ggplot(data=UMD.20yr.mult.dur)+
  geom_histogram(mapping=aes(x=Duration), binwidth=30,
                 color="light blue",fill="blue") +
  labs(title="Duration of Assistance for Clients Last 20 Yrs",
       x="Days Assisted (1 bar = 1 month)",y="Number of Clients") +
  theme_dark()

#Too long of a time frame.
#Let's look at 5-year periods.

ggplot(data=UMD.16.20yr.mult.dur)+
  geom_histogram(mapping=aes(x=Duration), binwidth=30,
                 color="light blue",fill="blue") +
  labs(title="Duration of Assistance for Clients 16-20 Yrs Ago",
       x="Days Assisted (1 bar = 1 month)",y="Number of Clients") +
  theme_dark()

ggplot(data=UMD.11.15yr.mult.dur)+
  geom_histogram(mapping=aes(x=Duration), binwidth=30,
                 color="light blue",fill="blue") +
  labs(title="Duration of Assistance for Clients 11-15 Yrs Ago",
       x="Days Assisted (1 bar = 1 month)",y="Number of Clients") +
  theme_dark()

ggplot(data=UMD.6.10yr.mult.dur)+
  geom_histogram(mapping=aes(x=Duration), binwidth=30,
                 color="light blue",fill="blue") +
  labs(title="Duration of Assistance for Clients 6-10 Yrs Ago",
       x="Days Assisted (1 bar = 1 month)",y="Number of Clients") +
  theme_dark()

ggplot(data=UMD.5yr.mult.dur)+
  geom_histogram(mapping=aes(x=Duration), binwidth=30,
                 color="light blue",fill="blue") +
  labs(title="Duration of Assistance for Clients Last 5 Yrs",
       x="Days Assisted (1 bar = 1 month)",y="Number of Clients") +
  theme_dark()

#Now we are starting to see some possible trends.
#The proportion of clients who only receive assistance for
#one month or less appears to actually be decreasing
#The biggest spike is around 2 months.
#After that, duration appears to decrease exponentially

#Let's add some summary variables for analysis
#These will be grouped by Client File Number.
#Hopefully we can find similarities and trends to analyze.
#'Duration' will measure the total time receiving assistance
#'Freq' will measure how often on average a client receives assistance
#Others are mostly self-explanatory

UMD.20yr.dur<-UMD.20yr %>%
  group_by(`Client File Number`)%>%
  summarize(Duration=max(Date)-min(Date),Visits=n(),
            Freq=format(Duration/Visits,digits=3),
            `Food Pounds Per Visit`=sum(`Food Pounds`,na.rm=TRUE)/Visits,
            `Food Provided for Per Visit`=sum(`Food Provided for`,na.rm=TRUE)/Visits,
            `Financial Support Per Visit`=sum(`Financial Support`,na.rm=TRUE),
            `Clothing Items Per Visit`=sum(`Clothing Items`,na.rm=TRUE)/Visits)

UMD.20yr<-left_join(UMD.20yr,UMD.20yr.dur) %>%
  view()
view(UMD.20yr)

#Let's split the Duration into several categories
#The histogram suggested the duration of assistance 
#Could be split into a few relatively similar groups

x<-tibble(UMD.20yr$Duration)

g<-"initialize"
for(i in 1:nrow(x)){
  if(x[i,1]==0){
    g[i]<-paste0("1 Day")
  } else if(x[i,1]>0 & x[i,1]<=31){
    g[i]<-paste0("2-31 Days")
  } else if(x[i,1]>31 & x[i,1]<=183){
    g[i]<-paste0("1-6 Months")
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
  view(UMD.20yr)

UMD.15yr<-UMD.20yr %>%
  filter(Date>"2005-01-01") 

UMD.10yr<-UMD.15yr %>%
  filter(Date>"2010-01-01") 
  
UMD.5yr<-UMD.10yr %>%
  filter(Date>"2015-01-01")
  
UMD.16.20yr<-anti_join(UMD.20yr,UMD.15yr)

UMD.11.15yr<-anti_join(UMD.15yr,UMD.10yr)

UMD.6.10yr<-anti_join(UMD.10yr,UMD.5yr)

#Let's compare these duration groups with a bar graph

dur.group.order<-c("1 Day","2-31 Days","1-6 Months","6-12 Months","1-2 Years","2-5 Years","5+ Years")

ggplot(data=UMD.20yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`)) +
  labs(x="Time Assisted For",y="Number of Clients",
  title= "Clients' Duration of Assistance, Past 20 Years")+
  coord_flip() + theme(legend.position="none")
  
ggplot(data=UMD.15yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`)) +
  labs(x="Time Assisted For",y="Number of Clients",
       title= "Clients' Duration of Assistance, Past 15 Years")+
  coord_flip() + theme(legend.position="none")

ggplot(data=UMD.10yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`)) +
  labs(x="Time Assisted For",y="Number of Clients",
       title= "Clients' Duration of Assistance, Past 10 Years")+
  coord_flip() + theme(legend.position="none")

ggplot(data=UMD.5yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`)) +
  labs(x="Time Assisted For",y="Number of Clients",
       title= "Clients' Duration of Assistance, Past 5 Years")+
  coord_flip() + theme(legend.position="none")

ggplot(data=UMD.16.20yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`))+
  labs(x="Times",y="Clients",title= "Clients' Duration of Assistance, 16-20 Years Ago")+
  coord_flip() +theme(legend.position="none")

ggplot(data=UMD.11.15yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`))+
  labs(x="Times",y="Clients",title= "Clients' Duration of Assistance, 11-15 Years Ago")+
  coord_flip()+theme(legend.position="none")

ggplot(data=UMD.6.10yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`))+
  labs(x="Times",y="Clients",title= "Clients' Duration of Assistance, 6-10 Years Ago")+
  coord_flip()+theme(legend.position="none")

ggplot(data=UMD.5yr,aes(x=`Time Assisted for`))+
  scale_x_discrete(limit=dur.group.order)+
  geom_bar(aes(fill=`Time Assisted for`))+
  labs(x="Times",y="Clients",title= "Duration of Assistance for Clients, Past 5 Years")+
  coord_flip()+theme(legend.position="none")

#One clear trend is the decrease in clients who only
#received assistance for one day.
#5+ years is by far the biggest proportion
#However, in the past 5 years, 1-2 years and 2-5 years
#categories have also increased.

#Let's try to find some possible difference between
#these groups, to see if any particular types of assistance
#appear to have different associations

ggplot(data=UMD.20yr,aes(x=`Time Assisted for`,
      y=`Food Pounds Per Visit`))+ geom_boxplot()

#Here, a couple outliers has stretched the graph.
#Let's remove those outliers and have another look

ggplot(data=filter(UMD.20yr,`Food Pounds Per Visit`<75),
       aes(x=`Time Assisted for`,y=`Food Pounds Per Visit`))+
  geom_boxplot()+
  scale_x_discrete(limit=dur.group.order)

ggplot(data=filter(UMD.10yr,`Food Pounds Per Visit`<75 & `Food Pounds Per Visit` >0),
       aes(x=`Time Assisted for`,y=`Food Pounds Per Visit`))+
  geom_boxplot()+
  scale_x_discrete(limit=dur.group.order)

ggplot(data=filter(UMD.5yr,`Food Pounds Per Visit`<75 & `Food Pounds Per Visit`>0),aes(x=`Time Assisted for`,
              y=`Food Pounds Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)

#One interesting thing to note: the 1 day category
#Appears to consistently be receiving more food per visit
#Although it is only a slight association

ggplot(data=UMD.20yr,aes(x=`Time Assisted for`,
    y=`Clothing Items Per Visit`))+geom_boxplot()+
  scale_x_discrete(limit=dur.group.order) 

ggplot(data=UMD.10yr,aes(x=`Time Assisted for`,
    y=`Clothing Items Per Visit`))+geom_boxplot()+
  scale_x_discrete(limit=dur.group.order) 

ggplot(data=UMD.5yr,aes(x=`Time Assisted for`,
        y=`Clothing Items Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)       

#Clothing per visit does not appear to have as
#Noticeable a trend

ggplot(data=UMD.20yr,aes(x=`Time Assisted for`,
 y=`Financial Support Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)       

#Let's remove 0-values and see if this shows anything better.

ggplot(data=filter(UMD.20yr,`Financial Support Per Visit`!=0),aes(x=`Time Assisted for`,
 y=`Financial Support Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)

ggplot(data=filter(UMD.15yr,`Financial Support Per Visit`!=0),aes(x=`Time Assisted for`,
 y=`Financial Support Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)

ggplot(data=filter(UMD.10yr,`Financial Support Per Visit`!=0),aes(x=`Time Assisted for`,
  y=`Financial Support Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)

ggplot(data=filter(UMD.5yr,`Financial Support Per Visit`!=0),aes(x=`Time Assisted for`,
  y=`Financial Support Per Visit`))+geom_boxplot()+scale_x_discrete(limit=dur.group.order)

#Financial support appears to have dried up over the past 10 years.
#It also does not show any clear patterns.

