# Load Libraries and set working directory
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("~/R/Facebook")

# Read data (saved as csv file from google docs), keep first three columns
x<-read.csv('fb_data.csv',header=T, stringsAsFactors = F)
x<-x[,1:3]
# Rename columns
colnames(x)<-c('AccM','Client','Date')

#Separate date into Year, Month and Date
xc<-x%>%
  mutate(Date=as.Date(Date))%>%
  separate(Date,into=c('Year','Month','Date'),sep='-')

# Drop rows with NAs if any
xc<-na.omit(xc)

# Calculate sum for number of contacts in each month for all years 
xcc<-xc%>%
  group_by(Month)%>%
  summarise(Total=n())

# Calculate total number of clients
uclients<-as.data.frame(unique(xc$Client))
nuclients<-nrow(uclients)
print(nuclients)

# Get Number of contacts per client per month per year
xccz<-xc%>%
  group_by(Client,Month,Year)%>%
  summarise(NContacts=n())

# Drop Number of contacts and calculate % of all unique clients reached for each month and each year 
xcczz<-xccz%>%
  select(Client,Month,Year)%>%
  group_by(Month,Year)%>%
  summarise(PercentReached=n()/nuclients)%>%
  #mutate(MonYea=paste(Month,Year))%>%
  ungroup()%>%
  group_by(Year)

# Plot % of unique clients reached to visually analyze the trend
ggplot(xcczz, aes(x=Month, y=PercentReached, fill=Year)) + 
  geom_bar(stat="identity", position="dodge")
# From the plot it is obvious that October has consistently highest % of clients reached
# Actionable insight for the team is to make announcement in October.
