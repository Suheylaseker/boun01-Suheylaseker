library(tidyverse) 
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(rio)
#Data Read
raw_df <- read.csv("C:/Users/Suhey/OneDrive/Masaüstü/IE48A/Airbnb.csv")
raw_df %>% glimpse()
head(raw_df)
#converting data format from csv to rds
export(raw_df, "Airbnb.csv")
saveRDS(raw_df, file = "Airbnb.csv")
convert("Airbnb.csv", "Airbnb.rds")

#Creating a new data frame in order to see the effect of neighbourhood on prices
data1<-raw_df %>% group_by(neighbourhood_group)%>%summarise(AvgPrice=mean(price))

#Visualization of the relation between price and neighbourhood
#scatter plot
ggplot(data1,aes(x=neighbourhood_group,y=AvgPrice,group=1,fill=(neighbourhood_group)))+
 ggtitle("Changes in Prices over Neigbourhood Groups")+ geom_point()+
 geom_line()+
 theme_minimal() +
 labs(x = "Neigbouthood Groups",y = "Average Price Values") +
 theme(axis.text.x = element_text(angle =90,size=7,vjust=0.4))
#bar plot
ggplot(data1,aes(x=neighbourhood_group,y=AvgPrice,fill=(neighbourhood_group))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_minimal() + 
  labs(x="Neighbourhood Groups",y=" Average Price Values",title="Changes in Prices over Neigbourhood Groups") + 
  theme(axis.text.x = element_text(angle=90,size=7,vjust=0.4)) 
#Conclusion: Average price of houses in Manhattan and Brooklyn are more expensive than other neighbourhood groups.

#Creating a new data frame in order to see the effect of room types
data2<-raw_df %>% group_by(room_type)%>%summarise(AvgPrice=mean(price))
#bar plot
ggplot(data2,aes(x=room_type,y=AvgPrice,fill=(room_type))) +
  geom_bar(stat="identity",position="dodge") + 
  theme_minimal() + 
  labs(x="Room Types",y="Average Price Values",title="Changes in Prices over Room Types") + 
  theme(axis.text.x = element_text(angle=90,size=7,vjust=0.4)) 



ggplot(raw_df,aes(x=neighbourhood_group,y=price,group=1,color=room_type))+
  ggtitle(label="Prices over Neigbourhood Groups for Room Types")+
  theme(plot.title = element_text(hjust=1))+
  geom_point()+
  theme_minimal() +
  labs(x = "Neigbourhood Groups",y = "Price Values") +
  theme(axis.text.x = element_text(angle =90,size=7,vjust=0.4))

#Creating a new data frame in order to ss
data3<-raw_df%>%filter(neighbourhood_group=="Bronx")%>%summarise(neighbourhood_group,price,room_type)
data4<-data3%>%group_by(room_type)%>%summarise(AvgPriceBronx=mean(price),.groups = 'drop')

data5<-raw_df%>%filter(neighbourhood_group=="Brooklyn")%>%summarise(price,room_type)
data6<-data5%>%group_by(room_type)%>%summarise(AvgPriceBrooklyn=mean(price),.groups = 'drop')

data7<-raw_df%>%filter(neighbourhood_group=="Manhattan")%>%summarise(price,room_type)
data8<-data7%>%group_by(room_type)%>%summarise(AvgPriceManhattan=mean(price),.groups = 'drop')

data9<-raw_df%>%filter(neighbourhood_group=="Queens")%>%summarise(price,room_type)
data10<-data9%>%group_by(room_type)%>%summarise(AvgPriceQueens=mean(price),.groups = 'drop')

data11<-raw_df%>%filter(neighbourhood_group=="Staten Island")%>%summarise(price,room_type)
data12<-data11%>%group_by(room_type)%>%summarise(AvgPriceStatenIsland=mean(price),.groups = 'drop')

newdata<-inner_join(data4,data6)
newdata<-inner_join(newdata,data8)
newdata<-inner_join(newdata,data10)
newdata<-inner_join(newdata,data12)
summarise(AvgPrice=mean(price))

#visualization of neighbourhood  prices for room types
ggplot(raw_df,aes(x=neighbourhood_group,y=price, fill=room_type)) + geom_bar(stat="identity")+
  labs(x="Neighbourhood Group",y="Price Values",title="Prices over Room Types") + 
  theme(axis.text.x = element_text(angle=90,size=7,vjust=0.4))
#Relation between neighbourhood group and Availability of Room
ggplot(raw_df, aes(x=neighbourhood_group, y=availability_365, fill=neighbourhood_group)) +
  labs(x="Neighbourhood Group",y="Availability",title="Relation between Availability of Rooms and Neigbourhood Groups")+ 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=90,size=7,vjust=0.4))

max(raw_df$price)
min(raw_df$price)
which(raw_df$price == 0)

#Room numbers for different neighbourhood groups
piedata<-aggregate(cbind(count = neighbourhood_group) ~ neighbourhood_group, 
                   data = raw_df, 
                   FUN = function(x){NROW(x)})
#pie chart
ggplot(piedata,aes(x="",y=count,fill=neighbourhood_group)) + 
  labs(x="",y="",title="Number of Rooms in AirBnb in Neighbourhood Groups")+
  geom_bar(stat="identity",width=1) + coord_polar("y")
