


################################################################
#
# Author: Morgan Gere
# Purpose: Poster Project (Final Project)
#
################################################################



## Data size for project
#(NumberOfColumns * 4) * (NumberOfRows/100) >= 100 
#(13 *4) * (9994/100)>= 100
#(52*99.94) >= 100
#5196.88 > = 100


# loading in data set
superstore <- read.csv("C:/Users/Morga/OneDrive/Desktop/ISchool/IST719_information_Visualization/HW3/SampleSuperstore.csv"
                       , header = TRUE
                       ,stringsAsFactors = T)

colnames(superstore)<-c("Ship.Mode","Segment","Country","City","State","Postal.Code","Region","Category","Sub.Category"
                        , "Sales.Dollars","Quantity.Sold","Discount.Percent","Profit.Dollars") 
# Looking at the rows and columns 
dim(superstore)
# checking the column names
colnames(superstore)
# inspecting the structure of the data
str(superstore)
## Checking for NA Values
table(is.na(superstore))
# loading ggplot2
library(ggplot2)


## Creating a categorical label for profit and profit loss
superstore$Profit<- ifelse(superstore$Profit.Dollars>=0,"Profit","Loss") 


#superstore$addys = addys
View(superstore)

###################################### 1 
## Distribution of profit

## plotting profit distributs
p <- ggplot(superstore)+
  #setting the x to be profit.dollars selecting to color accorrding to if the profit was postive or negative
  aes(x=Profit.Dollars,color=Profit)+
  # Selecting binwidth and the x lim 
  ## This is removing some outliers but will give a better view of a majority of the data 
  geom_histogram(binwidth = 1)+xlim(-1000,1500)
# setting the colors to match profit being green and loss being red  
p <- p + scale_color_manual(values = c("Loss"= "red", "Profit"= "darkgreen"))
# displaying the plot
p

###################################### 2
## Distribution of store per state


# aggregating to Collect stores together (seperated into sales in each category)
superstore.store <- aggregate(superstore$Profit.Dollars, list(superstore$City,superstore$State),sum)
# changing the column names
colnames(superstore.store)<- c("city","state","profit.dollars")
## Plotting the distribution of stores per state
state <- ggplot(superstore.store)+
  # Making the order states with most stores to states with least stores
  aes(x=reorder(state, state, function(x)-length(x)))+geom_bar()
# Rotating the x labels and shifting them so they line up directly with the bars
state <- state + theme(axis.text.x = element_text(angle = 90, vjust = .3, hjust=1))
#Displaying the plot
state

###################################### 3
## sales vs profit

## Plotting sales vs profit
s <- ggplot(superstore)+
  # selecting color of the points to be representations of either profit or loss using Profit created column
  aes(x=Sales.Dollars,y=Profit.Dollars,color=Profit)+
  ## Making the transparency to be more able to see the many points stacked
  geom_point(alpha=.5)
# Setting the lim to better see the points of interest
s <- s + xlim(0,7500) + ylim(-4000,4000)
#selecting specific colors green for profit and red for loss
s <- s + scale_color_manual(values = c("Loss"= "red", "Profit"= "darkgreen"))
# displaying the plot
s

###################################### 4
## Distribution of items sold per sub category

# plotting items sold in each sub category
sc <- ggplot(superstore) + 
  #Selcting sub category for x and ordered from most items to least sold
  aes(x=reorder(Sub.Category, Sub.Category, function(x)-length(x)),y=Quantity.Sold) + 
  # selecting sum and summary to add together the quantity of each item in each sub category
  geom_bar(fun="sum",stat="summary")
# displaying the plot
sc

###################################### 5
## sum of Profit at each discount level
# Plotting the sum of profit at each discount percent applied to sales (changing the color when below zero and above)
d <- ggplot(superstore) + aes(x=Discount.Percent,y=Profit.Dollars) + geom_bar(fun="sum",stat="summary",aes(colour = after_stat(y < 0)))
# changing to the correct colors and adding correct labels
d <- d + scale_colour_manual(values = c("darkgreen","red"),labels=c("Profit","Loss"))
# displaying the plot
d


###################################### 6
# Creating data frames that only contain one category each
superstore.furnature<-superstore[superstore$Category=="Furniture",]
superstore.officesupplies<-superstore[superstore$Category=="Office Supplies",]
superstore.technology<-superstore[superstore$Category=="Technology",]

# Using the data frames created to make a pie chart of each total quantity sold 
#showing the part quantity that is profit and the quantity that is loss

# plotting furniture fill according to profit
cf <- ggplot(superstore.furnature) + aes(x=Category,y=Quantity.Sold,fill=Profit)
# Using sum not count
cf <- cf + geom_bar(fun = "sum",stat = "summary")
# Selecting proper colors
cf <- cf + scale_fill_manual(values = c("Loss"= "red", "Profit"= "darkgreen"))
# changing it to use the polar coordinate system
cf <- cf + coord_polar("y", start=0)
# displaying the plot
cf


## Calculating values to add in adobe
length(superstore.furnature$Profit)
sum(superstore.furnature$Profit=="Profit")
sum(superstore.furnature$Profit=="Loss")
sum(superstore.furnature$Profit=="Profit")/length(superstore.furnature$Profit)
sum(superstore.furnature$Profit=="Loss")/length(superstore.furnature$Profit)



# plotting office fill according to profit
co <- ggplot(superstore.officesupplies) + aes(x=Category,y=Quantity.Sold,fill=Profit)
# Using sum not count
co <- co + geom_bar(fun = "sum",stat = "summary")
# Selecting proper colors
co <- co + scale_fill_manual(values = c("Loss"= "red", "Profit"= "darkgreen"))
# changing it to use the polar coordinate system
co <- co + coord_polar("y", start=0)
# displaying the plot
co

## Calculating values to add in adobe
length(superstore.officesupplies$Profit)
sum(superstore.officesupplies$Profit=="Profit")
sum(superstore.officesupplies$Profit=="Loss")
sum(superstore.officesupplies$Profit=="Profit")/length(superstore.officesupplies$Profit)
sum(superstore.officesupplies$Profit=="Loss")/length(superstore.officesupplies$Profit)


# plotting office fill according to profit
ct <- ggplot(superstore.technology) + aes(x=Category,y=Quantity.Sold,fill=Profit)
# Using sum not count
ct <- ct + geom_bar(fun = "sum",stat = "summary")
# Selecting proper colors
ct <- ct + scale_fill_manual(values = c("Loss"= "red", "Profit"= "darkgreen"))
# changing it to use the polar coordinate system
ct <- ct + coord_polar("y", start=0)
# displaying the plot
ct

## Calculating values to add in adobe
length(superstore.technology$Profit)
sum(superstore.technology$Profit=="Profit")
sum(superstore.technology$Profit=="Loss")
sum(superstore.technology$Profit=="Profit")/length(superstore.technology$Profit)
sum(superstore.technology$Profit=="Loss")/length(superstore.technology$Profit)

## Recreating a categorical label for profit and profit loss
superstore$Profit<- ifelse(superstore$Profit.Dollars>=0,"A Profit","Loss") 

###################################### 7
## Distribution of profit per items per sub category


sc <- ggplot(superstore) + aes(x=reorder(Sub.Category,Sub.Category,
                                         function(x)-length(x))
                               ,y=Profit.Dollars
                               ,fill=Profit
)
sc <- sc + geom_bar(fun = "sum",stat = "summary"
                    #,position = "dodge"
)
sc <- sc + scale_fill_manual(values = c("Loss"= "red", "A Profit"= "darkgreen"))
sc <- sc + facet_grid(Profit~.)
sc


dim(superstore)



#################################### Map
library(maps)
library(mapproj)

# aggregating to find profit in dollars for each store
superstore.store <- aggregate(superstore$Profit.Dollars, list(superstore$City,superstore$State),sum)
# changing the column names
colnames(superstore.store)<- c("city","state","profit.dollars")
# adding a color for profit vs loss
superstore.store$p.color <- ifelse(superstore.store$profit.dollars>=0,"darkgreen","red")
# aggregateing store discounts
avgdiscount <- aggregate(superstore$Discount.Percent, list(superstore$City,superstore$State),mean)
# changing the column names
colnames(avgdiscount)<- c("city","state","avg.discount")
# adding the aggregated discounts to the profit aggregation
superstore.store$avg.discount <- avgdiscount$avg.discount

## Creating a size of the discounts


### Getting geo codes
library(tmaptools)
# setting the length for the loop
i=1:(length(superstore.store$city))

addys <-  paste(superstore.store$city[i],superstore.store$state[i],sep = ',')


g.codes <- geocode_OSM(addys)
# selecing long and lat and full address placing them into a data frame
superstore.store$lon <- g.codes[3]
superstore.store$lat <- g.codes[2]


# assigning size based off discount
superstore.store$dis.size <- ifelse(superstore.store$avg.discount <.2,1,
                                    ifelse(superstore.store$avg.discount >=.2 & superstore.store$avg.discount<.4,2,
                                           ifelse(superstore.store$avg.discount >=.4 & superstore.store$avg.discount<.6,3,
                                                  ifelse(superstore.store$avg.discount >=.6 & superstore.store$avg.discount<.8,4,
                                                         ifelse(superstore.store$avg.discount >=.8 & superstore.store$avg.discount<1,5,6)))))

table(superstore.store$dis.size)

## Plotting an empty map of the US (lower 48)
map("state")
# Adding points based off lat and lon chaning size to the discount and color to profit
points(as.numeric(unlist(superstore.store$lon)),as.numeric(unlist(superstore.store$lat))
       ,col =superstore.store$p.color
       ,cex= superstore.store$dis.size
       ,pch=16
       )
legend(1, 95, legend=c("darkgreen", "red"),col=c("darkgreen", "red"), pch=16)

str(superstore)


superstore$lat




