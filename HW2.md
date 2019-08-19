---
title: "STAT 380 Exercises"
author: "Jacob Padden, Daniel Oh, Chris Steeves, Victor Nguyen"
date: "8/14/2019"
output: html_document
---

QUESTION 1: Green Building

```{r}
library(ggplot2)
library(dplyr)
```

```{r}
setwd("~/STA380-master/STA380-master/data")
gb = read.csv("greenbuildings.csv")
gb$green_rating = as.logical(gb$green_rating)
gb = filter(gb, leasing_rate>=.1)
```

The first issue we immediately noticed with the Excel Guru's analysis is their valuation of rent of green versus non-green buildings. They considered this green factor as independent from all other factors, so we wanted to see how rent varied considerring multiple factors. We wanted to compare rent prices for buildings similar to one another as opposed to with every building in the dataset.

The first variable we explored was age. We started by plotting rent over building age and differentiating green building by coloring those points green.

```{r}
ggplot(data=gb) + geom_point(aes(age, Rent, color=green_rating)) + scale_color_manual(values=c("black", "#4EC62A"))
```

Looking at the plot, it's very hard to tell if green buildings have higher rent values than non green buildings. To more closely examine this, we will group by age and take the median.

```{r}
gb_age = group_by(gb, age, green_rating)
ggplot(data=summarize(gb_age, Median_Rent = median(Rent))) + geom_point(aes(age, Median_Rent, color=green_rating)) + scale_color_manual(values=c("black", "#4EC62A"))
```

When doing this, it's a little more coherent. Specifically when considerring newer buildings younger than 20 years old, it appears that median rent for green vs non-green does not differentiate much. Going forward, we should consider younger apartments since they're more reflective of the apartment the company is building.

We should additionally consider how rent of green vs non-green buildings compare within clusters. We also filter for only apartments 20 years old or younger.

```{r}
gb_cluster = group_by(filter(gb, age <= 20), cluster, green_rating)
#ggplot(data=summarize(gb_cluster, Median_Rent = median(Rent))) + geom_point(aes(cluster, Median_Rent, color=green_rating)) + scale_color_manual(values=c("black", "#4EC62A"))
gb_cluster_green = filter(summarize(gb_cluster, Rent = median(Rent)), green_rating==TRUE)
gb_cluster_nongreen = filter(summarize(gb_cluster, Rent = median(Rent)), green_rating==FALSE)
gb_cluster_merged = merge(gb_cluster_green, gb_cluster_nongreen, by="cluster", suffixes = c("_green","_nongreen"))
gb_cluster_merged$Difference = gb_cluster_merged$Rent_green-gb_cluster_merged$Rent_nongreen
print("Average within cluster difference between rent of green buildings and Median rent of non-green buildings")
mean(gb_cluster_merged$Difference)
```

By filterring by age and only comparing to buildings within the same clusters, we see that green buildings only have higher rent of 1.02 per square foot, much smaller than the Excel Guru's claim of 2.60.

```{r}
5000000/(1.02*250000)
```

Using this valuation and assuming 100% occupancy, the investors can only expect to recoup their extra investment in 19.6 years as opposed to the initial claim of 7.7. 

We additionally wanted to revisit the guru's assumption of 90%-100% occupancy. Because the anticipated payoff period is so long, small deviations in average occupancy can affect the expected return.

Filterring on age <= 20 and green buildings, we observe the following:

```{r}
filtered_gb = filter(gb, green_rating==TRUE & age <=20)
print("Proportion of 100% occupancy:")
nrow(filter(filtered_gb, leasing_rate==100))/nrow(filtered_gb)
print("Looking at some quantiles")
quantile(filtered_gb$leasing_rate, .75)
quantile(filtered_gb$leasing_rate, .5)
quantile(filtered_gb$leasing_rate, .25)
quantile(filtered_gb$leasing_rate, .05)
```

We see that 90% occupancy is a good estimate for the expected occupancy rate. By looking at the various quantiles, however, we should still consider other options. Here are some observations:
  Only 14% of these buildings have 100% occupancy.
  A little over 50% of these buildings have 90% occupancy or higher.
  75% of these buildings have 85% occupancy or higher.
  95% of these buildings have 65% occupancy or higher. 

It's important to note that theres slightly less than 50% of having occupancy less than 90%. To be ultra conservative, we can say that there's only a 5% chance of having occupancy of 65% or less.

We can visualize these potential payoff periods. 

```{r}
additionalRevenue <- data.frame(double(), double(), integer(), double())
occupancy_list = c(.65,.9,1)
for (rate in occupancy_list){
  year = 0
  while(year <= 30){
    additionalRevenue = rbind(additionalRevenue, c(1.02, rate, year, 1.02*year*250000*rate))
    year = year+1
  }
}
year = 0
while(year <= 25){
    additionalRevenue = rbind(additionalRevenue, c(2.60, .9, year, 2.60*year*250000))
    year = year+1
  }
colnames(additionalRevenue) <- c("additional_rent", "occupancy_rate", "year", "additional_revenue")
additionalRevenue$additional_rent = as.factor(additionalRevenue$additional_rent)
additionalRevenue$occupancy_rate = as.factor(additionalRevenue$occupancy_rate)

temp = filter(additionalRevenue, additional_rent==1.02)
temp$occupancy_rate = as.factor(temp$occupancy_rate)
ggplot() + 
  geom_line(data = filter(additionalRevenue, additional_rent==2.60), aes(year, additional_revenue), color="red", linetype="solid") +
  geom_line(data = temp, aes(year, additional_revenue, linetype=occupancy_rate)) +
  scale_linetype_manual(values=c("dotted","twodash", "solid")) +
  coord_cartesian(ylim=c(0,6000000)) +
  geom_hline(yintercept=5000000, linetype="solid", color = "#4EC62A") +
  ggtitle("Expected Payoff") + 
  ylab("Additional Revenue") +
  xlab("Year")

 #plot(ecdf(filter(gb, green_rating==TRUE & age <=20)$leasing_rate))
#quantile(filter(gb, green_rating==TRUE & age <=20)$leasing_rate, .25)
```

The initial estimate is displayed in red. The black lines are our estimates. Best case scenario, the investors will only make back their money by year 20. More realistically, they could expect to break even between years 22 and 23. Conservatively, they can expect their money back after 30 years. 

There are other factors to consider that we weren't able to cover in our analysis. Given the data, it was difficult to quantify annual costs for a building, so if a green building has less annual operating expenses than a non-green building, the net payoff may occur earlier. Additionally, employee happiness and productivity could be higher in green buildings, but we don't have a way to quantify this. 

Given the current analysis, we think the Excel Guru's estimate is too optimistic. He anticipates payoffs in year 7 or 8, but it's more realistic to expect payoffs in year 22 or later. In our anlaysis, even in best case scenario the payoff won't occur until at least year 20. The investors may be better off investing the additional $5M unless they can uncover additional benefits of green buildings.



Question 2: flights at ABIA
```{r}
library(ggplot2)
library(dplyr)
setwd("~/STA380-master/STA380-master/data")
abia <- read.csv("ABIA.csv")

#Creating dummy variable
abia$dummy <- 1
attach(abia)
#Converting numeric to factors 
Month <- as.factor(Month)
DayofMonth <- as.factor(DayofMonth)
DayOfWeek <- as.factor(DayOfWeek)
```

```{r multiplot}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```


Checking out which airports are the best to fly from. In this case, US and F9 have the lowest percentage of delayed flights with very short delays relative to the other airlines.
```{r}
#Creating subsets of the data for all flights leaving Austin as well as when they were delayed.
departABIA <- subset(abia, Origin == "AUS")
delayABIA <- subset(departABIA, DepDelay > 0)

#Sum of delayed flights by each airline
delayAirline <- aggregate(delayABIA$dummy,by=list(delayABIA$UniqueCarrier),FUN = sum, na.rm=TRUE)
names(delayAirline)[1] <- "Airline"
names(delayAirline)[2] <- "DelaysAnnually"

#Average delay time by Airline
avgDelayAir <- aggregate(delayABIA$DepDelay,by=list(delayABIA$UniqueCarrier),FUN = mean, na.rm=TRUE)
names(avgDelayAir)[1] <- "Airline"
names(avgDelayAir)[2] <- "AverageDelayTime"

#Sum of delayed flights by all airlines
totalAirline <- aggregate(departABIA$dummy,by=list(departABIA$UniqueCarrier),FUN = sum, na.rm=TRUE)
names(totalAirline)[1] <- "Airline"
names(totalAirline)[2] <- "FlightsAnnually"

#Merging the dataframes
delayPerc <- merge(totalAirline,delayAirline, by = "Airline")
delayPerc <- merge(delayPerc, avgDelayAir, by = "Airline")
delayPerc$perc <- round(delayPerc$DelaysAnnually/totalAirline$FlightsAnnually,3)*100
delayPerc$AverageDelayTime <- round(delayPerc$AverageDelayTime, 2)

#Percent of delayed flights by airline
p1 <- ggplot(delayPerc, aes(x=delayPerc$Airline, y=delayPerc$perc, fill = delayPerc$Airline)) + geom_col() + geom_text(aes(label = delayPerc$perc ), vjust = -0.5) + ylim(0,100) + xlab("") + ylab("Percent of Flight Delayed") + theme(legend.position = 'none',)

#Average delays by airline
p2 <- ggplot(delayPerc, aes(x=delayPerc$Airline, y=delayPerc$AverageDelayTime, fill = delayPerc$Airline)) + geom_col() + geom_text(aes(label = delayPerc$AverageDelayTime ), vjust = -0.5) + ylim(0,60) + xlab("Airlines") + ylab("Average Delay in Minutes") + theme(legend.position = 'none') 

multiplot(p1, p2, col = 1)

```


Now looking at delays by airport,there is an outlier to be ignored. The DSM airport has a 100% delay rate with a delay of 129 minutes. However, this was for only 1 delay, so we can't reliably say that it will happen again. It would be our recommendation to arrive to the airport earlier than normal if flying to EWR, IAD, and STL.
```{r}
#Sum of delays by each destination
delayDest <- aggregate(delayABIA$dummy,by=list(delayABIA$Dest),FUN = sum, na.rm=TRUE)
names(delayDest)[1] <- "Destination"
names(delayDest)[2] <- "DelaysAnnually"

#Average delay time by destination
avgDelay <- aggregate(delayABIA$DepDelay,by=list(delayABIA$Dest),FUN = mean, na.rm=TRUE)
names(avgDelay)[1] <- "Destination"
names(avgDelay)[2] <- "AverageDelayTime"

#Sum of delays all destinations
totalDest <- aggregate(departABIA$dummy,by=list(departABIA$Dest),FUN = sum, na.rm=TRUE)
names(totalDest)[1] <- "Destination"
names(totalDest)[2] <- "FlightsAnnually"

#Delay percentage by destination
delayPercDest <- merge(totalDest,delayDest, by = "Destination")
delayPercDest <- merge(delayPercDest, avgDelay, by = "Destination")
delayPercDest$perc <-round(delayPercDest$DelaysAnnually/delayPercDest$FlightsAnnually,3)*100

#Rounding the delay time to 2 decimal places
delayPercDest$AverageDelayTime <- round(delayPercDest$AverageDelayTime,2)

#Destination percentage plot
p3 <- ggplot(delayPercDest, aes(x=delayPercDest$Destination, y=delayPercDest$perc, fill = delayPercDest$Destination)) + geom_col() + geom_text(aes(label = delayPercDest$perc), vjust = -0.5, size = 3) + ylim(0,100) + xlab("") + ylab("Percent of Flight Delayed") + theme(text = element_text(size=8),legend.position = 'none')

#Average delay by destination plot
p4 <- ggplot(delayPercDest, aes(x=delayPercDest$Destination, y=delayPercDest$AverageDelayTime, fill = delayPercDest$Destination)) + geom_col() + geom_text(aes(label = delayPercDest$AverageDelayTime), vjust = -0.5, size = 3) + ylim(0,150) + xlab("Airport") + ylab("Average Delay in Minutes") + theme(text = element_text(size=8),legend.position = 'none')


multiplot(p3, p4, cols=1)
```


Intuitively there would be some interesting story to tell from delays by day or month however there isn't much to be gleaned from the plots except that Decemeber and March have the highest average delay times. That, and the fall months like September, October, and Novemeber are good times to fly out of ABIA.
```{r}
#Average Delays by month, day of month, and day of week.
avgDelayMonth = aggregate(delayABIA$DepDelay~factor(Month), delayABIA, mean)
names(avgDelayMonth)[1] <- "Month"
names(avgDelayMonth)[2] <- "AverageDelayTime"
avgDelayMonth$AverageDelayTime <- round(avgDelayMonth$AverageDelayTime,2)

avgDelayDoM = aggregate(delayABIA$DepDelay~factor(DayofMonth), delayABIA, mean)
names(avgDelayDoM)[1] <- "DayOfMonth"
names(avgDelayDoM)[2] <- "AverageDelayTime"
avgDelayDoM$AverageDelayTime <- round(avgDelayDoM$AverageDelayTime,2)


avgDelayDoW = aggregate(delayABIA$DepDelay~factor(DayOfWeek), delayABIA, mean)
names(avgDelayDoW)[1] <- "DayOfWeek"
names(avgDelayDoW)[2] <- "AverageDelayTime"
avgDelayDoW$AverageDelayTime <- round(avgDelayDoW$AverageDelayTime,2)


#Average delay by Month
p5 <- ggplot(avgDelayMonth, aes(x=avgDelayMonth$Month, y=avgDelayMonth$AverageDelayTime, fill = avgDelayMonth$Month)) + geom_col() + geom_text(aes(label = avgDelayMonth$AverageDelayTime), vjust = -0.5, size = 3) + ylim(0,60) + xlab("Month") + ylab("Average Delay in Minutes") + theme(text = element_text(size=8),legend.position = 'none')


#Average delay by Day of Month
p6 <- ggplot(avgDelayDoM, aes(x=avgDelayDoM$DayOfMonth, y=avgDelayDoM$AverageDelayTime, fill = avgDelayDoM$DayOfMonth)) + geom_col() + geom_text(aes(label = avgDelayDoM$AverageDelayTime), vjust = -0.5, size = 3) + ylim(0,60) + xlab("Day Of Month") + ylab("Average Delay in Minutes") + theme(text = element_text(size=8),legend.position = 'none')


#Average delay by Day of Week
p7 <- ggplot(avgDelayDoW, aes(x=avgDelayDoW$DayOfWeek, y=avgDelayDoW$AverageDelayTime, fill = avgDelayDoW$DayOfWeek)) + geom_col() + geom_text(aes(label = avgDelayDoW$AverageDelayTime), vjust = -0.5, size = 3) + ylim(0,60) + xlab("Day Of Week") + ylab("Average Delay in Minutes") + theme(text = element_text(size=8),legend.position = 'none')

multiplot(p5, p6, p7, cols=1)


```


QUESTION 3: Portfolio Modeling

```{r, echo=FALSE}
rm(list=ls())
library(mosaic)
library(quantmod)
library(foreach)
library(reshape2)
```

First, we choose our 3 portfolios.

We chose a small tech portfolio, mid-sized real estate portfolio, and a large diverse portfolio. We expect the small tech portfolio to outperform the others due to the technology industry having the best overall performance in the past five years. However, this portfolio will also have very high volatility due to the nature of technology stocks over the past five years and the lack of diversification.  The real estate portfolio will have slightly lower volatility than the technology one, but we suspect that the returns will not be as high. The diverse portfolio may have the worst returns due to its investment in things like commodity and treasury bonds which do not have high yield, however it will perform much more consistently due to the nature of the markets it is invested in as well as the high number of ETFs included in the portfolio.

```{r}
portfolio_tech = c("PTF","XSD","PSJ")
portfolio_re = c("VNQ","USRT","SCHH","RWR","MORT")
portfolio_diverse = c("VB", "UGA", "TLT", "XLE", "RLY", "IYF", "REET", "UUP")
```



```{r}
tech_prices = getSymbols(portfolio_tech, from = "2014-08-14")
re_prices = getSymbols(portfolio_re, from = "2014-08-14")
diverse_prices = getSymbols(portfolio_diverse, from = "2014-08-14")

for(ticker in portfolio_tech) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}
tech_returns = cbind(	ClCl(PTFa),
								ClCl(XSDa),
								ClCl(PSJa))
for(ticker in portfolio_re) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}
re_returns = cbind(	ClCl(VNQa),
								ClCl(USRTa),
								ClCl(SCHHa),
								ClCl(RWRa),
								ClCl(MORTa))
for(ticker in portfolio_diverse) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}
diverse_returns = cbind(	ClCl(VBa),
								ClCl(UGAa),
								ClCl(TLTa),
								ClCl(XLEa),
								ClCl(RLYa),
								ClCl(IYFa),
								ClCl(REETa),
								ClCl(UUPa))
```

```{r}
tech_returns = as.matrix(na.omit(tech_returns))
re_returns = as.matrix(na.omit(re_returns))
diverse_returns = as.matrix(na.omit(diverse_returns))
```

Run 1000 20 day market simulations.

```{r}
initial_wealth = 100000
num_simulations = 1000

tech_final_value = rep(0,num_simulations)
re_final_value = rep(0,num_simulations)
diverse_final_value = rep(0,num_simulations)
for(i in 1:num_simulations) {
#	total_wealth = initial_wealth
	tech_wealth = initial_wealth
	re_wealth = initial_wealth
	diverse_wealth = initial_wealth
	tech_weights = c(1/3,1/3,1/3)
	re_weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
	diverse_weights = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8)
	tech_holdings = tech_weights * tech_wealth
	re_holdings = re_weights * re_wealth
	diverse_holdings = diverse_weights * diverse_wealth

	n_days = 20
	tech_wealthtracker = rep(0, n_days)
	re_wealthtracker = rep(0, n_days)
	diverse_wealthtracker = rep(0, n_days)
	for(today in 1:n_days) {
		day = sample(c(1:nrow(tech_returns)), 1)
#	  return.today = resample(all_returns, 1, orig.ids=FALSE)
#		holdings = holdings + holdings*return.today
		tech_holdings = tech_holdings + tech_holdings*tech_returns[day,]
		re_holdings = re_holdings + re_holdings*re_returns[day,]
		diverse_holdings = diverse_holdings + diverse_holdings*diverse_returns[day,]
		tech_total_wealth = sum(tech_holdings)
		re_total_wealth = sum(re_holdings)
		diverse_total_wealth = sum(diverse_holdings)
		tech_wealthtracker[today] = tech_total_wealth
		re_wealthtracker[today] = re_total_wealth
		diverse_wealthtracker[today] = diverse_total_wealth
	}
	tech_final_value[i]=tech_wealthtracker[n_days]
	re_final_value[i]=re_wealthtracker[n_days]
	diverse_final_value[i]=diverse_wealthtracker[n_days]
}
```

```{r}
finalValues = data.frame(sim = 1:num_simulations, tech = tech_final_value, real_estate = re_final_value, diverse = diverse_final_value)
ggplot(finalValues) + 
  geom_histogram(aes(x=tech, fill="Technology"), alpha=.2) +
  geom_histogram(aes(x=real_estate, fill="Real Estate"), alpha=.2) +
  geom_histogram(aes(x=diverse, fill="Diverse"), alpha=.2) +
  xlab("Final Returns") +
  ylab("Count") + 
  ggtitle("Histogram of Final Return")
```

As you can see from the graph above, our hypthesis was fairly consistent with the results.  The Diverse portfolio was very consistent in getting a value at or around 100000 due to the number of ETFs and the diversity of types in them.  The Real Estate portfolio performed more sporadically and was distributed over a longer range.  The Technology portfolio was very volatile as it was composed entirely of ETFs in one of the most volatile sectors, tech.  The lack of ETFs made it have an extremely large range of ending values, with a few ending up between 140,000 and 160,000.  The Technology portfolio definitely performed the best, with the majority of their returns being positive.  It also had the largest chance to lose money, so that is one thing to consider.

```{r}
finalValues_melted=melt(finalValues, id.vars = "sim")
finalValues_melted
ggplot(finalValues_melted) + geom_boxplot(aes(x=variable, y=value)) + ylab("Final Return") + xlab("") + ggtitle("Boxplot for Final Returns")
```

The points from the previous statement are shown better in this boxplot, where you can see how the plot for tech is very large with numerous positive outliers and the diverse is very condensed.  Realistically the tech portfolio is better in almost every situation, unless you want that really consistent but very small increase in value. This trend does not tell the future, however, and is very skewed by the increase in market cap of the technology sector in the past five years.

```{r}
print("Technology")
quantile(finalValues$tech)
print("Real Estate")
quantile(finalValues$real_estate)
print("Diverse")
quantile(finalValues$diverse)
```

Again the same points are reiterated with the quantiles, showing that in the majority of cases tech will prevail, however the diverse option prevents you from the poor times when tech lost over 17%.  In a little less than 3/4 of the cases, tech will be the most profitable, going up significantly with a 5.8% profit margin in a quarter of the cases compared to the 2.9% and 1.9% of Real Estate and Diverse, respectively.


Question 4: Market Segmentation

```{r setup, include=FALSE}
library(LICORS)
library(ggplot2)
library(foreach)
library(mosaic)
library(purrr)
library(factoextra)
library(cluster)

setwd("~/STA380-master/STA380-master/data")
twitter = read.csv("social_marketing.csv")

#Trimming columns that won't be used: chatter, uncategorized, spam, and adult.
h20 <- twitter[-c(1,5,35,36)]
h20[1] <- NULL
h20[3] <- NULL
h20[31]<- NULL

#Scaling the data
h20 = scale(h20, center=TRUE, scale=TRUE)
```

Running the WSS
```{r}
#Computing kmeans
set.seed(77)
# Compute and plot wss for k = 1 to k = 20.
k.valuesues <- 1:20
wss <- function(k) {kmeans(h20, k, nstart = 20 )$tot.withinss}
wss_valuesues <- map_dbl(k.valuesues, wss)  
```

Plotting the different k-vlaues and their Total WSS
```{r}
plot(k.valuesues, wss_valuesues,
     pch = 12, frame = TRUE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares")

```

Plotting the clusters
```{r}
k10 <- kmeans(h20, centers = 10, nstart = 30)
fviz_cluster(k10, data = h20)
```


Deciphering the 10 Market Segments
```{r}
# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(h20,"scaled:center")
sigma = attr(h20,"scaled:scale")
```

Looking at each of top categories of each cluster, you can see identifiers for news, college universities, cooking, health nutrition, shopping. So it would be recommended to advertise to demographics interested in those categories. In the future, it would be useful to apply PCA in order to be able to specify the categories and demographics better.
```{r}
#Understanding how each cluster is defined
allClust <- rbind(k10$center[1,]*sigma + mu,k10$center[2,]*sigma + mu,k10$center[3,]*sigma + mu,k10$center[4,]*sigma + mu,k10$center[5,]*sigma + mu,k10$center[6,]*sigma + mu,k10$center[7,]*sigma + mu,k10$center[8,]*sigma +mu,k10$center[9,]*sigma+ mu,k10$center[10,]*sigma + mu)
allClust <- data.frame(allClust)

#Taking the top 5 characterstics of each cluster
valuesues <- data.frame(t(allClust))
names(valuesues)

clust1 <- valuesues[order(valuesues$X1 ,decreasing=T)[1:5],]
clust2 <- valuesues[order(valuesues$X2 ,decreasing=T)[1:5],]
clust3 <- valuesues[order(valuesues$X3 ,decreasing=T)[1:5],]
clust4 <- valuesues[order(valuesues$X4 ,decreasing=T)[1:5],]
clust5 <- valuesues[order(valuesues$X5 ,decreasing=T)[1:5],]
clust6 <- valuesues[order(valuesues$X6 ,decreasing=T)[1:5],]
clust7 <- valuesues[order(valuesues$X7 ,decreasing=T)[1:5],]
clust8 <- valuesues[order(valuesues$X8 ,decreasing=T)[1:5],]
clust9 <- valuesues[order(valuesues$X9 ,decreasing=T)[1:5],]
clust10 <- valuesues[order(valuesues$X10 ,decreasing=T)[1:5],]

#Top categories of each cluster
par(mfrow=c(2,2))
barplot(cluster1$X1, main="Cluster 1", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster1))
barplot(cluster2$X2, main="Cluster 2", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster2))
barplot(cluster3$X3, main="Cluster 3", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster3))
barplot(cluster4$X4, main="Cluster 4", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster4))
barplot(cluster5$X5, main="Cluster 5", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster5))
barplot(cluster6$X6, main="Cluster 6", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster6))
barplot(cluster7$X7, main="Cluster 7", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster7))
barplot(cluster8$X8, main="Cluster 8", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster8))
barplot(cluster9$X9, main="Cluster 9", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster9))
barplot(cluster10$X10, main="Cluster 10", xlab="Top Categories", ylab="Frequency", names.arg=row.names(cluster10))
```



Question 5: Author Attribution

```{r }
library(tm) 
library(magrittr)
library(slam)
library(proxy)
#Setting Up the Training Data
#Make sure wording directory is correct
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
setwd("~/STA380-master/STA380-master/data/ReutersC50/C50train")
doc_list = Sys.glob('*')
file_list = Sys.glob(paste0(doc_list, '/*.txt'))

temp = lapply(file_list, readerPlain) 
mynames = file_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist
names(temp) = mynames
#Conversion to Corpus and setting names to authors
documents_raw = VCorpus(VectorSource(temp))
my_documents = documents_raw
names(my_documents) = names(temp)
#Preprocessing to remove certain features, such as number, space, etc...
my_documents = tm_map(my_documents, content_transformer(tolower))  #Lower Case
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) #Removes numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) #removes puntuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) #Removes whitespace
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en")) #Removes Stop Words
#Setting up train dtm
DTM_train = DocumentTermMatrix(my_documents)
#chose to do 90% sparsity to decrease terms
DTM_train = removeSparseTerms(DTM_train, 0.9)
#TFIDF for train
tfidf = weightTfIdf(DTM_train)
```



```{r}
#Setting Up Test Set
#Make sure to set directory to test set
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
setwd("~/STA380-master/STA380-master/data/ReutersC50/C50test")
doc_list2 = Sys.glob('*')
file_list = Sys.glob(paste0(doc_list2, '/*.txt'))
file_list = Sys.glob(paste0(doc_list2, '/*.txt'))

temp2 = lapply(file_list, readerPlain) 

mynames = file_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist
names(temp2) = mynames
#Conversion to Corpus and setting names to authors
documents_raw_1 = VCorpus(VectorSource(temp2))
#Preprocessing similar to train set
my_documents1 = documents_raw_1
my_documents1 = tm_map(my_documents1, content_transformer(tolower)) 
my_documents1 = tm_map(my_documents1, content_transformer(removeNumbers))
my_documents1 = tm_map(my_documents1, content_transformer(removePunctuation)) 
my_documents1 = tm_map(my_documents1, content_transformer(stripWhitespace))
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
#Setting up test DTM
DTM_test = DocumentTermMatrix(my_documents1,control = list(dictionary=Terms(DTM_train)))
DTM_test = removeSparseTerms(DTM_test, 0.90)
#TFIDF for test
tfidf_test = weightTfIdf(DTM_test)
```

```{r}
x_train = as.matrix(DTM_train)
x_test = as.matrix(DTM_test)
#Isolates words in training and test set
X_words= colnames(x_train)
X_test_words = colnames(x_test)
# Here empty vectors are created to track dropped and added words from the test set
added = vector(length=0)
dropped = vector(length=0)

# Here you drop the test words not in train
for (word in X_test_words) {
  if (!word %in% X_words) {
    dropped = c(dropped, word)
  }
}

# Here you add the train words not in test
for (word in X_words) {
  if (!word %in% X_test_words) {
    added = c(added, word)
  }
}

# Dummy matrix of 0's is inserted into the test matrix
dummy = matrix(0, nrow = nrow(DTM_train), ncol=length(added))
colnames(dummy) = added
X_test_actual = cbind(x_test, dummy)

# Sort so that column order mathces training set, and drop words 
X_test_actual = X_test_actual[,order(colnames(X_test_actual))]
X_test_actual = X_test_actual[,!colnames(X_test_actual) %in% dropped]

```

```{r}
counter = 1
#Here Doct_list has our authors
for (author in doc_list){
  dummy[counter] = author
  counter = counter+1
}
#Here we are creating a list of the authors for the train set of same size as the test sets
author_names = vector()
for( i in 1:2500){
  author_names[i] = dummy[ceiling(i/50)]
}

counter = 1
for (author in doc_list2){
  dummy[counter] = author
  counter = counter+1
}
#Here we are creating a list of authors for the test set of same size as the test sets
author_test = vector()
for( i in 1:2500){
  author_test[i] = dummy[ceiling(i/50)]
}
```


```{r}
#PCA w/ Multinomial Regression
library(glmnet)
library(nnet)
#SCaled PCA
princ = prcomp(x_train,scale=TRUE)
princ$rotation
#Calculate Score with 300 components
iso = princ$rotation[,1:300]
scores = x_train%*%iso

new_test_x = X_test_actual %*% iso

# Set Up inputs
train_X = scores
train_y = author_names

# Run multinomial regression
multi = glmnet(x=train_X, y=train_y, alpha=0, family="multinomial")

# Predict
predict = predict(multi, newx=new_test_x, type="class", s=0)
#Accuracy
(sum(predict==author_test, na.rm=T) + sum(is.na(predict) & is.na(author_test))) / length(predict)
#For PCA multi. regression we got an accuracy of 49.6%
```

```{r}
#Naive Bayes Model

# Calculate the smoothing factor and set up to get smoothing for each word for each author
smooth = 1/nrow(x_train)
by_word = rowsum(x_train + smooth, author_names)
total_wc = rowSums(by_word)

# Get multinomial probability vector and multiply to orginal test vector
int = log(by_word / total_wc)
X2 = t(int)
log_prob = X_test_actual %*% X2

# Prediction is max value outputted
pred = colnames(log_prob)[max.col(log_prob)]

#Here we check the accuracy, and we see that naive bayes is correct in predicting the correct author 52% of the time.
(sum(pred==author_test, na.rm=T) + sum(is.na(pred) & is.na(author_test))) / length(pred)
```
For our models here, we acheieved about 50% accuracy in classification. This could perhaps be improved by increasing the amount of data given, so that we have a better count of unique words associated to specific authors. OUt of the two models, Naive Bayes provided the better accuracy and was also computationally less intensive. However, it may not be the best model out of all of the possible model choices as the key assumption of independence of the X's is not true here.



QUESTION 6:Association rule mining

Just studying the dataset here we can see that the common items you would buy from the grocery store appear more frequently than others.

```{r}
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

#groceries = read.transactions("C:\\Users\\chris\\Desktop\\UT MSBA\\Summer 2019\\Predictive Models\\Part #2\\STA380-master\\STA380-master\\data\\groceries.txt",
#                              sep=',')


groceries = read.transactions("C:\\Users\\Daniel Oh\\Documents\\STA380-master\\STA380-master\\data\\groceries.txt",sep=',')

inspect(groceries[1:10])

itemFrequencyPlot(groceries, topN = 20)

```

Here we run the apriori algorithm on different subsets characterized by different confidence and support levels.  We decided to try ones with high support and low confidence, low support and high confidence, and one where both were high.  The third subset with high confidence and low support ended up having the most interesting rules, due to the high lifts created from that subset.


```{r}

groceryrules = apriori(groceries, parameter = list(support =0.001, confidence = 0.1, minlen = 2))
summary(groceryrules)

sub1 = subset(groceryrules, subset=confidence > 0.25 & support > 0.01)
inspect(sort(sub1, by="lift")[1:20])

sub2 = subset(groceryrules, subset=confidence > 0.25 & support > 0.005)
inspect(sort(sub2, by="lift")[1:20])

sub3 = subset(groceryrules, subset=confidence > 0.40 & support > 0.001)
inspect(sort(sub3, by="lift")[1:20])

```

From looking at the subsets we picked out three items that seemed to have some interesting combinations.  Beef ended up being a somewhat boring one since it is such a common item.  Beer was interesting due to its high confidence to be bought with other alcohol items.  The three highest lift rules all have to do with buying combinations of liquor, wine, and beer.  This came as somewhat of a surprise to me because usually if I go to the grocery store I'll get one or the other.  Berries has some interesting rules with whipped cream, which makes sense because they are a common combo.

```{r}

beefrules = subset(groceryrules, items %in% "beef")
inspect(sort(beefrules, by="lift")[1:10])

beerrules = subset(groceryrules, items %in% "bottled beer")
inspect(sort(beerrules, by="lift")[1:20])

berryrules = subset(groceryrules, items %in% "berries")
inspect(sort(berryrules, by="lift")[1:20])

```

Here we first make a graph with all the rules, however it is very difficult to look at so we made one for just 50 rules to make it easier to comprehend any interesting connections.

```{r}
plot(sub1, method='graph')
plot(head(sub1, 50, by='lift'), method='graph')

```

This plot shoes all the rules colored by confidence and shows their lift.

```{r}
plot(groceryrules, measure = c("support", "lift"), shading = "confidence")

```

Here we see how often certain orders of pairs are picked.  As you can see, the more items that are involved in a rule usually means it has a higher confidence on average.  This makes sense because of the rules behind apriori.

```{r}
plot(groceryrules, method='two-key plot')
```

