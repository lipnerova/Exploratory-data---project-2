#https://class.coursera.org/exdata-032/forum/thread?thread_id=248#comment-700

## Creates sixth plot for the Coursera Exploratory Data Analysis Second Course Project 
## CAUTION: DPLYR, ggplot2 and cowplot package required
library("dplyr")
library("cowplot")
library("ggplot2")

## Reads the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Filter SCC for vehicles only 
coal <- SCC[grepl("vehicle",SCC$EI.Sector, ignore.case = TRUE),]

## Filter NEI for vehicles only
NEI <- NEI[NEI$SCC %in% coal$SCC,]

## Filter to Baltimore and LA data only
cities <- filter(NEI, fips == "24510"|fips == "06037")

## Groups data by year and type
grouped <- group_by(cities, fips, year)

## Summarises the total emissions of each year
totaled <- summarise(grouped, PM25 = sum(Emissions))

## Replace fips by proper name
totaled$fips[totaled$fips == "06037"] <- "Los Angeles County"
totaled$fips[totaled$fips == "24510"] <- "Baltimore City"

## Plots the first graph
plot <- ggplot(totaled,aes(factor(year),PM25,fill=fips)) +
        geom_bar(stat="identity") +
        facet_grid(.~fips) +
        scale_fill_discrete(name="Region") +
        labs(x="Year",y="Emissions in tons", title="PM25 Emissions by Region and Year") +
        geom_text(aes(label=round(PM25)),size=4,vjust=1)

## Creating a second graph showing relative change to previous year
totaled_relative <- mutate(totaled, base=lag(PM25), change = (100*(PM25 / base)-100))

## Plots the second graph
plot2 <- ggplot(totaled_relative,aes(factor(year),change,fill=fips)) +
        geom_bar(stat="identity") +
        facet_grid(.~fips) +
        scale_fill_discrete(name="Region") +
        labs(x="Year",y="Change in percent relative to previous year", title="Change in PM25 Emissions by Region compared to previous year") +
        geom_abline(intercept = 0, slope = 0, colour = "black") +
        geom_text(aes(label=paste0(round(change,1),"%")),size=4,vjust=1)

## Creating a third graph showing relative change to 1999
totaled_relative <- mutate(totaled_relative, beginchange = ifelse(fips=="Baltimore City",100*PM25/346.82,100*PM25/3931.12))

## Plots the third graph
plot3 <- ggplot(totaled_relative,aes(factor(year), beginchange,fill=fips)) +
        geom_bar(stat="identity") +
        facet_grid(.~fips) +
        scale_fill_discrete(name="Region") +
        labs(x="Year",y="Emissions as Percentage of 1999's", title="Relative levels PM25 Emissions by Region and Year compared to 1999") +
        geom_abline(intercept = 100, slope = 0, colour = "black") +
        geom_text(aes(label=paste0(round(beginchange,1),"%")),size=4,vjust=1)

## Creating a fourth graph showing the absolute change compared to previous year
totaled_relative <- mutate(totaled_relative, abs_change = PM25 - base)

## Plots the fourth graph
plot4 <- ggplot(totaled_relative,aes(factor(year),abs_change,fill=fips)) +
        geom_bar(stat="identity") +
        facet_grid(.~fips) +
        scale_fill_discrete(name="Region") +
        labs(x="Year",y="Change of Emissions in tons", title="Absolute change of PM25 Emissions by Region and Year compared to previous year") +
        geom_abline(intercept = 0, slope = 0, colour = "black") +
        geom_text(aes(label=round(abs_change)),size=4,vjust=1)

## Creates a PNG graphic device
png(filename = "plot6.PNG", width=1280, height=800)

## Prints the graphs
plot_grid(plot,plot4,plot3,plot2, align="hv", ncol=2)

## Ends the PNG graphic device
dev.off()
