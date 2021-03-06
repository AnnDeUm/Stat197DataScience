library(ggplot2)
library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
head(SCC)

#1
EandY<- tapply(NEI$Emissions,NEI$year,sum)
barplot(EandY/10^6, main = "Total PM2.5 Emissions in the US",
        xlab = "Year",
        ylab = "PM2.5 Emissions (1,000,000 Tons)"
)
#Based on the barplot, total emissions from PM2.5 in the United States 
#decreased from 1999 to 2008

#2
EBandY <- filter(NEI, fips == "24510")
TotalsBaltimore <- tapply(EBandY$Emissions,EBandY$year, sum)
barplot(TotalsBaltimore, main = "Total PM2.5 Emissions in Baltimore City", 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (Tons)"
)
#Based on the barplot, total emissions from PM2.5 in Baltimore City fluctuates
#from 1999 to 2008. There was a decrease from 1999 to 2002, an increase from 
#2002 to 2005, and an increase from 2005 to 2008

#3
EBbtandY <- filter(NEI, fips == "24510")
PlotbySource <- ggplot(EBbtandY, aes(factor(year), Emissions, fill = type)) +
 geom_bar(stat = "identity") +
 theme_bw() + 
 guides(fill = FALSE)+
 facet_grid(.~type, scales = "free", space = "free") + 
 labs(x = "Year", y = expression("Total PM2.5 Emissions (Tons)")) + 
 labs(title = expression("PM2.5 Emissions by Source Type in Baltimore City")
 )
print(PlotbySource)
#Based on the plot, non-road, nonpoint, and on-road have seen a decrease in 
#PM2.5 emissions from 1999 to 2008 while point have had an increase from year 
#1999 to 2005 and a decline from 2005 to 2008. 

#4
combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE) 
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 

combcoal <- (combustion & coal) 
combcoalscc <- SCC[combcoal, ]$SCC 
combcoalnei <- NEI[NEI$SCC %in% combcoalscc,] 

x <- tapply(combcoalnei$Emissions, combcoalnei$year, sum)

barplot(x/10^5, main = "Total PM2.5 Emissions from Coal Combustion Sources
        Across the US", 
        xlab = "Year", 
        ylab = "Total PM2.5 Emissions (100,000 Tons)"
)
#In the United States, emissions from coal comnbustion-related sources
#experienced a decline from 1999 to 2008.

#5
Baltimore <- filter(NEI, fips == "24510")

vehicle <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehicleSCC <- SCC[vehicle, ]$SCC
vehicleBaltimore <- Baltimore[Baltimore$SCC %in% vehicleSCC,]
y <- tapply(vehicleBaltimore$Emissions, vehicleBaltimore$year, sum)
barplot(y, main = "Total PM2.5 Emissions from Vehicles in Baltimore City", 
        xlab = "Year", 
        ylab = "Total Vehicle PM2.5 Emissions (Tons)"
)

#In Baltimore City in the Unites States, motor vehicle sources experienced 
#experienced a dedline from the 1999 to 2008.

#6
Baltimore <- filter(NEI, fips == "24510")
LosAngeles <- filter(NEI, fips == "06037")

vehicle <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehicleSCC <- SCC[vehicle, ]$SCC
vehicleBaltimore <- Baltimore[Baltimore$SCC %in% vehicleSCC,]
vehicleLosAngeles <- LosAngeles[LosAngeles$SCC %in% vehicleSCC,]

a <- tapply(vehicleBaltimore$Emissions, vehicleBaltimore$year, sum)

b <- tapply(vehicleLosAngeles$Emissions, vehicleLosAngeles$year, sum)

par(mfrow=c(1,2), mar=c(4, 4, 4, 1), oma=c(1, 0, 2, 0))

barplot(a, main = "Baltimore City",
        xlab = "Year",
        ylab = "Total Vehicle PM2.5 Emissions (Tons)",
        ylim = c(0,7000)
)

barplot(b, main = "Los Angeles",
        xlab = "Year",
        ylab = "Total Vehicle PM2.5 Emissions (Tons)",
        ylim = c(0,7000)
)

mtext("Total Vehicle PM2.5 Emissions (Tons)", side=3, outer=TRUE, cex=1.4)
#In comparison, Los Angeles City experience greater changes from 1999 to 2008. 
#Baltimore City experienced a decline from 1999 to 2002 and didn't seem to have 
#huge decline meanwhile Los Angeles City experienced an increase and decrease 
#in total PM2.5 emissions between years 1999 to 2008.