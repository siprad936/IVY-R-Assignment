###### ASSIGNMENT ######
# EXPORT each plot to a png/pdf programmatically and through UI

# Scatter plot
pdf("Scatterplot.pdf")
png("Scatterplot.png")
plot(df$DMC, df$DC, main="Scatterplot Example", 
     xlab="DMC ", ylab="DC", pch=19)
dev.off()

df <- read.csv("C:/Users/TE213958/Desktop/I/R/2/Forest Fires/forestfires.csv")

# 3D Scatterplot
attach(df)
library(scatterplot3d)
pdf("3D Scatterplot.pdf")
png("3D Scatterplot.png")
scatterplot3d(wind,rain,temp, main="3D Scatterplot")
dev.off()


# Interactive 3D Scatterplot
library(rgl)
pdf("Wind_DMC_DC.pdf")
png("Wind_DMC_DC.png")
plot3d(wind, DMC, DC, col="red", size=3)
dev.off()

# Boxplot
pdf("Boxplot.pdf")
png("Boxplot.png")
boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")
dev.off()

# Simple Bar Plot 
pdf("Simple Bar Plot.pdf")
png("Simple Bar Plot.png")
counts <- table(df$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")
dev.off()


# Stacked Bar Plot with Colors and Legend
pdf("Stacked Bar Plot.pdf")
png("Stacked Bar Plot.png")
counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts))
dev.off()

# Grouped Bar Plot
pdf(" Grouped Bar Plot.pdf")
png(" Grouped Bar Plot.png")
counts <- table(df$X, df$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
dev.off()

# Histogram Probability Dist
pdf("Histogram Probability Dist.pdf")
png("Histogram Probability Dist.png")
hist(df$wind, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$wind))
dev.off()

# Simple Pie Chart
pdf("Simple Pie Chart.pdf")
png("Simple Pie Chart.png")
library(dplyr)
df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] #c(10, 12,4, 16, 8)
pie(slices, labels=df[["month"]], main="Pie Chart of Wind")
dev.off()


# MAP PLOT
pdf("MAP PLOT.pdf")
png("MAP PLOT.png")
airports <- read.csv("C:/Users/TE213958/Desktop/I/R/3/airports.dat")
head(airports)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airports)

routes <- read.csv("C:/Users/TE213958/Desktop/I/R/3/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

library(plyr)
pdf("Simple Bar Plot.pdf")
png("Simple Bar Plot.png")
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportA <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")
dev.off()

# install.packages("ggmap")
library(ggmap)
pdf("mapPoints Plot.pdf")
png("mapPoints Plot.png")
map <- get_map(location = 'Europe', zoom = 4)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportA, alpha = .5)

mapPoints
dev.off()

detach(df)




