# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time in motor vehicle emissions?

# setup

options("scipen"=100, "digits"=4) # disable scientific notation
library(ggplot2)

# smart loading data and transformation

if (!exists("NEI"))
    NEI = readRDS("summarySCC_PM25.rds")
if (!exists("SCC"))
    SCC = readRDS("Source_Classification_Code.rds")

# Motorcycles and Motor Vehicles
scc_ids = as.vector(SCC[grepl("Motor",SCC$Short.Name),]$SCC) 

if (!exists("total_emission_CC"))
{
    total_emission_CC = aggregate(Emissions ~ year * fips, NEI[NEI$SCC %in% scc_ids & (NEI$fips=="24510" | NEI$fips=="06037"),], sum)
    total_emission_CC = transform(total_emission_CC, year=factor(year))
    
    total_emission_CC$fips[total_emission_CC$fips=="24510"] = "Baltimore City, Maryland"
    total_emission_CC$fips[total_emission_CC$fips=="06037"] = "Los Angeles County, California"
}

# figure creation

png(filename="plot6.png", width=760, height=480, units="px")

g = ggplot(total_emission_CC, aes(x=year, y=Emissions)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label = round(Emissions,2), y = Emissions+2), colour="black", size = 5) +
    facet_grid(. ~ fips) +
    ggtitle("Emissions from motor vehicle sources 
from 1999 to 2008 in Baltimore City and Los Angeles County") +
    ylab("total PM2.5 emitted (tons)")
print(g)

dev.off()
