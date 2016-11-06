# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# setup

options("scipen"=100, "digits"=4) # disable scientific notation
library(ggplot2)

# smart loading data and transformation

if (!exists("NEI"))
    NEI = readRDS("summarySCC_PM25.rds")
if (!exists("SCC"))
    SCC = readRDS("Source_Classification_Code.rds")

if (!exists("total_emission_ML"))
{
    total_emission_ML = aggregate(Emissions ~ year * type, NEI[NEI$fips==24510,], sum)
    total_emission_ML = transform(total_emission_ML, year=factor(year))
}

# figure creation

png(filename="plot3.png", width=960, height=480, units="px")

g = ggplot(total_emission_ML, aes(x=year, y=Emissions)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label = round(Emissions), y = Emissions*0.55), colour="white", size = 3) +
    facet_grid(. ~ type) +
    ggtitle("Total PM2.5 emission from all sources in the Baltimore City, Maryland in 1999, 2002, 2005 and 2008") +
    ylab("total PM2.5 emitted (tons)")
print(g)

dev.off()
