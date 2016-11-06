# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# setup

options("scipen"=100, "digits"=4) # disable scientific notation
library(ggplot2)

# smart loading data and transformation

if (!exists("NEI"))
    NEI = readRDS("summarySCC_PM25.rds")
if (!exists("SCC"))
    SCC = readRDS("Source_Classification_Code.rds")

# coal combustion-related sources
scc_ids = as.vector(SCC[grepl("Coal",SCC$Short.Name) & grepl("Comb",SCC$Short.Name),]$SCC)

if (!exists("total_emission_CC"))
{
    total_emission_CC = aggregate(Emissions ~ year, NEI[NEI$SCC %in% scc_ids,], sum)
    total_emission_CC = transform(total_emission_CC, year=factor(year))
}

# figure creation

png(filename="plot4.png", width=480, height=480, units="px")

g = ggplot(total_emission_CC, aes(x=year, y=Emissions)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(label = round(Emissions), y = Emissions/2), colour="white", size = 4) +
    ggtitle("Change of total PM2.5 emission from coal combustion-related 
sources in USA from 1999 to 2008") +
    ylab("total PM2.5 emitted (tons)")
print(g)

dev.off()
