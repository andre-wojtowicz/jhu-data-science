# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# setup

options("scipen"=100, "digits"=4) # disable scientific notation

# smart loading data and transformation

if (!exists("NEI"))
    NEI = readRDS("summarySCC_PM25.rds")
if (!exists("SCC"))
    SCC = readRDS("Source_Classification_Code.rds")

if (!exists("total_emission_ML"))
    total_emission_ML = aggregate(Emissions ~ year, NEI[NEI$fips==24510,], sum)

dm = as.matrix(total_emission_ML["Emissions"])
rownames(dm) = total_emission_ML$year

# figure creation

png(filename="plot2.png", width=480, height=480, units="px")

par(mar=c(5.1, 4.1, 6.1, 2.1)) # make more space around title

bplt = barplot(dm, beside = T, names.arg=rownames(dm),
               main="Total PM2.5 emission from all sources in the Baltimore City,
Maryland in 1999, 2002, 2005 and 2008",
               xlab="years",
               ylab="total PM2.5 emitted (tons)")

text(y=as.vector(dm)+par("cxy")[2]*0.5, x=bplt, labels=round(as.vector(dm)), 
     xpd=TRUE, cex=1)

dev.off()
