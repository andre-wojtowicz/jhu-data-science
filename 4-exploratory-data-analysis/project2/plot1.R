# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the **base** plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# setup

options("scipen"=100, "digits"=4) # disable scientific notation

# smart loading data and transformation

if (!exists("NEI"))
    NEI = readRDS("summarySCC_PM25.rds")
if (!exists("SCC"))
    SCC = readRDS("Source_Classification_Code.rds")

if (!exists("total_emission"))
    total_emission = aggregate(Emissions ~ year, NEI, sum)

dm = as.matrix(total_emission["Emissions"]/1000000) # conv. to millions
rownames(dm) = total_emission$year

# figure creation

png(filename="plot1.png", width=480, height=480, units="px")

par(mar=c(5.1, 4.1, 6.1, 2.1)) # make more space around title

bplt = barplot(dm, beside = T, names.arg=rownames(dm),
               main="Total PM2.5 emission from all sources in USA
in 1999, 2002, 2005 and 2008",
               xlab="years",
               ylab="total PM2.5 emitted (mln tons)")

text(y=as.vector(dm)+par("cxy")[2]*0.5, x=bplt, labels=round(as.vector(dm),3), 
     xpd=TRUE, cex=1)

dev.off()
