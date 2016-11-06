# fast and efficient subset data load

unzip("exdata-data-household_power_consumption.zip")

df5rows = read.table("household_power_consumption.txt",
                     sep=";", 
                     na.strings="?",
                     header=TRUE,
                     nrows=5)
classes = sapply(df5rows, class)
df = read.table("household_power_consumption.txt",
                sep=";", 
                na.strings="?",
                header=FALSE,
                colClasses=classes,
                nrows=2880, # subset for dates 2007-02-01
                skip=66637) # and 2007-02-02
colnames(df) = colnames(df5rows)

# diagram creation

png(filename="plot1.png", width=480, height=480, units="px")

hist(df$Global_active_power,
     col="red",
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)")

dev.off()