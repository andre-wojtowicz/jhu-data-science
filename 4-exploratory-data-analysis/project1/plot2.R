# config for English dayweeks names

Sys.setlocale("LC_TIME", "English")

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

# format date and time attributes

df$Date = as.Date(df$Date, "%d/%m/%Y")
df$DateTime = strptime(paste(df$Date, df$Time), "%Y-%m-%d %H:%M:%S")

# diagram creation

png(filename="plot2.png", width=480, height=480, units="px")

with(df, plot(DateTime, Global_active_power,
             type="l",
             xlab="",
             ylab="Global Active Power (kilowatts)")
)

dev.off()