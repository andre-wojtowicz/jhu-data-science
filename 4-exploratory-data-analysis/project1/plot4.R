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

png(filename="plot4.png", width=480, height=480, units="px")

par(mfcol=c(2,2))

with(df, {

    # first plot
    plot(DateTime, Global_active_power,
                  type="l",
                  xlab="",
                  ylab="Global Active Power")
    
    # second plot
    plot(DateTime, Sub_metering_1,
         col="black",
         type="l",
         xlab="",
         ylab="Energy sub metering")
    lines(DateTime, Sub_metering_2,
          col="red")
    lines(DateTime, Sub_metering_3,
          col="blue")
    legend("topright", 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col=c("black", "red", "blue"),
           lty=1,
           bty="n")
    
    # third plot
    plot(DateTime, Voltage,
         type="l",
         xlab="datetime")
    
    # fourth plot
    plot(DateTime, Global_reactive_power,
         type="l",
         lwd=1,
         xlab="datetime")

    }
)

dev.off()