#Reproducible Research Peer Assignment 2

##Loading packages
#Load packages
pack <- c("ggplot2", "dplyr", "gridExtra")
usePackage <- function(p) {
        if (!is.element(p, installed.packages()[,1]))
                install.packages(p, dep = TRUE)
        require(p, character.only = TRUE)
}
sapply(pack, usePackage)


#Getting Data

if(!file.exists(".\\data")) {dir.create(".\\data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists(".\\data\\NOAA_Data.csv.bz2")) {
        download.file(fileurl, destfile = ".\\data\\NOAA_Data.csv.bz2")
}


df <- read.csv(".\\data\\NOAA_Data.csv.bz2", stringsAsFactors = FALSE)


               
#Create a copy of the dataset

copy <- df

df <- copy

#Explore the data
head(df)
names(df)
str(df)



unique(df$EVTYPE)
unique(df$CROPDMGEXP)
unique(df$PROPDMGEXP)

#df[which(df$CROPDMGEXP %in% c("?", "0", "2")), ]


#Subset rows with at least one value of the variables of interest greater 
#than 0.

df <- df[which(df$FATALITIES > 0 | df$INJURIES > 0 | df$CROPDMG > 0 | 
                       df$PROPDMG > 0), ]




#I mannually created a txt file with the 48 categories of event according to
#NOAA 
#https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
#page 6. Afterwards, I loaded the data.

txt <- read.csv(".\\data\\event.csv", header = F, stringsAsFactors = FALSE)

txt <- mutate_each(txt, funs(toupper))

#Now in order to match the event type in the dataset with the official ones, ch
#according to NOAA, we first need to change couple of values in the 
#variable EVTYPE:

df$EVTYPE[grep("TSTM WIND", df$EVTYPE)] <- "TROPICAL STORM"
df$EVTYPE[grep("^THUNDERSTORM", df$EVTYPE)] <- "THUNDERSTORM WIND"
df$EVTYPE[grep("SEVERE TH", df$EVTYPE)] <- "THUNDERSTORM"
df$EVTYPE[grep("HURRICANE", df$EVTYPE)] <- "HURRICANE/TYPHOON"
df$EVTYPE[grep("FLASH FL", df$EVTYPE)] <- "FLASH FLOOD"
df$EVTYPE[grep("FLOOD", df$EVTYPE)] <- "FLOOD"
df$EVTYPE[grep("HEAVY R", df$EVTYPE)] <- "HEAVY RAIN"
df$EVTYPE[grep("^HAIL", df$EVTYPE)] <- "HAIL"
df$EVTYPE[grep("HIGH W", df$EVTYPE)] <- "HIGH WIND"
df$EVTYPE[grep("TORNADO", df$EVTYPE)] <- "TORNADO"
df$EVTYPE[grep("TROPICAL ST", df$EVTYPE)] <- "TROPICAL STORM"
df$EVTYPE[grep("WILD F", df$EVTYPE)] <- "WILDFIRE"
df$EVTYPE[grep("WILDF", df$EVTYPE)] <- "WILDFIRE"
df$EVTYPE[grep("FIRE", df$EVTYPE)] <- "WILDFIRE"
df$EVTYPE[grep("EXTREME HEAT", df$EVTYPE)] <- "EXCESSIVE HEAT"
df$EVTYPE[grep("^HEAT", df$EVTYPE)] <- "HEAT"
df$EVTYPE[grep("WATERSP", df$EVTYPE)] <- "WATERSPOUT"
df$EVTYPE[grep("HEAVY SN", df$EVTYPE)] <- "HEAVY SNOW"
df$EVTYPE[grep("SNOW", df$EVTYPE)] <- "WINTER WEATHER"
df$EVTYPE[grep("COLD", df$EVTYPE)] <- "WINTER WEATHER"
df$EVTYPE[grep("Cold", df$EVTYPE)] <- "WINTER WEATHER"
df$EVTYPE[grep("WINTER ST", df$EVTYPE)] <- "WINTER STORM"
df$EVTYPE[grep("ICE", df$EVTYPE)] <- "ICE STORM"
df$EVTYPE[grep("^FREEZ", df$EVTYPE)] <- "FROST/FREEZE"
df$EVTYPE[grep("^FOG", df$EVTYPE)] <- "DENSE FOG"


#Now let's get the cleaner dataset:

cleandata <- df[df$EVTYPE %in% txt$V1, ]

#In order to compare the percent of data excluded due typos or unknown event
#types: 

df2 <- setdiff(df[, c(8, 23:28)], cleandata[, c(8, 23:28)])

#Percent of excluded rows
round(nrow(df2)/nrow(df)*100, digits = 2)





###########################
#Question 1

q1 <- aggregate(cbind(FATALITIES, INJURIES) ~ 
                        EVTYPE, sum, data = cleandata)

topfatalities <- q1[order(q1$FATALITIES, decreasing = TRUE), c(1, 2)]
topfatalities <- topfatalities[1:5, ]
topinjuries <- q1[order(q1$INJURIES, decreasing = TRUE), c(1, 3)]
topinjuries <- topinjuries[1:5, ]


#Graph 1
p1 <- ggplot(topfatalities, aes(x = reorder(EVTYPE, -FATALITIES), 
                                y = FATALITIES))
p1 <- p1 + geom_bar(stat = "identity", colour  = "red", fill = "chocolate2") +
        labs(x = "Event Type", y = "Number of Fatalities") +
        ggtitle(expression(atop("", 
                                atop("Fatalities", "")))) +
        theme(axis.title = element_text(face = "bold", size = 14), 
              plot.title = element_text(face = "bold", size = 20, vjust = 1.5), 
              legend.position = "none")
        
print(p1)


#Graph 2
p2 <- ggplot(topinjuries, aes(x = reorder(EVTYPE, -INJURIES), 
                                y = INJURIES))
p2 <- p2 + geom_bar(stat = "identity", colour  = "red", fill = "burlywood") +
        labs(x = "Event Type", y = "Number of Injuries") +
        ggtitle(expression(atop("", 
                atop("Injuries", "")))) +
        theme(axis.title = element_text(face = "bold", size = 14), 
              plot.title = element_text(face = "bold", size = 20, vjust = 1.5), 
              legend.position = "none")

print(p2)

#Join both graphs
grid.arrange(p1, p2, ncol=2, 
             top = "Top 5 Harmful Events Related to Population Health")


######################
#Question 2:
copy2 <- cleandata
cleandata <- copy2

names(cleandata)
unique(cleandata$PROPDMGEXP)
unique(cleandata$CROPDMGEXP)

multiprefix <- c("0" = 1, "K" = 1000, "k" = 1000, "M" = 1000000, "m" = 1000000, 
                 "H" = 100, "h" = 100, "5" = 100000, "6" = 1000000, "4" = 10000, 
                 "2" = 100, "7" = 10000000, "3" = 1000, "B" = 1000000000,
                 "b" = 1000000000)

#This observation has a typo according to the California Department of Water
#Resources: http://www.water.ca.gov/floodsafe/ca-flood-preparedness/fpw-day3.cfm
#It should be millions instead of billions. So I'm gonna change it.
cleandata$PROPDMGEXP[which(cleandata$REFNUM == 605943)] <- "M"

#Now let's create a variable corresponding to the amount of money in damages:
#Properties damages
cleandata$propdamage <- cleandata$PROPDMG * multiprefix[cleandata$PROPDMGEXP]

#Crops damages
cleandata$cropsdamage <- cleandata$PROPDMG * multiprefix[cleandata$PROPDMGEXP]

#Now let's create data to answer question 2:

q2 <- aggregate(cbind(propdamage, cropsdamage) ~ 
                        EVTYPE, sum, data = cleandata)


#Getting top 5 event types with the greatest economic consequences
toppropdmg <- q2[order(q2$propdamage, decreasing = TRUE), c(1, 2)]
toppropdmg <- toppropdmg[1:5, ]
topcropsdmg <- q2[order(q2$cropsdamage, decreasing = TRUE), c(1, 3)]
topcropsdmg <- topcropsdmg[1:5, ]


#Graph 1
p11 <- ggplot(toppropdmg, aes(x = reorder(EVTYPE, -propdamage), 
                                y = propdamage))
p11 <- p11 + geom_bar(stat = "identity", colour  = "red", fill = "chocolate2") +
        labs(x = "Event Type", y = "Amount of Money Spent") +
        ggtitle(expression(atop("", 
                                atop("Properties Damages", "")))) +
        theme(axis.title = element_text(face = "bold", size = 14), 
              plot.title = element_text(face = "bold", size = 20, vjust = 1.5), 
              legend.position = "none")

print(p11)


#Graph 2
p22 <- ggplot(topcropsdmg, aes(x = reorder(EVTYPE, -cropsdamage), 
                              y = cropsdamage))
p22 <- p22 + geom_bar(stat = "identity", colour  = "red", fill = "burlywood") +
        labs(x = "Event Type", y = "Amount of Money Spent") +
        ggtitle(expression(atop("", 
                                atop("Crops Damages", "")))) +
        theme(axis.title = element_text(face = "bold", size = 14), 
              plot.title = element_text(face = "bold", size = 20, vjust = 1.5), 
              legend.position = "none")

print(p22)


#Join both graphs
grid.arrange(p11, p22, ncol=2, 
             top = "Top 5 Expensive Natural Events in the USA")

