library(MASS)
library(klaR)
library(ICS)
library(ROCR)
library(boot)
library(ipred)
set.seed(4630)

# Reading in the data
hotels_raw = read.csv("hotel_bookings_raw.csv")

##### Data Cleaning #####
hotels_raw$NightsStayed <- hotels_raw$stays_in_weekend_nights + hotels_raw$stays_in_week_nights
hotels_raw <- subset(hotels_raw, NightsStayed != 0)
# Log Transform Response
hotels_raw$LogNightsStayed <- log(hotels_raw$NightsStayed)

# Histogram for log transformed response variable
hist(hotels_raw$LogNightsStayed, main="Histogram of NightsStayed", xlab="LogNightsStayed")

# Subsetting by relevant columns for regression question
hotels = hotels_raw[, c("LogNightsStayed", "adr", "adults", "children", 
                        "babies", "lead_time", 
                        "CPI_HOTELS", "FUEL_PRCS", "DIS_INC")]

# Convert variables into proper types
hotels$FUEL_PRCS = as.numeric(hotels$FUEL_PRCS)
hotels$CPI_HOTELS = as.numeric(hotels$CPI_HOTELS)
hotels$DIS_INC = as.numeric(hotels$DIS_INC)

# Remove all rows with NA values
hotels = na.omit(hotels)


##### Split into training and test data #####
sample.data<-sample.int(nrow(hotels), floor(.70*nrow(hotels)), replace = F)
train<-hotels[sample.data, ]
test<-hotels[-sample.data, ]






























