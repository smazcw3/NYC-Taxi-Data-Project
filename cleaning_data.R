library(data.table)
library(dplyr)

###Cleaning April 2014 Green Taxi data
trans1 <- function(x) {
    x <- x[, -c(21:22)]
    colnames(x) <- c("VendorID", "PickUpDateTime", "DropOffDateTime", "StoreAndFwdFlag", 
                     "RateCodeID", "PickUpLongitude", "PickUpLatitude", "DropOffLongitude" ,
                     "DropOffLatitude", "PassengerCount", "TripDistance", "FareAmount", "Extra", "MTATax",
                     "TipAmount", "TipTolls", "EhailFee", "TotalAmount", "PaymentType", "TripType")
    x
    
}

##Reading Green Taxi April 2014 data
gt_april_14 <- trans1(as.data.frame(fread("CSE587-Term Project-Data/green_tripdata_2014-04.csv", sep = ",")))
gt_april_14 <- filter(gt_april_14, PickUpLatitude != 0 & PickUpLongitude != 0 & DropOffLatitude != 0 & DropOffLongitude != 0)
write.csv(gt_april_14, "CSE587-Term Project-Data/new_green_tripledata_2014-04.csv")


###Cleaning April 2015 and 2016 Green Taxi data
trans2 <- function(x) {
    x <- x[, -c(18,22:23)]
    colnames(x) <- c("VendorID", "PickUpDateTime", "DropOffDateTime", "StoreAndFwdFlag", 
                     "RateCodeID", "PickUpLongitude", "PickUpLatitude", "DropOffLongitude" ,
                     "DropOffLatitude", "PassengerCount", "TripDistance", "FareAmount", "Extra", "MTATax",
                     "TipAmount", "TipTolls", "EhailFee", "TotalAmount", "PaymentType", "TripType")
    x
    
}

##Reading Green Taxi April 2015 data
gt_april_15 <- trans2(as.data.frame(fread("CSE587-Term Project-Data/green_tripdata_2015-04.csv", sep = ",", skip = 1)))
gt_april_15 <- filter(gt_april_15, PickUpLatitude != 0 & PickUpLongitude != 0 & DropOffLatitude != 0 & DropOffLongitude != 0)
write.csv(gt_april_15, "CSE587-Term Project-Data/new_green_tripledata_2015-04.csv")


##Reading Green Taxi April 2016 data
gt_april_16 <- trans2(as.data.frame(fread("CSE587-Term Project-Data/green_tripdata_2016-04.csv", sep = ",")))
gt_april_16 <- filter(gt_april_16, PickUpLatitude != 0 & PickUpLongitude != 0 & DropOffLatitude != 0 & DropOffLongitude != 0)
write.csv(gt_april_16, "CSE587-Term Project-Data/new_green_tripledata_2016-04.csv")




####Tab2
gt_april_14_v5 <- select(gt_april_14, PickUpDateTime, PassengerCount, TripDistance, TipAmount, TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_14_v5$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_14_v5$Day <- Day
gt_april_14_v5$PickUpDateTime <- as.POSIXct(gt_april_14_v5$PickUpDateTime)
gt_april_14_v5$Day <- as.numeric(gt_april_14_v5$Day)
gt_april_14_v5 <- filter(gt_april_14_v5, Day == 1)
write.csv(gt_april_14_v5, "CSE587-Term Project-Data/lm_green_tripledata_2014-04.csv")



gt_april_15_v5 <- select(gt_april_15, PickUpDateTime, PassengerCount, TripDistance, TipAmount , TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_15_v5$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_15_v5$Day <- Day
gt_april_15_v5$PickUpDateTime <- as.POSIXct(gt_april_15_v5$PickUpDateTime)
gt_april_15_v5$Day <- as.numeric(gt_april_15_v5$Day)
gt_april_15_v5 <- filter(gt_april_15_v5, Day == 1)
write.csv(gt_april_15_v5, "CSE587-Term Project-Data/lm_green_tripledata_2015-04.csv")



gt_april_16_v5 <- select(gt_april_16, PickUpDateTime, PassengerCount, TripDistance, TipAmount ,TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_16_v5$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_16_v5$Day <- Day
gt_april_16_v5$PickUpDateTime <- as.POSIXct(gt_april_16_v5$PickUpDateTime)
gt_april_16_v5$Day <- as.numeric(gt_april_16_v5$Day)
gt_april_16_v5 <- filter(gt_april_16_v5, Day == 1)
write.csv(gt_april_16_v5, "CSE587-Term Project-Data/lm_green_tripledata_2016-04.csv")




####Tab3
gt_april_14_exp <- select(gt_april_14, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_14_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_14_exp$Day <- Day
gt_april_14_exp$PickUpDateTime <- as.POSIXct(gt_april_14_exp$PickUpDateTime)
gt_april_14_exp$Day <- as.numeric(gt_april_14_exp$Day)
gt_april_14_exp <- filter(gt_april_14_exp, Day >= 1 & Day <=7)
write.csv(gt_april_14_exp, "CSE587-Term Project-Data/exp_green_tripledata_2014-04.csv")


gt_april_15_exp <- select(gt_april_15, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_15_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_15_exp$Day <- Day
gt_april_15_exp$PickUpDateTime <- as.POSIXct(gt_april_15_exp$PickUpDateTime)
gt_april_15_exp$Day <- as.numeric(gt_april_15_exp$Day)
gt_april_15_exp <- filter(gt_april_15_exp, Day >= 1 & Day <=7)
write.csv(gt_april_15_exp, "CSE587-Term Project-Data/exp_green_tripledata_2015-04.csv")


gt_april_16_exp <- select(gt_april_16, PickUpDateTime, PassengerCount, TripDistance, TotalAmount)
Day <- format(as.POSIXct(strptime(gt_april_16_exp$PickUpDateTime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d")
gt_april_16_exp$Day <- Day
gt_april_16_exp$PickUpDateTime <- as.POSIXct(gt_april_16_exp$PickUpDateTime)
gt_april_16_exp$Day <- as.numeric(gt_april_16_exp$Day)
gt_april_16_exp <- filter(gt_april_16_exp, Day >= 1 & Day <=7)
write.csv(gt_april_16_exp, "CSE587-Term Project-Data/exp_green_tripledata_2016-04.csv")
