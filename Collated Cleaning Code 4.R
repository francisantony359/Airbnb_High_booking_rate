#Libraries
install.packages("stringr")
library("stringr")
library(data.table)
library(glmnet)
library(tree)
library(class)
library(dplyr)
library(plyr)
library(mlbench)
library(qdapTools)
library(caret)
library(randomForest)
detach("package:dplyr",unload=TRUE)

airbnb <- fread("airbnb_train_x.csv", header=T, na.strings="")
#airbnb <- fread("airbnb_train_test.csv", header=T, na.strings="")

#*********************************
#Code to merge the independent and dependent variables.
rm(airbnb_train_y)
airbnb_train_y <- fread("airbnb_train_y.csv", header=T, na.strings="")
str(airbnb_train_y)
count(airbnb_train_y,"high_booking_rate")
airbnb_train_y$high_booking_rate <- ifelse(is.na(airbnb_train_y$high_booking_rate),0,airbnb_train_y$high_booking_rate)
airbnb_train_y$high_booking_rate <- as.numeric(airbnb_train_y$high_booking_rate)
sum(is.na(airbnb_train_y$high_booking_rate))
airbnb_train_y$high_booking_rate <- ifelse(is.na(airbnb_train_y$high_booking_rate),0,airbnb_train_y$high_booking_rate)
airbnb_train_y$V1 <- as.numeric(airbnb_train_y$V1)
colnames(airbnb)[colnames(airbnb)=="V1"] <- "ID"
airbnb$ID <- as.numeric(airbnb$ID)
colnames(airbnb_train_y)[colnames(airbnb_train_y)=="V1"] <- "ID"
airbnb <- merge(airbnb,airbnb_train_y,by="ID")
rm(airbnb_train_y)
#***************************************

#airbnb <- fread("airbnb_train_test.csv", header=T, na.strings="")
ammenities <- fread("FrequencyAmmenitiesTrain3.csv", header=T, na.strings="")
str(ammenities)
#ammenities[sapply(ammenities, is.numeric)] <- lapply(ammenities[sapply(ammenities, is.numeric)], as.factor)
#ammenities$V1 <- NULL
colnames(airbnb)[colnames(airbnb)=="V1"] <- "ID"
class(airbnb$ID)
airbnb$ID <- as.numeric(airbnb$ID)
class(ammenities$ID)
ammenities$ID <- as.numeric(ammenities$ID)
airbnb <- merge(airbnb,ammenities,by = "ID")


#-----------------------------Begin of Code - Utkarsh------------------------------------------------------------

#accomodates
count(airbnb,"accommodates")
airbnb$accommodates <- as.numeric(airbnb$accommodates)
airbnb$accommodates <- ifelse(is.na(airbnb$accommodates) ==TRUE, round(mean(airbnb$accommodates, na.rm=TRUE)),airbnb$accommodates)
count(airbnb,"accommodates")
airbnb$accommodates <- as.numeric(airbnb$accommodates)
str(airbnb$accommodates)
sum(is.na(airbnb$accommodates))

#availability_30
count(airbnb,"availability_30")
airbnb$availability_30 <- as.numeric(airbnb$availability_30)
airbnb$availability_30 <- ifelse(is.na(airbnb$availability_30) ==TRUE, round(mean(airbnb$availability_30, na.rm=TRUE)),airbnb$availability_30)
count(airbnb,"availability_30")

#Bathrooms
count(airbnb,"bathrooms")#7 junk values and 246 blank values. will need to make some assumptions about that. 
airbnb$bathrooms<- as.numeric(as.character(airbnb$bathrooms))
airbnb$bathrooms <- ifelse(is.na(airbnb$bathrooms) ==TRUE, round(mean(airbnb$bathrooms, na.rm=TRUE)),airbnb$bathrooms)
count(airbnb,"bathrooms")

#beds
count(airbnb,"beds")
airbnb$beds<- as.numeric(as.character(airbnb$beds))
airbnb$beds <- ifelse(is.na(airbnb$beds) ==TRUE, round(mean(airbnb$beds, na.rm=TRUE)),airbnb$beds)
airbnb$beds <- ifelse(airbnb$beds > 40, 40,airbnb$beds)
count(airbnb,"beds")

#bed_type
count(airbnb,"bed_type")
airbnb$bed_type <- ifelse(str_detect(airbnb$bed_type,"1"),"Real Bed" ,airbnb$bed_type)
airbnb$bed_type <- ifelse(is.na(airbnb$bed_type),"Real Bed" ,airbnb$bed_type)
count(airbnb,"bed_type")
class(airbnb$bed_type)
airbnb$bed_type <- as.factor(airbnb$bed_type)
class(airbnb$bed_type)

#bedrooms
count(airbnb,"bedrooms") # 8 junck walues and 93 blank ones. convert to numerical
airbnb$bedrooms<- as.numeric(as.character(airbnb$bedrooms))
str(airbnb$bedrooms)
mean(airbnb$bedrooms, na.rm =TRUE)
airbnb$bedrooms <- ifelse(is.na(airbnb$bedrooms) ==TRUE, round(mean(airbnb$bedrooms, na.rm=TRUE)),airbnb$bedrooms)
class(airbnb$bedrooms)

#cancellation policy
count(airbnb,"cancellation_policy") #some numbers. in 9 rows. 
airbnb$cancellation_policy <- ifelse(str_detect(airbnb$cancellation_policy,"1"),"other" ,airbnb$cancellation_policy)
airbnb$cancellation_policy <- ifelse(str_detect(airbnb$cancellation_policy,"2"),"other" ,airbnb$cancellation_policy)
airbnb$cancellation_policy <- ifelse(str_detect(airbnb$cancellation_policy,"5"),"other" ,airbnb$cancellation_policy)
count(airbnb,"cancellation_policy")
airbnb$cancellation_policy <- as.factor(airbnb$cancellation_policy)
class(airbnb$cancellation_policy)
count(airbnb,"cancellation_policy")


#cleaning Fee
count(airbnb,"cleaning_fee")
airbnb$cleaning_fee <- gsub("\\$","",airbnb$cleaning_fee)
airbnb$cleaning_fee <- gsub(".\\.","",airbnb$cleaning_fee)
airbnb$cleaning_fee <- gsub("\\,","",airbnb$cleaning_fee)
airbnb$cleaning_fee[(airbnb$cleaning_fee == "f" | airbnb$cleaning_fee == "t")] <- NA
class(airbnb$cleaning_fee)
airbnb$cleaning_fee <- as.numeric(airbnb$cleaning_fee)
class(airbnb$cleaning_fee)
airbnb$cleaning_fee <- ifelse(is.na(airbnb$cleaning_fee),0,airbnb$cleaning_fee) #converted the NA to zero for lack of any better option.

count(airbnb,"cleaning_fee")

#Extra_people
# count(airbnb,"extra_people")
# airbnb$extra_people <- gsub("\\$","",airbnb$extra_people)
# airbnb$extra_people <- gsub(".\\.","",airbnb$extra_people)
# airbnb$extra_people <- as.numeric(airbnb$extra_people)
# count(airbnb,"extra_people")


#host_listing Count
count(airbnb,"host_listings_count")
airbnb$host_listings_count<- as.numeric(as.character(airbnb$host_listings_count))
sum(is.na(airbnb$host_listings_count))
airbnb$host_listings_count <- ifelse(is.na(airbnb$host_listings_count) ==TRUE, round(mean(airbnb$host_listings_count, na.rm=TRUE)),airbnb$host_listings_count)
count(airbnb,"host_listings_count")

#Price
airbnb$price <- gsub("\\$","",airbnb$price)
airbnb$price <- gsub(".\\.","",airbnb$price)
airbnb$price <- gsub("\\,","",airbnb$price)
sum(is.na(airbnb$price))
airbnb$price <- as.numeric(airbnb$price)
mean(airbnb$price, na.rm=TRUE)
airbnb$price <- ifelse(is.na(airbnb$price) ==TRUE, mean(airbnb$price, na.rm=TRUE),airbnb$price)
sum(is.na(airbnb$price))

#weekly_price
airbnb$weekly_price <- gsub("\\$","",airbnb$weekly_price)
airbnb$weekly_price <- gsub(".\\.","",airbnb$weekly_price)
airbnb$weekly_price <- gsub("\\,","",airbnb$weekly_price)
count(airbnb,"weekly_price")
sum(is.na(airbnb$weekly_price)) #79436 blank values. no sense.
airbnb$weekly_price <- as.numeric(airbnb$weekly_price)
airbnb$weekly_price <- ifelse(is.na(airbnb$weekly_price) ==TRUE, airbnb$price*7,airbnb$weekly_price)

airbnb$price_diff_week <- ifelse(airbnb$price*7 == airbnb$weekly_price,0,1)
count(airbnb,"price_diff_week")


#property Type
count(airbnb,"property_type")
airbnb$property_type <- ifelse(is.na(airbnb$property_type),"Apartment",airbnb$property_type)
count(airbnb,"property_type")
class(airbnb$property_type)
airbnb$property_type <- as.factor(airbnb$property_type)
count(airbnb,"property_type")
class(airbnb$property_type)



#-----------------------------End of Code - Utkarsh--------------------------------------------------------------


#-----------------------------Begin of Code - Francis------------------------------------------------------------

airbnb$availability_90<- as.integer(as.character(airbnb$availability_90))
summary(airbnb$availability_90)
airbnb$availability_90[is.na(airbnb$availability_90)]<-median(airbnb$availability_90,na.rm = T)
summary(airbnb$availability_90)
count(airbnb,"availability_90")
#airbnb$availability_90[airbnb$availability_90 == 0] <- NA



#######################################################


airbnb$availability_60<- as.integer(as.character(airbnb$availability_60))
summary(airbnb$availability_60)
count(airbnb,"availability_60")
airbnb$availability_60[is.na(airbnb$availability_60)]<-median(airbnb$availability_60,na.rm = T)
summary(airbnb$availability_60)

airbnb$availability_365<- as.integer(as.character(airbnb$availability_365))
summary(airbnb$availability_365)
airbnb$availability_365[is.na(airbnb$availability_365)]<-median(airbnb$availability_365,na.rm = T)
summary(airbnb$availability_365)


#######################################################

#added by utkarsh

#ib_count <- 
count(airbnb, "host_identity_verified")
#ib_max <- max(ib_count$freq)
#ib_row <- as.numeric(which(ib_count$freq == max(ib_count$freq)))
#ib_replace <- as.character(droplevels(ib_count$host_identity_verified[ib_row]))
airbnb$host_identity_verified[!(airbnb$host_identity_verified == "f" | airbnb$host_identity_verified == "t") | is.na(airbnb$host_identity_verified)] <- "t"

#end if code added  by utkarsh

airbnb$host_identity_verified <- (sub("f", "0",airbnb$host_identity_verified,fixed=TRUE))
airbnb$host_identity_verified <- (sub("t", "1",airbnb$host_identity_verified,fixed=TRUE))
airbnb$host_identity_verified<- as.integer(as.character(airbnb$host_identity_verified))
airbnb$host_identity_verified[is.na(airbnb$host_identity_verified)]<-1
count(airbnb,"host_identity_verified")
airbnb$host_identity_verified <- as.factor(airbnb$host_identity_verified)



#######################################################
# class(airbnb$host_response_rate)
# 
# count(airbnb,"host_response_rate")
# airbnb$host_response_rate <- as.numeric(sub("%", "",airbnb$host_response_rate,fixed=TRUE))/100
# summary(airbnb$host_response_rate)
# airbnb$host_response_rate<-as.numeric(airbnb$host_response_rate)
# sum(is.na(airbnb$host_response_rate))

# NA values. think if anything needs to be done. 


#######################################################
count(airbnb,"is_business_travel_ready")
unique(airbnb$is_business_travel_ready)
airbnb$is_business_travel_ready[!(airbnb$is_business_travel_ready == "f" | airbnb$is_business_travel_ready == "t")] <- NA

airbnb$is_business_travel_ready <- ifelse(is.na(airbnb$is_business_travel_ready),"O",airbnb$is_business_travel_ready)
airbnb$is_business_travel_ready <- as.factor(airbnb$is_business_travel_ready)
class(airbnb$is_business_travel_ready)
count(airbnb,"is_business_travel_ready")



#dunno - Updated the junk values with NA. NO sure what do with them though.
# class(airbnb$is_business_travel_ready)
# levels(airbnb$is_business_travel_ready)
# summary(airbnb$is_business_travel_ready)
# count(airbnb,"is_business_travel_ready")
# airbnb$availability_60<- as.integer(airbnb$availability_60)


#######################################################
#dunno
# class(airbnb$longitude)
# summary(airbnb$longitude)
# airbnb$airbnb$longitude<- as.integer(as.character(airbnb$longitude))
# count(is.na(airbnb$longitude))
# 
# 
# #######################################################
# #what to do??
# class(airbnb$neighbourhood)
# summary(airbnb$neighbourhood)
# count(airbnb, "neighbourhood")
# count(is.na(airbnb$neighbourhood))


#######################################################
class(airbnb$requires_license)
summary(airbnb$requires_license)
airbnb$requires_license[is.na(airbnb$requires_license)]<-"f" # why replacing with t then the majority class is f ?
count(airbnb,"requires_license")
airbnb$requires_license <-  as.factor(airbnb$requires_license)
class(airbnb$requires_license)

#-----------------------------End of Code - Francis--------------------------------------------------------------

#-----------------------------Begin of Code - Vikrant------------------------------------------------------------

#city_name
# 
# airbnb$city_length <- nchar(as.character(airbnb$city_name))
# 
# airbnb$city_name<-ifelse(airbnb$city_length>=20,NA,airbnb$city_name)
# count(airbnb,"city_name")
# airbnb$city_name <- as.factor(airbnb$city_name)

#host_has_profile_pic

#replace numeric values with f

#updated the  code. It was giving wrong values. 
#**************************************8
count(airbnb,"host_has_profile_pic")
airbnb$host_has_profile_pic[!(airbnb$host_has_profile_pic == "f" | airbnb$host_has_profile_pic == "t")] <- "f"
class(airbnb$host_has_profile_pic)
airbnb$host_has_profile_pic <- ifelse(is.na(airbnb$host_has_profile_pic),"O",airbnb$host_has_profile_pic)
airbnb$host_has_profile_pic <- as.factor(airbnb$host_has_profile_pic)
class(airbnb$host_has_profile_pic)
count(airbnb,"host_has_profile_pic")
#*************************

#replace blank values with t commented this.because made a seperate factor of the blank values. 
#we should do this for all factors actually. need to ask someone. 

# unique(airbnb$host_has_profile_pic)
# airbnb$host_has_profile_pic<-ifelse(is.na(airbnb$host_has_profile_pic),'t',airbnb$host_has_profile_pic)


#host_is_supehost

#if value is not t or f, replace with f(majority)
count(airbnb,"host_is_superhost")
airbnb$host_is_superhost[!(airbnb$host_is_superhost == "f" | airbnb$host_is_superhost == "t")] <- "f"
class(airbnb$host_has_profile_pic)
airbnb$host_is_superhost <- ifelse(is.na(airbnb$host_is_superhost),"O",airbnb$host_is_superhost)
airbnb$host_is_superhost <- as.factor(airbnb$host_is_superhost)
class(airbnb$host_is_superhost)
count(airbnb,"host_is_superhost")

# edited the code it wasnt working.  

#host_response_time

#replace blanks and junk values with -within an hour(majority)

#airbnb$host_response_time<-if(airbnb$host_response_time==" ",'within an hour', airbnb$host_response_time)

#is_location exact
#replace blanks with t(majority)

count(airbnb,"is_location_exact")
class(airbnb$is_location_exact)
airbnb$is_location_exact <- ifelse(is.na(airbnb$is_location_exact),"O",airbnb$is_location_exact)
airbnb$is_location_exact <- as.factor(airbnb$is_location_exact)
class(airbnb$is_location_exact)
count(airbnb,"is_location_exact")
#airbnb$is_location_exact<-ifelse(airbnb$is_location_exact==" ",'t',airbnb$is_location_exact)

#market
#if numeric or blank, replace with city_name
# 
# count(airbnb,"market")
# numbers <- c(1,2,3,4,5,6,7,8,9,0)
# airbnb$market <- ifelse((grepl(1,airbnb$market)),airbnb$city_name,airbnb$market)  
# 

#code isnt working. need to replace with working code. cant figure it out as of yet. 

#room_type

#if blank, set to Entire home/apt(majority)

count(airbnb,"room_type")
airbnb$room_type<-ifelse(is.na(airbnb$room_type),'Entire home/apt',airbnb$room_type)
class(airbnb$room_type)
airbnb$room_type <- as.factor(airbnb$room_type)
class(airbnb$room_type)

#state
#replace junk values with blank and all blank with NA(which can later be ignored)

count(airbnb,"state")
airbnb$state <- toupper(airbnb$state)
airbnb$state <- ifelse(airbnb$state=='NEW YORK',"NY",airbnb$state)
airbnb$state <- ifelse(airbnb$state=='BAJA CALIFORNIA',"CA",airbnb$state)
class(airbnb$state)
airbnb$state <- as.factor(airbnb$state)
class(airbnb$state)
# airbnb$state_length <- nchar(as.character(airbnb$state))
# 
# airbnb$state<-ifelse(airbnb$state_length>=2," ",airbnb$state)
# airbnb$state<-ifelse(is.na(airbnb$state),'NA',airbnb$state)


#-----------------------------End of Code - Vikrant------------------------------------------------------------

#-----------------------------Begin of Code - Ashwini----------------------------------------------------------

#Monthly Price - fix this after fixing Price as blank values have to be filled first
#not needed. I did this.
# count(airbnb,"monthly_price")
# #Fix
# airbnb$monthly_price <- as.numeric(airbnb$monthly_price)
# airbnb$price <- as.numeric(airbnb$price)
# airbnb$monthly_price <- ifelse(is.na(airbnb$monthly_price) == TRUE,airbnb$price*30,airbnb$monthly_price)

#License
count(airbnb, "license") #Not worth pursuing, not a single legitimate value

#Experiences Offered
count(airbnb, "experiences_offered") #Just 6 have values, 99991 as none and 3 NA - Not worth pursuing

#Instant Bookable
class(airbnb$instant_bookable)
# ib_count <- count(airbnb, "instant_bookable") #7 column shift issues, 12 NA, everything else is fine
# ib_max <- max(ib_count$freq)
# ib_row <- as.numeric(which(ib_count$freq == max(ib_count$freq)))
# ib_replace <- as.character(droplevels(ib_count$instant_bookable[ib_row]))
airbnb$instant_bookable[!(airbnb$instant_bookable == "f" | airbnb$instant_bookable == "t") | is.na(airbnb$instant_bookable)] <- "f"
count(airbnb, "instant_bookable")
airbnb$instant_bookable <- as.factor(airbnb$instant_bookable)
class(airbnb$instant_bookable)

#-----------------------------End of Code - Ashwini------------------------------------------------------------

#-----------------------------Begin of Code - Abhishek---------------------------------------------------------

#country
#airbnb$country <- NULL

#CountryCode
#is.data.frame(airbnb)
str(airbnb$country_code)
count(airbnb,"country_code")
unique(airbnb$country_code) 

airbnb$ccode <- gsub("f","US",airbnb$country_code)
airbnb$ccode[is.na(airbnb$ccode)]<-"US"
unique(airbnb$ccode)
airbnb$country_code <- airbnb$ccode

unique(airbnb$country_code)
airbnb$ccode <- NULL

##Host Location

count(airbnb,"host_location")
unique(airbnb$host_location)

airbnb$host_is_US <- ifelse(str_detect(airbnb$host_location, "United States"),1,0)
airbnb$is_US <- ifelse(str_detect(airbnb$host_location,"US"),1,0)
airbnb$host_from_US <- ifelse((airbnb$host_is_US==1)|(airbnb$is_US==1),1,0)
airbnb$host_from_US[is.na(airbnb$host_from_US)]<-1
unique(airbnb$host_from_US)
count(airbnb,"host_from_US")
#airbnb$host_location <- NULL
airbnb$host_is_US <- NULL
airbnb$is_US <- NULL
class(airbnb$host_from_US)
airbnb$host_from_US <- as.factor(airbnb$host_from_US)
class(airbnb$host_from_US)

##Host verification
# count(airbnb,"host_verification")
# unique(airbnb$host_identity_verified)

##Latitude
# airbnb$latitude <- NULL


##Max and min nights

unique(airbnb$maximum_nights)
count(airbnb,"maximum_nights")
is.numeric(airbnb$minimum_nights)
airbnb$minimum_nights <- as.numeric(airbnb$minimum_nights)
airbnb$maximum_nights <- as.numeric(airbnb$maximum_nights)
count(airbnb,"maximum_nights")
class(airbnb$maximum_nights)
airbnb$maximum_nights <- ifelse(airbnb$maximum_nights<=365,airbnb$maximum_nights,365)

airbnb$minimum_nights <- ifelse(airbnb$minimum_nights<=365,airbnb$minimum_nights,365)


count(airbnb,"maximum_nights")
count(airbnb,"minimum_nights")

##Guest Phone Verification
unique(airbnb$require_guest_phone_verification)
unique(airbnb$require_guest_phone_verif)
count(airbnb,"require_guest_phone_verif")
airbnb$require_guest_phone_verif <- ifelse(airbnb$require_guest_phone_verification=="f",0,1)
airbnb$require_guest_phone_verif[is.na(airbnb$require_guest_phone_verif)]<-0
unique(airbnb$require_guest_phone_verif)
airbnb$require_guest_phone_verification <- airbnb$require_guest_phone_verif
airbnb$require_guest_phone_verif <- NULL
class(airbnb$require_guest_phone_verification)
airbnb$require_guest_phone_verification <- as.factor(airbnb$require_guest_phone_verification)
class(airbnb$require_guest_phone_verification)

##Smart Location
unique(airbnb$smart_location)
airbnb$smart.location <- ifelse(airbnb$smart_location!="",1,0)
airbnb$smart.location[is.na(airbnb$smart.location)]<-0
class(airbnb$smart.location)
airbnb$smart.location <- as.factor(airbnb$smart.location)
class(airbnb$smart.location)
unique(airbnb$smart.location)

#guest_Included

count(airbnb,"guests_included")
airbnb$guests_included <- as.numeric(airbnb$guests_included)
str(airbnb$guests_included)
airbnb$guests_included <- ifelse(airbnb$guests_included < 0,round(mean(airbnb$guests_included, na.rm=TRUE)),airbnb$guests_included)
airbnb$guests_included <- ifelse(is.na(airbnb$guests_included) ==TRUE, round(mean(airbnb$guests_included, na.rm=TRUE)),airbnb$guests_included)


#-----------------------------End of Code - Abhishek-----------------------------------------------------------


str(airbnb)
