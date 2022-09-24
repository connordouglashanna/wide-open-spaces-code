#loading dataset

library(readr)
#land_database_2016 <- read_csv("land_database_2016.csv")
#View(land_database_2016)

#modified in Python to do all the things I couldn't accomplish in R

land_database_modified <- read_csv("~/Wide_Open_Spaces/Thesis Work/python/land_database_dists_to_cap.csv")
#View(land_database_dists_to_cap)

#loading packages

#install.packages("dplyr", "rgeos", "sp", "sf", "readr", "raster", "maptools", "stringr", "rangeMapper")
library(dplyr)
library(rgeos)
library(sp)
library(sf)
library(raster)
library(tidyverse)
library(maptools)
library(stringr)
library(rangeMapper)
library(wellknown)
library(broom)
library(texreg)
library(stargazer)
library(stringi)

#dropping stuff we don't need
#mostly the tax-relevant parts of the dataset
dropvars <- c("CONDOID", "SOURCE", "NOTES", "OWNER", "BLOCK", "DBA", "AG_USE_VAL", "AG_MARKET", "HS_EXEMPT", "ENTITIES",
              "COA_TAXABL", "TRAVIS_TAX", "COUNCIL_DI", "OV65_EXEMP", "LEGAL_DESC", "LEGAL_DE_1", "HOOD_CD",
              "TEN_PERCEN", "CONSTRAINE", "LAND_HSTD_", "LAND_NON_H", "DEED_DT", "TRACT_OR_L", "PROP_TYPE_", "PROPVAL_YR",
              "SUP_NUM", "PY_OWNER_I", "GISLINK", "PROPVAL_YR", "ABS_SUBDV_", "APPRAISED_", "ASSESSED_V", "LOTSIZE", "LAND_ACRES",
              "LAND_STATE", "DEPRECIATI", "PREVIOUSLU", "PREVIOUSGE", "Cnt_GISLIN", "LAND_USE", "GEN_LAND_U", 
              "LU_DESC", "GEN_LU_DES", "PID_10", "IMPRV_HSTD", "IMPRV_STAT", "IMPRV_TYDE", "BASEZONE" )

ld_2016 <- dplyr::select (land_database_modified, -all_of(dropvars))
rm(dropvars)

#converting I35 variable to binary

ld_2016$I35SIDE_str <- ld_2016$I35SIDE

ld_2016$EastofI35 <- ifelse(ld_2016$I35SIDE == "East", 1, ifelse(ld_2016$I35SIDE == "West", 0, NA))

#####

#geometry is classified as character strings... this is a problem

#class(ld_2016$the_geom)

#loop to apply readWKT across the entire dataset, and transform the variable class into something that doesn't 
#read an error message

#the_geom_sp_entry <- SpatialPolygons(list())
#the_geom_sp <- list()

#update: what if I run lapply() here instead?

#for (n in 1:length(ld_2016$PROP_ID)) {
#  the_geom_sp[n] <- the_geom_sp_entry
#  the_geom_sp[n] <- readWKT(ld_2016$the_geom[n])
#}

#sp_2016 <- SpatialPolygonsDataFrame(the_geom_sp, ld_2016)

#ld_2016$centroid <- gCentroid(ld_2016$the_geom)


#generating point representing central austin
#capitol will be used as "central" polygon

#capitol_hill <- subset(ld_2016, ld_2016$PROP_ID == 197003)
#capitol_hill <- select (capitol_hill, c(the_geom, centroid))

#adding variable for distance to the city center

#ld_2016$distance_capitol <- gDistance(capitol_hill$centroid, ld_2016$centroid)

#####

#parsing ZIP codes from the SITUS variable

ld_2016$ZIP <- stri_extract_last_regex(ld_2016$SITUS, "\\d{5}")

table(ld_2016$ZIP)

#now to drop the weirdo extras...

ld_2016$ZIP <- str_remove_all(ld_2016$ZIP, "1\\d{4}|00000|2\\d{4}")

ld_2016$ZIP <- as.numeric(ld_2016$ZIP)

ld_2016 <- dplyr::filter(ld_2016, ld_2016$ZIP > 0)

ld_2016$ZIP <- as.character(ld_2016$ZIP)

table(ld_2016$ZIP)
class(ld_2016$ZIP)

#removing a bunch of empty entries that were labeled 0

ld_2016 <- dplyr::filter(ld_2016, ld_2016$MARKET_VAL > 0)
ld_2016$market_val <- ld_2016$MARKET_VAL

ld_2016 <- dplyr::filter(ld_2016, ld_2016$SUM_IMPRV_ > 0)
ld_2016$building_sqft <- ld_2016$SUM_IMPRV_

ld_2016 <- dplyr::filter(ld_2016, ld_2016$YR_BUILT > 0)
ld_2016$yr_built <- ld_2016$YR_BUILT

#lots of missing lot size values... until land area can be recalculated manually using polygons, this will have to do

ld_2016 <- dplyr::filter(ld_2016, ld_2016$TOTAL_LAND > 0)
ld_2016$lotsize_sqft <- ld_2016$TOTAL_LAND

#converting distance to imperial

ld_2016$distance_capitol_km <- ld_2016$dists_to_cap
ld_2016$distance_capitol_mi <- (.6213712*ld_2016$distance_capitol_km)

#dropping old names

old_names <- c("MARKET_VAL", "SUM_IMPRV_", "YR_BUILT", "TOTAL_LAND", "dists_to_cap")

ld_2016 <- ld_2016 <- dplyr::select (ld_2016, -all_of(old_names))

rm(old_names)

#removing erroneous FAR entries

ld_2016 <- dplyr::filter(ld_2016, ld_2016$FAR > 0)

#sfh only dataframe
#removing "Single Family Condominium (SF-4B)" until minlotsize dynamic is better understood

single_family_types <- c("Single Family Large Lot (SF-1)", "Single Family Standard Lot (SF-2)",
                         "Single Family Residence (SF-3)", "Single Family Small Lot (SF-4A)",
                         "Urban Family Residence (SF-5)", "Townhouse and Condominium (SF-6)")

sfh_db <- ld_2016[ld_2016$EFF_ZONE %in% single_family_types, ]

#residential only dataframe

residential_db <- subset(ld_2016, ld_2016$UNITS > 0)

#encoding minimum lot sizes

minimum_lot_size_sqft <- c(10000, 5750, 5750, 3600, 5750, 5750)
single_family_iteration <- c(1,2,3,4,5,6)
properties_count <- c(6969, 63059, 67334, 10430, 77, 2127)

min_lot_df <- data.frame(min_lot_size_sqft = minimum_lot_size_sqft, EFF_ZONE = single_family_types, 
                         single_family_iteration = single_family_iteration, properties_count = properties_count)

sfh_db <- base::merge(sfh_db, min_lot_df)

#cleaning up the environment

rm(ld_2016)
rm(land_database_2016)
rm(land_database_modified)

#generating log(market_val) to run log regressions

sfh_db$lmarketval <- log(sfh_db$market_val)

#generating ZIP_density

zip_list <- unique(sfh_db$ZIP)
zip_density <- c()

for (i in 1:56) {
        zip_df = subset(sfh_db, sfh_db$ZIP == zip_list[i])
        zip_density[i] = (nrow(zip_df))/(sum(zip_df$lotsize_sqft)/43560.04)
}

#this variable is now in acres

#merging lists to a dataframe

zip_df <- data.frame(zip_density = zip_density, ZIP = zip_list)

#cool, now let's attach it to the main df...

sfh_db <- base::merge(sfh_db, zip_df)

#proportion of SFH in total housing stock

total_housing_units <- sum(residential_db$UNITS, na.rm = TRUE)
sfh_housing_units <- sum(sfh_db$UNITS, na.rm = TRUE)

sfh_percent_total_stock <- (sfh_housing_units/total_housing_units)
print(sfh_percent_total_stock)

#priors for mathematical intuition
#generating placeholder construction cost

construction <- 150

#regressions

#lotsize and housing characteristics

lm_nomin_marketval <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35 
                      + building_sqft + FAR + ILR, data = sfh_db)
summary(lm_nomin_marketval)

#lotsize and minimum lot size

lm_hc_marketval <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35 
                     + building_sqft + min_lot_size_sqft + FAR + ILR, data = sfh_db)
summary(lm_hc_marketval)

#added density control 

lm_dens_marketval <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35 
                     + building_sqft + min_lot_size_sqft + zip_density + FAR + ILR, data = sfh_db)
summary(lm_hc_marketval)

#proportion of SFH in total housing stock

total_housing_units <- sum(residential_db$UNITS, na.rm = TRUE)
sfh_housing_units <- sum(sfh_db$UNITS, na.rm = TRUE)

sfh_percent_total_stock <- (sfh_housing_units/total_housing_units)
print(sfh_percent_total_stock)

#priors for mathematical intuition
#generating placeholder construction cost

construction <- 150

#generating mean variables for model

mlotsize <- mean(sfh_db$lotsize_sqft)
mdistance <- mean(sfh_db$distance_capitol_mi)
myrbuilt <- mean(sfh_db$yr_built)
mI35 <- mean(sfh_db$EastofI35)
msqft <- mean(sfh_db$building_sqft)
mminlotsize <- mean(sfh_db$min_lot_size)
mFAR <- mean(sfh_db$FAR)
mILR <- mean(sfh_db$ILR)

#density calculations using available data...

total_sqft <- sum(sfh_db$lotsize_sqft)
sfh_density <- total_sqft/(sum(sfh_db$UNITS))
mean_sqft_lot_per_unit <- sfh_density

#model output...

stored_priors <- tidy(lm_hc_marketval)

mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
        (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
        (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)

equation2 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val) + (msqft * construction))
print(equation2)

inequality3 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
print(inequality3)

#great! Now what about individually...

        #SFH-1
        #########################
        
        sfh_db_1 <- subset(sfh_db, sfh_db$single_family_iteration == 1)
        
        lm_hc_marketval_sfh_db_1 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35
                                       + building_sqft + FAR + ILR, data = sfh_db_1)
        summary(lm_hc_marketval)
       
        mlotsize <- mean(sfh_db_1$lotsize_sqft)
        mdistance <- mean(sfh_db_1$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_1$yr_built)
        mI35 <- mean(sfh_db_1$EastofI35)
        msqft <- mean(sfh_db_1$building_sqft)
        mminlotsize <- mean(sfh_db_1$min_lot_size)
        mFAR <- mean(sfh_db_1$FAR)
        mILR <- mean(sfh_db_1$ILR)
        
        total_sqft <- sum(sfh_db_1$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_1$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_1 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_1 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        #####
        #SFH-2
        #########################
        
        sfh_db_2 <- subset(sfh_db, sfh_db$single_family_iteration == 2)
        
        lm_hc_marketval_sfh_db_2 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35
                                       + building_sqft + FAR + ILR, data = sfh_db_2)
        summary(lm_hc_marketval)
        
        mlotsize <- mean(sfh_db_2$lotsize_sqft)
        mdistance <- mean(sfh_db_2$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_2$yr_built)
        mI35 <- mean(sfh_db_2$EastofI35)
        msqft <- mean(sfh_db_2$building_sqft)
        mminlotsize <- mean(sfh_db_2$min_lot_size)
        mFAR <- mean(sfh_db_2$FAR)
        mILR <- mean(sfh_db_2$ILR)
        
        total_sqft <- sum(sfh_db_2$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_2$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_2 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_2 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        #####
        #SFH-3
        #########################
        
        sfh_db_3 <- subset(sfh_db, sfh_db$single_family_iteration == 3)
       
        lm_hc_marketval_sfh_db_3 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35
                                       + building_sqft + FAR + ILR, data = sfh_db_3)
        summary(lm_hc_marketval)
        
        mlotsize <- mean(sfh_db_3$lotsize_sqft)
        mdistance <- mean(sfh_db_3$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_3$yr_built)
        mI35 <- mean(sfh_db_3$EastofI35)
        msqft <- mean(sfh_db_3$building_sqft)
        mminlotsize <- mean(sfh_db_3$min_lot_size)
        mFAR <- mean(sfh_db_3$FAR)
        mILR <- mean(sfh_db_3$ILR)
        
        total_sqft <- sum(sfh_db_3$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_3$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_3 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_3 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        #####
        #SFH-4
        #########################
        
        sfh_db_4 <- subset(sfh_db, sfh_db$single_family_iteration == 4)
        
        lm_hc_marketval_sfh_db_4 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35 + 
                                               building_sqft + FAR + ILR, data = sfh_db_4)
        summary(lm_hc_marketval)
        
        mlotsize <- mean(sfh_db_4$lotsize_sqft)
        mdistance <- mean(sfh_db_4$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_4$yr_built)
        mI35 <- mean(sfh_db_4$EastofI35)
        msqft <- mean(sfh_db_4$building_sqft)
        mminlotsize <- mean(sfh_db_4$min_lot_size)
        mFAR <- mean(sfh_db_4$FAR)
        mILR <- mean(sfh_db_4$ILR)
        
        total_sqft <- sum(sfh_db_4$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_4$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_4 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_4 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        #####
        #SFH-5
        #########################
        
        sfh_db_5 <- subset(sfh_db, sfh_db$single_family_iteration == 5)
        
        lm_hc_marketval_sfh_db_5 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35
                                       + building_sqft + FAR + ILR, data = sfh_db_5)
        summary(lm_hc_marketval)
        
        mlotsize <- mean(sfh_db_5$lotsize_sqft)
        mdistance <- mean(sfh_db_5$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_5$yr_built)
        mI35 <- mean(sfh_db_5$EastofI35)
        msqft <- mean(sfh_db_5$building_sqft)
        mminlotsize <- mean(sfh_db_5$min_lot_size)
        mFAR <- mean(sfh_db_5$FAR)
        mILR <- mean(sfh_db_5$ILR)
        
        total_sqft <- sum(sfh_db_5$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_5$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_5 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_5 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        
        #####
        #SFH-6
        #########################
        
        sfh_db_6 <- subset(sfh_db, sfh_db$single_family_iteration == 6)
        
        lm_hc_marketval_sfh_db_6 <- lm(market_val ~ lotsize_sqft + distance_capitol_mi + yr_built + EastofI35 + 
                                               building_sqft + min_lot_size_sqft + FAR + ILR, data = sfh_db_6)
        summary(lm_hc_marketval)
        
        mlotsize <- mean(sfh_db_6$lotsize_sqft)
        mdistance <- mean(sfh_db_6$distance_capitol_mi)
        myrbuilt <- mean(sfh_db_6$yr_built)
        mI35 <- mean(sfh_db_6$EastofI35)
        msqft <- mean(sfh_db_6$building_sqft)
        mminlotsize <- mean(sfh_db_6$min_lot_size)
        mFAR <- mean(sfh_db_6$FAR)
        mILR <- mean(sfh_db_6$ILR)
        
        total_sqft <- sum(sfh_db_6$lotsize_sqft)
        sfh_density <- total_sqft/(sum(sfh_db_6$UNITS))
        
        stored_priors <- tidy(lm_hc_marketval)
        
        mean_sfh_val <- (stored_priors$estimate[1] * mminlotsize ) + (stored_priors$estimate[2] * mdistance) + 
                (stored_priors$estimate[3] * myrbuilt) + (stored_priors$estimate[4] * mI35) + 
                (stored_priors$estimate[5] * msqft) + (stored_priors$estimate[6] * mFAR) + (stored_priors$estimate[7] * mILR)
        
        equation2_sfh_db_6 <- ((total_sqft/(mminlotsize^2)) * mean_sfh_val) - ((mminlotsize^(-1) * mean_sfh_val)) - (mean_sfh_val) + (msqft * construction)
        print(equation2)
        
        inequality3_sfh_db_6 <- ifelse(equation2 <= 0, "TRUE", ifelse(equation2 > 0, "FALSE", NA))
        print(inequality3)
        
        #####

#collating mathematical intuition results into a dataframe

#inequality 3

inequality3_results <- c(inequality3, inequality3_sfh_db_1, inequality3_sfh_db_2, inequality3_sfh_db_3, 
                         inequality3_sfh_db_4, inequality3_sfh_db_5, inequality3_sfh_db_6)

#equation 2

equation2_results <- c(equation2, equation2_sfh_db_1, equation2_sfh_db_2, equation2_sfh_db_3, equation2_sfh_db_4,
                       equation2_sfh_db_5, equation2_sfh_db_6)

#no. of obervations

no_obs <- c(153160, 7055, 63878, 69494, 10489, 77, 2167)

#organizing by test subset

item_testno <- c("All SFH", single_family_types)

#adding minimum lot sizes

minimum_lot_size_sqft <- c("NA", minimum_lot_size_sqft)

#creating a dataframe to be printed as a table

full_test_results <- data.frame(test_sample = item_testno, equation2_magnitude = equation2_results, 
                                inequality3_results = inequality3_results, no_obs = no_obs, 
                                min_lot_sqft = minimum_lot_size_sqft)

#tidying the environment, removing a bunch of values

rm(stored_priors, inequality3, inequality3_sfh_db_1, inequality3_sfh_db_2, inequality3_sfh_db_3, 
   inequality3_sfh_db_4, inequality3_sfh_db_5, inequality3_sfh_db_6, item_testno, mlotsize, mdistance, myrbuilt,
   mI35, msqft, mminlotsize, single_family_iteration, single_family_types, total_sqft, 
   equation2, equation2_sfh_db_1, equation2_sfh_db_2, equation2_sfh_db_3, equation2_sfh_db_4, equation2_sfh_db_5,
   equation2_sfh_db_6, mean_sfh_val) 

#finally got the fucking permit data...

#permits <- read_csv("C:/Users/Connor/Downloads/fredgraph.csv")

#permits$year <- stri_extract_last_regex(permits$DATE, "\\d{4}")

#permits$permits_issued <- permits$AUST448BPPRIVSA

#Bgon <- c("DATE", "AUST448BPPRIVSA", "POPTHM")

#permits <- dplyr::select(permits, -all_of(Bgon))

# <- dplyr::filter(permits, year >= 2010)

#permits <- dplyr::filter(permits, year < 2020)

#census population data 2010-2019

#pop <- read_csv("C:/Users/Connor/Downloads/cbsa-est2019-alldata.csv")

#pop <- dplyr::filter(pop, pop$CBSA == 12420)

#pop <- dplyr::filter(pop, pop$LSAD == "Metropolitan Statistical Area")

#years <- c("POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012", "POPESTIMATE2013", "POPESTIMATE2014",
#           "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017", "POPESTIMATE2018", "POPESTIMATE2019")

#pop <- dplyr::select(pop, all_of(years))

#pop <- pivot_longer(pop, cols = years)

#pop$population <- pop$value

#pop$pop_dens <- pop$population/4279

#pop$year <- pop$name

#years <- c(2010:2019)

#$year <- years

#keepers <- c("year", "population", "pop_dens")

#pop <- dplyr::select(pop, all_of(keepers))

#now kith

#poppermits <- merge(pop, permits)

#regression

#lm_denspermits <- lm(permits_issued ~ pop_dens, data = poppermits)

#graph 

#permit_plot <- ggplot(data = poppermits, mapping = aes(x = year, y = pop_dens)
#                      + geom_line(permits_issued)
#                      + geom_line(pop_dens))
#permit_plot

#####
#NLLUS has too many NA variables to be truly useful... at least in longitudinal form?
#####


#importing regulation index dataset

#Longitudinal_NLLUS_Data_Long_w_Census_ <- read_csv("2020-2021/Thesis Work/Thesis Data/Longitudinal_NLLUS_Data_Long (w Census).csv")

#renaming

#NLLUS_data <- Longitudinal_NLLUS_Data_Long_w_Census_

#dropping original

#rm(Longitudinal_NLLUS_Data_Long_w_Census_)

#subsetting to Austin

#NLLUS_data <- subset(NLLUS_data, NLLUS_data$metro_nllus == "Austin")

#filtering to important variables

#keep_variables <- c()

#NLLUS_data <- dplyr::select(NLLUS_data, all_of(keep_variables))


#####
#continuing to table generation

#linear regressions

#triplet hc table, fix the formatting?

#stargazer(lm_nomin_marketval, lm_hc_marketval, lm_dens_marketval, 
 #         covariate.labels = c("Lot Size", "Distance from Capitol", "Year Built", "East of I35", "Home Size",
  #        "Minimum Lot Size", "ZIP Code Density", "FAR", "ILR"), title = "Outputs for Regressions 1-3",
   #       column.sep.width = "4", style = "aer")

#lm nominlot

stargazer(lm_nomin_marketval, style = "aer")

#lm hc minlot

stargazer(lm_hc_marketval, style = "aer")

#lm dens minlot

stargazer(lm_dens_marketval, style = "aer")

#sfh-1

stargazer(lm_hc_marketval_sfh_db_1, style = "aer")

#sfh-2

stargazer(lm_hc_marketval_sfh_db_2, style = "aer")

#sfh-3

stargazer(lm_hc_marketval_sfh_db_3, style = "aer")

#sfh-4a

stargazer(lm_hc_marketval_sfh_db_4, style = "aer")

#sfh-5

stargazer(lm_hc_marketval_sfh_db_5, style = "aer")

#sfh-6

stargazer(lm_hc_marketval_sfh_db_6, style = "aer")

#mathematical intuition
#results df

stargazer(full_test_results, summary = F, style = "aer", font.size = "small")

#dataframe summary

stargazer(sfh_db, summary = T, style = "aer", font.size = 4)

#other odds and ends in the appendices section
#minimum lot index

stargazer(min_lot_df, summary = F, style = "aer")

#total housing usits table

housing_breakdown <- data.frame(total_housing_units = total_housing_units, sf_housing_units = sfh_housing_units,
                                 percent_single_family = sfh_percent_total_stock)
stargazer(housing_breakdown, summary = F, style = "aer")


##################################### shapefile witchcraft

#second_reading_dbf <- read.dbf("/2020-2021/Thesis Work/Thesis Data/Proposed Zoning Second Reading/Proposed_Zoning_Second_Reading.dbf")
