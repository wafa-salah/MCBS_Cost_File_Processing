#                                         #
# Title:  MCBS Cost Suplement Cleaning    #
# Author: Wafa Salah                      #
# Date:   01/26/2023                      #
#                                         #
###########################################


library(tidyverse)
library(CRbasics)
library(stringr)
library(lubridate)

#CRprojectBuilder()  #run the very first time only

getwd()

#Import ---- 

raw_files_dir <- "C:/Users/ws486/Documents/MCBS/Data/Cost_Supplement/"
var_fl <- read.csv("data/cost_supplement_variables3.csv")
seg <- unique(var_fl$file_segment) #list of the file segments 'index' parameter in fltr_data indexes from

# file downloaded from: https://data.bls.gov/cgi-bin/surveymost?cu --> Medical care - CUUR0000SAM
cpi <- as.data.frame(read.csv("data/CPI_MedicalCare_SeriesReport-20230122174001_83cddb.csv"))

seg
#each year's files
fl_12 <- fltr_data(2012,5) #update 2nd param depending on ope,ipe,mpe,iue, fae
fl_13 <- fltr_data(2013,5)
fl_15 <- fltr_data(2015,5) 
fl_16 <- fltr_data(2016,5)
fl_17 <- fltr_data(2017,5)
fl_18 <- fltr_data(2018,5)
fl_19 <- fltr_data(2019,5)
fl_20 <- fltr_data(2020,5)

#ISSUE1
table(fl_15$EVBEGYY) #returns 93 obs with EVBEGYY in 2016, but FROMDT has correct year



#Cleaning CPI----

cpi$Annual_Avg <- round2(rowMeans(cpi[,c("HALF1","HALF2")]),3)
row.names(cpi) <- cpi$Year



#Cleaning OPE ---- 

  #OPE ----
  #TODO's before merging: 
    # (1) 2015/2016 no STATUS var present
    # (2) 2015/2016 get date following FROMDT/THRUDT --> EVBEGXX
    # (3) 2015 has separare AMTVA column; add onto AMTOTH
    # (3) remove unwanted columns and readjust colnames

    
  # 2012
  fl_12$FRMdate <- mapply(to_date_1213,fl_12$FROMDT)
  fl_12$THRUdate <- mapply(to_date_1213,fl_12$THRUDT)
  table(fl_12$EVBEGYY)
  fl_12$EVBEGYY <- ifelse(fl_12$EVBEGYY == "11", "11", ifelse(fl_12$EVBEGYY == "12", "12", "") )
  fl_12$EVBEGMM <- ifelse(fl_12$EVBEGMM < 0, "", paste0(fl_12$EVBEGMM))
  fl_12$EVBEGDD <- ifelse(fl_12$EVBEGDD < 0, "", paste0(fl_12$EVBEGDD)) 
  fl_12$DateTmp <- paste0("20", fl_12$EVBEGYY,"-",fl_12$EVBEGMM,"-",fl_12$EVBEGDD)
  fl_12 <- fl_12 %>% mutate(Date = ifelse(FRMdate == "NA", DateTmp, FRMdate)) %>% 
    relocate(Date, .after = SURVEYYR)
  
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  #write.csv(fl_12, file = "data/OPE_2012.csv", quote = FALSE, row.names = FALSE)
  fl_12$STATUS <- NA
  fl_12$AMTOTH <- fl_12$AMTOTH2
  # fl_12$BASEID2 <- substr(fl_12$BASEID,2,str_length(fl_12$BASEID))
  # fl_12$BASEID <- fl_12$BASEID2
  fl_12 <- select(fl_12, -c(FROMDT, THRUDT, EVBEGYY, EVBEGMM, EVBEGDD, AMTVA, FRMdate, THRUdate, DateTmp, AMTOTH2) )
  colnames(fl_12)
  colnames(fl_12)[13] <- "AMTMADV" # AMTHMOM --> AMTMADV
  

  # 2013
  fl_13$FRMdate <- mapply(to_date_1213,fl_13$FROMDT)
  fl_13$THRUdate <- mapply(to_date_1213,fl_13$THRUDT)
  table(fl_13$EVBEGYY)
  fl_13$EVBEGYY <- ifelse(fl_13$EVBEGYY == "12", "12", ifelse(fl_13$EVBEGYY == "13", "13", "") )
  fl_13$EVBEGMM <- ifelse(fl_13$EVBEGMM < 0, "", paste0(fl_13$EVBEGMM))
  fl_13$EVBEGDD <- ifelse(fl_13$EVBEGDD < 0, "", paste0(fl_13$EVBEGDD)) 
  fl_13$DateTmp <- paste0("20", fl_13$EVBEGYY,"-",fl_13$EVBEGMM,"-",fl_13$EVBEGDD)
  fl_13 <- fl_13 %>% mutate(Date = ifelse(FRMdate == "NA", DateTmp, FRMdate)) %>% 
    relocate(Date, .after = SURVEYYR)
  
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  #write.csv(fl_13, file = "data/OPE_2013.csv", quote = FALSE, row.names = FALSE)
  
  fl_13$STATUS <- NA
  fl_13$AMTOTH <- fl_13$AMTOTH2
  # fl_13$BASEID2 <- substr(fl_13$BASEID,2,str_length(fl_13$BASEID))
  # fl_13$BASEID <- fl_13$BASEID2
  fl_13 <- select(fl_13, -c(FROMDT, THRUDT, EVBEGYY, EVBEGMM, EVBEGDD, AMTVA, FRMdate, THRUdate, DateTmp, AMTOTH2) )
  colnames(fl_13)
  colnames(fl_13)[13] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  # 2015 
  fl_15$FRMdate <- mapply(to_date,fl_15$FROMDT)
  fl_15$THRUdate <- mapply(to_date,fl_15$THRUDT)
  fl_15$DateTmp <- paste0("20", fl_15$EVBEGYY,"-",fl_15$EVBEGMM,"-",fl_15$EVBEGDD)
  fl_15 <- fl_15 %>% mutate(Date = ifelse(FRMdate == "NA", DateTmp, FRMdate)) %>% 
                     relocate(Date, .after = SURVEYYR)
  
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  #write.csv(fl_15, file = "data/OPE_2015.csv", quote = FALSE, row.names = FALSE)
  
  fl_15$STATUS <- NA
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(FROMDT, THRUDT, EVBEGYY, EVBEGMM, EVBEGDD, AMTVA, FRMdate, THRUdate, DateTmp, AMTOTH2) )

  
  
  # 2016
  fl_16$FRMdate <- mapply(to_date,fl_16$FROMDT)
  fl_16$THRUdate <- mapply(to_date,fl_16$THRUDT)
  fl_16$DateTmp <- paste0("20", fl_16$EVBEGYY,"-",fl_16$EVBEGMM,"-",fl_16$EVBEGDD)
  fl_16 <- fl_16 %>% mutate(Date = ifelse(FRMdate == "NA", DateTmp, FRMdate)) %>% 
    relocate(Date, .after = SURVEYYR)
  #write.csv(fl_16, file = "data/OPE_2016.csv", quote = FALSE, row.names = FALSE)
  
  fl_16$STATUS <- NA
  fl_16 <- select(fl_16, -c(FROMDT, THRUDT, EVBEGYY, EVBEGMM, EVBEGDD, FRMdate, THRUdate, DateTmp) )
  
  
  
  # 2017
  fl_17$DateTmp <- paste0("20", fl_17$D_BEGYY,"-",fl_17$D_BEGMM,"-",fl_17$D_BEGDD)
  fl_17 <- fl_17 %>% mutate(Date = DateTmp) %>% relocate(Date, .after = SURVEYYR)
  #write.csv(fl_17, file = "data/OPE_2017.csv", quote = FALSE, row.names = FALSE)
  
  fl_17 <- select(fl_17, -c(D_BEGYY, D_BEGMM, D_BEGDD, DateTmp) )
  
  
  # 2018
  fl_18$DateTmp <- paste0("20", fl_18$D_BEGYY,"-",fl_18$D_BEGMM,"-",fl_18$D_BEGDD)
  fl_18 <- fl_18 %>% mutate(Date = DateTmp) %>% relocate(Date, .after = SURVEYYR)
  #write.csv(fl_18, file = "data/OPE_2018.csv", quote = FALSE, row.names = FALSE)
  
  fl_18 <- select(fl_18, -c(D_BEGYY, D_BEGMM, D_BEGDD, DateTmp) )
  
  
  
  # 2019
  fl_19$DateTmp <- paste0("20", fl_19$D_BEGYY,"-",fl_19$D_BEGMM,"-",fl_19$D_BEGDD)
  fl_19 <- fl_19 %>% mutate(Date = DateTmp) %>% relocate(Date, .after = SURVEYYR)
  #write.csv(fl_19, file = "data/OPE_2019.csv", quote = FALSE, row.names = FALSE)

  fl_19 <- select(fl_19, -c(D_BEGYY, D_BEGMM, D_BEGDD, DateTmp) )
  
  #2020
  fl_20$DateTmp <- paste0("20", fl_20$D_BEGYY,"-",fl_20$D_BEGMM,"-",fl_20$D_BEGDD)
  fl_20 <- fl_20 %>% mutate(Date = DateTmp) %>% relocate(Date, .after = SURVEYYR)
  #write.csv(fl_20, file = "data/OPE_2020.csv", quote = FALSE, row.names = FALSE)
  
  fl_20 <- select(fl_20, -c(D_BEGYY, D_BEGMM, D_BEGDD, DateTmp) )
  
  
  
#Cleaning IPE ---- 
  
  #IPE ----
  #TODO's before merging: 
  # (1) 2015 has separate AMTVA column; add onto AMTOTH
  # (2) remove unwanted columns and readjust colnames


  #2012
  #fl_12 <- add_column(fl_12, AMTMADV = NA, .after = "AMTCARE")
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  #table(fl_12$EVBEGDD)
  #summary(fl_12)
  fl_12$EVBEGYY <- ifelse(fl_12$EVBEGYY < 0, "", fl_12$EVBEGYY)
  fl_12$EVBEGMM <- ifelse(fl_12$EVBEGMM < 0, "", paste0(fl_12$EVBEGMM))
  fl_12$EVBEGDD <- ifelse(fl_12$EVBEGDD < 0, "", paste0(fl_12$EVBEGDD)) 
  fl_12$EVENDYY <- ifelse(fl_12$EVENDYY < 0, "", fl_12$EVENDYY)
  fl_12$EVENDMM <- ifelse(fl_12$EVENDMM < 0, "", paste0(fl_12$EVENDMM))
  fl_12$EVENDDD <- ifelse(fl_12$EVENDDD < 0, "", paste0(fl_12$EVENDDD)) 
  fl_12 <- add_column(fl_12, Date = paste0("20", fl_12$EVBEGYY,"-",fl_12$EVBEGMM,"-",fl_12$EVBEGDD), .after = "SURVEYYR")
  fl_12 <- add_column(fl_12, Date_End = paste0("20", fl_12$EVENDYY,"-",fl_12$EVENDMM,"-",fl_12$EVENDDD), .after = "Date")
  #write.csv(fl_12, file = "data/IPE_2012.csv", quote = FALSE, row.names = FALSE)
  fl_12$AMTOTH <- fl_12$AMTOTH2
  # fl_12$BASEID2 <- substr(fl_12$BASEID,2,str_length(fl_12$BASEID))
  # fl_12$BASEID <- fl_12$BASEID2
  fl_12 <- select(fl_12, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA))
  colnames(fl_12)
  colnames(fl_12)[14] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  #2013
  #fl_13 <- add_column(fl_13, AMTMADV = NA, .after = "AMTCARE")
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  fl_13$EVBEGYY <- ifelse(fl_13$EVBEGYY < 0, "", fl_13$EVBEGYY)
  fl_13$EVBEGMM <- ifelse(fl_13$EVBEGMM < 0, "", paste0(fl_13$EVBEGMM))
  fl_13$EVBEGDD <- ifelse(fl_13$EVBEGDD < 0, "", paste0(fl_13$EVBEGDD)) 
  fl_13$EVENDYY <- ifelse(fl_13$EVENDYY < 0, "", fl_13$EVENDYY)
  fl_13$EVENDMM <- ifelse(fl_13$EVENDMM < 0, "", paste0(fl_13$EVENDMM))
  fl_13$EVENDDD <- ifelse(fl_13$EVENDDD < 0, "", paste0(fl_13$EVENDDD)) 
  fl_13 <- add_column(fl_13, Date = paste0("20", fl_13$EVBEGYY,"-",fl_13$EVBEGMM,"-",fl_13$EVBEGDD), .after = "SURVEYYR")
  fl_13 <- add_column(fl_13, Date_End = paste0("20", fl_13$EVENDYY,"-",fl_13$EVENDMM,"-",fl_13$EVENDDD), .after = "Date")
  #write.csv(fl_13, file = "data/IPE_2013.csv", quote = FALSE, row.names = FALSE)
  fl_13$AMTOTH <- fl_13$AMTOTH2
  # fl_13$BASEID2 <- substr(fl_13$BASEID,2,str_length(fl_13$BASEID))
  # fl_13$BASEID <- fl_13$BASEID2
  fl_13 <- select(fl_13, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA ))
  colnames(fl_13)
  colnames(fl_13)[14] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  # 2015 
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  fl_15 <- add_column(fl_15, Date = paste0("20", fl_15$EVBEGYY,"-",fl_15$EVBEGMM,"-",fl_15$EVBEGDD), .after = "SURVEYYR")
  fl_15 <- add_column(fl_15, Date_End = paste0("20", fl_15$EVENDYY,"-",fl_15$EVENDMM,"-",fl_15$EVENDDD), .after = "Date")
  #write.csv(fl_15, file = "data/IPE_2015.csv", quote = FALSE, row.names = FALSE)
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA))
  
  
  # 2016 
  fl_16 <- add_column(fl_16, Date = paste0("20", fl_16$EVBEGYY,"-",fl_16$EVBEGMM,"-",fl_16$EVBEGDD), .after = "SURVEYYR")
  fl_16 <- add_column(fl_16, Date_End = paste0("20", fl_16$EVENDYY,"-",fl_16$EVENDMM,"-",fl_16$EVENDDD), .after = "Date")
  write.csv(fl_16, file = "data/IPE_2016.csv", quote = FALSE, row.names = FALSE)
  fl_16 <- select(fl_16, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD) )
  
  
  # 2017 
  fl_17 <- add_column(fl_17, Date = paste0("20", fl_17$D_BEGYY,"-",fl_17$D_BEGMM,"-",fl_17$D_BEGDD), .after = "SURVEYYR")
  fl_17 <- add_column(fl_17, Date_End = paste0("20", fl_17$D_ENDYY,"-",fl_17$D_ENDMM,"-",fl_17$D_ENDDD), .after = "Date")
  write.csv(fl_17, file = "data/IPE_2017.csv", quote = FALSE, row.names = FALSE)
  fl_17 <- select(fl_17, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  # 2018 
  fl_18 <- add_column(fl_18, Date = paste0("20", fl_18$D_BEGYY,"-",fl_18$D_BEGMM,"-",fl_18$D_BEGDD), .after = "SURVEYYR")
  fl_18 <- add_column(fl_18, Date_End = paste0("20", fl_18$D_ENDYY,"-",fl_18$D_ENDMM,"-",fl_18$D_ENDDD), .after = "Date")
  write.csv(fl_18, file = "data/IPE_2018.csv", quote = FALSE, row.names = FALSE)
  fl_18 <- select(fl_18, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  
  # 2019 
  fl_19 <- add_column(fl_19, Date = paste0("20", fl_19$D_BEGYY,"-",fl_19$D_BEGMM,"-",fl_19$D_BEGDD), .after = "SURVEYYR")
  fl_19 <- add_column(fl_19, Date_End = paste0("20", fl_19$D_ENDYY,"-",fl_19$D_ENDMM,"-",fl_19$D_ENDDD), .after = "Date")
  write.csv(fl_19, file = "data/IPE_2019.csv", quote = FALSE, row.names = FALSE)
  fl_19 <- select(fl_19, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  # 2020 
  fl_20 <- add_column(fl_20, Date = paste0("20", fl_20$D_BEGYY,"-",fl_20$D_BEGMM,"-",fl_20$D_BEGDD), .after = "SURVEYYR")
  fl_20 <- add_column(fl_20, Date_End = paste0("20", fl_20$D_ENDYY,"-",fl_20$D_ENDMM,"-",fl_20$D_ENDDD), .after = "Date")
  write.csv(fl_20, file = "data/IPE_2020.csv", quote = FALSE, row.names = FALSE)
  fl_20 <- select(fl_20, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  

#Cleaning IUE ---- 
  
  #IUE ----
  #TODO's before merging: 
  # (1) 2012, 2013, 2015 has separate AMTVA column; add onto AMTOTH
  # (2) 2012, 2013 rename AMTHMOM --> AMTMADV
  # (2) remove unwanted columns and readjust colnames
  
  # 2012
  table(fl_12$EVBEGYY)
  fl_12$EVBEGYY <- ifelse(fl_12$EVBEGYY == "11", "11", ifelse(fl_12$EVBEGYY == "12", "12", "") )
  fl_12$EVBEGMM <- ifelse(fl_12$EVBEGMM < 0, "", paste0(fl_12$EVBEGMM))
  fl_12$EVBEGDD <- ifelse(fl_12$EVBEGDD < 0, "", paste0(fl_12$EVBEGDD)) 
  fl_12$EVENDYY <- ifelse(fl_12$EVENDYY < 0, "", fl_12$EVENDYY)
  fl_12$EVENDMM <- ifelse(fl_12$EVENDMM < 0, "", paste0(fl_12$EVENDMM))
  fl_12$EVENDDD <- ifelse(fl_12$EVENDDD < 0, "", paste0(fl_12$EVENDDD)) 
  fl_12 <- add_column(fl_12, Date = paste0("20", fl_12$EVBEGYY,"-",fl_12$EVBEGMM,"-",fl_12$EVBEGDD), .after = "SURVEYYR")
  fl_12 <- add_column(fl_12, Date_End = paste0("20", fl_12$EVENDYY,"-",fl_12$EVENDMM,"-",fl_12$EVENDDD), .after = "Date")
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  fl_12$AMTOTH <- fl_12$AMTOTH2
  # fl_12$BASEID2 <- substr(fl_12$BASEID,2,str_length(fl_12$BASEID))
  # fl_12$BASEID <- fl_12$BASEID2
  fl_12 <- select(fl_12, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA))
  colnames(fl_12)
  colnames(fl_12)[14] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  # 2013
  table(fl_13$EVBEGYY)
  fl_13$EVBEGYY <- ifelse(fl_13$EVBEGYY == "12", "12", ifelse(fl_13$EVBEGYY == "13", "13", "") )
  fl_13$EVBEGMM <- ifelse(fl_13$EVBEGMM < 0, "", paste0(fl_13$EVBEGMM))
  fl_13$EVBEGDD <- ifelse(fl_13$EVBEGDD < 0, "", paste0(fl_13$EVBEGDD)) 
  fl_13$EVENDYY <- ifelse(fl_13$EVENDYY < 0, "", fl_13$EVENDYY)
  fl_13$EVENDMM <- ifelse(fl_13$EVENDMM < 0, "", paste0(fl_13$EVENDMM))
  fl_13$EVENDDD <- ifelse(fl_13$EVENDDD < 0, "", paste0(fl_13$EVENDDD)) 
  fl_13 <- add_column(fl_13, Date = paste0("20", fl_13$EVBEGYY,"-",fl_13$EVBEGMM,"-",fl_13$EVBEGDD), .after = "SURVEYYR")
  fl_13 <- add_column(fl_13, Date_End = paste0("20", fl_13$EVENDYY,"-",fl_13$EVENDMM,"-",fl_13$EVENDDD), .after = "Date")
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  fl_13$AMTOTH <- fl_13$AMTOTH2
  # fl_13$BASEID2 <- substr(fl_13$BASEID,2,str_length(fl_13$BASEID))
  # fl_13$BASEID <- fl_13$BASEID2
  fl_13 <- select(fl_13, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA))
  colnames(fl_13)
  colnames(fl_13)[14] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  
  # 2015 
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  fl_15 <- add_column(fl_15, Date = paste0("20", fl_15$EVBEGYY,"-",fl_15$EVBEGMM,"-",fl_15$EVBEGDD), .after = "SURVEYYR")
  fl_15 <- add_column(fl_15, Date_End = paste0("20", fl_15$EVENDYY,"-",fl_15$EVENDMM,"-",fl_15$EVENDDD), .after = "Date")
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD, AMTOTH2, AMTVA))
  
  
  # 2016 
  fl_16 <- add_column(fl_16, Date = paste0("20", fl_16$EVBEGYY,"-",fl_16$EVBEGMM,"-",fl_16$EVBEGDD), .after = "SURVEYYR")
  fl_16 <- add_column(fl_16, Date_End = paste0("20", fl_16$EVENDYY,"-",fl_16$EVENDMM,"-",fl_16$EVENDDD), .after = "Date")
  fl_16 <- select(fl_16, -c(EVBEGYY, EVBEGMM, EVBEGDD, EVENDYY, EVENDMM, EVENDDD) )
  
  
  # 2017 
  fl_17 <- add_column(fl_17, Date = paste0("20", fl_17$D_BEGYY,"-",fl_17$D_BEGMM,"-",fl_17$D_BEGDD), .after = "SURVEYYR")
  fl_17 <- add_column(fl_17, Date_End = paste0("20", fl_17$D_ENDYY,"-",fl_17$D_ENDMM,"-",fl_17$D_ENDDD), .after = "Date")
  fl_17 <- select(fl_17, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  # 2018 
  fl_18 <- add_column(fl_18, Date = paste0("20", fl_18$D_BEGYY,"-",fl_18$D_BEGMM,"-",fl_18$D_BEGDD), .after = "SURVEYYR")
  fl_18 <- add_column(fl_18, Date_End = paste0("20", fl_18$D_ENDYY,"-",fl_18$D_ENDMM,"-",fl_18$D_ENDDD), .after = "Date")
  fl_18 <- select(fl_18, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  
  # 2019 
  fl_19 <- add_column(fl_19, Date = paste0("20", fl_19$D_BEGYY,"-",fl_19$D_BEGMM,"-",fl_19$D_BEGDD), .after = "SURVEYYR")
  fl_19 <- add_column(fl_19, Date_End = paste0("20", fl_19$D_ENDYY,"-",fl_19$D_ENDMM,"-",fl_19$D_ENDDD), .after = "Date")
  fl_19 <- select(fl_19, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  # 2020 
  fl_20 <- add_column(fl_20, Date = paste0("20", fl_20$D_BEGYY,"-",fl_20$D_BEGMM,"-",fl_20$D_BEGDD), .after = "SURVEYYR")
  fl_20 <- add_column(fl_20, Date_End = paste0("20", fl_20$D_ENDYY,"-",fl_20$D_ENDMM,"-",fl_20$D_ENDDD), .after = "Date")
  fl_20 <- select(fl_20, -c(D_BEGYY, D_BEGMM, D_BEGDD, D_ENDYY, D_ENDMM, D_ENDDD) )
  
  
  

#Cleaning MPE ---- 
  
  #MPE ----
  #TODO's before merging: 
  # (1) 2015 has separate AMTVA column; add onto AMTOTH
  # (2) remove unwanted columns and readjust colnames

  
  # 2012
  table(fl_12$EVBEGYY)
  fl_12$EVBEGYY <- ifelse(fl_12$EVBEGYY == "11", "11", ifelse(fl_12$EVBEGYY == "12", "12", "") )
  fl_12$EVBEGMM <- ifelse(fl_12$EVBEGMM < 0, "", paste0(fl_12$EVBEGMM))
  fl_12$EVBEGDD <- ifelse(fl_12$EVBEGDD < 0, "", paste0(fl_12$EVBEGDD)) 
  fl_12 <- add_column(fl_12, Date = paste0("20", fl_12$EVBEGYY,"-",fl_12$EVBEGMM,"-",fl_12$EVBEGDD), .after = "SURVEYYR")
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  fl_12$AMTOTH <- fl_12$AMTOTH2
  # fl_12$BASEID2 <- substr(fl_12$BASEID,2,str_length(fl_12$BASEID))
  # fl_12$BASEID <- fl_12$BASEID2
  fl_12 <- select(fl_12, -c(EVBEGYY, EVBEGMM, EVBEGDD, AMTOTH2, AMTVA))
  colnames(fl_12)
  colnames(fl_12)[15] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  # 2013
  table(fl_13$EVBEGYY)
  fl_13$EVBEGYY <- ifelse(fl_13$EVBEGYY == "12", "12", ifelse(fl_13$EVBEGYY == "13", "13", "") )
  fl_13$EVBEGMM <- ifelse(fl_13$EVBEGMM < 0, "", paste0(fl_13$EVBEGMM))
  fl_13$EVBEGDD <- ifelse(fl_13$EVBEGDD < 0, "", paste0(fl_13$EVBEGDD)) 
  fl_13 <- add_column(fl_13, Date = paste0("20", fl_13$EVBEGYY,"-",fl_13$EVBEGMM,"-",fl_13$EVBEGDD), .after = "SURVEYYR")
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  fl_13$AMTOTH <- fl_13$AMTOTH2
  # fl_13$BASEID2 <- substr(fl_13$BASEID,2,str_length(fl_13$BASEID))
  # fl_13$BASEID <- fl_13$BASEID2
  fl_13 <- select(fl_13, -c(EVBEGYY, EVBEGMM, EVBEGDD, AMTOTH2, AMTVA))
  colnames(fl_13)
  colnames(fl_13)[15] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  # 2015 
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  fl_15 <- add_column(fl_15, Date = paste0("20", fl_15$EVBEGYY,"-",fl_15$EVBEGMM,"-",fl_15$EVBEGDD), .after = "SURVEYYR")
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(EVBEGYY, EVBEGMM, EVBEGDD, AMTOTH2, AMTVA))
  
  
  # 2016 
  fl_16 <- add_column(fl_16, Date = paste0("20", fl_16$EVBEGYY,"-",fl_16$EVBEGMM,"-",fl_16$EVBEGDD), .after = "SURVEYYR")
  write.csv(fl_16, file = "data/MPE_2016.csv", quote = FALSE, row.names = FALSE)
  fl_16 <- select(fl_16, -c(EVBEGYY, EVBEGMM, EVBEGDD) )
  
  
  # 2017 
  fl_17 <- add_column(fl_17, Date = paste0("20", fl_17$D_BEGYY,"-",fl_17$D_BEGMM,"-",fl_17$D_BEGDD), .after = "SURVEYYR")
  write.csv(fl_17, file = "data/MPE_2017.csv", quote = FALSE, row.names = FALSE)
  fl_17 <- select(fl_17, -c(D_BEGYY, D_BEGMM, D_BEGDD) )
  
  
  # 2018 
  fl_18 <- add_column(fl_18, Date = paste0("20", fl_18$D_BEGYY,"-",fl_18$D_BEGMM,"-",fl_18$D_BEGDD), .after = "SURVEYYR")
  write.csv(fl_18, file = "data/MPE_2018.csv", quote = FALSE, row.names = FALSE)
  fl_18 <- select(fl_18, -c(D_BEGYY, D_BEGMM, D_BEGDD) )
  
  
  
  # 2019 
  fl_19 <- add_column(fl_19, Date = paste0("20", fl_19$D_BEGYY,"-",fl_19$D_BEGMM,"-",fl_19$D_BEGDD), .after = "SURVEYYR")
  write.csv(fl_19, file = "data/MPE_2019.csv", quote = FALSE, row.names = FALSE)
  fl_19 <- select(fl_19, -c(D_BEGYY, D_BEGMM, D_BEGDD) )
  

  # 2020 
  fl_20 <- add_column(fl_20, Date = paste0("20", fl_20$D_BEGYY,"-",fl_20$D_BEGMM,"-",fl_20$D_BEGDD), .after = "SURVEYYR")
  write.csv(fl_20, file = "data/MPE_2020.csv", quote = FALSE, row.names = FALSE)
  fl_20 <- select(fl_20, -c(D_BEGYY, D_BEGMM, D_BEGDD) )
  

#Cleaning FAE ---- 
  
  #FAE ----
  #TODO's before merging: 
  # (1) 2012, 2013, 2015 has separate AMTVA column; add onto AMTOTH
  # (2) remove unwanted columns and readjust colnames  
  
  #2012
  table(fl_12$REFBEGGDD)
  table(fl_12$REFENDDD)
  #summary(fl_12)
  fl_12 <- add_column(fl_12, Date = paste0("20", fl_12$REFBEGYY,"-",fl_12$REFBEGMM,"-",fl_12$REFBEGDD), .after = "SURVEYYR")
  fl_12 <- add_column(fl_12, Date_End = paste0("20", fl_12$REFENDYY,"-",fl_12$REFENDMM,"-",fl_12$REFENDDD), .after = "Date")
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  fl_12$AMTOTH <- fl_12$AMTOTH2
  # fl_12$BASEID2 <- substr(fl_12$BASEID,2,str_length(fl_12$BASEID))
  # fl_12$BASEID <- fl_12$BASEID2
  fl_12 <- select(fl_12, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD, AMTVA, AMTOTH2))
  colnames(fl_12)
  
  #2013
  table(fl_13$REFBEGGDD)
  table(fl_13$REFENDDD)
  #summary(fl_13)
  fl_13 <- add_column(fl_13, Date = paste0("20", fl_13$REFBEGYY,"-",fl_13$REFBEGMM,"-",fl_13$REFBEGDD), .after = "SURVEYYR")
  fl_13 <- add_column(fl_13, Date_End = paste0("20", fl_13$REFENDYY,"-",fl_13$REFENDMM,"-",fl_13$REFENDDD), .after = "Date")
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  fl_13$AMTOTH <- fl_13$AMTOTH2
  # fl_13$BASEID2 <- substr(fl_13$BASEID,2,str_length(fl_13$BASEID))
  # fl_13$BASEID <- fl_13$BASEID2
  fl_13 <- select(fl_13, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD, AMTVA, AMTOTH2))
  
  
  
  #2015
  table(fl_15$REFBEGGDD)
  table(fl_15$REFENDDD)
  #summary(fl_15)
  fl_15 <- add_column(fl_15, Date = paste0("20", fl_15$REFBEGYY,"-",fl_15$REFBEGMM,"-",fl_15$REFBEGDD), .after = "SURVEYYR")
  fl_15 <- add_column(fl_15, Date_End = paste0("20", fl_15$REFENDYY,"-",fl_15$REFENDMM,"-",fl_15$REFENDDD), .after = "Date")
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD, AMTVA, AMTOTH2))

  
  #2016
  table(fl_16$REFBEGGDD)
  table(fl_16$REFENDDD)
  #summary(fl_16)
  fl_16 <- add_column(fl_16, Date = paste0("20", fl_16$REFBEGYY,"-",fl_16$REFBEGMM,"-",fl_16$REFBEGDD), .after = "SURVEYYR")
  fl_16 <- add_column(fl_16, Date_End = paste0("20", fl_16$REFENDYY,"-",fl_16$REFENDMM,"-",fl_16$REFENDDD), .after = "Date")
  fl_16 <- select(fl_16, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD))
  
  
  #2017
  table(fl_17$REFBEGGDD)
  table(fl_17$REFENDDD)
  #summary(fl_17)
  fl_17 <- add_column(fl_17, Date = paste0("20", fl_17$REFBEGYY,"-",fl_17$REFBEGMM,"-",fl_17$REFBEGDD), .after = "SURVEYYR")
  fl_17 <- add_column(fl_17, Date_End = paste0("20", fl_17$REFENDYY,"-",fl_17$REFENDMM,"-",fl_17$REFENDDD), .after = "Date")
  fl_17 <- select(fl_17, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD))
  
  
  #2018
  table(fl_18$REFBEGGDD)
  table(fl_18$REFENDDD)
  #summary(fl_18)
  fl_18 <- add_column(fl_18, Date = paste0("20", fl_18$REFBEGYY,"-",fl_18$REFBEGMM,"-",fl_18$REFBEGDD), .after = "SURVEYYR")
  fl_18 <- add_column(fl_18, Date_End = paste0("20", fl_18$REFENDYY,"-",fl_18$REFENDMM,"-",fl_18$REFENDDD), .after = "Date")
  fl_18 <- select(fl_18, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD))
  colnames(fl_18)[10] <- "AMTCARE" # AMTUCARE --> AMTCARE
  
  #2019
  table(fl_19$REFBEGGDD)
  table(fl_19$REFENDDD)
  #summary(fl_19)
  fl_19 <- add_column(fl_19, Date = paste0("20", fl_19$REFBEGYY,"-",fl_19$REFBEGMM,"-",fl_19$REFBEGDD), .after = "SURVEYYR")
  fl_19 <- add_column(fl_19, Date_End = paste0("20", fl_19$REFENDYY,"-",fl_19$REFENDMM,"-",fl_19$REFENDDD), .after = "Date")
  fl_19 <- select(fl_19, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD))
  colnames(fl_19)[10] <- "AMTCARE" # AMTUCARE --> AMTCARE
  
  #2020
  table(fl_20$REFBEGGDD)
  table(fl_20$REFENDDD)
  #summary(fl_20)
  fl_20 <- add_column(fl_20, Date = paste0("20", fl_20$REFBEGYY,"-",fl_20$REFBEGMM,"-",fl_20$REFBEGDD), .after = "SURVEYYR")
  fl_20 <- add_column(fl_20, Date_End = paste0("20", fl_20$REFENDYY,"-",fl_20$REFENDMM,"-",fl_20$REFENDDD), .after = "Date")
  fl_20 <- select(fl_20, -c(REFBEGYY, REFBEGMM, REFBEGDD, REFENDYY, REFENDMM, REFENDDD))
  colnames(fl_20)[10] <- "AMTCARE" # AMTUCARE --> AMTCARE
  
  
#Cleaning PME ---- 
  
  #PME ----
  #TODO's before merging: 
  # (1) 2012, 2013, 2015 has separate AMTVA column; add onto AMTOTH
  # (2) change SERV_DT into proper date format using to_date_pme
  # (3) remove unwanted columns and readjust colnames  
  

  # 2012
  fl_12 <- add_column(fl_12, Date = mapply(to_date_pme,fl_12$SERV_DT, 2012), .after = "SURVEYYR")
  fl_12$AMTOTH2 <- fl_12$AMTVA + fl_12$AMTOTH
  fl_12$AMTOTH <- fl_12$AMTOTH2
  fl_12 <- select(fl_12, -c(SERV_DT, AMTVA, AMTOTH2) )
  colnames(fl_12)
  colnames(fl_12)[8] <- "AMTMADV" # AMTHMOM --> AMTMADV

  
  # 2013
  fl_13 <- add_column(fl_13, Date = mapply(to_date_pme,fl_13$SERV_DT, 2013), .after = "SURVEYYR")
  fl_13$AMTOTH2 <- fl_13$AMTVA + fl_13$AMTOTH
  fl_13$AMTOTH <- fl_13$AMTOTH2
  fl_13 <- select(fl_13, -c(SERV_DT, AMTVA, AMTOTH2) )
  colnames(fl_13)
  colnames(fl_13)[8] <- "AMTMADV" # AMTHMOM --> AMTMADV
  
  
  # 2015
  fl_15 <- add_column(fl_15, Date = mapply(to_date_pme,fl_15$SERV_DT, 2015), .after = "SURVEYYR")
  fl_15$AMTOTH2 <- fl_15$AMTVA + fl_15$AMTOTH
  fl_15$AMTOTH <- fl_15$AMTOTH2
  fl_15 <- select(fl_15, -c(SERV_DT, AMTVA, AMTOTH2) )
  
  
  # 2016
  fl_16 <- add_column(fl_16, Date = mapply(to_date_pme,fl_16$SERV_DT, 2016), .after = "SURVEYYR")
  fl_16 <- select(fl_16, -c(SERV_DT))
  

  # 2017
  fl_17 <- add_column(fl_17, Date = mapply(to_date_pme,fl_17$SERV_DT, 2017), .after = "SURVEYYR")
  fl_17 <- select(fl_17, -c(SERV_DT))

  
  # 2018
  fl_18 <- add_column(fl_18, Date = mapply(to_date_pme,fl_18$SERV_DT, 2018), .after = "SURVEYYR")
  fl_18 <- select(fl_18, -c(SERV_DT))

  
  # 2019
  fl_19 <- add_column(fl_19, Date = mapply(to_date_pme,fl_19$SERV_DT, 2019), .after = "SURVEYYR")
  fl_19 <- select(fl_19, -c(SERV_DT))

  
  # 2020
  fl_20 <- add_column(fl_20, Date = mapply(to_date_pme,fl_20$SERV_DT, 2020), .after = "SURVEYYR")
  fl_20 <- select(fl_20, -c(SERV_DT))

  
  
  
#Merging ----

  
  master_df <- data.frame()
  master_df <- rbind(fl_12, fl_13, fl_15,fl_16,fl_17,fl_18,fl_19, fl_20)
  nrow(master_df) == nrow(fl_12) + nrow(fl_13) + nrow(fl_15) + nrow(fl_16) + nrow(fl_17) + nrow(fl_18) + nrow(fl_19) + nrow(fl_20)
  summary(master_df)


  
#Organizing after Merge ---- 

  # () Split Date into components YY MM DD
  # () add BSID_SRVYR
  # () add BSID_DATE
  # () Standardize cost to 2020 aka add column AMT_CS2020
  
  master_df <- splt_date_to_components(master_df,"pme")
  master_df <- add_column(master_df, BSID_SRVYR = paste0(master_df$BASEID,"_",master_df$SURVEYYR), .after = "Date" )
  master_df <- add_column(master_df, ID_DATE = paste0(master_df$BASEID,"_",master_df$Date), .after = "BSID_SRVYR" )
  
  
  #Cost standardized to 2020 dollars; adjusting for inflation
      # Note: the following variables don't exist for FAE: AMTCOV, AMTNCOV, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTDISC
      # Note: variables don't exist for PME: AMTCOV, AMTNCOV,
  master_df<- add_column(master_df, AMTTOT_CS2020 = mapply(cost_infl_adj,master_df$AMTTOT,master_df$SURVEYYR,2020), .after = "AMTTOT")
  master_df<- add_column(master_df, AMTCARE_CS2020 = mapply(cost_infl_adj,master_df$AMTCARE,master_df$SURVEYYR,2020), .after = "AMTCARE")
  master_df<- add_column(master_df, AMTCAID_CS2020 = mapply(cost_infl_adj,master_df$AMTCAID,master_df$SURVEYYR,2020), .after = "AMTCAID")
  master_df<- add_column(master_df, AMTOOP_CS2020 = mapply(cost_infl_adj,master_df$AMTOOP,master_df$SURVEYYR,2020), .after = "AMTOOP")
  master_df<- add_column(master_df, AMTPRVU_CS2020 = mapply(cost_infl_adj,master_df$AMTPRVU,master_df$SURVEYYR,2020), .after = "AMTPRVU")
  master_df<- add_column(master_df, AMTOTH_CS2020 = mapply(cost_infl_adj,master_df$AMTOTH,master_df$SURVEYYR,2020), .after = "AMTOTH")
  master_df<- add_column(master_df, AMTMADV_CS2020 = mapply(cost_infl_adj,master_df$AMTMADV,master_df$SURVEYYR,2020), .after = "AMTMADV")
  master_df<- add_column(master_df, AMTHMOP_CS2020 = mapply(cost_infl_adj,master_df$AMTHMOP,master_df$SURVEYYR,2020), .after = "AMTHMOP")
  master_df<- add_column(master_df, AMTPRVE_CS2020 = mapply(cost_infl_adj,master_df$AMTPRVE,master_df$SURVEYYR,2020), .after = "AMTPRVE")
  master_df<- add_column(master_df, AMTPRVI_CS2020 = mapply(cost_infl_adj,master_df$AMTPRVI,master_df$SURVEYYR,2020), .after = "AMTPRVI")
  master_df<- add_column(master_df, AMTDISC_CS2020 = mapply(cost_infl_adj,master_df$AMTDISC,master_df$SURVEYYR,2020), .after = "AMTDISC")
  master_df<- add_column(master_df, AMTCOV_CS2020 = mapply(cost_infl_adj,master_df$AMTCOV,master_df$SURVEYYR,2020), .after = "AMTCOV")
  master_df<- add_column(master_df, AMTNCOV_CS2020 = mapply(cost_infl_adj,master_df$AMTNCOV,master_df$SURVEYYR,2020), .after = "AMTNCOV")
  
   
#Save file ---- 
  
  #OPE
  write.csv(master_df,file = "output/OPE_AllYears.csv", quote = FALSE, row.names = FALSE)
  
  #IPE
  write.csv(master_df,file = "output/IPE_AllYears.csv", quote = FALSE, row.names = FALSE)
  
  #IUE
  write.csv(master_df,file = "output/IUE_AllYears.csv", quote = FALSE, row.names = FALSE)
  
  #MPE
  write.csv(master_df,file = "output/MPE_AllYears.csv", quote = FALSE, row.names = FALSE)

  #FAE
  write.csv(master_df,file = "output/FAE_AllYears.csv", quote = FALSE, row.names = FALSE)
  
  #PME
  write.csv(master_df,file = "output/PME_AllYears.csv", row.names = FALSE) #intentionally removed the quote = FALSE
  
  
#Functions ---- 

fltr_data <- function(year, index) {
  #extract variables unique to the index/file segment
  
  vars <- (subset(var_fl, var_fl$file_segment == seg[[index]]))$variable_name  
  vars[[(length(vars) + 1)]] <- "BASEID"
  vars[[(length(vars) + 1)]] <- "SURVEYYR"
  
  ## change date variables for ipe,iue, mpe, and ope
  if (year < 2017) {
    print("Make column name adjustments of date_vars before merging later on")
    if (seg[[index]] == "ope" ||
        seg[[index]] == "mpe" ||
        seg[[index]] == "iue" || seg[[index]] == "ipe") {
      vars[vars == "D_BEGYY"] <- "EVBEGYY"
      vars[vars == "D_BEGMM"] <- "EVBEGMM"
      vars[vars == "D_BEGDD"] <- "EVBEGDD"
      
      #these end dates only exist in ipe & iue
      vars[vars == "D_ENDYY"] <- "EVENDYY"
      vars[vars == "D_ENDMM"] <- "EVENDMM"
      vars[vars == "D_ENDDD"] <- "EVENDDD"
    } 
    
    if (year < 2015){
      vars[vars == "AMTMADV"] <- "AMTHMOM"
    }

  }
  
  if (year > 2017 & seg[[index]] == "fae"){
    vars[vars == "AMTCARE"] <- "AMTUCARE"
  }
  
  
  
  
  ## read in data and clean up
  data <- as.data.frame(read.csv(paste0(raw_files_dir,year, "/", seg[[index]], ".csv"))) 
  data <- data %>% select(any_of(vars)) #get rid of unwanted variables
  
  
  if (year < 2015){
    
    data$SURVEYYR <- year
  }

  
  data <- data %>% relocate(c("BASEID", "SURVEYYR"))
  
  
  #if true (good) --> all variables needed are included; 
  #if false, follow up accordingly before merging
  var_chk <- ncol(data) == length(vars) 
  print(var_chk)
  if (!var_chk) {
    miss_vars <- length(vars) - ncol(data)
    print(paste0(miss_vars,
                 " required variable(s) were NOT properly selected from: ",
                 seg[[index]] ," ", year) )
    
  slctd_vars <- colnames(data)
  msing_vars <- subset(vars,!(vars %in% slctd_vars))
  print(paste0("Unselected variables for ", year,": ", toString(msing_vars)))
    
  }
  
  #creates a new var with baseID_claimID
  #data$BC_ID <-paste0(data$BASEID, "_", data$CLAIMID) 
  
  
  return(data)
}



#change string to date for yrs 2012 and 2013
to_date_1213 <- function(date_str, file_seg) {
  
  if (is.na(date_str)) {
      date <- "NA"
    }
  
  else{

      yy <- substr(date_str,1,2)
      mm <- substr(date_str,3,4)
      dd <- substr(date_str,5,6)
      
      date <- paste0(yy,"-",mm,"-",dd)
      #date <- ymd(date) #doesn't work for some reason
      
    }
    
    return(date)
    
  }
  
to_date <- function(date_str) {
  
  if (is.na(date_str)) {
    date <- "NA"
  }
  
  else {
    #num-of_digit is either 7/8  
    #if the dd is one digit, it will always have a leading zero
    #if the mm is one digit, it will never have a leading zero
    
    if (str_length(date_str) == 7) {
      mm <- substr(date_str,1,1)
      dd <- substr(date_str,2,3)
      yy <- substr(date_str,4,7) 
    }
    
    
    else { #digits == 8
      mm <- substr(date_str,1,2)
      dd <- substr(date_str,3,4)
      yy <- substr(date_str,5,8)
    }
    
    date <- paste0(yy,"-",mm,"-",dd)
    #date <- ymd(date) #doesn't work for some reason

  }
  
  return(date)
  
}



to_date_pme <- function(date_str, year){
  
  if (is.na(date_str)) {
    date <- "NA"
  }
  
  else{
    
    if (year <= 2015){
      yy <- substr(date_str,1,4)
      mm <- substr(date_str,5,6)
      dd <- substr(date_str,7,8)
      
      date <- paste0(yy,"-",mm,"-",dd)
    }
    
    if (year > 2015){ #could also been else;
      
      if (str_length(date_str) == 7) {
        mm <- substr(date_str,1,1)
        dd <- substr(date_str,2,3)
        yy <- substr(date_str,4,7) 
      }
      
      
      else { #digits == 8
        mm <- substr(date_str,1,2)
        dd <- substr(date_str,3,4)
        yy <- substr(date_str,5,8)
      }
      
      date <- paste0(yy,"-",mm,"-",dd)
      
    }
    
    
    
  }
  
  return(date)
  
}

splt_date_to_components <- function(dataframe, file_seg){

  if (file_seg == "ope" | file_seg == "mpe" | file_seg == "pme"){
    dataframe$Year_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,1]
    dataframe$Month_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,2]
    dataframe$Day_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,3]
    
    dataframe <- dataframe %>% relocate(c(Year_DT,Month_DT,Day_DT), .after = Date)
  }
  
  else{
    
    dataframe$Year_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,1]
    dataframe$Month_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,2]
    dataframe$Day_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,3]
    
    dataframe <- dataframe %>% relocate(c(Year_DT,Month_DT,Day_DT), .after = Date)
    
    
    dataframe$YearEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,1]
    dataframe$MonthEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,2]
    dataframe$DayEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,3]
    
    dataframe <- dataframe %>% relocate(c(YearEnd_DT,MonthEnd_DT,DayEnd_DT), .after = Day_DT)
    
    
  }
    
  return(dataframe)
  
}

#requires cpi data file to be read in
cost_infl_adj <- function(pst_dlr_amnt, pst_yr, rcnt_yr){
  pst_yr_cpi <- cpi[paste0(pst_yr),"Annual_Avg"]
  rcnt_yr_cpi <- cpi[paste0(rcnt_yr),"Annual_Avg"]
  
  pstDlrs_in_rcntDlrs <- pst_dlr_amnt * (rcnt_yr_cpi / pst_yr_cpi)
  pstDlrs_in_rcntDlrs <- round2(pstDlrs_in_rcntDlrs,2)
  return(pstDlrs_in_rcntDlrs)
}



lst_duplicates <- function(df, indx_var_of_intrst){
  
  n_occur <- data.frame(table(df[,indx_var_of_intrst]))
  #n_occur[n_occur$Freq > 1,]
  dups <- df[df[,indx_var_of_intrst] %in% n_occur$Var1[n_occur$Freq > 1],]
  return(dups)
}




#scratch ---- 



# Exploring FAE data

fae <- master_df
table(fae$FACDESC)

length(unique(fae$BSID_SRVYR)) == nrow(fae) # FALSE; --> duplicate BASEIDs present
length(unique(fae$BSID_SRVYR))
fae_dups <- lst_duplicates(fae,4) # var of interest = BSID_SRVYR


ope <- as.data.frame(read.csv("output/OPE_AllYears.csv"))
ipe <- as.data.frame(read.csv("output/IPE_AllYears.csv"))

ipe_fae <- subset(ipe,ipe$BSID_SRVYR %in% fae_dups$BSID_SRVYR)
ope_fae <- subset(ope,ope$BSID_SRVYR %in% fae_dups$BSID_SRVYR)





w$EVBEGYY <- ifelse(w$EVBEGYY == "11", "11", ifelse(w$EVBEGYY == "12", "12", "") )
w$EVBEGMM <- ifelse(w$EVBEGMM < 0, "", paste0(w$EVBEGMM))
w$EVBEGDD <- ifelse(w$EVBEGDD < 0, "", paste0(w$EVBEGDD)) 
  
  
  w %>% mutate(EVBEGDD = case_when(
     
  #        )


