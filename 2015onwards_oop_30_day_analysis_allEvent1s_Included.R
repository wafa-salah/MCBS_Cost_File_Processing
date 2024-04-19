#                                               #
# Title:  30_day_Analysis_Split                 #
# Author: Wafa Salah                            #
# Date:   02/02/2023                            #
# Last Modified: 08/19/2023                     #
#                                               #
# # # # # # # # # # # # # # # # # # # # # # # # #


library(tidyverse)
#library(CRbasics)
library(stringr)
library(lubridate)

#CRprojectBuilder()  #run the very first time only

## Basic Outline for ED_homebase:
  # (1) Select cost__ope_files only; select 2015 - 2020 only
  # (2) Merge with ope to get the cost variables

getwd()

# Import -----

ED_dir <- "C:/Users/ws486/Documents/MCBS/Data/Cleaning/cost_&_claims/output/" #"C:/Users/ws486/Documents/MCBS/Data/Cleaning/both_final/data/" 
cost_dir <- "C:/Users/ws486/Documents/MCBS/Data/Cleaning/cost/output/"

#op_ED <- as.data.frame(read.csv(paste0(ED_dir,"homebase_wAccess_v1.csv"))) # n = 56,153 
op_ED <- as.data.frame(read.csv(paste0(ED_dir,"homebase_wAccess_wNagidis.csv"))) # n = 56,153 #modified on 5/31/2023

ope <- as.data.frame(read.csv(paste0(cost_dir,"OPE_AllYears.csv")))
mpe <- as.data.frame(read.csv(paste0(cost_dir,"MPE_AllYears.csv")))
ipe <- as.data.frame(read.csv(paste0(cost_dir,"IPE_AllYears.csv")))
iue <- as.data.frame(read.csv(paste0(cost_dir,"IUE_AllYears.csv")))
pme <- as.data.frame(read.csv(paste0(cost_dir,"PME_AllYears.csv")))
fae <- as.data.frame(read.csv(paste0(cost_dir,"FAE_AllYears.csv")))

# Cleaning ----

  #OP_ED ---- 

op_ED <- subset(op_ED, op_ED$SourceFile == "Cost_OPE") # n = 12,669
op_ED <- subset(op_ED, op_ED$SURVEYYR > 2013) # n = 9,173
op_ED <- select(op_ED, c(BASEID, SURVEYYR, BSID_SRVYR, ID_DATE, Date, Date_End, ID_DateEnd, Incomplete_Date, SOURCE, CLAIMID, INT_TYPE, H_MAFF, SourceFile))
op_ED <- splt_date_to_components(op_ED)
table(op_ED$Day_DT) # D, R, "", two-digit-day 
table(op_ED$Month_DT) # D, R, "", one-digit-month, two-digit-month --> fix all to two-digit-month
table(op_ED$MonthEnd_DT)

#ensure all known months are in two-digit format
op <- op_ED %>% mutate(Month_DT2 = case_when(
  Month_DT == "D" | Month_DT == "R" ~ Month_DT,
  str_length(Month_DT) == 1 ~ paste0("0",Month_DT),
  T ~ Month_DT )
  )

op_ED$Month_DT <- op$Month_DT2 # must now change, Date,Date_End, ID_Date, and all other variables that use this variable

rm(op)

table(op_ED$Month_DT)

#remove unknown months --> n = 8,978
op_ED <- op_ED[!(op_ED$Month_DT=="D" | op_ED$Month_DT=="R" | op_ED$Month_DT == ""),] 

#fix unknown days
table(op_ED$Day_DT) #  1,480 obs with unknown day for date
op_ED$Day_computed <- mapply(fix_unknown_days, op_ED$BASEID, op_ED$Month_DT, op_ED$Day_DT)
table(op_ED$Day_computed)

#remove same day events of same patient
op_ED$Date_computed <- paste0(op_ED$Year_DT,"-",op_ED$Month_DT,"-",op_ED$Day_computed)
op_ED <- add_column(op_ED, ID_DATE_computed = paste0(op_ED$BASEID,"_",op_ED$Date_computed), .before = "ID_DATE")

n_occur <- data.frame(table(op_ED$ID_DATE_computed))
sameDayEvents <- n_occur[n_occur$Freq > 1,] #identify which events are same day events for a given patient 
colnames(sameDayEvents)[1] <- "ID_DATE_computed"

#remove same day events n = 8,609
op_ED <- op_ED[!(op_ED$ID_DATE_computed %in% sameDayEvents$ID_DATE_computed),] 
rm(n_occur,sameDayEvents)
length(unique(op_ED$ID_DATE_computed)) == nrow(op_ED) #must now return TRUE



#OPE merging to get cost data
#cleaning the opeD
opeD <- subset(ope, ope$OREVTYPE == "ER")
table(opeD$Day_DT)
table(opeD$Month_DT)

op <- opeD %>% mutate(Day_DT2 = case_when(
  Day_DT == "D" | Day_DT == "R" ~ Day_DT,
  str_length(Day_DT) == 1 ~ paste0("0",Day_DT),
  T ~ Day_DT )
)

opeD$Day_DT <- op$Day_DT2

op <- opeD %>% mutate(Month_DT2 = case_when(
  Month_DT == "D" | Month_DT == "R" ~ Month_DT,
  str_length(Month_DT) == 1 ~ paste0("0",Month_DT),
  T ~ Month_DT )
)


opeD$Month_DT <- op$Month_DT2
rm(op)

opeD <- opeD %>% mutate(Incomplete_Date = case_when(
  Day_DT %in% c("R","D","N") | Day_DT == "" ~ 1, #if
  Month_DT %in% c("N","D") ~ 1, #else if
  T ~ 0   # finally (everything else)
))

table(opeD$Incomplete_Date)

head(opeD)
costED_full_dates <- opeD %>% filter(Incomplete_Date == 0) %>% 
  mutate(Date = paste0(ymd(Date))) %>% #mutate(Date_End = paste0(ymd(Date_End))) %>%  #must standardize date-formatting
  mutate(ID_DATE = paste0(BASEID,"_",Date)) 

opeD1 <- opeD[opeD$Incomplete_Date == 1,]
opeD <- rbind(costED_full_dates, opeD1) # bring in the standardized date records for the complete dates

op_ovlp <- subset(op_ED, op_ED$ID_DATE %in% opeD$ID_DATE) 
nrow(op_ovlp) == nrow(op_ED) #must return True
length(unique(op_ovlp$ID_DATE)) == nrow(op_ovlp) #must return TRUE

opeD_cost <- subset(opeD, opeD$ID_DATE %in% op_ovlp$ID_DATE)

rm(costED_full_dates,opeD1,op_ovlp, opeD)


#bringing in cost data (opeD_cost) into homebase identified cost_OPE ED visits (op_ED)
opeD_cost <- select(opeD_cost, -c(BASEID,SURVEYYR,BSID_SRVYR,Date,Year_DT, Month_DT, Day_DT, Incomplete_Date, SOURCE, CLAIMID))
op_ED <- merge(op_ED,opeD_cost, by= "ID_DATE")
rm(opeD_cost)



#######################    op_ED:  ope ED visits with cost data. N = 8,609






# Cleaning Event2 data sets ---- 

ope <- subset(ope, ope$BASEID %in% op_ED$BASEID)
mpe <- subset(mpe, mpe$BASEID %in% op_ED$BASEID)
ipe <- subset(ipe, ipe$BASEID %in% op_ED$BASEID)
iue <- subset(iue, iue$BASEID %in% op_ED$BASEID)
fae <- subset(fae, fae$BASEID %in% op_ED$BASEID)
pme <- subset(pme, pme$BASEID %in% op_ED$BASEID)

  ## OPE
    # TODO's 
    #     (1) Add Date_End, YearEnd_DT, MonthEnd_DT, DayEnd_DT
    ope <- add_column(ope, "Date_End" = ope$Date, .after = "ID_DATE")
    ope <- add_column(ope, YearEnd_DT = ope$Year_DT, .after = "Day_DT")
    ope <- add_column(ope, MonthEnd_DT = ope$Month_DT, .after = "YearEnd_DT")
    ope <- add_column(ope, DayEnd_DT = ope$Day_DT, .after = "MonthEnd_DT")
    ope <- select(ope, -c(PRINDIAG, STATUS) )
    ope$FACDESC <- NA
    ope$File <- "OPE"
    ope <- ope[!(ope$OREVTYPE == "ER"),] #remove these obs as already included in treat and release dataset aka op_ED
    
    

  ## MPE
    # TODO's 
    #     (1) Add Date_End, YearEnd_DT, MonthEnd_DT, DayEnd_DT
    mpe <- add_column(mpe, "Date_End" = mpe$Date, .after = "ID_DATE")
    mpe <- add_column(mpe, YearEnd_DT = mpe$Year_DT, .after = "Day_DT")
    mpe <- add_column(mpe, MonthEnd_DT = mpe$Month_DT, .after = "YearEnd_DT")
    mpe <- add_column(mpe, DayEnd_DT = mpe$Day_DT, .after = "MonthEnd_DT")
    mpe <- select(mpe, -c(PAMTMED, EVNTTYPE, CLAIMTYP) )
    mpe$FACDESC <- NA
    mpe$File <- "MPE"
    
    
  ## IPE
    # TODO's 
    #     (1) remove unwanted columns
    ipe <- select(ipe, -c(PRINDIAG, STATUS, UTLZNDAY, DRG) )
    ipe$FACDESC <- NA
    ipe$File <- "IPE"
    
    
  ## IUE
    # TODO's 
    #     (1) remove unwanted columns
    iue <- select(iue, -c(PRINDIAG, STATUS, UTLZNDAY) )
    iue$FACDESC <- NA
    iue$File <- "IUE"

    
  ## FAE
    # TODO's
    #     (1) remove unwanted columns
    #     (2) Add Missing columns for merging purposes
    fae <- select(fae, -c(STAYDAYS, BEGSTAT, ENDSTAT, EMNUM) )
    fae[c("EVNTNUM","OREVTYPE", "CLAIMID","SOURCE", "SITCODE","AMTCOV", "AMTCOV_CS2020","AMTNCOV", "AMTNCOV_CS2020","AMTMADV", "AMTMADV_CS2020",
          "AMTHMOP", "AMTHMOP_CS2020","AMTPRVE", "AMTPRVE_CS2020","AMTPRVI", "AMTPRVI_CS2020","AMTDISC", "AMTDISC_CS2020")] <- NA
    fae$File <- "FAE"
    
    
  ## PME
    # TODO's
    #     (1) remove unwanted columns
    #     (2) Add Missing columns for merging purposes
    pme <- subset(pme,!is.na(pme$Date)) #remove records without dates
    pme <- add_column(pme, "Date_End" = pme$Date, .after = "ID_DATE")
    pme <- add_column(pme, YearEnd_DT = pme$Year_DT, .after = "Day_DT")
    pme <- add_column(pme, MonthEnd_DT = pme$Month_DT, .after = "YearEnd_DT")
    pme <- add_column(pme, DayEnd_DT = pme$Day_DT, .after = "MonthEnd_DT")
    pme <- select(pme, -c(TYPE, CORF, DRUGNAME, THERCC, DAYSUPP, PDEFLAG) )
    pme[c("EVNTNUM","OREVTYPE", "CLAIMID","SOURCE", "SITCODE","AMTCOV", "AMTCOV_CS2020","AMTNCOV", "AMTNCOV_CS2020", "FACDESC")] <- NA
    pme$File <- "PME"
    

    
# Merging and cleaning event2 dataset ---- 
    
#chk
colnames(ope) == colnames(ipe)
colnames(ope) == colnames(iue)
colnames(ope) == colnames(mpe)


event2s <- data.frame()
event2s <- rbind(ope, ipe, iue, mpe, fae, pme)
nrow(event2s) == nrow(ipe) + nrow(iue) + nrow(mpe) + nrow(ope) + nrow(fae) + nrow(pme) 


# change unknown days to 30th (cause all event2s) + fix formatting of the day_DT values    

tmp <- event2s %>% mutate(Day_DT2 = case_when(
  Month_DT == "2" & Day_DT == "D" ~ "28",
  Month_DT == "2" & Day_DT == "N" ~ "28",
  Month_DT == "2" & Day_DT == "R" ~ "28",
  Month_DT == "2" & Day_DT == "" ~ "28",
  Day_DT == "D" | Day_DT == "R" | Day_DT == "N" | Day_DT == "" ~ "30",
  str_length(Day_DT) == 1 ~ paste0("0",Day_DT),
  T ~ Day_DT )
)


tmp <- tmp %>% mutate(DayEnd_DT2 = case_when(
  MonthEnd_DT == "2" & DayEnd_DT == "D" ~ "28",
  MonthEnd_DT == "2" & DayEnd_DT == "N" ~ "28",
  MonthEnd_DT == "2" & DayEnd_DT == "R" ~ "28",
  MonthEnd_DT == "2" & DayEnd_DT == "" ~ "28",
  DayEnd_DT == "D" | DayEnd_DT == "R" | DayEnd_DT == "N" | DayEnd_DT == "" ~ "30",
  str_length(DayEnd_DT) == 1 ~ paste0("0",DayEnd_DT),
  T ~ DayEnd_DT )
)


tmp <- tmp %>% mutate(Month_DT2 = case_when(
  Month_DT == "D" | Month_DT == "R" | Month_DT == "N" ~ Month_DT,
  str_length(Month_DT) == 1 ~ paste0("0",Month_DT),
  T ~ Month_DT )
)

tmp <- tmp %>% mutate(MonthEnd_DT2 = case_when(
  MonthEnd_DT == "D" | MonthEnd_DT == "R" | MonthEnd_DT == "N" ~ MonthEnd_DT,
  str_length(MonthEnd_DT) == 1 ~ paste0("0",MonthEnd_DT),
  T ~ MonthEnd_DT )
)


event2s$Day_DT <- tmp$Day_DT2
event2s$DayEnd_DT <- tmp$DayEnd_DT2
event2s$Month_DT <- tmp$Month_DT2
event2s$MonthEnd_DT <- tmp$MonthEnd_DT2
rm(tmp)

table(event2s$Month_DT)
table(event2s$MonthEnd_DT)

#remove unknown months: n = 1,089,782 --> 1,065,078 
event2s <- event2s[!(event2s$Month_DT=="D" | event2s$Month_DT=="R" | event2s$Month_DT == "" | event2s$Month_DT == "N"),] 
event2s <- event2s[!(event2s$MonthEnd_DT=="D" | event2s$MonthEnd_DT=="R" | event2s$MonthEnd_DT == "" | event2s$MonthEnd_DT == "N"),]


event2s$Date_computed <- paste0(event2s$Year_DT,"-",event2s$Month_DT,"-",event2s$Day_DT)
event2s$DateEnd_computed <- paste0(event2s$YearEnd_DT,"-",event2s$MonthEnd_DT,"-",event2s$DayEnd_DT)





#### ------- Building Final Dataset: Identify Event1 - Event2, and then Merge ---------


#TODO's
#(1) categorize the op_ED visits into exclusively event1_ED and event2_ED
#(2) add event2_ED's into ev2 dataframe
#(3) left join event1_ED with new ev2 dataframe that now includes event2_ED
#(4) for end dates outside of the 30 day, add column for proportion of days within that 30 day window --> 1 for same day events
#(5) create new AMT vars that factor in the proportion calsulated in (4)
#(6) add proportion/total column as done in ED visit numbs
#(7) then (6)  %>% group_by(ID_DATE_computed) %>%  summarise(AMTTOT_CS2020.y)
#(8) divide by FF vs MA




#(1) categorize the op_ED visits into exclusively event1_ED and event2_ED

ED <- op_ED %>% select(BASEID, SURVEYYR, BSID_SRVYR, AMTTOT_CS2020, AMTMADV_CS2020, Date_computed, ID_DATE_computed)


n_occur <- data.frame(table(ED$BASEID))
non_unq_pnts <- n_occur[n_occur$Freq > 1,] #identify which patients (BASEIDs) have more that one event
colnames(non_unq_pnts)[1] <- "BASEID"
ED_mltpl_evnts <- ED[(ED$BASEID %in% non_unq_pnts$BASEID),] #remove same day events
ED_sngl_evnts <- ED[!(ED$BASEID %in% non_unq_pnts$BASEID),]
nrow(ED_sngl_evnts) + nrow(ED_mltpl_evnts) == nrow(ED) #must return true
rm(n_occur,non_unq_pnts)
ED_sngl_evnts$event_desig <- "event1" #since there is only one record of these patients with a visit (i.e. there's no way it can be an event2)


ED_mltpl_evnts <- ED_mltpl_evnts[order(ED_mltpl_evnts$SURVEYYR,ED_mltpl_evnts$BASEID,ED_mltpl_evnts$Date_computed),]
rownames(ED_mltpl_evnts) <- ED_mltpl_evnts$ID_DATE_computed
ED_mltpl_evnts$event_desig <- NA


# for-loop to identify the event1s and event2s in the ED_mltpl_evnts df
for (id in unique(ED_mltpl_evnts$BASEID)){
  
  # subset all the events for that BASEID
  id_evnts <- subset(ED_mltpl_evnts, ED_mltpl_evnts$BASEID == id)
  
  for (i in 1:nrow(id_evnts)){
    
    id_date <- id_evnts[i,"ID_DATE_computed"] # allows to get value from both main dataset (ED_mltpl_evnts) as well as subset (id_events)
    #print(paste0("working on... ", id_date))
    
    
    #if event i already designated as 1 or 2, move onto next item
    if (is.na(ED_mltpl_evnts[id_date,"event_desig"])){
      
      date_of_intrst <- id_evnts[i,"Date_computed"]
      ED_mltpl_evnts <- ev1_2_recur(ED_mltpl_evnts, id_evnts,i+1, date_of_intrst)
      
      if (is.na(ED_mltpl_evnts[id_date,"event_desig"])){ 
        ED_mltpl_evnts[id_date,"event_desig"] <- "event1"}
      #print(ED_mltpl_evnts)
      
    }
    
    
  }
  
}



ED_wEvntNmbr <- data.frame()
ED_wEvntNmbr <- rbind(ED_mltpl_evnts, ED_sngl_evnts)
nrow(ED_wEvntNmbr) == nrow(ED_mltpl_evnts) + nrow(ED_sngl_evnts)
ED_wEvntNmbr <- select(ED_wEvntNmbr, ID_DATE_computed, event_desig)
table(ED_wEvntNmbr$event_desig)
nrow(ED_wEvntNmbr) == nrow(op_ED) #must return TRUE, otherwise cannot merge

op_ED <- merge(op_ED,ED_wEvntNmbr, by = "ID_DATE_computed")

opED_ev1 <- subset(op_ED, op_ED$event_desig == "event1") #Exclusive event1s from ED-treat-release dataset ( n = 7,822)
opED_ev2 <- subset(op_ED, op_ED$event_desig == "event2") #Exclusive event2s from ED-treat-release dataset (n = 1069 )
rm(ED_wEvntNmbr)


#allows for potential manual checking of whether 
# #event categorization (exclusive event1 vs 2) occurred properly & as expected
# chk_evnts <- ED_mltpl_evnts %>% select(ID_DATE_computed, Date_computed) %>% 
#   group_by(ID_DATE_computed.x) %>% summarise(EVENT2 = paste(ID_DATE_computed.y, collapse = " | "))

#Checks
subset(opED_ev1, opED_ev1$ID_DATE_computed == "1428714_2015-1-19") #should return df with 0 observations
table(opED_ev2$ID_DATE_computed %in% opED_ev1$ID_DATE_computed) #should return all FALSE if event categorization occurred properly
nrow(opED_ev1) + nrow(opED_ev2) == nrow(op_ED) #must return true if event categorization occurred properly


# remove event1s occurring in December 2020 b/c incomplete case; 
w <- opED_ev1 
w1 <- subset(w, w$SURVEYYR == 2020 & w$Month_DT == '12')
w2 <- subset(w, w$SURVEYYR == 2015 & w$Month_DT == '01')
w3 <- rbind(w1,w2)
w <- subset(w, !(w$ID_DATE_computed %in% w3$ID_DATE_computed))
opED_ev1 <- w # n = 7,606
rm(w,w1,w2,w3)






################################################################################# 

#(2) add event2_ED's into ev2 dataframe


opED_ev2_tmerg <- opED_ev2 %>% select(BASEID,SURVEYYR,Date,BSID_SRVYR, ID_DATE, 
                                      EVNTNUM,OREVTYPE, CLAIMID, SOURCE, Year_DT,Month_DT,Day_DT, SITCODE, AMTTOT,AMTTOT_CS2020,AMTCOV, AMTCOV_CS2020,
                                      AMTNCOV, AMTNCOV_CS2020, AMTCARE, AMTCARE_CS2020, AMTMADV, AMTMADV_CS2020, AMTOOP, AMTOOP_CS2020, AMTCAID,
                                      AMTCAID_CS2020, AMTHMOP, AMTHMOP_CS2020, AMTPRVE, AMTPRVE_CS2020, AMTPRVI, AMTPRVI_CS2020,
                                      AMTPRVU, AMTPRVU_CS2020, AMTDISC, AMTDISC_CS2020, AMTOTH, AMTOTH_CS2020, Date_computed) # must add Date_End YearEnd_DT MonthEnd_DT DayEnd_DT File DateEnd_computed FACDESC



opED_ev2_tmerg <- add_column(opED_ev2_tmerg, Date_End = opED_ev2_tmerg$Date, .after = "ID_DATE")
opED_ev2_tmerg <- add_column(opED_ev2_tmerg, YearEnd_DT = opED_ev2_tmerg$Year_DT, .after = "Day_DT")
opED_ev2_tmerg <- add_column(opED_ev2_tmerg, MonthEnd_DT = opED_ev2_tmerg$Month_DT, .after = "YearEnd_DT")
opED_ev2_tmerg <- add_column(opED_ev2_tmerg, DayEnd_DT = opED_ev2_tmerg$Day_DT, .after = "MonthEnd_DT")
opED_ev2_tmerg <- add_column(opED_ev2_tmerg, "File" = "OPE_ED", .after = "AMTOTH_CS2020")
opED_ev2_tmerg <- add_column(opED_ev2_tmerg, DateEnd_computed = opED_ev2_tmerg$Date_computed, .after = "Date_computed")
opED_ev2_tmerg$FACDESC <- NA

evnt2_All <- rbind(event2s, opED_ev2_tmerg) # n = 1,066,147
nrow(evnt2_All) == nrow(event2s) + nrow(opED_ev2_tmerg) 
rm(opED_ev2_tmerg)

####################################################################

#(3) left join event1_ED with new ev2 dataframe that now includes event2_ED

ev1 <- opED_ev1 %>% select(ID_DATE_computed, BASEID, SURVEYYR, BSID_SRVYR, Date_computed, H_MAFF, AMTTOT_CS2020, AMTMADV_CS2020, 
                           AMTTOT_CS2020, AMTCOV_CS2020, AMTNCOV_CS2020, AMTCARE_CS2020, AMTMADV_CS2020, AMTOOP_CS2020, AMTCAID_CS2020,
                           AMTHMOP_CS2020, AMTPRVE_CS2020, AMTPRVI_CS2020, AMTPRVU_CS2020, AMTDISC_CS2020, AMTOTH_CS2020)

ev2 <- evnt2_All %>% select(BASEID, SURVEYYR, BSID_SRVYR, Date_computed, DateEnd_computed, File, OREVTYPE, FACDESC, AMTTOT_CS2020, AMTMADV_CS2020, 
                            AMTTOT_CS2020, AMTCOV_CS2020, AMTNCOV_CS2020, AMTCARE_CS2020, AMTMADV_CS2020, AMTOOP_CS2020, AMTCAID_CS2020,
                            AMTHMOP_CS2020, AMTPRVE_CS2020, AMTPRVI_CS2020, AMTPRVU_CS2020, AMTDISC_CS2020, AMTOTH_CS2020)
mgd_evnts <- ev1 %>% 
  left_join(ev2,by = "BASEID") %>% 
  mutate(Date_computed.x = ymd(Date_computed.x),Date_computed.y = ymd(Date_computed.y)) %>% 
  mutate(Date_computed.xplus30 = Date_computed.x+days(30)) %>% mutate(within30Days = ifelse(Date_computed.y<Date_computed.xplus30 & Date_computed.y>Date_computed.x,1,0))


# selects all event1s (duplicates possible) with a corresponding event2; n = 87,812
mgd_evnts_wEv2 <- subset(mgd_evnts,mgd_evnts$within30Days == 1) 

ev1_wEv2 <- unique(mgd_evnts_wEv2$ID_DATE_computed)
ev1_noEv2 <- subset(ev1,!(ev1$ID_DATE_computed %in% ev1_wEv2))
colnames(ev1_noEv2) <- colnames(mgd_evnts_wEv2)[1:19]



rm(ED,ED_mltpl_evnts, ED_sngl_evnts, id_evnts, ev1_wEv2)


#############################################################

#(4) for end dates outside of the 30 day, add column for proportion of days within that 30 day window --> 1 for same day events
# Takes a while to run

mgd_evnts_wEv2 <- add_column(mgd_evnts_wEv2, 
                            prpDaysin30Window = mapply(prption_evntdays_in_30dayWindow,
                                                       mgd_evnts_wEv2$Date_computed.y, 
                                                       mgd_evnts_wEv2$DateEnd_computed, 
                                                       mgd_evnts_wEv2$Date_computed.xplus30), 
                            .after= "DateEnd_computed")





###########################################################

#(5) create new AMT vars for ev2 (.y) that factor in the proportion calculated in (4)

mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTTOT_in30Window = round2(mgd_evnts_wEv2$AMTTOT_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window, 2), .after = "AMTTOT_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTCOV_in30Window = round2(mgd_evnts_wEv2$AMTCOV_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTCOV_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTNCOV_in30Window = round2(mgd_evnts_wEv2$AMTNCOV_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTNCOV_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTCARE_in30Window = round2(mgd_evnts_wEv2$AMTCARE_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTCARE_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTCAID_in30Window = round2(mgd_evnts_wEv2$AMTCAID_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTCAID_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTMADV_in30Window = round2(mgd_evnts_wEv2$AMTMADV_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTMADV_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTHMOP_in30Window = round2(mgd_evnts_wEv2$AMTHMOP_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTHMOP_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTPRVE_in30Window = round2(mgd_evnts_wEv2$AMTPRVE_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTPRVE_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTPRVI_in30Window = round2(mgd_evnts_wEv2$AMTPRVI_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTPRVI_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTOOP_in30Window = round2(mgd_evnts_wEv2$AMTOOP_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTOOP_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTPRVU_in30Window = round2(mgd_evnts_wEv2$AMTPRVU_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTPRVU_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTDISC_in30Window = round2(mgd_evnts_wEv2$AMTDISC_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTDISC_CS2020.y")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, AMTOTH_in30Window = round2(mgd_evnts_wEv2$AMTOTH_CS2020.y * mgd_evnts_wEv2$prpDaysin30Window,2), .after = "AMTOTH_CS2020.y")


#########################################################

#(6) add proportion/total column as done in ED visit numbs

mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCOV.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCOV_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTCOV_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTNCOV.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTNCOV_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTNCOV_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCARE.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCARE_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTCARE_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCAID.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCAID_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTCAID_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTMADV.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTMADV_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTMADV_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTHMOP.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTHMOP_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTHMOP_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVE.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVE_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTPRVE_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVI.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVI_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTPRVI_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTOOP.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTOOP_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTOOP_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVU.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVU_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTPRVU_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTDISC.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTDISC_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTDISC_CS2020.x")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTOTH.x = mapply(percent_of_totl,mgd_evnts_wEv2$AMTOTH_CS2020.x,mgd_evnts_wEv2$AMTTOT_CS2020.x), .after = "AMTOTH_CS2020.x")



ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTCOV.x = mapply(percent_of_totl,ev1_noEv2$AMTCOV_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTCOV_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTNCOV.x = mapply(percent_of_totl,ev1_noEv2$AMTNCOV_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTNCOV_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTCARE.x = mapply(percent_of_totl,ev1_noEv2$AMTCARE_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTCARE_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTCAID.x = mapply(percent_of_totl,ev1_noEv2$AMTCAID_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTCAID_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTMADV.x = mapply(percent_of_totl,ev1_noEv2$AMTMADV_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTMADV_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTHMOP.x = mapply(percent_of_totl,ev1_noEv2$AMTHMOP_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTHMOP_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTPRVE.x = mapply(percent_of_totl,ev1_noEv2$AMTPRVE_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTPRVE_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTPRVI.x = mapply(percent_of_totl,ev1_noEv2$AMTPRVI_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTPRVI_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTOOP.x = mapply(percent_of_totl,ev1_noEv2$AMTOOP_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTOOP_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTPRVU.x = mapply(percent_of_totl,ev1_noEv2$AMTPRVU_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTPRVU_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTDISC.x = mapply(percent_of_totl,ev1_noEv2$AMTDISC_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTDISC_CS2020.x")
ev1_noEv2<- add_column(ev1_noEv2, PROP2020AMTOTH.x = mapply(percent_of_totl,ev1_noEv2$AMTOTH_CS2020.x,ev1_noEv2$AMTTOT_CS2020.x), .after = "AMTOTH_CS2020.x")



mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCOV.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCOV_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTCOV_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTNCOV.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTNCOV_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTNCOV_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCARE.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCARE_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTCARE_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTCAID.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTCAID_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTCAID_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTMADV.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTMADV_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTMADV_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTHMOP.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTHMOP_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTHMOP_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVE.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVE_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTPRVE_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVI.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVI_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTPRVI_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTOOP.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTOOP_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTOOP_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTPRVU.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTPRVU_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTPRVU_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTDISC.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTDISC_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTDISC_in30Window")
mgd_evnts_wEv2<- add_column(mgd_evnts_wEv2, PROP2020AMTOTH.y = mapply(percent_of_totl,mgd_evnts_wEv2$AMTOTH_in30Window,mgd_evnts_wEv2$AMTTOT_in30Window), .after = "AMTOTH_in30Window")




    

# Analysis ----
ev1_noEv2[c(colnames(mgd_evnts_wEv2)[32:79])] <- NA
ev1_noEv2[c(colnames(ev1_noEv2)[40:77])] <- 0
ev1_noEv2$contains_event2 <- 0

mgd_evnts_wEv2$contains_event2 <- 1

fnl_mgd_evnts <- rbind(mgd_evnts_wEv2,ev1_noEv2)

df <- fnl_mgd_evnts # n = 66,603


#event1
df$AMT_OtherCS2020.x <- round2(rowSums(df[,c("AMTHMOP_CS2020.x", "AMTPRVE_CS2020.x",
                                                   "AMTPRVI_CS2020.x", "AMTPRVU_CS2020.x", "AMTDISC_CS2020.x",
                                                   "AMTOTH_CS2020.x")]),2)


df$PROP2020_Other.x <- round2(rowSums(df[,c("PROP2020AMTHMOP.x", "PROP2020AMTPRVE.x",
                                                  "PROP2020AMTPRVI.x", "PROP2020AMTPRVU.x", "PROP2020AMTDISC.x",
                                                  "PROP2020AMTOTH.x")]),2)

#event2s
df$AMT_Otherin30Window <- round2(rowSums(df[,c("AMTHMOP_in30Window", "AMTPRVE_in30Window",
                                                     "AMTPRVI_in30Window", "AMTPRVU_in30Window", "AMTDISC_in30Window",
                                                     "AMTOTH_in30Window")], na.rm = TRUE),2)

df$PROP2020_Other.y <- round2(rowSums(df[,c("PROP2020AMTHMOP.y", "PROP2020AMTPRVE.y",
                                                  "PROP2020AMTPRVI.y", "PROP2020AMTPRVU.y", "PROP2020AMTDISC.y",
                                                  "PROP2020AMTOTH.y")], na.rm = TRUE),2)


################## saving for regression dataset
# df15to20 <- subset(df, df$SURVEYYR.x > 2013)
# df15_20_grpd <- grping_evnt2s(df15to20)
# saveRDS(df15_20_grpd, "../30_day_manuscript/data/post30_groupedEV2_Regression_noHmbsInfo.rds")

################## saving for manuscript study popln characteristics analysis 
# saveRDS(df, "output/post30day_2015_2020_dataset2.rds")
# saveRDS(df, "30_day_manuscript/data/post30_OOP_masterdf2.rds")
# saveRDS(df, "output/post30_2015_2020_OOP_final.rds") #06/09/2023


df_FF <- subset(df, df$H_MAFF == "FF") #49,617
df_MA <- subset(df, df$H_MAFF == "MA") #16,986


#FF ----
table(df_FF$SURVEYYR.x)
table(df_FF$SURVEYYR.y)

mismatch_yrs <- subset(df_FF,df_FF$SURVEYYR.x != df_FF$SURVEYYR.y) # the event1 is in Dec and event2 happens in Jan the following year



FF_15to20 <- subset(df_FF, df_FF$SURVEYYR.x > 2013)
FF_2015 <- subset(df_FF, df_FF$SURVEYYR.x == 2015)
FF_2016 <- subset(df_FF, df_FF$SURVEYYR.x == 2016)
FF_2017 <- subset(df_FF, df_FF$SURVEYYR.x == 2017)
FF_2018 <- subset(df_FF, df_FF$SURVEYYR.x == 2018)
FF_2019 <- subset(df_FF, df_FF$SURVEYYR.x == 2019)
FF_2020 <- subset(df_FF, df_FF$SURVEYYR.x == 2020)


## the ED visits with no event2's have NOT been removed
####  NOTE: use this to fill in the table shells for the ED visits themselves and only the median of event2
fnl_FF_15to20 <- grping_evnt2s(FF_15to20)
fnl_FF_2015 <- grping_evnt2s(FF_2015)
fnl_FF_2016 <- grping_evnt2s(FF_2016)
fnl_FF_2017 <- grping_evnt2s(FF_2017)
fnl_FF_2018 <- grping_evnt2s(FF_2018)
fnl_FF_2019 <- grping_evnt2s(FF_2019)
fnl_FF_2020 <- grping_evnt2s(FF_2020)

## added 8/19
## To make table2 proportions add upto a 100%
## filter out the observations that have an AMTTOT of $0.0, b/c
## these didn't have total cost associated with their visit
## and so wouldn't have the split proportion (medicare, medicaid, oop, etc) either
fnl0_FF_2015 <- subset(fnl_FF_2015, fnl_FF_2015$AMTTOT_CS2020.x != 0.0)
fnl0_FF_2016 <- subset(fnl_FF_2016, fnl_FF_2016$AMTTOT_CS2020.x != 0.0)
fnl0_FF_2017 <- subset(fnl_FF_2017, fnl_FF_2017$AMTTOT_CS2020.x != 0.0)
fnl0_FF_2018 <- subset(fnl_FF_2018, fnl_FF_2018$AMTTOT_CS2020.x != 0.0)
fnl0_FF_2019 <- subset(fnl_FF_2019, fnl_FF_2019$AMTTOT_CS2020.x != 0.0)
fnl0_FF_2020 <- subset(fnl_FF_2020, fnl_FF_2020$AMTTOT_CS2020.x != 0.0)

##no_payor 
round2(((nrow(fnl_FF_2015) - nrow(fnl0_FF_2015)) / nrow(fnl_FF_2015)) *100, 2)
round2(((nrow(fnl_FF_2016) - nrow(fnl0_FF_2016)) / nrow(fnl_FF_2016)) *100, 2)
round2(((nrow(fnl_FF_2017) - nrow(fnl0_FF_2017)) / nrow(fnl_FF_2017)) *100, 2) 
round2(((nrow(fnl_FF_2018) - nrow(fnl0_FF_2018)) / nrow(fnl_FF_2018)) *100, 2) 
round2(((nrow(fnl_FF_2019) - nrow(fnl0_FF_2019)) / nrow(fnl_FF_2019)) *100, 2) 
round2(((nrow(fnl_FF_2020) - nrow(fnl0_FF_2020)) / nrow(fnl_FF_2020)) *100, 2) 

rm(fnl0_FF_2015,fnl0_FF_2016,fnl0_FF_2017,fnl0_FF_2018,fnl0_FF_2019,fnl0_FF_2020)

### v3 added on 8/19 to add no payor category (both non event & strictly no payor) in payor split list
## aligning docs --> v4 excel; v5 worddoc table 
fnl2_FF_2015 <- subset(fnl_FF_2015, fnl_FF_2015$sumAMTOT_ev2 != 0)
fnl2_FF_2016 <- subset(fnl_FF_2016, fnl_FF_2016$sumAMTOT_ev2 != 0)
fnl2_FF_2017 <- subset(fnl_FF_2017, fnl_FF_2017$sumAMTOT_ev2 != 0)
fnl2_FF_2018 <- subset(fnl_FF_2018, fnl_FF_2018$sumAMTOT_ev2 != 0)
fnl2_FF_2019 <- subset(fnl_FF_2019, fnl_FF_2019$sumAMTOT_ev2 != 0)
fnl2_FF_2020 <- subset(fnl_FF_2020, fnl_FF_2020$sumAMTOT_ev2 != 0)

##no_payor 
## ---> if needed to differentiate between non-event and strictly no payor
## first must change fnl2_FF_20XX to fnl_FF_20XX$sumAMTOT_ev2 == 0), then table(fnl2_FF_20XX$contains_event2)
round2(( (nrow(fnl_FF_2015) - nrow(fnl2_FF_2015)) / nrow(fnl_FF_2015) )  *100, 2)
round2(( (nrow(fnl_FF_2016) - nrow(fnl2_FF_2016)) / nrow(fnl_FF_2016) )  *100, 2)
round2(( (nrow(fnl_FF_2017) - nrow(fnl2_FF_2017)) / nrow(fnl_FF_2017) )  *100, 2)
round2(( (nrow(fnl_FF_2018) - nrow(fnl2_FF_2018)) / nrow(fnl_FF_2018) )  *100, 2)
round2(( (nrow(fnl_FF_2019) - nrow(fnl2_FF_2019)) / nrow(fnl_FF_2019) )  *100, 2)
round2(( (nrow(fnl_FF_2020) - nrow(fnl2_FF_2020)) / nrow(fnl_FF_2020) )  *100, 2)

rm(fnl2_FF_2015,fnl2_FF_2016,fnl2_FF_2017,fnl2_FF_2018,fnl2_FF_2019,fnl2_FF_2020)


sum(fnl_FF_2015$n) # find out number of ev2 events



#one of these two (or ideally a combination of both is the answer for the first row of event2 table in the document)
table(FF_2015$File)
table(FF_2015$OREVTYPE)
table(FF_2015$FACDESC)



#MA ----
table(df_MA$SURVEYYR.x)
table(df_MA$SURVEYYR.y)
mismatch_yrsMA <- subset(df_MA,df_MA$SURVEYYR.x != df_MA$SURVEYYR.y)


MA_15to20 <- subset(df_MA, df_MA$SURVEYYR.x > 2013)
MA_2015 <- subset(df_MA, df_MA$SURVEYYR.x == 2015)
MA_2016 <- subset(df_MA, df_MA$SURVEYYR.x == 2016)
MA_2017 <- subset(df_MA, df_MA$SURVEYYR.x == 2017)
MA_2018 <- subset(df_MA, df_MA$SURVEYYR.x == 2018)
MA_2019 <- subset(df_MA, df_MA$SURVEYYR.x == 2019)
MA_2020 <- subset(df_MA, df_MA$SURVEYYR.x == 2020)


## the ED visits with no event2's have NOT been removed
####  NOTE: use this to fill in the table shells for the ED visits themselves and only the median of event2
fnl_MA_15to20 <- grping_evnt2s(MA_15to20)
fnl_MA_2015 <- grping_evnt2s(MA_2015)
fnl_MA_2016 <- grping_evnt2s(MA_2016)
fnl_MA_2017 <- grping_evnt2s(MA_2017)
fnl_MA_2018 <- grping_evnt2s(MA_2018)
fnl_MA_2019 <- grping_evnt2s(MA_2019)
fnl_MA_2020 <- grping_evnt2s(MA_2020)

## added 8/19
## To make table2 proportions add upto a 100%
## filter out the observations that have an AMTTOT of $0.0, b/c
## these didn't have total cost associated with their visit
## and so wouldn't have the split proportion (medicare, medicaid, oop, etc) either
fnl0_MA_2015 <- subset(fnl_MA_2015, fnl_MA_2015$AMTTOT_CS2020.x != 0.0)
fnl0_MA_2016 <- subset(fnl_MA_2016, fnl_MA_2016$AMTTOT_CS2020.x != 0.0)
fnl0_MA_2017 <- subset(fnl_MA_2017, fnl_MA_2017$AMTTOT_CS2020.x != 0.0)
fnl0_MA_2018 <- subset(fnl_MA_2018, fnl_MA_2018$AMTTOT_CS2020.x != 0.0)
fnl0_MA_2019 <- subset(fnl_MA_2019, fnl_MA_2019$AMTTOT_CS2020.x != 0.0)
fnl0_MA_2020 <- subset(fnl_MA_2020, fnl_MA_2020$AMTTOT_CS2020.x != 0.0)

##no_payor 
round2(((nrow(fnl_MA_2015) - nrow(fnl0_MA_2015)) / nrow(fnl_MA_2015)) *100, 2)
round2(((nrow(fnl_MA_2016) - nrow(fnl0_MA_2016)) / nrow(fnl_MA_2016)) *100, 2)
round2(((nrow(fnl_MA_2017) - nrow(fnl0_MA_2017)) / nrow(fnl_MA_2017)) *100, 2) 
round2(((nrow(fnl_MA_2018) - nrow(fnl0_MA_2018)) / nrow(fnl_MA_2018)) *100, 2) 
round2(((nrow(fnl_MA_2019) - nrow(fnl0_MA_2019)) / nrow(fnl_MA_2019)) *100, 2) 
round2(((nrow(fnl_MA_2020) - nrow(fnl0_MA_2020)) / nrow(fnl_MA_2020)) *100, 2) 

rm(fnl0_MA_2015,fnl0_MA_2016,fnl0_MA_2017,fnl0_MA_2018,fnl0_MA_2019,fnl0_MA_2020)


### v3 adjusted on 8/19: docs --> v4 excel; v5 worddoc table
fnl2_MA_2015 <- subset(fnl_MA_2015, fnl_MA_2015$sumAMTOT_ev2 != 0)
fnl2_MA_2016 <- subset(fnl_MA_2016, fnl_MA_2016$sumAMTOT_ev2 != 0)
fnl2_MA_2017 <- subset(fnl_MA_2017, fnl_MA_2017$sumAMTOT_ev2 != 0)
fnl2_MA_2018 <- subset(fnl_MA_2018, fnl_MA_2018$sumAMTOT_ev2 != 0)
fnl2_MA_2019 <- subset(fnl_MA_2019, fnl_MA_2019$sumAMTOT_ev2 != 0)
fnl2_MA_2020 <- subset(fnl_MA_2020, fnl_MA_2020$sumAMTOT_ev2 != 0)

##no_payor 
## ---> if needed to differentiate between non-event and strictly no payor
## first must change fnl2_MA_20XX to fnl_MA_20XX$sumAMTOT_ev2 == 0), then table(fnl2_MA_20XX$contains_event2)
round2(( (nrow(fnl_MA_2015) - nrow(fnl2_MA_2015)) / nrow(fnl_MA_2015) )  *100, 2)
round2(( (nrow(fnl_MA_2016) - nrow(fnl2_MA_2016)) / nrow(fnl_MA_2016) )  *100, 2)
round2(( (nrow(fnl_MA_2017) - nrow(fnl2_MA_2017)) / nrow(fnl_MA_2017) )  *100, 2)
round2(( (nrow(fnl_MA_2018) - nrow(fnl2_MA_2018)) / nrow(fnl_MA_2018) )  *100, 2)
round2(( (nrow(fnl_MA_2019) - nrow(fnl2_MA_2019)) / nrow(fnl_MA_2019) )  *100, 2)
round2(( (nrow(fnl_MA_2020) - nrow(fnl2_MA_2020)) / nrow(fnl_MA_2020) )  *100, 2)

rm(fnl2_MA_2015,fnl2_MA_2016,fnl2_MA_2017,fnl2_MA_2018,fnl2_MA_2019,fnl2_MA_2020)


prnt_otpt(fnl_MA_2015)

#one of these two (or ideally a combination of both is the answer for the first row of event2 table in the document)
table(MA_2019$File)
table(MA_2016$OREVTYPE)

table(subset(MA_2015,MA_2015$File == "IUE")$OREVTYPE)
table(subset(MA_2015,MA_2015$File == "OPE")$OREVTYPE)
table(subset(MA_2015,MA_2015$File == "IPE")$OREVTYPE)
table(subset(MA_2015,MA_2015$File == "MPE")$OREVTYPE)



#fnl analysis data

fnl_df <- rbind(fnl_FF_2015,fnl_FF_2016, fnl_FF_2017,fnl_FF_2018,fnl_FF_2019,fnl_FF_2020,fnl_MA_2015,fnl_MA_2016, fnl_MA_2017,fnl_MA_2018,fnl_MA_2019,fnl_MA_2020)





# Saving ---- 

  write.csv(op_ED,file = "data/30_day_analysis_EDTreatnRelease_dataset.csv", quote = FALSE, row.names = FALSE)

  #30_day_analysis
  write.csv(fnl_mgd_evnts,file = "data/post_30_analysis_dataset1.csv", quote = FALSE, row.names = FALSE)

  #post 30 day 2015-2020
  write.csv(df,file = "30_day_manuscript/data/post30day_2015_2020_dataset.csv", row.names = FALSE)
  saveRDS(df, file = "30_day/output/post30day_2015_2020_dataset.rds")
  
  #pre/post 30 day 2015-2020
  write.csv(df,file = "30_day_manuscript/data/both_30day_2015_2020_dataset.csv", row.names = FALSE)
  saveRDS(df, file = "30_day_manuscript/output/both_30day_2015_2020_dataset.rds")
  

  
# Functions ----

#must have op_ED dataset in global envn to work
fix_unknown_days <- function(baseid,month,day){
  
  if( day == "D" | day == "R" | day == "") {
    
    op_ids <- subset(op_ED,op_ED$BASEID == baseid)
    preced_mnth <- paste0(as.numeric(month) - 1)
    vists_prior_mnth <- subset(op_ids, op_ids$Month_DT == preced_mnth)
    
    if (nrow(vists_prior_mnth) == 0){ # means there were no ED visits by the patient in the prior month
      new_day_val <- "01"
    }
    
    else{
      # chk if Feb vs all other months
      if(month == "2") { new_day_val <- "28" }
      else{ new_day_val <- "30"}
    }
  }
  
  else {
    new_day_val <- day
  }
  
  return(new_day_val)
}

#calculate the number of days of event2 that lies within the 30-day window
#calculates it in proportion to total days of visit
prption_evntdays_in_30dayWindow <- function(ev2BegDate, ev2EndDate, xplus30day){
  xplus30day <- ymd(xplus30day)
  ev2BegDate <- ymd(ev2BegDate)
  ev2EndDate <- ymd(ev2EndDate)
  
  if(ev2EndDate <= xplus30day) { 
    #event days are entirely in the 30 day window
    prption_of_days <- 1.00 
  }
  
  else {
    #event days NOT entirely in the 30 day window --> some of the days fall outside of 30 day window
    
    ##  -->  (xplus30days - ev2BegDate) / (ev2EndDate - ev2BegDate)
    prption_of_days <- as.numeric(difftime(xplus30day, ev2BegDate, units = "days")) / as.numeric(difftime(ev2EndDate, ev2BegDate, units = "days"))
    prption_of_days <- round2(prption_of_days,2)
  }
  
  return(prption_of_days)
  
}

#works only with the specific t30_after dataset and/or subsets
#the function groups all the event2s for each event1, then sums up each of the costs for the event2s, then returns the new_df
grping_evnt2s <- function(dataframe){
  
  df_x <- dataframe %>% select(ID_DATE_computed, SURVEYYR.x, BASEID, Date_computed.x, H_MAFF, contains_event2, AMTTOT_CS2020.x, 
                               AMTCOV_CS2020.x, PROP2020AMTCOV.x, AMTNCOV_CS2020.x, PROP2020AMTNCOV.x,
                               AMTCARE_CS2020.x, PROP2020AMTCARE.x, AMTMADV_CS2020.x, PROP2020AMTMADV.x,
                               AMTCAID_CS2020.x, PROP2020AMTCAID.x, AMTOOP_CS2020.x, PROP2020AMTOOP.x,  
                               AMT_OtherCS2020.x, PROP2020_Other.x) 

  
  df_x <- df_x[!duplicated(df_x), ]   
  
  
  df_y <- dataframe %>% group_by(ID_DATE_computed) %>% 
    summarise(sumAMTOT_ev2 = sum(AMTTOT_in30Window), 
              sumAMTCARE_ev2 = sum(AMTCARE_in30Window),
              sumAMTMADV_ev2 = sum(AMTMADV_in30Window),
              sumAMTCAID_ev2 = sum(AMTCAID_in30Window),
              sumAMTOOP_ev2 = sum(AMTOOP_in30Window),
              sumAMT_Other_ev2 = sum(AMT_Otherin30Window),
              n=n())
  
  
  new_df <- merge(df_x, df_y, by = "ID_DATE_computed")
  
  new_df<- add_column(new_df, PROPsumAMTCARE_ev2 = mapply(percent_of_totl,new_df$sumAMTCARE_ev2,new_df$sumAMTOT_ev2), .after = "sumAMTCARE_ev2")
  new_df<- add_column(new_df, PROPsumAMTMADV_ev2 = mapply(percent_of_totl,new_df$sumAMTMADV_ev2,new_df$sumAMTOT_ev2), .after = "sumAMTMADV_ev2")
  new_df<- add_column(new_df, PROPsumAMTCAID_ev2 = mapply(percent_of_totl,new_df$sumAMTCAID_ev2,new_df$sumAMTOT_ev2), .after = "sumAMTCAID_ev2")
  new_df<- add_column(new_df, PROPsumAMTOOP_ev2 = mapply(percent_of_totl,new_df$sumAMTOOP_ev2,new_df$sumAMTOT_ev2), .after = "sumAMTOOP_ev2")
  new_df<- add_column(new_df, PROPsumAMT_Other_ev2 = mapply(percent_of_totl,new_df$sumAMT_Other_ev2,new_df$sumAMTOT_ev2), .after = "sumAMT_Other_ev2")
  
  return(new_df)
  
}


percent_of_totl <- function(amt, total){
  
  if (is.na(total)){
    proportion <- NA
  }
  
  else if (total == 0){
    proportion <- 0
  }
  
  
  else{
    proportion <- (amt/ total) *100
    proportion <- round2(proportion,2) #round to 2 decimal digits
  }
  return(proportion)
  
}


splt_date_to_components <- function(dataframe, file_seg){

    
    dataframe$Year_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,1]
    dataframe$Month_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,2]
    dataframe$Day_DT <- str_split(dataframe$Date,"-",simplify = TRUE)[,3]
    
    dataframe <- dataframe %>% relocate(c(Year_DT,Month_DT,Day_DT), .after = Date)
    
    
    dataframe$YearEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,1]
    dataframe$MonthEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,2]
    dataframe$DayEnd_DT <- str_split(dataframe$Date_End,"-",simplify = TRUE)[,3]
    
    dataframe <- dataframe %>% relocate(c(YearEnd_DT,MonthEnd_DT,DayEnd_DT), .after = Date_End)
    
  return(dataframe)
  
}


ev1_2_recur <- function(main_df,subset_df,indx,date_of_evnt){
  
  ##base func 1
  # If there are no more observations to look through
  if(indx > nrow(subset_df)){ 
    return(main_df) 
  } 
  
  
  ##base func 2
  # If the event's date falls outside of the (date of interest -- i.e 'date_of_evnt' parameter)
  else if (ymd(subset_df[indx, "Date_computed"]) > (ymd(date_of_evnt) + days(30))) {
    return(main_df)
  } 
  
  else { #recursion
    
    id_date_rownm <- subset_df[indx, "ID_DATE_computed"]
    main_df[id_date_rownm,"event_desig"] <- "event2"
    return(ev1_2_recur(main_df,subset_df,indx+1,date_of_evnt))
  }
  
}

prnt_otpt <- function(df){
  
  df_xout <- select(df, ID_DATE_computed, AMTTOT_CS2020.x, PROP2020AMTCARE.x,
                    PROP2020AMTMADV.x, PROP2020AMTCAID.x, PROP2020AMTOOP.x, PROP2020_Other.x)
  
  cat(summary(df_xout)[3,2],"\n",
      summary(df_xout)[4,3],"\n",
      summary(df_xout)[4,4],"\n",
      summary(df_xout)[4,5],"\n",
      summary(df_xout)[4,6],"\n",
      summary(df_xout)[4,7],"\n")
  
  
  #print(summary(df_xout))
  
  print('----------------------------------------------------------')
  
  df_yout <- select(df, sumAMTOT_ev2, PROPsumAMTCARE_ev2, PROPsumAMTMADV_ev2, PROPsumAMTCAID_ev2,
                    PROPsumAMTOOP_ev2,  PROPsumAMT_Other_ev2)
  
  print(sum(df$n))
  
  
  print('-----------------------------------------------------------')
  
  cat(summary(df_yout)[3,1],"\n",
      summary(df_yout)[4,2],"\n",
      summary(df_yout)[4,3],"\n",
      summary(df_yout)[4,4],"\n",
      summary(df_yout)[4,5],"\n",
      summary(df_yout)[4,6])
  
  #print(summary(df_yout))

  
}

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}





# scratch ----- 


head(fnl_mgd_evnts)
HF <- subset(op_ED, op_ED$ID_DATE_computed %in% fnl_mgd_evnts$ID_DATE_computed)
HF <- select(HF, c(ID_DATE_computed, H_MAFF))
length(unique(fnl_mgd_evnts$ID_DATE_computed)) == nrow(HF)
t30_aftr <- merge(fnl_mgd_evnts, HF, by = "ID_DATE_computed")


fnl_mgd_evnts <- t30_aftr









