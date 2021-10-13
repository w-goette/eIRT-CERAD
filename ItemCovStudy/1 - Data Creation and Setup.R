#################################################
## DATA CREATION FROM HRS AND RAND DATA FILES
#################################################

#load in needed packages
library(asciiSetupReader) #needed to read in HRS data
library(haven)            #needed to read in RAND data
library(tidyverse)        #needed for data wrangling
library(reshape2)         #provides simpler wide-to-long transformation

########################
## CREATE DATA FOR STUDY
########################

#read respondent and informant files into R
df_resp <- read_ascii_setup(data = "Data Files/HC16HP_R.da", setup_file = "Data Files/HC16HP_R.sas", use_value_labels = TRUE, use_clean_names = TRUE)
df_info <- read_ascii_setup(data = "Data Files/HC16HP_I.da", setup_file = "Data Files/HC16HP_I.sas", use_value_labels = TRUE, use_clean_names = TRUE)

#get rid of cases with diagnoses
stroke <- which(df_info$INFORMANT_R_DIAGNOSED_WITH_STROKE == 1)
parkin <- which(df_info$INFORMANT_R_DIAGNOSED_WITH_PARKINSON_S == 1)
alzhie <- which(df_info$INFORMANT_R_DIAGNOSED_WITH_ALZHEIMER_S == 1)
memory <- which(df_info$INFORMANT_R_DIAGNOSED_WITH_MEM_PROBLEMS == 1)
impaired <- unique(c(stroke, parkin, alzhie, memory))

df_imp <- df_resp[impaired, ]
df_resp <- df_resp[-impaired, ]
df_info <- df_info[-impaired, ]

#get rid of cases with functional impairment related to Blessed
Blessed1 <- which(df_info[, 159] > 1 & df_info[, 160] == 2 | df_info[, 160] == 3)
Blessed2 <- which(df_info[, 161] > 1 & df_info[, 162] == 2 | df_info[, 162] == 3)
Blessed3 <- which(df_info[, 163] > 1 & df_info[, 164] == 2 | df_info[, 164] == 3)
Blessed4 <- which(df_info[, 165] > 1 & df_info[, 166] == 2 | df_info[, 166] == 3)
Blessed5 <- which(df_info[, 167] > 1 & df_info[, 168] == 2 | df_info[, 168] == 3)
Blessed6 <- which(df_info[, 169] > 1 & df_info[, 170] == 2 | df_info[, 170] == 3)
Blessed7 <- which(df_info[, 171] > 1 & df_info[, 172] == 2 | df_info[, 172] == 3)
Blessed8 <- which(df_info[, 173] > 1 & df_info[, 174] == 2 | df_info[, 174] == 3)
blessed <- unique(c(Blessed1, Blessed2, Blessed3, Blessed4, Blessed5, Blessed6, Blessed7, Blessed8))

df_blsd <- df_resp[blessed, ]
df_resp <- df_resp[-blessed, ]

#merge the data
df_resp$Impaired <- rep("No", nrow(df_resp))
df_imp$Impaired <- rep("Yes", nrow(df_imp))
df_blsd$Impaired <- rep("Yes", nrow(df_blsd))
df_resp <- rbind(df_resp, df_imp)
df_resp <- rbind(df_resp, df_blsd)
df_resp$Impaired <- as.factor(df_resp$Impaired)

#clean up data environment
rm(df_info, df_imp, df_blsd, alzhie, blessed, Blessed1, Blessed2, Blessed3, Blessed4, Blessed5, Blessed6, Blessed7, Blessed8, impaired, memory, parkin, stroke)

#get just the CERAD data
df_resp <- df_resp[which(df_resp$CERAD_WORD_LIST_IMMEDIATE_COMPLETION_STATUS == 1), ] #keep only cases who completed CERAD trials
df_resp <- df_resp[which(df_resp$IWER_CHECKPOINT_IW_LANGUAGE == 1), ] #keep only those who completed the interview in English
df_resp <- df_resp[, c(1:2, 51:60, 66:75, 81:90, 14, 49, 64, 79, 95, 125, 149, 176:177, 200, 213, 220, 242, 269, 327, 346, 355, 364, 395)]

#combine some variables
df_resp$CERADImm <- rowSums(df_resp[, 34:36])
df_resp$CERADDisc <- df_resp[, 40]-(10-df_resp[, 41])
df_resp[, c(34:36, 40:41)] <- NULL

#recode missing values
df_resp[, 37] <- ifelse(df_resp[, 37] == 97, NA, df_resp[, 37]) #Constructional Praxis - 97 = cannot draw
df_resp[, 39] <- ifelse(df_resp[, 39] == 97, NA, df_resp[, 39]) #Constructional Praxis - 91 = cannot draw
df_resp[, 43] <- ifelse(df_resp[, 43] == 997, 300, df_resp[, 43]) #TMTA - 997 = could not complete in 5 minutes
df_resp[, 44] <- ifelse(df_resp[, 44] == 997, 300, df_resp[, 44]) #TMTB - 997 = could not complete in 5 minutes

#get demographic variables from broader HRS data set
df_geogr  <- read_ascii_setup(data = "Data Files/HRSXREGION16.da", setup_file = "Data Files/HRSXREGION16.sas", use_value_labels = TRUE, use_clean_names = TRUE)

df_sesvb <- read_sas(data = "Data Files/randhrs1992_2016v2.sas7bdat")

#simplify the demographic variables to just those of interest
df_sesvb <- select(df_sesvb, c(1, contains("R13") | contains("H13") | starts_with("RA"))) #R13 = respondent answers in 2016, H13 = household answers in 2016, RA = cross-wave stable variables

df_geogr <- df_geogr[, c(1:2, 22, 96)]
df_sesvb <- df_sesvb[, c(1, 14:15, 30, 35, 38, 50:52, 62:67, 70,
                         73, 78, 83, 88, 93, 98, 103, 108, 
                         113, 118, 126:127, 215:216, 280:281, 283, 
                         430, 433, 435, 522:526, 539, 541, 550, 
                         553:554, 558:559, 570:572, 583:585, 
                         588:590, 597, 574)]

#recode the variables
df_geogr$CENSUS_REGION_DIVISION_WHERE_LIVE_WHEN_IN_SCHOOL <- factor(df_geogr$CENSUS_REGION_DIVISION_WHERE_LIVE_WHEN_IN_SCHOOL, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 98, 99), labels = c("NEr_NEd", "NEr_MAd", "MWr_ENd", "MWr_WNd", "Sr_SAd", "Sr_ESd", "Sr_WSd", "Wr_Md", "Wr_Pd", "US_na", "Foreign", "NA", "NA"))
df_geogr$HRS_URBAN_RURAL_CODE_2016_BEALE_2013 <- ordered(factor(df_geogr$HRS_URBAN_RURAL_CODE_2016_BEALE_2013, levels = c(1, 2, 3, 9), labels = c("Urban", "Suburban", "Exurban", "NA")))

df_sesvb$R13CENREG <- factor(df_sesvb$R13CENREG, levels = c(1, 2, 3, 4, 5), labels = c("NE", "MW", "S", "W", "Other"))
df_sesvb$R13CENDIV <- factor(df_sesvb$R13CENDIV, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11), labels = c("NE", "MA", "ENC", "WNC", "SA", "ESC", "WSC", "Mtn", "Pcf", "NotUS"))
df_sesvb$R13SHLT <- ordered(df_sesvb$R13SHLT, levels = c(5, 4, 3, 2, 1), labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))
df_sesvb$R13HLTC <- ordered(df_sesvb$R13HLTC, levels = c(5, 4, 3, 2, 1), labels = c("MuchWorse", "SomewhatWorse", "Same", "SomewhatBetter", "MuchBetter"))
df_sesvb$R13VGACTX <- ordered(df_sesvb$R13VGACTX, levels = c(5, 4, 3, 2, 1), labels = c("Never", "1-3/month", "1/week", ">1/week", "Daily"))
df_sesvb$R13MDACTX <- ordered(df_sesvb$R13MDACTX, levels = c(5, 4, 3, 2, 1), labels = c("Never", "1-3/month", "1/week", ">1/week", "Daily"))
df_sesvb$R13LTACTX <- ordered(df_sesvb$R13LTACTX, levels = c(5, 4, 3, 2, 1), labels = c("Never", "1-3/month", "1/week", ">1/week", "Daily"))
df_sesvb$R13BACK <- factor(df_sesvb$R13BACK, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13SMOKEV <- factor(df_sesvb$R13SMOKEV, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13SMOKEN <- factor(df_sesvb$R13SMOKEN, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13DRINK <- factor(df_sesvb$R13DRINK, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13SLEEPE <- factor(df_sesvb$R13SLEEPE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13HIBPE <- factor(df_sesvb$R13HIBPE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13DIABE <- factor(df_sesvb$R13DIABE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13CANCRE <- factor(df_sesvb$R13CANCRE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13LUNGE <- factor(df_sesvb$R13LUNGE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13HEARTE <- factor(df_sesvb$R13HEARTE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13STROKE <- factor(df_sesvb$R13STROKE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13PSYCHE <- factor(df_sesvb$R13PSYCHE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13ARTHRE <- factor(df_sesvb$R13ARTHRE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13ALZHEE <- factor(df_sesvb$R13ALZHEE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13DEMENE <- factor(df_sesvb$R13DEMENE, levels = c(0, 1), labels = c("No", "Yes"))
df_sesvb$R13SLFMEM <- ordered(df_sesvb$R13SLFMEM, levels = c(5, 4, 3, 2, 1), labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))
df_sesvb$R13PSTMEM <- ordered(df_sesvb$R13PSTMEM, levels = c(3, 2, 1), labels = c("Worse", "Same", "Better"))
df_sesvb$R13PRMEM <- ordered(df_sesvb$R13PRMEM, levels = c(5, 4, 3, 2, 1), labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))
df_sesvb$H13INPOVA <- factor(df_sesvb$H13INPOVA, levels = c(0, 1), labels = c("Above", "Below"))
df_sesvb$H13INPOV <- factor(df_sesvb$H13INPOV, levels = c(0, 1), labels = c("Above", "Below"))
df_sesvb$RAGENDER <- factor(df_sesvb$RAGENDER, levels = c(1, 2), labels = c("Male", "Female"))
df_sesvb$RAHISPAN <- factor(df_sesvb$RAHISPAN, levels = c(0, 1), labels = c("NonHisp", "Hispanic"))
df_sesvb$RARACEM <- factor(df_sesvb$RARACEM, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))
df_sesvb$RAEDEGRM <- ordered(df_sesvb$RAEDEGRM, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8), labels = c("NoDeg", "GED", "HS", "HS/GED", "AA", "BA", "MA", "PhD", "Other"))
df_sesvb$RAEDUC <- ordered(df_sesvb$RAEDUC, levels = c(1, 2, 3, 4, 5), labels = c("<HS", "GED", "HS", "SomeColl", "College+"))
df_sesvb$RABPLACE <- factor(df_sesvb$RABPLACE, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), labels = c("NE", "MA", "ENC", "WNC", "SA", "ESC", "WSC", "Mnt", "Pcf", "NAdiv", "NotUS"))
df_sesvb$RACOHBYR <- factor(df_sesvb$RACOHBYR, levels = c(0, 1, 2, 3, 4, 5, 6, 7), labels = c("None", "Ahead", "Coda", "HRS", "WarBabies", "EarlyBoomers", "MidBoomers", "LateBoomers"))

#combine with the item-response data
df_geogr$HHIDPN <- (df_geogr$HOUSEHOLD_IDENTIFICATION_NUMBER*1000) + df_geogr$PERSON_NUMBER #recode ID to conform with RAND
df_geogr[, 1:2] <- NULL #delete old ID information
df_resp$HHIDPN <- (df_resp$HRS_2016_HHID*1000) + df_resp$HRS_PERSON_NUMBER #recode ID to conform with RAND
df_resp[, 1:2] <- NULL #delete old ID information

df_resp <- merge(df_resp, df_geogr, by = "HHIDPN")
df_resp <- merge(df_resp, df_sesvb, by = "HHIDPN")

#drop HHIDPN
df_resp$HHIDPN <- NULL

#recode CERAD Trials into dichotomous variables
df_CERAD <- matrix(c(apply(df_resp[, 1:10], 1, function(x) any(1 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(2 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(3 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(4 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(5 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(6 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(7 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(8 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(9 %in% x)),
                     apply(df_resp[, 1:10], 1, function(x) any(10 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(1 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(2 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(3 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(4 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(5 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(6 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(7 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(8 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(9 %in% x)),
                     apply(df_resp[, 11:20], 1, function(x) any(10 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(1 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(2 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(3 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(4 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(5 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(6 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(7 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(8 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(9 %in% x)),
                     apply(df_resp[, 21:30], 1, function(x) any(10 %in% x))),
                   nrow = nrow(df_resp),
                   ncol = 30,
                   byrow = FALSE)
df_CERAD <- apply(df_CERAD, 2, function(x) as.numeric(x))
colnames(df_CERAD) <- c("Butter1", "Arm1", "Shore1", "Letter1", "Queen1", "Cabin1", "Pole1", "Ticket1", "Grass1", "Engine1", "Ticket2", "Cabin2", "Butter2", "Shore2", "Engine2", "Arm2", "Queen2", "Letter2", "Pole2", "Grass2", "Queen3", "Grass3", "Arm3", "Cabin3", "Pole3", "Shore3", "Butter3", "Engine3", "Ticket3", "Letter3")

#replace the new CERAD data
df_resp[, 1:30] <- df_CERAD
colnames(df_resp)[1:30] <- colnames(df_CERAD)

item_ident <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                8, 6, 1, 3, 10, 2, 5, 4, 7, 9,
                5, 9, 2, 6, 7, 3, 1, 10, 8, 4)

#convert data file to long format
df_resp$ID <- 1:nrow(df_resp)
df_long <- melt(df_resp, measure.vars = 1:30, id.vars = 31:ncol(df_resp)) #get long data for responses

df_long$ItemPos <- rep(rep(1:10, each = nrow(df_resp)), 3) #indicates the item order within each trial

Dep <- matrix(0, 30, 30)
temp <- rep(1:10, 6)
for(i in 1:30) {
  Dep[i, which(item_ident[11:30] == temp[i])+10] <- 1
}
Dep[11:30, 11:20] <- 0
Dep[1:10, 21:30] <- 0
Dep[21:30, 11:30] <- 0

Dep <- as.matrix(df_resp[, 1:30]) %*% Dep

#get unique dependency weight matrix for each item
DepButter <- Dep
DepButter[, c(2:12, 14:26, 28:30)] <- 0
DepArm <- Dep
DepArm[, c(1, 3:15, 17:22, 24:30)] <- 0
DepShore <- Dep
DepShore[, c(1:2, 4:13, 15:25, 27:30)] <- 0
DepLetter <- Dep
DepLetter[, c(1:3, 5:17, 19:29)] <- 0
DepQueen <- Dep
DepQueen[, c(1:4, 6:16, 18:20, 22:30)] <- 0
DepCabin <- Dep
DepCabin[, c(1:5, 7:11, 13:23, 25:30)] <- 0
DepPole <- Dep
DepPole[, c(1:6, 8:18, 20:24, 26:30)] <- 0
DepTicket <- Dep
DepTicket[, c(1:7, 9:10, 12:28, 30)] <- 0
DepGrass <- Dep
DepGrass[, c(1:8, 10:19, 21, 23:30)] <- 0
DepEngine <- Dep
DepEngine[, c(1:9, 11:14, 16:27, 29:30)] <- 0

#local item dependency matrix in long format
DepButter <- melt(DepButter)[, 3]
DepArm <- melt(DepArm)[, 3]
DepShore <- melt(DepShore)[, 3]
DepLetter <- melt(DepLetter)[, 3]
DepQueen <- melt(DepQueen)[, 3]
DepCabin <- melt(DepCabin)[, 3]
DepPole <- melt(DepPole)[, 3]
DepTicket <- melt(DepTicket)[, 3]
DepGrass <- melt(DepGrass)[, 3]
DepEngine <- melt(DepEngine)[, 3]
Dep <- melt(Dep)[, 3]

#add local dependency for each item to the dataframe
df_long$Dep <- Dep
df_long$DepButter <- DepButter
df_long$DepArm <- DepArm
df_long$DepShore <- DepShore
df_long$DepLetter <- DepLetter
df_long$DepQueen <- DepQueen
df_long$DepCabin <- DepCabin
df_long$DepPole <- DepPole
df_long$DepTicket <- DepTicket
df_long$DepGrass <- DepGrass
df_long$DepEngine <- DepEngine

#separate local dependency by trial
df_long$LocDep.1 <- ifelse(df_long$Trial2 == 1, df_long$LocDep, 0)
df_long$LocDep.2 <- ifelse(df_long$Trial3 == 1, df_long$LocDep, 0)

#rename variables for later
colnames(df_long) <- c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "Ravens", "TMTA", "TMTB", "CESD", "Impaired", "CERADimm", "CERADdisc",
                       "CensusSchool", "UrbanRural", "CensusRegion", "CensusDivision", "Age", "HealthRating", "HealthChng", "VigorousEx", "ModerateEx", 
                       "LightEx", "BackPrx", "SmokeHx", "SmokeNow", "DrinkHx", "DaysDrink", "DrinksSitting", "SleepHx", "BPHx", "DMHx", "CancerHx", "LungHx", "HeartHx", 
                       "StrokeHx", "PsychHx", "ArthritisHx", "AlzhHx", "DementiaHx", "ConditionsTotal", "ConditionsNew", "SelfMemory", "PastMemory", "BMI", "Waist", 
                       "OtherMemory", "YearsWorked", "HourlyWage", "WeeklyWage", "TotalAssets", "TotalWealthNoIRA", "ValueOfHouse", "TotalAssetsNonHousing", "FinancialWealth", 
                       "CapitalIncome", "TotalIncome", "PovertyThreshold", "InPovertyInst", "PovertyRatioInst", "InPoverty", "PovertyRatio", "Gender", "Ethnicity", "Race", 
                       "Education", "EducCat", "EdDegree", "MaternalEdu", "PaternalEdu", "CensusBirth", "Cohort", "BirthYear", "ID", "Item", "Resp", "ItemPos", "LocDep", 
                       "DepButter", "DepArm", "DepShore", "DepLetter", "DepQueen", "DepCabin", "DepPole", "DepTicket", "DepGrass", "DepEngine", "LocDep.1", "LocDep.2") #rename everything
df_long$Resp <- as.factor(df_long$Resp)
colnames(df_resp)[31:106] <- colnames(df_long)[1:76]

#add in the identification variables
df_long$Item <- factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                             18, 16, 11, 13, 20, 12, 15, 14, 17, 19,
                             25, 29, 22, 26, 27, 23, 21, 30, 28, 24), each = nrow(df_resp)),
                       levels = 1:30,
                       labels = c("Butter1", "Arm1", "Shore1", "Letter1", "Queen1", "Cabin1", "Pole1", "Ticket1", "Grass1", "Engine1",
                                  "Ticket2", "Cabin2", "Butter2", "Shore2", "Engine2", "Arm2", "Queen2", "Letter2", "Pole2", "Grass2", 
                                  "Queen3", "Grass3", "Arm3", "Cabin3", "Pole3", "Shore3", "Butter3", "Engine3", "Ticket3", "Letter3")) #identifier for each item (i.e., each word)

df_long$Trial1 <- c(rep(1, nrow(df_resp)*10), rep(0, nrow(df_resp)*20))
df_long$Trial2 <- c(rep(0, nrow(df_resp)*10), rep(1, nrow(df_resp)*10), rep(0, nrow(df_resp)*10))
df_long$Trial3 <- c(rep(0, nrow(df_resp)*20), rep(1, nrow(df_resp)*10))

df_long$Time1 <- rep(1, nrow(df_long))
df_long$Time2 <- c(rep(0, nrow(df_resp)*10), rep(1, nrow(df_resp)*20))
df_long$Time3 <- c(rep(0, nrow(df_resp)*20), rep(1, nrow(df_resp)*10))

#include a variable for learning over trials (time)
df_long$Time.c <- as.factor(ifelse(df_long$Trial2 == 1, 1, ifelse(df_long$Trial3 == 1, 2, 0)))
df_long$Time.n <- ifelse(df_long$Trial2 == 1, 1, ifelse(df_long$Trial3 == 1, 2, 0))

#condense some variables to deal with small group sizes
df_long$Race <- as.factor(ifelse(df_long$Race == "NA", "NA", ifelse(df_long$Race == "White", "White", "NotWhite")))

levels(df_long$CensusSchool)[levels(df_long$CensusSchool) == "NA"] <- NA
levels(df_long$UrbanRural)[levels(df_long$UrbanRural) == "NA"] <- NA

#clean up working space
rm(df_CERAD, Dep, temp, i, item_ident, df_geogr, df_sesvb, DepArm, DepButter, DepCabin, DepEngine, DepGrass, DepLetter, DepPole, DepQueen, DepShore, DepTicket)

########################
## ADD ELP ITEM COVARIATE DATA
########################

df_long$FreqHAL <- ifelse(grepl("Arm", df_long$Item), 9.925, ifelse(grepl("Butter", df_long$Item), 8.832, ifelse(grepl("Cabin", df_long$Item), 8.049, ifelse(grepl("Engine", df_long$Item), 10.586, ifelse(grepl("Grass", df_long$Item), 8.903, ifelse(grepl("Letter", df_long$Item), 10.973, ifelse(grepl("Pole", df_long$Item), 8.653, ifelse(grepl("Queen", df_long$Item), 9.768, ifelse(grepl("Shore", df_long$Item), 8.718, 9.555))))))))) #log scale, frequency of reported word in HAL study
df_long$FreqSTX <- ifelse(grepl("Arm", df_long$Item), 3.523, ifelse(grepl("Butter", df_long$Item), 3.018, ifelse(grepl("Cabin", df_long$Item), 3.001, ifelse(grepl("Engine", df_long$Item), 3.211, ifelse(grepl("Grass", df_long$Item), 2.933, ifelse(grepl("Letter", df_long$Item), 3.625, ifelse(grepl("Pole", df_long$Item), 2.808, ifelse(grepl("Queen", df_long$Item), 3.446, ifelse(grepl("Shore", df_long$Item), 3.006, 3.366))))))))) #log scale, frequency/1,000,000 words in SUBTLXus corpus
df_long$Concrete <- ifelse(grepl("Arm", df_long$Item), 4.960, ifelse(grepl("Butter", df_long$Item), 4.900, ifelse(grepl("Cabin", df_long$Item), 4.920, ifelse(grepl("Engine", df_long$Item), 4.860, ifelse(grepl("Grass", df_long$Item), 4.930, ifelse(grepl("Letter", df_long$Item), 4.700, ifelse(grepl("Pole", df_long$Item), 4.660, ifelse(grepl("Queen", df_long$Item), 4.450, ifelse(grepl("Shore", df_long$Item), 4.790, 4.700)))))))))
df_long$Density <- ifelse(grepl("Arm", df_long$Item), 0.650, ifelse(grepl("Butter", df_long$Item), 0.568, ifelse(grepl("Cabin", df_long$Item), 0.601, ifelse(grepl("Engine", df_long$Item), 0.641, ifelse(grepl("Grass", df_long$Item), 0.611, ifelse(grepl("Letter", df_long$Item), 0.664, ifelse(grepl("Pole", df_long$Item), 0.610, ifelse(grepl("Queen", df_long$Item), 0.668, ifelse(grepl("Shore", df_long$Item), 0.633, 0.611)))))))))
df_long$Diversity <- ifelse(grepl("Arm", df_long$Item), 1.657, ifelse(grepl("Butter", df_long$Item), 1.302, ifelse(grepl("Cabin", df_long$Item), 1.259, ifelse(grepl("Engine", df_long$Item), 1.334, ifelse(grepl("Grass", df_long$Item), 1.565, ifelse(grepl("Letter", df_long$Item), 1.639, ifelse(grepl("Pole", df_long$Item), 1.694, ifelse(grepl("Queen", df_long$Item), 1.544, ifelse(grepl("Shore", df_long$Item), 1.384, 1.658)))))))))
df_long$AoA <- ifelse(grepl("Arm", df_long$Item), 3.260, ifelse(grepl("Butter", df_long$Item), 5.780, ifelse(grepl("Cabin", df_long$Item), 6.390, ifelse(grepl("Engine", df_long$Item), 6.280, ifelse(grepl("Grass", df_long$Item), 3.940, ifelse(grepl("Letter", df_long$Item), 4.740, ifelse(grepl("Pole", df_long$Item), 5.630, ifelse(grepl("Queen", df_long$Item), 4.420, ifelse(grepl("Shore", df_long$Item), 6.925, 5.320)))))))))
df_long$BOI <- ifelse(grepl("Arm", df_long$Item), 6.478, ifelse(grepl("Butter", df_long$Item), 6.217, ifelse(grepl("Cabin", df_long$Item), 4.560, ifelse(grepl("Engine", df_long$Item), 5.333, ifelse(grepl("Grass", df_long$Item), 5.455, ifelse(grepl("Letter", df_long$Item), 5.259, ifelse(grepl("Pole", df_long$Item), 5.320, ifelse(grepl("Queen", df_long$Item), 4.083, ifelse(grepl("Shore", df_long$Item), 4.333, 5.333)))))))))
df_long$Phonemes <- ifelse(grepl("Arm", df_long$Item), 3, ifelse(grepl("Butter", df_long$Item), 4, ifelse(grepl("Cabin", df_long$Item), 5, ifelse(grepl("Engine", df_long$Item), 5, ifelse(grepl("Grass", df_long$Item), 4, ifelse(grepl("Letter", df_long$Item), 4, ifelse(grepl("Pole", df_long$Item), 3, ifelse(grepl("Queen", df_long$Item), 4, ifelse(grepl("Shore", df_long$Item), 3, 5)))))))))
df_long$Ambiguous <- ifelse(grepl("Arm", df_long$Item), 1, ifelse(grepl("Butter", df_long$Item), 1, ifelse(grepl("Cabin", df_long$Item), 0, ifelse(grepl("Engine", df_long$Item), 0, ifelse(grepl("Grass", df_long$Item), 1, ifelse(grepl("Letter", df_long$Item), 0, ifelse(grepl("Pole", df_long$Item), 0, ifelse(grepl("Queen", df_long$Item), 0, ifelse(grepl("Shore", df_long$Item), 1, 1))))))))) #based on parts of speech per elexicon
df_long$NamingZ <- ifelse(grepl("Arm", df_long$Item), -0.591, ifelse(grepl("Butter", df_long$Item), -0.465, ifelse(grepl("Cabin", df_long$Item), -0.469, ifelse(grepl("Engine", df_long$Item), -0.351, ifelse(grepl("Grass", df_long$Item), -0.561, ifelse(grepl("Letter", df_long$Item), -0.667, ifelse(grepl("Pole", df_long$Item), -0.631, ifelse(grepl("Queen", df_long$Item), -0.543, ifelse(grepl("Shore", df_long$Item), -0.394, -0.338)))))))))

#mean center non-standardized variables for more stable model estimation
df_long$FreqHAL <- (df_long$FreqHAL-mean(df_long$FreqHAL))/sd(unique(df_long$FreqHAL))
df_long$FreqSTX <- (df_long$FreqSTX-mean(df_long$FreqSTX))/sd(unique(df_long$FreqSTX))
df_long$Concrete <- (df_long$Concrete-mean(df_long$Concrete))/sd(unique(df_long$Concrete))
df_long$Diversity <- (df_long$Diversity-mean(df_long$Diversity))/sd(unique(df_long$Diversity))
df_long$AoA <- (df_long$AoA-mean(df_long$AoA))/sd(unique(df_long$AoA))
df_long$BOI <- (df_long$BOI-mean(df_long$BOI))/sd(unique(df_long$BOI))
df_long$Phonemes <- (df_long$Phonemes-mean(df_long$Phonemes))/sd(unique(df_long$Phonemes))
