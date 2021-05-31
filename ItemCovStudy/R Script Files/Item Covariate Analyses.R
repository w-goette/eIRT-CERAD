###################################################################################################
#               #####|===-- CODE TO CREATE DATAFRAME FROM HCAP DATA FILE --===|#####              #
###################################################################################################

#read respondent and informant files into R
library(asciiSetupReader)
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

library(haven)
df_sesvb <- read_sas(data = "Data Files/randhrs1992_2016v2.sas7bdat")

#simplify the demographic variables to just those of interest
library(dplyr)
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
                5, 9, 2, 6, 7, 3, 1, 10, 8, 4,
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

#convert data file to long format
library(reshape2)
df_resp$ID <- 1:nrow(df_resp)
df_long <- melt(df_resp, measure.vars = 1:30, id.vars = 31:ncol(df_resp)) #get long data for responses

df_long$ItemPos <- rep(rep(1:10, each = nrow(df_resp)), 3) #indicates the item order within each trial

Dep <- matrix(0, 30, 30)
temp <- rep(1:10, 6)
for(i in 1:30) {
  Dep[i, which(item_ident[11:30] == temp[i])+10] <- 1
}
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

#rename variables for later
colnames(df_long) <- c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "Ravens", "TMTA", "TMTB", "CESD", "Impaired", "CERADimm", "CERADdisc",
                       "CensusSchool", "UrbanRural", "CensusRegion", "CensusDivision", "Age", "HealthRating", "HealthChng", "VigorousEx", "ModerateEx", 
                       "LightEx", "BackPrx", "SmokeHx", "SmokeNow", "DrinkHx", "DaysDrink", "DrinksSitting", "SleepHx", "BPHx", "DMHx", "CancerHx", "LungHx", "HeartHx", 
                       "StrokeHx", "PsychHx", "ArthritisHx", "AlzhHx", "DementiaHx", "ConditionsTotal", "ConditionsNew", "SelfMemory", "PastMemory", "BMI", "Waist", 
                       "OtherMemory", "YearsWorked", "HourlyWage", "WeeklyWage", "TotalAssets", "TotalWealthNoIRA", "ValueOfHouse", "TotalAssetsNonHousing", "FinancialWealth", 
                       "CapitalIncome", "TotalIncome", "PovertyThreshold", "InPovertyInst", "PovertyRatioInst", "InPoverty", "PovertyRatio", "Gender", "Ethnicity", "Race", 
                       "Education", "EducCat", "EdDegree", "MaternalEdu", "PaternalEdu", "CensusBirth", "Cohort", "BirthYear", "ID", "Item", "Resp", "ItemPos", "LocDep", 
                       "DepButter", "DepArm", "DepShore", "DepLetter", "DepQueen", "DepCabin", "DepPole", "DepTicket", "DepGrass", "DepEngine") #rename everything
df_long$Resp <- as.factor(df_long$Resp)
colnames(df_resp)[31:106] <- colnames(df_long)[1:76]

#add in the identification variables
df_long$Item <- factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                             8, 6, 1, 3, 10, 2, 5, 4, 7, 9,
                             5, 9, 2, 6, 7, 3, 1, 10, 8, 4), each = nrow(df_resp)),
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       labels = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #identifier for each item (i.e., each word)

df_long$Trial1 <- c(rep(1, nrow(df_resp)*10), rep(0, nrow(df_resp)*20))
df_long$Trial2 <- c(rep(0, nrow(df_resp)*10), rep(1, nrow(df_resp)*10), rep(0, nrow(df_resp)*10))
df_long$Trial3 <- c(rep(0, nrow(df_resp)*20), rep(1, nrow(df_resp)*10))

df_long$Time1 <- rep(1, nrow(df_long))
df_long$Time2 <- c(rep(0, nrow(df_resp)*10), rep(1, nrow(df_resp)*20))
df_long$Time3 <- c(rep(0, nrow(df_resp)*20), rep(1, nrow(df_resp)*10))

#include a variable for learning over trials (time)
df_long$Time <- as.factor(ifelse(df_long$Trial2 == 1, 1, ifelse(df_long$Trial3 == 1, 2, 0)))

#condense some variables to deal with small group sizes
df_long$Race <- as.factor(ifelse(df_long$Race == "NA", "NA", ifelse(df_long$Race == "White", "White", "NotWhite")))

levels(df_long$CensusSchool)[levels(df_long$CensusSchool) == "NA"] <- NA
levels(df_long$UrbanRural)[levels(df_long$UrbanRural) == "NA"] <- NA

#clean up working space
rm(df_CERAD, Dep, DepButter, DepArm, DepShore, DepLetter, DepQueen, DepCabin, DepPole, DepTicket, DepGrass, DepEngine, temp, i, item_ident, df_geogr, df_sesvb)

###################################################################################################
#                      #####|===-- CODE FOR ITEM / WORD COVARIATES --===|#####                    #
###################################################################################################

df_long$FreqHAL <- ifelse(df_long$Item == "Arm", 9.925, ifelse(df_long$Item == "Butter", 8.832, ifelse(df_long$Item == "Cabin", 8.049, ifelse(df_long$Item == "Engine", 10.586, ifelse(df_long$Item == "Grass", 8.903, ifelse(df_long$Item == "Letter", 10.973, ifelse(df_long$Item == "Pole", 8.653, ifelse(df_long$Item == "Queen", 9.768, ifelse(df_long$Item == "Shore", 8.718, 9.555))))))))) #log scale, frequency of reported word in HAL study
df_long$FreqSTX <- ifelse(df_long$Item == "Arm", 3.523, ifelse(df_long$Item == "Butter", 3.018, ifelse(df_long$Item == "Cabin", 3.001, ifelse(df_long$Item == "Engine", 3.211, ifelse(df_long$Item == "Grass", 2.933, ifelse(df_long$Item == "Letter", 3.625, ifelse(df_long$Item == "Pole", 2.808, ifelse(df_long$Item == "Queen", 3.446, ifelse(df_long$Item == "Shore", 3.006, 3.366))))))))) #log scale, frequency/1,000,000 words in SUBTLXus corpus
df_long$Concrete <- ifelse(df_long$Item == "Arm", 4.960, ifelse(df_long$Item == "Butter", 4.900, ifelse(df_long$Item == "Cabin", 4.920, ifelse(df_long$Item == "Engine", 4.860, ifelse(df_long$Item == "Grass", 4.930, ifelse(df_long$Item == "Letter", 4.700, ifelse(df_long$Item == "Pole", 4.660, ifelse(df_long$Item == "Queen", 4.450, ifelse(df_long$Item == "Shore", 4.790, 4.700)))))))))
df_long$Density <- ifelse(df_long$Item == "Arm", 0.650, ifelse(df_long$Item == "Butter", 0.568, ifelse(df_long$Item == "Cabin", 0.601, ifelse(df_long$Item == "Engine", 0.641, ifelse(df_long$Item == "Grass", 0.611, ifelse(df_long$Item == "Letter", 0.664, ifelse(df_long$Item == "Pole", 0.610, ifelse(df_long$Item == "Queen", 0.668, ifelse(df_long$Item == "Shore", 0.633, 0.611)))))))))
df_long$Diversity <- ifelse(df_long$Item == "Arm", 1.657, ifelse(df_long$Item == "Butter", 1.302, ifelse(df_long$Item == "Cabin", 1.259, ifelse(df_long$Item == "Engine", 1.334, ifelse(df_long$Item == "Grass", 1.565, ifelse(df_long$Item == "Letter", 1.639, ifelse(df_long$Item == "Pole", 1.694, ifelse(df_long$Item == "Queen", 1.544, ifelse(df_long$Item == "Shore", 1.384, 1.658)))))))))
df_long$AoA <- ifelse(df_long$Item == "Arm", 3.260, ifelse(df_long$Item == "Butter", 5.780, ifelse(df_long$Item == "Cabin", 6.390, ifelse(df_long$Item == "Engine", 6.280, ifelse(df_long$Item == "Grass", 3.940, ifelse(df_long$Item == "Letter", 4.740, ifelse(df_long$Item == "Pole", 5.630, ifelse(df_long$Item == "Queen", 4.420, ifelse(df_long$Item == "Shore", 6.925, 5.320)))))))))
df_long$BOI <- ifelse(df_long$Item == "Arm", 6.478, ifelse(df_long$Item == "Butter", 6.217, ifelse(df_long$Item == "Cabin", 4.560, ifelse(df_long$Item == "Engine", 5.333, ifelse(df_long$Item == "Grass", 5.455, ifelse(df_long$Item == "Letter", 5.259, ifelse(df_long$Item == "Pole", 5.320, ifelse(df_long$Item == "Queen", 4.083, ifelse(df_long$Item == "Shore", 4.333, 5.333)))))))))
df_long$Phonemes <- ifelse(df_long$Item == "Arm", 3, ifelse(df_long$Item == "Butter", 4, ifelse(df_long$Item == "Cabin", 5, ifelse(df_long$Item == "Engine", 5, ifelse(df_long$Item == "Grass", 4, ifelse(df_long$Item == "Letter", 4, ifelse(df_long$Item == "Pole", 3, ifelse(df_long$Item == "Queen", 4, ifelse(df_long$Item == "Shore", 3, 5)))))))))
df_long$Ambiguous <- ifelse(df_long$Item == "Arm", 1, ifelse(df_long$Item == "Butter", 1, ifelse(df_long$Item == "Cabin", 0, ifelse(df_long$Item == "Engine", 0, ifelse(df_long$Item == "Grass", 1, ifelse(df_long$Item == "Letter", 0, ifelse(df_long$Item == "Pole", 0, ifelse(df_long$Item == "Queen", 0, ifelse(df_long$Item == "Shore", 1, 1))))))))) #based on parts of speech per elexicon
df_long$NamingZ <- ifelse(df_long$Item == "Arm", -0.591, ifelse(df_long$Item == "Butter", -0.465, ifelse(df_long$Item == "Cabin", -0.469, ifelse(df_long$Item == "Engine", -0.351, ifelse(df_long$Item == "Grass", -0.561, ifelse(df_long$Item == "Letter", -0.667, ifelse(df_long$Item == "Pole", -0.631, ifelse(df_long$Item == "Queen", -0.543, ifelse(df_long$Item == "Shore", -0.394, -0.338)))))))))

#mean center non-standardized variables for more stable model estimation
df_long$FreqHAL <- (df_long$FreqHAL-mean(df_long$FreqHAL))/sd(unique(df_long$FreqHAL))
df_long$FreqSTX <- (df_long$FreqSTX-mean(df_long$FreqSTX))/sd(unique(df_long$FreqSTX))
df_long$Concrete <- (df_long$Concrete-mean(df_long$Concrete))/sd(unique(df_long$Concrete))
df_long$Diversity <- (df_long$Diversity-mean(df_long$Diversity))/sd(unique(df_long$Diversity))
df_long$AoA <- (df_long$AoA-mean(df_long$AoA))/sd(unique(df_long$AoA))
df_long$BOI <- (df_long$BOI-mean(df_long$BOI))/sd(unique(df_long$BOI))
df_long$Phonemes <- (df_long$Phonemes-mean(df_long$Phonemes))/sd(unique(df_long$Phonemes))

###################################################################################################
#                    #####|===-- CODE TO FINISH INCLUSION CRITERIA --===|#####                    #
###################################################################################################

#setup the packages
library(mice) #for multiple imputation of missing data
library(brms) #workhorse of model fitting and analyses
library(bayestestR) #used to help evaluate models
library(tidyverse) #used to help analyze results
library(ggplot2) #used to visualize results

#make sure chains run in parallel across multiple cores
options(mc.cores = parallel::detectCores())

#make results comparable across machines
SEED <- 1932392
set.seed(SEED)

#compute the regression-based inclusion criterion
Missingness <- function(x) {
  sum(is.na(x)) / length(x) * 100
} #function computes percent of missing data
mean(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                              "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                              "Race", "Education", "MaternalEdu", "PaternalEdu")], 2, Missingness)) #average % missing data = 1.81
max(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                             "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                             "Race", "Education", "MaternalEdu", "PaternalEdu")], 2, Missingness)) #highest observed % missing data (by column) = 13.10
max(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                             "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                             "Race", "Education", "MaternalEdu", "PaternalEdu")], 1, Missingness)) #highest observed % missing data (by row) = 46.15

sapply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                          "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                          "Race", "Education", "MaternalEdu", "PaternalEdu")], function(x) Missingness(x)) #show how many missing points there are in each column

df_imp <- mice(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                        "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                        "Race", "Education", "MaternalEdu", "PaternalEdu", "Impaired")], m = 5, maxit = 30, seed = SEED, print = FALSE) #defaults make sense for data
saveRDS(df_imp, "Data_imputed.rds")

plot(df_imp) #good mixing overall of the chains
densityplot(df_imp) #results are plausible, support that those missing test data were generally more impaired, no apparent bias in the demographic variables

df_imp <- complete(df_imp, action = "all")
df_imps <- as.list(1:5)
for(i in 1:5) {
  df_imps[[i]] <- df_imp[[i]]
}

CogRegression <- brm_multiple(data = df_imps, family = student(), seed = SEED,
                              bf(mvbind(MMSE, Animals, CERADdel, LMIM, CPIM, SDMT, CPDM, LMDM, LMrcg, TMTA, TMTB, CERADimm, CERADdisc) ~
                                   Ravens + UrbanRural + Age + TotalAssets + FinancialWealth + TotalIncome + PovertyRatioInst + Gender + Ethnicity +
                                   Race + Education + MaternalEdu + PaternalEdu + Impaired, decomp = "QR"),
                              prior = c(prior("normal(0, 1)", class = "b", resp = "MMSE"),
                                        prior("student_t(3, 27, 11.5)", class = "Intercept", resp = "MMSE"),
                                        prior("normal(0, 1)", class = "b", resp = "Animals"),
                                        prior("student_t(3, 16, 20)", class = "Intercept", resp = "Animals"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADdel"),
                                        prior("student_t(3, 5, 8)", class = "Intercept", resp = "CERADdel"),
                                        prior("normal(0, 1)", class = "b", resp = "LMIM"),
                                        prior("student_t(3, 10, 15)", class = "Intercept", resp = "LMIM"),
                                        prior("normal(0, 1)", class = "b", resp = "CPIM"),
                                        prior("student_t(3, 8, 7)", class = "Intercept", resp = "CPIM"),
                                        prior("normal(0, 1)", class = "b", resp = "SDMT"),
                                        prior("student_t(3, 33, 37)", class = "Intercept", resp = "SDMT"),
                                        prior("normal(0, 1)", class = "b", resp = "CPDM"),
                                        prior("student_t(3, 6, 10)", class = "Intercept", resp = "CPDM"),
                                        prior("normal(0, 1)", class = "b", resp = "LMDM"),
                                        prior("student_t(3, 7.5, 16)", class = "Intercept", resp = "LMDM"),
                                        prior("normal(0, 1)", class = "b", resp = "LMrcg"),
                                        prior("student_t(3, 10, 8)", class = "Intercept", resp = "LMrcg"),
                                        prior("normal(0, 1)", class = "b", resp = "TMTA"),
                                        prior("student_t(3, 56, 122)", class = "Intercept", resp = "TMTA"),
                                        prior("normal(0, 1)", class = "b", resp = "TMTB"),
                                        prior("student_t(3, 142, 228)", class = "Intercept", resp = "TMTB"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADimm"),
                                        prior("student_t(3, 18, 16)", class = "Intercept", resp = "CERADimm"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADdisc"),
                                        prior("student_t(3, 9, 7)", class = "Intercept", resp = "CERADdisc"),
                                        prior("gamma(2, 0.1)", class = "nu"),
                                        prior("student_t(3, 0, 10)", class = "Intercept"),
                                        prior("normal(0, 1)", class = "b")),
                              file = "Fitted Models/cognitiveDiagnosisRegression")

CogRegression <- add_criterion(CogRegression, criterion = "bayes_R2") #add R2 estimate

pred <- array(0, dim = c(20000, 3167, 13))
for(i in 1:5) {
  pred[(1+((i-1)*4000)):(i*4000), 1:3167, 1:13] <- predictive_error(CogRegression, newdata = df_imps[[i]], subset = (1+((i-1)*4000)):(i*4000))
}

df_resp$MMSE_resid <- colMeans(pred[, , 1])/sd(pred[, , 1])
df_resp$Animals_resid <- colMeans(pred[, , 2])/sd(pred[, , 2])
df_resp$CERADdel_resid <- colMeans(pred[, , 3])/sd(pred[, , 3])
df_resp$LMIM_resid <- colMeans(pred[, , 4])/sd(pred[, , 4])
df_resp$CPIM_resid <- colMeans(pred[, , 5])/sd(pred[, , 5])
df_resp$SDMT_resid <- colMeans(pred[, , 6])/sd(pred[, , 6])
df_resp$CPDM_resid <- colMeans(pred[, , 7])/sd(pred[, , 7])
df_resp$LMDM_resid <- colMeans(pred[, , 8])/sd(pred[, , 8])
df_resp$LMrcg_resid <- colMeans(pred[, , 9])/sd(pred[, , 9])
df_resp$TMTA_resid <- colMeans(pred[, , 10])/sd(pred[, , 10])
df_resp$TMTB_resid <- colMeans(pred[, , 11])/sd(pred[, , 11])
df_resp$CERADimm_resid <- colMeans(pred[, , 12])/sd(pred[, , 12])
df_resp$CERADdisc_resid <- colMeans(pred[, , 13])/sd(pred[, , 13])

df_resp$MMSE_cut <- ifelse(df_resp$MMSE_resid < -1.0, 1, 0)
df_resp$Animals_cut <- ifelse(df_resp$Animals_resid < -1.0, 1, 0)
df_resp$CERADdel_cut <- ifelse(df_resp$CERADdel_resid < -1.0, 1, 0)
df_resp$LMIM_cut <- ifelse(df_resp$LMIM_resid < -1.0, 1, 0)
df_resp$CPIM_cut <- ifelse(df_resp$CPIM_resid < -1.0, 1, 0)
df_resp$SDMT_cut <- ifelse(df_resp$SDMT_resid < -1.0, 1, 0)
df_resp$CPDM_cut <- ifelse(df_resp$CPDM_resid < -1.0, 1, 0)
df_resp$LMDM_cut <- ifelse(df_resp$LMDM_resid < -1.0, 1, 0)
df_resp$LMrcg_cut <- ifelse(df_resp$LMrcg_resid < -1.0, 1, 0)
df_resp$TMTA_cut <- ifelse(df_resp$TMTA_resid < -1.0, 1, 0)
df_resp$TMTB_cut <- ifelse(df_resp$TMTB_resid < -1.0, 1, 0)
df_resp$CERADimm_cut <- ifelse(df_resp$CERADimm_resid < -1.0, 1, 0)
df_resp$CERADdisc_cut <- ifelse(df_resp$CERADdisc_resid < -1.0, 1, 0)

#run latent class analysis on cut scores
library(BayesLCA)
LCAres <- blca.vb(df_resp[, 121:133], 10, delta = 1/10)
df_resp$Group <- as.factor(max.col(Zscore(df_resp[, 121:133], LCAres)))
df_resp$Impaired <- as.factor(ifelse(df_resp$Impaired == "Yes", "Yes", ifelse(df_resp$Group != "1", "Yes", "No")))
saveRDS(LCAres, "LatentClassModel.rds")

#tidy up the data
df_resp <- df_resp[, -c(108:134)]
saveRDS(df_resp, "Data_wide.rds")

#keep only non-impaired samples for estimates
df_long$Impaired <- melt(df_resp[, c(1:30, 44)], measure.vars = 1:30)[, 1]
df_long <- subset(df_long, df_long$Impaired == "No")
df_long$ID <- rep(1:nrow(subset(df_resp, df_resp$Impaired == "No")), 30)
saveRDS(df_long, "Data_long.rds")

#tidy the environment
rm(pred, LCAres, df_imps, CogRegression)

###################################################################################################
#                      #####|===-- CODE FOR ITEM MODELS WITH BRMS --===|#####                     #
###################################################################################################

#prior predictive checks
Rasch_priors <-
  prior("normal(0, 2)", class = "Intercept") +
  prior("normal(0, 3)", class = "sd", group = "ID") + 
  prior("normal(0, 3)", class = "sd", group = "Item") #weakly informative priors

TwoPL_priors <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "ID", nlpar = "theta") + 
  prior("normal(0, 3)", class = "sd", group = "Item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "Item", nlpar = "logalpha") #weakly informative priors

Rasch_prior <- brm(Resp ~ 1 + (1 | Item) + (1 | ID),
                   data = df_long, family = bernoulli("logit"),
                   prior = Rasch_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "only",
                   file = "Fitted Models/1PL_prior_check")

plot(Rasch_prior) #wide coverage of possible values
pp_check(Rasch_prior, nsamples = 25, type = "bars")
pp_check(Rasch_prior, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(Rasch_prior, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))

TwoPL_prior <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + (1 |i| Item),
                      logalpha ~ 1 + (1 |i| Item),
                      nl = TRUE),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "only",
                   file = "Fitted Models/2PL_prior_check")

plot(TwoPL_prior) #wide coverage of possible values
pp_check(TwoPL_prior, nsamples = 25, type = "bars")
pp_check(TwoPL_prior, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_prior, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))

#save memory and space in the R environment
rm(Rasch_prior, TwoPL_prior)
gc()

#intercept only comparison of 1PL v. 2PL models
Rasch_inter <- brm(Resp ~ 1 + (1 | Item) + (1 | ID),
                   data = df_long, family = bernoulli("logit"),
                   prior = Rasch_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/1PL_intercept")

Rasch_inter <- add_criterion(Rasch_inter, criterion = "loo")

TwoPL_inter <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + (1 |i| Item),
                      logalpha ~ 1 + (1 |i| Item),
                      nl = TRUE),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_intercept")

TwoPL_inter <- add_criterion(TwoPL_inter, criterion = "loo")

LooCom1 <- loo_compare(Rasch_inter, TwoPL_inter)
ModWgt1 <- model_weights(Rasch_inter, TwoPL_inter, weights = "pseudobma")

pp_check(TwoPL_inter, nsamples = 25, type = "bars")
pp_check(TwoPL_inter, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_inter, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_inter)

#save memory and space in the R environment
rm(Rasch_inter)
gc()

#run random item models
TwoPL_ranItemTrialsInt <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                                 theta ~ 0 + (1 | ID),
                                 beta ~ 1 + (1 + Time |i| Item),
                                 logalpha ~ 1 + (1 + Time |i| Item),
                                 nl = TRUE),
                              data = df_long, family = bernoulli("logit"),
                              prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                              sample_prior = "yes",
                              file = "Fitted Models/2PL_random_items_allThree_int")

TwoPL_ranItemTrialsInt <- add_criterion(TwoPL_ranItemTrialsInt, criterion = "loo")

TwoPL_ranItemTrialsNoInt <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                                   theta ~ 0 + (1 | ID),
                                   beta ~ 1 + (0 + Time |i| Item),
                                   logalpha ~ 1 + (0 + Time |i| Item),
                                   nl = TRUE),
                                data = df_long, family = bernoulli("logit"),
                                prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                                sample_prior = "yes",
                                file = "Fitted Models/2PL_random_items_allThree_noInt")

TwoPL_ranItemTrialsNoInt <- add_criterion(TwoPL_ranItemTrialsNoInt, criterion = "loo")

df_long$Trial1 <- as.factor(df_long$Trial1) #convert to a factor for estimation
TwoPL_ranItemTrial1Int <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                                 theta ~ 0 + (1 | ID),
                                 beta ~ 1 + (1 + Trial1 |i| Item),
                                 logalpha ~ 1 + (1 + Trial1 |i| Item),
                                 nl = TRUE),
                              data = df_long, family = bernoulli("logit"),
                              prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                              sample_prior = "yes", control = list(adapt_delta = 0.95),
                              file = "Fitted Models/2PL_random_items_trialOne_int")

TwoPL_ranItemTrial1Int <- add_criterion(TwoPL_ranItemTrial1Int, criterion = "loo")

TwoPL_ranItemTrial1NoInt <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                                   theta ~ 0 + (1 | ID),
                                   beta ~ 1 + (0 + Trial1 |i| Item),
                                   logalpha ~ 1 + (0 + Trial1 |i| Item),
                                   nl = TRUE),
                                data = df_long, family = bernoulli("logit"),
                                prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                                sample_prior = "yes", control = list(adapt_delta = 0.95),
                                file = "Fitted Models/2PL_random_items_trialOne_noInt")

TwoPL_ranItemTrial1NoInt <- add_criterion(TwoPL_ranItemTrial1NoInt, criterion = "loo")
df_long$Trial1 <- as.numeric(df_long$Trial1)-1 #return factor to numeric

LooCom2 <- loo_compare(TwoPL_inter, TwoPL_ranItemTrialsInt, TwoPL_ranItemTrialsNoInt, TwoPL_ranItemTrial1Int, TwoPL_ranItemTrial1NoInt)
ModWgt2 <- model_weights(TwoPL_inter, TwoPL_ranItemTrialsInt, TwoPL_ranItemTrialsNoInt, TwoPL_ranItemTrial1Int, TwoPL_ranItemTrial1NoInt, weights = "pseudobma")

pp_check(TwoPL_ranItemTrialsNoInt, nsamples = 25, type = "bars")
pp_check(TwoPL_ranItemTrialsNoInt, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_ranItemTrialsNoInt, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_ranItemTrialsNoInt)

#save memory and space in the R environment
rm(TwoPL_inter, TwoPL_ranItemTrial1Int, TwoPL_ranItemTrial1NoInt, TwoPL_ranItemTrialsInt)
gc()

#run local dependency model
TwoPL_depmd <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + (-1 + Time |i| Item),
                      logalpha ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependency_model")

TwoPL_depmd <- add_criterion(TwoPL_depmd, criterion = "loo")

LooCom3 <- loo_compare(TwoPL_ranItemTrialsNoInt, TwoPL_depmd)
ModWgt3 <- model_weights(TwoPL_ranItemTrialsNoInt, TwoPL_depmd, weights = "pseudobma")

pp_check(TwoPL_depmd, nsamples = 25, type = "bars")
pp_check(TwoPL_depmd, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_depmd, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_depmd)

#save memory and space in the R environment
rm(TwoPL_ranItemTrialsNoInt)
gc()

#run local dependency model only on easiness parameter (local dependency credible intervals all cross 0 on the discrimination parameter)
TwoPL_depea <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependency_easinessOnly")

TwoPL_depea <- add_criterion(TwoPL_depea, criterion = "loo")

LooCom4 <- loo_compare(TwoPL_depmd, TwoPL_depea)
ModWgt4 <- model_weights(TwoPL_depmd, TwoPL_depea, weights = "pseudobma")

pp_check(TwoPL_depea, nsamples = 25, type = "bars")
pp_check(TwoPL_depea, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_depea, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_depea)

#save memory and space in the R environment
rm(TwoPL_depmd)
gc()

#run single local dependency model only on easiness parameter (test whether each item needs a unique estimate or not)
TwoPL_depsi <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + LocDep + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependency_trialOne")

TwoPL_depsi <- add_criterion(TwoPL_depsi, criterion = "loo")

LooCom5 <- loo_compare(TwoPL_depea, TwoPL_depsi)
ModWgt5 <- model_weights(TwoPL_depea, TwoPL_depsi, weights = "pseudobma")

pp_check(TwoPL_depsi, nsamples = 25, type = "bars")
pp_check(TwoPL_depsi, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_depsi, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_depsi)

#save memory and space in the R environment
rm(TwoPL_depsi)
gc()

#run multidimensional model that includes learning as a person-factor
TwoPL_learn <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (-1 + Time1 + Time2 + Time3 | ID),
                      beta ~ 1 + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_learningModel")

TwoPL_learn <- add_criterion(TwoPL_learn, criterion = "loo")

LooCom6 <- loo_compare(TwoPL_depea, TwoPL_learn)
ModWgt6 <- model_weights(TwoPL_depea, TwoPL_learn, weights = "pseudobma")

pp_check(TwoPL_learn, nsamples = 25, type = "bars")
pp_check(TwoPL_learn, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_learn, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_learn)

#run multidimensional model that includes correlated trial scores
TwoPL_multi <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (-1 + Time | ID),
                      beta ~ 1 + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 4000, warmup = 2000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_multidimensional")

TwoPL_multi <- add_criterion(TwoPL_multi, criterion = "loo")

LooCom7 <- loo_compare(TwoPL_depea, TwoPL_multi)
ModWgt7 <- model_weights(TwoPL_depea, TwoPL_multi, weights = "pseudobma")

pp_check(TwoPL_multi, nsamples = 25, type = "bars")
pp_check(TwoPL_multi, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_multi, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_multi)

#perform complete model comparisons for local independence assumption
loo_compare(TwoPL_ranItemTrialsNoInt, TwoPL_depea, TwoPL_learn, TwoPL_multi, criterion = "loo")
model_weights(TwoPL_ranItemTrialsNoInt, TwoPL_depea, TwoPL_learn, TwoPL_multi, weights = "pseudobma")

#run local dependency model + serial position effect
TwoPL_srlps <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + poly(ItemPos-1, 2) + (-1 + Time |i| Item),
                      logalpha ~ 1 + poly(ItemPos-1, 2) + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_serialPosition")

TwoPL_srlps <- add_criterion(TwoPL_srlps, criterion = "loo")

LooCom8 <- loo_compare(TwoPL_depea, TwoPL_srlps)
ModWgt8 <- model_weights(TwoPL_depea, TwoPL_srlps, weights = "pseudobma")

pp_check(TwoPL_srlps, nsamples = 25, type = "bars")
pp_check(TwoPL_srlps, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_srlps, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_srlps)

#save memory and space in the R environment
rm(TwoPL_srlps) #serial position effect does not improve model performance
gc()

#run local dependency model + serial position effect w/ interaction (note model has interaction to ensure serial position effect is able to differ across repetitions of the word list)
TwoPL_t3spe <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + poly(ItemPos-1, 2):Time + (-1 + Time |i| Item),
                      logalpha ~ 1 + poly(ItemPos-1, 2):Time + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_serialPosition_trialUnique")

TwoPL_t3spe <- add_criterion(TwoPL_t3spe, criterion = "loo")

LooCom9 <- loo_compare(TwoPL_depea, TwoPL_t3spe)
ModWgt9 <- model_weights(TwoPL_depea, TwoPL_t3spe, weights = "pseudobma")

pp_check(TwoPL_t3spe, nsamples = 25, type = "bars")
pp_check(TwoPL_t3spe, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_t3spe, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_t3spe)

#save memory and space in the R environment
rm(TwoPL_t3spe)
gc()

#run local dependency model + item covariates
TwoPL_itmex <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + FreqSTX + Concrete + Density + Diversity + AoA + BOI + Phonemes + Ambiguous + NamingZ + (-1 + Time |i| Item),
                      logalpha ~ 1 + FreqSTX + Concrete + Density + Diversity + AoA + BOI + Phonemes + Ambiguous + NamingZ + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes", control = list(max_treedepth = 20),
                   file = "Fitted Models/2PL_itemCovariates")

TwoPL_itmex <- add_criterion(TwoPL_itmex, criterion = "loo")

LooCom10 <- loo_compare(TwoPL_depea, TwoPL_itmex)
ModWgt10 <- model_weights(TwoPL_depea, TwoPL_itmex, weights = "pseudobma")

pp_check(TwoPL_itmex, nsamples = 25, type = "bars")
pp_check(TwoPL_itmex, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmex, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_itmex)

#save memory and space in the R environment
rm(TwoPL_itmex)
gc()

#run local dependency model + item covariates (more complex relationship based on previous model, no effects on discrimination since credible intervals all crossed 0)
TwoPL_itmcp <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + FreqSTX:Time + Concrete:Time + Density:Time + Diversity:Time + AoA:Time + BOI:Time + Phonemes:Time + Ambiguous:Time + NamingZ:Time + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes", control = list(adapt_delta = 0.85),
                   file = "Fitted Models/2PL_itemCovariates_byTrial")

TwoPL_itmcp <- add_criterion(TwoPL_itmcp, criterion = "loo")

LooCom11 <- loo_compare(TwoPL_depea, TwoPL_itmcp)
ModWgt11 <- model_weights(TwoPL_depea, TwoPL_itmcp, weights = "pseudobma")

pp_check(TwoPL_itmcp, nsamples = 25, type = "bars")
pp_check(TwoPL_itmcp, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmcp, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_itmcp)
plot(conditional_effects(TwoPL_itmcp), ask = FALSE) #visual inspection of conditional effects
#Item covariates by trial 3 are essentially non-predictive
#Covariates for items in trial 1 do tend to support some effect
#Covariates during trial 2 are variable (FreqSTX & AoA ~ trial 1 = trial2; Concrete, Diversity, & BOI ~ trial2 = trial 3)

#run local dependency model + item covariates (effects only on difficulty, new interactions for trial1/trial2 or just trial 1 + new frequency indicator)
df_long$Trial12 <- as.factor(ifelse(df_long$Trial3 == 1, 0, 1)) #new variable = 1 if in first or second trial and 0 otherwise
df_long$Trial23 <- as.factor(ifelse(df_long$Trial1 == 1, 0, 1)) #new variable = 1 if in second or third trial and 0 otherwise

TwoPL_itmcr <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + FreqHAL:Trial23 + Concrete:Trial23 + Diversity:Trial23 + AoA:Trial12 + BOI:Trial23 + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_itemCovariates_uniqueInteractions")

TwoPL_itmcr <- add_criterion(TwoPL_itmcr, criterion = "loo")

LooCom12 <- loo_compare(TwoPL_itmcp, TwoPL_itmcr)
ModWgt12 <- model_weights(TwoPL_itmcp, TwoPL_itmcr, weights = "pseudobma")

pp_check(TwoPL_itmcr, nsamples = 25, type = "bars")
pp_check(TwoPL_itmcr, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmcr, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_itmcr)
plot(conditional_effects(TwoPL_itmcr), ask = FALSE)
#Concrete conditional effects are identical, so no need for interaction
#BOI conditional effects are identical as well

#use results so far to simplify explanatory item model
TwoPL_itmsd <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + FreqHAL:Trial23 + Concrete + BOI + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_itemCovariates_simplified")

TwoPL_itmsd <- add_criterion(TwoPL_itmsd, criterion = "loo")

LooCom13 <- loo_compare(TwoPL_itmcp, TwoPL_itmsd)
ModWgt13 <- model_weights(TwoPL_itmcp, TwoPL_itmsd, weights = "pseudobma")

pp_check(TwoPL_itmsd, nsamples = 25, type = "bars")
pp_check(TwoPL_itmsd, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmsd, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_itmsd)

#use results to get a finalized explanatory item model (add back near-sig AoA variable)
TwoPL_itmfn <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + FreqHAL:Trial23 + AoA:Trial12 + BOI + Concrete + (-1 + Time |i| Item),
                      logalpha ~ 1 + (-1 + Time |i| Item),
                      nl = TRUE, decomp = "QR"),
                   data = df_long, family = bernoulli("logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_itemCovariates_reduced")

TwoPL_itmfn <- add_criterion(TwoPL_itmfn, criterion = "loo")

LooCom14 <- loo_compare(TwoPL_itmsd, TwoPL_itmfn)
ModWgt14 <- model_weights(TwoPL_itmsd, TwoPL_itmfn, weights = "pseudobma")

pp_check(TwoPL_itmfn, nsamples = 25, type = "bars")
pp_check(TwoPL_itmfn, nsamples = 25, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmfn, nsamples = 25, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 6, replace = FALSE))))
plot(TwoPL_itmfn)

#combine all the model comparisons
ModelComparisons <- list("LOOIC" = list("Comparison.1" = LooCom1, "Comparison.2" = LooCom2, "Comparison.3" = LooCom3, "Comparison.4" = LooCom4, "Comparison.5" = LooCom5, "Comparison.6" = LooCom6, "Comparison.7" = LooCom7, "Comparison.8" = LooCom8, "Comparison.9" = LooCom9, "Comparison.10" = LooCom10, "Comparison.11" = LooCom11, "Comparison.12" = LooCom12, "Comparison.13" = LooCom13, "Comparison.14" = LooCom14),
                         "BMAs"= list("Comparison.1" = ModWgt1, "Comparison.2" = ModWgt2, "Comparison.3" = ModWgt3, "Comparison.4" = ModWgt4, "Comparison.5" = ModWgt5, "Comparison.6" = ModWgt6, "Comparison.7" = ModWgt7, "Comparison.8" = ModWgt8, "Comparison.9" = ModWgt9, "Comparison.10" = ModWgt10, "Comparison.11" = ModWgt11, "Comparison.12" = ModWgt12, "Comparison.13" = ModWgt13, "Comparison.14" = ModWgt14))

rm(LooCom1, LooCom2, LooCom3, LooCom4, LooCom5, LooCom6, LooCom7, LooCom8, LooCom9, LooCom10, LooCom11, LooCom12, LooCom13, LooCom14,
   ModWgt1, ModWgt2, ModWgt3, ModWgt4, ModWgt5, ModWgt6, ModWgt7, ModWgt8, ModWgt9, ModWgt10, ModWgt11, ModWgt12, ModWgt13, ModWgt14)

#save data for use in .rmd
saveRDS(df_long, "Data_long.rds")
saveRDS(ModelComparisons, "ModelComparisons.rds")

###################################################################################################
#                     #####|===-- CODE FOR MODEL RESULTS FROM BRMS --===|#####                    #
###################################################################################################

#make functions for item and person fit testing (code provided by Burkner, 2020, online supplement - https://dx.doi.org/10.3390%2Fjintelligence8010005)
ll <- function(y, p) {
  y * log(p) + (1 - y) * log(1 - p)
}

fit_statistic <- function(model, criterion, group, nsamples = NULL) {
  group <- enquo(group)
  subset <- NULL
  if (!is.null(nsamples)) {
    subset <- sample(seq_len(nsamples(model)), nsamples) 
  }
  ppe <- posterior_epred(model, subset = subset) %>%
    t() %>%
    as.data.frame() %>%
    cbind(model$data) %>%
    gather("draw", "ppe", starts_with("V"))
  
  yrep <- posterior_predict(model, subset = subset) %>%
    t() %>%
    as.data.frame() %>%
    cbind(df_long$Resp) %>%
    gather("draw", "yrep", starts_with("V"))
  
  ppe %>%
    mutate(yrep = yrep$yrep) %>%
    mutate(
      crit = criterion(as.numeric(Resp)-1, ppe),
      crit_rep = criterion(yrep, ppe)
    ) %>%
    group_by(!!group, draw) %>%
    summarise(
      crit = sum(crit), 
      crit_rep = sum(crit_rep),
      crit_diff = crit_rep - crit
    ) %>%
    mutate(draw = as.numeric(sub("^V", "", draw))) %>%
    arrange(!!group, draw) %>%
    identity()
}

theme_hist <- function(...) {
  bayesplot::theme_default() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      ...
    )
}

#make functions for creating plots
windowsFonts(Times=windowsFont("Times New Roman"))
ICC <- function(Diff, Disc, probs = c(0.025, 0.975), items = c(1:10)) {
  #setup grids for analysis
  theta_grid <- seq(-6, 6, 0.01)
  P <- matrix(0, ncol = length(theta_grid)*ncol(Diff), nrow = nrow(Diff))
  item_ident <- c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")
  
  #compute item score functions
  for(j in 1:ncol(Diff)) {
    for(n in 1:length(theta_grid)) {
      P[, n+(length(theta_grid)*(j-1))] <- 1 / (1 + exp((-1*Disc[, j]) * (theta_grid[n] - (Diff[, j]))))
    }
  }
  
  #generate table and plot of item score functions
  plots <- list()
  i <- 0
  for(j in items) {
    i <- i + 1
    dat <- as.data.frame(posterior_summary(P[, (1+(length(theta_grid)*(j-1))):(length(theta_grid)*(j))], probs = c(probs[1], probs[2]))) %>%
      rename("LB" = 3, "UB" = 4) %>%
      add_column(Theta = theta_grid)
    plots[[i]] <- ggplot(data = dat, aes(x = Theta, y = Estimate)) +
      geom_line() +
      geom_ribbon(aes(x = Theta, y = Estimate, ymin = LB, ymax = UB), alpha = 0.25) +
      ggtitle(paste(item_ident[j], sep = "")) +
      xlab("Theta (Latent Trait)") +
      ylab("Probability of Correct Response") + 
      theme_bw() +
      theme(text=element_text(family="Times", face="bold", size=12))
  }
  
  #return results
  return(plots)
}

TES <- function(Diff, Disc, probs = c(0.025, 0.975)) {
  #setup grids for analysis
  theta_grid <- seq(-6, 6, 0.01)
  P <- matrix(0, ncol = length(theta_grid)*ncol(Diff), nrow = nrow(Diff))
  i <- matrix(0, ncol = length(theta_grid), nrow = nrow(Diff))
  
  #compute expected scores
  for(j in 1:ncol(Diff)) {
    for(n in 1:length(theta_grid)) {
      P[, n+(length(theta_grid)*(j-1))] <- 1 / (1 + exp((-1*Disc[, j]) * (theta_grid[n] - (Diff[, j]))))
    }
    i <- i + P[, (1+(length(theta_grid)*(j-1))):(length(theta_grid)*(j))]
  }
  
  #generate table and plot of expected score functions
  dat1 <- as.data.frame(posterior_summary(i, probs = c(probs[1], probs[2]))) %>%
    rename("LB" = 3, "UB" = 4) %>%
    add_column(Theta = theta_grid)
  plot1 <- ggplot(data = dat1, aes(x = Theta, y = Estimate)) +
    geom_line() +
    geom_ribbon(aes(y = Estimate, ymin = LB, ymax = UB), alpha = 0.25) +
    xlab("Theta (Latent Trait)") +
    ylab("Expected Total Score") +
    theme_bw() +
    theme(text=element_text(family="Times", face="bold", size=12))
  
  #return results
  return(plot1)
}

TIF <- function(Diff, Disc, probs = c(0.025, 0.975)) {
  #setup grids for analysis
  theta_grid <- seq(-6, 6, 0.01)
  ii <- P <- matrix(0, ncol = length(theta_grid)*ncol(Diff), nrow = nrow(Diff))
  i <- matrix(0, ncol = length(theta_grid), nrow = nrow(Diff))
  
  #compute test information
  for(j in 1:ncol(Diff)) {
    for(n in 1:length(theta_grid)) {
      P[, n+(length(theta_grid)*(j-1))] <- 1 / (1 + exp((-1*Disc[, j]) * (theta_grid[n] - (Diff[, j]))))
    }
  }
  
  Q <- 1-P
  
  for(k in 1:ncol(Diff)) {
    ii[, (1+(length(theta_grid)*(k-1))):(length(theta_grid)*(k))] <- as.matrix(Disc[, k]**2 * P[, (1+(length(theta_grid)*(k-1))):(length(theta_grid)*(k))] * Q[, (1+(length(theta_grid)*(k-1))):(length(theta_grid)*(k))])
    i <- i + ii[, (1+(length(theta_grid)*(k-1))):(length(theta_grid)*(k))]
  }
  
  #generate table and plot of test information functions
  dat1 <- as.data.frame(posterior_summary(i, probs = c(probs[1], probs[2]))) %>%
    rename("LB" = 3, "UB" = 4) %>%
    add_column(Theta = theta_grid)
  plot1 <- ggplot(data = dat1, aes(x = Theta, y = Estimate)) +
    geom_line() +
    geom_ribbon(aes(y = Estimate, ymin = LB, ymax = UB), alpha = 0.25) +
    xlab("Theta (Latent Trait)") +
    ylab("Information") +
    theme_bw() + 
    theme(text=element_text(family="Times", face="bold", size=12))
  
  #return results
  return(plot1)
}

###   summarize test functioning   ###
item_pars <- coef(TwoPL_itmfn, summary = FALSE)$Item #extract regression coefficients

item_cov <- data.frame(matrix(0, nrow = 10, ncol = 10, dimnames = list(c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine"),
                                                                       c("FreqHAL", "FreqSTX", "Concrete", "Density", "Diversity", "AoA", "BOI", "Phonemes", "Ambiguous", "NamingZ"))))
item_cov$Item <- c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")
item_cov$FreqHAL <- ifelse(item_cov$Item == "Arm", 9.925, ifelse(item_cov$Item == "Butter", 8.832, ifelse(item_cov$Item == "Cabin", 8.049, ifelse(item_cov$Item == "Engine", 10.586, ifelse(item_cov$Item == "Grass", 8.903, ifelse(item_cov$Item == "Letter", 10.973, ifelse(item_cov$Item == "Pole", 8.653, ifelse(item_cov$Item == "Queen", 9.768, ifelse(item_cov$Item == "Shore", 8.718, 9.555))))))))) #log scale, frequency of reported word in HAL study
item_cov$FreqSTX <- ifelse(item_cov$Item == "Arm", 3.523, ifelse(item_cov$Item == "Butter", 3.018, ifelse(item_cov$Item == "Cabin", 3.001, ifelse(item_cov$Item == "Engine", 3.211, ifelse(item_cov$Item == "Grass", 2.933, ifelse(item_cov$Item == "Letter", 3.625, ifelse(item_cov$Item == "Pole", 2.808, ifelse(item_cov$Item == "Queen", 3.446, ifelse(item_cov$Item == "Shore", 3.006, 3.366))))))))) #log scale, frequency/1,000,000 words in SUBTLXus corpus
item_cov$Concrete <- ifelse(item_cov$Item == "Arm", 4.960, ifelse(item_cov$Item == "Butter", 4.900, ifelse(item_cov$Item == "Cabin", 4.920, ifelse(item_cov$Item == "Engine", 4.860, ifelse(item_cov$Item == "Grass", 4.930, ifelse(item_cov$Item == "Letter", 4.700, ifelse(item_cov$Item == "Pole", 4.660, ifelse(item_cov$Item == "Queen", 4.450, ifelse(item_cov$Item == "Shore", 4.790, 4.700)))))))))
item_cov$Density <- ifelse(item_cov$Item == "Arm", 0.650, ifelse(item_cov$Item == "Butter", 0.568, ifelse(item_cov$Item == "Cabin", 0.601, ifelse(item_cov$Item == "Engine", 0.641, ifelse(item_cov$Item == "Grass", 0.611, ifelse(item_cov$Item == "Letter", 0.664, ifelse(item_cov$Item == "Pole", 0.610, ifelse(item_cov$Item == "Queen", 0.668, ifelse(item_cov$Item == "Shore", 0.633, 0.611)))))))))
item_cov$Diversity <- ifelse(item_cov$Item == "Arm", 1.657, ifelse(item_cov$Item == "Butter", 1.302, ifelse(item_cov$Item == "Cabin", 1.259, ifelse(item_cov$Item == "Engine", 1.334, ifelse(item_cov$Item == "Grass", 1.565, ifelse(item_cov$Item == "Letter", 1.639, ifelse(item_cov$Item == "Pole", 1.694, ifelse(item_cov$Item == "Queen", 1.544, ifelse(item_cov$Item == "Shore", 1.384, 1.658)))))))))
item_cov$AoA <- ifelse(item_cov$Item == "Arm", 3.260, ifelse(item_cov$Item == "Butter", 5.780, ifelse(item_cov$Item == "Cabin", 6.390, ifelse(item_cov$Item == "Engine", 6.280, ifelse(item_cov$Item == "Grass", 3.940, ifelse(item_cov$Item == "Letter", 4.740, ifelse(item_cov$Item == "Pole", 5.630, ifelse(item_cov$Item == "Queen", 4.420, ifelse(item_cov$Item == "Shore", 6.925, 5.320)))))))))
item_cov$BOI <- ifelse(item_cov$Item == "Arm", 6.478, ifelse(item_cov$Item == "Butter", 6.217, ifelse(item_cov$Item == "Cabin", 4.560, ifelse(item_cov$Item == "Engine", 5.333, ifelse(item_cov$Item == "Grass", 5.455, ifelse(item_cov$Item == "Letter", 5.259, ifelse(item_cov$Item == "Pole", 5.320, ifelse(item_cov$Item == "Queen", 4.083, ifelse(item_cov$Item == "Shore", 4.333, 5.333)))))))))
item_cov$Phonemes <- ifelse(item_cov$Item == "Arm", 3, ifelse(item_cov$Item == "Butter", 4, ifelse(item_cov$Item == "Cabin", 5, ifelse(item_cov$Item == "Engine", 5, ifelse(item_cov$Item == "Grass", 4, ifelse(item_cov$Item == "Letter", 4, ifelse(item_cov$Item == "Pole", 3, ifelse(item_cov$Item == "Queen", 4, ifelse(item_cov$Item == "Shore", 3, 5)))))))))
item_cov$Ambiguous <- ifelse(item_cov$Item == "Arm", 1, ifelse(item_cov$Item == "Butter", 1, ifelse(item_cov$Item == "Cabin", 0, ifelse(item_cov$Item == "Engine", 0, ifelse(item_cov$Item == "Grass", 1, ifelse(item_cov$Item == "Letter", 0, ifelse(item_cov$Item == "Pole", 0, ifelse(item_cov$Item == "Queen", 0, ifelse(item_cov$Item == "Shore", 1, 1))))))))) #based on parts of speech per elexicon
item_cov$NamingZ <- ifelse(item_cov$Item == "Arm", -0.591, ifelse(item_cov$Item == "Butter", -0.465, ifelse(item_cov$Item == "Cabin", -0.469, ifelse(item_cov$Item == "Engine", -0.351, ifelse(item_cov$Item == "Grass", -0.561, ifelse(item_cov$Item == "Letter", -0.667, ifelse(item_cov$Item == "Pole", -0.631, ifelse(item_cov$Item == "Queen", -0.543, ifelse(item_cov$Item == "Shore", -0.394, -0.338)))))))))
item_cov$Item <- NULL

item_cov %>%
  summarize_all(list(range = range))
#FreqHAL = 2.924
#Concrete = 0.51
#Density = 0.10
#Diversity = 0.435
#AoA = 3.665
#BOI = 2.395

cor(item_cov, method = "kendall") #get summary of item trait similarities

item_cov$FreqHAL <- as.numeric(scale(item_cov$FreqHAL))
item_cov$FreqSTX <- as.numeric(scale(item_cov$FreqSTX))
item_cov$Concrete <- as.numeric(scale(item_cov$Concrete))
item_cov$Diversity <- as.numeric(scale(item_cov$Diversity))
item_cov$AoA <- as.numeric(scale(item_cov$AoA))
item_cov$BOI <- as.numeric(scale(item_cov$BOI))
item_cov$Phonemes <- as.numeric(scale(item_cov$Phonemes))

beta1 <- (-1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time0"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial230"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial121"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))) %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item easiness for trial 1 parameters into a table
beta2 <- (-1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time1"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial231"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial121"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))) %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item easiness for trial 2 parameters into a table
beta3 <- (-1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time2"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial231"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial120"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))) %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item easiness for trial 3 parameters into a table

person_pars <- ranef(TwoPL_itmfn, summary = FALSE)$ID[, , "theta_Intercept"]
person_sds <- apply(person_pars, 1, sd)

alpha1 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time0"]) %>%
  exp() %>%
  sweep(1, person_sds, "*") %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item discrimination for trial 1 parameters into a table
alpha2 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time1"]) %>%
  exp() %>%
  sweep(1, person_sds, "*") %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item discrimination for trial 2 parameters into a table
alpha3 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time2"]) %>%
  exp() %>%
  sweep(1, person_sds, "*") %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(Item = rowname) %>%
  mutate(Item = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")) #put item discrimination for trial 3 parameters into a table
item_pars <- bind_rows(beta1, beta2, beta3, alpha1, alpha2, alpha3, .id = "nlpar") %>%
  mutate(
    nlpar = factor(nlpar, labels = rep(c("Difficulty", "Discrimination"), each = 3))
  ) #combine parameters into table
rm(alpha1, alpha2, alpha3, beta1, beta2, beta3, person_pars, person_sds) #keep environment tidy

#check item fit
item_fit <- fit_statistic(
  TwoPL_itmfn, criterion = ll, group = Item,
  nsamples = 1000
) #run test statistic
item_diff <- item_fit %>%
  group_by(Item) %>%
  summarise(bp = mean(crit_diff > 0)) #get results from test statistic
item_fit %>% 
  ggplot(aes(crit_diff)) +
  geom_histogram() +
  facet_wrap("Item", scales = "free") +
  theme_hist()  +
  theme(text=element_text(family="Times", face="bold", size=12)) #plot test statistic results
saveRDS(item_fit, "ItemFit.rds")

#get test reliability
theta_est <- as.data.frame(ranef(TwoPL_itmfn, summary = TRUE)$ID)[, 1:2]
var_theta <- as.numeric(VarCorr(TwoPL_itmfn)[[1]][[1]][1])
reliability_est <- var_theta/(var_theta + theta_est[, 2]^2)
rel_dat <- data.frame(Theta = theta_est[, 1], Rxx = reliability_est)
rm(theta_est, var_theta, reliability_est) #keep environment tidy
ggplot(data = rel_dat, aes(x = Theta, y = Rxx)) +
  geom_smooth(color = "black", size = 1.10) +
  ylab("Reliability Estimate") +
  xlab("Person Ability Estimate") +
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=12))

###   summarize latent trait   ###
person_pars <- ranef(TwoPL_itmfn, summary = FALSE)$ID[, , "theta_Intercept"] #get latent ability estimates
person_sds <- apply(person_pars, 1, sd) #add standard deviations
person_pars <- person_pars %>%
  sweep(1, person_sds, "/") %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column(var = "person") %>%
  mutate(person = as.numeric(person)) #put together a table with estimate, SD, and credible intervals
person_pars %>%
  arrange(Estimate) %>%
  mutate(id2 = seq_len(n())) %>%
  ggplot(aes(id2, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (sorted after Estimate)") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  theme(text=element_text(family="Times", face="bold", size=12)) #plot table results with credible intervals shown
saveRDS(person_pars, "PersonEstimates.rds")

#check person fit
person_fit <- fit_statistic(
  TwoPL_itmfn, criterion = ll, group = ID, 
  nsamples = 1000
) #run test statistic
person_diff <- person_fit %>%
  group_by(ID) %>%
  summarise(bp = mean(crit_diff > 0)) #get results from test statistic
person_max <- which.max(person_diff$bp) #find maximum statistic value
person_fit %>% 
  filter(ID == person_max) %>%
  ggplot(aes(crit_diff)) +
  geom_histogram() +
  theme_hist() +
  theme(text=element_text(family="Times", face="bold", size=12)) #show result from maximum statistic
sum(ifelse(person_diff[, 2] < .05, 1, 0)) #count number of person misfits using a Bayesian p-value of < .05
#24 < .05 or 1.97% of total sample
saveRDS(person_fit, "PersonFit.rds")

###     generate plots     ###
item_pars <- coef(TwoPL_itmfn, summary = FALSE)$Item #extract regression coefficients

beta1 <- -1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time0"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial230"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial121"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))
beta2 <- -1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time1"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial231"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial121"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))
beta3 <- -1*(item_pars[, , "beta_Intercept"] + item_pars[, , "beta_Time2"] + t(item_cov$FreqHAL*t(item_pars[, , "beta_FreqHAL:Trial231"])) + t(item_cov$AoA*t(item_pars[, , "beta_AoA:Trial120"])) + t(item_cov$Concrete*t(item_pars[, , "beta_Concrete"])) + t(item_cov$BOI*t(item_pars[, , "beta_BOI"])))

person_pars <- ranef(TwoPL_itmfn, summary = FALSE)$ID[, , "theta_Intercept"]
person_sds <- apply(person_pars, 1, sd)

alpha1 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time0"]) %>%
  exp() %>%
  sweep(1, person_sds, "*")
alpha2 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time1"]) %>%
  exp() %>%
  sweep(1, person_sds, "*")
alpha3 <- (item_pars[, , "logalpha_Intercept"] + item_pars[, , "logalpha_Time2"]) %>%
  exp() %>%
  sweep(1, person_sds, "*")

item_pars <- list(Difficulty = list(Trial1 = beta1, Trial2 = beta2, Trial3 = beta3, Total = cbind(beta1, beta2, beta3)),
                  Discrimination = list(Trial1 = alpha1, Trial2 = alpha2, Trial3 = alpha3, Total = cbind(alpha1, alpha2, alpha3)))
rm(alpha1, alpha2, alpha3, beta1, beta2, beta3, person_pars, person_sds) #keep environment tidy

Trial1 <- ICC(Diff = item_pars$Difficulty$Trial1, Disc = item_pars$Discrimination$Trial1)
Trial2 <- ICC(Diff = item_pars$Difficulty$Trial2, Disc = item_pars$Discrimination$Trial2)
Trial3 <- ICC(Diff = item_pars$Difficulty$Trial3, Disc = item_pars$Discrimination$Trial3)

#stitch figures together
library(patchwork)
(Trial1[[1]] | Trial1[[2]] | Trial1[[3]] | Trial1[[4]] | Trial1[[5]]) /
(Trial1[[6]] | Trial1[[7]] | Trial1[[8]] | Trial1[[9]] | Trial1[[10]])

(Trial2[[8]] | Trial2[[6]] | Trial2[[1]] | Trial2[[3]] | Trial2[[10]]) /
(Trial2[[2]] | Trial2[[5]] | Trial2[[4]] | Trial2[[7]] | Trial2[[9]])

(Trial3[[5]] | Trial3[[9]] | Trial3[[2]] | Trial3[[6]] | Trial3[[7]]) /
(Trial3[[3]] | Trial3[[1]] | Trial3[[10]] | Trial3[[8]] | Trial3[[4]])

ExpectedScore <- TES(Diff = item_pars$Difficulty$Total, Disc = item_pars$Discrimination$Total)
Information <- TIF(Diff = item_pars$Difficulty$Total, Disc = item_pars$Discrimination$Total)
saveRDS(ExpectedScore, "ExpectedScoreFunction.rds")
saveRDS(Information, "InformationFunction.rds")
