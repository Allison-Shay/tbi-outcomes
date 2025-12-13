library(stringr)
library(tidyverse)
library(dplyr)
library(readr)
#library(factoextra)
#library(Hmisc)
#library(corrplot)
#library(ggplot2)
#library(scales)
#library(knitr)
#library(broom)    # for tidy test output
#library(MatchIt)  # optional later for PS matching


#CHANGE THE YEAR ONLY FOR SETWD
setwd("/Users/hunter/Downloads/Carilion/TQIP 2007-2023/PUF AY 2018/CSV")
df <- read.csv("PUF_TRAUMA.csv")
#df_ais_lookup <- read.csv("PUF_AISDIAGNOSIS_LOOKUP.csv")
df_ais <- read.csv("PUF_AISDIAGNOSIS.csv")
df_icdproc <- read.csv("PUF_ICDPROCEDURE.csv")
df_icddiag<-read.csv("PUF_ICDDIAGNOSIS.csv")
#df_icdproc_lookup <- read.csv("PUF_ICDPROCEDURE_LOOKUP.csv")
#head(df_icdproc_lookup)

# lowercase column names
colnames(df) <- tolower(colnames(df))
#colnames(df_ais_lookup) <- tolower(colnames(df_ais_lookup))
colnames(df_ais) <- tolower(colnames(df_ais))
colnames(df_icdproc) <- tolower(colnames(df_icdproc))
#colnames(df_icdproc_lookup) <- tolower(colnames(df_icdproc_lookup))
colnames(df_icddiag) <- tolower(colnames(df_icddiag))
length(unique(df$inc_key))

df <- df %>% filter(ageyears >= 18)
df <- df[, !grepl("_biu$", names(df))] #drop BIU columns
#colnames(df)
#nrow(df) 
#ncol(df) 


# List of trach ICD codes
trach_codes <- c("0B110F4", "0B110Z4", "0B113F4", "0B113Z4", "0B114F4", "0B114Z4")
# list of gastrostomy codes
gastro_codes<-c("0DH60UZ", "0DH63UZ", "0DH64UZ", "0DH68UZ")
# list of craniotomy codes
crani_codes <- c("0N500ZZ","0N503ZZ","0N504ZZ","0N510ZZ","0N513ZZ","0N514ZZ","0N520ZZ","0N523ZZ","0N524ZZ","0N530ZZ","0N533ZZ","0N534ZZ","0N540ZZ","0N543ZZ","0N544ZZ","0N550ZZ","0N553ZZ","0N554ZZ","0N560ZZ","0N563ZZ","0N564ZZ","0N570ZZ","0N573ZZ","0N574ZZ","0N580ZZ","0N583ZZ","0N584ZZ",
                 "009430Z", "00943ZZ", "009440Z", "0N9000Z", "0N900ZZ", "0N9040Z", "0N904ZZ",
                 "0WJ10ZZ", "00J00ZZ", "0N800ZZ", "0N803ZZ", "0N804ZZ", "0NC10ZZ", "0NC13ZZ",
                 "0NC14ZZ", "0NC30ZZ", "0NC33ZZ", "0NC34ZZ", "0NC40ZZ", "0NC43ZZ", "0NC44ZZ",
                 "0NC50ZZ", "0NC53ZZ", "0NC54ZZ", "0NC60ZZ", "0NC63ZZ", "0NC64ZZ", "0N500ZZ",
                 "0N503ZZ", "0N504ZZ", "0NB00ZZ", "0NB03ZZ", "0NB04ZZ", "0NT10ZZ", "0NT30ZZ",
                 "0NT40ZZ", "0NT50ZZ", "0NT60ZZ", "0NT70ZZ", "009100Z", "00910ZZ", "00C10ZZ",
                 "00C13ZZ", "00C14ZZ", "00800ZZ", "00803ZZ", "00804ZZ", "00870ZZ", "00873ZZ",
                 "00874ZZ", "00590ZZ", "00593ZZ", "00594ZZ", "009900Z", "00990ZZ", "009930Z",
                 "00993ZZ", "009940Z", "00B90ZZ", "00B93ZZ", "00B94ZZ", "00C90ZZ", "00C93ZZ",
                 "00C94ZZ", "00N90ZZ", "00N93ZZ", "00N94ZZ", "00Q90ZZ", "00Q93ZZ", "00Q94ZZ",
                 "00580ZZ", "00583ZZ", "00584ZZ", "00880ZZ", "00883ZZ", "00884ZZ", "009800Z",
                 "00980ZZ", "009830Z", "00983ZZ", "009840Z", "00984ZZ", "00B80ZZ", "00B83ZZ",
                 "00B84ZZ", "00C80ZZ", "00C83ZZ", "00C84ZZ", "00510ZZ", "00513ZZ", "00514ZZ",
                 "00B10ZZ",
                 "00B13ZZ", "00B14ZZ", "00D10ZZ", "00D13ZZ", "00D14ZZ", "00T70ZZ", "00T73ZZ",
                 "00T74ZZ", "00B70ZZ", "00B73ZZ", "00B74ZZ", "00500ZZ", "00503ZZ", "00504ZZ",
                 "00B00ZZ", "00B03ZZ", "00B04ZZ", "0NS004Z", "0NS005Z", "0NS00ZZ", "0NS034Z",
                 "0NS035Z", "0NS03ZZ", "0NS044Z", "0NS045Z", "0NS04ZZ", "0NS0XZZ", "00Q20ZZ",
                 "00Q23ZZ", "00Q24ZZ", "00560ZZ", "00563ZZ", "00564ZZ", "00Q00ZZ", "00Q03ZZ",
                 "00Q04ZZ", "0NQ0XZZ",
                 
                 "00963ZZ", "00964ZZ", "009130Z", "00913ZZ", "009140Z", "00914ZZ", "009230Z",
                 "00923ZZ", "009240Z", "00924ZZ", "4A003BD", "4A007BD", "4A103BD", "4A107BD",
                 "4A003RD", "4A007RD", "4A103RD", "4A107RD", "4A003KD", "4A007KD", "4A103KD",
                 "4A107KD", "00J03ZZ", "00J04ZZ", "00K00ZZ", "00K03ZZ", "00K04ZZ", "00K70ZZ",
                 "00K73ZZ", "00K74ZZ", "00K80ZZ", "00K83ZZ", "00K84ZZ", "00K90ZZ", "00K93ZZ",
                 "00K94ZZ", "00KA0ZZ", "00KA3ZZ", "00KA4ZZ", "00KB0ZZ", "00KB3ZZ", "00KB4ZZ",
                 "00KC0ZZ", "00KC3ZZ", "00KC4ZZ", "00KD0ZZ", "00KD3ZZ", "00KD4ZZ", "0WJ13ZZ",
                 "0WJ14ZZ", "0NJ00ZZ", "0NJ03ZZ", "0NJ04ZZ", "0WH103Z", "0WH133Z", "0WH143Z",
                 "0WP103Z", "0WP133Z", "0WP143Z", "0WP1X3Z", "00H033Z", "009600Z", "009630Z",
                 "009640Z")


#midline shift is AIS predot codes but actually there's already a binary category
#midline_codes<-c("140608","140616","140624")


#crani code pts only
df_icdproc_crani <- df_icdproc %>%
  filter(icdprocedurecode %in% crani_codes)
crani_inc_keys <- df_icdproc_crani %>%
  pull(inc_key) %>%
  unique()
crani_inc_keys
length(crani_inc_keys) # so these are the inclusion keys

df_crani <- df %>%
  filter(inc_key %in% crani_inc_keys) #51 rows in 2022

# Merge df_crani (patient-level) with df_icdproc (procedure-level)
df_crani <- df_crani %>%
  left_join(df_icdproc, by = "inc_key")

# Collapse by inc_key
df_crani <- df_crani %>%
  group_by(inc_key) %>%
  summarise(
    # keep first for all patient-level columns except icdprocedurecode
    across(-icdprocedurecode, first),
    
    # collapse all ICD procedure codes into one string
    icd_procedures = paste(icdprocedurecode, collapse = "; ")
  ) %>%
  ungroup()

colnames(df_crani)[grepl("icdprocedurecode", colnames(df_crani))]


# Merge df_crani (patient-level) with df_icddiag
df_crani <- df_crani %>%
  left_join(df_icddiag, by = "inc_key")

# Collapse by inc_key
df_crani <- df_crani %>%
  group_by(inc_key) %>%
  summarise(
    # keep first for all patient-level columns except icdprocedurecode
    across(-icddiagnosiscode, first),
    
    # collapse all ICD procedure codes into one string
    icd_diagnoses = paste(icddiagnosiscode, collapse = "; ")
  ) %>%
  ungroup()


#ICH codes
# Define ICH type patterns
df_crani <- df_crani %>%
  mutate(
    ich_edh = ifelse(str_detect(icd_diagnoses, "S06\\.4"), 1, 0),
    ich_sdh = ifelse(str_detect(icd_diagnoses, "S06\\.5"), 1, 0),
    ich_sah = ifelse(str_detect(icd_diagnoses, "S06\\.6"), 1, 0),
    ich_iph = ifelse(str_detect(icd_diagnoses, "S06\\.3"), 1, 0),
    ich_other = ifelse(str_detect(icd_diagnoses, "S06\\.89|S06\\.9"), 1, 0)
  )

# Count number of ICH types per patient
df_crani <- df_crani %>%
  rowwise() %>%
  mutate(
    ich_type_count = sum(c(ich_edh, ich_sdh, ich_sah, ich_iph, ich_other), na.rm = TRUE)
  ) %>%
  ungroup()

# Create a single ICH category column
df_crani <- df_crani %>%
  mutate(
    ich_category = case_when(
      ich_type_count >= 2 ~ ">=2 concomitant ICHs",
      ich_edh == 1 & ich_type_count == 1 ~ "isolated EDH",
      ich_sdh == 1 & ich_type_count == 1 ~ "isolated SDH",
      ich_sah == 1 & ich_type_count == 1 ~ "isolated SAH",
      ich_iph == 1 & ich_type_count == 1 ~ "isolated IPH",
      ich_other == 1 & ich_type_count == 1 ~ "other/unspecified ICH",
      TRUE ~ NA_character_
    )
  )

table(df_crani$ich_category)



#make minority column
race_cols <- c("americanindian", "asian", "black", 
               "pacificislander", "raceother")

df_crani$minority <- ifelse(
  df_crani$white == 1, 
  0,
  ifelse(rowSums(df_crani[race_cols]) > 0, 1, NA)
)

table(df_crani$minority, useNA = "ifany")


colnames(df_crani) <- tolower(colnames(df_crani))
colnames(df_crani)

df_crani <- df_crani %>%
  rename(
    iss = iss_05,
    )

df_crani <- df_crani %>%
  rename(finaldischargedays = losdays
  )
df_crani <- df_crani %>%
  rename(tbicerebralmonitordays = cerebralmonitordays,
         tbicerebralmonitorhrs = cerebralmonitormins
        )
         
df_crani <- df_crani %>%
  rename(hospitalprocedurestarthrs = proceduremins,
         hospitalprocedurestartdays = proceduredays)

# YEAR MATTERS HERE
#pull the variables from email
df_crani1 <- df_crani %>%
  select(
    inc_key,
    sex,
    ageyears,
    minority,
    verificationlevel,
    teachingstatus,
    totalgcs,
    iss, #remove_05 for later years
    totalventdays,
    totaliculos,
    #losdays, #for 2018 
    finaldischargedays, # for 2019,2020
    #inpatientdays, #for later years; 
    hospdischargedisposition,
    icpparench,
    icpevdrain,
    icd_procedures,
    icd_diagnoses,
    ich_category,
   
     #emsmins,retired after 2018?
    #emsresponsemins,retired after 2018?
   # emsscenemins,retired after 2018?
   # hospitalarrivalhrs, new var
   # hospitalarrivaldays, new var
    withdrawallst,
    withdrawallstdays,
   # withdrawallstmins,retired after 2018
    statedesignation,
   # tccskullfracture, Flattened many-to-one response values from Trauma Center Criteria -> retired 2020
  
    # cerebralmonitordays, #2018
   # cerebralmonitormins, #2018
    tbicerebralmonitordays, #any yr after
    tbicerebralmonitorhrs, #any yr after
    
    tbimidlineshift,
    hospitalprocedurestarthrs, #proceduremins
    hospitalprocedurestartdays #proceduredays
  )
nrow(df_crani1)#51 for 2022, 63 for 2018

# YEAR MATTERS HERE
#for 2018
cols_to_keep <- c("inc_key","sex","ageyears","minority","verificationlevel",
                  "teachingstatus","totalgcs","iss_05","totalventdays",
                  "totaliculos","losdays","hospdischargedisposition",
                  "icpparench","icpevdrain","icd_procedures",
                  "icd_diagnoses","ich_category","withdrawallst","withdrawallstdays",
                  "statedesignation","cerebralmonitordays","cerebralmonitormins",
                  "tbimidlineshift","hospitalprocedurestarthrs","hospitalprocedurestartdays")

#for 2019,2020
cols_to_keep <- c("inc_key","sex","ageyears","minority","verificationlevel",
                  "teachingstatus","totalgcs","iss_05","totalventdays",
                  "totaliculos","finaldischargedays","hospdischargedisposition",
                  "icpparench","icpevdrain","icd_procedures",
                  "icd_diagnoses","ich_category","withdrawallst","withdrawallstdays",
                  "statedesignation","tbicerebralmonitordays",)

#I think I standardized the variables see above
cols_to_keep <- c("inc_key","sex","ageyears","minority","verificationlevel",
                  "teachingstatus","totalgcs","iss","totalventdays",
                  "totaliculos","finaldischargedays","hospdischargedisposition",
                  "icpparench","icpevdrain","icd_procedures",
                  "icd_diagnoses","ich_category","withdrawallst","withdrawallstdays",
                  "statedesignation","tbicerebralmonitordays","tbicerebralmonitormins",
                  "tbimidlineshift","hospitalprocedurestarthrs","hospitalprocedurestartdays")


# Keep only those that exist in df_crani
cols_to_keep <- intersect(cols_to_keep, colnames(df_crani))
df_crani1 <- df_crani %>% select(all_of(cols_to_keep))


#now make a trach and gastro column
df_crani1 <- df_crani1 %>%
  mutate(trach = ifelse(
    str_detect(icd_procedures, str_c(trach_codes, collapse = "|")),
    1, 0
  ))

df_crani1 <- df_crani1 %>%
  mutate(gastro = ifelse(
    str_detect(icd_procedures, str_c(gastro_codes, collapse = "|")),
    1, 0
  ))

print(head(df_crani1), width = Inf)
# Count
table(df_crani1$trach)
table(df_crani1$gastro)


# Head injuries only
df_head <- df_ais %>% filter(str_detect(aispredot, "^1"))
df_nonhead <- df_ais %>% filter(!str_detect(aispredot, "^1"))

# Max AIS per patient
ais_head <- df_head %>%
  group_by(inc_key) %>%
  summarise(ais_head = max(aisseverity, na.rm = TRUE))

ais_nonhead <- df_nonhead %>%
  group_by(inc_key) %>%
  summarise(ais_nonhead = max(aisseverity, na.rm = TRUE))

# Merge both into one summary
ais_summary <- ais_head %>%
  full_join(ais_nonhead, by = "inc_key")

df_crani1 <- df_crani1 %>%
  left_join(ais_summary, by = "inc_key")
print(df_crani1, n = 10, width = Inf)

#Patients with isolated blunt TBI defined as patients with any Head-Abbreviated Injury Scale (AIS) and non-Head AIS score <3.
isolated_tbi <- df_crani1 %>%
  mutate(ais_nonhead = ifelse(is.na(ais_nonhead), 0, ais_nonhead)) %>%
  filter(ais_head >= 1 & ais_nonhead < 3)

#YEAR MATTERS HERE
#for older years, change teaching status 0 = nonteaching 1= university
# --- Clean & convert teachingstatus for isolated_tbi only ---
if ("teachingstatus" %in% names(isolated_tbi)) {
  
  isolated_tbi$teachingstatus <- isolated_tbi$teachingstatus |>
    tolower() |>
    trimws() |>
    recode(
      "university" = "1",
      "community" = "0",
      "nonteaching" = "0"
    )
  
  isolated_tbi$teachingstatus <- as.integer(isolated_tbi$teachingstatus)
}

#for 2020, change teaching status 0 = nonteaching 1= university
# --- Clean & convert teachingstatus for isolated_tbi only ---
if ("teachingstatus" %in% names(isolated_tbi)) {
  
  isolated_tbi$teachingstatus <- isolated_tbi$teachingstatus |>
    tolower() |>
    trimws() |>
    recode(
      "academic" = "1",
      "community" = "0",
      "nonteaching" = "0"
    )
  
  isolated_tbi$teachingstatus <- as.integer(isolated_tbi$teachingstatus)
}

#for 2021 after teaching status
isolated_tbi$teachingstatus <- ifelse(isolated_tbi$teachingstatus == 1, 1, 0)


print(isolated_tbi, n = 10, width = Inf)


#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2018cleaned.csv")
#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2019cleaned.csv")
#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2020cleaned.csv")
#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2021cleaned.csv")
#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2022cleaned.csv")
#write_csv(isolated_tbi, "/Users/hunter/Downloads/Carilion/current attempt/2023cleaned.csv")


setwd("/Users/hunter/Downloads/Carilion/current attempt")
cleaned2018 <- read.csv("2018cleaned.csv")
cleaned2019 <- read.csv("2019cleaned.csv")
cleaned2020 <- read.csv("2020cleaned.csv")
cleaned2021 <- read.csv("2021cleaned.csv")
cleaned2022 <- read.csv("2022cleaned.csv")
cleaned2023 <- read.csv("2023cleaned.csv")

#head(cleaned2018)
#head(cleaned2019)

names(cleaned2018)[names(cleaned2018) == "losdays"] <- "finaldischargedays"
names(cleaned2018)[names(cleaned2018) == "iss_05"] <- "iss"
names(cleaned2019)[names(cleaned2019) == "iss_05"] <- "iss"
names(cleaned2020)[names(cleaned2020) == "iss_05"] <- "iss"

#add year column
cleaned2018$year <- 2018
cleaned2019$year <- 2019
cleaned2020$year <- 2020
cleaned2021$year <- 2021
cleaned2022$year <- 2022
cleaned2023$year <- 2023

lapply(list(cleaned2018, cleaned2019, cleaned2020, cleaned2021, cleaned2022, cleaned2023), names)
df_all <- bind_rows(
  cleaned2018,
  cleaned2019,
  cleaned2020,
  cleaned2021,
  cleaned2022,
  cleaned2023
)
glimpse(df_all)
write_csv(df_all, "/Users/hunter/Downloads/Carilion/current attempt/merged years.csv")

#what is the avg age now? 
mean_age <- df_all %>%
  summarise(mean_age = mean(ageyears, na.rm = TRUE)) %>%
  pull(mean_age)
mean_age
#EMSMins
#EMSResponseMins
#EMSSceneMins

#WITHDRAWALLST
#WithdrawalLSTMins
#WithdrawalLSTDays

#STATEDESIGNATION

#ich match post filtration but preanalysis TBD
#investigate skull fracture and balance covariates along with ich
  #binary TCCSKULLFRACTURE

#transfer to ltc = code 12
#time to procedure?? cont

#CerebralMonitorDays cont

##########
1=Discharged/Transferred to a short-term general hospital for inpatient care
2=Discharged/Transferred to an Intermediate Care Facility (ICF)
3=Discharged/Transferred to home under care of organized home health service
4=Left against medical advice or discontinued care
5=Deceased/Expired
6=Discharged to home or self-care (routine discharge)
7=Discharged/Transferred to Skilled Nursing Facility (SNF)
8=Discharged/Transferred to hospice care
10=Discharged/Transferred to court/law enforcement.
11=Discharged/Transferred to inpatient rehab or designated unit
12=Discharged/Transferred to Long Term Care Hospital (LTCH)
13=Discharged/Transferred to a psychiatric hospital or psychiatric distinct part unit of a hospital
14=Discharged/Transferred to another type of institution not defined elsewhere


#discharge disposition table 
###pull the midline shift code should be in the loveplot