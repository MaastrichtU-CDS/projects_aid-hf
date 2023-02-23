#Packages###################################################################################################################

library("haven")
library("readxl")
library("ggplot2")
library("tidyverse")
library("ggnewscale")
library("data.table")
library("labelled")
library('tidymodels')
library('labelled')
library('easyGgplot2')

#Read data####
setwd("~/Data/aid-hf")

data <- read_sav(".\\TIME_CHF_all_correct_follow_up.sav")
colnames(data)[1] <- "id"
data <- data[order(c(data$id)),]

data$diff_refdate_entrydate <- as.numeric(difftime(data$Dateofstudyentry, data$RefDate, units = c("days")))

# Diuretics
data.loop_diuretics <- read_excel(".\\Medication daily doses.xlsx", sheet = 6)
colnames(data.loop_diuretics)[1] <- "id"
table(data$id %in% data.loop_diuretics$id)
data.loop_diuretics <- merge(data[,c("id")], 
                             data.loop_diuretics, 
                             by = "id")
data.loop_diuretics <- data.loop_diuretics[order(c(data.loop_diuretics$id)),]

# Hospitalisations
data.hospitalisations <- read_excel(".\\1. Data\\Hospitalisations overview.xlsx")
colnames(data.hospitalisations)[1] <- "id"
table(unique(data.hospitalisations$id) %in% data$id)
data.hospitalisations <- merge(data[,c("id", "RefDate")], 
                               data.hospitalisations, 
                               by = "id")
data.hospitalisations <- data.hospitalisations[order(c(data.hospitalisations$id)),]#$table(unique(data.hospitalisations$id) %in% data$id)


# Symptoms at visits
data.symptoms <- read_excel(".\\1. Data\\Visits Symptoms.xlsx", sheet = 1)
colnames(data.symptoms)[1] <- "id"
data.symptoms <- merge(data[,c("id")], data.symptoms, 
                       by = "id")
data.symptoms <- data.symptoms[order(c(data.symptoms$id)),]


# Biomarker measurements at visits
data.biomarkers <- read_excel(".\\1. Data\\TIME-CHF biomarkers.xlsx", sheet = 1)
colnames(data.biomarkers)[1] <- "id"
data.biomarkers <- merge(data[,c("id")], data.biomarkers, 
                       by = "id")
data.biomarkers <- data.biomarkers[order(c(data.biomarkers$id)),]

# Dose change indications
data.indications <- read_excel(".\\1. Data\\Combi diuretic all indication 1221.xlsx", sheet = 1)
colnames(data.indications)[1] <- "id"
data.indications <- merge(data[,c("id")], data.indications, 
                         by = "id")
data.indications <- data.indications[order(c(data.indications$id)),]

data.indications.def <- read_excel(".\\1. Data\\Indications.xlsx", sheet = 1)

# Alt congestion score
data.congention_score <- read_excel(".\\1. Data\\Congestion score.xlsx");
colnames(data.congention_score)[1] <- "id"
data.congention_score <- merge(data[,c("id")], data.congention_score, 
                         by = "id")
data.congention_score <- data.congention_score[order(c(data.congention_score$id)),]

# Combined congestion score
data.new_congestion_scores <- read_excel(".\\1. Data\\Congestion score new scores.xlsx", sheet = 1)
colnames(data.new_congestion_scores)[1] <- "id"
data.new_congestion_scores <- merge(data[,c("id")], data.new_congestion_scores, 
                                    by = "id")
data.new_congestion_scores <- data.new_congestion_scores[order(c(data.new_congestion_scores$id)),]

palliative_care_downtitrations <- c("{23CB22D7-900E-4E03-B276-E9B28840CEAB}","{26B7D47E-B1F4-478B-9714-9FCA42D3A7F8}",
   "{62DD8DFC-CA66-4CEA-AB5C-75F70A733C2F}", "{6A6300FB-5356-4B00-B2C8-51FCE5FFB79F}",
   "{6ED0814F-8C00-4461-9D4A-6730026C3615}","{893280EF-D927-4D66-B816-6C3AB8AF1B5A}",
   "{9DE5DC1B-0932-4C7C-B461-DE310C769F17}","{A38AB090-1AAC-46BC-A7C6-A25C61D01EDD}",
   "{D003D007-5927-4860-8417-620F1E992827}")

#Dose per day####
data.daily_dose <- pivot_longer(data.loop_diuretics[, c(1, 4:650)], !id, names_to = "Dx", values_to = "Dose") 
data.daily_dose$Dx <- as.numeric(str_replace(data.daily_dose$Dx, "D", ""))
# Dose change: present day - previous day 
data.daily_dose <- data.daily_dose %>%
  group_by(id) %>%
  arrange(id,Dx) %>%
  mutate(Change = Dose - lag(Dose, default = first(Dose)))

data.daily_dose$Change_cat = ifelse(data.daily_dose$Change > 0, "Up-titration", 
                                    ifelse(data.daily_dose$Change < 0, "Down-titration", "No change"))

data.daily_dose <- data.daily_dose[!is.na(data.daily_dose$Dose),]

#Hospitalisations###############################################################

colnames(data.hospitalisations) <- str_replace(colnames(data.hospitalisations), "Cause of Hospitalisation", "Cause")
 
data.hospitalisations$cause.CHF <- ifelse(data.hospitalisations$Cause == "Worsening CHF", 1,0)
data.hospitalisations$cause.linked <- ifelse(data.hospitalisations$Cause  %in%  c('Trauma / fracture', 'Syncope/Tachyarrhythmias', 'Syncope/Hypotension', 'Stroke', 'Syncope unknown'), 1, 0)
data.hospitalisations$cause.renal <- ifelse(data.hospitalisations$Cause == "Renal failure", 1, 0)
data.hospitalisations$cause.planned <- ifelse(as.numeric(rownames(data.hospitalisations)) %in% grep("planned|Planned|Elective|elective|Battery|battery", data.hospitalisations$Comment), 1, 0)
data.hospitalisations$cause.other <- ifelse((data.hospitalisations$cause.CHF == 0) & (data.hospitalisations$cause.planned != 1)& (data.hospitalisations$cause.linked != 1)& (data.hospitalisations$cause.renal != 1), 1, 0)

data.hospitalisations$Dx.admission <- difftime(data.hospitalisations$"Date of Admission", 
                                               data.hospitalisations$RefDate, units = c("days"))
data.hospitalisations$Dx.discharge <- difftime(data.hospitalisations$"Date of Discharge", 
                                               data.hospitalisations$RefDate, units = c("days"))

data.hospitalisations.stripped <- data.hospitalisations %>% 
  select("id", 
         "RefDate",
         "cause.CHF", 
         "cause.renal",
         "cause.planned", 
         "cause.linked", 
         "cause.other", 
         "Dx.admission", 
         "Dx.discharge")

#Daily status####
data.daily_status <- data.hospitalisations %>% 
  select("id","Dx.admission","Dx.discharge", "cause.CHF") %>% 
  filter(cause.CHF == 1) %>% 
  merge(data.daily_dose, by="id", all.y = TRUE)

data.daily_status$Hospitalised <- with(data.daily_status, 
                                       ifelse(!is.na(Dx.admission) & Dx > Dx.admission & (is.na(Dx.discharge) | Dx < Dx.discharge),   "Hospitalised", NA))
data.daily_status$Hospitalised <- with(data.daily_status, 
                                       ifelse(!is.na(Dx.admission) & Dx == Dx.admission,   "Hospital admission", data.daily_status$Hospitalised))
data.daily_status$Hospitalised <- with(data.daily_status, 
                                       ifelse(!is.na(Dx.discharge) & Dx == Dx.discharge,   "Hospital discharge", data.daily_status$Hospitalised))

#ifelse(!is.na(Dx.admission) & Dx >= Dx.admission & (is.na(Dx.discharge) | Dx < Dx.discharge) & cause.CHF == 1,   1, 0))
data.daily_status <- data.daily_status %>%
  subset(select= -c(Dx.admission,Dx.discharge)) %>%
  group_by(id, Dx, Dose, Change, Change_cat) %>% 
  summarise(Hospitalised = ifelse("Hospital admission" %in% Hospitalised, "Hospital admission", 
                                  ifelse("Hospital discharge" %in% Hospitalised, "Hospital discharge",
                                  ifelse("Hospitalised" %in% Hospitalised, "Hospitalised",NA)))
  )

data.daily_status <- data.daily_status %>%
  merge(data[, c("id", "RefDate", "Dateofdeath")], by = "id")
data.daily_status$Dx.Deceased <- difftime(data.daily_status$Dateofdeath, 
                                          data.daily_status$RefDate, units = c("days"))

data.daily_status$Deceased <- with(data.daily_status, ifelse(!is.na(Dx.Deceased) & Dx == Dx.Deceased -1, 1, 0))
data.daily_status <- data.daily_status[!with(data.daily_status, !is.na(Dx.Deceased) & Dx >= Dx.Deceased),]
  
data.daily_status <- data.daily_status %>% 
  subset(select= -c(RefDate, Dateofdeath, Dx.Deceased))

ignore_hospitalisations <- FALSE
data.daily_status$Event <- with(data.daily_status, ifelse(Deceased == 1, "Deceased", ifelse(!is.na(Hospitalised) & !ignore_hospitalisations, Hospitalised, Change_cat)))
#data.daily_status$Event <- with(data.daily_status, ifelse(Deceased == 1, "Deceased", ifelse(!is.na(Hospitalised) & Hospitalised != "Hospitalised", Hospitalised, Change_cat)))
data.daily_status$Event <- ifelse(data.daily_status$Event == "No change" & data.daily_status$Dx > 0, NA, data.daily_status$Event)
data.daily_status$Event <- ifelse(data.daily_status$Event == "Hospitalised", NA, data.daily_status$Event)

data.daily_status <-
  data.daily_status %>% 
  group_by(id) %>% 
#  filter(!is.na(Hospitalised)) %>% 
  mutate(Event = ifelse(Dx == max(Dx) & is.na(Event), "No change" , Event))
data.events <- data.daily_status[!is.na(data.daily_status$Event),]
data.event_to_event <- data.events %>% 
  group_by(id) %>% 
  mutate(
    next_event = lead(Event, default = "No change"),
    length = lead(Dx) - Dx
  ) %>% 
  select(c(id, Dx, Event, next_event, length, Change_cat)) # %>% 
  #filter(!(next_event %in% c("EndOfFU","Hospitalised", "Hospital discharge")))

data.event_to_event <- data.event_to_event %>% 
#  filter(Event != "Hospital discharge", Event != "Deceased", next_event != "Hospital admission")
 filter(Event != "Hospital admission", next_event != "Hospital discharge")
prop.table(table(data.event_to_event$Event, data.event_to_event$next_event),1)
#Counts periods#################################################################
time_horizon <- 30

data.periods <- data.event_to_event
# Add the number of uneventful time spans within each row.
data.periods$uneventful_ends <- floor((data.periods$length - 1) / time_horizon)
# Add the number of uneventful time spans within each row.
data.periods.ends_no_change <-  data.periods %>% 
  filter(uneventful_ends > 0) %>% 
  mutate(next_event = "No change",
         length = time_horizon,
         n = 1)
data.periods.starts_no_change <-  data.periods %>% 
  filter(uneventful_ends > 0) %>% 
  mutate(Event = "No change",
         Dx = Dx + time_horizon * floor(length / time_horizon),
         length = length - time_horizon * floor(length / time_horizon),
         n = 1)
data.periods.uneventful <-  data.periods %>% 
  filter(uneventful_ends >= 2) %>% 
  mutate(Event = "No change",
         next_event = "No change",
         Dx = Dx + time_horizon,
         length = time_horizon,
         n = uneventful_ends - 1)
data.periods.eventful <-  data.periods %>% 
  filter(uneventful_ends == 0) %>% 
  mutate(n = 1)
data.periods <- data.periods.eventful %>% 
  rbind(data.periods.uneventful) %>% 
  rbind(data.periods.starts_no_change) %>% 
  rbind(data.periods.ends_no_change) %>% 
  arrange(id, Dx) %>% 
  subset(select = -c(uneventful_ends))

#Counts -  Exclude palliative down titration####
data.periods <- data.periods[!(data.periods$id %in% palliative_care_downtitrations 
                               & data.periods$next_event == "Deceased"),]
# Aggregated counts
data.periods.rep <- data.periods %>% 
  select(c(id, Event, next_event, n, Dx, length)) %>%
  uncount(n) #%>% 

data.periods.table <-
  data.periods.rep %>% 
  with(table(Event, next_event))

data.periods.table_cum <-
  data.periods.rep %>% 
  with(table(Event))

prop.table(data.periods.table, 1)

# Z-test
#prop.test(n = c(1327, 1368), x = c(26, 16), alternative = "greater")

#Dose change indications########################################################

data.indications.pivotted <- pivot_longer(data.indications[,c(1,6:72)], !id, names_to = "Indication", values_to = "Value")
data.indications.pivotted <- na.omit(data.indications.pivotted)
data.indications.pivotted$Number <- str_replace(data.indications.pivotted$Indication, "D|Ind", "")
data.indications.pivotted$Column <- str_replace(data.indications.pivotted$Indication, "\\d+", "")
data.indications.pivotted$Column <- str_replace(data.indications.pivotted$Column, "ind", "Ind")
data.indications.pivotted <- pivot_wider(data.indications.pivotted[,c(1,3:5)], c(id, Number), names_from = "Column", values_from = "Value")
data.indications.pivotted$Reason <- data.indications.def$Reason[data.indications.pivotted$Ind]

#Counts per participant################ 

counts_per_participant <- counts %>% count(id, values)

h = hist(counts_per_participant$n[counts_per_participant$values == "Down-titration"])
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, xlab = "Number of down-titrations per participant", main = "Histogram of number of down-titrations per participant", ylab="Percentage", col="#ce1831")

h = hist(counts_per_participant$n[counts_per_participant$values == "Up-titration"], )
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, xlab = "Number of up-titrations per participant", main = "Histogram of number of up-titrations per participant", ylab="Percentage", col="#ce1831")

percentage_no_up_titration = 1 - (sum(counts_per_participant$n[counts_per_participant$values == "Up-titration"] > 0, na.rm = TRUE))/length(unique(counts_per_participant$id));
percentage_no_down_titration = 1 - (sum(counts_per_participant$n[counts_per_participant$values == "Down-titration"] > 0, na.rm = TRUE))/length(unique(counts_per_participant$id));

max(counts_per_participant$n[counts_per_participant$values == "Up-titration"], na.rm = TRUE);
max(counts_per_participant$n[counts_per_participant$values == "Down-titration"], na.rm = TRUE);


#Counts -  Conditional probability tables####

condititonal_probability_table <- with(counts_periods, table(baseline_state, end_state) / rowSums(table(baseline_state, end_state)))

counts_periods_up_titration <- counts_periods[counts_periods$baseline_state == "Up-titration",]

write.csv(table(counts_periods_up_titration$end_state, counts_periods_up_titration$indication), "table_post_uptitration.csv")

counts_periods_down_titration <- counts_periods[counts_periods$baseline_state == "Down-titration",]

write.csv(table(counts_periods_down_titration$end_state, counts_periods_down_titration$indication), "table_post_downtitration.csv")

counts_periods_no_change <- counts_periods[counts_periods$baseline_state == "No change",]

write.csv(table(counts_periods_no_change$end_state, counts_periods_no_change$indication), "table_no_change.csv")

cpt_orthopnea_numHospi <- with(data, table(Orthopnea, NumberHosp_HF) / rowSums(table(Orthopnea, NumberHosp_HF)))

cpt_orthopnea_numHospi <- with(data, table(Orthopnea, NumberHosp_HF) / rowSums(table(Orthopnea, NumberHosp_HF)))

cpt_PND_numHospi <- with(data, table(PND, NumberHosp_HF) / rowSums(table(PND, NumberHosp_HF)))

cpt_Rales_numHospi <- with(data, table(Rales, NumberHosp_HF) / rowSums(table(Rales, NumberHosp_HF)))

#Analysis of determinants of background fluid state: cause of CHF, age and gender###################################
# 

ggplot(data=data, mapping = aes(x = to_factor(Cause_HF), y = Own_congestionscore_BL)) + 
  geom_boxplot() +
  xlab("Cause Heart Failure") +
  ylab("Own congestion score (baseline)") + 
  theme_bw()

ggplot(data=data, mapping = aes(x = to_factor(Cause_HF), y = Framingham_congestionscore_BL)) + 
  geom_boxplot() +
  xlab("Cause Heart Failure") +
  ylab("Framingham congestion score (baseline)") + 
  theme_bw()

# Gender 
data$Gender_cat <- ifelse(data$Gender ==1, "male", "female")
ggplot(data=data, mapping = aes(x = to_factor(Gender_cat), y = Own_congestionscore_BL)) + 
  geom_boxplot() +
  xlab("Gender") +
  ylab("Framingham congestion score (baseline)") + 
  theme_bw()

# Correlation between age and baseline congestion score
plot(data$Age, data$Own_congestionscore_BL, main="Congestion score vs age",
     xlab="Age ", ylab="Congestion score ", pch=19)
abline(lm(data$Own_congestionscore_BL ~ data$Age), col="red")

# Correlation between BNP and baseline congestion score
plot(data$BNP0_ln, data$Own_congestionscore_BL, main="Congestion score vs BNP",
     xlab="BNP (log)", ylab="Congestion score (baseline)", pch=19)
abline(lm(data$Own_congestionscore_BL ~ data$BNP0_ln), col="red")

#Dose change days###############################################################
### 

id.Dx.dose.visit_days <- merge(data.daily_dose, data[, c("id", "V1", "V3", "V6", "V12", "V18")], by = 'id')

dose_change_days <- data.daily_dose[data.daily_dose$Change_cat == 'Up-titration' | data.daily_dose$Change_cat == 'Down-titration' ,]
#Events after visits####

time_horizon <- 30
dose_change_lag <- 7

data.first_dose_change_after_visit <- data.frame(id=data$id, BL = rep(NA, nrow(data)), V1 = rep(NA, nrow(data)), V3 = rep(NA, nrow(data)), V6 = rep(NA, nrow(data)), V12 = rep(NA, nrow(data)), V18 = rep(NA, nrow(data)),
                                           BL_direction = rep(NA, nrow(data)), V1_direction = rep(NA, nrow(data)), V3_direction = rep(NA, nrow(data)), V6_direction = rep(NA, nrow(data)), V12_direction = rep(NA, nrow(data)), V18_direction = rep(NA, nrow(data)))


data.second_dose_change_after_visit <- data.frame(id=data$id, BL = rep(NA, nrow(data)), V1 = rep(NA, nrow(data)), V3 = rep(NA, nrow(data)), V6 = rep(NA, nrow(data)), V12 = rep(NA, nrow(data)), V18 = rep(NA, nrow(data)),
                                                 BL_direction = rep(NA, nrow(data)), V1_direction = rep(NA, nrow(data)), V3_direction = rep(NA, nrow(data)), V6_direction = rep(NA, nrow(data)), V12_direction = rep(NA, nrow(data)), V18_direction = rep(NA, nrow(data)))


data.hospit_CHF_after_visit <- data.frame(id=data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.hospit_renal_after_visit <- data.frame(id=data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.hospit_linked_after_visit <- data.frame(id=data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.hospit_after_visit <- data.frame(id=data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.death_after_visit <- data.frame(id=data$id, death = as.numeric(difftime(data$Dateofdeath, data$RefDate, units = "days")), BL = rep(NA, nrow(data)), V1 = rep(NA, nrow(data)), V3 = rep(NA, nrow(data)), V6 = rep(NA, nrow(data)), V12 = rep(NA, nrow(data)), V18 = rep(NA, nrow(data)))

data.death_after_visit$BL <- ifelse(data$Death == 0 | data.death_after_visit$death > time_horizon, Inf,data.death_after_visit$death)

data.death_after_visit$V1 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V1 |
                                      data.death_after_visit$death > data$V1 + time_horizon, Inf, data.death_after_visit$death - data$V1)
data.death_after_visit$V3 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V3 |
                                      data.death_after_visit$death > data$V3 + time_horizon, Inf, data.death_after_visit$death - data$V3)
data.death_after_visit$V6 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V6 |
                                      data.death_after_visit$death > data$V6 + time_horizon, Inf, data.death_after_visit$death - data$V6)
data.death_after_visit$V12 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V12 |
                                       data.death_after_visit$death > data$V12 + time_horizon, Inf, data.death_after_visit$death - data$V12)
data.death_after_visit$V18 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V18 |
                                       data.death_after_visit$death > data$V18 + time_horizon, Inf, data.death_after_visit$death - data$V18)

data.hospitalisations.merged.stripped <- data.hospitalisations.stripped[!is.na(data.hospitalisations.stripped$Dx.admission),]

for(i in 1:nrow(data))
{
  current_id <- data$id[i];
  
  # Hospitalisation
  data.hospit_after_visit$BL[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$diff_refdate_entrydate[i] & Dx.admission <= data$diff_refdate_entrydate[i] + time_horizon]), default = Inf);
  data.hospit_after_visit$V1[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$V1[data$id == current_id] & Dx.admission <= data$V1[data$id == current_id] + time_horizon] - data$V1[i]), default = Inf);
  data.hospit_after_visit$V3[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$V3[data$id == current_id] & Dx.admission <= data$V3[data$id == current_id] + time_horizon] - data$V3[i]), default = Inf);
  data.hospit_after_visit$V6[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$V6[data$id == current_id] & Dx.admission <= data$V6[data$id == current_id] + time_horizon] - data$V6[i]), default = Inf);
  data.hospit_after_visit$V12[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$V12[data$id == current_id] & Dx.admission <= data$V12[data$id == current_id] + time_horizon] - data$V12[i]), default = Inf);
  data.hospit_after_visit$V18[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & Dx.admission > data$V18[data$id == current_id] & Dx.admission <= data$V18[data$id == current_id] + time_horizon] - data$V18[i]), default = Inf);
  
  # Hospitalisation: Worsening CHF
  data.hospit_CHF_after_visit$BL[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$diff_refdate_entrydate[i] & Dx.admission <= data$diff_refdate_entrydate[i] + time_horizon]), default = Inf);
  data.hospit_CHF_after_visit$V1[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$V1[data$id == current_id] & Dx.admission <= data$V1[data$id == current_id] + time_horizon] - data$V1[i]), default = Inf);
  data.hospit_CHF_after_visit$V3[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$V3[data$id == current_id] & Dx.admission <= data$V3[data$id == current_id] + time_horizon] - data$V3[i]), default = Inf);
  data.hospit_CHF_after_visit$V6[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$V6[data$id == current_id] & Dx.admission <= data$V6[data$id == current_id] + time_horizon] - data$V6[i]), default = Inf);
  data.hospit_CHF_after_visit$V12[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$V12[data$id == current_id] & Dx.admission <= data$V12[data$id == current_id] + time_horizon] - data$V12[i]), default = Inf);
  data.hospit_CHF_after_visit$V18[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.CHF == 1 & Dx.admission > data$V18[data$id == current_id] & Dx.admission <= data$V18[data$id == current_id] + time_horizon] - data$V18[i]), default = Inf);
  
  # Hospitalisation: renal  
  data.hospit_renal_after_visit$BL[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$diff_refdate_entrydate[i] & Dx.admission <= data$diff_refdate_entrydate[i] + time_horizon]), default = Inf);
  data.hospit_renal_after_visit$V1[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$V1[data$id == current_id] & Dx.admission <= data$V1[data$id == current_id] + time_horizon] - data$V1[i]), default = Inf);
  data.hospit_renal_after_visit$V3[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$V3[data$id == current_id] & Dx.admission <= data$V3[data$id == current_id] + time_horizon] - data$V3[i]), default = Inf);
  data.hospit_renal_after_visit$V6[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$V6[data$id == current_id] & Dx.admission <= data$V6[data$id == current_id] + time_horizon] - data$V6[i]), default = Inf);
  data.hospit_renal_after_visit$V12[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$V12[data$id == current_id] & Dx.admission <= data$V12[data$id == current_id] + time_horizon] - data$V12[i]), default = Inf);
  data.hospit_renal_after_visit$V18[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.renal == 1 & Dx.admission > data$V18[data$id == current_id] & Dx.admission <= data$V18[data$id == current_id] + time_horizon] - data$V18[i]), default = Inf);

  # Hospitalisation: linked  
  data.hospit_linked_after_visit$BL[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$diff_refdate_entrydate[i] & Dx.admission <= data$diff_refdate_entrydate[i] + time_horizon]), default = Inf);
  data.hospit_linked_after_visit$V1[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$V1[data$id == current_id] & Dx.admission <= data$V1[data$id == current_id] + time_horizon] - data$V1[i]), default = Inf);
  data.hospit_linked_after_visit$V3[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$V3[data$id == current_id] & Dx.admission <= data$V3[data$id == current_id] + time_horizon] - data$V3[i]), default = Inf);
  data.hospit_linked_after_visit$V6[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$V6[data$id == current_id] & Dx.admission <= data$V6[data$id == current_id] + time_horizon] - data$V6[i]), default = Inf);
  data.hospit_linked_after_visit$V12[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$V12[data$id == current_id] & Dx.admission <= data$V12[data$id == current_id] + time_horizon] - data$V12[i]), default = Inf);
  data.hospit_linked_after_visit$V18[i] <- dplyr::first(with(data.hospitalisations.merged.stripped, Dx.admission[id == current_id & cause.linked == 1 & Dx.admission > data$V18[data$id == current_id] & Dx.admission <= data$V18[data$id == current_id] + time_horizon] - data$V18[i]), default = Inf);

  # Day of first dose change after follow up visit
  data.first_dose_change_after_visit$BL[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$diff_refdate_entrydate[i]  & Dx < min(time_horizon, data.hospit_CHF_after_visit$BL[i], data.hospit_renal_after_visit$BL[i], data.hospit_linked_after_visit$BL[i])]), na.rm = TRUE);
  data.first_dose_change_after_visit$V1[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V1[data$id == current_id]  & Dx < data$V1[data$id == current_id]+ min(time_horizon, data.hospit_CHF_after_visit$V1[i], data.hospit_renal_after_visit$V1[i], data.hospit_linked_after_visit$V1[i])]), na.rm = TRUE);
  data.first_dose_change_after_visit$V3[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V3[data$id == current_id]  & Dx < data$V3[data$id == current_id]+ min(time_horizon, data.hospit_CHF_after_visit$V3[i], data.hospit_renal_after_visit$V3[i], data.hospit_linked_after_visit$V3[i])]), na.rm = TRUE);
  data.first_dose_change_after_visit$V6[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V6[data$id == current_id]  & Dx < data$V6[data$id == current_id]+ min(time_horizon, data.hospit_CHF_after_visit$V6[i], data.hospit_renal_after_visit$V6[i], data.hospit_linked_after_visit$V6[i])]), na.rm = TRUE);
  data.first_dose_change_after_visit$V12[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V12[data$id == current_id]  & Dx < data$V12[data$id == current_id]+ min(time_horizon, data.hospit_CHF_after_visit$V12[i], data.hospit_renal_after_visit$V12[i], data.hospit_linked_after_visit$V12[i])]), na.rm = TRUE);
  data.first_dose_change_after_visit$V18[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V18[data$id == current_id]  & Dx < data$V18[data$id == current_id]+ min(time_horizon, data.hospit_CHF_after_visit$V18[i], data.hospit_renal_after_visit$V18[i], data.hospit_linked_after_visit$V18[i])]), na.rm = TRUE);
  
  # Day of first dose change after follow up visit - Direction of change
  data.first_dose_change_after_visit$BL_direction[i] <- ifelse(is.infinite(data.first_dose_change_after_visit$BL[i]), 'No change', 
                                                               with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$BL[i]])));
  data.first_dose_change_after_visit$V1_direction[i] <- ifelse(is.na(data.first_dose_change_after_visit$V1[i]), NA, ifelse(is.infinite(data.first_dose_change_after_visit$V1[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$V1[i]]))));
  data.first_dose_change_after_visit$V3_direction[i] <- ifelse(is.na(data.first_dose_change_after_visit$V3[i]), NA, ifelse(is.infinite(data.first_dose_change_after_visit$V3[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$V3[i]]))));
  data.first_dose_change_after_visit$V6_direction[i] <- ifelse(is.na(data.first_dose_change_after_visit$V6[i]), NA, ifelse(is.infinite(data.first_dose_change_after_visit$V6[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$V6[i]]))));
  data.first_dose_change_after_visit$V12_direction[i] <- ifelse(is.na(data.first_dose_change_after_visit$V12[i]), NA, ifelse(is.infinite(data.first_dose_change_after_visit$V12[i]), 'No change', 
                                                                                                                             with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$V12[i]]))));
  data.first_dose_change_after_visit$V18_direction[i] <- ifelse(is.na(data.first_dose_change_after_visit$V18[i]), NA, ifelse(is.infinite(data.first_dose_change_after_visit$V18[i]), 'No change', 
                                                                                                                             with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.first_dose_change_after_visit$V18[i]]))));

  # Day of first dose change after follow up visit (lagged by a few days)
  data.second_dose_change_after_visit$BL[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$BL[i]  & Dx < min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$BL[i], data.hospit_renal_after_visit$BL[i], data.hospit_linked_after_visit$BL[i])]), na.rm = TRUE);
  data.second_dose_change_after_visit$V1[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$V1[i]  & Dx < data$V1[data$id == current_id]+ min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$V1[i], data.hospit_renal_after_visit$V1[i], data.hospit_linked_after_visit$V1[i])]), na.rm = TRUE);
  data.second_dose_change_after_visit$V3[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$V3[i]  & Dx < data$V3[data$id == current_id]+ min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$V3[i], data.hospit_renal_after_visit$V3[i], data.hospit_linked_after_visit$V3[i])]), na.rm = TRUE);
  data.second_dose_change_after_visit$V6[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$V6[i]  & Dx < data$V6[data$id == current_id]+ min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$V6[i], data.hospit_renal_after_visit$V6[i], data.hospit_linked_after_visit$V6[i])]), na.rm = TRUE);
  data.second_dose_change_after_visit$V12[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$V12[i]  & Dx < data$V12[data$id == current_id]+ min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$V12[i], data.hospit_renal_after_visit$V12[i], data.hospit_linked_after_visit$V12[i])]), na.rm = TRUE);
  data.second_dose_change_after_visit$V18[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data.first_dose_change_after_visit$V18[i]  & Dx < data$V18[data$id == current_id]+ min(time_horizon + dose_change_lag, data.hospit_CHF_after_visit$V18[i], data.hospit_renal_after_visit$V18[i], data.hospit_linked_after_visit$V18[i])]), na.rm = TRUE);
  
  # Day of first dose change after follow up visit (lagged by a few days) - Direction of change
  data.second_dose_change_after_visit$BL_direction[i] <- ifelse(is.infinite(data.second_dose_change_after_visit$BL[i]), 'No change', 
                                                               with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$BL[i]])));
  data.second_dose_change_after_visit$V1_direction[i] <- ifelse(is.na(data.second_dose_change_after_visit$V1[i]), NA, ifelse(is.infinite(data.second_dose_change_after_visit$V1[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$V1[i]]))));
  data.second_dose_change_after_visit$V3_direction[i] <- ifelse(is.na(data.second_dose_change_after_visit$V3[i]), NA, ifelse(is.infinite(data.second_dose_change_after_visit$V3[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$V3[i]]))));
  data.second_dose_change_after_visit$V6_direction[i] <- ifelse(is.na(data.second_dose_change_after_visit$V6[i]), NA, ifelse(is.infinite(data.second_dose_change_after_visit$V6[i]), 'No change', 
                                                                                                                           with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$V6[i]]))));
  data.second_dose_change_after_visit$V12_direction[i] <- ifelse(is.na(data.second_dose_change_after_visit$V12[i]), NA, ifelse(is.infinite(data.second_dose_change_after_visit$V12[i]), 'No change', 
                                                                                                                             with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$V12[i]]))));
  data.second_dose_change_after_visit$V18_direction[i] <- ifelse(is.na(data.second_dose_change_after_visit$V18[i]), NA, ifelse(is.infinite(data.second_dose_change_after_visit$V18[i]), 'No change', 
                                                                                                                             with(dose_change_days, as.character(Change_cat[id == current_id & Dx == data.second_dose_change_after_visit$V18[i]]))));
  
}

data.first_dose_change_after_visit$V1 <- data.first_dose_change_after_visit$V1 - data$V1
data.first_dose_change_after_visit$V3 <- data.first_dose_change_after_visit$V3 - data$V3
data.first_dose_change_after_visit$V6 <- data.first_dose_change_after_visit$V6 - data$V6
data.first_dose_change_after_visit$V12 <- data.first_dose_change_after_visit$V12 - data$V12
data.first_dose_change_after_visit$V18 <- data.first_dose_change_after_visit$V18 - data$V18


data.second_dose_change_after_visit$V1  <- data.second_dose_change_after_visit$V1 - data$V1
data.second_dose_change_after_visit$V3  <- data.second_dose_change_after_visit$V3 - data$V3
data.second_dose_change_after_visit$V6  <- data.second_dose_change_after_visit$V6 - data$V6
data.second_dose_change_after_visit$V12 <- data.second_dose_change_after_visit$V12 - data$V12
data.second_dose_change_after_visit$V18 <- data.second_dose_change_after_visit$V18 - data$V18

data.first_dose_change_after_visit_lagged     <- data.first_dose_change_after_visit
data.first_dose_change_after_visit_lagged$BL  <- ifelse(data.first_dose_change_after_visit$BL < dose_change_lag, data.second_dose_change_after_visit$BL, data.first_dose_change_after_visit$BL) 
data.first_dose_change_after_visit_lagged$V1  <- ifelse(data.first_dose_change_after_visit$V1 < dose_change_lag, data.second_dose_change_after_visit$V1, data.first_dose_change_after_visit$V1) 
data.first_dose_change_after_visit_lagged$V3  <- ifelse(data.first_dose_change_after_visit$V3 < dose_change_lag, data.second_dose_change_after_visit$V3, data.first_dose_change_after_visit$V3) 
data.first_dose_change_after_visit_lagged$V6  <- ifelse(data.first_dose_change_after_visit$V6 < dose_change_lag, data.second_dose_change_after_visit$V6, data.first_dose_change_after_visit$V6) 
data.first_dose_change_after_visit_lagged$V12 <- ifelse(data.first_dose_change_after_visit$V12 < dose_change_lag, data.second_dose_change_after_visit$V12, data.first_dose_change_after_visit$V12) 
data.first_dose_change_after_visit_lagged$V18 <- ifelse(data.first_dose_change_after_visit$V18 < dose_change_lag, data.second_dose_change_after_visit$V18, data.first_dose_change_after_visit$V18) 

hist(data.first_dose_change_after_visit_lagged$BL, breaks = 31)
hist(data.first_dose_change_after_visit_lagged$V1, breaks = 31)
table(data.first_dose_change_after_visit_lagged$BL)

#Transform the data to feed HUGIN####
# 


## Discretisation functions

discretise_quantiles <- function(data, labels)
{
  n_ranges <- length(labels)
  bin_width <- 1/n_ranges 
  breaks <- quantile(data, probs = seq(from = 0, to = 1, by = bin_width), na.rm = TRUE)
  as.character(cut(data, breaks, labels, include.lowest = TRUE))
}

discretise_width <- function(data, labels)
{
  n_ranges <- length(labels)
  data_min <- min(data, na.rm = TRUE)
  data_max <- max(data, na.rm = TRUE)
  bin_width <- as.integer((data_max - data_min) /  n_ranges)
  breaks <- seq(from = data_min, to = data_max, by = bin_width)
  cut(data, breaks, labels)
}


Fill_Hugin_data <- function()
{
  # Data elements
  # Immutable elements
  pat_id <- rep(data$id, 6)
  Gender <- rep(data$Gender_cat, 6)
  Cause_HF <- rep(to_character(data$Cause_HF), 6)
  
                  
  # Initialise dataframe
  data.h <- data.frame(pat_id, Gender, Cause_HF)
                  
  # Visit data
  # fluid_level <- with(data.congention_score, c(CCS, CCS1, CCS3, CCS6, CCS12, CCS18))
  
  # fluid_level <- with(data, c(Own_congestionscore_BL,
  #                             Own_congestionscore_1,
  #                             Own_congestionscore_3,
  #                             Own_congestionscore_6,
  #                             Own_congestionscore_12,
  #                             Own_congestionscore_18))
  # fluid_level <- with(data.new_congestion_scores,
  #                     c(Clinical_congestion_score_BL,
  #                       Clinical_congestion_score_1,
  #                       Clinical_congestion_score_3,
  #                       Clinical_congestion_score_6,
  #                       Clinical_congestion_score_12,
  #                       Clinical_congestion_score_18))
  
  fluid_level <- with(data.new_congestion_scores,
                      c(Combined_congestion_score_BL,
                        Combined_congestion_score_1,
                        Combined_congestion_score_3,
                        Combined_congestion_score_6,
                        Combined_congestion_score_12,
                        Combined_congestion_score_18))
  
  data.h$Fluid_level_cont <- fluid_level
  
  data.h$Fluid_level <- discretise_quantiles(fluid_level, labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  # data.h$Fluid_level <- as.character(cut(fluid_level, c(0,1,2,3,4,7), labels = c("Very Low", "Low", "Average", "High", "Very High")), include.lowest = TRUE) 
  
  data.h$Heart_rate <- discretise_quantiles(with(data, c(HR, HR1, HR3, HR6, HR12, HR18)),
                                            labels = c("Very Low", "Low", "Average", "High", "Very High"))
  data.h$SBP <- discretise_quantiles(with(data, c(BPsyst, BPsyst1, BPsyst3, BPsyst6, BPsyst12, BPsyst18)), 
                                 labels = c("Very Low", "Low", "Average", "High", "Very High"))
  data.h$DBP <- discretise_quantiles(with(data, c(BPdiast, BPdiast1, BPdiast3, BPdiast6, BPdiast12, BPdiast18)),
                                      labels = c("Very Low", "Low", "Average", "High", "Very High"))
  data.h$Fatigue <- recode(dplyr::na_if(with(data.symptoms, c(Fatigue_BL, Fatigue1, Fatigue3, Fatigue6, Fatigue12, Fatigue18)),9), 
                       '0'= 'No', '1'='Mild', '2'='Moderate', '3'='Severe')
  data.h$NYHA <- recode(dplyr::na_if(with(data.symptoms, c(NYHA_class_BL, NYHA1_class, NYHA3_class, NYHA6_class, NYHA12_class, NYHA18_class)),9),
                    '1'= 'I', '2'='II', '3'='III', '4'='IV')
  data.h$Orthopnea <- recode(dplyr::na_if(with(data.symptoms, c(Orthopnea_BL, Orthopnea1, Orthopnea3, Orthopnea6, Orthopnea12, Orthopnea18)), 9),
                         '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$PND <- recode(dplyr::na_if(with(data.symptoms, c(PND_BL, PND1, PND3, PND6, PND12, PND18)), 9),
                       '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Nocturia <- recode(dplyr::na_if(with(data.symptoms, c(Nocturia_BL, Nocturia1, Nocturia3, Nocturia6, Nocturia12, Nocturia18)), 9),
                            '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2')
  data.h$Dry_cough <- recode(dplyr::na_if(with(data.symptoms, c(Cough_BL, Cough1, Cough3, Cough6, Cough12, Cough18)), 9),
                             '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Constipation <- recode(dplyr::na_if(with(data.symptoms, c(Constipation_BL, Constip1, Constip3, Constip6, Constip12, Constip18)), 9),
                                '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Reduced_memory <- recode(dplyr::na_if(with(data.symptoms, c(RedMemory_BL, RedMemory1, RedMemory3, RedMemory6, RedMemory12, RedMemory18)), 9),
                                  '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Dizziness <- recode(dplyr::na_if(with(data.symptoms, c(Dizziness_BL, Dizziness1, Dizziness3, Dizziness6, Dizziness12, Dizziness18)), 9), 
                                          '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Orthostasis <- recode(dplyr::na_if(with(data.symptoms, c(Orthostasis_BL, Orthostasis1, Orthostasis3, Orthostasis6, Orthostasis12, Orthostasis12)), 9),
                                            '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Syncope <- recode(dplyr::na_if(with(data.symptoms, c(Syncope_BL, Syncope1, Syncope3, Syncope6, Syncope12, Syncope18)), 9),
                           '0' = 'no', '1' = 'yes')
  data.h$Exercise_intolerance <- recode(dplyr::na_if(with(data.symptoms, c(ExIntol_BL, ExIntol1, ExIntol3, ExIntol6, ExIntol12, ExIntol18)), 9),
                                                     '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Oedema <- recode(dplyr::na_if(with(data.symptoms, c(EdemaHist_BL, EdemaHist1, EdemaHist3, EdemaHist6, EdemaHist12, EdemaHist18)), 9),
                                    '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  
  data.h$CRP_cont <- with(data, c(hsCRP_BL, hsCRP_V1, hsCRP_V3, hsCRP_V6, hsCRP_V12, hsCRP_V18))
  data.h$CRP <- discretise_quantiles(data.h$CRP_cont, labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  data.h$IL6_cont <- with(data, c(IL6_BL, IL6_V1, IL6_V3, IL6_V6, IL6_V12, IL6_V18))
  data.h$IL6 <- discretise_quantiles(data.h$IL6_cont, labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  data.h$BNP_cont <- with(data, c(BNP0_ln, BNP1_ln, BNP3_ln, BNP6_ln, BNP12_ln, BNP18_ln))
  data.h$BNP <- discretise_quantiles(data.h$BNP_cont, labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  data.h$Urea_creatinine_ratio_cont <- with(data, c(Urea, Urea_V1, Urea_V3, Urea6, Urea12, Urea18) / c(Creatinine, Crea1, Crea3, Crea6, Crea12, Crea18))
  data.h$Urea_creatinine_ratio <- discretise_quantiles(data.h$Urea_creatinine_ratio_cont,  labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  data.h$Cystatin_C_cont <-with(data, c(CysC_BL, CysC_V1, CysC_V3, CysC_V6, CysC_V12, CysC_V18))
  data.h$Cystatin_C <- discretise_quantiles(data.h$Cystatin_C_cont,
                                            labels = c("Very Low", "Low", "Average", "High", "Very High"))
  data.h$GFR_cont <- with(data, c(eGFR_MDRD_BL, eGFR_MDRD_1, eGFR_MDRD_3, eGFR_MDRD_6, eGFR_MDRD_12, eGFR_MDRD_18))
  data.h$GFR <- discretise_quantiles(data.h$GFR, labels = c("Very Low", "Low", "Average", "High", "Very High"))
  
  data.h$Rales <- recode(with(data, c(Rales, Rales1, Rales3, Rales6, Rales12, Rales18)),
                         '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$'Jugular veins' <- recode(with(data, c(Jugularvein, JV1, JV3, JV6, JV12, JV18)),
                                 '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Hepatomegaly <- recode(with(data, c(Hepatomegaly, Hepatomegaly1, Hepatomegaly3, Hepatomegaly6, Hepatomegaly12, Hepatomegaly18)),
                                '0' = 'absent', '1' = 'present')
  data.h$S3 <- recode(with(data, c(S3, S3_1, S3_3, S3_6, S3_12, S3_18)),  '0' = 'absent', '1' = 'present')
  data.h$S4 <- recode(data$S4,  '0' = 'absent', '1' = 'present')
  data.h$'Systolic murmur' <- recode(data$SystmurmurMR, '0' = 'Grade 0', '1' = 'Grade 1', '2' = 'Grade 2', '3' = 'Grade 3')
  data.h$Bronchial_obstruction <- recode(data$Bronchialobstruction, '0' = 'absent', '1' = 'present')
  
  data.h$Intervention <- recode(with(data, c(Diuretics_recom_BL, Diuretics_recom_1, Diuretics_recom_3, Diuretics_recom_6, Diuretics_recom_12, rep(NA, length(Diuretics_recom_BL)))),
                                '-1' = 'Down-titration', '0' = 'No change', '1' = 'Up-titration')
  data.h$Loop_dose <- with(data, c(Loop_dose_BL, Loop_dose_V1, Loop_dose_V3, Loop_dose_V6, Loop_dose_V12, Loop_dose_V18))
  data.h$Dose_adjustment <- with(data.first_dose_change_after_visit, c(BL_direction, V1_direction, V3_direction, V6_direction, V12_direction, V18_direction))
  
  data.h$Death <- with(
    data.death_after_visit, ifelse(c(BL, V1, V3, V6, V12, V18) <= time_horizon,'yes', 'no'))
  
  data.h$Hospitalisation_CHF <- with(data.hospit_CHF_after_visit, ifelse(c(BL, V1, V3, V6, V12, V18) <= time_horizon,'yes', 'no'))
  data.h$Hospitalisation_Renal_Failure <- with(data.hospit_renal_after_visit, ifelse(c(BL, V1, V3, V6, V12, V18) <= time_horizon,'yes', 'no'))
  data.h$Hospitalisation_Linked <- with(data.hospit_linked_after_visit, ifelse(c(BL, V1, V3, V6, V12, V18) <= time_horizon,'yes', 'no'))
  
  # Remove censored datapoints
  #data.h <- data.h[!is.na(with(data, c(rep(0, nrow(data)), V1, V3, V6, V12, V18))),]
  
  # Mutate missing values to Hugin format
  #data.h <- mutate_all(data.h, ~if_else(is.na(.), '<EMPTY>', .)) 
  
  #write.csv(data.h, "Hugin-TIME-CHF-visits-OwnCS.dat", row.names = FALSE)
  data.h
}

data.h <- Fill_Hugin_data()

# Add periods (yes, I know this duplicates some data)
data.h.periods <- data.frame(Intervention = counts_periods$baseline_state, Dose_adjustment = counts_periods$end_state,
                             Hospitalisation_CHF = ifelse(counts_periods$end_state == 'Hospitalisation: CHF', 'yes', 'no'),
                             Death = ifelse(counts_periods$end_state == 'Deceased', 'yes', 'no'))
data.h.periods <- data.h.periods[!is.na(data.h.periods$Dose_adjustment),]
# Move those who were hospitalised, then died, to the same row
data.h.periods$Death[which(data.h.periods$Intervention == 'Hospitalisation: CHF')-1] <- data.h.periods$Death[which(data.h.periods$Intervention == 'Hospitalisation: CHF')]
data.h.periods<- data.h.periods[data.h.periods$Intervention != 'Hospitalisation: CHF',]
data.h.periods$Dose_adjustment[data.h.periods$Dose_adjustment == 'Hospitalisation: CHF' | data.h.periods$Dose_adjustment == 'Deceased' ] <- NA

#data.h <- dplyr::bind_rows(data.h, data.h.periods)
# Mutate missing values to Hugin format
data.h.periods <- mutate_all(data.h.periods, ~if_else(is.na(.), '<EMPTY>', .)) 
write.csv(data.h.periods, "Hugin-TIME-CHF-periods.dat", row.names = FALSE)


#Which congestion score is more predictive?#####################################


data.CCI <- data.frame(hospitalisation =  to_factor(with(data.hospit_CHF_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))), 
                    congestion_score =  with(data.congention_score, c(CCS, CCS1, CCS3, CCS6, CCS12, CCS18)),
                    death = with(data.death_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes')))
ggplot(data=data.CCI, mapping = aes(x = hospitalisation, y = congestion_score )) + 
  geom_boxplot() +
  xlab("Hospitalisation") +
  ylab("Congestion score (CCI)") + 
  theme_bw()

ggplot(data=data.CCI, mapping = aes(x = death, y = congestion_score )) + 
  geom_boxplot() +
  xlab("Death") +
  ylab("Congestion score (CCI)") + 
  theme_bw()

data.Own <- data.frame(hospitalisation =  to_factor(with(data.hospit_CHF_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))), 
                     congestion_score =  with(data, c(Own_congestionscore_BL, 
                                                      Own_congestionscore_1,
                                                      Own_congestionscore_3,
                                                      Own_congestionscore_6,
                                                      Own_congestionscore_12,
                                                      Own_congestionscore_18)),
                     death = with(data.death_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes')))
ggplot(data=data.Own, mapping = aes(x = hospitalisation, y = congestion_score )) + 
  geom_boxplot() +
  xlab("Hospitalisation") +
  ylab("Congestion score (Own)") + 
  theme_bw()

ggplot(data=data.Own, mapping = aes(x = death, y = congestion_score )) + 
  geom_boxplot() +
  xlab("Death") +
  ylab("Congestion score (Own)") + 
  theme_bw()

#Why is no-change linked to lower hospitalisation?####

table_Int_Dose_adj <- table(data.h$Intervention, data.h$Dose_adjustment, data.h$Fluid_level) # /rowSums(table(data.h$Intervention, data.h$Dose_adjustment))

write.csv(table_Int_Dose_adj, "table_Int_Dose_adj.csv", row.names = FALSE)

table(data.h$Fluid_level, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Fluid_level, data.h$Hospitalisation_CHF))

table_FL_Int_Hospit <-table( data.h$Intervention, data.h$Hospitalisation_CHF, data.h$Fluid_level)

write.csv(table_FL_Int_Hospit, "table_FL_Int_Hospit.csv", row.names = FALSE)

write.csv(table(data.h$Fluid_level, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Fluid_level, data.h$Hospitalisation_CHF)),"table_FL_Hospit.csv", row.names = FALSE)
# Is it maybe low sample size?
#
# Should we learn a logistic regression model first?

logr_hosp <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ Fluid_level_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death <- glm(ifelse(Death == 'yes', 1,0) ~ Fluid_level_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')


loop_dose_tibble <- data.h %>%
  group_by(Fluid_level, Intervention) %>%
  summarise(Dose_mean = mean(Loop_dose, na.rm = TRUE), 
            Dose_sd = sd(Loop_dose, na.rm = TRUE),
            Dose_se = sd(Loop_dose, na.rm = TRUE) / sqrt(n()),
            Count = n())

wilcox.test(na.omit(data.h$Loop_dose[data.h$Fluid_level == 'Very High' & data.h$Intervention == 'No change']),
            na.omit(data.h$Loop_dose[data.h$Fluid_level == 'Very High' & data.h$Intervention == 'Up-titration']), paired = FALSE)

loop_dose_tibble_full <- na.omit(loop_dose_tibble)

write.csv(loop_dose_tibble_full, 'loop_dose_change.csv')

# Is the baseline dose different between the groups that had no change and uptitration?
linr_dose <- glm(Loop_dose ~ Fluid_level_cont + Intervention, data.h[data.h$Intervention == 'No change' | data.h$Intervention == 'Up-titration',], na.action = na.omit, family = gaussian)

# What happens if we add the baseline dose to the regression?
logr_hospi_bl_dose <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ Fluid_level_cont + Loop_dose + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_bl_dose <- glm(ifelse(Death == 'yes', 1,0) ~ Fluid_level_cont + Loop_dose + Intervention, data.h, na.action = na.omit, family = 'binomial')

summary(logr_hospi_bl_dose)
summary(logr_death_bl_dose)

sum(is.na(data.h$Loop_dose))

#Which symptoms are more predictive of hospitalisation?########################

# NYHA
table(data.h$NYHA, data.h$Hospitalisation_CHF)/rowSums(table(data.h$NYHA, data.h$Hospitalisation_CHF))
table(data.h$NYHA, data.h$Death)/rowSums(table(data.h$NYHA, data.h$Death))

# Fatigue
table(data.h$Fatigue, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Fatigue, data.h$Hospitalisation_CHF))
table(data.h$Fatigue, data.h$Death)/rowSums(table(data.h$Fatigue, data.h$Death))

# Orthopnea
table(data.h$Orthopnea, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Orthopnea, data.h$Hospitalisation_CHF))
table(data.h$Orthopnea, data.h$Death)/rowSums(table(data.h$Orthopnea, data.h$Death))

# PND
table(data.h$PND, data.h$Hospitalisation_CHF)#/rowSums(table(data.h$PND, data.h$Hospitalisation_CHF))
table(data.h$PND, data.h$Death)/rowSums(table(data.h$PND, data.h$Death))

# Nocturia
table(data.h$Nocturia, data.h$Hospitalisation_CHF)#/rowSums(table(data.h$Nocturia, data.h$Hospitalisation_CHF))
table(data.h$Nocturia, data.h$Death)/rowSums(table(data.h$Nocturia, data.h$Death))

# Dry cough
table(data.h$Dry_cough, data.h$Hospitalisation_CHF)#/rowSums(table(data.h$Dry_cough, data.h$Hospitalisation_CHF))
table(data.h$Dry_cough, data.h$Death)/rowSums(table(data.h$Dry_cough, data.h$Death))

# Orthostasis
table(data.h$Orthostasis, data.h$Hospitalisation_CHF)#/rowSums(table(data.h$Orthostasis, data.h$Hospitalisation_CHF))
table(data.h$Orthostasis, data.h$Death)/rowSums(table(data.h$Orthostasis, data.h$Death))

# Dizziness
table(data.h$Dizziness, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Dizziness, data.h$Hospitalisation_CHF))
table(data.h$Dizziness, data.h$Death)/rowSums(table(data.h$Dizziness, data.h$Death))

# Syncope
table(data.h$Syncope, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Syncope, data.h$Hospitalisation_CHF))
table(data.h$Syncope, data.h$Death)/rowSums(table(data.h$Syncope, data.h$Death))

# Constipation
table(data.h$Constipation, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Constipation, data.h$Hospitalisation_CHF))
table(data.h$Constipation, data.h$Death)/rowSums(table(data.h$Constipation, data.h$Death))

# Exercise intolerance
table(data.h$Exercise_intolerance, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Exercise_intolerance, data.h$Hospitalisation_CHF))
table(data.h$Exercise_intolerance, data.h$Death)/rowSums(table(data.h$Exercise_intolerance, data.h$Death))

# Reduced memory
table(data.h$Reduced_memory, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Reduced_memory, data.h$Hospitalisation_CHF))
table(data.h$Reduced_memory, data.h$Death)/rowSums(table(data.h$Reduced_memory, data.h$Death))

#Are there any biomarkers where the up-titration acts as expected in terms of outcomes####


# CRP
logr_hosp_CRP <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ CRP_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_CRP <- glm(ifelse(Death == 'yes', 1,0) ~ CRP_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_CRP)
summary(logr_death_CRP)

# IL6
logr_hosp_IL6 <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ IL6_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_IL6)
logr_death_IL6 <- glm(ifelse(Death == 'yes', 1,0) ~ IL6_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_death_IL6)

# BNP
logr_hosp_BNP <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ BNP_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_BNP <- glm(ifelse(Death == 'yes', 1,0) ~ BNP_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_BNP)
summary(logr_death_BNP)

# Urea-creatinine ratio
logr_hosp_Urea_creatinine_ratio <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ Urea_creatinine_ratio_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_Urea_creatinine_ratio <- glm(ifelse(Death == 'yes', 1,0) ~ Urea_creatinine_ratio_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_Urea_creatinine_ratio)
summary(logr_death_Urea_creatinine_ratio)

# Cystatin C
logr_hosp_Cystatin_C <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ Cystatin_C_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_Cystatin_C <- glm(ifelse(Death == 'yes', 1,0) ~ Cystatin_C_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_Cystatin_C)
summary(logr_death_Cystatin_C)

# GFR
logr_hosp_GFR <- glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ GFR_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
logr_death_GFR <- glm(ifelse(Death == 'yes', 1,0) ~ GFR_cont + Intervention, data.h, na.action = na.omit, family = 'binomial')
summary(logr_hosp_GFR)
summary(logr_death_GFR)


# Relationship between dose changes and symptoms / congestion scores (baseline vs month 1) ################################################################################

## Edema
tibble.edema_BL1_by_intervention <- data.frame(intervention = data$Diuretics_recom_BL, edema_1 = data$Edema1, edema_bl = data$Edema) %>%
  group_by(intervention) %>%
  summarise(bl_mean = mean(edema_bl, na.rm = TRUE), 
            m1_mean = mean(edema_1, na.rm = TRUE),
            diff_mean = mean(edema_1 - edema_bl, na.rm = TRUE),
            n = n())
write.csv(tibble.edema_BL1_by_intervention, "tibble.edema_BL1_by_intervention.csv", row.names = FALSE)
tibble.edemahist_BL1_by_intervention <- data.frame(intervention = data$Diuretics_recom_BL, edema_1 = data$EdemaHist1, edema_bl = data$EdemaHistory) %>%
  group_by(intervention) %>%
  summarise(bl_mean = mean(edema_bl, na.rm = TRUE), 
            m1_mean = mean(edema_1, na.rm = TRUE),
            diff_mean = mean(edema_1 - edema_bl, na.rm = TRUE),
            n = n())
write.csv(tibble.edemahist_BL1_by_intervention, "tibble.edemahist_BL1_by_intervention.csv", row.names = FALSE)

# ggplot2.scatterplot(data=data, xName='Edema',yName='Edema1', 
#                     groupName='Diuretics_recom_BL', size=3,
#                     backgroundColor="white",
#                     groupColors=c('#999999','#E69F00', '#56B4E9')) 
# 
# ggplot(data.frame(table(data$Edema, data$Edema1, data$Diuretics_recom_BL)), aes(x = Var1, y = Var2,
#                       size = Freq,
#                       color = Var3))+
#             geom_point(alpha = 0.7)+
#             scale_size(range = c(0.1, 10), name = "N")

ggplot(with(data, na.omit(data.frame(Edema,Edema_M1 =Edema1, Dose_Change = recode(Diuretics_recom_BL, '-1' = 'Down-titration', '0' = 'No change', '1' = 'Up-titration'))))) + 
  geom_histogram(aes(x=Edema_M1,y=..density.., fill=interaction(Edema,Dose_Change)),color="grey70", bins=4)+
  facet_grid(Edema~Dose_Change)+
  theme(legend.position = "none")

# Same but in a table
write.csv(prop.table(with(data,(table(Diuretics_recom_BL, Edema1, Edema))), margin = c("Diuretics_recom_BL", "Edema")), "edema_vs_dose_change.csv", row.names = FALSE)

# Linear regression
summary(glm(Edema1 ~ Loop_dose_BL1 + Edema, data = data))
summary(glm(Edema1 ~ Diuretics_recom_BL + Edema, data = data))
# The bigger the baseline edema, the bigger the drop
summary(glm((Edema1 - Edema) ~ Edema, data = data))

# What if we include baseline dose
summary(glm(data$Edema1 ~ data$Loop_dose_BL + data$Diuretics_recom_BL + data$Edema))
summary(glm(data$Edema1 ~ data$Loop_dose_BL + data$Loop_dose_BL1 + data$Edema))

# Logistic regression
summary(glm(ifelse(Edema1 == 0, 1, 0) ~ Diuretics_recom_BL + as.factor(Edema), data = data, family = "binomial"))
summary(glm(ifelse(Edema1 < 2, 1, 0) ~ Diuretics_recom_BL + as.factor(Edema), data = data, family = "binomial"))
summary(glm(ifelse(Edema1 < 3, 1, 0) ~ Diuretics_recom_BL + as.factor(Edema), data = data, family = "binomial"))
# Now with baseline dose
summary(glm(ifelse(Edema1 == 0, 1, 0) ~ Loop_dose + Loop_dose_BL + as.factor(Edema), data = data, family = "binomial"))
summary(glm(ifelse(Edema1 < 2, 1, 0) ~ Diuretics_recom_BL + Loop_dose_BL + as.factor(Edema), data = data, family = "binomial"))
summary(glm(ifelse(Edema1 < 3, 1, 0) ~ Diuretics_recom_BL + Loop_dose_BL + as.factor(Edema), data = data, family = "binomial"))


## Orthopnea

# Simple Table 
with(data,(table(Orthopnea, Diuretics_recom_BL)))

# Plot of distributions
ggplot(with(data, na.omit(data.frame(Orthopnea,Orthopnea_M1 =Orthopnea1, Dose_Change = recode(Diuretics_recom_BL, '-1' = 'Down-titration', '0' = 'No change', '1' = 'Up-titration'))))) + 
  geom_histogram(aes(x=Orthopnea_M1,y=..density.., fill=interaction(Orthopnea,Dose_Change)),color="grey70", bins=4)+
  facet_grid(Orthopnea~Dose_Change)+
  theme(legend.position = "none")

# Tibble
tibble.orthopnea_BL1_by_intervention <- data.frame(intervention = data$Diuretics_recom_BL, ortho_1 = data$Orthopnea1, ortho_bl = data$Orthopnea) %>%
  group_by(intervention) %>%
  summarise(bl_mean = mean(ortho_bl, na.rm = TRUE), 
            m1_mean = mean(ortho_1, na.rm = TRUE),
            diff_mean = mean(ortho_1 - ortho_bl, na.rm = TRUE),
            n = n())
write.csv(tibble.orthopnea_BL1_by_intervention, "tibble.orthopnea_BL1_by_intervention.csv", row.names = FALSE)

# Linear regression
summary(glm(Orthopnea1 ~ Diuretics_recom_BL + Orthopnea, data = data))
summary(glm(Orthopnea1 ~ Diuretics_recom_BL + Orthopnea + Own_congestionscore_BL, data = data))

# Logistic regressions
summary(glm(ifelse(Orthopnea1 == 0, 1, 0) ~ Diuretics_recom_BL + as.factor(Orthopnea), data = data, family = "binomial"))
summary(glm(ifelse(Orthopnea1 < 2, 1, 0) ~ Diuretics_recom_BL + as.factor(Orthopnea), data = data, family = "binomial"))
summary(glm(ifelse(Orthopnea1 < 3, 1, 0) ~ Diuretics_recom_BL + as.factor(Orthopnea), data = data, family = "binomial"))

## Congestion scores
summary(glm(data$Own_congestionscore_1 ~ data$Diuretics_recom_BL + data$Own_congestionscore_BL))

write.csv(prop.table(with(data,(table(Diuretics_recom_BL, Orthopnea1, Orthopnea))), margin = c("Diuretics_recom_BL", "Orthopnea")), "orthopnea_vs_dose_change.csv", row.names = FALSE)

# Linear regression: Congestion score vs Diuretics_recom_BL
summary(with(data, glm(Own_congestionscore_1 ~ Loop_dose_BL1 + Own_congestionscore_BL)))
summary(with(data, glm(Own_congestionscore_1 ~ Diuretics_recom_BL + Own_congestionscore_BL)))

# Linear regression: Congestion score vs Diuretics_recom_BL
summary(glm(data.new_congestion_scores$Combined_congestion_score_1 ~ data$Diuretics_recom_BL + data.new_congestion_scores$Combined_congestion_score_BL))
summary(glm(data.new_congestion_scores$Clinical_congestion_score_1 ~ data$Diuretics_recom_BL + data.new_congestion_scores$Clinical_congestion_score_BL))
summary(glm(data.congention_score$CCS1 ~ data$Diuretics_recom_BL + data.congention_score$CCS))
summary(glm(data$Own_congestionscore_1 ~ data$Diuretics_recom_BL + data$Own_congestionscore_BL))

# Linear regression: Congestion score vs Loop_dose_BL1
summary(glm(data.new_congestion_scores$Combined_congestion_score_1 ~ data$Loop_dose_BL1 + data.new_congestion_scores$Combined_congestion_score_BL))
summary(glm(data.new_congestion_scores$Clinical_congestion_score_1 ~ data$Loop_dose_BL1 + data.new_congestion_scores$Clinical_congestion_score_BL))
summary(glm(data.congention_score$CCS1 ~  data$Loop_dose_BL1 + data.congention_score$CCS))
summary(glm(data$Own_congestionscore_1 ~ data$Loop_dose_BL1 + data$Own_congestionscore_BL))

# Linear regression: Congestion score vs Loop_dose_BL1 (with current dose)
summary(glm(data.new_congestion_scores$Combined_congestion_score_1 ~ data$Loop_dose_BL + data$Loop_dose_BL1 + data.new_congestion_scores$Combined_congestion_score_BL))
summary(glm(data.new_congestion_scores$Clinical_congestion_score_1 ~ data$Loop_dose_BL + data$Loop_dose_BL1 + data.new_congestion_scores$Clinical_congestion_score_BL))
summary(glm(data.congention_score$CCS1 ~ data$Loop_dose_BL + data$Loop_dose_BL1 + data.congention_score$CCS))
summary(glm(data$Own_congestionscore_1 ~ data$Loop_dose_BL + data$Loop_dose_BL1 + data$Own_congestionscore_BL))
# What if this is diuretic resistance? What if we use the half  patients that have the lowest baseline dose?
summary(glm(Own_congestionscore_1 ~ Loop_dose_BL + Loop_dose_BL1 + Own_congestionscore_BL, data = data[data$Loop_dose_BL <= 80,]))
summary(glm(Own_congestionscore_1 ~ Loop_dose_BL + Diuretics_recom_BL + Own_congestionscore_BL, data = data[data$Loop_dose_BL <= 80,]))


# Same but with tibbles
congest_score_by_intervention_tibble_combined <- data.frame(intervention = data$Diuretics_recom_BL, cs_1 = data.new_congestion_scores$Combined_congestion_score_1, cs_bl = data.new_congestion_scores$Combined_congestion_score_BL) %>%
  group_by(intervention) %>%
  summarise(cs_bl_mean = mean(cs_bl, na.rm = TRUE), 
            cs_1_mean = mean(cs_1, na.rm = TRUE),
            cs_diff_mean = mean(cs_1 - cs_bl, na.rm = TRUE),
            n = n())
write.csv(congest_score_by_intervention_tibble_combined, "congest_score_by_intervention_tibble_combined.csv", row.names = FALSE)
  
congest_score_by_intervention_tibble_clinical <- data.frame(intervention = data$Diuretics_recom_BL, cs_1 = data.new_congestion_scores$Clinical_congestion_score_1, cs_bl = data.new_congestion_scores$Clinical_congestion_score_BL) %>%
  group_by(intervention) %>%
  summarise(cs_bl_mean = mean(cs_bl, na.rm = TRUE), 
            cs_1_mean = mean(cs_1, na.rm = TRUE),
            cs_diff_mean = mean(cs_1 - cs_bl, na.rm = TRUE),
            n = n())
write.csv(congest_score_by_intervention_tibble_clinical, "congest_score_by_intervention_tibble_clinical.csv", row.names = FALSE)

congest_score_by_intervention_tibble_own <- data.frame(intervention = data$Diuretics_recom_BL, cs_1 = data$Own_congestionscore_1, cs_bl = data$Own_congestionscore_BL) %>%
  group_by(intervention) %>%
  summarise(cs_bl_mean = mean(cs_bl, na.rm = TRUE), 
            cs_1_mean = mean(cs_1, na.rm = TRUE),
            cs_diff_mean = mean(cs_1 - cs_bl, na.rm = TRUE),
            n = n())
write.csv(congest_score_by_intervention_tibble_own, "congest_score_by_intervention_tibble_own.csv", row.names = FALSE)

congest_score_by_intervention_tibble_CCI <- data.frame(intervention = data$Diuretics_recom_BL, cs_1 = data.congention_score$CCS1, cs_bl = data.congention_score$CCS) %>%
  group_by(intervention) %>%
  summarise(cs_bl_mean = mean(cs_bl, na.rm = TRUE), 
            cs_1_mean = mean(cs_1, na.rm = TRUE),
            cs_diff_mean = mean(cs_1 - cs_bl, na.rm = TRUE),
            n = n())
write.csv(congest_score_by_intervention_tibble_CCI, "congest_score_by_intervention_tibble_cci.csv", row.names = FALSE)

## BNP

## Orthopnea

# Tibble
tibble.BNP_BL1_by_intervention <- data.frame(intervention = data$Diuretics_recom_BL, bnp_1 = data.biomarkers$BNP1, bnp_bl = data.biomarkers$NBNP) %>%
  group_by(intervention) %>%
  summarise(bl_mean = mean(bnp_bl, na.rm = TRUE), 
            m1_mean = mean(bnp_1, na.rm = TRUE),
            diff_mean = mean(bnp_1 - bnp_bl, na.rm = TRUE),
            n = n())
write.csv(tibble.BNP_BL1_by_intervention, "tibble.BNP_BL1_by_intervention.csv", row.names = FALSE)

# Linear regression
summary(glm(data.biomarkers$BNP1 ~ data$Diuretics_recom_BL + data.biomarkers$NBNP))
summary(glm(data.biomarkers$BNP1 ~ data$Loop_dose_BL1 + data.biomarkers$NBNP))
summary(glm(data.biomarkers$BNP1 ~ data$Diuretics_recom_BL + data.biomarkers$NBNP + data$Own_congestionscore_BL))
summary(glm(data.biomarkers$BNP1 ~ data$Loop_dose_BL1 + data.biomarkers$NBNP + data$Own_congestionscore_BL))
summary(glm(data.biomarkers$BNP1 ~ data$Diuretics_recom_BL + data.biomarkers$NBNP + data$Loop_dose_BL))
summary(glm(data.biomarkers$BNP1 ~ data$Loop_dose_BL1 + data.biomarkers$NBNP + data$Loop_dose_BL))
summary(glm(data.biomarkers$BNP1 ~ data$Loop_dose_BL1 + data.biomarkers$NBNP + data$Loop_dose_BL + data$Own_congestionscore_BL))

# Linear regression - log
summary(glm(log(data.biomarkers$BNP1) ~ data$Diuretics_recom_BL + log(data.biomarkers$NBNP)))
summary(glm(log(data.biomarkers$BNP1) ~ data$Diuretics_recom_BL + log(data.biomarkers$NBNP) + data$Own_congestionscore_BL))
summary(glm(log(data.biomarkers$BNP1) ~ data$Loop_dose_BL1 + log(data.biomarkers$NBNP) + data$Own_congestionscore_BL))
summary(glm(log(data.biomarkers$BNP1) ~ data$Diuretics_recom_BL + log(data.biomarkers$NBNP) + data$Loop_dose_BL))
summary(glm(log(data.biomarkers$BNP1) ~ data$Loop_dose_BL1 + log(data.biomarkers$NBNP) + data$Loop_dose_BL))

#How accurate are the Diuretics_recom_XX variables?#############################
tibble_dose_V1 <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_V1_dose = dose[Dx == (V1-1)], 
                post_V1_dose = dose[Dx == V1],
                post_V1_3_dose = dose[Dx == (V1 + 3)], 
                post_V1_5_dose = dose[Dx == (V1 + 5)])),
  data[,c("id", "Diuretics_recom_1")], by = "id")

# Only 20% of the cases where the Diuretics_recom_1 == 1, there is an up titration the day of the visit
with(tibble_dose_V1, sum(post_V1_dose - pre_V1_dose > 0 & Diuretics_recom_1 == 1, na.rm = TRUE) / 
       sum(Diuretics_recom_1 == 1, na.rm = TRUE))
with(tibble_dose_V1, sum(post_V1_3_dose - pre_V1_dose > 0 & Diuretics_recom_1 != 1, na.rm = TRUE)) 
with(tibble_dose_V1, sum(post_V1_3_dose - pre_V1_dose <= 0 & post_V1_5_dose - pre_V1_dose > 0 & Diuretics_recom_1 == 1, na.rm = TRUE) / 
       sum(post_V1_3_dose - pre_V1_dose <= 0 & post_V1_5_dose - pre_V1_dose > 0, na.rm = TRUE))
sum(data$Diuretics_recom_BL == 1 & data.first_dose_change_after_visit$BL_direction == "Up-titration", na.rm = TRUE)
double_increase_at_V1 <-
  tibble_dose_V1 %>% 
  merge(data.first_dose_change_after_visit,  by = "id") %>%
  filter(post_V1_dose - pre_V1_dose > 0 & Diuretics_recom_1 == 1 & V1_direction == "Up-titration") 
double_increase_at_BL <-
  tibble_dose_V1 %>% 
  merge(data.first_dose_change_after_visit,  by = "id") %>%
  filter(post_V1_dose - pre_V1_dose > 0 & Diuretics_recom_1 == 1 & V1_direction == "Up-titration") 

# Only in 30% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V1, sum(post_V1_dose - pre_V1_dose < 0 & Diuretics_recom_1 == -1, na.rm = TRUE) / sum(Diuretics_recom_1 == -1, na.rm = TRUE))
# 99% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V1, sum(post_V1_dose - pre_V1_dose == 0 & Diuretics_recom_1 == 0, na.rm = TRUE) / sum(Diuretics_recom_1 == 0, na.rm = TRUE))

# What if we add a margin of x days?
tibble_dose_V1_3days <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_V1_dose = dose[Dx == (V1-1)], post_V1_dose = dose[Dx == (V1+5)])),
  data[,c("id", "Diuretics_recom_1")], by = "id")
tibble_dose_V1_3days$diff = tibble_dose_V1_3days$post_V1_dose - tibble_dose_V1_3days$pre_V1_dose
# It goes up to 45% of the cases where the Diuretics_recom_1 == 1, there is a down titration the day of the visit
with(tibble_dose_V1_3days, sum(diff > 0 & Diuretics_recom_1 == 1, na.rm = TRUE) / sum(Diuretics_recom_1 == 1, na.rm = TRUE))
# It goes up to 50% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V1_3days, sum(diff < 0 & Diuretics_recom_1 == -1, na.rm = TRUE) / sum(Diuretics_recom_1 == -1, na.rm = TRUE))
# Goes down to 93% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V1_3days, sum(diff == 0 & Diuretics_recom_1 == 0, na.rm = TRUE) / sum(Diuretics_recom_1 == 0, na.rm = TRUE))

# What about V3?
tibble_dose_V3 <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_dose = dose[Dx == (V3-1)], post_dose = dose[Dx == (V3+3)])),
  data[,c("id", "Diuretics_recom_3")], by = "id")
tibble_dose_V3$diff = tibble_dose_V3$post_dose - tibble_dose_V3$pre_dose
# Only 16% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff > 0 & Diuretics_recom_3 == 1, na.rm = TRUE) / sum(Diuretics_recom_3 == 1, na.rm = TRUE))
# Only 21% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff < 0 & Diuretics_recom_3 == -1, na.rm = TRUE) / sum(Diuretics_recom_3 == -1, na.rm = TRUE))
# 99% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V3, sum(diff == 0 & Diuretics_recom_3 == 0, na.rm = TRUE) / sum(Diuretics_recom_3 == 0, na.rm = TRUE))

# What if we add a margin of x days?
tibble_dose_V3 <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_dose = dose[Dx == (V3-1)], post_dose = dose[Dx == (V3+5)])),
  data[,c("id", "Diuretics_recom_3")], by = "id")
tibble_dose_V3$diff = tibble_dose_V3$post_dose - tibble_dose_V3$pre_dose
# It goes up to 44% of the cases where the Diuretics_recom_1 == 1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff > 0 & Diuretics_recom_3 == 1, na.rm = TRUE) / sum(Diuretics_recom_3 == 1, na.rm = TRUE))
# It goes up to 61% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff < 0 & Diuretics_recom_3 == -1, na.rm = TRUE) / sum(Diuretics_recom_3 == -1, na.rm = TRUE))
# 96% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V3, sum(diff == 0 & Diuretics_recom_3 == 0, na.rm = TRUE) / sum(Diuretics_recom_3 == 0, na.rm = TRUE))

#Is the BL dose already updated?####
# Choose 10 IDs for HP to check pre-registration dose
# Better choose those with a Diuretics_recom_BL == 1 who did not have a dose increase between baseline and the day before V1
tibble_dose_BL_vs_beforeV1 <- 
  na.omit(
    id.Dx.dose.visit_days %>%
    group_by(id)%>%
    summarise(pre_dose = dose[Dx == 0], post_dose = dose[Dx == (V1-1)]))
suspects_preBL_dose_change <-data$id[data$id %in% tibble_dose_BL_vs_beforeV1$id[tibble_dose_BL_vs_beforeV1$pre_dose == tibble_dose_BL_vs_beforeV1$post_dose] & data$Diuretics_recom_BL == 1]
write.csv(merge(data[data$id %in% suspects_preBL_dose_change,c("id", "Loop_dose_BL", "Diuretics_recom_BL")], tibble_dose_BL_vs_beforeV1, by="id"), file = "check_preBL_dose.csv")

#Calculate doses 7 days before a visit and 30 days after or until the hospitalisation or death####
# Dose interval definition
dose_start <- -7
dose_end <- 0

# Import dose data
# Furosemide
furo <- read_excel(".\\1. Data\\Furo def.xls", sheet = 1)
colnames(furo)[1] <- "id"
furo <- merge(data[,c("id")], furo, 
                                    by = "id")
furo <- furo[order(c(furo$id)),]

furo <- furo %>% select(-c("Furo BL", "Dose BL", "V1day", "V3day", "V6day", "V12day", "V18day",
                           "Last FU", "Date of death", "RefDate", "Indicator Start"))
furo_pivot <-   furo %>% 
  pivot_longer(!id, 
               names_to = "column", 
               values_to = "value")

furo_pivot$column <- str_replace(furo_pivot$column, "Start", "D0")
furo_pivot$n <- ifelse(str_starts(furo_pivot$column, "Furo"), str_replace(furo_pivot$column, "Furo", ""), NA)
furo_pivot$n <- ifelse(str_starts(furo_pivot$column, "Dose"), str_replace(furo_pivot$column, "Dose", ""), furo_pivot$n)
furo_pivot$n <- ifelse(str_starts(furo_pivot$column, "D\\d+"), str_replace(furo_pivot$column, "D", ""), furo_pivot$n)
furo_pivot$column <- ifelse(str_starts(furo_pivot$column, "Furo"), "Furo", furo_pivot$column)
furo_pivot$column <- ifelse(str_starts(furo_pivot$column, "Dose"), "Dose", furo_pivot$column)
furo_pivot$column <- ifelse(str_starts(furo_pivot$column, "D\\d+"), "D", furo_pivot$column)

furo_tidy <-   furo_pivot %>% 
  pivot_wider(c(id, column, value, n), 
              names_from = "column", 
              values_from = "value")
# Remove empty rows
furo_tidy <- furo_tidy[!is.na(furo_tidy$Dose),]
# Remove rows after baseline
furo_tidy <- furo_tidy[furo_tidy$D < dose_end,]
# Calculate the amount of days in the last 15 that they used each dose
furo_tidy <- furo_tidy %>%
  group_by(id) %>%
  arrange(D) %>%
  mutate(diff = lead(D, default = max(last(D), dose_end)) - pmax(dose_start, D)) %>%
  arrange(id)
# Remove non positive values
furo_tidy <- furo_tidy[furo_tidy$diff > 0,]
furo_pre_BL_dose <- furo_tidy %>%
  select(c("id", "Dose", "diff")) %>% 
  group_by(id) %>%
  summarise(furo_dose_mean = sum(Dose * diff)/(dose_end-dose_start), 
            furo_dose_before = last(Dose))

# Torasemide
tora <- read_excel(".\\1. Data\\Tora def.xls", sheet = 1)
colnames(tora)[1] <- "id"
tora <- merge(data[,c("id")], tora, 
              by = "id")
tora <- tora[order(c(tora$id)),]

tora <- tora %>% select(-c("Tora BL", "Dose BL", "V1day", "V3day", "V6day", "V12day", "V18day",
                           "Last FU", "Date of death", "RefDate", "Indicator Start"))
tora_pivot <-   tora %>% 
                  pivot_longer(!id, 
                    names_to = "column", 
                    values_to = "value")

tora_pivot$column <- str_replace(tora_pivot$column, "Start", "D0")
tora_pivot$n <- ifelse(str_starts(tora_pivot$column, "Tora"), str_replace(tora_pivot$column, "Tora", ""), NA)
tora_pivot$n <- ifelse(str_starts(tora_pivot$column, "Dose"), str_replace(tora_pivot$column, "Dose", ""), tora_pivot$n)
tora_pivot$n <- ifelse(str_starts(tora_pivot$column, "D\\d+"), str_replace(tora_pivot$column, "D", ""), tora_pivot$n)
tora_pivot$column <- ifelse(str_starts(tora_pivot$column, "Tora"), "Tora", tora_pivot$column)
tora_pivot$column <- ifelse(str_starts(tora_pivot$column, "Dose"), "Dose", tora_pivot$column)
tora_pivot$column <- ifelse(str_starts(tora_pivot$column, "D\\d+"), "D", tora_pivot$column)

tora_tidy <-   tora_pivot %>% 
               pivot_wider(c(id, column, value, n), 
               names_from = "column", 
               values_from = "value")
# Remove empty rows
tora_tidy <- tora_tidy[!is.na(tora_tidy$Dose),]
# Remove rows after baseline
tora_tidy <- tora_tidy[tora_tidy$D < dose_end,]
# Calculate the amount of days in the last 15 that they used each dose
tora_tidy <- tora_tidy %>%
              group_by(id) %>%
              arrange(D) %>%
              mutate(diff = lead(D, default = max(last(D), dose_end)) - pmax(dose_start, D)) %>%
              arrange(id)
# Remove non positive values
tora_tidy <- tora_tidy[tora_tidy$diff > 0,]
tora_pre_BL_dose <- tora_tidy %>%
                    select(c("id", "Dose", "diff")) %>% 
                    group_by(id) %>%
                    summarise(tora_dose_mean = sum(Dose * diff)/(dose_end-dose_start), 
                              tora_dose_before = last(Dose))

pre_BL_dose <- merge(furo_pre_BL_dose, tora_pre_BL_dose, by="id", all = TRUE)
pre_BL_dose <- merge(pre_BL_dose, select(data, c(id, Loop_dose_BL)), by="id", all.y = TRUE)
pre_BL_dose[is.na(pre_BL_dose)] <- 0
pre_BL_dose$loop_dose_mean <- pre_BL_dose$furo_dose_mean + pre_BL_dose$tora_dose_mean * 4
pre_BL_dose$loop_dose_before <- pre_BL_dose$furo_dose_before + pre_BL_dose$tora_dose_before * 4
sum(pre_BL_dose$Loop_dose_BL != pre_BL_dose$loop_dose_mean)

# Doses before visits
id.Dx.dose.visit_days$is_pre_V1 <- with(id.Dx.dose.visit_days, Dx >= (V1 + dose_start) & Dx < V1)
id.Dx.dose.visit_days$is_pre_V3 <- with(id.Dx.dose.visit_days, Dx >= (V3 + dose_start) & Dx < V3)
id.Dx.dose.visit_days$is_pre_V6 <- with(id.Dx.dose.visit_days, Dx >= (V6 + dose_start) & Dx < V6)
id.Dx.dose.visit_days$is_pre_V12 <- with(id.Dx.dose.visit_days, Dx >= (V12 + dose_start) & Dx < V12)
id.Dx.dose.visit_days$is_pre_V18 <- with(id.Dx.dose.visit_days, Dx >= (V12 + dose_start) & Dx < V12)
pre_visit_mean_dose <- 
  id.Dx.dose.visit_days %>% 
  group_by(id) %>% 
  summarise(V1 = mean(dose[is_pre_V1], na.rm = TRUE),
            V3 = mean(dose[is_pre_V3], na.rm = TRUE),
            V6 = mean(dose[is_pre_V6], na.rm = TRUE),
            V12 = mean(dose[is_pre_V12], na.rm = TRUE),
            V18 = mean(dose[is_pre_V18], na.rm = TRUE))
pre_visit_mean_dose$BL <- pre_BL_dose$loop_dose_mean
  
pre_visit_day_dose <- 
  id.Dx.dose.visit_days %>% 
  group_by(id) %>% 
  summarise(V1 = ifelse(any(is.na(V1)), NA, dose[Dx == (V1-1)]), 
            V3 = ifelse(any(is.na(V3)), NA, dose[Dx == (V3-1)]),
            V6 = ifelse(any(is.na(V6)), NA, dose[Dx == (V6-1)]),
            V12 = ifelse(any(is.na(V12)), NA, dose[Dx == (V12-1)]),
            V18 = ifelse(any(is.na(V18)), NA, ifelse(any(Dx == (V18-1)), dose[Dx == (V18-1)], 0)))
pre_visit_day_dose$BL <- pre_BL_dose$loop_dose_before

# 
# post_visit_time_to_event <- data.frame(id = data.hospit_CHF_after_visit$id,
#                                        BL = pmin(data.hospit_CHF_after_visit$BL, data.hospit_linked_after_visit$BL, data.hospit_renal_after_visit$BL, data.death_after_visit$BL),
#                                        V1 = pmin(data.hospit_CHF_after_visit$V1, data.hospit_linked_after_visit$V1, data.hospit_renal_after_visit$V1, data.death_after_visit$V1),
#                                        V3 = pmin(data.hospit_CHF_after_visit$V3, data.hospit_linked_after_visit$V3, data.hospit_renal_after_visit$V3, data.death_after_visit$V3),
#                                        V6 = pmin(data.hospit_CHF_after_visit$V6, data.hospit_linked_after_visit$V6, data.hospit_renal_after_visit$V6, data.death_after_visit$V6),
#                                        V12 = pmin(data.hospit_CHF_after_visit$V12, data.hospit_linked_after_visit$V12, data.hospit_renal_after_visit$V12, data.death_after_visit$V12),
#                                        V18 = pmin(data.hospit_CHF_after_visit$V18, data.hospit_linked_after_visit$V18, data.hospit_renal_after_visit$V18, data.death_after_visit$V18))


post_visit_time_to_event <- data.frame(id = data.hospit_CHF_after_visit$id,
                                       BL = pmin(data.first_dose_change_after_visit_lagged$BL, data.hospit_CHF_after_visit$BL, data.hospit_linked_after_visit$BL, data.hospit_renal_after_visit$BL, data.death_after_visit$BL),
                                       V1 = pmin(data.first_dose_change_after_visit_lagged$BL, data.hospit_CHF_after_visit$V1, data.hospit_linked_after_visit$V1, data.hospit_renal_after_visit$V1, data.death_after_visit$V1),
                                       V3 = pmin(data.first_dose_change_after_visit_lagged$BL, data.hospit_CHF_after_visit$V3, data.hospit_linked_after_visit$V3, data.hospit_renal_after_visit$V3, data.death_after_visit$V3),
                                       V6 = pmin(data.first_dose_change_after_visit_lagged$BL, data.hospit_CHF_after_visit$V6, data.hospit_linked_after_visit$V6, data.hospit_renal_after_visit$V6, data.death_after_visit$V6),
                                       V12 = pmin(data.first_dose_change_after_visit_lagged$BL, data.hospit_CHF_after_visit$V12, data.hospit_linked_after_visit$V12, data.hospit_renal_after_visit$V12, data.death_after_visit$V12),
                                       V18 = pmin(data.first_dose_change_after_visit_lagged$BL,data.hospit_CHF_after_visit$V18, data.hospit_linked_after_visit$V18, data.hospit_renal_after_visit$V18, data.death_after_visit$V18))
# Censor events after the first
# data.hospit_CHF_after_visit$BL <- ifelse(data.hospit_CHF_after_visit$BL > post_visit_time_to_event$BL, Inf, data.hospit_CHF_after_visit$BL)
# data.hospit_CHF_after_visit$V1 <- ifelse(data.hospit_CHF_after_visit$V1 > post_visit_time_to_event$V1, Inf, data.hospit_CHF_after_visit$V1)
# data.hospit_CHF_after_visit$V3 <- ifelse(data.hospit_CHF_after_visit$V3 > post_visit_time_to_event$V3, Inf, data.hospit_CHF_after_visit$V3)
# data.hospit_CHF_after_visit$V6 <- ifelse(data.hospit_CHF_after_visit$V6 > post_visit_time_to_event$V6, Inf, data.hospit_CHF_after_visit$V6)
# data.hospit_CHF_after_visit$V12 <- ifelse(data.hospit_CHF_after_visit$V12 > post_visit_time_to_event$V12, Inf, data.hospit_CHF_after_visit$V12)
# data.hospit_CHF_after_visit$V18 <- ifelse(data.hospit_CHF_after_visit$V18 > post_visit_time_to_event$V18, Inf, data.hospit_CHF_after_visit$V18)
# data.death_after_visit$BL <- ifelse(data.death_after_visit$BL > post_visit_time_to_event$BL, Inf, data.death_after_visit$BL)
# data.death_after_visit$V1 <- ifelse(data.death_after_visit$V1 > post_visit_time_to_event$V1, Inf, data.death_after_visit$V1)
# data.death_after_visit$V3 <- ifelse(data.death_after_visit$V3 > post_visit_time_to_event$V3, Inf, data.death_after_visit$V3)
# data.death_after_visit$V6 <- ifelse(data.death_after_visit$V6 > post_visit_time_to_event$V6, Inf, data.death_after_visit$V6)
# data.death_after_visit$V12 <- ifelse(data.death_after_visit$V12 > post_visit_time_to_event$V12, Inf, data.death_after_visit$V12)
# data.death_after_visit$V12 <- ifelse(data.death_after_visit$V18 > post_visit_time_to_event$V18, Inf, data.death_after_visit$V18)

# post_visit_time_to_event <- data.frame(id = data.hospit_CHF_after_visit$id,
#                                        BL = pmin(data.hospit_CHF_after_visit$BL, data.hospit_linked_after_visit$BL, data.hospit_renal_after_visit$BL, data.death_after_visit$BL),
#                                        V1 = pmin(data.hospit_CHF_after_visit$V1, data.hospit_linked_after_visit$V1, data.hospit_renal_after_visit$V1, data.death_after_visit$V1),
#                                        V3 = pmin(data.hospit_CHF_after_visit$V3, data.hospit_linked_after_visit$V3, data.hospit_renal_after_visit$V3, data.death_after_visit$V3),
#                                        V6 = pmin(data.hospit_CHF_after_visit$V6, data.hospit_linked_after_visit$V6, data.hospit_renal_after_visit$V6, data.death_after_visit$V6),
#                                        V12 = pmin(data.hospit_CHF_after_visit$V12, data.hospit_linked_after_visit$V12, data.hospit_renal_after_visit$V12, data.death_after_visit$V12),
#                                        V18 = pmin(data.hospit_CHF_after_visit$V18, data.hospit_linked_after_visit$V18, data.hospit_renal_after_visit$V18, data.death_after_visit$V18))

#Dose changes after a visit###############
post_visit_time_to_event<- data.frame(lapply(post_visit_time_to_event, function(x){ifelse(is.infinite(x), time_horizon, x)}))

post_visit_to_event_dose <- 
  id.Dx.dose.visit_days %>% 
  merge(post_visit_time_to_event, by = "id", suffixes = c(".day",".th")) %>% 
  group_by(id) %>%
  arrange(Dx) %>%
  summarise(BL = ifelse(any(is.na(BL)), NA, mean(dose[Dx < BL], na.rm = FALSE)),
            V1 = ifelse(any(is.na(V1.day)), NA, mean(dose[Dx >= V1.day & Dx < (V1.day + V1.th)], na.rm = FALSE)),
            V3 = ifelse(any(is.na(V3.day)), NA, mean(dose[Dx >= V3.day & Dx < (V3.day + V3.th)], na.rm = FALSE)),
            V6 = ifelse(any(is.na(V6.day)), NA, mean(dose[Dx >= V6.day & Dx < (V6.day + V6.th)], na.rm = FALSE)),
            V12 = ifelse(any(is.na(V12.day)), NA, mean(dose[Dx >= V12.day & Dx < (V12.day + V12.th)], na.rm = FALSE)),
            V18 = ifelse(any(is.na(V18.day)), NA, mean(dose[Dx >= V18.day & Dx < (V18.day + V18.th)], na.rm = FALSE)),
  )

post_visit_lagged_dose <- 
  id.Dx.dose.visit_days %>% 
  merge(post_visit_time_to_event, by = "id", suffixes = c(".day",".th")) %>% 
  merge(data.first_dose_change_after_visit_lagged, by = "id", suffixes = c("",".dc")) %>% #
  group_by(id) %>%
  arrange(Dx) %>%
  summarise(BL = ifelse(any(is.na(BL)),       NA, dose[Dx == (min(BL, BL.dc) - 1)]),
            V1 = ifelse(any(is.na(V1.day)),   NA, dose[Dx == (V1.day + min(V1.th, V1)-1)]),
            V3 = ifelse(any(is.na(V3.day)),   NA, dose[Dx == (V3.day + min(V3.th, V3)-1)]),
            V6 = ifelse(any(is.na(V6.day)),   NA, dose[Dx == (V6.day + min(V6.th, V6)-1)]),
            V12 = ifelse(any(is.na(V12.day)), NA, dose[Dx == (V12.day + min(V12.th, V12)-1)]),
            V18 = ifelse(any(is.na(V18.day)), NA, dose[Dx == (V18.day + min(V18.th, V18)-1)]),
  )

post_visit_dose_change <- data.frame(id = post_visit_lagged_dose$id,
                                     BL = post_visit_lagged_dose$BL - pre_visit_day_dose$BL,
                                     V1 = post_visit_lagged_dose$V1 - pre_visit_day_dose$V1,
                                     V3 = post_visit_lagged_dose$V3 - pre_visit_day_dose$V3,
                                     V6 = post_visit_lagged_dose$V6 - pre_visit_day_dose$V6,
                                     V12 = post_visit_lagged_dose$V12 - pre_visit_day_dose$V12,
                                     V18 = post_visit_lagged_dose$V18 - pre_visit_day_dose$V18)

# post_visit_dose_change <- data.frame(id = post_visit_to_event_dose$id,
#                                      BL = post_visit_to_event_dose$BL - pre_visit_mean_dose$BL,
#                                      V1 = post_visit_to_event_dose$V1 - pre_visit_mean_dose$V1,
#                                      V3 = post_visit_to_event_dose$V3 - pre_visit_mean_dose$V3,
#                                      V6 = post_visit_to_event_dose$V6 - pre_visit_mean_dose$V6,
#                                      V12 = post_visit_to_event_dose$V12 - pre_visit_mean_dose$V12,
#                                      V18 = post_visit_to_event_dose$V18 - pre_visit_mean_dose$V18)

post_visit_dose_change_cat <- data.frame(id = post_visit_dose_change$id,
                                         BL= with(post_visit_dose_change, ifelse(BL>=0,ifelse(BL>0, "Up-titration", "No change"), "Down-titration")),
                                         V1 = with(post_visit_dose_change, ifelse(V1>=0,ifelse(V1>0, "Up-titration", "No change"), "Down-titration")),
                                         V3 = with(post_visit_dose_change, ifelse(V3>=0,ifelse(V3>0, "Up-titration", "No change"), "Down-titration")),
                                         V6 = with(post_visit_dose_change, ifelse(V6>=0,ifelse(V6>0, "Up-titration", "No change"), "Down-titration")),
                                         V12 = with(post_visit_dose_change, ifelse(V12>=0,ifelse(V12>0, "Up-titration", "No change"), "Down-titration")),
                                         V18 = with(post_visit_dose_change, ifelse(V18>=0,ifelse(V18>0, "Up-titration", "No change"), "Down-titration")))

# post_visit_dose_change_cat <- data.frame(id = post_visit_dose_change$id,
#                                          BL= with(post_visit_dose_change, ifelse(BL>-20,ifelse(BL>20, "Up-titration", "No change"), "Down-titration")),
#                                          V1 = with(post_visit_dose_change, ifelse(V1>-20,ifelse(V1>20, "Up-titration", "No change"), "Down-titration")),
#                                          V3 = with(post_visit_dose_change, ifelse(V3>-20,ifelse(V3>20, "Up-titration", "No change"), "Down-titration")),
#                                          V6 = with(post_visit_dose_change, ifelse(V6>-20,ifelse(V6>20, "Up-titration", "No change"), "Down-titration")),
#                                          V12 = with(post_visit_dose_change, ifelse(V12>-20,ifelse(V12>20, "Up-titration", "No change"), "Down-titration")),
#                                          V18 = with(post_visit_dose_change, ifelse(V18>-20,ifelse(V18>20, "Up-titration", "No change"), "Down-titration")))

  
####
data.h <- Fill_Hugin_data()
#data.h$Fluid_level_cont <-  with(data.biomarkers, c(NBNP, BNP1, BNP3, BNP6, BNP12, BNP18))
data.h$Fluid_level_cont <-  with(data.new_congestion_scores, c(Clinical_congestion_score_BL, Clinical_congestion_score_1, Clinical_congestion_score_3, Clinical_congestion_score_6, Clinical_congestion_score_12, Clinical_congestion_score_18))
data.h$Fluid_level <- data.h$Fluid_level <- discretise_quantiles(data.h$Fluid_level_cont, labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$Dose_change <- with(post_visit_dose_change, c(BL,V1,V3,V6,V12,V18)) 
data.h$Intervention <- with(post_visit_dose_change_cat, c(BL,V1,V3,V6,V12,V18)) 
data.h$Subsequent_dose_change <- with(data.first_dose_change_after_visit_lagged, c(BL_direction,V1_direction,V3_direction,V6_direction,V12_direction,V18_direction)) 

# Remove censored datapoints
data.h <- data.h[!is.na(with(data, c(rep(0, nrow(data)), V1, V3, V6, V12, V18))),]
write.csv(data.h, "Hugin-TIME-CHF-visits-CombinedCS-correctdosechanges.dat", row.names = FALSE)
#Fit models to uptitration######################################################
# Discretised data
# Up-titration still linked to higher death and hospitalisation. Why?
with(data.h,table(Intervention, Death, Fluid_level))
with(data.h,prop.table(table(Intervention, Death, Fluid_level), margin=c(3,1)))
with(data.h,table(Intervention, Hospitalisation_CHF, Fluid_level))
with(data.h,prop.table(table(Intervention, Hospitalisation_CHF, Fluid_level), margin=c(3,1)))
with(data.h,table(Intervention, Subsequent_dose_change, Fluid_level))
with(data.h,prop.table(table(Intervention, Subsequent_dose_change, Fluid_level), margin=c(3,1)))
# Looking in individual cases
with(data.h,pat_id[Fluid_level == "High" & Hospitalisation_CHF == "yes" & Intervention == "Up-titration"])
with(data.h,pat_id[Fluid_level == "Average" & Hospitalisation_CHF == "yes" & Intervention == "Up-titration"])
with(data.h,pat_id[Fluid_level == "Very High" & Hospitalisation_CHF == "yes" & Intervention == "Up-titration"])
with(data.h,pat_id[Fluid_level == "Very High" & Death == "yes" & Intervention == "Up-titration"])
with(data.h,data.h[Fluid_level == "Low" & Death == "yes", c("pat_id", "Intervention")])

# Raw continuous data
summary(glm(ifelse(data.hospit_CHF_after_visit$BL <= time_horizon,1,0) ~ post_visit_dose_change$BL + data.congention_score$CCS))
summary(glm(ifelse(data.hospit_CHF_after_visit$V1 <= time_horizon,1,0) ~ post_visit_dose_change$V1 + data.congention_score$CCS1 + pre_visit_day_dose$V1))
summary(glm(ifelse(data.hospit_CHF_after_visit$V3 <= time_horizon,1,0) ~ post_visit_dose_change$V3 + data.congention_score$CCS3))
summary(glm(ifelse(data.hospit_CHF_after_visit$V6 <= time_horizon,1,0) ~ post_visit_dose_change$V6 + data.congention_score$CCS6))
summary(glm(ifelse(data.hospit_CHF_after_visit$V12 <= time_horizon,1,0) ~ post_visit_dose_change$V12 + data.congention_score$CCS12))

summary(glm(ifelse(data.hospit_CHF_after_visit$BL <= time_horizon,1,0) ~ post_visit_dose_change$BL + data.new_congestion_scores$Clinical_congestion_score_BL))
summary(glm(ifelse(data.hospit_CHF_after_visit$V1 <= time_horizon,1,0) ~ post_visit_dose_change$V1 + pre_visit_day_dose$V1 + data.new_congestion_scores$Clinical_congestion_score_1))
summary(glm(ifelse(data.hospit_CHF_after_visit$V3 <= time_horizon,1,0) ~ post_visit_dose_change$V3 + data.new_congestion_scores$Clinical_congestion_score_3))
summary(glm(ifelse(data.hospit_CHF_after_visit$V6 <= time_horizon,1,0) ~ post_visit_dose_change$V6 + data.new_congestion_scores$Clinical_congestion_score_6))
summary(glm(ifelse(data.hospit_CHF_after_visit$V12 <= time_horizon,1,0) ~ post_visit_dose_change$V12 + data.new_congestion_scores$Clinical_congestion_score_12))

mean(pre_visit_day_dose$V1[post_visit_dose_change$V1 > 0], na.rm = TRUE)
mean(pre_visit_day_dose$V1[post_visit_dose_change$V1 < 0], na.rm = TRUE)

data.death_after_visit_all_bin <- ifelse(with(data.death_after_visit, c(BL, V1, V3, V6, V12, V18)) < time_horizon, 1,0)
summary(glm(ifelse(data.death_after_visit$V1 <= time_horizon,1,0) ~ post_visit_dose_change$V1 + + pre_visit_day_dose$V1 + data.congention_score$CCS1))
summary(glm(ifelse(data.death_after_visit$V3 <= time_horizon,1,0) ~ post_visit_dose_change$V3 + data.congention_score$CCS3))
summary(glm(ifelse(data.death_after_visit$V6 <= time_horizon,1,0) ~ post_visit_dose_change$V6 + data.congention_score$CCS6))
summary(glm(ifelse(data.death_after_visit$V12 <= time_horizon,1,0) ~ post_visit_dose_change$V12 + data.congention_score$CCS12))

summary(glm(ifelse(data.death_after_visit$BL <= time_horizon,1,0) ~ post_visit_dose_change$BL + data.new_congestion_scores$Clinical_congestion_score_BL))
summary(glm(ifelse(data.death_after_visit$V1 <= time_horizon,1,0) ~ post_visit_dose_change$V1 + + pre_visit_day_dose$V1 + data.new_congestion_scores$Clinical_congestion_score_1))
summary(glm(ifelse(data.death_after_visit$V3 <= time_horizon,1,0) ~ post_visit_dose_change$V3 + data.new_congestion_scores$Clinical_congestion_score_3))
summary(glm(ifelse(data.death_after_visit$V6 <= time_horizon,1,0) ~ post_visit_dose_change$V6 + data.new_congestion_scores$Clinical_congestion_score_6))
summary(glm(ifelse(data.death_after_visit$V12 <= time_horizon,1,0) ~ post_visit_dose_change$V12 + data.new_congestion_scores$Clinical_congestion_score_12))

summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V1_direction == "Up-titration",1,0) ~ post_visit_dose_change$V1 + pre_visit_day_dose$V1 + data.congention_score$CCS1))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V3_direction == "Up-titration",1,0) ~ post_visit_dose_change$V3 + data.congention_score$CCS3))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V6_direction == "Up-titration",1,0) ~ post_visit_dose_change$V6 + data.congention_score$CCS6))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V12_direction == "Up-titration",1,0) ~ post_visit_dose_change$V12 + data.congention_score$CCS12))

summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V1_direction == "Up-titration",1,0) ~ post_visit_dose_change_cat$V1 + pre_visit_day_dose$V1 + data.new_congestion_scores$Clinical_congestion_score_1))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V3_direction == "Up-titration",1,0) ~ post_visit_dose_change$V3 + data.new_congestion_scores$Clinical_congestion_score_3))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V6_direction == "Up-titration",1,0) ~ post_visit_dose_change$V6 + data.new_congestion_scores$Clinical_congestion_score_6))
summary(glm(ifelse(data.first_dose_change_after_visit_lagged$V12_direction == "Up-titration",1,0) ~ post_visit_dose_change$V12 + data.new_congestion_scores$Clinical_congestion_score_12))


summary(glm(ifelse(Hospitalisation_CHF == 'yes', 1,0) ~ Fluid_level_cont + Dose_change, data = data.h))
summary(glm(ifelse(Death == 'yes', 1,0) ~ Fluid_level_cont + Dose_change, data = data.h))

data.model <- data.frame(id          = rep(data$id, 4), 
                         hosp        = with(data.hospit_CHF_after_visit, c(V1, V3, V6, V12)),
                         death       +
                         dose_change = with(post_visit_dose_change_cat, c(V1, V3, V6, V12)),
                         NYHA        = with(data, c(NYHA1, NYHA3, NYHA6, NYHA12)), 
                         BNP         = with(data.biomarkers, c(BNP1, BNP3, BNP6, BNP12)),
                         recom       = with(data, c(Diuretics_recom_1, Diuretics_recom_3, Diuretics_recom_6, Diuretics_recom_12)))
                         
data.model$hosp10 <- ifelse(data.model$hosp <= time_horizon,1,0)
data.model$dose_change_up_or_not <- ifelse(data.model$dose_change == "Up-titration",1,0)

summary(glm(hosp10 ~ dose_change + NYHA + BNP, data = data.model))

summary(glm(hosp10 ~ dose_change_up_or_not + NYHA + BNP, data = data.model[data.model$recom == 1,]))


