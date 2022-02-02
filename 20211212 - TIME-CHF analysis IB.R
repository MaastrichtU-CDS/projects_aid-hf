####################################################################################################################

# Packages

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

####################################################################################################################

# Data

data <- read_sav(".\\TIME_CHF_all_correct_follow_up.sav")
colnames(data)[1] <- "id"
data <- subset(data, Withdrawn != 1)
data <- data[order(c(data$id)),]

data$diff_refdate_entrydate <- as.numeric(difftime(data$Dateofstudyentry, data$RefDate, units = c("days")))

# Diuretics
data.loop_diuretics <- read_excel(".\\Medication daily doses.xlsx", sheet = 6)
colnames(data.loop_diuretics)[1] <- "id"
table(data$id %in% data.loop_diuretics$id)
data.loop_diuretics <- merge(data[,c("id", 
                                     "Withdrawn")], 
                             data.loop_diuretics, 
                             by = "id")
data.loop_diuretics <- subset(data.loop_diuretics, Withdrawn != 1)
data.loop_diuretics <- data.loop_diuretics[order(c(data.loop_diuretics$id)),]

# Hospitalisations
data.hospitalisations <- read_excel(".\\1. Data\\Hospitalisations overview.xlsx")
colnames(data.hospitalisations)[1] <- "id"
table(unique(data.hospitalisations$id) %in% data$id)
data.hospitalisations <- merge(data[,c("id", 
                                       "Withdrawn", "RefDate")], 
                               data.hospitalisations, 
                               by = "id")
data.hospitalisations <- subset(data.hospitalisations, Withdrawn != 1)
data.hospitalisations <- data.hospitalisations[order(c(data.hospitalisations$id)),]$table(unique(data.hospitalisations$id) %in% data$id)


# Symptoms at visits
data.symptoms <- read_excel(".\\1. Data\\Visits Symptoms.xlsx", sheet = 1)
colnames(data.symptoms)[1] <- "id"
data.symptoms <- merge(data[,c("id", "Withdrawn")], data.symptoms, 
                       by = "id")
data.symptoms <- data.symptoms[order(c(data.symptoms$id)),]


# Biomarker measurements at visits
data.biomarkers <- read_excel(".\\1. Data\\TIME-CHF biomarkers.xlsx", sheet = 1)
colnames(data.biomarkers)[1] <- "id"
data.biomarkers <- merge(data[,c("id", "Withdrawn")], data.biomarkers, 
                       by = "id")
data.biomarkers <- data.biomarkers[order(c(data.biomarkers$id)),]

# Dose change indications
data.indications <- read_excel(".\\1. Data\\Combi diuretic all indication 1221.xlsx", sheet = 1)
colnames(data.indications)[1] <- "id"
data.indications <- merge(data[,c("id", "Withdrawn")], data.indications, 
                         by = "id")
data.indications <- data.indications[order(c(data.indications$id)),]

data.indications.def <- read_excel(".\\1. Data\\Indications.xlsx", sheet = 1)

# Alt congestion score
data.congention_score <- read_excel(".\\1. Data\\Congestion score.xlsx");
colnames(data.congention_score)[1] <- "id"
data.congention_score <- merge(data[,c("id", "Withdrawn")], data.congention_score, 
                         by = "id")
data.congention_score <- data.congention_score[order(c(data.congention_score$id)),]

# Combined congestion score
data.new_congestion_scores <- read_excel(".\\1. Data\\Congestion score new scores.xlsx", sheet = 1)
colnames(data.new_congestion_scores)[1] <- "id"
data.new_congestion_scores <- merge(data[,c("id", "Withdrawn")], data.new_congestion_scores, 
                                    by = "id")
data.new_congestion_scores <- data.new_congestion_scores[order(c(data.new_congestion_scores$id)),]

####################################################################################################################
maxFU <- 580
# Loop diuretics

## Dose per day

data.dose <- data.loop_diuretics[, c(1, 5:(maxFU+5))] 
colnames(data.dose)[1] <- "id"
id.Dx.dose <- 
  data.dose %>% 
  pivot_longer(!id, 
               names_to = "Dx", 
               values_to = "dose")
id.Dx.dose$Dx <- str_replace(id.Dx.dose$Dx, "D", "")
id.Dx.dose$Dx <- as.numeric(as.character(id.Dx.dose$Dx))

ggplot(id.Dx.dose, aes(x = Dx, y = id, fill = dose)) + 
  geom_raster() + 
  scale_x_continuous(name = "Days from baseline", 
                     breaks = seq(0, maxFU, 30),
                     labels = seq(0, maxFU, 30)) + 
  scale_y_discrete(name = "ID", 
                   labels = NULL) + 
  scale_fill_continuous(name = "Dose")

## Dose change: present day - subsequent day 

dose_change <- as.data.frame(matrix(nrow = nrow(data.dose), ncol = (maxFU+1)))
colnames(dose_change)[1] <- "id"
dose_change[,1] <- data.dose[,1]
for (i in 2:(maxFU+1)) {
  colnames(dose_change)[i] <- paste("D", i - 2, "_minus_", "D", i - 1, sep = "")
  }

for (i in 1:maxFU) {
  for (j in 2:(maxFU+1)) { 
    dose_change[i, j] <- data.dose[i, j] - data.dose[i, (j + 1)] 
  }
}

id.Dx_minus_Dy.dose_change <- 
  dose_change %>% 
  pivot_longer(!id, 
               names_to = "Dx_minus_Dy", 
               values_to = "dose_change")

dose_change.qualitative_indicator <- dose_change
fun.qualitative.na.9999 <- function(x) { ifelse(is.na(x), 
                                                9999, 
                                                x)
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)], 
                                                   2,
                                                   FUN = fun.qualitative.na.9999)
fun.qualitative.up.minus_8888 <- function(x) { ifelse(x < 0,
                                                      -8888,
                                                      x) 
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)], 
                                                   2, 
                                                   FUN = fun.qualitative.up.minus_8888)
fun.qualitative.down.8888 <- function(x) { ifelse( (x > 0) & (x < 9999), 
                                                   8888, 
                                                   x)
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)], 
                                                   2,
                                                   FUN = fun.qualitative.down.8888)
fun.qualitative.na <- function(x) { ifelse(x == 9999, 
                                           "Missing", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)], 
                                                   2, 
                                                   FUN = fun.qualitative.na)
fun.qualitative.up <- function(x) { ifelse(x == -8888, 
                                           "Up-titration", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)],
                                                    2,
                                                    FUN = fun.qualitative.up)
fun.qualitative.nc <- function(x) { ifelse(x == 0, 
                                           "No change", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[, 2:(maxFU+1)], 
                                                    2, 
                                                    FUN = fun.qualitative.nc)

fun.qualitative.down <- function(x) { ifelse(x == 8888, 
                                             "Down-titration", 
                                             x)
    }
dose_change.qualitative_indicator[,2:(maxFU+1)] <- apply(dose_change.qualitative_indicator[,2:(maxFU+1)], 
                                                   2, 
                                                   FUN = fun.qualitative.down)

id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx <- 
  dose_change.qualitative_indicator %>% 
  pivot_longer(!id, 
               names_to = "Dx_minus_Dy", 
               values_to = "dose_change.qualitative_indicator")
id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$dose_change.qualitative_indicator <- 
  factor(id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$dose_change.qualitative_indicator,
         levels = c("Up-titration", 
                    "No change",
                    "Down-titration", 
                    "Missing"))
id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$Dx <- rep(0:(maxFU-1), nrow(dose_change.qualitative_indicator))

ggplot(id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, aes(y = id, x = Dx, fill = dose_change.qualitative_indicator)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, (maxFU-1)), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                paste0("Day ",(maxFU-1)," - ", maxFU))) + 
  scale_y_discrete(name = "ID",
                   labels = NULL) + 
  scale_fill_manual(name = "Legend", 
                    values = c("pink", 
                               "deepskyblue4", 
                               "deepskyblue", 
                               "black")) + 
  theme(axis.text.x = element_text(angle = 90))

####################################################################################################################

# Hospitalisations

hospitalisations.cause <- table(data.hospitalisations$"Cause of Hospitalisation")

data.hospitalisations$cause.CHF <- ifelse(data.hospitalisations$"Cause of Hospitalisation" 
                                          == 
                                          "Worsening CHF", 
                                          1, 
                                          0)
data.hospitalisations$cause.linked <- ifelse(data.hospitalisations$"Cause of Hospitalisation" 
                                            %in% 
                                            c('Trauma / fracture', 'Syncope/Tachyarrhythmias', 'Syncope/Hypotension', 'Stroke', 'Syncope unknown'), 
                                          1, 
                                          0)

data.hospitalisations$cause.renal <- ifelse(data.hospitalisations$"Cause of Hospitalisation" 
                                            == 
                                              "Renal failure", 
                                            1, 
                                            0)

data.hospitalisations$cause.planned <- ifelse(as.numeric(rownames(data.hospitalisations)) 
                                              %in% 
                                              grep("planned|Planned|Elective|elective|Battery|battery", data.hospitalisations$Comment), 
                                              1, 
                                              0)
data.hospitalisations$cause.other <- ifelse((data.hospitalisations$cause.CHF != 1) 
                                            & 
                                            (data.hospitalisations$cause.planned != 1), 
                                            1, 
                                            0)

data.hospitalisations.merged <- subset(data, select = c("id"))
data.hospitalisations.merged <- merge(data.hospitalisations.merged, data.hospitalisations, 
                                      by = "id", 
                                      all.x = TRUE, 
                                      all.y = TRUE)
data.hospitalisations.merged <- subset(data.hospitalisations.merged, select = c("id", 
                                                                                "cause.CHF", 
                                                                                "cause.renal",
                                                                                "cause.planned", 
                                                                                "cause.linked",
                                                                                "cause.other",
                                                                                "RefDate", 
                                                                                "Date of Admission", 
                                                                                "Date of Discharge"))
data.hospitalisations.merged$Dx.admission <- difftime(data.hospitalisations.merged$"Date of Admission", 
                                                      data.hospitalisations.merged$RefDate, units = c("days"))
data.hospitalisations.merged$Dx.discharge <- difftime(data.hospitalisations.merged$"Date of Discharge", 
                                                      data.hospitalisations.merged$RefDate, units = c("days"))
data.hospitalisations.merged <- merge(data[, c("id", 
                                               "Death", 
                                               "Dateofdeath")], 
                                      data.hospitalisations.merged, 
                                      by = "id", 
                                      all.x = TRUE, 
                                      all.y = TRUE)
colnames(data.hospitalisations.merged)[2] <- "deceased_indicator.overall"
data.hospitalisations.merged$Dx.deceased <- difftime(data.hospitalisations.merged$Dateofdeath, 
                                                     data.hospitalisations.merged$RefDate)
data.hospitalisations.merged <- subset(data.hospitalisations.merged, 
                                       select = c("id", 
                                                  "cause.CHF", 
                                                  "cause.renal",
                                                  "cause.planned", 
                                                  "cause.linked", 
                                                  "cause.other", 
                                                  "deceased_indicator.overall", 
                                                  "Dx.admission", 
                                                  "Dx.discharge", 
                                                  "Dx.deceased"))

data.hospitalisations.merged.maxFU <- data.hospitalisations.merged
data.hospitalisations.merged.maxFU$Dx.admission <- ifelse(data.hospitalisations.merged$Dx.admission > maxFU, 
                                                        NA, 
                                                        data.hospitalisations.merged$Dx.admission)
data.hospitalisations.merged.maxFU$cause.CHF <- ifelse(data.hospitalisations.merged$Dx.admission > maxFU, 
                                                     NA, 
                                                     data.hospitalisations.merged$cause.CHF)
data.hospitalisations.merged.maxFU$cause.planned <- ifelse(data.hospitalisations.merged$Dx.admission > maxFU, 
                                                         NA, 
                                                         data.hospitalisations.merged$cause.planned)
data.hospitalisations.merged.maxFU$cause.other <- ifelse(data.hospitalisations.merged$Dx.admission > maxFU, 
                                                       NA, 
                                                       data.hospitalisations.merged$cause.other)
data.hospitalisations.merged.maxFU$Dx.discharge <- ifelse(data.hospitalisations.merged$Dx.admission > maxFU, 
                                                        NA, 
                                                        data.hospitalisations.merged$Dx.discharge)
data.hospitalisations.merged.maxFU$deceased_indicator.maxFU <- ifelse(data.hospitalisations.merged$Dx.deceased > maxFU, 
                                                                  NA, 
                                                                  data.hospitalisations.merged$deceased_indicator.overall)
data.hospitalisations.merged.maxFU$Dx.deceased.maxFU <- ifelse(data.hospitalisations.merged$Dx.deceased > maxFU, 
                                                           NA, 
                                                           data.hospitalisations.merged$Dx.deceased)

length(unique(data.hospitalisations.merged.maxFU$id))

table(data.hospitalisations.merged.maxFU$cause.CHF, exclude = NULL)
data.hospitalisations.merged.maxFU$cause.CHF <- ifelse(is.na(data.hospitalisations.merged.maxFU$cause.CHF), 
                                                     0, 
                                                     data.hospitalisations.merged.maxFU$cause.CHF)
cause.CHF.pp.maxFU <- aggregate(data.hospitalisations.merged.maxFU$cause.CHF, 
                              by = list(data.hospitalisations.merged.maxFU$id), 
                              FUN = sum)
table(cause.CHF.pp.maxFU[, 2], exclude = NULL)

table(data.hospitalisations.merged.maxFU$cause.planned, exclude = NULL)
data.hospitalisations.merged.maxFU$cause.planned <- ifelse(is.na(data.hospitalisations.merged.maxFU$cause.planned), 
                                                         0, 
                                                         data.hospitalisations.merged.maxFU$cause.planned)
cause.planned.pp.maxFU <- aggregate(data.hospitalisations.merged.maxFU$cause.planned, 
                                  by = list(data.hospitalisations.merged.maxFU$id), 
                                  FUN = sum)
table(cause.planned.pp.maxFU[, 2], exclude = NULL)

table(data.hospitalisations.merged.maxFU$cause.other, exclude = NULL)
data.hospitalisations.merged.maxFU$cause.other <- ifelse(is.na(data.hospitalisations.merged.maxFU$cause.other), 
                                                       0, 
                                                       data.hospitalisations.merged.maxFU$cause.other)
cause.other.pp.maxFU <- aggregate(data.hospitalisations.merged.maxFU$cause.other, 
                                by = list(data.hospitalisations.merged.maxFU$id), 
                                FUN = sum)
table(cause.other.pp.maxFU[, 2], exclude = NULL)

table(is.na(data.hospitalisations.merged.maxFU$Dx.admission))
table(is.na(data.hospitalisations.merged.maxFU$Dx.discharge))
table(is.na(data.hospitalisations.merged.maxFU$Dx.admission), is.na(data.hospitalisations.merged.maxFU$Dx.discharge))
#View(subset(data.hospitalisations.merged.maxFU, !is.na(Dx.admission) & is.na(Dx.discharge)))
##3 patients with admission date before maxFU days after baseline, and no discharge date. Most likely: 1 stayed in hospital, reason for stay changed; 1 discharge after trial ended; 1 passed away in hospital. 
data.hospitalisations.merged.maxFU$Dx.discharge <- ifelse( (data.hospitalisations.merged.maxFU$id == "{782C9C2B-EE5A-4B9A-BA4C-D549D0C0A690}") 
                                                          & 
                                                          (data.hospitalisations.merged.maxFU$Dx.admission == 536), 
                                                          587, 
                                                          data.hospitalisations.merged.maxFU$Dx.discharge) 
data.hospitalisations.merged.maxFU$Dx.discharge <- ifelse( (data.hospitalisations.merged.maxFU$id == "{C56F4698-75F0-40D2-8E4F-3C492F24CA4A}") 
                                                          & 
                                                          (data.hospitalisations.merged.maxFU$Dx.admission == 502), 
                                                          550, 
                                                          data.hospitalisations.merged.maxFU$Dx.discharge) 
data.hospitalisations.merged.maxFU$Dx.discharge <- ifelse( (data.hospitalisations.merged.maxFU$id == "{D4DEA51C-3010-48B7-B2E1-454A57BACA71}") 
                                                          & 
                                                          (data.hospitalisations.merged.maxFU$Dx.admission == 523), 
                                                          534, 
                                                          data.hospitalisations.merged.maxFU$Dx.discharge) 
table(is.na(data.hospitalisations.merged.maxFU$Dx.deceased))

data.hospitalisations.merged.maxFU$cause <- ifelse(data.hospitalisations.merged.maxFU$cause.CHF == 1, 
                                                 "CHF", 
                                                 NA)
data.hospitalisations.merged.maxFU$cause <- ifelse(data.hospitalisations.merged.maxFU$cause.planned == 1, 
                                                 "planned", 
                                                 data.hospitalisations.merged.maxFU$cause)
data.hospitalisations.merged.maxFU$cause <- ifelse(data.hospitalisations.merged.maxFU$cause.other == 1, 
                                                 "other", 
                                                 data.hospitalisations.merged.maxFU$cause)
data.hospitalisations.merged.maxFU$cause <- factor(data.hospitalisations.merged.maxFU$cause)

data.hospitalisations.merged.maxFU$Dx.discharge.plot <- ifelse( data.hospitalisations.merged.maxFU$Dx.discharge > maxFU, 
                                                              maxFU, 
                                                              data.hospitalisations.merged.maxFU$Dx.discharge)

ggplot() + 
  geom_segment(data = data.hospitalisations.merged.maxFU, aes(x = Dx.admission, xend = Dx.discharge.plot, y = id, yend = id, colour = cause)) + 
  geom_segment(data = data.hospitalisations.merged.maxFU, aes(x = Dx.deceased.maxFU, xend = maxFU, y = id, yend = id, colour = "deceased")) + 
  scale_x_continuous(name = "Days from baseline", 
                     breaks = seq(0, maxFU, 30), 
                     labels = seq(0, maxFU, 30)) + 
  scale_y_discrete(name = "ID", 
                   labels = NULL) + 
  scale_colour_manual("Legend", 
                      breaks = c("deceased", 
                                 "CHF", 
                                 "planned", 
                                 "other"), 
                      values = c("deceased" = "black", 
                                 "CHF" = "red3", 
                                 "planned" = "deepskyblue", 
                                 "other" = "pink"), 
                      labels = c("Deceased", 
                                 "Hospitalisation due to chronic heart failure", 
                                 "Planned hospitalisation", 
                                 "Hospitalisation due to other causes")) +
  theme_classic()

####################################################################################################################

# Loop diuretics excluding hospitalisation days due to all causes

## Dose change: present day - subsequent day

id.Dx <- id.Dx.dose[,c("id","Dx")]

id.Dx.hospitalisation_indicator <- merge(id.Dx, data.hospitalisations.merged.maxFU[, c("id", 
                                                                                     "Dx.admission", 
                                                                                     "Dx.discharge")], 
                                         by = "id", 
                                         all.x = TRUE, 
                                         all.y = TRUE)

id.Dx.hospitalisation_indicator$hospitalisation_indicator <- 0
id.Dx.hospitalisation_indicator$hospitalisation_indicator <- ifelse( (id.Dx.hospitalisation_indicator$Dx >= id.Dx.hospitalisation_indicator$Dx.admission) 
                                                                      & 
                                                                      (id.Dx.hospitalisation_indicator$Dx <= id.Dx.hospitalisation_indicator$Dx.discharge), 
                                                                      1, 
                                                                      id.Dx.hospitalisation_indicator$hospitalisation_indicator) 

id.Dx.hospitalisation_indicator <- table(id.Dx.hospitalisation_indicator$id, id.Dx.hospitalisation_indicator$Dx, 
                                         id.Dx.hospitalisation_indicator$hospitalisation_indicator) 
id.Dx.hospitalisation_indicator <- as.data.frame(id.Dx.hospitalisation_indicator)
colnames(id.Dx.hospitalisation_indicator) <- c("id", 
                                               "Dx", 
                                               "hospitalisation_indicator", 
                                               "freq")
id.Dx.hospitalisation_indicator <- id.Dx.hospitalisation_indicator[id.Dx.hospitalisation_indicator$freq != 0, ]
id.Dx.hospitalisation_indicator$duplicated <- duplicated(id.Dx.hospitalisation_indicator[c("id","Dx")], 
                                                         fromLast = TRUE)
id.Dx.hospitalisation_indicator$to_remove <- ifelse( (id.Dx.hospitalisation_indicator$duplicated == TRUE) 
                                                     & 
                                                     (id.Dx.hospitalisation_indicator$hospitalisation_indicator == 0), 
                                                     TRUE, 
                                                     FALSE)
id.Dx.hospitalisation_indicator <- id.Dx.hospitalisation_indicator[!(id.Dx.hospitalisation_indicator$to_remove == TRUE), ]
id.Dx.hospitalisation_indicator <- id.Dx.hospitalisation_indicator[,c("id",
                                                                      "Dx",
                                                                      "hospitalisation_indicator")]
id.Dx.hospitalisation_indicator$Dx <- as.numeric(as.character(id.Dx.hospitalisation_indicator$Dx))

id.Dx.dose.hospitalisation_indicator <- merge(id.Dx.dose, 
                                              id.Dx.hospitalisation_indicator, 
                                              by = c("id", "Dx"), 
                                              all.x = TRUE, 
                                              all.y = TRUE)
id.Dx.dose.hospitalisation_indicator$hospitalisation_indicator <- as.numeric(as.character(id.Dx.dose.hospitalisation_indicator$hospitalisation_indicator))
id.Dx.dose.hospitalisation_indicator$hospitalisation_indicator <- ifelse(is.na(id.Dx.dose.hospitalisation_indicator$hospitalisation_indicator), 
                                                                         0, 
                                                                         id.Dx.dose.hospitalisation_indicator$hospitalisation_indicator)

data.merged.maxFU <- merge(id.Dx.dose.hospitalisation_indicator, 
                         id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, 
                         by = c("id", "Dx"), 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.maxFU <- merge(data.merged.maxFU, 
                         unique(data.hospitalisations.merged.maxFU[, c("id", 
                                                                     "Dx.deceased")]), 
                         by = "id", 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.maxFU <- data.merged.maxFU[order(data.merged.maxFU$id, data.merged.maxFU$Dx),]

data.merged.maxFU$hospitalisation_indicator <- as.numeric(as.character(data.merged.maxFU$hospitalisation_indicator))
data.merged.maxFU$hospitalisation_indicator <- ifelse(is.na(data.merged.maxFU$hospitalisation_indicator), 
                                                    0, 
                                                    data.merged.maxFU$hospitalisation_indicator)
data.merged.maxFU$deceased_indicator <- ifelse( (data.merged.maxFU$Dx >= data.merged.maxFU$Dx.deceased), 
                                              1, 
                                              0)
data.merged.maxFU$deceased_indicator <- ifelse( is.na(data.merged.maxFU$deceased_indicator), 
                                              0, 
                                              data.merged.maxFU$deceased_indicator)

write.csv(data.merged.maxFU, 'data.merged.maxFU.csv')

data.merged.maxFU.plot <- data.merged.maxFU 
data.merged.maxFU.plot$plot <- as.character(data.merged.maxFU.plot$dose_change.qualitative_indicator)
data.merged.maxFU.plot$plot <- ifelse(data.merged.maxFU.plot$hospitalisation_indicator == 1, 
                                    "Hospitalisation", 
                                    data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- ifelse(data.merged.maxFU.plot$deceased_indicator == 1,  
                                    "Deceased", 
                                    data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- factor(data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- factor(data.merged.maxFU.plot$plot, levels = c("Up-titration", 
                                                                          "No change", 
                                                                          "Down-titration", 
                                                                          "Hospitalisation", 
                                                                          "Deceased", 
                                                                          "Missing"))

data.merged.maxFU.plot <- data.merged.maxFU.plot[!is.na(data.merged.maxFU.plot$plot),]

data.merged.maxFU.plot <- subset(data.merged.maxFU.plot, Dx != maxFU)

ggplot(data.merged.maxFU.plot, aes(x = Dx, y = id, fill = plot)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, 539), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                "Day 539 - maxFU")) + 
  scale_y_discrete(name = "ID", 
                   labels = NULL) + 
  scale_fill_manual(name = "Legend", 
                    values = c("pink", 
                               "deepskyblue4", 
                               "deepskyblue", 
                               "red3", 
                               "black", 
                               "dimgrey")) + 
  theme(axis.text.x = element_text(angle = 90))

####################################################################################################################

# Loop diuretics excluding hospitalisation days due to congestive heart failure 


## Dose change: present day - subsequent day

id.Dx <- id.Dx.dose[,c("id","Dx")]

id.Dx.CHF_hospitalisation_indicator <- merge(id.Dx, 
                                             subset(data.hospitalisations.merged.maxFU, cause.CHF == 1)[, c("id", 
                                                                                                          "Dx.admission", 
                                                                                                          "Dx.discharge")], 
                                             by = "id", 
                                             all.x = TRUE, 
                                             all.y = TRUE)

id.Dx.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator <- 0
id.Dx.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator <- ifelse( (id.Dx.CHF_hospitalisation_indicator$Dx >= id.Dx.CHF_hospitalisation_indicator$Dx.admission) 
                                                                             & 
                                                                             (id.Dx.CHF_hospitalisation_indicator$Dx <= id.Dx.CHF_hospitalisation_indicator$Dx.discharge), 
                                                                             1, 
                                                                             id.Dx.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator) 

id.Dx.CHF_hospitalisation_indicator <- table(id.Dx.CHF_hospitalisation_indicator$id, id.Dx.CHF_hospitalisation_indicator$Dx, 
                                             id.Dx.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator) 
id.Dx.CHF_hospitalisation_indicator <- as.data.frame(id.Dx.CHF_hospitalisation_indicator)
colnames(id.Dx.CHF_hospitalisation_indicator) <- c("id", "Dx", "CHF_hospitalisation_indicator", "freq")
id.Dx.CHF_hospitalisation_indicator <- id.Dx.CHF_hospitalisation_indicator[id.Dx.CHF_hospitalisation_indicator$freq != 0, ]
id.Dx.CHF_hospitalisation_indicator$duplicated <- duplicated(id.Dx.CHF_hospitalisation_indicator[c("id","Dx")], 
                                                             fromLast = TRUE)
id.Dx.CHF_hospitalisation_indicator$to_remove <- ifelse( (id.Dx.CHF_hospitalisation_indicator$duplicated == TRUE) 
                                                         & 
                                                         (id.Dx.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator == 0), 
                                                         TRUE, 
                                                         FALSE)
id.Dx.CHF_hospitalisation_indicator <- id.Dx.CHF_hospitalisation_indicator[!(id.Dx.CHF_hospitalisation_indicator$to_remove == TRUE), ]
id.Dx.CHF_hospitalisation_indicator <- id.Dx.CHF_hospitalisation_indicator[, c("id",
                                                                               "Dx",
                                                                               "CHF_hospitalisation_indicator")]
id.Dx.CHF_hospitalisation_indicator$Dx <- as.numeric(as.character(id.Dx.CHF_hospitalisation_indicator$Dx))

id.Dx.dose.CHF_hospitalisation_indicator <- merge(id.Dx.dose, 
                                                  id.Dx.CHF_hospitalisation_indicator, 
                                                  by = c("id", 
                                                         "Dx"), 
                                                  all.x = TRUE, 
                                                  all.y = TRUE)
id.Dx.dose.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator <- as.numeric(as.character(id.Dx.dose.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator))
id.Dx.dose.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator <- ifelse(is.na(id.Dx.dose.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator), 
                                                                                 0, 
                                                                                 id.Dx.dose.CHF_hospitalisation_indicator$CHF_hospitalisation_indicator)

data.merged.maxFU <- merge(id.Dx.dose.CHF_hospitalisation_indicator, 
                         id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, 
                         by = c("id", 
                                "Dx"), 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.maxFU <- merge(data.merged.maxFU, 
                         unique(data.hospitalisations.merged.maxFU[, c("id", 
                                                                     "Dx.deceased")]), 
                         by = "id", 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.maxFU <- data.merged.maxFU[order(data.merged.maxFU$id, data.merged.maxFU$Dx),]

data.merged.maxFU$CHF_hospitalisation_indicator <- as.numeric(as.character(data.merged.maxFU$CHF_hospitalisation_indicator))
data.merged.maxFU$CHF_hospitalisation_indicator <- ifelse(is.na(data.merged.maxFU$CHF_hospitalisation_indicator), 
                                                        0, 
                                                        data.merged.maxFU$CHF_hospitalisation_indicator)
data.merged.maxFU$deceased_indicator <- ifelse( (data.merged.maxFU$Dx >= data.merged.maxFU$Dx.deceased), 
                                              1, 
                                              0)
data.merged.maxFU$deceased_indicator <- ifelse( is.na(data.merged.maxFU$deceased_indicator), 
                                              0, 
                                              data.merged.maxFU$deceased_indicator)

data.merged.maxFU.plot <- data.merged.maxFU 
data.merged.maxFU.plot$plot <- as.character(data.merged.maxFU.plot$dose_change.qualitative_indicator)
data.merged.maxFU.plot$plot <- ifelse(data.merged.maxFU.plot$CHF_hospitalisation_indicator == 1, 
                                    "Hospitalisation: CHF", 
                                    data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- ifelse(data.merged.maxFU.plot$deceased_indicator == 1,  
                                    "Deceased", 
                                    data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- factor(data.merged.maxFU.plot$plot)
data.merged.maxFU.plot$plot <- factor(data.merged.maxFU.plot$plot, levels = c("Up-titration", 
                                                                          "No change", 
                                                                          "Down-titration", 
                                                                          "Hospitalisation: CHF", 
                                                                          "Deceased",
                                                                          "Missing"))

data.merged.maxFU.plot <- data.merged.maxFU.plot[!is.na(data.merged.maxFU.plot$plot), ]

data.merged.maxFU.plot <- subset(data.merged.maxFU.plot, Dx != maxFU)

ggplot(data.merged.maxFU.plot, aes(x = Dx, y = id, fill = plot)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, 539), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                "Day 539 - maxFU")) + 
  scale_y_discrete(name = "ID", labels = NULL) + 
  scale_fill_manual(name = "Legend", 
                    values = c("pink", 
                               "deepskyblue4", 
                               "deepskyblue", 
                               "red3", 
                               "black", 
                               "dimgrey")) + 
  theme(axis.text.x = element_text(angle = 90))

####################################################################################################################

# 30-day slots

data.timeslots <- data.merged.maxFU

data.timeslots$stop.up <- ifelse(data.timeslots$dose_change.qualitative_indicator == "Up-titration",
                                 "Up-titration",
                                 0)
data.timeslots$stop.down <- ifelse(data.timeslots$dose_change.qualitative_indicator == "Down-titration",
                                   "Down-titration",
                                   0)
data.timeslots$stop.CHF_hospitalisation <- ifelse(data.timeslots$CHF_hospitalisation_indicator == 1,
                                                  "Hospitalisation: CHF",
                                                  0)
data.timeslots$stop.deceased <- ifelse(data.timeslots$deceased_indicator == 1,
                                       "Deceased",
                                       0)
data.timeslots$stop <- data.timeslots$stop.up
data.timeslots$stop <- ifelse(data.timeslots$stop.down == "Down-titration",
                              "Down-titration",
                              data.timeslots$stop)
data.timeslots$stop <- ifelse(data.timeslots$stop.CHF_hospitalisation == "Hospitalisation: CHF",
                              "Hospitalisation: CHF",
                              data.timeslots$stop)
data.timeslots$stop <- ifelse(data.timeslots$stop.deceased == "Deceased",
                              "Deceased",
                              data.timeslots$stop)
data.timeslots$stop <- ifelse(data.timeslots$stop == 0,
                              "No change",
                              data.timeslots$stop)

counts.list <- tapply(data.timeslots$stop, list(data.timeslots$id), FUN = rle)
counts <- rbindlist(counts.list, use.names = TRUE, idcol = "id")

#

counts$length_above_30 <- apply(counts, 1, function(counts) { as.numeric(counts[2]) / 30 })
counts$length_above_30 <- ifelse(counts$length_above_30 <= 1,
                                 FALSE,
                                 TRUE)

counts.length_below_or_equal_to_30 <- subset(counts, length_above_30 == FALSE)
counts.length_above_30 <- subset(counts, length_above_30 == TRUE)

counts.length_above_30$length_minus_30 <- counts.length_above_30$lengths - 30

id.lengths <- counts.length_above_30[, c("id", "length_minus_30")] 
colnames(id.lengths)[2] <- "lengths" 

counts.30_day_lengths <- rbind(counts.length_below_or_equal_to_30, 
                               id.lengths,
                               fill = TRUE)

################################################################################
### Dose change indications

data.indications.pivotted <- pivot_longer(data.indications[,c(1,6:73)], !id, names_to = "Indication", values_to = "Value")
data.indications.pivotted <- na.omit(data.indications.pivotted)
data.indications.pivotted$Number <- str_replace(data.indications.pivotted$Indication, "D|Ind", "")
data.indications.pivotted$Column <- str_replace(data.indications.pivotted$Indication, "\\d+", "")
data.indications.pivotted$Column <- str_replace(data.indications.pivotted$Column, "ind", "Ind")
data.indications.pivotted <- pivot_wider(data.indications.pivotted[,c(1,3:5)], c(id, Number), names_from = "Column", values_from = "Value")
data.indications.pivotted$Reason <- data.indications.def$Reason[data.indications.pivotted$Ind]

################################################################################

# counts_periods
# id  | baseline_state | end_state      | length
# xxx | no_change      | down_titration |  5
# xxx | down_titration | no_change      | 30
# xxx | no_change      | no_change      | 30
# xxx | no_change      | down_titration | 10
# 

#time_horizons <- c(5, 7, 10, 15, 20, 25, 30)
#for time_horizon in time_horizons
#{
  sink("log_counts_periods.txt");
  i <- 1
  counts_periods <- data.frame(matrix(ncol = 5, nrow = 0))
  names_counts_periods <- c("id", "baseline_state", "end_state", "length", "indication")
  colnames(counts_periods) <- names_counts_periods
  time_horizon <- 30
  
  while(i < nrow(counts)) {
    current_id = counts$id[i];
    current_state <- counts$values[i]
    next_state <- counts$values[i+1]
    current_day <- 0
    # loop through different ids
    while(!is.na(current_state) && current_state != "Deceased")
    {
      current_timespan <- counts$lengths[i]
      length_timespan <- current_timespan
      current_day <- current_day + current_timespan
      cat(paste0("Id:", current_id ,"; current state:", current_state, "; Next state:", next_state, "; Length: ", length_timespan, "\n"))
      # divide long time spans into X day time spans
      while(length_timespan > 0 && !is.na(current_state))
      {
        
        if(current_state == "No change")
        {
          # Add row to data frame
          end_state <- ifelse(length_timespan>time_horizon, current_state, next_state)
          current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)], "Not found")) # paste0("Not found: ", (current_day+1))))
          cat(paste0(">> Current indication: ", current_indication, "(",current_day ,")","\n"))
          counts_periods<- rbind(counts_periods, data.frame(id = current_id, baseline_state = current_state, end_state = end_state, length= min(length_timespan, time_horizon), indication = ifelse(end_state == "Down-titration" || end_state == "Up-titration", current_indication, NA)));
          cat(paste0(">> Adding row (1): id: ", current_id,"; baseline state:", current_state, "; End state:", end_state, "; Length: ", min(length_timespan, time_horizon)),"\n")
        }
        else if (current_state == "Down-titration" || current_state == "Up-titration")
        {
          if(!is.na(next_state) && next_state == "No change")
          {
            cat(paste0("Id:", current_id ,"; current state:", next_state, "; Next state:", "?", "; Length: ", counts$lengths[i+1], "\n"))
            # Sum current down titration + no_change days, unless it's a hospitalisation
            length_timespan <- length_timespan + counts$lengths[i+1];
            # If the combined timespan is shorther than time_horizon days, look in the next row
            if(length_timespan <= time_horizon)
            {
              # skip the "no change" line
              i <- i + 1;
              next_state <- counts$values[i+1];
              current_day <- current_day + counts$lengths[i]
            }
            
            # Add row to data frame
            current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)],  "Not found")) #paste0("Not found: ", (current_day+1))))
            cat(paste0(">> Current indication: ", current_indication, "(",current_day ,")","\n"))
            counts_periods<- rbind(counts_periods, data.frame(id = current_id, baseline_state = current_state, end_state = next_state, length= min(length_timespan, time_horizon),  indication = ifelse(next_state == "Down-titration" || next_state == "Up-titration", current_indication, NA)));
            cat(paste0(">> Adding row (2): id: ", current_id,"; baseline state:", current_state, "; End state:", next_state, "; Length: ", min(length_timespan, time_horizon), "\n"))
            
            # If the "no change" time span is > time_horizon days, next row should be "no change" with the updated time span length 
            if(length_timespan > time_horizon)
            {
              i <- i + 1;
              current_state = counts$values[i];
              next_state <- counts$values[i+1];
              current_day <- current_day + counts$lengths[i]
            }
          }else
          {
            # Add row to data frame
            current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)],  "Not found")) #paste0("Not found: ", (current_day+1))))
            cat(paste0(">> Current indication: ", current_indication, "(",current_day ,")","\n"))
            counts_periods<- rbind(counts_periods, data.frame(id = current_id, baseline_state = current_state, end_state = next_state, length= min(length_timespan, time_horizon),  indication = ifelse(next_state == "Down-titration" || next_state == "Up-titration", current_indication, NA)));
            cat(paste0(">> Adding row (3): id: ", current_id,"; baseline state:", current_state, "; End state:", next_state, "; Length: ", min(length_timespan, time_horizon), "\n"))
          }
          
        } else if (current_state == "Hospitalisation: CHF" && next_state == "Deceased")
        {
          counts_periods<- rbind(counts_periods, data.frame(id = current_id, baseline_state = current_state, end_state = next_state, length= length_timespan,  indication = ""));
          cat(paste0(">> Adding row (4): id: ", current_id,"; baseline state:", current_state, "; End state:", next_state, "; Length: ", length_timespan, "\n"))
          length_timespan <- 0
        }
        length_timespan <- length_timespan - time_horizon;
      }
      i<-i+1;
      current_state = counts$values[i];
      next_state <- counts$values[i+1];
    }
    i<-i+1;
  }
  
  sink()
  #}
# Remove NAs? (censored) datapoints?

counts_periods_complete <- na.omit(counts_periods)

################# 

counts_per_participant <- counts %>% count(id, values)

h = hist(counts_per_participant$n[counts_per_participant$values == "Down-titration"])
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, xlab = "Number of down-titrations per participant", main = "Histogram of number of down-titrations per participant", ylab="Percentage", col="#ce1831")

h = hist(counts_per_participant$n[counts_per_participant$values == "Up-titration"], )
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, xlab = "Number of up-titrations per participant", main = "Histogram of number of up-titrations per participant", ylab="Percentage", col="#ce1831")

hist(counts_per_participant$n[counts_per_participant$values == "Hospitalisation: CHF"])

percentage_no_up_titration = 1 - (sum(counts_per_participant$n[counts_per_participant$values == "Up-titration"] > 0, na.rm = TRUE))/length(unique(counts_per_participant$id));
percentage_no_down_titration = 1 - (sum(counts_per_participant$n[counts_per_participant$values == "Down-titration"] > 0, na.rm = TRUE))/length(unique(counts_per_participant$id));

max(counts_per_participant$n[counts_per_participant$values == "Up-titration"], na.rm = TRUE);
max(counts_per_participant$n[counts_per_participant$values == "Down-titration"], na.rm = TRUE);

#####################
# Conditional probability tables

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

####################################
# Analysis of determinants of background fluid state: cause of CHF, age and gender

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


######################################
# For each visit, find whether hospitalisation, dose change or death happened in the next X days
time_horizon <- 30

dose_change_days <- id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx[id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$dose_change.qualitative_indicator == 'Up-titration' | 
                                                                          id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$dose_change.qualitative_indicator == 'Down-titration' ,]
dose_change_days$Dx <- dose_change_days$Dx+1
data.dose_change_after_visit <- data.frame(data$id, BL = rep(NA, nrow(data)), V1 = rep(NA, nrow(data)), V3 = rep(NA, nrow(data)), V6 = rep(NA, nrow(data)), V12 = rep(NA, nrow(data)), V18 = rep(NA, nrow(data)),
                                           BL_change = rep(NA, nrow(data)), V1_change = rep(NA, nrow(data)), V3_change = rep(NA, nrow(data)), V6_change = rep(NA, nrow(data)), V12_change = rep(NA, nrow(data)), V18_change = rep(NA, nrow(data)))


data.hospit_CHF_after_visit <- data.frame(data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.hospit_renal_after_visit <- data.frame(data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.hospit_linked_after_visit <- data.frame(data$id, BL = rep(0, nrow(data)), V1 = ifelse(is.na(data$V1), NA, 0), V3 = ifelse(is.na(data$V3), NA, 0), V6 = ifelse(is.na(data$V6), NA, 0), V12 = ifelse(is.na(data$V12), NA, 0), V18 = ifelse(is.na(data$V18), NA, 0))
data.death_after_visit <- data.frame(data$id, death = as.numeric(difftime(data$Dateofdeath, data$RefDate, units = "days")), BL = rep(NA, nrow(data)), V1 = rep(NA, nrow(data)), V3 = rep(NA, nrow(data)), V6 = rep(NA, nrow(data)), V12 = rep(NA, nrow(data)), V18 = rep(NA, nrow(data)))

data.death_after_visit$BL <- ifelse(data$Death == 0 | data.death_after_visit$death > time_horizon, 0,1)

data.death_after_visit$V1 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V1 |
                                      data.death_after_visit$death > time_horizon, 0, data.death_after_visit$death - data$V1)
data.death_after_visit$V3 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V3 |
                                      data.death_after_visit$death > data$V3 + time_horizon, 0, data.death_after_visit$death - data$V3)
data.death_after_visit$V6 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V6 |
                                      data.death_after_visit$death > data$V6 + time_horizon, 0, data.death_after_visit$death - data$V6)
data.death_after_visit$V12 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V12 |
                                       data.death_after_visit$death > data$V12 + time_horizon, 0, data.death_after_visit$death - data$V12)
data.death_after_visit$V18 <- ifelse(data$Death == 0 | data.death_after_visit$death < data$V18 |
                                       data.death_after_visit$death > data$V18 + time_horizon, 0, data.death_after_visit$death - data$V18)

for(i in 1:nrow(data))
{
  current_id <- data$id[i];
  
  # Day of first dose change after follow up visit
  data.dose_change_after_visit$BL[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$diff_refdate_entrydate[i] & Dx <= time_horizon ]), na.rm = TRUE);
  data.dose_change_after_visit$V1[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V1[data$id == current_id] & Dx <= data$V1[data$id == current_id]+time_horizon]), na.rm = TRUE);
  data.dose_change_after_visit$V3[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V3[data$id == current_id] & Dx <= data$V3[data$id == current_id]+time_horizon]), na.rm = TRUE);
  data.dose_change_after_visit$V6[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V6[data$id == current_id] & Dx <= data$V6[data$id == current_id]+time_horizon]), na.rm = TRUE);
  data.dose_change_after_visit$V12[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V12[data$id == current_id] & Dx <= data$V12[data$id == current_id]+time_horizon]), na.rm = TRUE);
  data.dose_change_after_visit$V18[i] <- min(with(dose_change_days, Dx[id == current_id & Dx > data$V18[data$id == current_id] & Dx <= data$V18[data$id == current_id]+time_horizon]), na.rm = TRUE);
  
  # Direction of change
  data.dose_change_after_visit$BL_change[i] <- ifelse(is.infinite(data.dose_change_after_visit$BL[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$BL[i]])));
  data.dose_change_after_visit$V1_change[i] <- ifelse(is.na(data.dose_change_after_visit$V1[i]), NA, ifelse(is.infinite(data.dose_change_after_visit$V1[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$V1[i]]))));
  data.dose_change_after_visit$V3_change[i] <- ifelse(is.na(data.dose_change_after_visit$V3[i]), NA, ifelse(is.infinite(data.dose_change_after_visit$V3[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$V3[i]]))));
  data.dose_change_after_visit$V6_change[i] <- ifelse(is.na(data.dose_change_after_visit$V6[i]), NA, ifelse(is.infinite(data.dose_change_after_visit$V6[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$V6[i]]))));
  data.dose_change_after_visit$V12_change[i] <- ifelse(is.na(data.dose_change_after_visit$V12[i]), NA, ifelse(is.infinite(data.dose_change_after_visit$V12[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$V12[i]]))));
  data.dose_change_after_visit$V18_change[i] <- ifelse(is.na(data.dose_change_after_visit$V18[i]), NA, ifelse(is.infinite(data.dose_change_after_visit$V18[i]), 'No change', 
                                                      with(dose_change_days, as.character(dose_change.qualitative_indicator[id == current_id & Dx == data.dose_change_after_visit$V18[i]]))));
  # Hospitalisation: Worsening CHF
  data.hospit_CHF_after_visit$BL[i] <-dplyr::first(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF] > data$diff_refdate_entrydate[i] & 
                                                    Dx.admission[id == current_id & cause.CHF] <= data$diff_refdate_entrydate[i] + time_horizon), default = 0);
  # If the followup is shorter than the time horizon, censor it
  data.hospit_CHF_after_visit$BL[i] <- ifelse(data.hospit_CHF_after_visit$BL[i] == 0 & data$FU[i] < time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$BL[i]),0, data.hospit_CHF_after_visit$BL[i]))
    
  data.hospit_CHF_after_visit$V1[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF == 1] > data$V1[data$id == current_id] & 
                                                         Dx.admission[id == current_id & cause.CHF] <= data$V1[data$id == current_id] + time_horizon)), 1,0);
  data.hospit_CHF_after_visit$V1[i] <- ifelse(data.hospit_CHF_after_visit$V1[i] != 1 & data$FU[i] < data$V1[i] + time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$V1[i]),0, data.hospit_CHF_after_visit$V1[i]))
  data.hospit_CHF_after_visit$V3[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF == 1] > data$V3[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.CHF] <= data$V3[data$id == current_id] + time_horizon)), 1,0);
  data.hospit_CHF_after_visit$V3[i] <- ifelse(data.hospit_CHF_after_visit$V3[i] != 1 & data$FU[i] < data$V3[i] + time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$V3[i]),0, data.hospit_CHF_after_visit$V3[i]))
  data.hospit_CHF_after_visit$V6[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF == 1] > data$V6[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.CHF] <= data$V6[data$id == current_id] + time_horizon)), 1,0);
  data.hospit_CHF_after_visit$V6[i] <- ifelse(data.hospit_CHF_after_visit$V6[i] != 1 & data$FU[i] < data$V6[i] + time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$V6[i]),0, data.hospit_CHF_after_visit$V6[i]))
  data.hospit_CHF_after_visit$V12[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF == 1] > data$V12[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.CHF] <= data$V12[data$id == current_id] + time_horizon)), 1,0);
  data.hospit_CHF_after_visit$V12[i] <- ifelse(data.hospit_CHF_after_visit$V12[i] != 1 & data$FU[i] < data$V12[i] + time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$V12[i]),0, data.hospit_CHF_after_visit$V12[i]))
  data.hospit_CHF_after_visit$V18[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.CHF == 1] > data$V18[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.CHF] <= data$V18[data$id == current_id] + time_horizon)), 1,0);
  data.hospit_CHF_after_visit$V18[i] <- ifelse(data.hospit_CHF_after_visit$V18[i] != 1 & data$FU[i] < data$V18[i] + time_horizon, NA, ifelse(is.na(data.hospit_CHF_after_visit$V18[i]),0, data.hospit_CHF_after_visit$V18[i]))
  
  # Hospitalisation: renal  
  data.hospit_renal_after_visit$BL[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal] <= time_horizon)), 1,0);
  data.hospit_renal_after_visit$BL[i] <- ifelse(data$FU[i] < time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$BL[i]),0, data.hospit_renal_after_visit$BL[i]))
  data.hospit_renal_after_visit$V1[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal == 1] > data$V1[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.renal] <= (data$V1[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_renal_after_visit$V1[i] <- ifelse(data$FU[i] < data$V1[i] + time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$V1[i]),0, data.hospit_renal_after_visit$V1[i]))
  data.hospit_renal_after_visit$V3[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal == 1] > data$V3[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.renal] <= (data$V3[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_renal_after_visit$V3[i] <- ifelse(data$FU[i] < data$V3[i] + time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$V3[i]),0, data.hospit_renal_after_visit$V3[i]))
  data.hospit_renal_after_visit$V6[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal == 1] > data$V6[data$id == current_id] &
                                                         Dx.admission[id == current_id & cause.renal] <= (data$V6[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_renal_after_visit$V6[i] <- ifelse(data$FU[i] < data$V6[i] + time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$V6[i]),0, data.hospit_renal_after_visit$V6[i]))
  data.hospit_renal_after_visit$V12[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal == 1] > data$V12[data$id == current_id] &
                                                          Dx.admission[id == current_id & cause.renal] <= (data$V12[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_renal_after_visit$V12[i] <- ifelse(data$FU[i] < data$V12[i] + time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$V12[i]),0, data.hospit_renal_after_visit$V12[i]))
  data.hospit_renal_after_visit$V18[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.renal == 1] > data$V18[data$id == current_id] &
                                                          Dx.admission[id == current_id & cause.renal] <= (data$V18[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_renal_after_visit$V18[i] <- ifelse(data$FU[i] < data$V18[i] + time_horizon, NA, ifelse(is.na(data.hospit_renal_after_visit$V18[i]),0, data.hospit_renal_after_visit$V18[i]))

  # Hospitalisation: linked  
  data.hospit_linked_after_visit$BL[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked] <= time_horizon)), 1,0);
  data.hospit_linked_after_visit$BL[i] <- ifelse(data$FU[i] < time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$BL[i]),0, data.hospit_linked_after_visit$BL[i]))
  data.hospit_linked_after_visit$V1[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked == 1] > data$V1[data$id == current_id] &
                                                           Dx.admission[id == current_id & cause.linked] <= (data$V1[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_linked_after_visit$V1[i] <- ifelse(data$FU[i] < data$V1[i] + time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$V1[i]),0, data.hospit_linked_after_visit$V1[i]))
  data.hospit_linked_after_visit$V3[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked == 1] > data$V3[data$id == current_id] &
                                                           Dx.admission[id == current_id & cause.linked] <= (data$V3[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_linked_after_visit$V3[i] <- ifelse(data$FU[i] < data$V3[i] + time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$V3[i]),0, data.hospit_linked_after_visit$V3[i]))
  data.hospit_linked_after_visit$V6[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked == 1] > data$V6[data$id == current_id] &
                                                           Dx.admission[id == current_id & cause.linked] <= (data$V6[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_linked_after_visit$V6[i] <- ifelse(data$FU[i] < data$V6[i] + time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$V6[i]),0, data.hospit_linked_after_visit$V6[i]))
  data.hospit_linked_after_visit$V12[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked == 1] > data$V12[data$id == current_id] &
                                                            Dx.admission[id == current_id & cause.linked] <= (data$V12[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_linked_after_visit$V12[i] <- ifelse(data$FU[i] < data$V12[i] + time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$V12[i]),0, data.hospit_linked_after_visit$V12[i]))
  data.hospit_linked_after_visit$V18[i] <- ifelse(any(with(data.hospitalisations.merged, Dx.admission[id == current_id & cause.linked == 1] > data$V18[data$id == current_id] &
                                                            Dx.admission[id == current_id & cause.linked] <= (data$V18[data$id == current_id] + time_horizon))), 1,0);
  data.hospit_linked_after_visit$V18[i] <- ifelse(data$FU[i] < data$V18[i] + time_horizon, NA, ifelse(is.na(data.hospit_linked_after_visit$V18[i]),0, data.hospit_linked_after_visit$V18[i]))
  
}

data.dose_change_after_visit$V1 <- data.dose_change_after_visit$V1 - data$V1
data.dose_change_after_visit$V3 <- data.dose_change_after_visit$V3 - data$V3
data.dose_change_after_visit$V6 <- data.dose_change_after_visit$V6 - data$V6
data.dose_change_after_visit$V12 <- data.dose_change_after_visit$V12 - data$V12
data.dose_change_after_visit$V18 <- data.dose_change_after_visit$V18 - data$V18


######################################
# Transform the data to feed HUGIN
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


# Data elements
# Immutable elements
pat_id <- rep(data$id, 6)
Gender <- rep(data$Gender_cat, 6)
Cause_HF <- rep(to_character(data$Cause_HF), 6)

                
# Initialise dataframe
data.h <- data.frame(pat_id, Gender, Cause_HF)
                
# Visit data
#fluid_level <- with(data.congention_score, c(CCS, CCS1, CCS3, CCS6, CCS12, CCS18))

fluid_level <- with(data, c(Own_congestionscore_BL,
                            Own_congestionscore_1,
                            Own_congestionscore_3,
                            Own_congestionscore_6,
                            Own_congestionscore_12,
                            Own_congestionscore_18))
# fluid_level <- with(data.new_congestion_scores,
#                     c(Clinical_congestion_score_BL,
#                       Clinical_congestion_score_1,
#                       Clinical_congestion_score_3,
#                       Clinical_congestion_score_6,
#                       Clinical_congestion_score_12,
#                       Clinical_congestion_score_18))

# fluid_level <- with(data.new_congestion_scores,
#                     c(Combined_congestion_score_BL,
#                       Combined_congestion_score_1,
#                       Combined_congestion_score_3,
#                       Combined_congestion_score_6,
#                       Combined_congestion_score_12,
#                       Combined_congestion_score_18))

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
data.h$Dose_adjustment <- with(data.dose_change_after_visit, c(BL_change, V1_change, V3_change, V6_change, V12_change, V18_change))

data.h$Death <- with(data.death_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))

data.h$Hospitalisation_CHF <- with(data.hospit_CHF_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))
data.h$Hospitalisation_Renal_Failure <- with(data.hospit_renal_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))
data.h$Hospitalisation_Linked <- with(data.hospit_linked_after_visit, recode(c(BL, V1, V3, V6, V12, V18), '0' = 'no', '1' = 'yes'))

# Remove censored datapoints
data.h <- data.h[!is.na(with(data, c(rep(0, nrow(data)), V1, V3, V6, V12, V18))),]

# Mutate missing values to Hugin format
#data.h <- mutate_all(data.h, ~if_else(is.na(.), '<EMPTY>', .)) 

write.csv(data.h, "Hugin-TIME-CHF-visits-OwnCS.dat", row.names = FALSE)

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


##################################################################
### Which congestion score is more predictive?

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

################################################################################
## Why is no-change linked to lower hospitalisation?

table_Int_Dose_adj <- table(data.h$Intervention, data.h$Dose_adjustment, data.h$Fluid_level) # /rowSums(table(data.h$Intervention, data.h$Dose_adjustment))

write.csv(table_Int_Dose_adj, "table_Int_Dose_adj.csv", row.names = FALSE)

table(data.h$Fluid_level, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Fluid_level, data.h$Hospitalisation_CHF))

table_FL_Int_Hospit <-table( data.h$Intervention, data.h$Hospitalisation_CHF, data.h$Fluid_level)

write.csv(table_FL_Int_Hospit, "table_FL_Int_Hospit.csv", row.names = FALSE)

write.csv(table(data.h$Fluid_level, data.h$Hospitalisation_CHF)/rowSums(table(data.h$Fluid_level, data.h$Hospitalisation_CHF)),"table_FL_Hospit.csv", row.names = FALSE)
################################################################################
# Is it maybe low sample size?
#
# Should we learn a logistic regression model first?
#

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

################################################################################
# Which symptoms are more predictive of hospitalisation?
################################################################################

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

################################################################################
# Are there any biomarkers where the uptitration acts as expected in terms of outcomes
################################################################################


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


################################################################################
# Relationship between dose changes and symptoms / congestion scores (baseline vs month 1)
################################################################################

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

id.Dx.dose.visit_days <- merge(id.Dx.dose, data[, c("id", "V1", "V3", "V6", "V12", "V18")], by = 'id')
v1_change <- data$iddata$ data$V1

tibble_dose_V1 <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_V1_dose = dose[Dx == (V1-1)], post_V1_dose = dose[Dx == V1])),
  data[,c("id", "Diuretics_recom_1")], by = "id")
tibble_dose_V1$diff = tibble_dose_V1$post_V1_dose - tibble_dose_V1$pre_V1_dose

# Only 20% of the cases where the Diuretics_recom_1 == 1, there is an up titration the day of the visit
with(tibble_dose_V1, sum(diff > 0 & Diuretics_recom_1 == 1, na.rm = TRUE) / sum(Diuretics_recom_1 == 1, na.rm = TRUE))
# Only in 30% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V1, sum(diff < 0 & Diuretics_recom_1 == -1, na.rm = TRUE) / sum(Diuretics_recom_1 == -1, na.rm = TRUE))
# 99% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V1, sum(diff == 0 & Diuretics_recom_1 == 0, na.rm = TRUE) / sum(Diuretics_recom_1 == 0, na.rm = TRUE))

# What if we add a margin of x days?
tibble_dose_V1_3days <- merge(
  na.omit(
    id.Dx.dose.visit_days %>%
      group_by(id)%>%
      summarise(pre_V1_dose = dose[Dx == (V1-1)], post_V1_dose = dose[Dx == (V1+10)])),
  data[,c("id", "Diuretics_recom_1")], by = "id")
tibble_dose_V1_3days$diff = tibble_dose_V1_3days$post_V1_dose - tibble_dose_V1_3days$pre_V1_dose
# It goes up to 45% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
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
      summarise(pre_dose = dose[Dx == (V3-1)], post_dose = dose[Dx == (V3+10)])),
  data[,c("id", "Diuretics_recom_3")], by = "id")
tibble_dose_V3$diff = tibble_dose_V3$post_dose - tibble_dose_V3$pre_dose
# It goes up to 45% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff > 0 & Diuretics_recom_3 == 1, na.rm = TRUE) / sum(Diuretics_recom_3 == 1, na.rm = TRUE))
# It goes up to 50% of the cases where the Diuretics_recom_1 == -1, there is a down titration the day of the visit
with(tibble_dose_V3, sum(diff < 0 & Diuretics_recom_3 == -1, na.rm = TRUE) / sum(Diuretics_recom_3 == -1, na.rm = TRUE))
# Goes down to 93% of the cases where the Diuretics_recom_1 == 0, there is no dose change the day of the visit
with(tibble_dose_V3, sum(diff == 0 & Diuretics_recom_3 == 0, na.rm = TRUE) / sum(Diuretics_recom_3 == 0, na.rm = TRUE))
