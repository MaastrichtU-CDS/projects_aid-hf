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

####################################################################################################################

# Data

data <- read_sav(".\\TIME_CHF_all_correct_follow_up.sav")
colnames(data)[1] <- "id"
data <- subset(data, Withdrawn != 1)
data <- data[order(c(data$id)),]

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
                                       "Withdrawn")], 
                               data.hospitalisations, 
                               by = "id")
data.hospitalisations <- subset(data.hospitalisations, Withdrawn != 1)
data.hospitalisations <- data.hospitalisations[order(c(data.hospitalisations$id)),]
table(unique(data.hospitalisations$id) %in% data$id)


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
data.indications <- read_excel(".\\1. Data\\Combi loop diuretic dose change indication_no absolute dose.xlsx", sheet = 1)
colnames(data.indications)[1] <- "id"
data.indications <- merge(data[,c("id", "Withdrawn")], data.indications, 
                         by = "id")
data.indications <- data.indications[order(c(data.indications$id)),]

data.indications.def <- read_excel(".\\1. Data\\Indications.xlsx", sheet = 1)


# Combined congestion score
data.new_congestion_scores <- read_excel(".\\1. Data\\Congestion score new scores.xlsx", sheet = 1)
colnames(data.new_congestion_scores)[1] <- "id"
data.new_congestion_scores <- merge(data[,c("id", "Withdrawn")], data.new_congestion_scores, 
                          by = "id")
data.new_congestion_scores <- data.new_congestion_scores[order(c(data.new_congestion_scores$id)),]
####################################################################################################################

# Loop diuretics

## Dose per day

data.dose <- data.loop_diuretics[, c(1, 5:545)] 
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
                     breaks = seq(0, 540, 30),
                     labels = seq(0, 540, 30)) + 
  scale_y_discrete(name = "ID", 
                   labels = NULL) + 
  scale_fill_continuous(name = "Dose")

## Dose change: present day - subsequent day 

dose_change <- as.data.frame(matrix(nrow = 540, ncol = 541))
colnames(dose_change)[1] <- "id"
dose_change[,1] <- data.dose[,1]
for (i in 2:541) {
  colnames(dose_change)[i] <- paste("D", i - 2, "_minus_", "D", i - 1, sep = "")
  }

for (i in 1:540) {
  for (j in 2:541) { 
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
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541], 
                                                   2,
                                                   FUN = fun.qualitative.na.9999)
fun.qualitative.up.minus_8888 <- function(x) { ifelse(x < 0,
                                                      -8888,
                                                      x) 
  }
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541], 
                                                   2, 
                                                   FUN = fun.qualitative.up.minus_8888)
fun.qualitative.down.8888 <- function(x) { ifelse( (x > 0) & (x < 9999), 
                                                   8888, 
                                                   x)
  }
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541], 
                                                   2,
                                                   FUN = fun.qualitative.down.8888)
fun.qualitative.na <- function(x) { ifelse(x == 9999, 
                                           "Missing", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541], 
                                                   2, 
                                                   FUN = fun.qualitative.na)
fun.qualitative.up <- function(x) { ifelse(x == -8888, 
                                           "Up-titration", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541],
                                                    2,
                                                    FUN = fun.qualitative.up)
fun.qualitative.nc <- function(x) { ifelse(x == 0, 
                                           "No change", 
                                           x) 
  }
dose_change.qualitative_indicator[, 2:541] <- apply(dose_change.qualitative_indicator[, 2:541], 
                                                    2, 
                                                    FUN = fun.qualitative.nc)

fun.qualitative.down <- function(x) { ifelse(x == 8888, 
                                             "Down-titration", 
                                             x)
    }
dose_change.qualitative_indicator[,2:541] <- apply(dose_change.qualitative_indicator[,2:541], 
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
id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx$Dx <- rep(0:539, 540)

ggplot(id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, aes(y = id, x = Dx, fill = dose_change.qualitative_indicator)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, 539), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                "Day 539 - 540")) + 
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

data.hospitalisations.merged <- subset(data, select = c("id", 
                                                        "RefDate"))
data.hospitalisations.merged <- merge(data.hospitalisations.merged, data.hospitalisations, 
                                      by = "id", 
                                      all.x = TRUE, 
                                      all.y = TRUE)
data.hospitalisations.merged <- subset(data.hospitalisations.merged, select = c("id", 
                                                                                "cause.CHF", 
                                                                                "cause.planned", 
                                                                                "cause.other",
                                                                                "RefDate", 
                                                                                "Date of Admission", 
                                                                                "Date of Discharge"))
data.hospitalisations.merged$Dx.admission <- difftime(data.hospitalisations.merged$"Date of Admission", 
                                                      data.hospitalisations.merged$RefDate)
data.hospitalisations.merged$Dx.discharge <- difftime(data.hospitalisations.merged$"Date of Discharge", 
                                                      data.hospitalisations.merged$RefDate)
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
                                                  "cause.planned", 
                                                  "cause.other", 
                                                  "deceased_indicator.overall", 
                                                  "Dx.admission", 
                                                  "Dx.discharge", 
                                                  "Dx.deceased"))

data.hospitalisations.merged.540 <- data.hospitalisations.merged
data.hospitalisations.merged.540$Dx.admission <- ifelse(data.hospitalisations.merged$Dx.admission > 540, 
                                                        NA, 
                                                        data.hospitalisations.merged$Dx.admission)
data.hospitalisations.merged.540$cause.CHF <- ifelse(data.hospitalisations.merged$Dx.admission > 540, 
                                                     NA, 
                                                     data.hospitalisations.merged$cause.CHF)
data.hospitalisations.merged.540$cause.planned <- ifelse(data.hospitalisations.merged$Dx.admission > 540, 
                                                         NA, 
                                                         data.hospitalisations.merged$cause.planned)
data.hospitalisations.merged.540$cause.other <- ifelse(data.hospitalisations.merged$Dx.admission > 540, 
                                                       NA, 
                                                       data.hospitalisations.merged$cause.other)
data.hospitalisations.merged.540$Dx.discharge <- ifelse(data.hospitalisations.merged$Dx.admission > 540, 
                                                        NA, 
                                                        data.hospitalisations.merged$Dx.discharge)
data.hospitalisations.merged.540$deceased_indicator.540 <- ifelse(data.hospitalisations.merged$Dx.deceased > 540, 
                                                                  NA, 
                                                                  data.hospitalisations.merged$deceased_indicator.overall)
data.hospitalisations.merged.540$Dx.deceased.540 <- ifelse(data.hospitalisations.merged$Dx.deceased > 540, 
                                                           NA, 
                                                           data.hospitalisations.merged$Dx.deceased)

length(unique(data.hospitalisations.merged.540$id))

table(data.hospitalisations.merged.540$cause.CHF, exclude = NULL)
data.hospitalisations.merged.540$cause.CHF <- ifelse(is.na(data.hospitalisations.merged.540$cause.CHF), 
                                                     0, 
                                                     data.hospitalisations.merged.540$cause.CHF)
cause.CHF.pp.540 <- aggregate(data.hospitalisations.merged.540$cause.CHF, 
                              by = list(data.hospitalisations.merged.540$id), 
                              FUN = sum)
table(cause.CHF.pp.540[, 2], exclude = NULL)

table(data.hospitalisations.merged.540$cause.planned, exclude = NULL)
data.hospitalisations.merged.540$cause.planned <- ifelse(is.na(data.hospitalisations.merged.540$cause.planned), 
                                                         0, 
                                                         data.hospitalisations.merged.540$cause.planned)
cause.planned.pp.540 <- aggregate(data.hospitalisations.merged.540$cause.planned, 
                                  by = list(data.hospitalisations.merged.540$id), 
                                  FUN = sum)
table(cause.planned.pp.540[, 2], exclude = NULL)

table(data.hospitalisations.merged.540$cause.other, exclude = NULL)
data.hospitalisations.merged.540$cause.other <- ifelse(is.na(data.hospitalisations.merged.540$cause.other), 
                                                       0, 
                                                       data.hospitalisations.merged.540$cause.other)
cause.other.pp.540 <- aggregate(data.hospitalisations.merged.540$cause.other, 
                                by = list(data.hospitalisations.merged.540$id), 
                                FUN = sum)
table(cause.other.pp.540[, 2], exclude = NULL)

table(is.na(data.hospitalisations.merged.540$Dx.admission))
table(is.na(data.hospitalisations.merged.540$Dx.discharge))
table(is.na(data.hospitalisations.merged.540$Dx.admission), is.na(data.hospitalisations.merged.540$Dx.discharge))
#View(subset(data.hospitalisations.merged.540, !is.na(Dx.admission) & is.na(Dx.discharge)))
##3 patients with admission date before 540 days after baseline, and no discharge date. Most likely: 1 stayed in hospital, reason for stay changed; 1 discharge after trial ended; 1 passed away in hospital. 
data.hospitalisations.merged.540$Dx.discharge <- ifelse( (data.hospitalisations.merged.540$id == "{782C9C2B-EE5A-4B9A-BA4C-D549D0C0A690}") 
                                                          & 
                                                          (data.hospitalisations.merged.540$Dx.admission == 536), 
                                                          587, 
                                                          data.hospitalisations.merged.540$Dx.discharge) 
data.hospitalisations.merged.540$Dx.discharge <- ifelse( (data.hospitalisations.merged.540$id == "{C56F4698-75F0-40D2-8E4F-3C492F24CA4A}") 
                                                          & 
                                                          (data.hospitalisations.merged.540$Dx.admission == 502), 
                                                          550, 
                                                          data.hospitalisations.merged.540$Dx.discharge) 
data.hospitalisations.merged.540$Dx.discharge <- ifelse( (data.hospitalisations.merged.540$id == "{D4DEA51C-3010-48B7-B2E1-454A57BACA71}") 
                                                          & 
                                                          (data.hospitalisations.merged.540$Dx.admission == 523), 
                                                          534, 
                                                          data.hospitalisations.merged.540$Dx.discharge) 
table(is.na(data.hospitalisations.merged.540$Dx.deceased))

data.hospitalisations.merged.540$cause <- ifelse(data.hospitalisations.merged.540$cause.CHF == 1, 
                                                 "CHF", 
                                                 NA)
data.hospitalisations.merged.540$cause <- ifelse(data.hospitalisations.merged.540$cause.planned == 1, 
                                                 "planned", 
                                                 data.hospitalisations.merged.540$cause)
data.hospitalisations.merged.540$cause <- ifelse(data.hospitalisations.merged.540$cause.other == 1, 
                                                 "other", 
                                                 data.hospitalisations.merged.540$cause)
data.hospitalisations.merged.540$cause <- factor(data.hospitalisations.merged.540$cause)

data.hospitalisations.merged.540$Dx.discharge.plot <- ifelse( data.hospitalisations.merged.540$Dx.discharge > 540, 
                                                              540, 
                                                              data.hospitalisations.merged.540$Dx.discharge)

ggplot() + 
  geom_segment(data = data.hospitalisations.merged.540, aes(x = Dx.admission, xend = Dx.discharge.plot, y = id, yend = id, colour = cause)) + 
  geom_segment(data = data.hospitalisations.merged.540, aes(x = Dx.deceased.540, xend = 540, y = id, yend = id, colour = "deceased")) + 
  scale_x_continuous(name = "Days from baseline", 
                     breaks = seq(0, 540, 30), 
                     labels = seq(0, 540, 30)) + 
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

id.Dx.hospitalisation_indicator <- merge(id.Dx, data.hospitalisations.merged.540[, c("id", 
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

data.merged.540 <- merge(id.Dx.dose.hospitalisation_indicator, 
                         id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, 
                         by = c("id", "Dx"), 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.540 <- merge(data.merged.540, 
                         unique(data.hospitalisations.merged.540[, c("id", 
                                                                     "Dx.deceased")]), 
                         by = "id", 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.540$hospitalisation_indicator <- as.numeric(as.character(data.merged.540$hospitalisation_indicator))
data.merged.540$hospitalisation_indicator <- ifelse(is.na(data.merged.540$hospitalisation_indicator), 
                                                    0, 
                                                    data.merged.540$hospitalisation_indicator)
data.merged.540$deceased_indicator <- ifelse( (data.merged.540$Dx >= data.merged.540$Dx.deceased), 
                                              1, 
                                              0)
data.merged.540$deceased_indicator <- ifelse( is.na(data.merged.540$deceased_indicator), 
                                              0, 
                                              data.merged.540$deceased_indicator)

write.csv(data.merged.540, 'data.merged.540.csv')

data.merged.540.plot <- data.merged.540 
data.merged.540.plot$plot <- as.character(data.merged.540.plot$dose_change.qualitative_indicator)
data.merged.540.plot$plot <- ifelse(data.merged.540.plot$hospitalisation_indicator == 1, 
                                    "Hospitalisation", 
                                    data.merged.540.plot$plot)
data.merged.540.plot$plot <- ifelse(data.merged.540.plot$deceased_indicator == 1,  
                                    "Deceased", 
                                    data.merged.540.plot$plot)
data.merged.540.plot$plot <- factor(data.merged.540.plot$plot)
data.merged.540.plot$plot <- factor(data.merged.540.plot$plot, levels = c("Up-titration", 
                                                                          "No change", 
                                                                          "Down-titration", 
                                                                          "Hospitalisation", 
                                                                          "Deceased", 
                                                                          "Missing"))

data.merged.540.plot <- data.merged.540.plot[!is.na(data.merged.540.plot$plot),]

data.merged.540.plot <- subset(data.merged.540.plot, Dx != 540)

ggplot(data.merged.540.plot, aes(x = Dx, y = id, fill = plot)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, 539), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                "Day 539 - 540")) + 
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
                                             subset(data.hospitalisations.merged.540, cause.CHF == 1)[, c("id", 
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

data.merged.540 <- merge(id.Dx.dose.CHF_hospitalisation_indicator, 
                         id.Dx_minus_Dy.dose_change.qualitative_indicator.Dx, 
                         by = c("id", 
                                "Dx"), 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.540 <- merge(data.merged.540, 
                         unique(data.hospitalisations.merged.540[, c("id", 
                                                                     "Dx.deceased")]), 
                         by = "id", 
                         all.x = TRUE, 
                         all.y = TRUE)
data.merged.540$CHF_hospitalisation_indicator <- as.numeric(as.character(data.merged.540$CHF_hospitalisation_indicator))
data.merged.540$CHF_hospitalisation_indicator <- ifelse(is.na(data.merged.540$CHF_hospitalisation_indicator), 
                                                        0, 
                                                        data.merged.540$CHF_hospitalisation_indicator)
data.merged.540$deceased_indicator <- ifelse( (data.merged.540$Dx >= data.merged.540$Dx.deceased), 
                                              1, 
                                              0)
data.merged.540$deceased_indicator <- ifelse( is.na(data.merged.540$deceased_indicator), 
                                              0, 
                                              data.merged.540$deceased_indicator)

data.merged.540.plot <- data.merged.540 
data.merged.540.plot$plot <- as.character(data.merged.540.plot$dose_change.qualitative_indicator)
data.merged.540.plot$plot <- ifelse(data.merged.540.plot$CHF_hospitalisation_indicator == 1, 
                                    "Hospitalisation: CHF", 
                                    data.merged.540.plot$plot)
data.merged.540.plot$plot <- ifelse(data.merged.540.plot$deceased_indicator == 1,  
                                    "Deceased", 
                                    data.merged.540.plot$plot)
data.merged.540.plot$plot <- factor(data.merged.540.plot$plot)
data.merged.540.plot$plot <- factor(data.merged.540.plot$plot, levels = c("Up-titration", 
                                                                          "No change", 
                                                                          "Down-titration", 
                                                                          "Hospitalisation: CHF", 
                                                                          "Deceased",
                                                                          "Missing"))

data.merged.540.plot <- data.merged.540.plot[!is.na(data.merged.540.plot$plot), ]

data.merged.540.plot <- subset(data.merged.540.plot, Dx != 540)

ggplot(data.merged.540.plot, aes(x = Dx, y = id, fill = plot)) + 
  geom_tile() + 
  scale_x_continuous(name = "Difference over 2 consecutive days", 
                     breaks = c(0, 30, 90, 180, 360, 539), 
                     labels = c("Day 0 - day 1", 
                                "Day 30 - day 31", 
                                "Day 90 - day 91", 
                                "Day 180 - day 181", 
                                "Day 360 - day 361", 
                                "Day 539 - 540")) + 
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

data.timeslots <- data.merged.540

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

data.indications.pivotted <- pivot_longer(data.indications[,c(1,13:60)], !id, names_to = "Indication", values_to = "Value")
data.indications.pivotted <- na.omit(data.indications.pivotted)
data.indications.pivotted$Number <- str_replace(data.indications.pivotted$Indication, "D|Ind", "")
data.indications.pivotted$Column <- str_replace(data.indications.pivotted$Indication, "\\d+", "")
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
          current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)], paste0("Not found: ", (current_day+1))))
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
            current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)], paste0("Not found: ", (current_day+1))))
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
            current_indication <- with(data.indications.pivotted, ifelse(sum(id == current_id & D == (current_day+1))> 0, Reason[id == current_id & D == (current_day+1)], paste0("Not found: ", (current_day+1))))
            cat(paste0(">> Current indication: ", current_indication, "(",current_day ,")","\n"))
            counts_periods<- rbind(counts_periods, data.frame(id = current_id, baseline_state = current_state, end_state = next_state, length= min(length_timespan, time_horizon),  indication = ifelse(next_state == "Down-titration" || next_state == "Up-titration", current_indication, NA)));
            cat(paste0(">> Adding row (3): id: ", current_id,"; baseline state:", current_state, "; End state:", next_state, "; Length: ", min(length_timespan, time_horizon), "\n"))
          }
          
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
# Conditional probability table

condititonal_probability_table <- with(counts_periods, table(baseline_state, end_state) / rowSums(table(baseline_state, end_state)))

counts_periods_up_titration <- counts_periods[counts_periods$baseline_state == "Up-titration",]

write.csv(table(counts_periods_up_titration$end_state, counts_periods_up_titration$indication), "table_post_uptitration.csv")

counts_periods_down_titration <- counts_periods[counts_periods$baseline_state == "Down-titration",]

write.csv(table(counts_periods_down_titration$end_state, counts_periods_down_titration$indication), "table_post_downtitration.csv")

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
data.h <- data.frame(Gender, Cause_HF)
                

# Time-point specific
# Baseline 
# data.h$Fluid_level <- discretise_quantiles(with(data, c(Own_congestionscore_BL, 
#                                                         Own_congestionscore_1, 
#                                                         Own_congestionscore_3, 
#                                                         Own_congestionscore_6, 
#                                                         Own_congestionscore_12, 
#                                                         Own_congestionscore_18)), labels = c("Very Low", "Low", "Average", "High", "Very High"))
# data.h$Fluid_level <- discretise_quantiles(with(data.new_congestion_scores, c(Clinical_congestion_score_BL, 
#                                                                               Clinical_congestion_score_1,
#                                                                               Clinical_congestion_score_3, 
#                                                                               Clinical_congestion_score_6,
#                                                                               Clinical_congestion_score_12, 
#                                                                               Clinical_congestion_score_18)), labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$Fluid_level <- discretise_quantiles(with(data.new_congestion_scores, c(Combined_congestion_score_BL, 
                                                                              Combined_congestion_score_1,
                                                                              Combined_congestion_score_3, 
                                                                              Combined_congestion_score_6,
                                                                              Combined_congestion_score_12, 
                                                                              Combined_congestion_score_18)), labels = c("Very Low", "Low", "Average", "High", "Very High"))
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
data.h$CRP <- discretise_quantiles(with(data, c(hsCRP_BL, hsCRP_V1, hsCRP_V3, hsCRP_V6, hsCRP_V12, hsCRP_V18)), 
                                   labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$IL6 <- discretise_quantiles(with(data, c(IL6_BL, IL6_V1, IL6_V3, IL6_V6, IL6_V12, IL6_V18)), 
                                   labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$BNP <- discretise_quantiles(with(data, c(BNP0_ln, BNP1_ln, BNP3_ln, BNP6_ln, BNP12_ln, BNP18_ln)),
                                   labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$Urea_creatinine_ratio <- discretise_quantiles(with(data, c(Urea, Urea_V1, Urea_V3, Urea6, Urea12, Urea18) / rbind(Creatinine, Crea1, Crea3, Crea6, Crea12, Crea18)), 
                                                     labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$Cystatin_C <- discretise_quantiles(with(data, c(CysC_BL, CysC_V1, CysC_V3, CysC_V6, CysC_V12, CysC_V18)),
                                          labels = c("Very Low", "Low", "Average", "High", "Very High"))
data.h$GFR <- discretise_quantiles(with(data, c(eGFR_MDRD_BL, eGFR_MDRD_1, eGFR_MDRD_3, eGFR_MDRD_6, eGFR_MDRD_12, eGFR_MDRD_18)), 
                                   labels = c("Very Low", "Low", "Average", "High", "Very High"))
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
data.h <- mutate_all(data.h, ~if_else(is.na(.), '<EMPTY>', .)) 

write.csv(data.h, "Hugin-TIME-CHF-data-combined-cs.dat", row.names = FALSE)
