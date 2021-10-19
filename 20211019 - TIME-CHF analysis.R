####################################################################################################################

# Packages

library("haven")
library("readxl")
library("ggplot2")
library("tidyverse")
library("ggnewscale")

####################################################################################################################

# Data

data <- read_sav(".\\TIME_CHF_all_correct_follow_up.sav")
colnames(data)[1] <- "id"
data <- subset(data, Withdrawn != 1)

data.loop_diuretics <- read_excel(".\\Medication daily doses.xlsx", sheet = 6)
colnames(data.loop_diuretics)[1] <- "id"
table(data$id %in% data.loop_diuretics$id)
data.loop_diuretics <- merge(data[,c("id", 
                                     "Withdrawn")], 
                             data.loop_diuretics, 
                             by = "id")
data.loop_diuretics <- subset(data.loop_diuretics, Withdrawn != 1)

data.hospitalisations <- read_excel(".\\1. Data\\Hospitalisations overview.xlsx")
colnames(data.hospitalisations)[1] <- "id"
table(unique(data.hospitalisations$id) %in% data$id)
data.hospitalisations <- merge(data[,c("id", 
                                       "Withdrawn")], 
                               data.hospitalisations, 
                               by = "id")
data.hospitalisations <- subset(data.hospitalisations, Withdrawn != 1)
table(unique(data.hospitalisations$id) %in% data$id)

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
                                                        "Dateofstudyentry"))
data.hospitalisations.merged <- merge(data.hospitalisations.merged, data.hospitalisations, 
                                      by = "id", 
                                      all.x = TRUE, 
                                      all.y = TRUE)
data.hospitalisations.merged <- subset(data.hospitalisations.merged, select = c("id", 
                                                                                "cause.CHF", 
                                                                                "cause.planned", 
                                                                                "cause.other",
                                                                                "Dateofstudyentry", 
                                                                                "Date of Admission", 
                                                                                "Date of Discharge"))
data.hospitalisations.merged$Dx.admission <- difftime(data.hospitalisations.merged$"Date of Admission", 
                                                      data.hospitalisations.merged$Dateofstudyentry)
data.hospitalisations.merged$Dx.discharge <- difftime(data.hospitalisations.merged$"Date of Discharge", 
                                                      data.hospitalisations.merged$Dateofstudyentry)
data.hospitalisations.merged <- merge(data[, c("id", 
                                               "Death", 
                                               "Dateofdeath")], 
                                      data.hospitalisations.merged, 
                                      by = "id", 
                                      all.x = TRUE, 
                                      all.y = TRUE)
colnames(data.hospitalisations.merged)[2] <- "deceased_indicator.overall"
data.hospitalisations.merged$Dx.deceased <- difftime(data.hospitalisations.merged$Dateofdeath, 
                                                     data.hospitalisations.merged$Dateofstudyentry)
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




