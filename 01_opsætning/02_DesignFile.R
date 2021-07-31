rm(list=ls())

### Packages
pacman::p_load(haven, 
               tidyverse,
               xlsx,
               lubridate,
               forcats,
               survey,
               foreign,
               tidyverse,
               dplyr,
               stringr,
               openxlsx,
               ggplot2,
               haven, 
               tidyr, 
               msm, 
               sandwich, 
               lmtest,
               rebus,
               cregg,
               estimatr,
               grid,
               emmeans,
               ggpubr,
               sjstats)

set.seed(12345)

# Load kategorier
# Af skal være betinget sandsynlighed (navn). Derfor inddeles den i 4 lige store grupper
a <- 1:20
a[1:3] <- 1
a[4:6] <- 2
a[7:13] <- 3
a[14:20] <- 4

b <- 1:6
c <- 1:4
d <- 1:4
e <- 1:5
f <- 1:5

grid <- expand.grid(a, 
                    b,
                    c,
                    d,
                    e,
                    f)

# Jeg trækker så mange samples, der lader sig gøre i sawtooth (999). Hver sample består 5 tasks med to observationer i hver
combined <- bind_rows(replicate(999, 
                                bind_rows(replicate(8, grid %>% sample_n(2), simplify=F), .id="Task"), 
                                simplify = F), .id="Version")

combined$Concept <- as.character(1:2)

clean <- combined %>%
  select(Version, Task, Concept, starts_with("Var")) %>%
  mutate(Id = as.character(1:nrow(combined)))

# Færdiggør sample
labels.vec <- paste(paste("Att", sprintf("%02d",1:6)),
                    c("Navn", 
                      "Alder",
                      "Civilstatus",
                      "Beskæftigelse",
                      "Politisk erfaring",
                      "Partimedlemskab"), sep=" - ")

names(clean)[4:9] <-labels.vec

#Gem hele dynen
setwd("C:/Users/anch/OneDrive - Epinion/Speciale/")
write.xlsx(clean, file="Design.file.And.Labelling.NEW.Conditional.xlsx", 
           sheetName = "Design.File")

table(clean$`Att 01 - Navn`)

