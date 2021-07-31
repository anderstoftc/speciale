
library(pacman)

p_load(glue, Imap, lubridate, survey, rlang, labelled, zoo, textshape)

rm(list=ls())

### 1. Preliminaries -------------------

### Make popmatrix
sample <- readRDS("03_SampleManagement/data/sample/Sample_fortrolig_20210518.rds")

sample <- sample %>%
  dplyr::filter(!SendGroup %in% c("Test", "Test2"))

#Åbner kategorisering af køn
sample_koen <- read.xlsx("03_SampleManagement/data/Kategorisering-af-køn-færdig.xlsx")

sample_koen <- sample_koen %>% mutate_if(is.character, str_trim)

### 3.1 Email -----------------------
#### Sæt til lower case
sample_koen$Email <- str_to_lower(sample_koen$Email)

#Joiner køn på
sample_all <- left_join(sample_koen, sample, by ="Email")

#vælger kun relevante variable
sample_all <- sample_all %>%
  dplyr::select(Id, Koen_kat =Koen, Parti, Kommune, Parti_wq)

sample_all <- sample_all %>%
  mutate(Blok = ifelse(Parti=='Socialdemokratiet'|Parti=='Radikale Venstre'|Parti=='Socialistisk Folkeparti'|Parti=='Enhedslisten'|Parti=='Alternativet', "Rod Blok",
                       ifelse(Parti=='Konservative'|Parti=='Nye Borgerlige'|Parti=='Liberal Alliance'|Parti=='Dansk Folkeparti'|Parti=='Venstre'|Parti=='Kristendemokraterne', "Blaa Blok", "Ukodet"))) %>%
  mutate(Koen_kat = case_when(Koen_kat=="M" ~ "Mand", 
                              Koen_kat=="K" ~ "Kvinde"))
sample_all$Koen_kat[is.na(sample_all$Koen_kat)] <- "Hovedmail/Ukendt"

table(sample_all$Koen_kat)

Blok <- sample_all %>%
  dplyr::group_by(Blok) %>%
  tally() %>%
  dplyr::mutate(antal = n) %>%
  dplyr::ungroup() %>%
  distinct(Blok  = Blok, antal) %>%
  dplyr::mutate(share = antal/sum(antal)) %>%  # calculate proportions
  relocate(Blok ) %>%
  dplyr::select(Blok , share)

Blok  <- Blok[order(Blok[,'Blok']),]

Koen <- sample_all %>%
  dplyr::group_by(Koen_kat) %>%
  tally() %>%
  dplyr::mutate(antal = n) %>%
  dplyr::mutate(Koen = Koen_kat) %>%
  dplyr::ungroup() %>%
  distinct(Koen  = Koen, antal) %>%
  dplyr::mutate(share = antal/sum(antal)) %>%  # calculate proportions
  relocate(Koen ) %>%
  dplyr::select(Koen , share)

Koen  <- Koen[order(Koen[,'Koen']),]

Parti <- sample_all %>%
  dplyr::group_by(Parti_wq) %>%
  tally() %>%
  dplyr::mutate(antal = n) %>%
  dplyr::mutate(Parti = Parti_wq) %>%
  dplyr::ungroup() %>%
  distinct(Parti  = Parti, antal) %>%
  dplyr::mutate(share = antal/sum(antal)) %>%  # calculate proportions
  relocate(Parti ) %>%
  dplyr::select(Parti , share)

Parti  <- Parti[order(Parti[,'Parti']),]

Parti

Koen <- as.data.frame(Koen)
Blok <- as.data.frame(Blok)
Parti <- as.data.frame(Parti)

pop_matrix <- list()
pop_matrix[["Koen"]] <- Koen
pop_matrix[["Blok"]] <- Blok
pop_matrix[["Parti"]] <- Parti

saveRDS(pop_matrix, "05_data/populationsmatrice.rds")


