### 1. DEPENDENCIES --------------------------------------------------------------------------------
rm(list=ls())

library(tidyverse) # Til alt
library(haven) #Til at indlæse data
library(openxlsx) #Til at gemme excelark
library(stringi) #Til at lave tilfældige id'er
library(rvest) # Kan scrape web-data
library(httr) # Kan tilgå web-data

#Set seed - OBS: DO NOT CHANGE THE SEED VALUE
set.seed(12345)

#Turn off scientific notation
options(scipen=999) 

### 2. PREPERATIONS ---------
# Set wd
rootfolder <- c("/Users/anderstoft/Desktop/Special/")
setwd(rootfolder)

# Read data
raw <- read.xlsx("udsendelsesgrundlag.xlsx", sheet = "Kategorisering af køn") %>%
  transmute(Email = as.character(Email),
            Navn = as.character(Navn),
            Parti = as.character(Parti),
            Kommune = as.character(Kommune)
            )

sample_raw <- raw %>%
  mutate(Navn = str_to_title(Navn))

#### Sikre, at der altid er et navn
sum(is.na(sample_raw$Navn)) 

sample_raw <- sample_raw %>%
  drop_na()

sum(is.na(sample_raw$Navn)) # 2 missing, disse slettes

# Hvad er de korsteste navne? (Ser fint ud)
sample_raw %>%
  arrange(str_length(Navn)) %>%
  head(20)

sample_raw %>%
  arrange(str_length(Navn)) %>%
  tail(20)

sample_raw <- extract(sample_raw, Navn, c("Navn", "Efternavn"), "([^ ]+) (.*)")

navn <- read.xlsx("koen.xlsx")

test <- left_join(sample_raw, navn, by = "Navn")

table(test$Køn)

write.xlsx(test, "Kategorisering af køn - færdig.xlsx")
