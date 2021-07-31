rm(list = ls())

# Dependicies
pacman::p_load(dplyr, tidyverse, ggplot2, haven, stargazer, psych, sjstats, 
               lubridate, foreign, broom, stringr, purrr, jsonlite, data.table, beepr,
               rlang, scales, magrittr, survey, corrplot, openxlsx, cregg, forcats, 
               cjoint, grid, ggpubr, estimatr, emmeans, gridExtra, knitr, readr,cowplot, xtable, lfe)

library(extrafont)
library(mapDK)
library(RColorBrewer)
library(mapproj)
library("ggmap")

loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 
