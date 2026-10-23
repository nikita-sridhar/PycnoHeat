#loading libraries:
load_libraries <- function(){
  library(tidyverse)
  library(here)
  library(googlesheets4)
  library(strex)
  library(lme4)
  library(performance)
  library(sjPlot)
  library(readr)
  library(lubridate)
  library(hms)
  library(ggpmisc)
  library(emmeans)
  library(flextable)
  library(marginaleffects)
  library(RColorBrewer)
  library(car)
}

load_libraries()

source(here::here("scripts/00_cleaning.R"))
source(here::here("scripts/01_manipulation.R"))
