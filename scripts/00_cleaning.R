#Data cleaning script for PycnoHeat (expeirment run in Sitka summer of 2025)
#By Nikita Sridhar
#8/1/25


#loading libraries:
library(tidyverse)
library(here)
library(googlesheets4)
library(strex)
library(lme4)
library(performance)
library(sjPlot)

#pulling data from web ---------------------------------------------------------
kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hYKwyw7xpImw0UkSa-QuuZrkBy2YU88_AIKomJshBQA/edit?gid=1832909041#gid=1832909041",
                                  "Kelp weight") 

