#ReadMe----
#Purpose: Putting together analysis data
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, showtext, scales)

# Remove everything
rm(list=ls())

# Conflict prefer
conflict_prefer(name = "filter", winner = "dplyr")

#Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

#Import data----


#Save data----