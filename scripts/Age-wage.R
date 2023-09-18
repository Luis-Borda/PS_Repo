################################################################################
##                      Age-wage profile.                                     ##
################################################################################

#Limpieza del área de trabajo 

rm(list = ls())



#CARGAMOS LOS PAQUETES


require(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer,
       knitr,
       boot,
       data.table)

library(dplyr)
library(tidyr)

#Directorio

setwd("/Users/lordb/OneDrive - Universidad de los andes/2. GRUPOS/2. BIG DATA/PS_Repo_Taller1_G10")

library(readr)
b_final <- read_csv("stores/b_final.csv")
View(b_final)



