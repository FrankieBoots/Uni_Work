### Set up ####

# working directory
setwd("~/uni/Uni_Work/project")
getwd()

#libraries

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#install.packages("lubridate")
library("lubridate")

#Bring in raw data
all <- read_excel("growth curves raw/Plate Reading GM LH CM JF.xlsx",
                  sheet = "Absorbance 1_01", skip = 9)

#########
#### PROCESSING DATA - SPECIFIC TO INTENDED GROWTH CURVES ####
#rename time column for all

##LOUISE
louise <- all %>% 
  select("avg. time [s]","Un0001 (B01)","Un0002 (B02)","Un0003 (B03)","Un0004 (B04)","Un0005 (B05)") %>%
  setNames(c("time_s", "one", "two","three","four","five"))
#Get mean
louise$mean <- rowMeans(louise[ , c(2,6)], na.rm=TRUE)
#remove anything not in plot
louise <- louise %>% select("time_s","mean")
louise

## CONNIE
connie <- all %>% select("avg. time [s]","Un0006 (D01)","Un0007 (D02)","Un0008 (D03)","Un0009 (D04)","Un0010 (D05)") %>%
  setNames(c("time_s", "one", "two","three","four","five"))
#Get mean
connie$mean <- rowMeans(connie[ , c(2,6)], na.rm=TRUE)
#remove anything not in plot
connie <- connie %>% select("time_s","mean")
connie

## GRACE
grace <- all %>% select("avg. time [s]","Un0011 (F01)","Un0012 (F02)","Un0013 (F03)","Un0014 (F04)","Un0015 (F05)") %>%
  setNames(c("time_s", "one", "two","three","four","five"))
#Get mean
grace$mean <- rowMeans(grace[ , c(2,6)], na.rm=TRUE)
#remove anything not in plot
grace <- grace %>% select("time_s","mean")
grace

## JAMES
james <- all %>% select("avg. time [s]","Un0016 (H01)","Un0017 (H02)","Un0018 (H03)","Un0019 (H04)","Un0020 (H05)") %>%
  setNames(c("time_s", "one", "two","three","four","five"))
#Get mean
james$mean <- rowMeans(james[ , c(2,6)], na.rm=TRUE)
#remove anything not in plot
james <- james %>% select("time_s","mean")
james
##

## HARRIET

#### Making the line plot - REPEATABLE ####

## Individual ggplots ##
#I have: james, grace, connie, louise

#Making variable for inputting each person's data
growth_curve <- louise

# Make seconds into hours
#plot
single <- ggplot(growth_curve) +
  geom_line(aes(x = time_s,
                y = mean)) +
  labs(y = "Mean Plate Reading",
       x = "Time (S)") +
  theme_classic()
single

# Fancier plot

## processing

#making minute and hour columns
growth_curve$time_m <- growth_curve$time_s/60
growth_curve$time_h <- growth_curve$time_m/60

#plot
fancy <- ggplot(growth_curve) +
  geom_line(aes(x = time_h,
                y = mean)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 18, by = 1)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 0.8, by = 0.1)) +
  labs(y = "Mean Plate Reading",
       x = "Time (Hours)") +
  theme_classic()
fancy

