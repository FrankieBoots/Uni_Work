#### Set up ####

##Packages
library(tidyverse)
library(readxl)

#function for me
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

##Loading in data
#Input data under columns 'distance' and 'mw_kDa'
wb_ladder <- read_excel("project/Figuring out molecular weight.xlsx")

#### my inputs ####
# If input everything here, the rest should auto-run without any changes needed

protein_name <- "PseB"
solvent_front <- 8.08
protein_distance <- 3.92
plot_save_location <- "project/plots/"

#### Finding linear regression coefficents ########

#solvent front:
wb_ladder

#finding the rf
wb_ladder$rf <- (solvent_front-wb_ladder$distance)/solvent_front

#getting log(molecular weight)
wb_ladder$log_mw <- log10(wb_ladder$mw_kDa)

## linear regression

wb_ladder_lm <- lm(log_mw~rf, data=wb_ladder)

#finding the equation

sum_wb_ladder_lm <- summary(wb_ladder_lm)
sum_wb_ladder_coeff <- sum_wb_ladder_lm[["coefficients"]]

# y = log(mw)
# x = rf
# y = mx + c

mx <- sum_wb_ladder_lm$coefficients["rf","Estimate"]
c <- sum_wb_ladder_lm$coefficients["(Intercept)","Estimate"]
r2 <- sum_wb_ladder_lm[["adj.r.squared"]]
#R^2 = 0.9691

line_equation <- paste("y = ",
                       specify_decimal(mx,2),
                       "x + ",specify_decimal(c,2),
                       ", R^2 = ",
                       specify_decimal(r2,2), sep="")
line_equation

#### Finding my MW for PseB ####

#find rf

rf <- (solvent_front-protein_distance)/solvent_front
rf

#find log(mw)
log_Mw <- (mx*rf) + c
log_Mw

#mw
PseB_Mw <- 10^(log_Mw)
PseB_Mw


#### plotting the calibration curve ####
wb_ladder %>%
  ggplot(aes(x = rf, y=log_mw)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  theme_classic() +
  annotate("text",x = 0.40, y=1, label = paste(line_equation), size = 4.5, hjust = 0) +
  annotate("text",x = 0.40, y=2.2, label = paste("PseB = 35 kDa"), size = 4.5, hjust = 0) +
  scale_y_continuous("log(Molecular Weight)") +
  scale_x_continuous("RF Value") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))


ggsave(paste(plot_save_location,protein_name,
             "_SDSPage_Mw.png",sep = ""),
       width= 1800,
       height= 2000,
       units="px")
