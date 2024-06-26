#### Set up ####

##Packages
#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#function for me
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#### Your inputs ####

##Loading in your data
#Input data under columns 'distance' and 'mw_kDa'

## From excel
#wb_ladder <- read_excel("project/Figuring out molecular weight.xlsx")

##manual input
wb_ladder <- data_frame(
  mw_kDa = c(10,15,25,35,40,55,70,100,130,180),  
  distance = c(6.55,5.63,4.65,4.03,3.36,2.65,2.03,1.43,0.82,0.32)
  )
##check
view(wb_ladder)

#Specifics 
protein_name <- "PseC"
solvent_front <- 7.43
protein_distance <- 3.62
plot_save_location <- "project/plots/"

#### Finding linear regression coefficients ########

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

line_equation <- paste("y = ",
                       specify_decimal(mx,2),
                       "x + ",specify_decimal(c,2),
                       ", R^2 = ",
                       specify_decimal(r2,2), sep="")
line_equation

#### Finding my MW for PseB ####

##find rf

# Alt if want quick Mw
#protein_distance <- (3.65+3.74+3.74+4.03+4.08)/5

rf <- (solvent_front-protein_distance)/solvent_front
rf

#find log(mw)
log_Mw <- (mx*rf) + c
log_Mw

#mw
PseB_Mw <- 10^(log_Mw)
PseB_Mw

Mw <- specify_decimal(PseB_Mw,0)
Mw <- paste(protein_name," = ",Mw," kDa", sep="")

#### plotting the calibration curve ####
wb_ladder %>%
  ggplot(aes(x = rf, y=log_mw)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  theme_classic() +
  annotate("text",x = 0.40, y=1, label = paste(line_equation), size = 4.5, hjust = 0) +
  annotate("text",x = 0.40, y=2.2, label = Mw, size = 4.5, hjust = 0) +
  scale_y_continuous("log(Molecular Weight)") +
  scale_x_continuous("RF Value") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))

## Save curve to inputted location
ggsave(paste(plot_save_location,protein_name,
             "_SDSPage_Mw.png",sep = ""),
       width= 1800,
       height= 2000,
       units="px")

#### Citations ####

# R Core Team (2023). _R: A
# Language and Environment for
# Statistical Computing_. R
# Foundation for Statistical
# Computing, Vienna, Austria.
# <https://www.R-project.org/>.

##TidyVerse
# Wickham H, Averick M, Bryan J,
# Chang W, McGowan LD, François
# R, Grolemund G, Hayes A, Henry
# L, Hester J, Kuhn M, Pedersen
# TL, Miller E, Bache SM, Müller
# K, Ooms J, Robinson D, Seidel
# DP, Spinu V, Takahashi K,
# Vaughan D, Wilke C, Woo K,
# Yutani H (2019). “Welcome to
# the tidyverse.” _Journal of
# Open Source Software_, *4*(43),
# 1686. doi:10.21105/joss.01686
# <https://doi.org/10.21105/joss.01686>.


##readxl
# Wickham H, Bryan J (2023). _readxl: Read Excel Files_. R package
# version 1.4.3, <https://CRAN.R-project.org/package=readxl>