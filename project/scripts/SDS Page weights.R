#### Set up ####

##Packages
library(tidyverse)
library(readxl)

##Loading in data
wb_ladder <- read_excel("project/Figuring out molecular weight.xlsx")

#### Finding linear regression coefficents ########

#solvent front:8.08
wb_ladder

#finding the rf
wb_ladder$rf <- (8.08-wb_ladder$distance)/8.08

#getting log(molecular weight)
wb_ladder$log_mw <- log10(wb_ladder$mw_kDa)

## linear regression

wb_ladder_lm <- lm(log_mw~rf, data=wb_ladder)

#finding the equation

summary(wb_ladder_lm)

# y = log(mw)
# x = rf
# y = 1.84828x + 2.44

mx <- 1.84828
c<- 0.59180

#R^2 = 0.9691

line_equation <- "y = 1.84828x + 0.59180, R^2 = 0.97"

#### plotting the calibration curve ####
wb_ladder %>%
  ggplot(aes(x = rf, y=log_mw)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  theme_classic() +
  annotate("text",x = 0.40, y=1, label = paste(line_equation), size = 4.5, hjust = 0) +
  scale_y_continuous("log(Molecular Weight)") +
  scale_x_continuous("RF Value") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))

ggsave("project/plots/wb_ladder_graph.png", width= 1800, height= 2000, units="px")

#### Finding my MW for PseB ####

#find rf

rf <- (8.08-3.74)/8.08
rf

#find log(mw)
log_Mw <- (1.84828*rf) + 0.59180
log_Mw

#mw
PseB_Mw <- 10^(log_Mw)
PseB_Mw
#38.42061