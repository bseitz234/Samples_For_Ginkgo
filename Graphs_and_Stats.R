# Graphs for poster for Spindle Disassembly Paper, 2021
library(tibble)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
#Everything for this paper lives here:
setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/LabThings/Papers/2021")

## Summary function:
##   Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#Replace Strains with descriptions
WT <- expression(italic('WT'))
sps1 <- expression(italic('sps1Δ'))
ama1 <- expression(italic('ama1Δ'))
double <- expression(italic('ama1Δ sps1Δ'))
strains <- c('WT', 'sps1Δ', 'ama1Δ', 'ama1Δ sps1Δ')

#Read Ase1 scores:
ase1_data <- read.delim(
  file = "Quantifications/Ase1.csv",
  header = TRUE,
  sep = ",", quote = "\"")
ase1_data <- ase1_data[1:5]
# ase1_data$Genotype <- factor(spindle_data$Genotype, 
#                             levels =c("WT", "sps1Δ", "ama1Δ", "sps1Δ ama1Δ"))

#Calculate Percentages: 
ase1_data <- ase1_data %>%
  dplyr::group_by(Strain, Spindles, Ase1) %>%
  dplyr::count() %>%
  dplyr::group_by(Strain, Spindles) %>%
  dplyr::mutate(Totaln=sum(n)) %>%
  dplyr::mutate(Percentage = n / Totaln);

ase1_data$Strain <- factor(x = as.factor(ase1_data$Strain),
                           labels = strains)

#
## CIN8:
#

#Read Cin8 scores:
cin8_data <- read.delim(
  file = "Quantifications/Cin8.csv",
  header = TRUE,
  sep = ",", quote = "\"")
cin8_data <- cin8_data[1:5]

#Calculate Percentages:
cin8_data <- cin8_data %>%
  dplyr::group_by(Strain, Spindles, Cin8) %>%
  dplyr::count() %>%
  dplyr::group_by(Strain, Spindles) %>%
  dplyr::mutate(Totaln=sum(n)) %>%
  dplyr::mutate(Percentage = n / Totaln);


#
## BIM1:
#

#Read Bim1 scores:
bim1_data <- read.delim(
  file = "Quantifications/Bim1.csv",
  header = TRUE,
  sep = ",", quote = "\"")
bim1_data <- bim1_data[1:5]

#Calculate Percentages:
bim1_data <- bim1_data %>%
  tidyr::unite("State", Spindles, PSMs) %>%
  dplyr::group_by(Strain, State, Bim1) %>%
  dplyr::count() %>%
  dplyr::group_by(Strain, State) %>%
  dplyr::mutate(Totaln=sum(n)) %>%
  dplyr::mutate(Percentage = n / Totaln);

#Replace Character Columns with Factors:
bim1_data$Strain <- factor(x = as.factor(bim1_data$Strain),
                           labels = strains[1:3])
bim1_data$State <- factor(bim1_data$State, 
                          levels(as.factor(bim1_data$State))[c(2,4,3,1)],
                          c("Anaphase Spindles, Open PSMs",
                            "Post-Anaphase Spindles, Open PSMs",
                            "Post-Anaphase Spindles, Closed PSMs", "Uncounted"))
bim1_data$State


#Graph!
ggplot(data = dplyr::arrange(ase1_data, Spindles), aes(
  x = Spindles, 
  y = Percentage,
  fill = Ase1)) +
  geom_col(position = position_stack(), width = .5) +
  facet_grid(cols = vars(Strain), switch="x") +
  scale_fill_manual(values = c("#cccccc", "#222222")) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  labs(title = "Percentage of Anaphase II or later cells
       with concentrated Ase1 foci",
       y = "Percentage of cells with \nAse1 foci") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 24, 
                                  color="#000000", face="bold")) +
  theme(legend.text = element_text(face = "italic", size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1, 
                                   face = "italic")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.y = element_text(vjust = 2, size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(strip.text.x = element_text(size = 14, face = "italic"))
#  theme(aspect.ratio = 2) +
#  theme(legend.position="none") + 
#  facet_wrap(. ~  Strain)

ggplot(data = dplyr::arrange(cin8_data, Spindles), aes(
  x = Spindles, 
  y = Percentage,
  fill = Cin8)) +
  geom_col(position = position_stack(), width = .5) +
  facet_grid(cols = vars(Strain), switch="x") +
  scale_fill_manual(values = c("#cccccc", "#222222")) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  labs(title = "Percentage of Anaphase II or later cells
       with concentrated Cin8 foci",
       y = "Percentage of cells with \nCin8 foci") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 24, 
                                  color="#000000", face="bold")) +
  theme(legend.text = element_text(face = "italic", size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1, 
                                   face = "italic")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.y = element_text(vjust = 2, size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(strip.text.x = element_text(size = 14, face = "italic"))

ggplot(data = dplyr::arrange(bim1_data, State), aes(
  x = State, 
  y = Percentage,
  fill = Bim1)) +
  geom_col(position = position_stack(), width = .5) +
  facet_grid(cols = vars(Strain), switch="x") +
  scale_fill_manual(values = c("#cccccc", "#222222", "#8a8a8a")) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  labs(title = "Percentage of Anaphase II or later cells
       with concentrated Bim1 foci",
       y = "Percentage of cells with \nBim1 foci") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 24, 
                                  color="#000000", face="bold")) +
  theme(legend.text = element_text(face = "italic", size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1, 
                                   face = "italic")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.y = element_text(vjust = 2, size = 20)) +
  theme(axis.title.x = element_blank()) +
  theme(strip.text.x = element_text(size = 14, face = "italic"))

