# ICS-Plots.r
#
# Make stacked bar graph of state Intentionally Created Surplus holdings by year
#
# Data is USBR Water Accounting Reports: https://www.usbr.gov/lc/region/g4000/wtracct.html in source Excel file
# Please report bugs/feedback to:
#
# Updated June 23, 2020 to include annual deposits and withdraws as year-to-year differnces
#
# Updated April 4, 2021 to look at ICS to DCP conversion

#
# David E. Rosenberg
# April 4, 2021
# 
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="https://cran.cnr.berkeley.edu/", verbose = TRUE) 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

  
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}

if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

if (!require(ggplot)) { 
  install.packages("ggPlot", repo="http://cran.r-project.org", dependencies = T)
  library(ggplot) 
}

if (!require(stringr)) { 
  install.packages("stringr", repo="http://cran.r-project.org")
  library(stringr) 
}



# Load Data

# Read in state balances each year
sExcelFile <- 'IntentionallyCreatedSurplus-Summary.xlsx'
dfICSBalance <- read_excel(sExcelFile, sheet = "Sheet1",  range = "B6:F16")
dfICStoDCP <- read_excel(sExcelFile, sheet = "ICStoDCP",  range = "A2:M14")
dfLimits <- read_excel(sExcelFile, sheet = "Sheet1",  range = "A22:E25")

#Read in max balance
nMaxBalance <- read_excel(sExcelFile, sheet = "Sheet1",  range = "A22:E25")
#create a data frame
dfMaxBalance <- data.frame(Year=dfICSBalance$Year, MaxBal = nMaxBalance$Total[2])

#Read in max deposit per year
dfMaxAnnualAmounts <- data.frame(Year=dfICSBalance$Year, MaxDeposit = nMaxBalance$Total[1], MaxWithdraw = nMaxBalance$Total[3])

cColNames <- colnames(dfICSBalance)

#Melt the data so state columns become a variable
dfICSBalanceMelt <- melt(data = dfICSBalance,id.vars = "Year", measure.vars = cColNames[1:3])

palBlues <- brewer.pal(9, "Blues")

#Plot #1. Stacked bar chart of account balance by state by year. Add individual state limits as secondary y axis
# Prepare state limits as a cumulative amount
cColNamesLimits <- colnames(dfLimits)
dfLimitsMelt <- melt(data=dfLimits, id.vars="New levels with DCP", measure.vars = cColNamesLimits[2:5]) 
dfMaxBalanceCum = dfLimitsMelt %>% filter(`New levels with DCP` == "Max Balance (AF)", variable != 'Total')
#Reorder so Arizona is on top
dfMaxBalanceCum$Order <- c(3,2,1)
dfMaxBalanceCum <- dfMaxBalanceCum[order(dfMaxBalanceCum$Order),]
#Calculate the cumulative total
dfMaxBalanceCum$CumVal <- cumsum(dfMaxBalanceCum$value)
#Replace the Arizona label
dfMaxBalanceCum$StateAsChar <- as.character(dfMaxBalanceCum$variable)
dfMaxBalanceCum$StateAsChar[3] <- "Total/Arizona"

ggplot() +
  
  geom_bar(data=dfICSBalanceMelt, aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  geom_line(data=dfMaxBalance, aes(color="Max Balance", y=MaxBal/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNames[1:3]) +
  scale_color_manual(name="Guide2", values=c("Black")) +
  
  scale_x_continuous(breaks=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2),labels=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2)) +
  
  #Secondary scale with total max balance
  #scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "", breaks = c(nMaxBalance$Total[2])/1e6, labels = c("Max Balance"))) +
  
  #Secondary scale with individual state max balances
  scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "Maximum Balance", breaks = dfMaxBalanceCum$CumVal/1e6, labels = dfMaxBalanceCum$StateAsChar)) +
  
 
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color=FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Intentionally Created Surplus\nAccount Balance\n(MAF)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), 
          legend.text=element_text(size=18),
          legend.position= c(0.1,0.85))
  

#Plot #2. Stacked bar chart of deposits to ICS accounts by state by year

#Calcualte deposits each year the differences by year
dfICSDeposit <- data.frame(-diff(as.matrix(dfICSBalance)))
#Put the correct year back in
dfICSDeposit$Year <- dfICSBalance$Year[1:nrow(dfICSDeposit)]
#Melt the data so state columns become a variable
dfICSDepositMelt <- melt(data = dfICSDeposit,id.vars = "Year", measure.vars = cColNames[1:3])

ggplot() +
  
  geom_bar(data=dfICSDepositMelt, aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  geom_line(data=dfMaxAnnualAmounts, aes(y=MaxDeposit/1e6,x=Year), size=2) +
  geom_line(data=dfMaxAnnualAmounts, aes(color="Max Withdrawal", y=-MaxWithdraw/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNames[1:3]) +
  scale_color_manual(name="Guide2", values=c("Black","Black")) +
  
  scale_x_continuous(breaks=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2),labels=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2)) +
  scale_y_continuous(sec.axis = sec_axis(~. +0, name = "", breaks = c(nMaxBalance$Total[1],-nMaxBalance$Total[3])/1e6, labels = c("Max Deposit","Max Withdraw"))) +
  
  #scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #                  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Deposit to Intentionally Created Surplus Account\n(MAF per year)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18),
        legend.position= c(1.075,0.5))


# Plot Years ICS balance can fund DCP obligation
# Ratio of ICS balance to DCP obligation (Years)
dfICStoDCP$ElevationText <- paste(dfICStoDCP$`Mead Elevation (ft)`, "feet")
cColNamesICStoDCP <- colnames(dfICStoDCP)

dfICStoDCPMelt <- melt(data = dfICStoDCP,id.vars = "ElevationText", measure.vars = cColNamesICStoDCP[5:7])

ggplot(data=dfICStoDCPMelt %>% filter((ElevationText == "1025 feet") | (ElevationText == "1045 feet") )) +
  
  geom_bar(aes(fill=variable,y=value,x=variable), position=position_dodge(), stat="identity") +

  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNamesICStoDCP[5:7], labels = cColNames[1:3]) +

  scale_x_discrete(labels = cColNames[1:3]) +

  facet_wrap( ~ ElevationText) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +

  theme_bw() +
  
  labs(x="", y="Years 2019 ICS balance can fund\nDCP obligation") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18),
        legend.position= c(1.075,0.5))


### Ratio of ICS max withdrawal to DCP obligation
dfICStoDCPMeltMaxWithdrawal <- melt(data = dfICStoDCP,id.vars = "ElevationText", measure.vars = cColNamesICStoDCP[8:10])

ggplot(data=dfICStoDCPMeltMaxWithdrawal %>% filter((ElevationText == "1025 feet") | (ElevationText == "1045 feet") )) +
  
  geom_bar(aes(fill=variable,y=value,x=variable), position=position_dodge(), stat="identity") +

  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNamesICStoDCP[8:10], labels = cColNames[1:3]) +

  scale_x_discrete(labels = cColNames[1:3]) +

  scale_y_continuous(labels = scales::percent) + 
 
  facet_wrap( ~ ElevationText) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Ratio of ICS max withdrawal\nto DCP obligation") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18),
        legend.position= c(1.075,0.5))


### Ratio of ICS max deposit to DCP obligation
dfICStoDCPMeltMaxDeposit <- melt(data = dfICStoDCP,id.vars = "ElevationText", measure.vars = cColNamesICStoDCP[11:13])

ggplot(data=dfICStoDCPMeltMaxDeposit %>% filter((ElevationText == "1025 feet") | (ElevationText == "1045 feet") )) +
  
  geom_bar(aes(fill=variable,y=value,x=variable), position=position_dodge(), stat="identity") +
  
  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNamesICStoDCP[11:13], labels = cColNames[1:3]) +
  #scale_color_manual(name="Guide2", values=c("Black","Black")) +
  
  #scale_fill_continuous(name="Guide1",values = c(palBlues[6],palBlues[9])) +
  
  scale_x_discrete(labels = cColNames[1:3]) +
  #scale_x_continuous(breaks=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2),labels=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2)) +
  scale_y_continuous(labels = scales::percent) + 
  
  facet_wrap( ~ ElevationText) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Ratio of ICS max deposit\nto DCP obligation") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18),
        legend.position= c(1.075,0.5))


