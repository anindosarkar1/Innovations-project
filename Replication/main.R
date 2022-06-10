rm(list = ls())
#'*Description ------------------------------------------------------------------------------------------------*
# This file processes the data that is the input to MATLAB for the Corporate default estimation model:
# Run this code modelu by module. Each module is independent.
#'*Load Libraries----------------------------------------------------------------------------------------------* 
library("berryFunctions")
library("dplyr")
library("expss")
library('fredr')
library('gridExtra')
library('gtools')
library('huxtable')
library("imputeTS")
library("ivreg")
library("lubridate")
library("jtools")
library('outreg')
library('plm')
library('pracma')
library("readxl")
library("reshape2")
library("rlist")
library("stringr")
library("tictoc")
library("tidyverse")
library("xtable")
library("doBy")

#'*Intro: -----------------------------------------------------------------------------------------------------*
tic()
#'*Set directories---------------------------------------------------------------------------------------------*
datafile12 <- "EBP.csv"
datafile18 <- "AEM_LevFactor.csv"

#'*EXCESS BOND PREMIUM --------------------------------------------------------------*
temp1 = read.csv(datafile12, header=TRUE, stringsAsFactors = FALSE)
temp1$year = substr(temp1$yearmonth, 1, 4)
temp1$month = substr(temp1$yearmonth, 5, 6)
temp1$date = as.Date(paste(temp1$year, temp1$month, "1", sep = "-"))
ebpTS = temp1[,c("date", "EBP")]

#'*LEVERAGE FACTOR ********************************************************************
temp1 = read.csv(datafile18, header=TRUE, stringsAsFactors = FALSE)
temp2 = temp1[3:nrow(temp1),]
colnames(temp2) = c("Date", "old_fac", "new_fac")
temp2$year = substr(temp2$Date, 1, 4)
temp2$month = as.integer(substr(temp2$Date, 5, 6))*3
temp2$date = as.Date(paste(temp2$year, temp2$month, "1", sep = "-"))
lev_facTS = temp2[, c("date", "new_fac")]
colnames(lev_facTS) = c("date", "lev_fac")
lev_facTS$lev_fac = as.integer(lev_facTS$lev_fac)

#'*FRED STATISTICS -----------------------------------------------------------*
fredr_set_key("6545cf1e393b2e0697316e0708fc31ed")
#(You can get it here: https://research.stlouisfed.org/docs/api/api_key.html)

nbsTS = fredr(series_id = "BABATOTALSAUS",observation_start = as.Date("1920-01-01")) # BUSINESS FORMATION STATISTICS
nbsTS = nbsTS[, c("date", "value")]
colnames(nbsTS) = c("date", "nbs")

ipTS = fredr(series_id = "INDPRO",observation_start = as.Date("1920-01-01")) # INDUSTRIAL PRODUCTION
ipTS = ipTS[, c("date", "value")]
colnames(ipTS) = c("date", "indpro")

vixTS = fredr(series_id = "VIXCLS",observation_start = as.Date("1920-01-01")) # CBOE VIX
vixTS = vixTS[, c("date", "value")]
# vixTS$year = as.integer(year(as.Date(vixTS$date)))
# vixTS$month = as.integer(month(as.Date(vixTS$date)))
# vixTS$yearmonth = vixTS$year*100 + vixTS$month
# vixTS = vixTS %>% group_by(yearmonth) %>% mutate(vix = mean(value, na.rm = TRUE)) # Monthly avergae of daily VIX values
# vixTS = vixTS[, c("yearmonth", "vix")]
# vixTS$year = as.integer(substr(vixTS$yearmonth,1 ,4))
# vixTS$month = as.integer(substr(vixTS$yearmonth,5 ,6))
# vixTS$date = as.Date(paste(vixTS$year, vixTS$month , "01", sep = "-"))
#vixTS = vixTS[, c("date", "vix")]
#vixTS = unique(vixTS)
colnames(vixTS) = c("date", "vix")

recTS = fredr(series_id = "USRECD",observation_start = as.Date("1920-01-01")) # NBER RECESSIONS
recTS = recTS[, c("date", "value")]
colnames(recTS) = c("date", "rec")

cpiTS = fredr(series_id = "CPIAUCSL",observation_start = as.Date("1920-01-01")) # NBER RECESSIONS
cpiTS = cpiTS[, c("date", "value")]
colnames(cpiTS) = c("date", "cpi")

fedfundsTS = fredr(series_id = "FEDFUNDS",observation_start = as.Date("1920-01-01")) # Fed Funds rate
fedfundsTS = fedfundsTS[, c("date", "value")]
colnames(fedfundsTS) = c("date", "fedfunds")
fedfundsTS$fedfunds = fedfundsTS$fedfunds*0.01 # Converting percentages to decimals

#'*COMPUSTATS PANEL FILE **********************************************************************
load(".\\compustatpanel.rda")

#'*Compustats static file ------------------------------------------------------------------*
load(".\\compustatfile.rda")
#'*RnD PANEL **********************************************************************************
load(".\\rndpanel.rda")

#'* STOCK RETURNS PANEL ***************************************************************************
load(".\\returnspanel.rda")

#'*ANALYSIS PANEL 5 -------------------------------------------------------------------*
temp1 = rndpanel

# Merge EBP
temp2 = merge(temp1, ebpTS, by = "date", all.x = TRUE)
#temp2 = na.omit(temp2)

# Merge AEM Lev Factor
temp2 = merge(temp2, lev_facTS, by = "date", all.x = TRUE)
temp2$lev_fac = - temp2$lev_fac  # Note the negative adjustment to counter the fact that lower leverage factor values correspond to higher risk premium. See AEM, 2014
#temp2 = na.omit(temp2)

# Merge firm level controls
temp2 = merge(temp2, compustatpanel, by = c("cusip", "date"), all.x = TRUE)
temp2 = temp2[, c("cusip", "date", "rnd", "total_assets", "EBP", "lev_fac", "roa", "lev")]
temp2$cusip = substr(temp2$cusip, 1,8) # Gotta remove the last checksum digit from compustat cusips so that it matches CRSP cusips.
temp2 = merge(temp2, compustatfile, by = "cusip", all.x = TRUE)

# Merge VIX
temp2 = merge(temp2, vixTS, by = "date", all.x = TRUE)

# Merge recessions
temp2 = merge(temp2, recTS, by = "date", all.x = TRUE)

# Merge CPI
temp2 = merge(temp2, cpiTS, by = "date", all.x = TRUE)

# # Merge interest rates
# temp4 = fedfundsTS
# temp2 = merge(temp2, temp4, by = "date", all.x = TRUE)


temp3 = na.omit(temp2)
temp3 = temp3[order(temp3$cusip, temp3$date),]
analysispanel5 = temp3
analysispanel5 = distinct(analysispanel5, cusip, date, .keep_all = TRUE)

#'* RnD vs EBP PANEL REGRESSIONS ***************************************************
temp1 = analysispanel5
temp2 = temp1 %>% group_by(cusip) %>% mutate(lag_ebp = dplyr::lag(EBP, n=1)) # Lag the EBP
temp2 = temp2 %>% group_by(cusip) %>% mutate(lag_rnd = dplyr::lag(rnd, n=1)) # Lag the rnd
temp2 = temp2 %>% group_by(cusip) %>% mutate(lag_cpi = dplyr::lag(cpi, n=1)) # Lag the rnd
temp2 = temp2 %>% group_by(cusip) %>% mutate(lag_levfac = dplyr::lag(lev_fac, n=1)) # Lag the lev_fac

temp2$growthraternd = log(1+ temp2$rnd) - log(1+temp2$lag_rnd)
temp2$FDrnd = temp2$rnd - temp2$lag_rnd
temp2$rndbyasset = temp2$rnd/ temp2$total_assets
temp2$inflation = log(temp2$cpi) - log(temp2$lag_cpi)
temp2$growthraterndreal = temp2$growthraternd - temp2$inflation

panel6<-pdata.frame(temp2,index=c('cusip', 'date'), drop.index = FALSE)
panel6$inter = panel6$quantile*panel6$EBP # Interaction term
panel6 = na.omit(panel6)

model1 <-lm(growthraternd ~ lag_ebp , data=panel6)
model2 <-plm(growthraternd ~ lag_ebp , index =  c("cusip"), data=panel6, model = "within")
model3 <-plm(growthraternd ~ lag_ebp + vix + rec + roa + lev, index =  c("cusip"), data=panel6, model = "within")
model4 <-lm(growthraternd ~ lag_ebp  + vix + rec + roa + lev + factor(sizequantile) + factor(sic), data=panel6)

model5 <-lm(growthraternd ~ lag_ebp + inter, data=panel6)
model6 <-plm(growthraternd ~ lag_ebp + inter, index =  c("cusip"), data=panel6, model = "within")
model7 <-plm(growthraternd ~ lag_ebp + inter + vix + rec + roa + lev, index =  c("cusip"), data=panel6, model = "within")
model8 <-lm(growthraternd ~ lag_ebp + inter  + vix + rec + roa + lev + factor(sizequantile) + factor(sic), data=panel6)

model9 <-lm(growthraternd ~ lag_levfac, data=panel6)
model10 <-plm(growthraternd ~ lag_levfac, index =  c("cusip", "yearmonth"), data=panel6, model = "within")
model11 <-plm(growthraternd ~ lag_levfac + vix + rec + roa + lev, index =  c("cusip", "yearmonth"), data=panel6, model = "within")
model12 <-lm(growthraternd ~ lag_levfac  + vix + rec + roa + lev + factor(sizequantile) + factor(sic), data=panel6)

model13 <-lm(growthraternd ~ lag_levfac + inter, data=panel6)
model14 <-plm(growthraternd ~ lag_levfac + inter, index =  c("cusip"), data=panel6, model = "within")
model15 <-plm(growthraternd ~ lag_levfac + inter+ vix + rec + roa + lev, index =  c("cusip"), data=panel6, model = "within")
model16 <-lm(growthraternd ~ lag_levfac + inter + vix + rec + roa + lev + factor(sizequantile) + factor(sic), data=panel6)

#'*TABLES ************************************************************************************************************ 
table1 = huxreg(model1, model2, model3, model4, model5, model6, model7, model8)
# Clean tables 
temp = as.data.frame(table1)
for(i in 1:(nrow(temp)-1)){
  temp1 = length(grep("factor", temp[i, 1]) == 1)
  if(temp1 == 1){
    temp[(i+1), 1] = "blah"
  }
}
temp = temp[- grep("factor", temp$names),]
temp = temp[- grep("blah", temp$names),]
table1 = temp

table2 = huxreg(model9, model10, model11, model12, model13, model14, model15, model16)
# Clean tables 
temp = as.data.frame(table2)
for(i in 1:(nrow(temp)-1)){
  temp1 = length(grep("factor", temp[i, 1]) == 1)
  if(temp1 == 1){
    temp[(i+1), 1] = "blah"
  }
}
temp = temp[- grep("factor", temp$names),]
temp = temp[- grep("blah", temp$names),]
table2 = temp

# Output latex tables
print(xtable(table1, type = "latex", digits = 3), file = "table1.tex", include.rownames=FALSE)
print(xtable(table2, type = "latex", digits = 3), file = "table2.tex", include.rownames=FALSE)
print(xtable(table1, digits = 3), file = "table1.txt", include.rownames=FALSE)
print(xtable(table2, digits = 3), file = "table2.txt", include.rownames=FALSE)

