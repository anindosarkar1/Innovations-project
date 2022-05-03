rm(list = ls())
#'*Description ------------------------------------------------------------------------------------------------*
# This file processes the data that is the input to MATLAB for the Corporate default estimation model:
# Run this code modelu by module. Each module is independent.
#'*Load Libraries----------------------------------------------------------------------------------------------* 
library("dplyr")
library("expss")
library('gtools')
library("imputeTS")
library("lubridate")
library("jtools")
library('plm')
library('pracma')
library("readxl")
library("reshape2")
library("stringr")
library("tidyverse")
library("xtable")
library("doBy")
#'*Set directories---------------------------------------------------------------------------------------------*
datafile1 <- ".\\Data\\KPSS_2020_public.csv\\KPSS_2020_public.csv"
datafile2 <- ".\\Data\\patent_permco_permno_match_public_2020.csv\\patent_permco_permno_match_public_2020.csv"
datafile3 <- ".\\Data\\Sec_CDS_data_v1.xlsx"
datafile4 <- ""
datafile5 <- ""
datafile6 <- ".\\Code\\CDScusipmatch.xlsx"
datafile7 <- ".\\Code\\CRSP_identifiers.csv"
datafile8 <- ".\\Code\\RnD.csv"
datafile9 <- ".\\Code\\compustat_v3.csv"
datafile10 <- ".\\Code\\compustat_v4.csv" # For firm level controls
#'* Patents panel --------------------------------------------------------------*
temp1 = read.csv(datafile1, header=TRUE, stringsAsFactors = FALSE)
temp1$patentdummy = 1
temp3 = summaryBy(cites + patentdummy ~ permno + yearmonth + issue_date, FUN=c(sum), data=temp1)
temp3$issue_date = as.Date(temp3$issue_date, format = '%m/%d/%Y')
colnames(temp3)[3] <- 'cites'
colnames(temp3)[4] <- 'num_patents'

# Add cusip and company name from CRSP to patents panel
temp2 = read.csv(datafile7, header=TRUE, stringsAsFactors = FALSE)
temp2 = temp2[, c(1,3,4,5)]
temp2 = unique(temp2)
colnames(temp2) = c('permno', 'firm_name', 'permco', 'cusip')

temp4 = read.csv(datafile2, header=TRUE, stringsAsFactors = FALSE)
temp4 = temp4[, c(2,3)]
temp4 = unique(temp4)
temp3 = merge(temp3, temp4, by = "permno", all.x = TRUE) # Adding the permco from the KPSS files
temp3 = temp3[, -c(1)]

temp3 = merge(temp3, temp2, by = "permco", all.x = TRUE)
temp3 = na.omit(temp3)

#patentspanel = temp3[, c(3:5, 7)]

#temp3 = aggregate(temp3[, c('num_patents', 'cites')], list(temp1$cusip, temp1$firm_name, temp1$issue_date), mean)

patentspanel = temp3[, c('cusip', 'firm_name', 'issue_date', 'num_patents', 'cites')]
patentspanel = patentspanel[order(patentspanel$cusip, patentspanel$issue_date),]

save(patentspanel,file=".\\Code\\patentspanel.rda")
write.table(patentspanel, ".\\Code\\patentspanel.csv", sep = ",", row.names = FALSE, col.names = TRUE )

#'*PATENTS STATIC FILE ---------------------------------------------------------------------------------------*
patentsstatic = patentspanel[, c(1:2)]
patentsstatic = unique(patentsstatic)

#'* CDS panel ----------------------------------------------*
temp1 = read_excel(datafile3)
temp2 <- reshape(temp1, idvar = colnames(temp1)[1:2], varying = colnames(temp1[3:ncol(temp1)]), v.names = "cdsspread",
                 timevar = "date", times = colnames(temp1[3:ncol(temp1)]), direction = "long", new.row.names = 1:356668)
temp2 = temp2[temp2$cdsspread != "NA",]

# Extract maturities

for (i in c("6M", "1Y", "2Y","3Y", "4Y", "5Y", "7Y", "10Y", "20Y", "30Y")){
  for (j in 1:nrow(temp2)){
    if (grepl(i, temp2[j, 2]) == TRUE){
      temp2[j, 'maturity'] = i
    }
  }  
}
colnames(temp2)[2] = c('cds_name')

temp3 = read_excel(datafile6)   # merge the cusips to cds ids
colnames(temp3)[4] = 'security_id'
temp3 = temp3[, c('cusip', 'security_id')]
temp4 = merge(x=temp2,y=temp3,by="security_id",all.x=TRUE)

cdspanel = temp4[temp4$maturity == '30Y', c('cusip', 'security_id', 'cds_name', 'date', 'maturity', 'cdsspread')]
cdspanel = cdspanel[order(cdspanel$cusip, cdspanel$security_id, cdspanel$date),]

save(cdspanel,file=".\\Code\\cdspanel.rda")
write.table(cdspanel, ".\\Code\\cdspanel.csv", sep = ",", row.names = FALSE, col.names = TRUE )

#'*CDS static file -------------------------------------------------------------------------*
cdsstatic = cdspanel[, c(1,2,3,5)]
cdsstatic = unique(cdsstatic)

#'*Compustats static file ------------------------------------------------------------------*
#'This file has the book values and market values used for growth and value deciles.
# Make quantiles by marketvalue to bookvalue
temp1 = read.csv(datafile9, header=TRUE, stringsAsFactors = FALSE)   
temp1$bookvalue = temp1$lt  + temp1$mkvalt
temp1$mvbv = temp1$mkvalt/ temp1$bookvalue
temp2 = aggregate(temp1[, c('mvbv', 'bookvalue')], list(temp1$cusip), mean)
colnames(temp2) = c('cusip', 'averagemvbv', 'avgbookvalue')
temp2 = na.omit(temp2)
temp2$quantile <- ntile(temp2$averagemvbv, 10)  
temp2$sizequantile <- ntile(temp2$avgbookvalue, 10)  

# Adding the controls file
temp3 = read.csv(datafile10, header=TRUE, stringsAsFactors = FALSE)   
temp3$year = as.integer(substr(temp3$datadate, 1,4))
temp3$month = as.integer(substr(temp3$datadate, 5,6))
temp3$yearmonth = temp3$year*100 + temp3$month

temp4 = temp3[, c('cusip', 'sic')]
temp4 = unique(temp4)

temp2 = merge(temp2, temp4, by = 'cusip', all.x = TRUE)
temp2 = na.omit(temp2)

temp2$cusip = substr(temp2$cusip, 1,8)  # Gotta remove the last checksum digit from compustat cusips so that it matches CRSP cusips.
compustatfile = temp2
save(compustatfile ,file=".\\Code\\compustatfile.rda")
write.table(compustatfile, ".\\Code\\compustatfile.csv", sep = ",", row.names = FALSE, col.names = TRUE )

#'*Analysis static ---------------------------------------------------------------------------*
# First merge the compustats file with the patents static file
temp1 = merge(patentsstatic, compustatfile, by = 'cusip', all.x = TRUE)
temp1 = na.omit(temp1)

# Then merge this file with the cdsstatic file
cdsstatic$cusip = substr(cdsstatic$cusip, 1,8)  # Gotta remove the last checksum digit from compustat cusips so that it matches CRSP cusips.
temp2 = merge(temp1, cdsstatic, by = 'cusip', all.x = TRUE)
temp2 = na.omit(temp2)
temp2 = temp2[temp2$maturity == '30Y',]
analysisstatic = temp2

#'*Analysis panel ------------------------------------------------------------------------------*
temp1 = analysisstatic
# merge the patents panel

temp3 = patentspanel
temp3$year = as.integer(substr(temp3$issue_date, 1,4))
temp3$month = as.integer(substr(temp3$issue_date, 6,7))
temp3$yearmonth = temp3$year*100 + temp3$month

temp3 = aggregate(temp3[, c('num_patents', 'cites')], list(temp3$cusip, temp3$firm_name, temp3$yearmonth), mean)
colnames(temp3)[1:3] = c('cusip', 'firm_name', 'yearmonth') 
  
temp2 = merge(temp1, temp3, by = 'cusip', all.x = TRUE)
temp2 = temp2[order(temp2$cusip, temp2$yearmonth),]
temp6 = temp2[, c('cusip', 'yearmonth', "num_patents", 'cites')]

# merge the cds panel
cdspanel$cusip = substr(cdspanel$cusip, 1,8) # Gotta remove the last checksum digit from compustat cusips so that it matches CRSP cusips.
temp4 = merge(temp1, cdspanel, by = 'cusip', all.x = TRUE)
temp4$year = as.integer(substr(temp4$date, 7,10))
temp4$month = as.integer(substr(temp4$date, 1,2))
temp4 = temp4[, c('cusip', 'firm_name', 'date', 'year', 'month', 'quantile','sic', 'sizequantile', 'cdsspread')]
temp4 = temp4[order(temp4$cusip, temp4$year, temp4$month),]
temp4$yearmonth = temp4$year*100 + temp4$month

temp5 = merge(temp4, temp6, by = c('cusip', 'yearmonth'), all.x = TRUE)
temp5 = temp5[, c('cusip', 'yearmonth', 'firm_name', 'date', 'quantile', 'sic', 'sizequantile', 'cdsspread', 'num_patents', 'cites')]

temp5[is.na(temp5)] = 0
temp5$cdsspread = as.integer(temp5$cdsspread)

analysispanel = temp5
dupidx = duplicated(analysispanel, by = c("cusip", "yearmonth")) # Remove some residual duplicates
analysispanel = analysispanel[!dupidx,]
analysispanel = distinct(analysispanel, cusip, yearmonth, .keep_all = TRUE)

# Make the yearmonth in date format for figures and stuff
analysispanel$ymdate = as.Date(paste(substr(analysispanel$yearmonth, 1,4), "-",  substr(analysispanel$yearmonth, 5,6), "-01", sep = ""))

#'*Fixed effects regression -------------------------------------------------------------------------*

panel<-pdata.frame(analysispanel,index=c('cusip', 'ymdate'), drop.index = FALSE) 
panel$lag_cdsspread=plm::lag(panel$cdsspread,24)
panel = cbind(panel, lag_cdsspread)
 
panel$inter = panel$quantile*panel$lag_cdsspread # Interaction term

# Basic fixed effects
model1 <-lm(num_patents ~ lag_cdsspread + factor(cusip) - 1, data=panel)
model2 <-lm(cites ~ lag_cdsspread + factor(cusip) - 1, data=panel)

# With interaction term
model3 <-lm(num_patents ~ lag_cdsspread + inter + factor(cusip) - 1, data=panel)
model4 <-lm(cites ~ lag_cdsspread + inter + factor(cusip) - 1, data=panel)

# With interaction term and controls
model5 <-lm(num_patents ~ lag_cdsspread + inter + factor(sizequantile) + factor(sic) + factor(cusip) - 1, data=panel)
model6 <-lm(cites ~ lag_cdsspread + inter + factor(sizequantile) + factor(sic) + factor(cusip) - 1, data = panel)

export_summs(model1, model2, model3, model4, model5, model6, digits =4)

table1 = huxreg(model1, model2, model3, model4, model5, model6)
table1 = table1[c(1:2,100,184:188),]
table1[3,1] = '(lag cds spreads) x (mkt to book)'
table1 = insert_row(table1,c('Industry FE','No','No','No','No','Yes','Yes' ), after = 3,fill = NULL,colspan = 1,copy_cell_props = TRUE)
table1 = insert_row(table1,c('Size bin FE','No','No','No','No','Yes','Yes' ), after = 4,fill = NULL,colspan = 1,copy_cell_props = TRUE)
print('************************************ TABLE 1: Effect of credit constraints on innovation **************************************')
table1
#'*Figures ------------------------------------------------------------------------------------------*

