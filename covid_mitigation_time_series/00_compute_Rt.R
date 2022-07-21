## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(EpiEstim)
library(readxl)
library(writexl)
library(lubridate)
library(data.table)

## SET DIRECTORY
mydir <- paste0("FILEPATH")

#############################################################################################
###                                     PROCESS DATA                                      ###
#############################################################################################

## LOAD DATA
data  <- data.table(readxl::read_excel(paste0(mydir, "/data/processed/covid_time_series_update_2021_08_10.xlsx")))
input <- data[, .(subnational_name, subnational_id, date, inc_cases, total_cases, moving_average_inc)]

## PREP FOR R COMPUTATION
us  <- input[, .(inc_cases=sum(inc_cases), total_cases=sum(total_cases), moving_average_inc=sum(moving_average_inc)), by=.(subnational_id, date)]
all <- data.table(subnational_id=character(), t=integer(), moving_average_inc=numeric(), eR=numeric())

## COMPUTE EFFECTIVE R VALUE BY STATE
for (loc in unique(us$subnational_id)){
  ## SUBSET TO LOCATION
  tmp <- us[(subnational_id == loc) & (date >= '2020-02-28'), .(subnational_id, date, moving_average_inc)]
  tmp[, t := 1:.N]
  
  ## COMPUTE Rt
  res <- estimate_R(incid=tmp$moving_average_inc, 
                    method='parametric_si',
                    config=make_config(list(mean_si=5.2, std_si=3.3/1.96, t_start=2:nrow(tmp), t_end=2:nrow(tmp))))
  
  ## PROCESSES Rt VALUES
  rs <- data.table(res$R)
  rs <- rs[, .(t_start, `Mean(R)`)]
  setnames(rs, old=c('t_start', 'Mean(R)'), new=c('t', 'eR'))
  
  ## MERGE DATA
  tmp <- merge(tmp, rs, by='t')
  all <- rbind(all, tmp, fill=T)
}

## MERGE DATA
sub <- all[, .(subnational_id, date, eR)]
data<- merge(data, sub, by=c('subnational_id', 'date'), all.x=T)

## SAVE DATA
writexl::write_xlsx(data, path=paste0(mydir, "/data/processed/covid_time_series_v2.xlsx"))

