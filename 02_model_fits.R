## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(lme4)
library(readxl)
library(writexl)
library(lubridate)
library(data.table)
library(performance)

## SET DIRECTORY
mylag <- 11
mydir <- paste0("FILEPATH")

start_date <- as.Date("")
end_date   <- as.Date("")

#############################################################################################
###                                 SET FUNCTION FOR SCRIPT                               ###
#############################################################################################

## CLEAN TABLE FOR VETTING MODEL RESULTS
outputs <- function(fit) {
  ## EXTRACT BETAS
  betas <- data.table(parameter=rownames(summary(fit)$coef),
                      beta=summary(fit)$coef[, 1],
                      exp_beta=exp(summary(fit)$coef[, 1]))
  ## EXTRACT CIS
  ints <- confint.merMod(fit, level=0.95, method='Wald')
  cis  <- data.table(ints)
  cis[, parameter := rownames(ints)]
  ## FORMAT CIS
  setnames(cis, old=c('2.5 %', '97.5 %'), new=c('lower', 'upper'))
  cis[, exp_lower := exp(lower)]
  cis[, exp_upper := exp(upper)]
  ## MERGE DATA
  betas <- merge(betas, cis, by='parameter')
  betas[, tval := summary(fit)$coef[, 3]]
  ## RETURN DATA
  return(betas)
}

## FORMATING FOR TABLE
get_mod_tab <- function(x, name){
  x[, (paste0('coef_', name)) := sprintf('%.03f/n(%.03f to %.03f)', exp_beta, exp_lower, exp_upper)]
  x <- x[, .SD, .SDcols=c('parameter', paste0('coef_', name))]
  return(x)
}

#############################################################################################
###                                     PROCESS DATA                                      ###
#############################################################################################

## LOAD DATA
data  <- data.table(readxl::read_excel(paste0(mydir, "/data/processed/covid_time_series.xlsx")))
input <- data[, .(subnational_name, subnational_id, date, inc_cases, total_cases, moving_average_inc, 
                  pop_2019, strigency_index, prop_home, home_ratio, trip_ratio, eR)]

## LAG CASES
input[, lag_total_cases := shift(total_cases, n=mylag, fill=0, type='lead'), by=subnational_name]
input[, lag_inc_cases := shift(inc_cases, n=mylag, fill=0, type='lead'), by=subnational_name]

## LOG TRANSFORM INC RATE
input[, index := 1:.N, by=subnational_name]
input[, rate := (lag_inc_cases/pop_2019)*100000]
input[, log_rate := log(rate)]

#############################################################################################
###                         FIT MIXED TIME SERIES MODELS FOR CASES                       ###
#############################################################################################

## TRIP RATIO MODEL
dt <- input[(date >= start_date) & (date <= end_date)]
fit_tr <- lmer(formula=log_rate~index+I(index^2)+I(index^3)+I(index^4)+trip_ratio+strigency_index+(1|subnational_name), 
               data=dt, 
               control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=2e+05)))

## SUMMARIES
summary(fit_tr)
out  <- outputs(fit_tr)
mod_tr1 <- get_mod_tab(out, 'mod1')
performance::icc(fit_tr)

## STRINGENCY INDEX MODEL
fit_si <- lmer(formula=log_rate~index+I(index^2)+I(index^3)+I(index^4)+strigency_index+(1|subnational_name), 
               data=dt, 
               control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=2e+05)))

## SUMMARIES
summary(fit_si)
out <- outputs(fit_si)
mod_si1 <- get_mod_tab(out, 'mod1')
performance::icc(fit_si)

#############################################################################################
###                           FIT MIXED TIME SERIES MODELS FOR TR                         ###
#############################################################################################

fit <- lmer(formula=log(trip_ratio)~index+I(index^2)+I(index^3)+I(index^4)+strigency_index+(1|subnational_name), 
            data=dt, 
            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=2e+05)))

## SUMMARIES
summary(fit)
out  <- outputs(fit)
str1 <- get_mod_tab(out, 'str1')
performance::icc(fit)

#############################################################################################
###                           FIT MIXED TIME SERIES MODELS FOR Rt                         ###
#############################################################################################

## TRIP RATIO MODEL
fit_tr <- lmer(formula=log(eR)~index+I(index^2)+trip_ratio+strigency_index+(1|subnational_name), 
               data=dt, 
               control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=2e+05)))

## SUMMARIES
summary(fit_tr)
out  <- outputs(fit_tr)
mod_tr1 <- get_mod_tab(out, 'mod1')
performance::icc(fit_tr)

## STRINGENCY INDEX MODEL
fit_si <- lmer(formula=log(eR)~index+I(index^2)+strigency_index+(1|subnational_name), 
            data=tmp, 
            control=lmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=2e+05)))

## SUMMARIES
summary(fit_si)
out <- outputs(fit_si)
mod_si1 <- get_mod_tab(out, 'mod1')
performance::icc(fit_si)


