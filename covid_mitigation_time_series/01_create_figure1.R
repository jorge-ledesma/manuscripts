## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(ggpubr)
library(ggplot2)
library(readxl)
library(lubridate)
library(data.table)

## SET DIRECTORY
mydir <- paste0("FILEPATH")

#############################################################################################
###                               CREATE COVID CASE FIGURE                                ###
#############################################################################################

## LOAD DATA
data  <- data.table(readxl::read_excel(paste0(mydir, "/data/processed/covid_time_series.xlsx")))
input <- data[, .(subnational_name, subnational_id, date, inc_cases, total_cases, moving_average_inc, pop_2019, 
                  strigency_index, prop_home, home_ratio, ihme_mobility_composite, trip_ratio, eR)]

## AGGREGATE CASES TO US LEVEL
cases <- data[date <= '2020-12-31', .(subnational_id, date, inc_cases)]
cases <- cases[inc_cases >= 0]
cases <- cases[, .(location='US', inc_cases=sum(inc_cases)), by=date]

## CREATE RATE
cases[, population := sum(unique(input$pop_2019))]
cases[, rate := inc_cases/population*100000]
cases[, date := ymd(date)]

## GET IQR
ci <- input[, .(lower=quantile(inc_cases/pop_2019, 0.25), upper=quantile(inc_cases/pop_2019, 0.75)), by=date]
ci[, lower := lower*100000][, upper := upper*100000][, date := as.Date(date)]
cases <- merge(cases, ci, by='date')

## CREATE CASE TIME TREND FIGURE
A<-ggplot(data=cases[date >= '2020-03-01'], aes(x=ymd(date), y=rate)) +
  geom_point(alpha=0.7, size=1.8, shape=19) + 
  scale_x_date('', date_breaks = "months", date_labels = "%b") +
  geom_vline(xintercept=as.Date('2020-06-19')) + geom_vline(xintercept=as.Date('2020-10-07')) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
  scale_y_continuous('COVID-19 incidence proportion', limits=c(0, 125), breaks = seq(0, 120, 20)) + theme_bw() +
  annotate(geom="text", x=as.Date('2020-04-20'), y=110, label="Wave 1", color="red", size=6) +
  annotate(geom="text", x=as.Date('2020-08-15'), y=110, label="Wave 2", color="red", size=6) +
  annotate(geom="text", x=as.Date('2020-12-01'), y=110, label="Wave 3", color="red", size=6) +
  theme(axis.title = element_text(size=13), axis.text=element_text(size=12))

#############################################################################################
###                             CREATE POLICY STRINGENCY FIGURE                           ###
#############################################################################################

## POPULATION WEIGHTED STRIGENCY INDEX
si <- data[date <= '2020-12-31', .(subnational_id, date, strigency_index, pop_2019)]
si[, us_pop  := sum(unique(si$pop_2019))]
si[, std_idx := strigency_index*(pop_2019/us_pop)]

## AGGREGATE TO US LEVEL
si <- si[, .(location='US', index=sum(std_idx)), by=date]
ci <- input[, .(lower=quantile(strigency_index, 0.20, na.rm=T), upper=quantile(strigency_index, 0.80, na.rm=T)), by=date]

## MERGE DATA
ci[, date := as.Date(date)]
si[, date := as.Date(date)]
si <- merge(si, ci, by='date')

## CREATE STRINGENCY TIME TREND
B<-ggplot(data=si[date >= '2020-03-01'], aes(x=ymd(date), y=index)) +
  geom_point(alpha=0.7, size=1.8, shape=19) + 
  scale_x_date('', date_breaks = "months", date_labels = "%b") +
  geom_vline(xintercept=as.Date('2020-06-19')) + geom_vline(xintercept=as.Date('2020-10-07')) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
  scale_y_continuous('Stringency Index', limits=c(0, 100), breaks = seq(0, 100, 20)) + theme_bw() +
  theme(axis.title = element_text(size=14), axis.text=element_text(size=12))

#############################################################################################
###                               CREATE TRIP RATIO FIGURE                                ###
#############################################################################################

## PULL TRAVEL DATA
travel <- fread(paste0(mydir, "/data/raw/us_trips_all.csv"))
travel <- travel[Level == "National"]
travel <- travel[, .(location='US', Date, `Number of Trips`)]
travel[, Date  := as.Date(ymd(Date))]

## PREP TO MERGE
setnames(travel, old=names(travel), new=c("location", "date", "num_trips"))
ref    <- travel[date < '2020-01-01']
travel <- travel[date >= '2020-01-01']

## GET DAY-MONTH INDICATOR
ref[, day := weekdays(date)]
ref[, mon := month(date)]

## COMPUTE MEAN BY DAY-MONTH COMBINATION
ref <- ref[, .(ref_trips=mean(num_trips)), by=.(day, mon, location)]
travel[, `:=` (day=weekdays(date), mon=month(date))]

## MERGE BASELINE DATA
travel <- merge(travel, ref)
travel <- travel[date <= '2020-12-31']

## COMPUTE TR
travel[, `:=` (day=NULL, mon=NULL)]
travel[, trip_ratio := num_trips/ref_trips]

## COMPUTE IQR
ci <- input[date < '2021-01-01', .(subnational_id, date, trip_ratio)]
ci <- ci[, .(lower=quantile(trip_ratio, 0.20, na.rm=T), upper=quantile(trip_ratio, 0.80, na.rm=T)), by=date]

## MERGE DATA
ci[, date := as.Date(date)]
travel[, date := as.Date(date)]
travel <- merge(travel, ci, by='date')

## CREATE TRAVEL TIME SERIES FIGURE
C<-ggplot(data=travel[date >= '2020-03-01'], aes(x=date, y=trip_ratio)) +
  geom_point(alpha=0.7, size=1.8, shape=19) + scale_x_date('', date_breaks = "months", date_labels = "%b") +
  geom_vline(xintercept=as.Date('2020-06-19')) + geom_vline(xintercept=as.Date('2020-10-07')) +
  scale_y_continuous('Trip Ratio', limits = c(0.25, 1.25)) + theme_bw() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
  theme(axis.title = element_text(size=14), axis.text=element_text(size=12))

#############################################################################################
###                                   CREATE RT FIGURE                                    ###
#############################################################################################

## CREATE RO FIGURE
er <- data.table(readxl::read_excel(paste0(mydir, "/data/processed/us_eR_v2.xlsx")))
er[, date := as.Date(date)]

D<-ggplot(data=er[date <= '2020-12-31'], aes(x=date, y=eR)) +
  geom_line(alpha=0.75, size=0.5) + 
  scale_x_date('', date_breaks = "months", date_labels = "%b") +
  geom_hline(yintercept=1, alpha=0.25) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
  geom_vline(xintercept=as.Date('2020-06-19'), alpha=0.6) + geom_vline(xintercept=as.Date('2020-10-07'), alpha=0.6) +
  scale_y_continuous('COVID-19 Effective Reproduction Number', limits = c(0, 18.5)) + theme_bw() +
  theme(axis.title = element_text(size=16), axis.text=element_text(size=14)) 

## SAVE FIGURE
pdf(paste0(mydir,'/figure1.pdf'), width=10, height=14)
ggarrange(A, D, B, C, nrow=4, labels=c('A)', 'B)', 'C)', 'D)'))
dev.off()

