## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(ggpubr)
library(ggplot2)
library(readxl)
library(data.table)

## SET DIRECTORY
mydir <- paste0("FILEPATH")

#############################################################################################
###                              GET TRIP RATIO MODEL RESULTS                             ###
#############################################################################################

## GET FILE NAMES
files <- list.files(paste0(mydir, 'data/results/v2_results/trip_ratio/'))
files <- paste0(mydir, 'data/results/v2_results/trip_ratio/', files)

## LOAD FIRST DATA
data  <- data.table(read_excel(files[1]))

## RECORD LAG
data[, lag := files[1]]
data[, lag := tstrsplit(lag, 'lag')[[2]]]
data[, lag := as.integer(tstrsplit(lag, '_')[[1]])]

## REMAINING FILES
for (f in files[-1]){
  tmp <- data.table(read_excel(f))
  
  tmp[, lag := f]
  tmp[, lag := tstrsplit(lag, 'lag')[[2]]]
  tmp[, lag := as.integer(tstrsplit(lag, '_')[[1]])]
  
  data <- rbind(data, tmp)
}

## GET CORRECT COLUMNS
for (mod in paste0('mod', 1:3)) {
  data[, (paste0(mod, '_mean')) := as.numeric(tstrsplit(get(paste0('coef_', mod)), '/n')[[1]])]
  
  data[, (paste0(mod, '_lower')) := tstrsplit(get(paste0('coef_', mod)), '/n')[[2]]]
  data[, (paste0(mod, '_upper')) := tstrsplit(get(paste0(mod, '_lower')), 'to ')[[2]]]
  data[, (paste0(mod, '_upper')) := as.numeric(tstrsplit(get(paste0(mod, '_upper')), ')')[[1]])]
  
  data[, (paste0(mod, '_lower')) := tstrsplit(get(paste0(mod, '_lower')), ' to')[[1]]]
  data[, (paste0(mod, '_lower')) := as.numeric(sub('.', '', get(paste0(mod, '_lower'))))]
}

tr <- data[parameter %in% c('trip_ratio')]

#############################################################################################
###                             GET STRINENCY INDEX MODEL RESULTS                         ###
#############################################################################################

## GET FILE NAMES
files <- list.files(paste0(mydir, 'data/results/v2_results/si/'))
files <- paste0(mydir, 'data/results/v2_results/si/', files)

## LOAD FIRST DATA
data  <- data.table(read_excel(files[1]))

## RECORD LAG
data[, lag := files[1]]
data[, lag := tstrsplit(lag, 'lag')[[2]]]
data[, lag := as.integer(tstrsplit(lag, '_')[[1]])]

## REMAINING FILES
for (f in files[-1]){
  tmp <- data.table(read_excel(f))
  
  tmp[, lag := f]
  tmp[, lag := tstrsplit(lag, 'lag')[[2]]]
  tmp[, lag := as.integer(tstrsplit(lag, '_')[[1]])]
  
  data <- rbind(data, tmp)
}

## GET CORRECT COLUMNS
for (mod in paste0('mod', 1:3)) {
  data[, (paste0(mod, '_mean')) := as.numeric(tstrsplit(get(paste0('coef_', mod)), '/n')[[1]])]
  
  data[, (paste0(mod, '_lower')) := tstrsplit(get(paste0('coef_', mod)), '/n')[[2]]]
  data[, (paste0(mod, '_upper')) := tstrsplit(get(paste0(mod, '_lower')), 'to ')[[2]]]
  data[, (paste0(mod, '_upper')) := as.numeric(tstrsplit(get(paste0(mod, '_upper')), ')')[[1]])]
  
  data[, (paste0(mod, '_lower')) := tstrsplit(get(paste0(mod, '_lower')), ' to')[[1]]]
  data[, (paste0(mod, '_lower')) := as.numeric(sub('.', '', get(paste0(mod, '_lower'))))]
}

si <- data[parameter %in% c('strigency_index')]

#############################################################################################
###                                 PLOT FIRST WAVE RESULTS                               ###
#############################################################################################

data <- rbind(tr, si)
data[parameter %like% 'index', parameter := 'Stringency index']
data[parameter %like% 'trip', parameter := 'Trip ratio']

data[parameter %like% 'index', y_min1 := 0.98]
data[parameter %like% 'index', y_max1 := 1.02]
data[parameter %like% 'ratio', y_min1 := 0]
data[parameter %like% 'ratio', y_max1 := 1.5]

A<-ggplot(data=data, aes(x=lag, y=mod1_mean)) +
  geom_point(size=2.5, alpha=0.8) +
  facet_wrap(~parameter, scales='free') +
  geom_hline(yintercept=1, alpha=0.9) +
  geom_errorbar(aes(ymin=mod1_lower, ymax=mod1_upper), size=0.5, alpha=0.6, width=0.45) +
  scale_x_continuous(breaks=8:14) +
  xlab('') + ylab('Regression coefficient') + theme_bw() +
  geom_blank(aes(y=y_min1)) + geom_blank(aes(y=y_max1)) +
  theme(axis.title = element_text(size=14.5), axis.text=element_text(size=12), 
        strip.text=element_text(size=12), panel.spacing.x = unit(1.5, 'lines'))

#############################################################################################
###                                 PLOT SECOND WAVE RESULTS                              ###
#############################################################################################

data[parameter %like% 'index', y_min2 := 0.98]
data[parameter %like% 'index', y_max2 := 1.02]
data[parameter %like% 'ratio', y_min2 := 0.5]
data[parameter %like% 'ratio', y_max2 := 8]

B<-ggplot(data=data, aes(x=lag, y=mod2_mean)) +
  geom_point(size=2.5, alpha=0.8) +
  facet_wrap(~parameter, scales='free') +
  geom_hline(yintercept=1, alpha=0.9) +
  geom_errorbar(aes(ymin=mod2_lower, ymax=mod2_upper), size=0.5, alpha=0.6, width=0.45) +
  scale_x_continuous(breaks=8:14) +
  xlab('') + ylab('Regression coefficient') + theme_bw() +
  geom_blank(aes(y=y_min2)) + geom_blank(aes(y=y_max2)) +
  theme(axis.title = element_text(size=14.5), axis.text=element_text(size=12), 
        strip.text=element_text(size=12), panel.spacing.x = unit(1.5, 'lines'))

#############################################################################################
###                                  PLOT THIRD WAVE RESULTS                              ###
#############################################################################################

data[parameter %like% 'index', y_min3 := 0.97]
data[parameter %like% 'index', y_max3 := 1.01]
data[parameter %like% 'ratio', y_min3 := 0.5]
data[parameter %like% 'ratio', y_max3 := 3.5]

C<-ggplot(data=data, aes(x=lag, y=mod3_mean)) +
  geom_point(size=2.5, alpha=0.8) +
  facet_wrap(~parameter, scales='free') +
  geom_hline(yintercept=1, alpha=0.9) +
  geom_errorbar(aes(ymin=mod3_lower, ymax=mod3_upper), size=0.5, alpha=0.6, width=0.45) +
  scale_x_continuous(breaks=8:14) +
  xlab('Lag (days)') + ylab('Regression coefficient') + theme_bw() +
  geom_blank(aes(y=y_min3)) + geom_blank(aes(y=y_max3)) +
  theme(axis.title = element_text(size=14.5), axis.text=element_text(size=12), 
        strip.text=element_text(size=12), panel.spacing.x = unit(1.5, 'lines'))

pdf(paste0(mydir, '/manuscript/sensitivity_analysis_v3.pdf'), width=9, height=12)
ggarrange(A, B, C, nrow=3, labels=c('A)', 'B)', 'C)'))
dev.off()

