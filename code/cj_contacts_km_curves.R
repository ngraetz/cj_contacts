library(data.table)
library(ggplot2)
library(haven)
library(survminer)
library(survival)
repo <- 'C:/Users/ncgra/Dropbox/Penn/repos/cj_contacts'
## Load data.
cj <- read_dta("C:/Users/ncgra/Downloads/CJ_tas_cleaned_v2.dta")
cj <- as.data.table(cj)
cj[ta_dem_race_eth==1, race_eth := 'NH White']
cj[ta_dem_race_eth==2, race_eth := 'NH Black']
cj[ta_dem_race_eth==3, race_eth := 'Hispanic']
cj <- cj[!is.na(race_eth),]
## Get max year/age of observation by id and subset to unique row for each individual.
cj[, max_age := max(ta_dem_age), by='id_tas']
cj[, min_age := min(ta_dem_age), by='id_tas']
cj[, max_year := max(year), by='id_tas']
cj_surv <- unique(cj[,c('id_tas','svywt_orig_cds','ta_edu','max_year','ta_dem_male','mom_edu','race_eth','max_age','ta_cj_f_arrest_age','ta_cj_f_prob_age','ta_cj_f_incar_age',
                        "svywt_05","svywt_07","svywt_09","svywt_11","svywt_13","svywt_15",'cs_svywt_17on')])
setnames(cj_surv, 'cs_svywt_17on', 'svywt_17')
## Make one variable for a wave-specific longitudinal weight for all respondents.
for(y in c(2005,2007,2009,2011,2013,2015,2017)) {
  this_year <- substr(y,3,4)
  cj_surv[max_year==y, full_long_weight := get(paste0('svywt_',this_year))]
}
## Standardize 2017 longitudinal weight if combining with previous weights.
## Divide max weight in 2017 by max weight in 2015. (87475.00 / 79.212 = 1104.315)
max_2017 <- cj_surv[max_year==2017, max(full_long_weight)]
max_2015 <- cj_surv[max_year==2015, max(full_long_weight)]
cj_surv[max_year==2017, full_long_weight := full_long_weight/(max_2017/max_2015)]
## Where individuals are censored, assign CJ event = 0 and age = max age of observation.
for(v in c('arrest','prob','incar')) {
  cj_surv[, (v) := ifelse(is.na(get(paste0('ta_cj_f_',v,'_age'))),0,1)]
  cj_surv[is.na(get(paste0('ta_cj_f_',v,'_age'))), (paste0('ta_cj_f_',v,'_age')) := max_age]
}
## Collapse < HS to <= HS and > C to C.
cj_surv[, mom_edu := ifelse(mom_edu==5,4,mom_edu)]
cj_surv[, mom_edu := ifelse(mom_edu==1,2,mom_edu)]
cj_surv[, ta_edu := ifelse(ta_edu==5,4,ta_edu)]
cj_surv[, ta_edu := ifelse(ta_edu==1,2,ta_edu)]
## KM function
km_curve_plot <- function(i, d=cj_surv) {
  v <- arg_table[i,v]
  target_sex <- arg_table[i,target_sex]
  target_edu <- arg_table[i,target_edu]
  target_weight <- arg_table[i,target_weight]
  ## Subset
  cj_surv_sub <- d[race_eth %in% c('NH White','NH Black','Hispanic') & !is.na(get(ifelse(target_weight=='Longitudinal weight','svywt_orig_cds','full_long_weight'))),]
  if(target_sex!='Both') cj_surv_sub <- cj_surv_sub[ta_dem_male==ifelse(target_sex=='Male',1,0),]
  if(target_edu!='AllEdu') {
    ## Weighted KM
    mort <- survfit(Surv(get(paste0('ta_cj_f_',v,'_age')),get(v)) ~ race_eth + as.factor(get(ifelse(target_edu=='Maternal education','mom_edu','ta_edu'))),
                    data=cj_surv_sub,
                    weights=get(ifelse(target_weight=='Longitudinal weight','svywt_orig_cds','full_long_weight')))
    ## Unweighted KM (for sample size table)
    mort_unweighted <- survfit(Surv(get(paste0('ta_cj_f_',v,'_age')),get(v)) ~ race_eth + as.factor(get(ifelse(target_edu=='Maternal education','mom_edu','ta_edu'))),
                               data=cj_surv_sub)
  }
  if(target_edu=='AllEdu') {
    ## Weighted KM
    mort <- survfit(Surv(get(paste0('ta_cj_f_',v,'_age')),get(v)) ~ race_eth,
                    data=cj_surv_sub,
                    weights=get(ifelse(target_weight=='Longitudinal weight','svywt_orig_cds','full_long_weight')))
    ## Unweighted KM (for sample size table)
    mort_unweighted <- survfit(Surv(get(paste0('ta_cj_f_',v,'_age')),get(v)) ~ race_eth,
                               data=cj_surv_sub)
  }
  ## Assign color palette
  plot_colors <- c('#c6dbef','#6baed6','#2171b5',
                   '#fcbba1','#fb6a4a','#cb181d',
                   '#c7e9c0','#74c476','#238b45')
  color_labels <- c('Hispanic, <=HS','Hispanic, Some College','Hispanic, College',
                    'NH Black, <=HS','NH Black, Some College','NH Black, College',
                    'NH White, <=HS','NH White, Some College','NH White, College')
  if(target_edu=='AllEdu') {
    plot_colors <- c('#2171b5','#cb181d','#238b45')
    color_labels <- c('Hispanic','NH Black','NH White')
  }
  ## Make plots of curves
  gg_unweighted <- 
  km_plot <- function(km_fit) {
    ggsurvplot(km_fit,
               data = cj_surv_sub,
               size = 2,
               xlim = c(12,28),
               ylim = c(min(mort$surv),1),
               palette = plot_colors,
               conf.int = F,
               pval = TRUE,
               risk.table = TRUE,
               risk.table.y.text.col = T,
               risk.table.y.text = FALSE,
               legend.labs = color_labels,
               risk.table.height = 0.25, 
               break.time.by = 2,
               ggtheme = theme_bw())
  }
  gg <- km_plot(mort)
  gg_unweighted <- km_plot(mort_unweighted)
  ## Grab unweighted sample size table.
  gg$table <- gg_unweighted$table
  ## Add title.
  if(v=='arrest') var <- 'Arrest'
  if(v=='prob') var <- 'Probation'
  if(v=='incar') var <- 'Incarceration'
  gg$plot <- gg$plot + ggtitle(paste0(var,': ',target_sex,', ',target_edu,', ',target_weight))
  ## Save.
  png(paste0(repo,'/',todays_date,'/',target_weight,'_',target_sex,'_',target_edu,'_',var,'.png'),height=9,width=11, units='in',res=800)
  print(gg)
  dev.off()
}
## Estimate KM survival curves for each indicator and strata.
arg_table <- expand.grid(list(c('arrest','prob','incar'),
                              c('Both','Male','Female'),
                              c('Maternal education','Respondent education','AllEdu'),
                              c('Longitudinal weight','Back fill weight')))
arg_table <- as.data.table(arg_table)
setnames(arg_table, c('v','target_sex','target_edu','target_weight'))
arg_table <- arg_table[, lapply(.SD, as.character)]
todays_date <- gsub('-','_',Sys.Date())
dir.create(paste0(repo,'/',todays_date))
lapply(1:dim(arg_table)[1], km_curve_plot)

