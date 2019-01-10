# BARRA RISK FACTORS
# to do ...
# sample fitlering within the GR func ???
# trading frequency minimum requirement

# fill in methodology ???


#### BETA ####
#' @export
gr.beta <- function(TS, bmk = "EI000985"){
  
  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  # param
  tau = 63
  h = 252
  #
  lambda <- 0.5^(1/tau)
  wgt <- lambda ^ ((h-1):0) # unscaled
  #
  datelist <- sort(unique(ts$date))
  ts2 <- data.table::data.table()
  for(i in 1:length(datelist)){
    date_ <- datelist[i]
    stock_ <- subset(ts, date == datelist[i], select = "stockID", drop = TRUE)
    rebdate_ <- getRebDates(trday.nearby(date_, by = -(h-1)), date_, rebFreq = "day") 
    # rebdate_ <- rebdate_[-length(rebdate_)]
    ts2_ <- data.table::CJ("date" = rebdate_, "stockID" = stock_, "rebdate" = date_)
    ts2 <- rbind(ts2, ts2_)
  }
  ts2 <- unique(ts2)
  ts2_shrink <- unique(ts2[,c("date","stockID")])
  datelist2 <- sort(unique(ts2_shrink$date))
  # ipo problem ?
  
  # MDATA_RF
  mdata_rf <- get_rf(datelist2)
  mdata_rf <- data.table::as.data.table(mdata_rf)
  mdata_rf[,Rf_day := (1+Rf/100)^(1/252) - 1,]

  # TS_UNION
  if(is.null(bmk)){
    ts_union <- ts2
  }else{
    ts_union <- getIndexComp(indexID = bmk, endT = datelist2)
  }
  
  # MDATA_RTN
  stocklist <- unique(c(unique(ts2$stockID), unique(ts_union$stockID)))
  mdata_rtn <- getQuote(stocklist,min(datelist2),max(datelist2),'pct_chg',tableName = 'QT_DailyQuote',datasrc = 'quant',split = FALSE)
  mdata_rtn <- data.table::as.data.table(mdata_rtn)
 
  # MDATA_RT
  mdata_ffv <- gf_cap(unique(ts_union[,c('date','stockID')]), var = "free_cap", varname = "ffv")
  mdata_ffv <- data.table::as.data.table(mdata_ffv)
  
  tsr_union <- mdata_rtn[ts_union, on = .(date,stockID)]
  tsr_union <- mdata_ffv[tsr_union, on = .(date,stockID)]
  tsr_union[is.infinite(ffv), ffv := NA]
  mdata_rt <- tsr_union[,.("Rt" = sum(pct_chg * ffv, na.rm = TRUE)/sum(ffv, na.rm = TRUE)), by = .(date)]
  
  # MDATA SUS
  mdata_sus <- is_suspend(ts2_shrink)
  mdata_sus <- data.table::as.data.table(mdata_sus)
  
  # 
  tsr2 <- mdata_rtn[ts2, on = .(date, stockID)]
  tsr2 <- mdata_sus[tsr2, on = .(date, stockID)]
  tsr2 <- mdata_rf[tsr2, on = .(date <= date), mult = 'last']
  tsr2 <- mdata_rt[tsr2, on = .(date)]
  # for periodrtn NA, set sus to TRUE
  tsr2[is.na(pct_chg), sus := TRUE]
  tsr2[is.na(pct_chg), pct_chg := 0]
  tsr2[,ind_vec := !sus,]

  # necessary, order matches wgt
  data.table::setorder(tsr2, rebdate, stockID, date)
  tsr2[,'exc_rtn_y' := pct_chg - Rf_day,]
  tsr2[,'exc_rtn_x' := Rt - Rf_day,]
  tsr2[,reg_wgt := wgt * ind_vec/sum(wgt * ind_vec), by = .(rebdate, stockID)]
  # NaN produces : sum(ind_vec) 0, all FALSE
  tsr2[,x_diff := exc_rtn_x - sum(exc_rtn_x * reg_wgt),by = .(rebdate, stockID)]
  tsr2[,y_diff := exc_rtn_y - sum(exc_rtn_y * reg_wgt),by = .(rebdate, stockID)]
  result <- tsr2[,.('factorscore' = sum(reg_wgt * x_diff * y_diff)/sum(reg_wgt * x_diff * x_diff)),by = .(rebdate, stockID)]
  
  colnames(result) <- c("date","stockID","factorscore")
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
  
}

#### MOMENTUM ####
#' @export
gr.rstr <- function(TS){
  
  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  h <- 504
  tau <- 126
  lag = 21
  #
  lambda <- 0.5^(1/tau)
  wgt <- lambda ^ ((h-1):0) # unscaled
  #
  datelist <- sort(unique(ts$date))
  ts2 <- data.table::data.table()
  for(i in 1:length(datelist)){
    date_ <- datelist[i]
    stock_ <- subset(ts, date == datelist[i], select = "stockID", drop = TRUE)
    rebdate_ <- getRebDates(trday.nearby(date_, by = -(h + lag - 1)), date_, rebFreq = "day")
    N_ <- length(rebdate_)
    rebdate_ <- rebdate_[-((N_ - lag + 1):N_)]
    ts2_ <- data.table::CJ("date" = rebdate_, "stockID" = stock_, "rebdate" = date_)
    ts2 <- rbind(ts2, ts2_)
  }
  ts2 <- unique(ts2)
  ts2_shrink <- unique(ts2[,c("date","stockID")])
  datelist2 <- sort(unique(ts2_shrink$date))
  # ipo problem?
  
  # MDATA RF
  mdata_rf <- get_rf(datelist2)
  mdata_rf <- data.table::as.data.table(mdata_rf)
  mdata_rf[,Rf_day := (1+Rf/100)^(1/252) - 1,]
  # MDATA RTN
  mdata_rtn <- getQuote(unique(ts2_shrink$stockID),min(datelist2),max(datelist2),'pct_chg',tableName = 'QT_DailyQuote',datasrc = 'quant',split = FALSE)
  mdata_rtn <- data.table::as.data.table(mdata_rtn)
  # MDATA SUS
  mdata_sus <- is_suspend(ts2_shrink)
  mdata_sus <- data.table::as.data.table(mdata_sus)
  
  
  tsr2 <- mdata_rtn[ts2, on = .(date, stockID)]
  tsr2 <- mdata_sus[tsr2, on = .(date, stockID)]
  tsr2 <- mdata_rf[tsr2, on = .(date <= date), mult = 'last']
  tsr2[is.na(pct_chg), sus := TRUE]
  tsr2[is.na(pct_chg), pct_chg := 0]
  tsr2[,ind_vec := !sus,]
  tsr2[,var_tmp := (log(1+pct_chg) - log(1+Rf_day)),]
  #
  data.table::setorder(tsr2, rebdate, stockID, date)
  result <- tsr2[,.('factorscore' = sum(var_tmp * ind_vec * wgt)/sum(ind_vec * wgt)),by = .(rebdate, stockID)]
  
  colnames(result) <- c("date","stockID","factorscore")
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}

#### SIZE ####
#' @export
gr.lncap <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf_cap(TS, var = 'mkt_cap')
  mdata$factorscore <- log(mdata$factorscore)
  return(mdata)
}

#### EARNING YIELDS ####
#' @export
gr.epibs <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.F_PE(TS = TS, fillna = FALSE)
  if(is.character(mdata$factorscore)){
    mdata$factorscore <- as.numeric(mdata$factorscore)
  }
  mdata$con_type <- NULL
  mdata$factorscore <- 1/mdata$factorscore
  return(mdata)
}
#' @export
gr.etop <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.EP_ttm(TS)
  return(mdata)
}
#' @export
gr.cetop <- function(TS){
  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  ts_rpt <- getrptDate_newest(ts)
  funchar <- '"OCF_LAST12M", Last12MData(Rdate,48018)'
  mdata <- rptTS.getFin_ts(ts_rpt, funchar)
  mdata <- gf_cap(mdata, var = 'mkt_cap', varname = 'mkt_cap')
  mdata$factorscore <- (mdata$OCF_LAST12M / (1e+8))/(mdata$mkt_cap)
  
  result <- mdata[,c("date","stockID","factorscore")]
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  return(final_result)
}

#### RESIDUAL VOLATILITY ####
#' @export
gr.dastd <- function(TS){
  
  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  h <- 252
  tau <- 42
  #
  lambda <- 0.5^(1/tau)
  wgt <- lambda ^ ((h-1):0) # unscaled
  #
  datelist <- sort(unique(ts$date))
  ts2 <- data.table::data.table()
  for(i in 1:length(datelist)){
    date_ <- datelist[i]
    stock_ <- subset(ts, date == datelist[i], select = "stockID", drop = TRUE)
    rebdate_ <- getRebDates(trday.nearby(date_, by = -(h-1)), date_, rebFreq = "day")
    # rebdate_ <- rebdate_[-length(rebdate_)]
    ts2_ <- expand.grid("date" = rebdate_, "stockID" = stock_, "rebdate" = date_)
    ts2 <- rbind(ts2, ts2_)
  }
  ts2 <- unique(ts2)
  ts2_shrink <- unique(ts2[,c("date","stockID")])
  datelist2 <- sort(unique(ts2_shrink$date))
  
  # MDATA RF
  mdata_rf <- get_rf(datelist2)
  mdata_rf <- data.table::as.data.table(mdata_rf)
  mdata_rf[,Rf_day := (1+Rf/100)^(1/252) - 1,]
  
  # MDATA RTN
  mdata_rtn <- getQuote(unique(ts2_shrink$stockID),min(datelist2),max(datelist2),'pct_chg',tableName = 'QT_DailyQuote',datasrc = 'quant',split = FALSE)
  mdata_rtn <- data.table::as.data.table(mdata_rtn)
  # MDATA SUS
  mdata_sus <- is_suspend(ts2_shrink)
  mdata_sus <- data.table::as.data.table(mdata_sus)
  
  tsr2 <- mdata_rtn[ts2, on = .(date, stockID)]
  tsr2 <- mdata_sus[tsr2, on = .(date, stockID)]
  tsr2 <- mdata_rf[tsr2, on = .(date <= date), mult = 'last']
  tsr2[is.na(pct_chg), sus := TRUE]
  tsr2[is.na(pct_chg), pct_chg := 0]
  tsr2[,ind_vec := !sus,]
  
  #
  tsr2[,'rtn_diff' := (pct_chg - sum(pct_chg * ind_vec * wgt)/sum(ind_vec * wgt))^2, by = .(rebdate, stockID)]
  result <- tsr2[,.('factorscore' = sqrt(sum(rtn_diff * ind_vec * wgt)/sum(ind_vec * wgt))),by = .(rebdate, stockID)]
  
  colnames(result) <- c("date","stockID","factorscore")
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}
#' @export
gr.cmra <- function(TS){
  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  datelist <- sort(unique(ts$date))
  ts2 <- data.table::data.table()
  for(i in 1:length(datelist)){
    date_ <- datelist[i]
    stock_ <- subset(ts, date == datelist[i], select = "stockID", drop = TRUE)
    rebdate_ <- date_
    for(j in 1:12){
      rebdate_ <- c(trday.nearby(rebdate_[1],by = -21), rebdate_)
    }
    reb_df <- data.table::data.table('date' = rebdate_[-length(rebdate_)], 'date_end' = rebdate_[-1])
    ts2_ <- data.table::CJ("date" = reb_df$date, "stockID" = stock_, "rebdate" = date_)
    ts2_ <- reb_df[ts2_, on = .(date)]
    ts2 <- rbind(ts2, ts2_)
  }
  ts2 <- unique(ts2)
  ts2_shrink <- unique(ts2[,c("date","date_end","stockID")])
  # ipo problem ?
  colnames(ts2) <- c("begT", "endT", "stockID", "rebdate")
  tsr2 <- getPeriodrtn(ts2)
  colnames(tsr2) <- c("date", "date_end", "stockID", "rebdate", "periodrtn")
  tsr2$periodrtn[is.na(tsr2$periodrtn)] <- 0   # to do ...
  
  # Rf
  datelist_union <- getRebDates(begT = min(tsr2$date), endT = max(tsr2$date_end), rebFreq = "day")
  mdata_rf <- get_rf(rebDates = datelist_union)
  mdata_rf <- data.table::as.data.table(mdata_rf)
  mdata_rf[,Rf_day := (1+Rf/100)^(1/252) - 1,]
  mdata_rf$Rf_M <- RcppRoll::roll_prodr( (1 + mdata_rf$Rf_day), n = 21L ) - 1
  mdata_rf <- mdata_rf[,c("date","Rf_M")]
  colnames(mdata_rf) <- c("date_end", "Rf_M")
  
  # match date and date_end
  tsr2 <- mdata_rf[tsr2, on = .(date_end <= date_end), mult = 'last']
  tsr2[,var_tmp := log(1+periodrtn) - log(1+Rf_M),]
  data.table::setorder(tsr2, rebdate, stockID, -date_end)
  mdata_zt <- tsr2[,.(z_t = cumsum(var_tmp)),by = .(rebdate, stockID)]
  
  result <- mdata_zt[,.('factorscore' = max(z_t) - min(z_t)),by = .(rebdate, stockID)]
  colnames(result) <- c("date","stockID","factorscore")
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}
#' @export
gr.hsigma <- function(TS, bmk = "EI000985"){

  # input checking
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  # param
  tau = 63
  h = 252
  #
  lambda <- 0.5^(1/tau)
  wgt <- lambda ^ ((h-1):0) # unscaled
  #
  datelist <- sort(unique(ts$date))
  ts2 <- data.table::data.table()
  for(i in 1:length(datelist)){
    date_ <- datelist[i]
    stock_ <- subset(ts, date == datelist[i], select = "stockID", drop = TRUE)
    rebdate_ <- getRebDates(trday.nearby(date_, by = -(h-1)), date_, rebFreq = "day") 
    # rebdate_ <- rebdate_[-length(rebdate_)]
    ts2_ <- data.table::CJ("date" = rebdate_, "stockID" = stock_, "rebdate" = date_)
    ts2 <- rbind(ts2, ts2_)
  }
  ts2 <- unique(ts2)
  ts2_shrink <- unique(ts2[,c("date","stockID")])
  datelist2 <- sort(unique(ts2_shrink$date))
  # ipo problem ?
  
  # MDATA_RF
  mdata_rf <- get_rf(datelist2)
  mdata_rf <- data.table::as.data.table(mdata_rf)
  mdata_rf[,Rf_day := (1+Rf/100)^(1/252) - 1,]
  
  # TS_UNION
  if(is.null(bmk)){
    ts_union <- ts2
  }else{
    ts_union <- getIndexComp(indexID = bmk, endT = datelist2)
  }
  
  # MDATA_RTN
  stocklist <- unique(c(unique(ts2$stockID), unique(ts_union$stockID)))
  mdata_rtn <- getQuote(stocklist,min(datelist2),max(datelist2),'pct_chg',tableName = 'QT_DailyQuote',datasrc = 'quant',split = FALSE)
  mdata_rtn <- data.table::as.data.table(mdata_rtn)
  
  # MDATA_RT
  mdata_ffv <- gf_cap(unique(ts_union[,c('date','stockID')]), var = "free_cap", varname = "ffv")
  mdata_ffv <- data.table::as.data.table(mdata_ffv)
  
  tsr_union <- mdata_rtn[ts_union, on = .(date,stockID)]
  tsr_union <- mdata_ffv[tsr_union, on = .(date,stockID)]
  tsr_union[is.infinite(ffv), ffv := NA]
  mdata_rt <- tsr_union[,.("Rt" = sum(pct_chg * ffv, na.rm = TRUE)/sum(ffv, na.rm = TRUE)), by = .(date)]
  
  # MDATA SUS
  mdata_sus <- is_suspend(ts2_shrink)
  mdata_sus <- data.table::as.data.table(mdata_sus)
  
  # 
  tsr2 <- mdata_rtn[ts2, on = .(date, stockID)]
  tsr2 <- mdata_sus[tsr2, on = .(date, stockID)]
  tsr2 <- mdata_rf[tsr2, on = .(date <= date), mult = 'last']
  tsr2 <- mdata_rt[tsr2, on = .(date)]
  # for pct_chg NA, set sus to TRUE
  tsr2[is.na(pct_chg), sus := TRUE]
  tsr2[is.na(pct_chg), pct_chg := 0]
  tsr2[,ind_vec := !sus,]
  
  # necessary, order matches wgt
  data.table::setorder(tsr2, rebdate, stockID, date)
  tsr2[,'exc_rtn_y' := pct_chg - Rf_day,]
  tsr2[,'exc_rtn_x' := Rt - Rf_day,]
  tsr2[,reg_wgt := wgt * ind_vec/sum(wgt * ind_vec), by = .(rebdate, stockID)] 
  # NaN produces : sum(ind_vec) 0, all FALSE
  tsr2[,x_diff := exc_rtn_x - sum(exc_rtn_x * reg_wgt),by = .(rebdate, stockID)]
  tsr2[,y_diff := exc_rtn_y - sum(exc_rtn_y * reg_wgt),by = .(rebdate, stockID)]
  tsr2[,'beta' := sum(reg_wgt * x_diff * y_diff)/sum(reg_wgt * x_diff * x_diff),by = .(rebdate, stockID)]
  tsr2[,'alpha' := sum(exc_rtn_y * reg_wgt) - beta * sum(exc_rtn_x * reg_wgt),by = .(rebdate, stockID)]
  tsr2[,'resid' := exc_rtn_y - alpha - beta * exc_rtn_y,]
  result <- tsr2[,.('beta' = head(beta,1), 'sigma' = sd(resid)),by = .(rebdate, stockID)]
  result <- renameCol(result, "rebdate", "date")
  result <- as.data.frame(result)
  result <- factor_orthogon(result, method = 'reg', sectorAttr = NULL, regType = "lm")
  result <- result[,c("date","stockID","sigma")]
  colnames(result) <- c("date","stockID","factorscore")
  # result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))

  return(final_result)
  
}

#### GROWTH ####
#' @export
gr.sgro <- function(TS){
  
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  
  # sales growth, trailing five years
  rpt_ts <- getrptDate_newest(ts, freq = "y")
  rpt_ts$year_index <- 5
  
  rpt_ts2 <- rpt_ts
  for(i in 1:4){
    rpt_ts$rptDate <- rptDate.offset(rpt_ts$rptDate, by = -1, freq = "y")
    rpt_ts$year_index <- rpt_ts$year_index - 1
    rpt_ts2 <- rbind(rpt_ts2, rpt_ts)
  }

  funchar <- '"Sales_PS", reportofall(9900011,Rdate)'
  mdata <- rptTS.getFin_ts(rpt_ts2, funchar)
  
  mdata <- data.table::as.data.table(mdata)
  data.table::setorder(mdata, date, stockID, rptDate)
  mdata_slope <- mdata[,.('slope' = ifelse(sum(is.na(Sales_PS)) > 2, as.numeric(NA), lm(data = .SD, Sales_PS ~ year_index)$coefficients[2])), by = .(date, stockID)]
  # the data will be dropped to NA if missing over 50%  
  mdata_denomi <- mdata[,.('denomi' = mean(Sales_PS)), by = .(date, stockID)]
  mdata_result <- mdata_slope[mdata_denomi, on = .(date, stockID)]
  mdata_result[,'factorscore' := slope / denomi,]
  
  result <- mdata_result[,c("date","stockID","factorscore")]
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}
#' @export
gr.egro <- function(TS){
  
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  # sales growth, trailing five years
  rpt_ts <- getrptDate_newest(ts, freq = "y")
  rpt_ts$year_index <- 5
  
  rpt_ts2 <- rpt_ts
  for(i in 1:4){
    rpt_ts$rptDate <- rptDate.offset(rpt_ts$rptDate, by = -1, freq = "y")
    rpt_ts$year_index <- rpt_ts$year_index - 1
    rpt_ts2 <- rbind(rpt_ts2, rpt_ts)
  }
  
  funchar <- '"Revenue_PS", reportofall(9900000,Rdate)'
  mdata <- rptTS.getFin_ts(rpt_ts2, funchar)
  
  mdata <- data.table::as.data.table(mdata)
  data.table::setorder(mdata, date, stockID, rptDate)
  mdata_slope <- mdata[,.('slope' = ifelse(sum(is.na(Revenue_PS)) > 2,as.numeric(NA),lm(data = .SD, Revenue_PS ~ year_index)$coefficients[2])), by = .(date, stockID)]
  # the data will be dropped to NA if missing over 50%  
  mdata_denomi <- mdata[,.('denomi' = mean(Revenue_PS, na.rm = TRUE)), by = .(date, stockID)]
  mdata_result <- mdata_slope[mdata_denomi, on = .(date, stockID)]
  mdata_result[,'factorscore' := slope / denomi,]
  
  result <- mdata_result[,c("date","stockID","factorscore")]
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}
#' @export
gr.egibs <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.F_NP_G2year(TS)
  return(mdata)
}
#' @export
gr.egibs_s <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.F_NP_YOY(TS)
  return(mdata)
}

#### BP ####
#' @export
gr.btop <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.BP_mrq(TS)
  return(mdata)
}

#### LEVERAGE ####
#' @export
gr.mlev <- function(TS){
  #
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  # ME AND LD
  rpt_ts <- getrptDate_newest(ts, freq = "q")
  funchar <- '"LD", report(44094,Rdate)'
  mdata <- rptTS.getFin_ts(rpt_ts, funchar)
  mdata <- gf_cap(mdata, var = "mkt_cap", varname = "mkt_cap")
  # for financial industry, LD = 0
  mdata <- data.table::as.data.table(mdata)
  mdata[,'factorscore' :=  (LD/(1e+8) + mkt_cap)/mkt_cap]
  
  result <- mdata[,c("date","stockID","factorscore")]
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
  
}
#' @export
gr.dtoa <- function(TS){
  #
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  rpt_ts <- getrptDate_newest(ts, freq = "q")
  funchar <- '"TD", report(44097,Rdate), "TA", report(44059,Rdate) '
  mdata <- rptTS.getFin_ts(rpt_ts, funchar)
  mdata <- data.table::as.data.table(mdata)
  mdata[,'factorscore' :=  TD/TA]
  
  result <- mdata[,c("date","stockID","factorscore")]
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
  
}
#' @export
gr.blev <- function(TS){
  #
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  rpt_ts <- getrptDate_newest(ts, freq = "q")
  funchar <- '"BE", reportofall(9901106,Rdate), "LD", report(44094,Rdate)'
  mdata <- rptTS.getFin_ts(rpt_ts, funchar)
  
  mdata <- data.table::as.data.table(mdata)
  mdata[,'factorscore' :=  (BE+LD)/BE]

  result <- mdata[,c("date","stockID","factorscore")]
  result <- as.data.frame(result)
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}

#### LIQUIDITY ####
#' @export
gr.stom <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.liquidity(TS, wgt = c(1,0,0))
  return(mdata)
}
#' @export
gr.stoq <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.liquidity(TS, wgt = c(0,1,0))
  return(mdata)
}
#' @export
gr.stoa <- function(TS){
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  mdata <- gf.liquidity(TS, wgt = c(0,0,1))
  return(mdata)
}

#### NLSIZE ####
#' @export
gr.nlsize <- function(TS){
  
  #
  check.TS(TS)
  ts <- TS[,c("date","stockID")]
  if('factorscore' %in% colnames(TS)){
    TS$factorscore <- NULL
    warnings("factorscore column exists and is overwriten.")
  }
  #
  tsf <- gf_cap(ts, var = "mkt_cap", log = TRUE)
  tsf$factorscore[is.infinite(tsf$factorscore)] <- NA
  # TO DO ...
  # 20070702 TO 20070731 MKTCAP INFINITE
  
  # rfPars <- list(outlier=list(method = "sd",
  #                             par = 3,
  #                             sectorAttr = NULL),
  #                std=list(method = "scale_barra_simple",
  #                         log=FALSE, 
  #                         sectorAttr=NULL,
  #                         regLists=NULL),
  #                na=list(method = "median", 
  #                        sectorAttr=defaultSectorAttr("ind"))
  #                )
  # tsf <- factor_refine(tsf, refinePar = rfPars)
  #
  tsf <- tsf[,c("date","stockID","factorscore")]
  colnames(tsf) <- c("date","stockID","mkt_cap")
  tsf$mkt_cap_cube <- tsf$mkt_cap ^ 3
  #
  # is.infinite(tsf) <- NA 
  result <- factor_orthogon(tsf, method = "reg", sectorAttr = NULL, regType = "lm")
  result <- result[,c("date","stockID","mkt_cap_cube")]
  colnames(result) <- c("date","stockID","factorscore")
  final_result <- merge.x(TS, result, by = c("date","stockID"))
  
  return(final_result)
}
