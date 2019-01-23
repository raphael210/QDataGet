
#' barra_fls_default
#' 
#' default barra factor lists
#' @export
barra_fls_default <- function(){
  
  #
  sectorAttr <- defaultSectorAttr("ind")
  #
  rfPars_scale <- list(outlier=list(method = "sd",
                                    par = 3,
                                    sectorAttr = NULL),
                       std=list(method = "scale",
                                log = FALSE,
                                sectorAttr = NULL,
                                regLists = NULL,
                                regWgt = NULL),
                       na=list(method = "median", 
                               sectorAttr=NULL)
                       )
  #
  rfPars_barra_sec <- list(outlier=list(method = "boxplot",
                                        par = 3,
                                        sectorAttr = sectorAttr),
                           std=list(method = "scale_barra_sec",
                                    log = FALSE,
                                    sectorAttr = sectorAttr,
                                    regLists = NULL,
                                    regWgt = NULL),
                           na=list(method = "median", 
                                   sectorAttr= sectorAttr))
  #
  rfPars_barra_simple <- list(outlier=list(method = "boxplot",
                                           par = 3,
                                           sectorAttr = sectorAttr),
                              std=list(method = "scale_barra_simple",
                                       log = FALSE,
                                       sectorAttr = NULL,
                                       regLists = NULL,
                                       regWgt = NULL),
                              na=list(method = "median", 
                                      sectorAttr= sectorAttr))
  #
  rfPars_none <- refinePar_default("none")
  #
  barra_fls <- buildFactorLists(
  buildFactorList_combi(factorLists = buildFactorLists_lcfs('R000001', factorRefine = rfPars_barra_simple), 
                        wgts = 1, factorRefine = rfPars_barra_simple, factorName = "SIZE"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs('R000002', factorRefine = rfPars_barra_sec), 
                        wgts = 1, factorRefine = rfPars_barra_simple, factorName = "BP"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs('R000003', factorRefine = rfPars_barra_simple), 
                        wgts = 1, factorRefine = rfPars_barra_simple, factorName = "MOMENTUM"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000004','R000005','R000006'), factorRefine = rfPars_barra_sec), 
                        wgts = c(0.68, 0.11, 0.21), factorRefine = rfPars_barra_simple, factorName = "EARNYIELD"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000007','R000008','R000009','R000010'), factorRefine = rfPars_barra_sec), 
                        wgts = c(0.47, 0.24, 0.18, 0.11), factorRefine = rfPars_barra_simple, factorName = "GROWTH"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000011','R000012','R000013'), factorRefine = rfPars_barra_sec), 
                        wgts = c(0.38, 0.35, 0.27), factorRefine = rfPars_barra_simple, factorName = "LEVERAGE"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000014','R000015','R000016'), factorRefine = rfPars_barra_simple), 
                        wgts = c(0.35, 0.35, 0.30), factorRefine = rfPars_barra_simple, factorName = "LIQUIDITY"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000017'), factorRefine = rfPars_barra_simple), 
                        wgts = 1, factorRefine = rfPars_barra_simple, factorName = "NLSIZE"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000018'), factorRefine = rfPars_barra_simple), 
                        wgts = 1, factorRefine = rfPars_barra_simple, factorName = "BETA"),
  buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000019','R000020','R000021'), factorRefine = rfPars_barra_simple), 
                        wgts = c(0.74, 0.16, 0.1), factorRefine = rfPars_barra_simple, factorName = "VOLATILITY")
  )
  return(barra_fls)
}

#' BUILD BARRA BASIC TABLES
#'
#' @description build QT_FactorReturn, QT_Res
#' @export
lcdb.build.barra_basic <- function(){
  
  # INDIVIDUAL'S SETTING
  # factorID <- barra_factorID_default()
  #
  con <- db.local("fs_r")
  if(DBI::dbExistsTable(con,"QT_FactorReturn")){DBI::dbRemoveTable(con,"QT_FactorReturn")}
  if(DBI::dbExistsTable(con,"QT_Res")){DBI::dbRemoveTable(con,"QT_Res")}
  DBI::dbDisconnect(con)
  
  # TIME WINDOW
  lcdb_min_date <- queryAndClose.dbi(db.local("fs_r"), "select min(TradingDay) from QT_FactorScore_R") [[1]] # to do ...
  begT <- intdate2r(lcdb_min_date)
  endT <- trday.nearby(begT, by = 10L)
  rebDates <- getRebDates(begT, endT, rebFreq = "day")
  
  # PARAM
  barra_param <- barra_param_default()
  barra_fls <- barra_fls_default()
  
  ###
  ts_union <- getIndexComp(indexID = "EI000985", endT = rebDates) 
  
  ts_union$ipo_date <- trday.IPO(ts_union$stockID)
  ts_union$ipo_date2 <- trday.nearby(ts_union$ipo_date, by = 120L) # at least half year required
  ts_union <- ts_union[ts_union$ipo_date2 < ts_union$date, ] 
  ts_union$ipo_date <- NULL
  ts_union$ipo_date2 <- NULL
  
  #
  reg_list <- get_frtn_res_series(ts_union = ts_union, barra_fls = barra_fls, 
                                  barra_param = barra_param)

  # prepare data writing
  reg_list$frtn_seri$date <- rdate2int(reg_list$frtn_seri$date)
  reg_list$frtn_seri$date_end <- rdate2int(reg_list$frtn_seri$date_end)
  reg_list$res_seri$date <- rdate2int(reg_list$res_seri$date)
  reg_list$res_seri$date_end <- rdate2int(reg_list$res_seri$date_end)
  
  con <- db.local("fs_r")
  #
  DBI::dbWriteTable(conn = con, name = "QT_FactorReturn", value = reg_list$frtn_seri, overwrite = TRUE, append = FALSE, row.names=FALSE)
  DBI::dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_FactorReturn] ON [QT_FactorReturn] ([date]);')
  #
  DBI::dbWriteTable(conn = con, name = "QT_Res", value = reg_list$res_seri, overwrite = TRUE, append = FALSE, row.names=FALSE)
  DBI::dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_Res] ON [QT_Res] ([date], [stockID]);')
  DBI::dbExecute(con,'CREATE INDEX [IX_QT_Res_ID] ON [QT_Res] ([stockID]);')
  #
  DBI::dbDisconnect(conn = con)
  #
  return("Done!")
  
}

#' UPDATE BARRA BASIC TABLES
#' @export
lcdb.update.barra_basic <- function(begT, endT){
  
  lcdb_max_date <- queryAndClose.dbi(db.local("fs_r"), "select max(date_end) from QT_FactorReturn")[[1]]
  lcdb_max_date <- intdate2r(lcdb_max_date)
  
  if(!missing(begT)){
    if(begT > lcdb_max_date) stop("The lcdb has not updated to the begT. begT should be set earlier or missing.")
  }else{
    begT <- lcdb_max_date
  }
  
  if(missing(endT)){
    endT <- queryAndClose.dbi(db.local("fs_r"), "select max(TradingDay) from QT_FactorScore_R ")[[1]]
    endT <- intdate2r(endT)
    endT <- trday.nearby(endT, by = -1) 
    # to do ...
    # what if periodrtn could not be obtained ??
  }
  
  if( endT < begT ) return("Done.")
  
  # TIME WINDOW
  rebDates <- getRebDates(begT, endT, rebFreq = "day")
  loop_times <- (length(rebDates) %/% 100) 
  if(length(rebDates) %% 100 > 0 ) loop_times <- loop_times + 1
  
  # PARAM
  barra_param <- barra_param_default()
  # sectorAttr <- defaultSectorAttr("ind")
  barra_fls <- barra_fls_default()
  
  # ROCK'N'ROLL
  for(i in 1:loop_times){
    
    if(i == loop_times){
      rebDates_ <- rebDates[(1+(i-1)*100):length(rebDates)]
    }else{
      rebDates_ <- rebDates[(1+(i-1)*100):(i*100)]
    }
    ###
    begT_ <- min(rebDates_)
    endT_ <- max(rebDates_)
    message(as.character(begT_)," to ",as.character(endT_), "... ")
    ts_union <- getIndexComp(indexID = "EI000985", endT = rebDates_) 
    
    ts_union$ipo_date <- trday.IPO(ts_union$stockID)
    ts_union$ipo_date[ts_union$ipo_date < as.Date("1990-12-19")] <- as.Date("1990-12-19")
    ts_union$ipo_date2 <- trday.nearby(ts_union$ipo_date, by = 120L) # at least half year required
    ts_union <- ts_union[ts_union$ipo_date2 < ts_union$date, ] 
    ts_union$ipo_date <- NULL
    ts_union$ipo_date2 <- NULL
    
    reg_list <- get_frtn_res_series(ts_union = ts_union, barra_fls = barra_fls, barra_param = barra_param)
    #
    reg_list$frtn_seri$date <- rdate2int(reg_list$frtn_seri$date)
    reg_list$frtn_seri$date_end <- rdate2int(reg_list$frtn_seri$date_end)
    reg_list$res_seri$date <- rdate2int(reg_list$res_seri$date)
    reg_list$res_seri$date_end <- rdate2int(reg_list$res_seri$date_end)
    
    #
    con <- db.local("fs_r")
    DBI::dbExecute(con,paste("delete from QT_FactorReturn where date >=", rdate2int(begT_),"and  date <=", rdate2int(endT_)))
    DBI::dbWriteTable(conn = con, name = "QT_FactorReturn", value = reg_list$frtn_seri, overwrite = FALSE, append = TRUE, row.names=FALSE)
    DBI::dbExecute(con,paste("delete from QT_Res where date >=", rdate2int(begT_),"and  date <=", rdate2int(endT_)))
    DBI::dbWriteTable(conn = con, name = "QT_Res", value = reg_list$res_seri, overwrite = FALSE, append = TRUE, row.names=FALSE)
    DBI::dbDisconnect(conn = con)
    
  }
  return("Done!")
}

#' BUILD BARRA ADVANCED TABLES
#'
#' @description build QT_Cov, QT_Res, QT_Bias
#' @export
lcdb.build.barra_adv <- function(){
  
  con <- db.local("fs_r")
  if(DBI::dbExistsTable(con,"QT_Bias")){DBI::dbRemoveTable(con,"QT_Bias")}
  if(DBI::dbExistsTable(con,"QT_Cov")){DBI::dbRemoveTable(con,"QT_Cov")}
  if(DBI::dbExistsTable(con,"QT_Sigma")){DBI::dbRemoveTable(con,"QT_Sigma")}
  DBI::dbDisconnect(con)
  
  barra_param <- barra_param_default()
  h_cov <- barra_param$cov_param$h_cov
  h_res <- barra_param$sigma_param$h_res
  
  qr1 <- paste("select * from QT_FactorReturn order by date limit", h_cov)
  mdata_frtn <- queryAndClose.dbi(db.local("fs_r"), qr1)
  if(nrow(mdata_frtn) < h_cov) stop("The local data is not enough to initiate barra_adv function.")
  
  qr2 <- paste("select * from QT_Res where date_end <= ", tail(mdata_frtn$date_end, 1))
  mdata_res <- queryAndClose.dbi(db.local("fs_r"), qr2)
  
  mdata_frtn$date <- intdate2r(mdata_frtn$date)
  mdata_frtn$date_end <- intdate2r(mdata_frtn$date_end)
  mdata_res$date <- intdate2r(mdata_res$date)
  mdata_res$date_end <- intdate2r(mdata_res$date_end)
  mdata_frtn <- data.table::as.data.table(mdata_frtn)
  mdata_res <- data.table::as.data.table(mdata_res)
  
  rebDate <- tail(mdata_frtn$date_end, 1)
  # for cov table
  cov_i <- get_frtn_cov(frtn_seri = mdata_frtn, rebDates = rebDate, barra_param = barra_param)
  cov_i <- fix_frtn_cov(cov_list = cov_i, barra_param = barra_param)
  bias_i <- fix_frtn_cov2(cov_list = cov_i, frtn_seri = mdata_frtn, barra_param = barra_param, 
                          result_type = "bias")
  colnames(bias_i) <- c("date","bias_cov")
  # for sigma table
  mdata_data_bank <- load_data_bank(mdata_res)
  sigma_i <- get_frtn_sigma(res_seri = mdata_res, rebDates = rebDate, barra_param = barra_param, data_bank = mdata_data_bank)
  sigma_i <- fix_frtn_sigma(sigma_i, barra_param = barra_param)
  bias_2_i <- fix_frtn_sigma2(sigma_i = sigma_i, res_seri = mdata_res, barra_param = barra_param, data_bank = mdata_data_bank,
                              result_type = "bias")
  colnames(bias_2_i) <- c("date","bias_sigma")
  # bias table
  bias_dt <- bias_i[bias_2_i, on = .(date)]
  # prepare for data saving
  bias_dt$date <- rdate2int(bias_dt$date)
  sigma_i$date <- rdate2int(sigma_i$date)
  cov_dt <- cbind(as.Date(names(cov_i)[1]), data.table::as.data.table(cov_i[[1]]))
  colnames(cov_dt)[1] <- 'date'
  cov_dt$date <- rdate2int(cov_dt$date)

  # write into lcdb
  con <- db.local("fs_r")
  DBI::dbWriteTable(conn = con, name = "QT_Bias", value = bias_dt, overwrite = TRUE, append = FALSE, row.names=FALSE)
  DBI::dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_Bias] ON [QT_Bias] ([date]);')
  DBI::dbWriteTable(conn = con, name = "QT_Cov", value = cov_dt, overwrite = TRUE, append = FALSE, row.names=FALSE)
  DBI::dbExecute(con,'CREATE INDEX [IX_QT_Cov] ON [QT_Cov] ([date]);')
  DBI::dbWriteTable(conn = con, name = "QT_Sigma", value = sigma_i, overwrite = TRUE, append = FALSE, row.names=FALSE)
  DBI::dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_Sigma] ON [QT_Sigma] ([date], [stockID]);')
  DBI::dbExecute(con,'CREATE INDEX [IX_QT_Sigma_ID] ON [QT_Sigma] ([stockID]);')
  DBI::dbDisconnect(conn = con)
  #
  return("Done!")  
}

#' UPDATE BARRA ADVANCED TABLES
#' @export
lcdb.update.barra_adv <- function(begT, endT){
  
  lcdb_max_date <- queryAndClose.dbi(db.local("fs_r"), "select max(date) from QT_bias")[[1]]
  lcdb_max_date <- intdate2r(lcdb_max_date)
  lcdb_max_date <- trday.nearby(lcdb_max_date, 1)
  
  lcdb_limit_date <- queryAndClose.dbi(db.local("fs_r"), "select max(date_end) from QT_FactorReturn")[[1]]
  lcdb_limit_date <- intdate2r(lcdb_limit_date)
  
  if(!missing(begT)){
    if(begT > lcdb_max_date) stop("The begT is later the lcdb max date. Eariler begT is required.")
    lcdb_max_date <- begT
  }

  if(!missing(endT)){
    if(endT > lcdb_limit_date) stop("The endT is later the lcdb limit date. Eariler endT is required.")
    lcdb_limit_date <- endT
  }
  
  if(lcdb_max_date > lcdb_limit_date) return("Done.")
  
  rebDates <- getRebDates(lcdb_max_date, lcdb_limit_date, rebFreq = "day")

  # rebDates <- rebDates[-1]
  loop_times <- length(rebDates) %/% 500
  if(length(rebDates) %% 500 > 0) loop_times <- loop_times + 1
  
  barra_param <- barra_param_default()
  h_cov <- barra_param$cov_param$h_cov
  h_res <- barra_param$sigma_param$h_res
  
  ### ROCK'N'ROLL
  for( i in 1:loop_times){
    
    if(i == loop_times){
      rebDates_ <- rebDates[(1+(i-1)*500):length(rebDates)]
    }else{
      rebDates_ <- rebDates[(1+(i-1)*500):(i*500)]
    }
    
    begT_ <- min(rebDates_)
    endT_ <- max(rebDates_)
    message(as.character(begT_)," to ",as.character(endT_), "... ")
    
    begT0_ <- trday.nearby(begT_, by = -(h_cov-1))
    qr1 <- paste("select * from QT_FactorReturn where date_end >=",rdate2int(begT0_)," and date_end <=", rdate2int(endT_))
    mdata_frtn <- queryAndClose.dbi(db.local("fs_r"), qr1)
    
    begT02_ <- trday.nearby(begT_, by = -(h_res-1))
    qr2 <- paste("select * from QT_Res where date_end >=",rdate2int(begT02_)," and date_end <=", rdate2int(endT_))
    mdata_res <- queryAndClose.dbi(db.local("fs_r"), qr2)
    
    mdata_frtn$date <- intdate2r(mdata_frtn$date)
    mdata_frtn$date_end <- intdate2r(mdata_frtn$date_end)
    mdata_res$date <- intdate2r(mdata_res$date)
    mdata_res$date_end <- intdate2r(mdata_res$date_end)

    mdata_frtn <- data.table::as.data.table(mdata_frtn)
    mdata_res <- data.table::as.data.table(mdata_res)
    
    # for cov table
    cov_i <- get_frtn_cov(frtn_seri = mdata_frtn, rebDates = rebDates_, barra_param = barra_param)
    cov_i <- fix_frtn_cov(cov_list = cov_i, barra_param = barra_param)
    bias_i <- fix_frtn_cov2(cov_list = cov_i, frtn_seri = mdata_frtn, barra_param = barra_param, result_type = "bias")
    colnames(bias_i) <- c("date","bias_cov")
    # for sigma table
    mdata_data_bank <- load_data_bank(mdata_res) # this step makes cutting loop useless # memory not enough ?
    sigma_i <- get_frtn_sigma(res_seri = mdata_res, rebDates = rebDates_, barra_param = barra_param, data_bank = mdata_data_bank)
    sigma_i <- fix_frtn_sigma(sigma_i, barra_param = barra_param)
    bias_2_i <- fix_frtn_sigma2(sigma_i = sigma_i, res_seri = mdata_res, barra_param = barra_param, data_bank = mdata_data_bank,
                                result_type = "bias")
    colnames(bias_2_i) <- c("date","bias_sigma")
    #
    bias_dt <- bias_i[bias_2_i, on = .(date)]

    # prepare for data saving
    bias_dt$date <- rdate2int(bias_dt$date)
    sigma_i$date <- rdate2int(sigma_i$date)
    cov_dt <- data.table::data.table()
    for( i in 1:length(cov_i)){
      cov_dt_ <- cbind(as.Date(names(cov_i)[i]), data.table::as.data.table(cov_i[[i]]))
      colnames(cov_dt_)[1] <- 'date'
      cov_dt_$date <- rdate2int(cov_dt_$date)
      cov_dt <- rbind(cov_dt, cov_dt_)
    }
    
    # write
    con <- db.local("fs_r")
    DBI::dbExecute(con,paste("delete from QT_Bias where date >=", rdate2int(begT_)))
    DBI::dbWriteTable(conn = con, name = "QT_Bias", value = bias_dt, overwrite = FALSE, append = TRUE, row.names=FALSE)
    DBI::dbExecute(con,paste("delete from QT_Cov where date >=", rdate2int(begT_)))
    DBI::dbWriteTable(conn = con, name = "QT_Cov", value = cov_dt, overwrite = FALSE, append = TRUE, row.names=FALSE)
    DBI::dbExecute(con,paste("delete from QT_Sigma where date >=", rdate2int(begT_)))
    DBI::dbWriteTable(conn = con, name = "QT_Sigma", value = sigma_i, overwrite = FALSE, append = TRUE, row.names=FALSE)
    DBI::dbDisconnect(conn = con)
  
  }
  
  return("Done!")
}


#' TS.get_barra
#' @export
TS.get_barra <- function(TS){
  
  check.TS(TS)
  date_list_ts <- sort(unique(TS$date))
  
  barra_param <- barra_param_default()
  h_vra_cov <- barra_param$cov_param$h_vra
  h_vra_res <- barra_param$sigma_param$h_vra
  h_vra <- max(h_vra_cov, h_vra_res)
  
  begT0 <- trday.nearby(min(date_list_ts), -(h_vra-1))
  endT <- max(date_list_ts)
  
  earliest_lcdb_date <- queryAndClose.dbi(db.local("fs_r"), "select min(date) from QT_Bias")[[1]]
  earliest_lcdb_date <- intdate2r(earliest_lcdb_date)
  if(begT0 < earliest_lcdb_date){
    earliest_ts_date <- as.character(trday.nearby(earliest_lcdb_date, by = (h_vra-1)))
    stop(paste("TS too early, not enough data for performing barra, TS should only have date >=",earliest_ts_date))
  }
  
  qr_1 <- paste("select * from QT_Bias where date >= ",rdate2int(begT0)," and date <=",rdate2int(endT))
  mdata_bias <- queryAndClose.dbi(db.local("fs_r"), qr_1)
  mdata_bias$date <- intdate2r(mdata_bias$date)
  
  # roll sum bias
  wgt_cov <- (0.5^(1/barra_param$cov_param$tau_vra)) ^ ((h_vra_cov-1):0)
  wgt_cov <- wgt_cov/sum(wgt_cov) 
  mdata_bias$lambda_cov <- RcppRoll::roll_sumr(mdata_bias$bias_cov, weights = wgt_cov, normalize = FALSE)
  
  wgt_sigma <- (0.5^(1/barra_param$sigma_param$tau_vra)) ^ ((h_vra_res-1):0)
  wgt_sigma <- wgt_sigma/sum(wgt_sigma) 
  mdata_bias$lambda_sigma <- RcppRoll::roll_sumr(mdata_bias$bias_sigma, weights = wgt_sigma, normalize = FALSE)
  
  # get COV and SIGMA
  date_list_ts_char <- paste("(",paste(rdate2int(date_list_ts), collapse = ","),")")
  
  ts_tmp <- TS[,c("date","stockID")]
  ts_tmp$date <- rdate2int(ts_tmp$date)
  qr1 <- paste("select b.* from QT_Cov b where date in ", date_list_ts_char)
  qr2 <- "select a.*, b.sigma from temp_table a left join QT_Sigma b on a.date = b.date and a.stockID = b.stockID"
  con <- db.local("fs_r")
  DBI::dbWriteTable(conn = con, name = "temp_table", value = ts_tmp, overwrite = TRUE, append = FALSE)
  DBI::dbExecute(con, 'CREATE INDEX [IX_temp_table] ON [temp_table]([date],[stockID]);')
  mdata_cov <- DBI::dbGetQuery(con, statement = qr1)
  mdata_sigma <- DBI::dbGetQuery(con, statement = qr2)
  DBI::dbDisconnect(con)
  
  mdata_cov$date <- intdate2r(mdata_cov$date)
  mdata_sigma$date <- intdate2r(mdata_sigma$date)
  
  mdata_cov <- data.table::as.data.table(mdata_cov)
  mdata_sigma <- data.table::as.data.table(mdata_sigma)
  mdata_bias <- data.table::as.data.table(mdata_bias)
  
  result_cov <- data.table::data.table()
  result_sigma <- data.table::data.table()
  for( i in 1:length(date_list_ts)){
    rebDate_ <- date_list_ts[i]
    
    mdata_cov_tmp <- mdata_cov[date == rebDate_]
    lambda_cov <- mdata_bias[date == rebDate_, lambda_cov]
    mdata_cov_tmp[,2:ncol(mdata_cov_tmp)] <- lambda_cov * mdata_cov_tmp[,-1]
    
    mdata_sigma_tmp <- mdata_sigma[date == rebDate_]
    lambda_sigma <- mdata_bias[date == rebDate_, lambda_sigma]
    mdata_sigma_tmp$sigma <- lambda_sigma * mdata_sigma_tmp[,sigma]
    
    result_cov <- rbind(result_cov, mdata_cov_tmp)
    result_sigma <- rbind(result_sigma, mdata_sigma_tmp)
  }
  
  # GET mTSF
  barra_fls <- barra_fls_default()
  ts_union <- getIndexComp(indexID = "EI000985", endT = date_list_ts, drop = FALSE)
  mdata_tsf <- getMultiFactor(TS = ts_union, FactorLists = barra_fls)
  mdata_tsf <- gf_sector(mdata_tsf, sectorAttr = defaultSectorAttr("ind"))
  mdata_tsf$sector <- NULL
  mdata_tsf$c0 <- 1
  
  # ordering
  mTSF <- TS[,c("date","stockID")]
  mdata_tsf <- mdata_tsf[,c(c("date","stockID"), setdiff(colnames(result_cov), 'date'))]
  mTSF <- merge.x(mTSF, mdata_tsf, by = c("date","stockID"))
  
  # output
  result_list <- list('mTSF' = mTSF,
                      'fCov' = as.data.frame(result_cov),
                      'sigma' = as.data.frame(result_sigma))
}






