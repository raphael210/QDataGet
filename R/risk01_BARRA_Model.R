
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- FUN GET FRTN RES SERIES -----------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' PARAMETER DEFAULT FUNCTIONS
#' 
#' @param subset_reg NULL or sectorIDs vector, supporting more than one sectorID. 
#' If not NULL, regression would only be done within the sectorIDs, but the residuals would cover all the stocks.
#' @param h training windows.
#' @param tau half-life parameter.
#' @param lag lags for Newey-west adjustment.
#' @param shrink_q parameters for Beyasian shrink.
#' @param fix_itr_n parameters for Monte Carlo.
#' @export
barra_param_default <- function(){
  result <- list('reg_param' = list('subset_reg' = NULL,
                                    'sectorAttr' = defaultSectorAttr("ind")),
                 'cov_param' = list('h_cov' = 750,
                                    'tau_c' = 504, 'tau_v' = 84, 
                                    'lag_c' = 2, 'lag_v' = 5,
                                    'fix_itr_n' = 300,
                                    'h_vra' = 150, 'tau_vra' = 42),
                 'sigma_param' = list('h_res' = 360,
                                      'tau_1' = 84, 'tau_2' = 252, 'lag' = 5,
                                      'shrink_q' = 0.25, 'decile_n' = 10,
                                      'h_vra' = 150, 'tau_vra' = 42))
  return(result)
}

#' THE REGRESSION FUNCTION
#' 

#' @param ts_union TS object. Recommanded use.
#' @param barra_fls The factorLists object to get regression X variables. 
#' @param barra_param A list, containing all the parameters in barra risk model. 
#' subset_reg flag is contained in the parameter list, default NULL.
#' The subset_reg could be a character vector containing many sectorIDs. Regression is done within this subset.
#' @param ts TS object. Carefully use. When ts is passed into the function, actually only the rebalance date series would be retrieved. 
#' A complete and longer period TS object would be generated inside the function to execute regression.
#' @description Barra regression function. Common usage is showed in examples.
#' @return A list, including four values, frtn_seri, res_seri, omega_seri, data_bank(mtsfr in fact).
#' omega_seri is the pure factor port, each column is the weight vector of one single pure factor.
#' @export
#' @examples 
#' fls <- barra_fls_default()
#' ts_union <- getIndexComp("EI000985", endT = as.Date("2018-01-31"), drop = FALSE)
#' get_frtn_res_series(ts_union = ts_union, barra_fls = fls, barra_param = barra_param_default())
get_frtn_res_series <- function(ts_union, barra_fls, barra_param, ts){
  # args : get maximum training window
  h_vra <- barra_param$cov_param$h_vra
  h_cov <- barra_param$cov_param$h_cov
  
  if(!missing(ts_union)){
    check.colnames(ts_union, c("date","stockID"))
  }else if(missing(ts_union) & !missing(ts)){
    # get ts_union, expand ts
    core_ts <- ts[,c("date","stockID")]
    core_datelist <- sort(unique(core_ts$date))
    
    for(i in 1:length(core_datelist)){
      date_ <- core_datelist[i]
      stock_ <- subset(core_ts, date == date_, select = "stockID", drop = TRUE)
      rebdate_ <- getRebDates(begT = trday.nearby(date_, by = -(h_vra+h_cov+3)), endT = date_, rebFreq = "day")
      ts_ <- expand.grid('date' = rebdate_, 'stockID' = stock_, stringsAsFactors = FALSE)
      if(i == 1L){
        ts_union <- ts_
      }else{
        ts_union <- rbind(ts_union, ts_)
      }
    }
    ts_union <- data.table::as.data.table(ts_union)
    ts_union <- unique(ts_union)
    ts_union <- data.table::setorder(ts_union, date, stockID)
    ts_union <- as.data.frame(ts_union) # to do ...
    ts_union$ipo_date <- trday.IPO(ts_union$stockID)
    ts_union$ipo_date[ts_union$ipo_date < as.Date("1990-12-19")] <- as.Date("1990-12-19")
    ts_union$ipo_date2 <- trday.nearby(ts_union$ipo_date, by = 120L) # at least half year required
    ts_union <- ts_union[ts_union$ipo_date2 < ts_union$date, ]  
    ts_union$ipo_date <- NULL
    ts_union$ipo_date2 <- NULL
  }else{
    stop("ts input error")
  }
  
  # subset reg flag
  subset_reg <- barra_param$reg_param$subset_reg
  if(!is.null(subset_reg)){
    ts_union$subset_reg_flag <- 0
    for(i in 1:length(subset_reg)){
      ts_union <- is_component(ts_union, sectorID = subset_reg[i])
      ts_union$subset_reg_flag <- ts_union$is_comp + ts_union$subset_reg_flag
      ts_union$is_comp <- NULL
    }
    ts_union$subset_reg_flag <- (ts_union$subset_reg_flag > 0) + 0 # ensure only 1 or 0
  }
  
  # get barra_fls
  mtsf <- getMultiFactor(ts_union, FactorLists = barra_fls, silence = TRUE)
  fnames <- sort(guess_factorNames(mtsf, silence = TRUE))
  fnames <- setdiff(fnames, c("subset_reg_flag"))
  
  # get sector ID
  sectorAttr <- barra_param$reg_param$sectorAttr
  mtsf <- gf_sector(mtsf, sectorAttr = sectorAttr)
  # get wgt
  mtsf <- gf_cap(mtsf, var = 'free_cap', varname = 'ffv')
  mtsf$sqrt_ffv <- sqrt(mtsf$ffv)
  # get r
  
  mdata_rtn <- getQuote(stocks = unique(mtsf$stockID), begT = min(mtsf$date), endT = trday.nearby(max(mtsf$date), 1), 
                        'pct_chg',tableName = 'QT_DailyQuote',datasrc = 'quant',split = FALSE)
  colnames(mdata_rtn) <- c("stockID","date_end","pct_chg")
  mdata_rtn$date <- trday.nearby(mdata_rtn$date_end, by = -1)
  mdata_rtn <- data.table::as.data.table(mdata_rtn)
  
  mtsf <- data.table::as.data.table(mtsf)
  mtsfr <- mdata_rtn[mtsf, on = .(date, stockID)]
  mtsfr[is.na(pct_chg), pct_chg := 0]
  mtsfr[pct_chg > 0.11,  pct_chg := 0]
  mtsfr[pct_chg < -0.11,  pct_chg := 0]
  mtsfr[is.na(date_end), date_end := trday.nearby(date, by = 1)]
  
  ### rock'n'roll
  datelist <- sort(unique(mtsfr$date))
  for( i in 1:length(datelist)){
    
    date_i <- datelist[i]
    mtsfr_i <- mtsfr[date == date_i]
    date_end_i <- unique(mtsfr_i$date_end)
    if(length(date_end_i) > 1) stop("code error 7190811745")
    reg_result <- reg_mazi(mtsfr_i = mtsfr_i, y = "pct_chg", fnames = fnames, subset_reg = subset_reg)
    frtn_dt <- cbind('date' = date_i, 'date_end' = date_end_i, reg_result$frtn)
    res_dt <- cbind('date' = date_i, 'date_end' = date_end_i, reg_result$res)
    omega_dt <- cbind('date' = date_i, 'date_end' = date_end_i, reg_result$omega)
    
    if(i == 1L){
      result_frtn <- frtn_dt
      result_res <- res_dt
      result_omega <- omega_dt
    }else{
      result_frtn <- rbind(result_frtn, frtn_dt)
      result_res <- rbind(result_res, res_dt)
      result_omega <- rbind(result_omega, omega_dt)
    }
    
  }
  # to do ...
  # industry not match along time ?
  result_list <- list("frtn_seri" = result_frtn,
                      "res_seri" = result_res,
                      "omega_seri" = result_omega,
                      "data_bank" = mtsfr)
  return(result_list)
}


#' regressing function for barra
#'
#' @description the regression satisfies the specific one constriant optimization. mtsfr_i SHOULD WENT THROUGH  GF_CAP, GF_SECTOR before input.
#' @param y string name for y.
#' @param fnames string vector for factor names.
#' @param subset_reg If not null, the column in mtsfr_i should include 'subset_reg_flag' to indicate the subset to regress.
#' @return A list, including three values, frtn, res, est_rtn(estimated values).
reg_mazi <- function(mtsfr_i, y, fnames, subset_reg = NULL){
  
  # mtsfr_i SHOULD BE DATA TABLE
  # mtsfr_i SHOULD WENT THROUGH  GF_CAP, GF_SECTOR
  if(!is.null(subset_reg)){
    check.colnames(data = mtsfr_i, coltest = c("stockID","ffv","sector", "subset_reg_flag", fnames, y))
    mtsfr_train_i <- mtsfr_i[subset_reg_flag == 1]
  }else{
    check.colnames(data = mtsfr_i, coltest = c("stockID","ffv","sector", fnames, y))
    mtsfr_train_i <- mtsfr_i
  }
  
  ind_stat <- mtsfr_train_i[,bmk_wgt := ffv/sum(ffv, na.rm = TRUE)]
  ind_stat <- ind_stat[,.(bmk_sec_wgt = sum(bmk_wgt, na.rm = TRUE)),by = "sector"]
  data.table::setorder(ind_stat, sector)
  
  sec_n <- nrow(ind_stat)
  sec_names <- ind_stat$sector
  
  # mat computations
  # R1
  sec_diag_mat <- diag(rep(1, sec_n - 1))
  w_vec <- ind_stat$bmk_sec_wgt
  last_row <- -w_vec[-length(w_vec)]/w_vec[length(w_vec)]
  sec_diag_mat <- rbind(sec_diag_mat, last_row)
  # R2
  fac_diag_mat <- diag(rep(1, length(fnames))) 
  # R3
  R_mat <- Matrix::bdiag(list(1,sec_diag_mat,fac_diag_mat))
  R <- as.matrix(R_mat)
  
  # X exposure
  # sec_expo
  x_mat_sec <- mtsfr_train_i[,sec_names, with = FALSE]
  # f_expo
  x_mat_fac <- mtsfr_train_i[,fnames, with = FALSE]
  x_mat <- cbind(1, x_mat_sec, x_mat_fac)
  X <- as.matrix(x_mat)
  
  # V wgt
  V <- diag(sqrt(mtsfr_train_i$ffv))
  
  # get omega
  comp_mat <- t(R) %*% t(X) %*% V %*% X %*% R
  comp_mat_inv <- solve(comp_mat) 
  omega <- R %*% comp_mat_inv %*% t(R) %*% t(X) %*% V
  omega_dt <- data.table::as.data.table(t(omega))
  omega_dt <- cbind('stockID' = mtsfr_train_i$stockID, omega_dt)
  colnames(omega_dt) <- c("stockID","c0",sec_names, fnames)
  
  # frtn
  frtn <- omega %*% (mtsfr_train_i[,get(y),])
  frtn_dt <- data.table::as.data.table(t(frtn))
  colnames(frtn_dt) <- c("c0",sec_names, fnames)
  
  ##### RES
  # X exposure
  # sec_expo
  x_mat_sec_2 <- mtsfr_i[,sec_names, with = FALSE]
  # f_expo
  x_mat_fac_2 <- mtsfr_i[,fnames, with = FALSE]
  x_mat_2 <- cbind(1, x_mat_sec_2, x_mat_fac_2)
  X_2 <- as.matrix(x_mat_2)
  est_rtn <- X_2 %*% frtn
  #
  est_rtn_dt <- cbind(mtsfr_i$stockID, data.table::data.table(est_rtn), stringsAsFactors = FALSE)
  colnames(est_rtn_dt) <- c("stockID", "est_rtn")
  #
  res <- data.table::data.table('res' = (as.vector(mtsfr_i[,get(y),]) - as.vector(est_rtn)))
  res_dt <- cbind(mtsfr_i$stockID, res, stringsAsFactors = FALSE)
  colnames(res_dt) <- c("stockID", "res")
  # output
  result_list <- list("frtn" = frtn_dt,
                      "res" = res_dt,
                      "est_rtn"  = est_rtn_dt,
                      "omega" = omega_dt)
  return(result_list)
}






# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- FUN GET FRTN COV -----------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' COV COMPUTE FUNCTION
#' 
#' @param frtn_seri data table input. The factor return data table. 
#' The first column is date and each of the rest columns is a risk factor return series. The data is in day frequency.
#' @param rebDates vector input. The rebDates vector is the dates in which covariance matrix would be obtained.
#' @return  a list. Each element in the list is a covariance matrix. 
#' The name of each element is the rebDates. 
#' The matrix is computed base on the data no later than rebDate, and represents the ONE YEAR forward covariance matrix from that rebDate on.
#' @export
get_frtn_cov <- function(frtn_seri, rebDates, barra_param){
  
  # frtn_seri <- data.table::as.data.table(frtn_seri)
  frtn_seri <- data.table::setorder(frtn_seri, date_end)
  result_list <- list()
  for( i in 1:length(rebDates)){
    rebdate_i <- rebDates[i]
    frtn_seri_sub <- tail(frtn_seri[date_end <= rebdate_i], barra_param$cov_param$h_cov)
    F_mat <- as.matrix(frtn_seri_sub[,-c('date','date_end')])
    # cov day to cov year
    cov_i <- 252 * get_frtn_cov_single(F_mat = F_mat, barra_param)
    result_list[[i]] <- cov_i 
    names(result_list)[i] <- as.character(rebdate_i)
  }
  return(result_list)
}

#' COV COMPUTE FUNCTION FOR SINGLE PERIOD
#' 
#' @description  The function only works for computing one single covariance matrix.
#' @param F_mat a matrix. factor return matrix. The matrix must not include date column, each column in F_mat is a factor return series.
#' @return a matrix. The ONE DAY forward covariance matrix on the specific rebDate.
get_frtn_cov_single <- function(F_mat, barra_param){
  # method : unbiased ?
  cov_c <- cov_mazi(f_mat = F_mat, tau = barra_param$cov_param$tau_c, lag = barra_param$cov_param$lag_c)
  cov_v <- cov_mazi(f_mat = F_mat, tau = barra_param$cov_param$tau_v, lag = barra_param$cov_param$lag_v)
  
  # merging diag
  cov_i <- cov_c
  # cov reduce to corr
  vec_old <- sqrt(diag(cov_c))
  mat_old <- (vec_old) %*% t(vec_old)
  cov_i <- cov_i / mat_old
  # corr back to cov
  vec_new <- sqrt(diag(cov_v))
  mat_new <- (vec_new) %*% t(vec_new)
  cov_i <- cov_i * mat_new
  # output
  return(cov_i)
}

#' Inner function for computing covariance
#' 
#' @param f_mat a matrix. factor return matrix. The matrix must not include date column, each column in f_mat is a factor return series.
#' @param tau integer number. The half-life parameter.
#' @param lag integer number. The Newey-west parameter.
#' @description  This function is a inner function to compute the Newey-west adjusted covariance matrix.
#' 
cov_mazi <- function(f_mat, tau, lag){
  
  lambda <- 0.5^(1/tau)
  h <- nrow(f_mat)
  wgt <- lambda ^ ((h-1):0)
  wgt <- wgt/sum(wgt)
  
  f_mean <- wgt %*% f_mat
  f_mat_scaled <- t(t(f_mat) - as.vector(f_mean))
  
  f_mat_scaled_0 <- f_mat_scaled * as.vector(sqrt(wgt))
  C_m <- t(f_mat_scaled_0) %*% f_mat_scaled_0
  
  if(lag > 0){
    for(ii in 1:lag){
      wgt_new <- wgt[-(1:ii)]
      fmat_lagged_1 <- f_mat_scaled[-(1:ii),,drop = FALSE] * as.vector(sqrt(wgt_new))
      fmat_lagged_2 <- f_mat_scaled[-((nrow(f_mat_scaled)-ii+1):nrow(f_mat_scaled)),,drop = FALSE] * as.vector(sqrt(wgt_new))
      D_plus <- t(fmat_lagged_1) %*% fmat_lagged_2
      C_m <- C_m + (lag+1-ii)*(D_plus + t(D_plus))/(lag+1)
    }
  }
  # output : no 21 adjustment
  return(C_m)
}

#' COV FIX FUNCTION FOR EIGENVALUES ADJUSTMENT.
#' 
#' @param cov_list A list. Each element in the list is a covariance matrix.
#' @return A list. Each element in the list is a covariance matrix.
#' @description The function is aimed for eigenvalues adjustment for covariance matrix.
#' @export
fix_frtn_cov <- function(cov_list, barra_param){
  h_cov <- barra_param$cov_param$h_cov
  itr_n <- barra_param$cov_param$fix_itr_n
  result_list <- list()
  for ( i in 1:length(cov_list)) {
    
    cov_ori_i <- cov_list[[i]]
    colnames_ <- colnames(cov_ori_i)
    rownames_ <- rownames(cov_ori_i)
    eigen_result <- eigen(cov_ori_i, symmetric = TRUE)
    D_ori <- eigen_result$values
    U_ori <- eigen_result$vectors
    record_df <- data.frame()
    #
    for(ii in 1:itr_n){
      b_ii <- data.frame()
      b_ii <- sapply(D_ori, FUN = function(x) {rnorm(n = h_cov, mean = 0, sd = sqrt(x))})
      b_ii <- as.matrix(t(b_ii))
      f_ii <- U_ori %*% b_ii # DIM(K,T)
      f_ii <- t(as.matrix(f_ii)) 
      rownames(f_ii) <- NULL
      F_ii <- cov(f_ii) 
      eigen_result_ii <- eigen(F_ii, symmetric = TRUE)
      D_est <- t(eigen_result_ii$vectors) %*% cov_ori_i %*% (eigen_result_ii$vectors)
      record_vec <- as.data.frame(t(diag(D_est)))
      record_df <- rbind(record_df, record_vec)
    }
    record_df <- as.matrix(record_df) # dim(itr_n, K), dim(T, K)
    v_ratio <- t(record_df) / as.vector(D_ori)
    v_ratio <- rowMeans(v_ratio) 
    # v_ratio : vector, length = K
    D_new <- v_ratio * D_ori # vector * vector
    cov_new_i <- (U_ori) %*% diag(D_new) %*% t(U_ori)
    colnames(cov_new_i) <- colnames_
    rownames(cov_new_i) <- rownames_
    result_list[[i]] <- cov_new_i
    names(result_list)[i] <- names(cov_list)[i]
  }
  
  return(result_list)
}

#' COV FIX FUNCTION FOR BIAS ADJUSTMENT
#' 
#' @param cov_list A list. Each element in the list is a covariance matrix.
#' @param frtn_seri data table input. The factor return data table. The first column is date and each of the rest columns is a risk factor return series. The data is in day frequency.
#' @param result_type "cov" or "bias". 
#' If "cov", the output would be the cov_list after fixation. 
#' If "bias", the output would be the bias recording for fixation.
#' @description The function is aimed for bias adjustment for covariance matrix.
#' @export
fix_frtn_cov2 <- function(cov_list, frtn_seri, barra_param, result_type = c("cov","bias")){
  
  result_type <- match.arg(result_type)
  h_cov <- barra_param$cov_param$h_cov
  h_vra <- barra_param$cov_param$h_vra
  
  frtn_seri <- data.table::setorder(frtn_seri, date_end)
  
  if(result_type == "cov"){
    
    begT <- min(as.Date(names(cov_list)))
    begT0 <- trday.nearby(begT, by = -(h_cov+h_vra-1))
    if(begT0 < min(frtn_seri$date_end)) stop("frtn_seri data history not long enough.")
    
    datelist_full <- sort(unique(frtn_seri$date_end))
    datelist_full <- datelist_full[datelist_full >= trday.nearby(begT, by = - h_vra)]
    
  }else if(result_type == "bias"){
    
    begT <- min(as.Date(names(cov_list)))
    begT0 <- trday.nearby(begT, by = -(h_cov-1))
    if(begT0 < min(frtn_seri$date_end)) stop("frtn_seri data history not long enough.")
    
    datelist_full <- sort(unique(frtn_seri$date_end))
    datelist_full <- datelist_full[datelist_full >= begT]
    
  }
  
  barra_param2 <- barra_param
  barra_param2$cov_param$lag_c <- 0
  barra_param2$cov_param$lag_v <- 0
  
  # RECORD BIAS DATA
  bias_df <- data.table::data.table()
  for( i in 1:length(datelist_full)){
    date_end_i <- datelist_full[i]
    # T0 TRAIN DATA
    frtn_seri_i <- tail(frtn_seri[date_end <= date_end_i], h_cov)
    F_mat_i <- as.matrix(frtn_seri_i[,-c('date','date_end')])
    # T0 DATA
    frtn_i <- frtn_seri[date_end == date_end_i]
    frtn_i <- frtn_i[,-c('date','date_end')]
    # removing adjustment of serial correlation, lags 0
    cov_i <- get_frtn_cov_single(F_mat_i, barra_param2)
    cov_i <- list(cov_i)
    cov_i <- fix_frtn_cov(cov_i, barra_param = barra_param2)
    cov_i <- cov_i[[1]]
    bias_i <- rowMeans((frtn_i^2) / diag(cov_i))
    bias_vec <- data.table::data.table('date_end' = date_end_i, 'bias' = bias_i)
    bias_df <- rbind(bias_df, bias_vec)
  }
  
  if(result_type == "bias") return(bias_df)
  
  ### WEIGHTING BIAS AND GET COVLIST
  lambda <- 0.5^(1/barra_param$cov_param$tau_vra)
  wgt <- lambda ^ ((h_vra-1):0)
  wgt <- wgt/sum(wgt)
  # lambda_record <- data.frame()
  for( i in 1:length(cov_list)){
    rebdate_i <- as.Date(names(cov_list)[i])
    bias_df_sub <- tail(bias_df[date_end <= rebdate_i], h_vra)
    # ensure bias_df have nrow h_vra
    vol_adj_par <- as.vector(t(wgt) %*% bias_df_sub$bias)
    cov_list[[i]] <- vol_adj_par * cov_list[[i]]
    # for observing records
    # lambda_vec <- data.frame('date' = rebdate_i, 'lambda' = vol_adj_par)
    # lambda_record <- rbind(lambda_record, lambda_vec)
  }
  
  return(cov_list)
}


  
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- FUN GET FRTN SIGMA -----------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# GET RESIDUALS


#' default structure risk factor list
#' 
#' @description This function is the default factor list setting which would be used for regression in sigma prediction.
#' @export
str_risk_fls_default <- function(){
  
  sectorAttr <- defaultSectorAttr("ind")
  rfPars_barra_simple <- list(outlier=list(method = "boxplot",
                                           par = 3,
                                           sectorAttr = sectorAttr),
                              std=list(method = "scale_barra_simple",
                                       log = FALSE,
                                       sectorAttr = NULL,
                                       regLists = NULL,
                                       regWgt = NULL),
                              na=list(method = "median", 
                                      sectorAttr= sectorAttr)
  )
  
  str_risk_fls <- buildFactorLists(
    buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000014','R000015','R000016'), factorRefine = rfPars_barra_simple), 
                          wgts = c(0.35, 0.35, 0.30), factorRefine = rfPars_barra_simple, factorName = "LIQUIDITY"),
    buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000018'), factorRefine = rfPars_barra_simple), 
                          wgts = 1, factorRefine = rfPars_barra_simple, factorName = "BETA"),
    buildFactorList_combi(factorLists = buildFactorLists_lcfs(c('R000019','R000020','R000021'), factorRefine = rfPars_barra_simple), 
                          wgts = c(0.74, 0.16, 0.1), factorRefine = rfPars_barra_simple, factorName = "VOLATILITY")
  )
  return(str_risk_fls)
}


#' DATA_BANK LOAD FUNCTION
#' 
#' @param res_seri data table or data frame. The residuals data. 
#' The columns should be 'date', 'date_end', 'stockID' and 'res'.
#' @description This function is used to prepare the data for get_frtn_sigma() and fix_frtn_sigma2().
#' It simply get factor scores from local data base for regression usage in further procedures.
#' @export
load_data_bank <- function(res_seri){
  
  str_risk_fls <- str_risk_fls_default()
  # fac
  res_seri <- as.data.frame(res_seri) # to do ...
  data_bank <- getMultiFactor(res_seri[,c("date","date_end","stockID")], 
                              FactorLists = str_risk_fls, silence = TRUE)
  # sec
  sectorAttr <- defaultSectorAttr("ind")
  data_bank <- gf_sector(data_bank, sectorAttr = sectorAttr)
  # ffv
  data_bank <- gf_cap(data_bank, var = "free_cap", varname = "ffv")
  data_bank$sqrt_ffv <- sqrt(data_bank$ffv)
  
  # output
  data_bank <- data.table::as.data.table(data_bank)
  return(data_bank)
}




#' SIGMA COMPUTE FUNCTION
#' 
#' @param res_seri data table or data frame. The residuals data. 
#' The columns should be 'date', 'date_end', 'stockID' and 'res'.
#' @param rebDates vector input. The rebDates vector is the dates in which sigma would be obtained.
#' @param data_bank data table. The data table obtained from load_data_bank(res_seri).
#' @return data table, including date, stockID, sigma. 
#' The sigma represents ONE YEAR forward sigma for each stock from that rebDate on.
#' @export
get_frtn_sigma <- function(res_seri, rebDates, barra_param, data_bank){
  
  h_res <- barra_param$sigma_param$h_res
  # res_seri <- data.table::as.data.table(res_seri)
  rebDates_0 <- trday.nearby(rebDates, by = -(h_res + 1))
  # res_seri <- setorder(res_seri, date_end, stockID)
  check.colnames(data_bank, coltest = c("sector", "sqrt_ffv", "LIQUIDITY","BETA","VOLATILITY"))
  
  for( i in 1:length(rebDates)){
    
    rebdate_i <- rebDates[i]
    rebdate_0_i <- rebDates_0[i]
    ori_set <- res_seri[date_end == rebdate_i, .(date_end, stockID)]
    colnames(ori_set) <- c("date","stockID")
    
    res_seri_sub <- res_seri[date_end <= rebdate_i,]
    res_seri_sub <- res_seri_sub[date_end >= rebdate_0_i,]
    res_seri_sub <- res_seri_sub[stockID %in% ori_set$stockID,]
    # the dcast will auto sort the first two index
    res_seri_wide_sub <- data.table::dcast(res_seri_sub, formula = date + date_end ~ stockID, value.var = "res")
    if(nrow(res_seri_wide_sub) < h_res) stop("ERRORCODE 2410180922")
    res_seri_wide_sub <- tail(res_seri_wide_sub, h_res)
    
    # sigma ts
    sigma_i <- get_frtn_sigma_single(res_seri_wide_sub = res_seri_wide_sub, 
                                     barra_param = barra_param, 
                                     data_bank = data_bank)
    # sigma_i DT format output
    sigma_i$sigma <- 252*sigma_i$sigma # turn to year
    sigma_0 <- sigma_i[ori_set, on = .(date,stockID), mult = 'all']
    
    # output
    if(i == 1L){
      result_res <- sigma_0
    }else{
      result_res <- rbind(result_res, sigma_0)
    }
  }
  return(result_res)
}

#' HEALTHY SAMPLE TEST
#' 
#' @return numeric vector. The values equal or bigger than 1 representing unhealthy sample points. 
healthy_sample_test <- function(vec){
  
  stat_vec <- quantile(vec, c(0.75, 0.25), na.rm = TRUE)
  sigma_robust <- (stat_vec[1] - stat_vec[2])/1.35
  attributes(sigma_robust) <- NULL
  vec2 <- vec[vec >= -10*sigma_robust & vec <= 10*sigma_robust]
  sigma_normal <- sd(vec2, na.rm = TRUE)
  zz = abs((sigma_normal - sigma_robust)/sigma_robust)
  if(is.na(zz)){
    zz = 1
  }
  # return zz >= 1, means not healthy sample 
  return(zz)
}

#' SIGMA COMPUTE FUNCTION FOR SINGLE PERIOD
#' 
#' @param res_seri_wide_sub data table. the residual series data table in wide format, including columns date, date_end.
#' @param data_bank data table. The data table obtained from load_data_bank(res_seri).
#' @return data table, including date, stockID, sigma. 
#' The sigma represents ONE DAY forward sigma for each stock from that rebDate on.
get_frtn_sigma_single <- function(res_seri_wide_sub, barra_param, data_bank){
  # require res_seri_wide_sub data table format
  # IF ALL NA, RETURN 0
  date_i <- tail(unique(res_seri_wide_sub$date),1)
  date_end_i <- tail(unique(res_seri_wide_sub$date_end),1)
  h_res <- nrow(res_seri_wide_sub)
  res_seri_mat <- as.matrix(res_seri_wide_sub[,-c('date','date_end')])
  
  # gamma_blend # gamma_blend 1 : use ts data # gamma_blend 0 : use str data
  ind_1 <- colSums(!is.na(res_seri_mat)) # > (h_res/2)
  ind_1 <- pmin(1,pmax(0,(ind_1 - h_res/6)/(h_res/3)))
  ind_2 <- apply(res_seri_mat, MARGIN = 2,FUN = healthy_sample_test)
  ind_2 <- pmin(1, pmax(0, exp(1-ind_2)))
  gamma_blend <- ind_1 * ind_2
  
  ###
  lambda_1 <- 0.5^(1/barra_param$sigma_param$tau_1)
  wgt_1 <- lambda_1 ^ ((h_res-1):0) # unscaled # wgt_1 <- wgt_1/sum(wgt_1) 
  lambda_2 <- 0.5^(1/barra_param$sigma_param$tau_2)
  wgt_2 <- lambda_2 ^ ((h_res-1):0) # unscaled # wgt_2 <- wgt_2/sum(wgt_2)
  
  # rewgt to make sure wgt sum to 1 for obervations that have NA
  # examle # sum(c(NA,1) * c(1,2), na.rm = T)/ sum(!is.na(c(NA,1)) * c(1,2), na.rm = T)
  subfun_mazi <- function(mat, wgt){
    mat_sum <- colSums(mat * wgt, na.rm = TRUE)
    mat_sum_wgt <- colSums((!is.na(mat)) * wgt, na.rm = TRUE)
    result <- mat_sum / mat_sum_wgt
    # IF ALL NA, THE COLSUMS WOULD BE ZERO, THEN RESULTS WOULD BE NaN
    result[is.na(result)] <- 0
    return(result)
  }
  
  # getting sigma ts
  res_seri_mean <- subfun_mazi(mat = res_seri_mat, wgt = wgt_1)
  res_seri_scaled <- t(t(res_seri_mat) - as.vector(res_seri_mean))
  # SIGMA_D
  sigma_d <- subfun_mazi(mat = res_seri_scaled * res_seri_scaled, wgt = wgt_1)
  sigma_ts <- sigma_d
  lag <- barra_param$sigma_param$lag
  if(lag > 0){
    for(ii in 1:lag){  
      sigma_plus <- subfun_mazi(mat = res_seri_scaled[-(1:ii),,drop = FALSE] * res_seri_scaled[-((nrow(res_seri_scaled)-ii+1):(nrow(res_seri_scaled))),,drop = FALSE], wgt = wgt_2[-(1:ii)])
      sigma_ts <- sigma_ts + 2*(1 - (ii)/(1+lag))*sigma_plus
    }
  }
  # GETTING SIGMA STR
  abs_res <- colMeans(abs(res_seri_mat), na.rm = TRUE) # ALL MISSING RETURN NaN
  abs_res[is.nan(abs_res)] <- 0
  sigma_ts_df <- data.table::data.table('date_end' = date_end_i, 
                                        'stockID' = names(sigma_ts), 
                                        'sigma_ts' = sigma_ts,
                                        'abs_res' = abs_res,
                                        "gamma_blend" = gamma_blend,
                                        stringsAsFactors = FALSE)
  # in sigma_ts_df, the sigma might be negative, due to unhealthy data. in this case, the gamma should be 0.
  if(any(as.logical((sigma_ts_df$sigma_ts < 0) * (gamma_blend == 1)))){
    warnings("BUG. ERRORCODE 2310181017.")
    message("warnings", as.character(date_i), 
        sum(as.logical((sigma_ts_df$sigma_ts < 0) * (gamma_blend == 1))))
    sigma_ts_df[as.logical((sigma_ts_df$sigma_ts < 0) * (gamma_blend == 1)), gamma_blend := 0]
  }
  sigma_str_df <- cal_sigma_str(sigma_ts_df, data_bank = data_bank, barra_param = barra_param)
  
  sigma_df <- sigma_str_df[sigma_ts_df, on = .(date_end, stockID), mult = 'all']
  sigma_df[,sigma := (sigma_ts*gamma_blend + (1-gamma_blend)*sigma_str)]
  
  sigma_result <- sigma_df[,c("date_end","stockID","sigma")]
  colnames(sigma_result) <- c("date","stockID","sigma")
  return(sigma_result)
}



#' STRUCTURE SIGMA ESTIMATION FUNCTION
#' 
#' @param sigma_ts_df data table. The historical sigma data table, 
#' including columns 'date_end', 'stockID','sigma_ts','abs_res','gamma_blend'
#' @param data_bank data table. The data table obtained from load_data_bank(res_seri).
#' @return data table, the structural sigma data table,
#' including columns 'date_end', 'stockID', 'sigma_str'
#' @description This function aimed to predict structural sigma using historical sigma.
cal_sigma_str <- function(sigma_ts_df, data_bank, barra_param){
  
  # INPUT : ONE PERIOD ONLY
  sigma_ts_df <- data_bank[sigma_ts_df, on = .(date_end, stockID), mult = 'all']
  str_risk_fls <- str_risk_fls_default()
  fnames <- sapply(str_risk_fls, `[[`, 'factorName')
  
  barra_param$reg_param$subset_reg
  # subset reg flag
  subset_reg <- barra_param$reg_param$subset_reg
  if(!is.null(subset_reg)){
    sigma_ts_df <- as.data.frame(sigma_ts_df) # affect by is_component
    sigma_ts_df$subset_reg_flag <- 0
    for(i in 1:length(subset_reg)){
      sigma_ts_df <- is_component(sigma_ts_df, sectorID = subset_reg[i])
      sigma_ts_df$subset_reg_flag <- sigma_ts_df$is_comp + sigma_ts_df$subset_reg_flag
      sigma_ts_df$is_comp <- NULL
    }
    sigma_ts_df$subset_reg_flag <- (sigma_ts_df$subset_reg_flag > 0) + 0 # ensure only 1 or 0
    sigma_ts_df <- data.table::as.data.table(sigma_ts_df)
    sigma_ts_df$subset_reg_flag <- sigma_ts_df$gamma_blend * sigma_ts_df$is_comp
  }else{
    sigma_ts_df$subset_reg_flag <- sigma_ts_df$gamma_blend
  }
  
  sigma_ts_df$sigma_ts <- log(sigma_ts_df$sigma_ts)
  reg_result <- reg_mazi(mtsfr_i = sigma_ts_df, y = "sigma_ts", fnames = fnames, subset_reg = TRUE)
  
  sigma_str_df <- data.table::data.table('date_end' = sigma_ts_df$date_end, 
                                         'stockID' = reg_result$est_rtn$stockID, 
                                         'sigma_str' = exp(reg_result$est_rtn$est_rtn) * 1.026,
                                         stringsAsFactors = FALSE)
  return(sigma_str_df)
}

#' SIGMA FIX FUNCTION FOR BEYASIAN ADJUSTMENT
#' 
#' @param sigma_i data table, including columns 'date', 'stockID', 'sigma'
#' @return sigma_i, same as input
#' @description This function is aimed for beyasian adjustment for sigma.
#' @export
fix_frtn_sigma <- function(sigma_i, barra_param){
  
  shrink_q <- barra_param$sigma_param$shrink_q
  decile_n <- barra_param$sigma_param$decile_n
  
  # sigma_i : date, stockID, sigma
  sigma_i <- as.data.frame(sigma_i) # to do ...
  sigma_i <- gf_cap(sigma_i, var = "free_cap", varname = "ffv")
  sigma_i <- getSectorID(sigma_i, sectorAttr = defaultSectorAttr("fct", 
                                                                 fct_std = list(fl_cap()), 
                                                                 fct_level = decile_n)) # silly move
  sigma_i <- data.table::as.data.table(sigma_i)
  
  sigma_i[,sigma_prior := (sum(sigma*ffv)/sum(ffv)),by = c("date","sector")]
  sigma_i[,sigma_diff := sigma - sigma_prior,]
  sigma_i[,delta_sn := sqrt(sum(sigma_diff^2)/.N),by = c("date","sector")]
  sigma_i[,v_n := (shrink_q * abs(sigma_diff))/(delta_sn + shrink_q * abs(sigma_diff)),]
  
  sigma_i[,sigma_new := v_n * sigma_prior + (1 - v_n) * sigma,]
  sigma_i <- sigma_i[,.(date, stockID, 'sigma' = sigma_new),]
  
  return(sigma_i)
}

#' SIGMA FIX FUNCTION FOR BIAS ADJUSTMENT
#' 
#' @param sigma_i data table, including columns 'date', 'stockID', 'sigma'
#' @param res_seri data table or data frame. The residuals data. 
#' The columns should be 'date', 'stockID' and 'res'.
#' @param data_bank data table. The data table obtained from load_data_bank(res_seri).
#' @param result_type "sigma" or "bias". 
#' If "sigma", the output would be the sigma_i after fixation. 
#' If "bias", the output would be the bias recording for fixation.
#' @description The function is aimed for bias adjustment for sigma.
#' @export
fix_frtn_sigma2 <- function(sigma_i, res_seri, barra_param,
                            data_bank, result_type = c("sigma", "bias")){
  result_type <- match.arg(result_type)
  h_res <- barra_param$sigma_param$h_res
  h_vra <- barra_param$sigma_param$h_vra
  
  if(result_type == "sigma"){
    
    begT <- min(sigma_i$date)
    begT0 <- trday.nearby(begT, by = -(h_res+h_vra-1))
    if(begT0 < min(res_seri$date_end)) stop("res_seri data history not long enough.")
    
    datelist_full <- sort(unique(res_seri$date_end))
    datelist_full <- datelist_full[datelist_full >= trday.nearby(begT, by = - h_vra)]
    
  }else if(result_type == "bias"){
    
    begT <- min(sigma_i$date)
    begT0 <- trday.nearby(begT, by = -(h_res-1))
    if(begT0 < min(res_seri$date_end)) stop("res_seri data history not long enough.")
    
    datelist_full <- sort(unique(res_seri$date_end))
    datelist_full <- datelist_full[datelist_full >= begT]
    
  }
  
  barra_param2 <- barra_param
  barra_param2$sigma_param$lag <- 0
  
  sigma_fullset <- get_frtn_sigma(res_seri = res_seri, rebDates = datelist_full, 
                                  barra_param = barra_param2, 
                                  data_bank = data_bank)
  sigma_fullset$sigma <- sigma_fullset$sigma / 252 # lag = 0, corresponding to daily data
  sigma_fullset <- fix_frtn_sigma(sigma_i = sigma_fullset, barra_param2)
  
  # merge sigma_i, res_seri
  colnames(sigma_fullset) <- c("date_end", "stockID", "sigma")
  union_dataset <- merge.x(sigma_fullset, res_seri, by = c("date_end","stockID"))
  union_dataset$res_sigma_ratio <- ((union_dataset$res)^2) / union_dataset$sigma 
  union_dataset <- gf_cap(union_dataset, varname = "ffv", var = "free_cap") # no sqrt
  union_dataset <- data.table::as.data.table(union_dataset) 
  bias_df <- union_dataset[,.('bias' = sum(ffv * res_sigma_ratio, na.rm = TRUE)/sum(ffv, na.rm = TRUE)),by = date_end] # date, bias_i
  
  if(result_type == "bias") return(bias_df)
  
  # compute new sigma
  lambda <- 0.5^(1/barra_param$sigma_param$tau_vra)
  wgt <- lambda ^ ((h_vra-1):0)
  wgt <- wgt/sum(wgt) 
  rebdate_list <- sort(unique(sigma_i$date))
  
  # lambda_record <- data.frame()
  sigma_new <- data.frame()
  for(i in 1:length(rebdate_list)){
    
    rebdate_i <- rebdate_list[i]
    #
    bias_df_sub <- tail(subset(bias_df, date_end <= rebdate_i), h_vra)
    vol_adj_par <- as.vector(t(wgt) %*% bias_df_sub$bias)
    #
    sigma_i_sub <- subset(sigma_i, date == rebdate_i)
    sigma_i_sub$sigma <- sigma_i_sub$sigma * vol_adj_par
    sigma_new <- rbind(sigma_new, sigma_i_sub)
    
    # for observing records
    # lambda_vec <- data.frame('date' = rebdate_i, 'lambda' = vol_adj_par)
    # lambda_record <- rbind(lambda_record, lambda_vec)
  }
  #
  return(sigma_new)
}

