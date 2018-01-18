


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  get RebDates,TS,TSR,TSF,TSFR object ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' getRebDates
#'
#' get the rebalance dates(a \bold{RebDates} object) between begT and endT, and then shift it foreward if necessary.
#' @param begT the begin date of class Date
#' @param endT the end date of class Date
#' @param rebFreq the rebalancing freq. an interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s". (See \code{\link{cut.Date}} for detail.) Or a character string of "allspan". "allspan" means no rebalance during the whole period between begT and endT, which could be realized also by a very large interval specification, eg. "100 year".
#' @param shiftby a integer,how many days the rebalancing date be shifted foreward
#' @param dates a vector of class Date. 
#' @return a \bold{RebDates} object giving the rebalancing dates list,a vector with class of \code{Date}. If dates is not null, then return the dates themselves.
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples 
#' RebDates <- getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'3 month', 10)
#' getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'allspan')
#' getRebDates(as.Date('2010-01-01'),as.Date('2012-06-30'),'100 year')
#' getRebDates(dates=as.Date(c("2003-04-09","2003-07-06","2003-09-12")))
getRebDates <- function(begT,endT,rebFreq="month",shiftby=0, dates=NULL){
  if(!is.null(dates)){
    re <- trday.nearest(dates)
  } else {
    begT <- trday.nearest(begT)
    endT <- trday.nearest(endT)
    trdays <- trday.get(begT,endT)
    # cut by rebalancing frequency
    if(rebFreq != "allspan"){
      rebdays <- as.Date(unique(cut.Date2(trdays,rebFreq))) 
    } else {
      rebdays <- c(begT,endT)
    }   
    # add begT as begin point
    if(rebdays[1] != begT){
      rebdays <- c(begT,rebdays) 
    } 
    # shift the rebalancing dates foreward
    re <- trday.nearby(rebdays,shiftby)
  }
  
  return(re)
}




#' getTS
#'
#' get the 'time*stock frame'(a \bold{TS} object) given the rebalancing dates and the universe. The universe could be a index, a sector or a plate.
#' @param RebDates a \bold{RebDates} object. a vector,with class of Date,usually the rebalancing dates list
#' @param indexID a character string. The ID of the index, sector or plate. Could be a single-ID-code(eg. "EI000300","ES09440000",...) or a more complicated express containing some set operations and ID-codes(eg. "setdiff(union(EI000300,EI399006),ES09440000)"). See detail in \code{\link{getComps}}.
#' @param stocks a vector of stockIDs
#' @param rm could be one or more of "suspend","priceLimit".default is NULL
#' @return a \bold{TS} object. a dataframe,with cols:
#'   \itemize{
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item stockID: the stockID 
#'   }.IF stocks is not null then return \code{expand.grid} of stocks and RebDates, else return components of index on RebDates. 
#' @author Ruifei.Yin
#' @seealso \code{\link{getComps}}
#' @export 
#' @family frame building functions
#' @examples
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TS2 <- getTS(RebDates,'ES09440000')  # financial servive sector
#' TS3 <- getTS(RebDates,'setdiff(EI000300,ES09440000)')  # CSI300 ex. financial servive sector
#' TS4 <- getTS(RebDates,stocks=c("EQ000002","EQ000023","EQ600000"))
getTS <- function(RebDates,indexID="EI000300",stocks=NULL,rm=NULL){  
  RebDates <- trday.nearest(RebDates)
  if(!is.null(stocks)){
    TS <- expand.grid(date=RebDates,stockID=stocks)
  } else {
    TS <- getComps(ID=indexID,endT=RebDates,drop=FALSE) 
  }
  if("suspend" %in% rm){
    TS <- rm_suspend(TS, nearby = 1L)
  }
  if("priceLimit" %in% rm){
    TS <- rm_priceLimit(TS, nearby = 1L, lim = c(-Inf,10))
  }
  return (TS)
}




#' getTSR
#'
#' Get the \bold{TSR}('time-stock-rtn') or \bold{TSFR}('time-stock-factor-rtn') object by adding the period rtn between the rebalance date and the date_end(\strong{(close of "date_end")/(prevclose of the day after "date")-1)}) to the \bold{TS} or \bold{TSF} object.
#' @param TS a \bold{TS} or \bold{TSF} object (See detail in \code{\link{getTS}} and \code{\link{getTSF}})
#' @param dure a period object from package \code{lubridate}. (ie. \code{months(1),weeks(2)}. See example in \code{\link{trday.offset}}.) If null, then get periodrtn between \code{date} and the next \code{date}, else get periodrtn of '\code{dure}' starting from \code{date}.
#' @param date_end_pad a Date value, padding the NAs of the last date_end. if missing, calculated automatically.
#' @return given a \bold{TS} object,return a \bold{TSR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class 
#'   \item date_end: the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   }
#'   given a \bold{TSF} object,return a \bold{TSFR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item factorscore:the factor score of the stocks on the  rebalancing date  
#'   \item date_end: the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   }  
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples
#' ## -- get a TSR object
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300') 
#' TSR <- getTSR(TS) # rebalance properly joined
#' require(lubridate)
#' TSR2 <- getTSR(TS,weeks(2)); TSR3 <- getTSR(TS,months(2))  # rebalance not properly joined
#' ## -- get a TSFR object
#' factorFun <- "gf_demo"
#' factorPar <- list(0,1)
#' TSF <- getTSF(TS,factorFun,factorPar)
#' TSFR <-  getTSR(TSF)
getTSR <- function(TS, dure=NULL, date_end_pad){ 
  cat(paste("Function getTSR: getting the periodrtn ....\n"))
  check.TS(TS)
  # ---- Add the end date (and the decayed dates)
  date <- unique(TS$date)
  if (is.null(dure)){
    if(missing(date_end_pad)){ # calculate the padding value automatically
      date_end_pad <- trday.offset(max(date),by = lubridate::days(round(periodicity_Ndays(date))), dir = 1L)
    }
    date_end <- c(date[-1],date_end_pad)
  } else {
    date_end <- trday.offset(date,dure)
  }  
  merging <- data.frame(date,date_end)
  TS <- merge.x(TS,merging,by="date")  
  periodrtn <- data.frame(periodrtn=getPeriodrtn(renameCol(TS,c("date","date_end"),c("begT","endT")),drop=TRUE))
  TSR <- cbind(TS,periodrtn)
  return(TSR)
}


#' getTSR_decay
#' 
#' @param prdLists
#' @return a \bold{TSR} including the decayed period rtns
#' @export
getTSR_decay <- function(TS, prd_lists = list(w1=lubridate::weeks(1),
                                              w2=lubridate::weeks(2),
                                              m1=months(1),
                                              m2=months(2),
                                              m3=months(3),
                                              m6=months(6))){
  check.TS(TS)
  date <- unique(TS$date)
  if(TRUE){ # --- add endTs
    prd_names <- names(prd_lists)
    if(all(is.na(prd_names))){
      warning("The names of prd_lists is missing, getting the names automatically.")
      prd_names <- sapply(prd_lists,substr,1,5)
    }
    date.decay <- data.frame()
    for(ii in 1:length(prd_lists)){
      endT_i <- trday.offset(date,prd_lists[[ii]])
      if(ii==1L){
        date.decay <- data.frame(endT_i)
      } else {
        date.decay <- cbind(date.decay,endT_i)
      }
    }
    names(date.decay) <- paste("endT_",prd_names,sep = "")
    merging <- data.frame(date,date.decay)
  } 
  TS <- merge.x(TS,merging,by="date")   
  if(TRUE){ # --- add prdrtns
    cat("Getting the decayed rtn ....\n")
    pb_ <- txtProgressBar(min=0,max=length(prd_lists),initial=0,style=3)
    for(ii in 1:length(prd_lists)){   
      SP <- TS[,c("stockID","date",paste("endT_",prd_names[ii],sep=""))]
      names(SP) <- c("stockID","begT","endT")
      rtnI <- data.frame(rtn=getPeriodrtn(SP,drop=TRUE))      
      rtnI <- renameCol(rtnI,"rtn",paste("prdrtn_",prd_names[ii],sep=""))
      TS <- cbind(TS,rtnI)
      setTxtProgressBar(pb_,ii)
    }
    close(pb_)
    TSR <- TS[,!names(TS) %in% paste("endT_",prd_names,sep="")]
  }
}





#' getTSF
#'
#' get the \bold{TSF} ('time-stock-factor') or \bold{TSFR}('time-stock-factor-rtn') object,by adding the factor score to a \bold{TS} or \bold{TSR} object.If necessary,cleaning and stardizing the factor score.
#' @param TS a \bold{TS} or \bold{TSR} objectobject(See detail in \code{\link{getTS}} and  \code{\link{getTSR}})
#' @param factorFun a character string naming the function to get the factor scores.
#' @param factorPar either a list or a character string, containing the parameters of the \code{factorFun}. If a character string, parameters are seprated by commas and the character parameters must quoted by  "\""s, see examples for details.
#' @param factorDir a integer,should be 1 or -1 (1 for the positive factor,-1 for the negative one).
#' @param factorRefine a list.
#' @param factorList
#' @param splitbin a integer or a charactor string of 'year','month',...
#' @return given a \bold{TS} object,return a \bold{TSF} object,a dataframe containing cols:
#'   \itemize{
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item stockID: the stockID contained in the index or BK
#'   \item factorscore:the factor score of the stocks on the  rebalancing date
#'   \item sector(optional):the sector of a stock
#'   }
#'   given a \bold{TSR} object,return a \bold{TSFR} object,a dataframe containing cols:
#'   \itemize{ 
#'   \item stockID: the stockID contained in the index or BK
#'   \item date: the rebalance dates, with \code{Date} class
#'   \item factorscore:the factor score of the stocks on the  rebalancing date  
#'   \item date_end(optional): the next rebalance dates, with \code{Date} class
#'   \item periodrtn:the period rtn between the rebalance date and the date_end
#'   \item sector(optional):the sector of a stock
#'   }
#' @author Ruifei.Yin
#' @export
#' @family frame building functions
#' @examples
#' # -- get the TSF step by step --
#' 
#' # - get a TSF object
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' factorFun <- "gf_demo"
#' factorPar <- list(mean=0,sd=1)
#' TSF <- getTSF(TS,factorFun,factorPar)
#' # - get a TSFR object
#' TSR <- getTSR(TS)
#' TSFR <- getTSF(TSR)
#' 
#' # -- you can also get the TSF through the modelPar object directively --
#' modelPar <- modelPar.default()
#' modelPar <- modelPar.factor(modelPar,factorFun="gf_demo",factorPar=list(0,1))
#' TSF <- Model.TSF(modelPar)
#' 
#' # -- factorPar as character string
#' TSF2 <- getTSF(TS,"gf_demo","0,1")
#' TSF2 <- getTSF(TS,"gf_demo","mean=0,sd=1")
#' TSF2 <- getTSF(TS,"gf.foo","2,3")
#' TSF2 <- getTSF(TS,"gf.bar","20,\"IF00\"")
getTSF <- function(TS,factorFun,factorPar=list(),factorDir=1,
                   factorRefine=refinePar_default("none"),
                   FactorList,
                   splitbin=100000L){
  if(!missing(FactorList)){
    factorFun <- FactorList$factorFun
    factorPar <- FactorList$factorPar
    factorName <- FactorList$factorName
    factorDir <- FactorList$factorDir
    factorRefine <- FactorList$factorRefine
  }
  if(! factorDir %in% c(1L,-1L)) stop("unsupported \"factorDir\" argument!")
  cat(paste("Function getTSF: getting the factorscore ....\n"))
  check.TS(TS)
  
  subFun <- function(TS){
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS[,c("date","stockID")],factorFun,factorPar)
    # ---- refine the factorscore
    TSF <- factor_refine(TSF, refinePar = factorRefine)
    # ---- adjust the direction
    TSF$factorscore <- TSF$factorscore*factorDir
    # ---- re-order
    TSF <- merge.x(TS,TSF,by=c("date","stockID"))
    return(TSF)
  }
  
  if(is.numeric(splitbin)){
    cutN <- nrow(TS) %/% splitbin +1
    if(cutN > 1){
      idx <- cut(1:nrow(TS), breaks= cutN ,labels=FALSE)
    } else {
      idx <- factor(rep(1, nrow(TS)))
    }
  } else if(is.character(splitbin)){
    idx <- cut(TS$date, splitbin) 
  } else {
    stop("unsupported \"splitbin\" argument!")
  }
  if(length(levels(idx))>1){
    cat(paste('Possibly dealing with a big data. The process is splited to ',length(levels(idx)),'groups.\n'))
  }
  TS_ <- data.frame(idx,TS)
  TS_ <- dplyr::group_by(TS_,idx)
  re <- dplyr::do(TS_, subFun(as.data.frame(.)))
  re <- as.data.frame(re)
  re <- dplyr::select(re,-idx)
  return(re)
}



#' @rdname getTSF
#' @export
#' @details \code{getRawFactor} return a \bold{TSF} object, with 'raw' factorscore, which has not been standardized and not been deal with missing values and outliers. Function \code{getRawFactor} is often called intermediately by function \code{getTSF}.
#' @return \code{getRawFactor} return a \bold{TSF} object, with 'raw' factorscore. 
#' @examples
#' # -- get 'raw' factorscore
#' TSF_raw <- getRawFactor(TS,"gf.pct_chg_per","20")
getRawFactor <- function (TS,factorFun,factorPar,FactorList) {
  if("factorscore" %in% names(TS)){
    stop("There has existed a 'factorscore' field in the TS object. Please remove or rename it first!")
  }
  if(!missing(FactorList)){
    factorFun <- FactorList$factorFun
    factorPar <- FactorList$factorPar
  }
  if(missing(factorPar)){
    TSF <- do.call(factorFun,c(list(TS)))
  } else if(is.list(factorPar)){
    TSF <- do.call(factorFun,c(list(TS),factorPar))    
  } else if(is.character(factorPar)){
    if(is.na(factorPar)||factorPar==""){
      funchar <- paste(factorFun,"(TS)")
    } else {
      funchar <- paste(factorFun,"(","TS,",factorPar,")")
    }    
    TSF <- eval(parse(text=funchar))
  } else {
    stop("The factorPar must be a list or a character string!")
  }
  if(!("factorscore" %in% names(TSF))){
    stop("There must be a 'factorscore' field in the TSF result!")
  }
  return(TSF)
}


#' get multiFactors
#' 
#' get multiple single-factor-scores and the combined-factor-score.#' 
#' @param TS
#' @param FactorLists a list, with elements of FactorList(See detail in \code{\link{buildFactorList}}).
#' @param wgts a numeric vector with the same length of FactorLists(if the sum of wgts not equals to 1, the wgts will be rescaled to sum 1). If 'eq', \code{wgts <- rep(1/length(FactorLists),length(FactorLists))} ;If missing, then only return all the single-factor-scores, without the combined-factor-score.
#' @return  \code{getMultiFactor} return a \bold{mTSF} object including cols of all the single-factor-scores, and (if param \code{wgts} not missing) the \bold{raw} combined-factor-score .
#' @note  Function \code{getMultiFactor} not only could be used to get the "raw" combined-factor-score, but also could be used as the \code{"factorFun"} parametre in function \code{getTSF} to get the "clean and standardized" combined-factor-score. See more detail in the examples.
#' @details Function \code{getMultiFactor} firstly get all the single-factor-scores and then calculating the weighted sum of them to get the combined-factor-score.
#' @export
#' @examples
#' # -- get multi-factor by FactorLists 
#' FactorLists <- list(
#'   buildFactorList(factorFun="gf.PE_ttm",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=20),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),       
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=120),
#'                   factorDir=-1))
#' wgts <- c(0.25,0.25,0.25,0.25)
#' 
#' # -- 0. get the all the single-factor-scores
#' TSF <- getMultiFactor(TS,FactorLists)
#' 
#' # -- 1. get the "raw" combined-factor-score
#' TSF.multi <- getMultiFactor(TS,FactorLists,wgts)
#' 
#' # -- 2. get the "clean and standardized" combined-factor-score
#' # --- 2.1 by \code{getTSF}
#' TSF.multi2 <- getTSF(TS,factorFun="getMultiFactor",factorPar=list(FactorLists,wgts),factorRefine=refinePar_default("scale"))
#' # --- 2.2 by \code{Model.TSF}
#' modelPar <- modelPar.factor(modelPar.default(),factorFun="getMultiFactor",factorPar=list(FactorLists,wgts),factorRefine=refinePar_default("scale"))
#' TSF.multi3 <- Model.TSF(modelPar)
#' TSF.multi4 <- Model.TSF_byTS(modelPar, TS)
getMultiFactor <- function(TS,FactorLists,wgts,silence=FALSE){
  # ---- get all the single-factor-scores
  for(i in 1:length(FactorLists)){
    factorFun <- FactorLists[[i]]$factorFun
    factorPar <- FactorLists[[i]]$factorPar
    factorName <- FactorLists[[i]]$factorName
    factorDir <- FactorLists[[i]]$factorDir
    factorRefine <- FactorLists[[i]]$factorRefine
    if(!silence){
      cat(paste("Function getMultiFactor: getting the score of factor",factorName,"....\n"))
    }
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS,factorFun,factorPar) 
    # ---- adjust the direction
    TSF$factorscore <- TSF$factorscore*factorDir
    # ---- standardize the factorscore 
    TSF <- factor_refine(TSF,refinePar = factorRefine)
    # ---- merge
    TSF <- renameCol(TSF,"factorscore",factorName)
    if(i==1L){
      re <- merge.x(TS,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    } else {
      re <- merge.x(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    }
  }
  # ---- get the combi-factor-score
  factorNames <- sapply(FactorLists,"[[","factorName")
  if(!missing(wgts)){
    re <- MultiFactor2CombiFactor(mTSF=re,wgts=wgts,factorNames=factorNames,keep_single_factors="TRUE")
  }
  
  return(re)
}



#' @rdname getMultiFactor
#' @export
#' @param TSFs a \bold{TSFs} object. see /code{/link{Model.TSFs}}
#' @return \code{getMultiFactorbyTSFs} return a \bold{TSF} object including cols of all the single-factor-scores, and (if param \code{wgts} not missing) the \bold{raw} combi-factor-score, from a \bold{TSFs} list. (If param TSFs is a TSFRs object, then will return a TSFR object.)
#' @examples 
#' # -- get multi-factor by TSFs 
#' MPs <- getMPs_FactorLists(FactorLists, modelPar.default())
#' TSFs <- Model.TSFs(MPs)
#' TSF.multi5 <- getMultiFactorbyTSFs(TSFs,wgts)
getMultiFactorbyTSFs <- function(TSFs,wgts,
                                 factorNames = names(TSFs)){
  
  # ---- get all the single-factor-scores
  re <- TSFs2mTSF(TSFs=TSFs, factorNames = factorNames)
  
  # ---- get the combi-factor-score(weighted sum of all the single-factor-scores)
  if(!missing(wgts)){
    re <- MultiFactor2CombiFactor(mTSF=re,wgts=wgts,factorNames=factorNames,keep_single_factors="TRUE")
  }
  
  return(re)
}


#' @rdname getMultiFactor
#' @export
getRawMultiFactor <- function(TS,FactorLists,name_suffix=FALSE){
  for(i in 1:length(FactorLists)){
    factorFun <- FactorLists[[i]]$factorFun
    factorPar <- FactorLists[[i]]$factorPar
    factorName <- FactorLists[[i]]$factorName
    cat(paste("Function getMultiFactor: getting the score of factor",factorName,"....\n"))
    # ---- get the raw factorscore
    TSF <- getRawFactor(TS,factorFun,factorPar) 
    # ---- merge
    if(name_suffix){
      factorName <- paste(factorName,"R",sep="_")
    }
    TSF <- renameCol(TSF,"factorscore",factorName)
    if(i==1L){
      re <- merge.x(TS,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    } else {
      re <- merge.x(re,TSF[,c("date","stockID",factorName)],by=c("date","stockID"))
    }
  }
  return(re)
}


getRawMultiFactor_lcfs <- function(TS,FactorIDs,name_suffix=FALSE){
  
}
getMultiFactor_lcfs <- function(){
  
}

#' @rdname getTSF
#' @export
gf_lcfs <- function(TS,factorID){
  re <- getTech(TS,variables=factorID,tableName="QT_FactorScore",datasrc = "local")
  re <- renameCol(re,factorID,"factorscore")
  return(re)
}

#' @rdname getMultiFactor
#' @param mTSF a dataframe of 'TS & MultiFactors'
#' @param factorNames
#' @param keep_single_factors
#' @export
MultiFactor2CombiFactor <- function(mTSF,
                                    wgts,
                                    factorNames = guess_factorNames(mTSF),
                                    na_deal=c("ignore","rm"),
                                    keep_single_factors=TRUE){
  na_deal <- match.arg(na_deal)
  # ---- get the combi-factor-score(weighted sum of all the single-factor-scores)
  TSF_mat <- mTSF[,factorNames,drop=FALSE]
  if(identical(wgts,'eq')){
    wgts <- rep(1/length(factorNames),length(factorNames))
  }
  if(ncol(TSF_mat) != length(wgts)) {
    stop("The numbers of factors and wgts are not equal!")
  }
  if(!isTRUE(all.equal(sum(wgts),1,tolerance=0.001))){
    warning("The sum of wgts is not 1. The wgts will be rescaled! ")
    wgts <- wgts/sum(wgts)
  }  
  
  TSF_mat <- as.matrix(TSF_mat)
  if(na_deal=="ignore"){ # ignore the na value of specific factorscore.
    is_na <- is.na(TSF_mat)
    N <- nrow(TSF_mat)
    M <- ncol(TSF_mat)
    wgts_mat <- kronecker(matrix(1,N,1),t(wgts))
    wgts_mat <- wgts_mat*(!is_na)
    wgts_mat <- wgts_mat/kronecker(matrix(1,1,M),rowSums(wgts_mat))
    TSF_mat[is_na] <- 0
    sumScore <- rowSums(TSF_mat*wgts_mat)
  } else if(na_deal=="rm"){ # return NA when any factorscore is NA.
    sumScore <- TSF_mat %*% wgts
  } else {
    stop("uncorrect value of param 'na_deal'!")
  }
  
  if(keep_single_factors){
    re <- cbind(mTSF,factorscore=sumScore)
  } else {
    re <- cbind(dplyr::select(mTSF,-one_of(factorNames)),factorscore=sumScore)
  }
  return(re)
}




#' @rdname getMultiFactor
#' @export
#' @examples 
#' #-- TSFs2mTSF
#' mTSF <- TSFs2mTSF(TSFs)
#' mTSFR <- TSFs2mTSF(TSFRs)
TSFs2mTSF <- function(TSFs, factorNames = names(TSFs)){
  nrows <- sapply(TSFs,NROW)
  if(!all(nrows[1]==nrows)){
    stop("NROWs of TSFs are not all equal!\n")
    print(nrows)
  }
  for(i in 1:length(TSFs)){
    factorName <- factorNames[i]
    TSF <- TSFs[[i]]
    TSF <- renameCol(TSF,"factorscore",factorNames[i])
    if(i==1L){
      kept_cols <- colnames(TSF)[is_usualcols(colnames(TSF))]
      re <- TSF[, c(kept_cols,factorNames[i])]
    } else {
      re <- merge.x(re,TSF[, c("date","stockID",factorNames[i])], by=c("date","stockID"))
    }
  } 
  return(re)
}


#' @rdname getMultiFactor
#' @export
#' @examples 
#' #-- mTSF2TSFs
#' TSFs <- mTSF2TSFs(mTSF)
#' TSFRs <- mTSF2TSFs(mTSFR)
mTSF2TSFs <- function(mTSF, factorNames = guess_factorNames(mTSF)){
  all_cols <- colnames(mTSF)
  if("factorscore" %in% all_cols){
    stop("There should not be 'factorscore' in the cols of mTSF! Please try to rename it.")
  }
  no_fnames <- setdiff(all_cols,factorNames)
  re <- list()
  for (i in 1:length(factorNames)){
    fname <- factorNames[i]
    re_i <- mTSF[,c(no_fnames,fname)]
    re_i <- renameCol(re_i,fname,"factorscore")
    re[[i]] <- re_i
  }
  names(re) <- factorNames
  return(re)
}


#' TS_filter
#' 
#' sample or filter the TS by date.
#' 
#' @param TS a TS, TSF, TSFR ...
#' @param sample_N integer. size of sample days
#' @export
#' @examples
#' re <- TS_filter(ts,sample_N=8)
#' re <- TS_filter(ts,as.Date("2010-01-01"),as.Date("2011-01-01"))
TS_filter <- function(TS, begT, endT, sample_N){
  if(!missing(sample_N) && (!missing(begT) | !missing(endT))){
    stop("sample_N and begT should have only one!")
  }
  if(!missing(sample_N)){
    dates <- unique(TS$date)
    dates <- sample(dates,min(sample_N, length(dates)))
    TS <- dplyr::filter(TS,date %in% dates)
  } else {
    begT <- if(missing(begT)) as.Date("1900-01-01") else  begT
    endT <- if(missing(endT)) as.Date("9999-01-01") else  endT
    TS <- dplyr::filter(TS, date>=begT & date<=endT)
  }
  # if(!missing(begT)){
  #   TS <- dplyr::filter(TS, date>=begT & date<=endT)
  # }
  return(TS)
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  factor refining functions ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



#' factor_bcPower
#' 
#' @param lambda "auto" or a numeric.
#' @param out_type "factor", "lambda" or "both".
#' @export
#' @author Han.Qian
factor_bcPower <- function(TSF,lambda="auto",out_type=c("factor","lambda","both"),fname = "factorscore"){
  out_type <- match.arg(out_type)
  if(!all(TSF[,fname,drop = FALSE] > 0, na.rm = TRUE)) stop("The factorscore should all be positive to apply bcPower.")
  if(lambda != "auto"){
    TSF[,fname] <- car::bcPower(TSF[,fname], lambda = lambda)
    return(TSF)
  }else{
    check.colnames(TSF, c("date",fname))
    TSF <- renameCol(TSF, fname, "mazi_tmp_column")
    mdata <- dplyr::group_by(TSF, date)
    subfun <- function(TSF){
      lambda <- (car::powerTransform(TSF$mazi_tmp_column))$lambda
      TSF$lambda <- lambda
      TSF$mazi_tmp_column <- car::bcPower(TSF$mazi_tmp_column, lambda)
      return(TSF)
    }
    mdata <- dplyr::do(mdata, subfun(.))
    mdata <- as.data.frame(mdata)
    mdata <- renameCol(mdata, "mazi_tmp_column", fname)
    if(out_type == "factor"){
      # drop lambda
      mdata$lambda <- NULL
    }else if(out_type == "lambda"){
      # unique lambda
      mdata <- unique(mdata[,c("date","lambda")])
    }
    return(mdata)
  }
}




#' factor_outlier
#' 
#' @param method Currently support four types : none, sd, mad, boxplot.
#' @param par The parameters passed into the method.
#' @param sectorAttr could be a sectorAttr list or NULL.
#' @export
#' @author Han.Qian
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TSF <- gf.NP_YOY(TS)
#' TSF <- factor_outlier(TSF, method = "mad", par = 1.5)
factor_outlier <- function (TSF, method=c("none","sd","mad","boxplot","percentage"), par,
                            sectorAttr=defaultSectorAttr()) {
  method <- match.arg(method)
  # direct return
  if(method == "none"){
    return(TSF)
  }
  # sector splitting
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TS = TSF,sectorAttr = sectorAttr, fillNA = TRUE)
    TSF <- dplyr::group_by(TSF, date, sector)
  }else{
    TSF <- dplyr::group_by(TSF, date)
  }
  # sub fun
  outlier_sub_fun <- function(TSF, method, par){
    if(method == "sd"){
      outlier_u <- mean(TSF$factorscore, na.rm = TRUE) + par * sd(TSF$factorscore, na.rm = TRUE)
      outlier_l <- mean(TSF$factorscore, na.rm = TRUE) - par * sd(TSF$factorscore, na.rm = TRUE)
    }else if(method == "mad"){
      outlier_u <- median(TSF$factorscore, na.rm = TRUE) + par * mad(TSF$factorscore, na.rm = TRUE)
      outlier_l <- median(TSF$factorscore, na.rm = TRUE) - par * mad(TSF$factorscore, na.rm = TRUE)
    }else if(method == "boxplot"){
      boxplot_stat <- boxplot.stats(TSF$factorscore, coef = par)[[1]]
      outlier_u <- max(boxplot_stat)
      outlier_l <- min(boxplot_stat)
    }else if(method == "percentage"){
      outlier_u <- quantile(TSF$factorscore, probs = 1-par/100, na.rm = TRUE)
      outlier_l <- quantile(TSF$factorscore, probs = 0+par/100, na.rm = TRUE)
    }
    TSF <- transform(TSF,factorscore = ifelse(factorscore > outlier_u, outlier_u,
                                              ifelse(factorscore < outlier_l, outlier_l, factorscore)))
    return(TSF)
  }
  # processing
  TSF <- dplyr::do(TSF, outlier_sub_fun(., method, par))
  TSF <- as.data.frame(TSF)
  # remove sector column if its not from the input
  if(!identical(sectorAttr, "existing") & !is.null(sectorAttr)){
    TSF <- TSF[,setdiff(colnames(TSF), "sector")]
  }
  # output
  return(TSF)
}

#' factor_std
#' 
#' @param method Currently supporting four types : none, scale, robustScale, reg.
#' @param log Logical value. Default FALSE.
#' @param sectorAttr could be a sectorAttr list or NULL.
#' @param regLists factorLists. This argument will used when method is reg. The factorscore will be orthogonized by the factors in regLists.
#' @param reg_glmwgt NULL,"existing","sector","cap"
#' @export
#' @author Han.Qian
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TSF <- gf.NP_YOY(TS)
#' regLists <- buildFactorLists(buildFactorList(factorFun = 'gf.PE_ttm',factorDir = -1,factorRefine=refinePar_default("scale")))
#' TSF <- factor_std(TSF, method = "reg", log = FALSE, regLists = regLists)
factor_std <- function(TSF,method=c("none","scale","robustScale","reg"),
                       log=FALSE, 
                       sectorAttr=defaultSectorAttr(),
                       regLists=NULL,
                       reg_glmwgt=NULL
){
  method <- match.arg(method)
  
  # log
  if(log){
    TSF <- dplyr::mutate(TSF, factorscore=ifelse(factorscore==0,NA,log(factorscore)))
  }
  
  if(method == "none"){ # do nothing
    return(TSF)
    
  }else if(method == "reg"){ # reg part
    TSF <- renameCol(TSF, "factorscore", "Y_for_reg")
    
    # shrink columns (merge back later)
    if(identical(sectorAttr, "existing") & identical(reg_glmwgt, "existing")){
      TSF_core <- TSF[,c("date","stockID","Y_for_reg","sector","glm_wgt")]
    }else if(identical(sectorAttr, "existing")){
      TSF_core <- TSF[,c("date","stockID","Y_for_reg","sector")]
    }else if(identical(reg_glmwgt, "existing")){
      TSF_core <- TSF[,c("date","stockID","Y_for_reg","glm_wgt")]
    }else{
      TSF_core <- TSF[,c("date","stockID","Y_for_reg")]
    }
    
    # get glm_wgt
    if(!is.null(reg_glmwgt) & !identical(reg_glmwgt, "existing")){
      if(reg_glmwgt == "sector"){
        if(!identical(sectorAttr, "existing")){
          TSF_core <- getSectorID(TSF_core, sectorAttr = sectorAttr, fillNA = TRUE) 
          sectorAttr <- "existing"
        }
        glm_wgt_data <- dplyr::group_by(TSF_core, date, sector)
        glm_wgt_data <- dplyr::summarise(glm_wgt_data, glm_wgt = 1/var(Y_for_reg, na.rm = TRUE))
        TSF_core <- merge.x(TSF_core, glm_wgt_data, by = c("date","sector"))
        TSF_core <- as.data.frame(TSF_core)
        
      }else if(reg_glmwgt == "cap"){
        TSF_core <- gf_cap(TSF_core, log = TRUE, var = "mkt_cap", varname = "glm_wgt")
      }else{
        stop("The glm_wgt could not be identified.")
      }
    }
    
    # glm_wgt checking
    if(!is.null(reg_glmwgt)){
      if(any(is.infinite(TSF_core$glm_wgt))){
        TSF_core$glm_wgt[is.infinite(TSF_core$glm_wgt)] <- NA
      }
      if(any(is.na(TSF_core$glm_wgt))){
        TSF_core <- dplyr::group_by(TSF_core, date)
        TSF_core2 <- dplyr::mutate(TSF_core, 
                                   glm_wgt = ifelse(is.na(glm_wgt), yes = median(glm_wgt, na.rm = TRUE), no = glm_wgt))
        TSF_core <- as.data.frame(TSF_core)
      }
    }
    
    # get regList 
    if(!is.null(regLists)){
      TSF_core <- getMultiFactor(TSF_core, FactorLists = regLists, silence = TRUE)
      fnames_list <- sapply(regLists, '[[', 'factorName')
    }
    
    # orthogon
    if(is.null(reg_glmwgt)){
      TSF_core <- factor_orthogon_single(TSF_core, y = "Y_for_reg", sectorAttr = sectorAttr, regType = "lm")
    }else{
      TSF_core <- factor_orthogon_single(TSF_core, y = "Y_for_reg", sectorAttr = sectorAttr, regType = "glm")
    }
    TSF_core <- TSF_core[,c("date","stockID","Y_for_reg")]
    TSF_core <- renameCol(TSF_core, "Y_for_reg", "factorscore")
    
    # merge back
    TSF <- TSF[,setdiff(colnames(TSF), "Y_for_reg")]
    TSF <- merge.x(TSF, TSF_core, by = c("date","stockID"))
    
    # scale to N(0,1) 
    if(is.null(reg_glmwgt)){ # by date*sector
      TSF <- factor_std(TSF, method = "robustScale", sectorAttr = sectorAttr)
    } else { # by date
      TSF <- factor_std(TSF, method = "robustScale", sectorAttr = NULL)
    }
    
    # output
    return(TSF)
    
  }else{ # scale part
    # sector splitting
    if(!is.null(sectorAttr)){
      TSF <- getSectorID(TS = TSF, sectorAttr = sectorAttr, fillNA = TRUE, ungroup=10)
      TSF <- dplyr::group_by(TSF, date, sector)
    }else{
      TSF <- dplyr::group_by(TSF, date)
    }
    # sub fun
    std_sub_fun <- function(TSF, method){
      if(method == "scale"){
        TSF$factorscore <- as.numeric(scale(TSF$factorscore))
      }else if(method == "robustScale"){
        median_ <- median(TSF$factorscore, na.rm = TRUE)
        sd_ <- sd(TSF$factorscore, na.rm = TRUE)
        if(is.na(sd_) | sd_ == 0){
          TSF$factorscore <- NA
        }else{
          TSF$factorscore <- (TSF$factorscore - median_)/sd_
        }
      }
      return(TSF)
    }
    # processing 
    TSF <- dplyr::do(TSF, std_sub_fun(., method))
    TSF <- as.data.frame(TSF)
    # remove sector column if its not from the input
    if(!identical(sectorAttr, "existing") & !is.null(sectorAttr)){
      TSF <- TSF[,setdiff(colnames(TSF), "sector")]
    }
    # output
    return(TSF)
  }
}






#' factor_na
#' 
#' @param method Currently support three types : none, mean, median.
#' @param sectorAttr could be a sectorAttr list or NULL.
#' @export
#' @author Han.Qian
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TSF <- gf.NP_YOY(TS)
#' TSF <- factor_na(TSF, method = "median")
factor_na <- function (TSF, method=c("none","mean","median"), 
                       sectorAttr=defaultSectorAttr()) {
  method <- match.arg(method)
  # direct return
  if(method == "none"){
    return(TSF)
  }
  # sector splitting
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TS = TSF,sectorAttr = sectorAttr, fillNA = TRUE)
    TSF <- dplyr::group_by(TSF, date, sector)
  }else{
    TSF <- dplyr::group_by(TSF, date)
  }
  # sub fun
  na_sub_fun <- function(TSF, method){
    if(method == "mean"){
      ind_na_ <- is.na(TSF$factorscore)
      mean_ <- mean(TSF$factorscore, na.rm = TRUE)
      TSF[ind_na_,"factorscore"] <- mean_
    }else if(method == "median"){
      ind_na_ <- is.na(TSF$factorscore)
      median_ <- median(TSF$factorscore, na.rm = TRUE)
      TSF[ind_na_,"factorscore"] <- median_
    }
    return(TSF)
  }
  # batch processing
  TSF <- dplyr::do(TSF, na_sub_fun(., method))
  if(sum(is.na(TSF$factorscore))>0){ # If sector is too small, ungroup it.
    TSF <- as.data.frame(TSF)
    TSF <- dplyr::group_by(TSF, date)
    TSF <- dplyr::do(TSF, na_sub_fun(., method))
  }
  TSF <- as.data.frame(TSF)
  # remove sector column if its not from the input
  if(!identical(sectorAttr, "existing") & !is.null(sectorAttr)){
    TSF <- TSF[,setdiff(colnames(TSF), "sector")]
  }
  return(TSF)
}



#' refinePar_default
#' 
#' @param type Could be "none", "reg", "scale"
#' @param sectorAttr could be a sectorAttr list, or NULL(means no grouping by sector).
#' @param log TRUE or FALSE
#' @param regLists a \bold{factorLists}
#' @export
#' @author Han.Qian
refinePar_default <- function(type=c("none","scale","reg","reg_glm"), 
                              sectorAttr=NULL,
                              log=FALSE,
                              regLists=list(fl_cap(bc_lambda = "auto"))
                              ){
  type <- match.arg(type)
  if(type=="none"){
    re <- list(outlier=list(method = "none",
                            par=NULL,
                            sectorAttr= NULL),
               std=list(method = "none",
                        log=log, 
                        sectorAttr=NULL,
                        regLists=NULL),
               na=list(method = "none", 
                       sectorAttr=NULL)
               )
  }else if(type=="scale"){
    re <- list(outlier=list(method = "boxplot",
                            par=1.5,
                            sectorAttr= NULL),
               std=list(method = "robustScale",
                        log=log, 
                        sectorAttr=sectorAttr,
                        regLists=NULL),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
    )
  }else if(type=="reg"){
    re <- list(outlier=list(method = "boxplot",
                            par=1.5,
                            sectorAttr= NULL),
               std=list(method = "reg",
                        log=log, 
                        sectorAttr=sectorAttr,
                        regLists=regLists,
                        reg_glmwgt=NULL),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
    )
  }else if(type=="reg_glm"){
    re <- list(outlier=list(method = "boxplot",
                            par=1.5,
                            sectorAttr= NULL),
               std=list(method = "reg",
                        log=log, 
                        sectorAttr=sectorAttr,
                        regLists=regLists,
                        reg_glmwgt="sector"),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
               )
  }
  return(re)
}

#' setrefinePar
#' 
#' @param refinePar A refinePar list.
#' @param all_sectorAttr set all sectorAttr of std, outlier, na, in the same time
#' @return A refinePar list.
#' @export
#' @author Han.Qian
setrefinePar <- function(refinePar=refinePar_default(),
                         outlier_method,
                         outlier_par,
                         outlier_sectorAttr,
                         std_method,
                         std_log,
                         std_sectorAttr,
                         std_regLists,
                         na_method,
                         na_sectorAttr,
                         all_sectorAttr){
  if(!missing(outlier_method)){
    refinePar$outlier$method <- outlier_method
  }
  if(!missing(outlier_par)){
    refinePar$outlier$par <- outlier_par
  }
  if(!missing(outlier_sectorAttr)){
    refinePar$outlier$sectorAttr <- outlier_sectorAttr
  }
  if(!missing(std_method)){
    refinePar$std$method <- std_method
  }
  if(!missing(std_log)){
    refinePar$std$log <- std_log
  }
  if(!missing(std_sectorAttr)){
    refinePar$std$sectorAttr <- std_sectorAttr
  }
  if(!missing(std_regLists)){
    refinePar$std$regLists <- std_regLists
  }
  if(!missing(na_method)){
    refinePar$na$method <- na_method
  }
  if(!missing(na_sectorAttr)){
    refinePar$na$sectorAttr <- na_sectorAttr
  }
  if(!missing(all_sectorAttr)){
    refinePar$outlier$sectorAttr <- all_sectorAttr
    refinePar$std$sectorAttr <- all_sectorAttr
    refinePar$na$sectorAttr <- all_sectorAttr
  }
  return(refinePar)
}



#' factor_refine
#' @rdname factor_refine
#' @name factor_refine
#' @param refinePar A refinePar list. Created by refinePar_default. Modified by setrefinePar.
#' @export
#' @author Han.Qian
factor_refine <- function(TSF, refinePar=refinePar_default(),drop_sector=TRUE){
  if(is.null(refinePar)){
    return(TSF)
  }
  sector_outlier <- refinePar$outlier$sectorAttr
  sector_std <- refinePar$std$sectorAttr
  sector_na <- refinePar$na$sectorAttr
  if(identical(sector_outlier,sector_std) & identical(sector_std,sector_na) & !is.null(sector_outlier)){
    TSF <- getSectorID(TSF,sectorAttr = sector_outlier, fillNA = TRUE)
    refinePar <- setrefinePar(refinePar,
                              outlier_sectorAttr = "existing",
                              std_sectorAttr = "existing",
                              na_sectorAttr = "existing")
  }
  
  TSF <- factor_outlier(TSF,
                        method = refinePar$outlier$method,
                        par = refinePar$outlier$par,
                        sectorAttr=refinePar$outlier$sectorAttr)
  
  TSF <- factor_std(TSF,
                    method = refinePar$std$method,
                    log = refinePar$std$log,
                    sectorAttr = refinePar$std$sectorAttr,
                    regLists = refinePar$std$regLists,
                    reg_glmwgt = refinePar$std$reg_glmwgt)
  
  TSF <- factor_na(TSF,
                   method = refinePar$na$method,
                   sectorAttr = refinePar$na$sectorAttr)
  if(drop_sector & ("sector" %in% colnames(TSF))){
    TSF <- dplyr::select(TSF,-sector)
  }
  return(TSF)
}

#' @param refinePar_lists A list of (refinePar)s, each refinePar is a list built by refinePar_default.
#' @param refinePar_names The character vector of names, could be missing.
#' @rdname factor_refine
#' @export
#' @examples 
#' rawTSF <- gf.NP_YOY(TS, src = "fin")
#' refinePar_lists <- list(refinePar_default(type = "none"),
#'                         refinePar_default(type = "reg"),
#'                         refinePar_default(type = "scale"))
#' factor_refine_MF(rawTSF, refinePar_lists)
factor_refine_MF <- function(TSF, refinePar_lists, refinePar_names){
  # ARGUMENTS CHECKING
  loop_length <- length(refinePar_lists)
  if(missing(refinePar_names)){
    refinePar_names <- names(refinePar_lists)
    if(is.null(refinePar_names)){
      refinePar_names <- paste0("refinePar_",1:loop_length)
    }
  }else{
    if(length(refinePar_lists) != length(refinePar_names)){
      stop("The length of refinePar_names does not match the length of refinePar_lists.")
    }
  }
  
  # REFINE PART
  for( i in 1:loop_length){
    refinePar_ <- refinePar_lists[[i]]
    TSF_ <- factor_refine(TSF, refinePar = refinePar_, drop_sector = TRUE)
    TSF_ <- renameCol(TSF_, "factorscore", refinePar_names[i])
    if( i == 1L){
      mTSF <- TSF_
    }else{
      mTSF <- merge.x(mTSF, TSF_, by = c("date", "stockID"))
    }
  }
  return(mTSF)
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  TS removing functions ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
#' remove suspension stock from TS
#'
#' @param TS 
#' @param nearby a integer vector. default is 1L, which means remove the suspending of the next tradingday. see detail in \code{\link{trday.nearby}}.
#' @param datasrc
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- rm_suspend(TS) # remove Suspend of nextday
#' re1 <- rm_suspend(TS,nearby=c(0,1))# remove Suspend of today and nextday
#' @export
rm_suspend <- function(TS,nearby=1L,
                       datasrc=defaultDataSRC()){
  TS_ <- is_suspend(TS=TS,nearby=nearby,datasrc = datasrc)
  TS <- TS[!TS_$sus,]
  return(TS)
}



#' remove price limits
#' 
#' @param nearby
#' @param lim a vector of length 2.
#' @param priceType "close" or "open".
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- rm_priceLimit(TS)
#' re1 <- rm_priceLimit(TS,nearby=-1:1)
#' re2 <- rm_priceLimit(TS,lim=c(-Inf,10)) # remove limit-up
#' @export
rm_priceLimit <- function(TS,nearby=1,lim=c(-10, 10),priceType=c("close","open"),
                          datasrc=defaultDataSRC()){
  TS_ <- is_priceLimit(TS=TS,nearby = nearby,lim = lim, datasrc = datasrc)
  TS <- TS[!TS_$overlim,]
  return(TS)
}


#' @export
rm_blacklist <- function(TS){
  TS_ <- is_blacklist(TS=TS)
  TS <- TS[!TS_$is_blacklist,]
}

rm_not_in_whitelist <- function(TS){
  
}
rm_st <- function(TS){
  
}


#' rm_delist
#'
#' remove delisted stocks
#' @export
rm_delist <- function(TS,nearby=months(2),datasrc='jy'){
  TS_ <- is_delist(TS=TS,nearby=nearby,datasrc = datasrc)
  TS <- TS[!TS_$delist,]
  return(TS)
}


#' is_suspend
#'
#' @param nearby a integer vector. 0 means today, 1 means next tradingday. default is 0L. see detail in \code{\link{trday.nearby}}.
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- is_suspend(TS) #  Suspend of nextday
#' re1 <- is_suspend(TS,nearby=c(0,1))#  Suspend of today and nextday
#' @export
is_suspend <- function(TS,nearby=0,
                       datelist,stockID, 
                       drop,
                       datasrc=defaultDataSRC()){
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)) TRUE else FALSE
  }
  
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  check.TS(TS)
  if(datasrc=='ts'){
    sus <- rep(FALSE,nrow(TS))
    for (by in nearby){
      TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
      TS_ <- getTech_ts(TS_, funchar="istradeday4()",varname="trading")
      sus_ <- (!TS_$trading == 1) & TS_$date<=Sys.Date()
      sus <- sus|sus_
    }
  } else {
    sus <- rep(FALSE,nrow(TS))
    for (by in nearby){
      TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
      istradingday <- trday.is(TS=TS_, drop = TRUE)
      sus_ <- (!istradingday) 
      sus <- sus|sus_
    }
  }
  TS <- data.frame(TS,sus=ifelse(is.na(sus), FALSE, sus))
  
  
  if(drop){
    return(TS$sus)
  }else{
    return(TS)
  }
}


#' is_priceLimit
#' 
#' if over-price-limit
#' @param nearby a integer vector. 0 means today, 1 means next tradingday. default is 0L. see detail in \code{\link{trday.nearby}}.
#' @param lim a vector of length 2.
#' @param priceType "close" or "open".
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- is_priceLimit(TS)
#' re1 <- is_priceLimit(TS,nearby=-1:1)
#' re2 <- is_priceLimit(TS,lim=c(-Inf,10)) #  limit-up
#' is_priceLimit(stockID = "EQ300576",datelist = as.Date("2016-12-22"),priceType = "open") #  open-price over limit
#' @export
is_priceLimit <- function(TS,nearby=0,lim=c(-10, 10), priceType=c("close","open"),
                          datelist,stockID, 
                          drop,
                          datasrc=defaultDataSRC()){
  
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)) TRUE else FALSE
  }
  
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  check.TS(TS)
  priceType <- match.arg(priceType)
  
  if(priceType=="close"){
    if(datasrc=='ts'){
      overlim <- rep(FALSE,nrow(TS))
      for (by in nearby){
        TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
        TS_ <- getTech_ts(TS_, funchar=c("StockPrevClose3()","close()"),varname=c("pre_close","close"))
        in_lim <- TS_$close > round(TS_$pre_close*(1+lim[1]/100),2) & TS_$close < round(TS_$pre_close*(1+lim[2]/100),2)
        overlim_ <- (!in_lim)  & TS_$date<=Sys.Date()
        overlim <- overlim|overlim_
      }
    } else {
      overlim <- rep(FALSE,nrow(TS))
      for (by in nearby){
        TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
        TS_ <- getTech(TS_,variables=c("pre_close","close") ,datasrc = datasrc)
        in_lim <- TS_$close > round(TS_$pre_close*(1+lim[1]/100),2) & TS_$close < round(TS_$pre_close*(1+lim[2]/100),2)
        overlim_ <- (!in_lim) 
        overlim <- overlim|overlim_
      }
    }
  } else if(priceType=="open"){
    if(datasrc=='ts'){
      overlim <- rep(FALSE,nrow(TS))
      for (by in nearby){
        TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
        TS_ <- getTech_ts(TS_, funchar=c("StockPrevClose3()","open()"),varname=c("pre_close","open"))
        in_lim <- TS_$open > round(TS_$pre_close*(1+lim[1]/100),2) & TS_$open < round(TS_$pre_close*(1+lim[2]/100),2)
        overlim_ <- (!in_lim)  & TS_$date<=Sys.Date()
        overlim <- overlim|overlim_
      }
    } else {
      overlim <- rep(FALSE,nrow(TS))
      for (by in nearby){
        TS_ <- data.frame(date=trday.nearby(TS$date,by), stockID=TS$stockID)
        TS_ <- getTech(TS_,variables=c("pre_close","open") ,datasrc = datasrc)
        in_lim <- TS_$open > round(TS_$pre_close*(1+lim[1]/100),2) & TS_$open < round(TS_$pre_close*(1+lim[2]/100),2)
        overlim_ <- (!in_lim) 
        overlim <- overlim|overlim_
      }
    }
  } else {
    stop("Invalid param of 'priceTpye'!")
  }
  
  
  TS <- data.frame(TS,overlim=ifelse(is.na(overlim), FALSE, overlim)) # if NA, set to FALSE
  
  if(drop){
    return(TS$overlim)
  }else{
    return(TS)
  }
}



#' @export
is_blacklist <- function(TS,
                         datelist,stockID, 
                         drop,
                         datasrc=defaultDataSRC()){
  
  blklist=c("EQ600061","EQ600886","EQ600216") # temporally
  
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)) TRUE else FALSE
  }
  
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  check.TS(TS)
  
  TS$is_blacklist <- TS$stockID %in% blklist
  
  
  if(drop){
    return(TS$is_blacklist)
  }else{
    return(TS)
  }
  
}

#' is_st
#' 
#' is st stock
#' @author Ruifei.yin
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- is_st(TS)
#' @export
is_st <- function(TS,
                  datelist,stockID, 
                  drop,
                  datasrc=defaultDataSRC()){
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)) TRUE else FALSE
  }
  
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  check.TS(TS)
  
  if(datasrc=='ts'){
    TS_ <- TS.getTech_ts(TS, funchar="IsST_()")
    TS_ <- renameCol(TS_, c("IsST_()"), c("is_st"))
    TS_$is_st <- ifelse(TS_$is_st==1,TRUE,FALSE)
  } else if(datasrc=="local"){
    TS_ <- transform(TS,date=rdate2int(date))
    con <- db.local("qt")
    RSQLite::dbWriteTable(con,"yrf_tmp",TS_,overwrite=TRUE,row.names=FALSE)
    qr <- "select y.*,q.SecuAbbr 'is_st' from yrf_tmp y
    left join QT_DailyQuote2 q on y.date=q.TradingDay and y.stockID=q.ID"
    TS_ <- RSQLite::dbGetQuery(con,qr)
    RSQLite::dbDisconnect(con)
    TS_ <- transform(TS_,date=intdate2r(date),
                     is_st=stringr::str_detect(is_st,'ST'))
  }
  
  
  if(drop){
    return(TS_$is_st)
  }else{
    return(TS_)
  }
  
}
