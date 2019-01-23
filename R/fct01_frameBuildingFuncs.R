


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  buildFactorList(s) ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' buildFactorList
#' 
#' @rdname buildFactorList
#' @return buildFactorList return a object of \bold{FactorList}, a list of parametres of factor setting
#' @export
#' @examples
#' # -- set modelPar.factor by a factorlist
#' FactorList <- buildFactorList(factorFun="gf.pct_chg_per",factorPar=list(N=20))
#' modelPar <- modelPar.factor(modelPar,FactorList=FactorList)
buildFactorList <- function(factorFun = "gf_demo" ,
                            factorPar  = list() ,
                            factorDir  = 1 ,
                            factorRefine  = refinePar_default("none") ,
                            factorName = default.factorName(factorFun,factorPar,factorDir), 
                            factorID ="" ,
                            factorType = ""  ,
                            factorDesc = "" ){
  re <-list(factorFun  = factorFun ,
            factorPar   = factorPar  ,  
            factorDir   = factorDir  ,
            factorRefine   = factorRefine  ,   
            factorName  = factorName ,  
            factorID  = factorID,
            factorType  = factorType,
            factorDesc = factorDesc)   
  return(re)
}

#' @rdname buildFactorList
#' @param factorID a character string of factorID, available in table \code{CT_FactorLists()} .
#' @return buildFactorList_lcfs get a object of \bold{FactorList} through "lcfs". See more detail in \code{\link{buildFactorLists_lcfs}}. 
#' @export
#' @examples
#' # -- build a factorlist through "lcfs"
#' FactorList2 <- buildFactorList_lcfs(factorID="F000001")
buildFactorList_lcfs <- function(factorID, factorRefine  = refinePar_default("none")
){
  re <- buildFactorList(factorFun = "gf_lcfs",
                        factorPar = list(factorID),
                        factorDir = CT_FactorLists(factorID=factorID)$factorDir,
                        factorRefine = factorRefine,
                        factorName = CT_FactorLists(factorID=factorID)$factorName,
                        factorID = factorID,
                        factorType = CT_FactorLists(factorID=factorID)$factorType,
                        factorDesc = CT_FactorLists(factorID=factorID)$factorDesc
  )
  return(re)
}

#' buildFactorLists
#' 
#' get a list of \bold{FactorList}, which is offen used in "multi-factor model builbing" or in "multifactor comparison".
#' @param ... one or more FactorList. See more detail in \code{\link{buildFactorList}}.
#' @note In function \code{buildFactorLists}, the settings of factorRefine will be changed samely by the given arguments, if they are not missing. 
#' @return a list of \bold{FactorList}
#' @export
#' @examples
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.float_cap",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.PE_ttm",
#'                   factorPar=list(),
#'                   factorDir=-1)
#' )
#' # - change the factor-refining method.
#' FactorLists2 <- buildFactorLists(
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.float_cap",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.PE_ttm",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   factorRefine = refinePar_default()
#' )
buildFactorLists <- function(... , factorRefine){
  re <- list(...)
  if(!missing(factorRefine)){
    re <- lapply(re,function(x){x$factorRefine <- factorRefine;return(x)})
  }
  return(re)
}



#' @rdname buildFactorLists
#' @param factorIDs a character vector of factorID, available in table \code{CT_FactorLists()} .
#' @note function \code{buildFactorLists_lcfs} give a simply and direct way to build a \bold{list of factorlist} through "lcfs", given a vector of factorID. Here, the settings of factorRefine will be set samely by the given arguments, and the other settings will be get through "lcfs" (\code{CT_FactorLists()}). See also \code{\link{buildFactorList_lcfs}}.
#' @export
#' @examples
#' # - through lcfs
#' factorIDs <- c("F000001","F000002","F000005")
#' FactorLists3 <- buildFactorLists_lcfs(factorIDs)
buildFactorLists_lcfs <- function(factorIDs, factorRefine = refinePar_default("none")){
  re <- lapply(factorIDs, buildFactorList_lcfs, 
               factorRefine=factorRefine)
  return(re)
}




#' @rdname buildFactorList
#' @export
buildFactorList_combi <- function(factorLists, wgts, 
                                  factorDir  = 1 ,
                                  factorRefine  = refinePar_default("none"),
                                  factorName = "combi_factor",
                                  factorID ="",
                                  factorType ="",
                                  factorDesc =""
){
  re <-list(factorFun="getMultiFactor",
            factorPar=list(factorLists,wgts),  
            factorDir   = factorDir  ,
            factorRefine   = factorRefine  ,
            factorName  = factorName ,  
            factorID  = factorID,
            factorType  = factorType,
            factorDesc = factorDesc)   
  return(re)
}



setFactorListsRefinePar <- function(factorlists, factorRefine){
  re <- lapply(factorlists,function(x){x$factorRefine <- factorRefine;return(x)})
  return(re)
}



#' fl2tbl
#' 
#' turn factorlist or factorlists to 2-D table.
#' 
#' @param fl FactorList
#' @param fls FactorLists
#' @return a 2-D table with elements of list class.
#' @export fl2tbl
#' @examples 
#' fl <- buildFactorList(factorFun="gf.pct_chg_per",factorPar=list(N=20,M="test"))
#' fls <- c(buildFactorLists_lcfs(c("F000001","F000003","F000008","F000009")),list(fl))
#' re1 <- fl2tbl(fl)
#' re2 <- fls2tbl(fls)
fl2tbl <- function(fl){
  nm <- names(fl)
  re <- t(tibble::tibble(fl))
  colnames(re) <- nm
  rownames(re) <- NULL
  return(re)
}
#' @export
#' @rdname fl2tbl
fls2tbl <- function(fls){
  fname <- sapply(fls,"[[","factorName")
  names(fls) <- fname
  colname <- names(fls[[1]])
  tb <- t(tibble::as_tibble(fls))
  colnames(tb) <- colname
  return(tb)
}









#' default.factorName
#' 
#' @examples
#' factorFun = "gf.pct_chg_per"
#' default.factorName(factorFun,"2,3")
#' default.factorName(factorFun,"2,3",-1)
#' default.factorName(factorFun,list(N=60,M=30))
#' default.factorName(factorFun,"20,\"IF00\"")
#' default.factorName(factorFun,"lag=20,id=\"IF00\"")
default.factorName <- function (factorFun, factorPar, factorDir) {
  f.fun <- substring(factorFun,4)
  if(is.list(factorPar)){
    f.par <- paste(factorPar,collapse="_")
  } else if(is.character(factorPar)){
    factorPar <- gsub("\\w*=","",factorPar)
    f.par <- gsub("\'","",gsub("\"","",gsub(",","_",factorPar)))
  } else {
    stop("The factorPar must be a list or a character string!")
  }
  factorName <- if(f.par != "") paste(f.fun,f.par,sep="_") else f.fun
  f.dir <- if(missing(factorDir) || factorDir==1L) "" else "_"
  factorName <- paste(factorName,f.dir,sep="")
  return(factorName)
}


#' A demo function of factor-getting 
#' 
#' You can get different result by adjusting the param \code{rho}.
#' 
#' @param rho a numeric. the desired correlation between Periodrtn and factorscore. See \code{\link{[QUtilily]getBiCop}}
#' @return a TSF object
#' @export
#' @author Ruifei.Yin
gf_demo <- function(TS,rho=0, dure=NULL, date_end_pad){
  check.TS(TS)
  rows <- dim(TS)[1]
  rtn <- getTSR(TS, dure = dure, date_end_pad = date_end_pad)$periodrtn
  na.len <- length(rtn[is.na(rtn)])
  na.replace <- rnorm(na.len)
  rtn[is.na(rtn)] <- na.replace
  factorscore <- getBiCop(n=rows, rho=rho, x=rtn, drop.x=TRUE)  
  TSF <- cbind(TS,data.frame(factorscore=factorscore))
  return(TSF)
}




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
#' @param prdLists a list of periods
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
    message("Getting the decayed rtn ....")
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
    message(paste('Possibly dealing with a big data. The process is splited to ',length(levels(idx)),'groups.'))
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
      message(paste("Function getMultiFactor: getting the score of factor",factorName,"...."))
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
    re <- MultiFactor2CombiFactor(mTSF=re,wgts=wgts,factorNames=factorNames,rescale=TRUE,keep_single_factors="TRUE")
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
    re <- MultiFactor2CombiFactor(mTSF=re,wgts=wgts,factorNames=factorNames,rescale=TRUE,keep_single_factors="TRUE")
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
    message(paste("Function getMultiFactor: getting the score of factor",factorName,"...."))
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


#' @rdname getTSF
#' @export
#' @examples 
#' # - 3. get multifactor through lcfs
#' factorIDs <- c("F000001","F000002","F000005")
#' # -- 3.1 getMultiFactor_lcfs
#' mTSF <- getMultiFactor_lcfs(TS,factorIDs,factorRefine = refinePar_default("reg"))
#' # -- 3.2 getRawMultiFactor_lcfs
#' mTSF <- getMultiFactor_lcfs(TS,factorIDs,dir_adj=FALSE,factorRefine = NULL)
getMultiFactor_lcfs <- function(TS, FactorIDs, dir_adj=TRUE, factorRefine = NULL, wgts, fname_out=c("name","ID")){
  fname_out <- match.arg(fname_out)
  # ---- raw factor getting
  check.TS(TS)  
  tmpdat <- transform(TS[,c("stockID","date")], date = rdate2int(date))
  tmpdat$PK_ <- 1:NROW(tmpdat)
  # factor IDs splitting
  FactorIDs_a <- FactorIDs[substr(FactorIDs,1,1) == "F"]
  FactorIDs_r <- FactorIDs[substr(FactorIDs,1,1) == "R"]
  #
  re_union <- TS
  factorNames_union <- c()
  # ALPHA
  if(length(FactorIDs_a)>0){
    con <- db.local("fs")
    vars <- paste(FactorIDs_a, collapse=", ")      
    qr <- paste("select a.*,",vars,"from temp_table a left join QT_FactorScore b on a.stockID=b.ID and a.date=b.TradingDay")
    dbWriteTable(con,name="temp_table",value=tmpdat,row.names = FALSE,overwrite = TRUE)
    dbExecute(con, 'CREATE INDEX [IX_temp_table] ON [temp_table]([date],[stockID]);')
    re <- dbGetQuery(con,qr)
    re <- dplyr::arrange(re,PK_)[,FactorIDs_a,drop=FALSE]
    factorNames <- if(fname_out=="ID") FactorIDs_a else factorID2name(FactorIDs_a)
    colnames(re) <- factorNames
    dbDisconnect(con)
    #
    factorNames_union <- c(factorNames_union, factorNames)
    re_union <- cbind(re_union,re)
  }
  # RISK
  if(length(FactorIDs_r)>0){
    con <- db.local("fs_r")
    vars <- paste(FactorIDs_r, collapse=", ")      
    qr <- paste("select a.*,",vars,"from temp_table a left join QT_FactorScore_R b on a.stockID=b.ID and a.date=b.TradingDay")
    dbWriteTable(con,name="temp_table",value=tmpdat,row.names = FALSE,overwrite = TRUE)
    dbExecute(con, 'CREATE INDEX [IX_temp_table] ON [temp_table]([date],[stockID]);')
    re <- dbGetQuery(con,qr)
    re <- dplyr::arrange(re,PK_)[,FactorIDs_r,drop=FALSE]
    factorNames <- if(fname_out=="ID") FactorIDs_r else factorID2name(FactorIDs_r)
    colnames(re) <- factorNames
    dbDisconnect(con)
    #
    factorNames_union <- c(factorNames_union, factorNames)
    re_union <- cbind(re_union,re)
  }
  # ---- dir adjusting & factor refining
  factorDirs <- CT_FactorLists(FactorIDs)$factorDir
  for(i in 1:length(factorNames_union)){
    re_union <- renameCol(re_union,factorNames_union[i],"factorscore")
    if(dir_adj){
      re_union$factorscore <- re_union$factorscore*factorDirs[i]
    }
    re_union <- factor_refine(re_union,refinePar = factorRefine)
    re_union <- renameCol(re_union,"factorscore",factorNames_union[i])
  }
  # ---- get the combi-factor-score
  if(!missing(wgts)){
    re_union <- MultiFactor2CombiFactor(mTSF=re_union,wgts=wgts,factorNames=factorNames_union,keep_single_factors="TRUE")
  }
  return(re_union)
}

#' @rdname getTSF
#' @aliases getRawFactor_lcfs
#' @export
gf_lcfs <- function(TS,factorID,dir_adj=FALSE,factorRefine = NULL){
  # re <- getTech(TS,variables=factorID,tableName="QT_FactorScore",datasrc = "local")
  # re <- renameCol(re,factorID,"factorscore")
  
  re <- getMultiFactor_lcfs(TS,factorID,dir_adj=dir_adj,factorRefine = factorRefine,fname_out="ID")
  re <- renameCol(re,factorID,"factorscore")
  return(re)
}

#' @rdname getMultiFactor
#' @param mTSF a dataframe of 'TS & MultiFactors'
#' @param keep_single_factors logical. if keep the single factors in the result?
#' @param rescale logic. if rescale the combifactor to mean 0 and std 1.
#' @export
MultiFactor2CombiFactor <- function(mTSF,
                                    wgts,
                                    factorNames = guess_factorNames(mTSF),
                                    na_deal=c("ignore","rm"),
                                    rescale=TRUE,
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
    
    wgt_rowsum <- rowSums(wgts_mat)
    wgt_rowsum[wgt_rowsum==0] <- NA	# if all the factorscores are NA, the sumScore would be NA as well.
    wgts_mat <- wgts_mat/kronecker(matrix(1,1,M),wgt_rowsum)
    
    TSF_mat[is_na] <- 0
    sumScore <- rowSums(TSF_mat*wgts_mat)
  } else if(na_deal=="rm"){ # if any of the factorscores is NA, the sumScore would be NA.
    sumScore <- TSF_mat %*% wgts
  }
  
  if(keep_single_factors){
    re <- cbind(mTSF,factorscore=sumScore)
  } else {
    re <- cbind(dplyr::select(mTSF,-one_of(factorNames)),factorscore=sumScore)
  }
  if(rescale){
    re <- factor_std(re,method = "scale",sectorAttr = NULL)
  }
  return(re)
}


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  TSF manufacture & transformation ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



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
  # if("factorscore" %in% all_cols){
  #   stop("There should not be 'factorscore' in the cols of mTSF! Please try to rename it.")
  # }
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





#' TSF2TSFs_byfactor
#'
#' split a TSF or a TSFR into TSFs or TSFRs by factor,such as size,vol...
#' @author Andrew Dow
#' @param ftype support size,trading volume,trading amount,industry,important index's component(000300.SH,000905.SH,000852.SH) and date.
#' @param N split into N group,default value is 5.
#' @param includeTSF whether add raw \code{TSF} to \code{TSFs} list,default value is \code{TRUE}.
#' @examples
#' #####split by size into 3 group####
#' TSFs <- TSF2TSFs_byfactor(tsf,N=3)
#'
#' #####split by itself into 3 group####
#' TSFs <- TSF2TSFs_byfactor(tsf,ftype='self',N=3)
#'
#' #####split by trading volumn of last 20 days ####
#' TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='vol')
#'
#' #####split by sector ####
#' TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='sector')
#' TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='sector',sectorAttr=defaultSectorAttr('fct')) #self defined sector
#'
#' #####split by date ####
#' TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='date',freq='2 years')
#'
#' @export
TSF2TSFs_byfactor <- function(TSF,ftype=c('size','self','vol','amt','sector','indexcomp','date','findate'),N=5,freq='year',
                              includeTSF=TRUE,sectorAttr=defaultSectorAttr()){
  ftype <- match.arg(ftype)
  
  if(ftype=='size'){
    tsf <- gf_cap(TSF[,c('date','stockID')],var = 'mkt_cap')
  }else if(ftype=='self'){
    tsf <- TSF[,c('date','stockID','factorscore')]
  }else if(ftype=='vol'){
    tsf <- TS.getTech_ts(TSF[,c('date','stockID')],"StockVolSum2(20)",'factorscore')
  }else if(ftype=='amt'){
    tsf <- TS.getTech_ts(TSF[,c('date','stockID')],"StockAmountSum2(20)",'factorscore')
  }else if(ftype=='sector'){
    tsf <- getSectorID(TSF[,c('date','stockID')],sectorAttr=sectorAttr,fillNA = TRUE)
    if(is.numeric(sectorAttr$std)){
      tsf$sector <- sectorID2name(tsf$sector)
    }
    colnames(tsf) <- c('date','stockID','group')
  }else if(ftype=='indexcomp'){
    suppressWarnings(tsf <- transform(TSF[,c('date','stockID')],hs300=is_component(TSF[,c('date','stockID')],sectorID = "EI000300",drop = TRUE),
                                      zz500=is_component(TSF[,c('date','stockID')],sectorID = "EI000905",drop = TRUE),
                                      zz1000=is_component(TSF[,c('date','stockID')],sectorID = "EI000852",drop = TRUE),
                                      other=0))
    tsf <- transform(tsf,num=hs300*1000+zz500*100+zz1000*10+other)
    tsf <- transform(tsf,group=ifelse(num>=1000,'hs300',
                                      ifelse(num>=100,'zz500',
                                             ifelse(num>=10,'zz1000','other'))))
    tsf <- tsf[,c('date','stockID','group')]
  }else if(ftype=='date'){
    tsf <- TSF[,c('date','stockID')]
    tsf$group <- cut.Date2(tsf$date,freq)
    tsf$group <- rdate2int(as.Date(tsf$group))
  }else if(ftype=='findate'){
    tsf <- TSF[,c('date','stockID')]
    tsf <- transform(tsf,group=ifelse(lubridate::month(date) %in% c(1,2,3),'mon2_4',
                                      ifelse(lubridate::month(date) %in% c(4,5,6),'mon5_7',
                                             ifelse(lubridate::month(date) %in% c(7,8,9),'mon8_10','mon11_1'))))
    tsf$group <- as.character(tsf$group)
  }
  
  if(any(is.na(tsf$factorscore))){
    tsf <- factor_na(tsf,method = 'median',sectorAttr = NULL)
  }
  
  if(!(ftype %in% c('sector','indexcomp','date','findate'))){
    tsf <- data.table::as.data.table(tsf)
    tsf <- tsf[order(date,-factorscore)][
      ,`:=`(ran=rank(-factorscore),len=.N),by=date][
        ,group:=floor((ran-1)*N/len+1)][
          ,`:=`(ran=NULL,len=NULL,factorscore=NULL,group=paste('G',group,sep=''))]
  }
  tsfr <- data.table::as.data.table(TSF)
  mTSF <- merge.x(tsfr,tsf)
  
  TSFs <- split(mTSF,f=mTSF$group)
  TSFs <- lapply(TSFs, function(x) { x["group"] <- NULL; x })
  if(includeTSF){
    TSFs[['Gall']] <- TSF
  }
  return(TSFs)
}



#' TS_split
#' 
#' split TS to lists, which are usually TSFs, TSFRs
#' @param by a vector, with the same length of TS
#' @return a list is TS
#' @export
#' @examples 
#' tsfr <- getSectorID(tsfr,sectorAttr = defaultSectorAttr("ind",336))
#' TSFRs1 <- TS_split(tsfr,by=tsfr$sector)
#' MC.chart.IC(tsfrs1)
#' TSFRs2 <- TS_split(tsfr,by=cut.Date2(tsfr$date,breaks = "2 year"))
#' MC.chart.IC(tsfrs2)
TS_split <- function(TS,by){
  re <- data.table::as.data.table(TS)
  re <- split(re,f=by)
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


#' TSF_nextF
#'
#' \code{TSF_nextF} turn \code{TSF} into \code{TSFR} by lead date and factorscore one period.
#' @return a \bold{TSFR}, in which the 'periodrtn' is the 'next_factorscore'.
#' @export
#' @examples
#' tsfr <- TSF_nextF(tsf)
#' # --- deal with the 'rptTSF'
#' rptTS <- getrptTS(as.Date("2011-12-31"),as.Date("2015-12-31"),freq = "q",univ = "EI000300")
#' rptTSF <- rptTS.getFin_windR(rptTS ,field = "np_belongto_parcomsh",varname = "factorscore")
#' TSF <- renameCol(rptTSF,"rptDate","date")
#' TSFR <- TSF_nextF(TSF)
#' # --- analysing
#' chart.IC(TSFR)
#' chart.Ngroup.overall(TSFR)
TSF_nextF <- function(TSF){
  TSF <- TSF[,c("date","stockID","factorscore")]
  
  datedf <- data.frame(date=unique(TSF$date))
  datedf <- datedf %>% dplyr::mutate(date_end=dplyr::lead(date)) %>%
    dplyr::filter(!is.na(date_end))
  
  TSF2 <- TSF %>% dplyr::select(date,stockID) %>%
    dplyr::inner_join(datedf,by='date')
  TSF2 <- TSF2 %>% dplyr::left_join(TSF,by=c("date_end"="date","stockID")) %>%
    dplyr::rename(periodrtn=factorscore)
  
  TSF <- TSF %>% dplyr::left_join(TSF2,by=c('date','stockID')) %>%
    as.data.frame()
  return(TSF)
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
    TSF <- renameCol(TSF, fname, ".tmp_column")
    mdata <- dplyr::group_by(TSF, date)
    subfun <- function(TSF){
      lambda <- (car::powerTransform(TSF$.tmp_column))$lambda
      TSF$lambda <- lambda
      TSF$mazi_tmp_column <- car::bcPower(TSF$.tmp_column, lambda)
      return(TSF)
    }
    mdata <- dplyr::do(mdata, subfun(.))
    mdata <- as.data.frame(mdata)
    mdata <- renameCol(mdata, ".tmp_column", fname)
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
                            sectorAttr=NULL) {
  method <- match.arg(method)
  # direct return
  if(method == "none"){
    return(TSF)
  }
  # shrink columns (merge back later)
  TSF_core <- TSF[,intersect(names(TSF), c("date","stockID","factorscore","sector"))]
  # sector splitting
  if(!is.null(sectorAttr)){
    TSF_core <- getSectorID(TS = TSF_core, sectorAttr = sectorAttr, fillNA = TRUE)
    key=c("date","sector")
  }else{
    key=c("date")
  }
  TSF_core <- data.table::as.data.table(TSF_core,key=key)
  # sub fun
  outlier_sub_fun <- function(sf, method, par){
    if(method == "sd"){
      outlier_u <- mean(sf, na.rm = TRUE) + par * sd(sf, na.rm = TRUE)
      outlier_l <- mean(sf, na.rm = TRUE) - par * sd(sf, na.rm = TRUE)
    }else if(method == "mad"){
      outlier_u <- median(sf, na.rm = TRUE) + par * mad(sf, na.rm = TRUE)
      outlier_l <- median(sf, na.rm = TRUE) - par * mad(sf, na.rm = TRUE)
    }else if(method == "boxplot"){
      boxplot_stat <- boxplot.stats(sf, coef = par)[[1]]
      outlier_u <- max(boxplot_stat)
      outlier_l <- min(boxplot_stat)
    }else if(method == "percentage"){
      outlier_u <- quantile(sf, probs = 1-par/100, na.rm = TRUE)
      outlier_l <- quantile(sf, probs = 0+par/100, na.rm = TRUE)
    }
    sf <- ifelse(sf > outlier_u, outlier_u,
                 ifelse(sf < outlier_l, outlier_l, sf))
    sf <- as.numeric(sf)
    return(sf)
  }
  # processing
  TSF_core <- TSF_core[,factorscore:=outlier_sub_fun(factorscore,method,par),by=key]
  # merge back
  TSF <- TSF[,setdiff(colnames(TSF), "factorscore")]
  TSF <- merge.x(TSF, TSF_core, by = c("date","stockID"))
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
#' @param regWgt NULL,"existing","sector","cap"
#' @export
#' @author Han.Qian
#' @examples 
#' RebDates <- getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17'),'month')
#' TS <- getTS(RebDates,'EI000300')
#' TSF <- gf.NP_YOY(TS)
#' regLists <- buildFactorLists(buildFactorList(factorFun = 'gf.PE_ttm',factorDir = -1,factorRefine=refinePar_default("scale")))
#' TSF <- factor_std(TSF, method = "reg", log = FALSE, regLists = regLists)
factor_std <- function(TSF,method=c("none","scale","robustScale","reg","scale_barra_simple","scale_barra_sec"),
                       log=FALSE, 
                       sectorAttr=NULL,
                       regLists=NULL,
                       regWgt=NULL
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
    TSF_core <- TSF[,intersect(names(TSF), c("date","stockID","Y_for_reg","sector","glm_wgt"))]
    
    # get glm_wgt
    if(!is.null(regWgt)){
      if(regWgt == "sector"){
        TSF_core <- getSectorID(TSF_core, sectorAttr = defaultSectorAttr("ind",33), fillNA = TRUE) # to be modified!
        glm_wgt_data <- dplyr::group_by(TSF_core, date, sector)
        glm_wgt_data <- dplyr::summarise(glm_wgt_data, glm_wgt = 1/var(Y_for_reg, na.rm = TRUE))
        TSF_core <- merge.x(TSF_core, glm_wgt_data, by = c("date","sector"))
        TSF_core <- as.data.frame(TSF_core)
      }else if(regWgt == "cap"){ # to be modified!
        TSF_core <- getSectorID(TSF_core, sectorAttr = defaultSectorAttr("fct",fct_std = list(fl_cap(var = "float_cap")),fct_level = 20), fillNA = TRUE) 
        glm_wgt_data <- dplyr::group_by(TSF_core, date, sector)
        glm_wgt_data <- dplyr::summarise(glm_wgt_data, glm_wgt = 1/var(Y_for_reg, na.rm = TRUE))
        TSF_core <- merge.x(TSF_core, glm_wgt_data, by = c("date","sector"))
        TSF_core <- as.data.frame(TSF_core)
      }else{
        stop("The glm_wgt could not be identified.")
      }
    }
    
    # glm_wgt checking
    if(!is.null(regWgt)){
      if(any(is.infinite(TSF_core$glm_wgt))){
        TSF_core$glm_wgt[is.infinite(TSF_core$glm_wgt)] <- NA
      }
      if(any(is.na(TSF_core$glm_wgt))){
        TSF_core <- dplyr::group_by(TSF_core, date)
        TSF_core <- dplyr::mutate(TSF_core,
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
    if(is.null(regWgt)){
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
    # if(is.null(regWgt)){ # by date*sector
    #   TSF <- factor_std(TSF, method = "robustScale", sectorAttr = sectorAttr)
    # } else { # by date
    #   TSF <- factor_std(TSF, method = "robustScale", sectorAttr = NULL)
    # }
    TSF <- factor_std(TSF, method = "robustScale", sectorAttr = sectorAttr)
    
    # output
    return(TSF)
    
  }else{ # scale part
    
    # shrink columns (merge back later)
    TSF_core <- TSF[,intersect(names(TSF), c("date","stockID","factorscore","sector"))]
    # sector splitting
    if(!is.null(sectorAttr)){
      TSF_core <- getSectorID(TS = TSF_core, sectorAttr = sectorAttr, fillNA = TRUE, ungroup=10)
      key=c("date","sector")
    }else{
      key=c("date")
    }
    
    
    
    
    if(method == "scale_barra_sec"){
      TSF_core <- gf_cap(TSF_core, var = "free_cap", varname = "ffv")
      TSF_core <- data.table::as.data.table(TSF_core)
      TSF_core <- TSF_core[, sd := sd(factorscore, na.rm = TRUE), by = date]
      TSF_core <- TSF_core[, miu := sum(ffv * factorscore, na.rm = TRUE)/sum(ffv, na.rm = TRUE), 
                           by = .(date, sector)]
      TSF_core <- TSF_core[, factorscore := (factorscore - miu)/sd, 
                           by = .(date, sector)]
      TSF_core <- TSF_core[,sd := NULL,]
      TSF_core <- TSF_core[,miu := NULL,]
      TSF_core <- as.data.frame(TSF_core)
    }else if(method == "scale_barra_simple"){
      TSF_core <- gf_cap(TSF_core, var = "free_cap", varname = "ffv")
      TSF_core <- data.table::as.data.table(TSF_core)
      TSF_core <- TSF_core[, sd := sd(factorscore, na.rm = TRUE), by = date]
      TSF_core <- TSF_core[, miu := sum(ffv * factorscore, na.rm = TRUE)/sum(ffv, na.rm = TRUE), by = date]
      TSF_core <- TSF_core[, factorscore := (factorscore - miu)/sd, by = date]
      TSF_core <- TSF_core[,sd := NULL,]
      TSF_core <- TSF_core[,miu := NULL,]
      TSF_core <- as.data.frame(TSF_core)
    } else {
      
      
      
      
      
      
      
      TSF_core <- data.table::as.data.table(TSF_core,key=key)
      # sub fun
      std_sub_fun <- function(sf, method){
        if(method == "scale"){
          sf <- as.numeric(scale(sf))
        }else if(method == "robustScale"){
          median_ <- median(sf, na.rm = TRUE)
          sd_ <- sd(sf, na.rm = TRUE)
          if(is.na(sd_) | sd_ == 0){
            sf <- as.numeric(NA)
          }else{
            sf <- (sf - median_)/sd_
          }
        }
        return(sf)
      }
      # processing 
      TSF_core <- TSF_core[,factorscore:=std_sub_fun(factorscore,method),by=key]
      
    }
    
    
    
    
    # merge back
    TSF <- TSF[,setdiff(colnames(TSF), "factorscore")]
    TSF <- merge.x(TSF, TSF_core, by = c("date","stockID"))
    
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
                       sectorAttr=NULL) {
  method <- match.arg(method)
  # direct return
  if(method == "none"){
    return(TSF)
  }
  # shrink columns (merge back later)
  TSF_core <- TSF[,intersect(names(TSF), c("date","stockID","factorscore","sector"))]
  # sector splitting
  if(!is.null(sectorAttr)){
    TSF_core <- getSectorID(TS = TSF_core, sectorAttr = sectorAttr, fillNA = TRUE)
    key=c("date","sector")
  }else{
    key=c("date")
  }
  TSF_core <- data.table::as.data.table(TSF_core,key=key)
  # sub fun
  na_sub_fun <- function(sf, method){
    # browser()
    if(method == "mean"){
      ind_na_ <- is.na(sf)
      mean_ <- mean(sf, na.rm = TRUE)
      sf[ind_na_] <- mean_
    }else if(method == "median"){
      ind_na_ <- is.na(sf)
      median_ <- median(sf, na.rm = TRUE)
      sf[ind_na_] <- median_
    }
    return(sf)
  }
  # batch processing
  TSF_core <- TSF_core[,factorscore:= na_sub_fun(factorscore,method),by=key]
  if(sum(is.na(TSF_core$factorscore))>0){ # If sector is too small, ungroup it.
    TSF_core <- TSF_core[,factorscore:= na_sub_fun(factorscore,method),by=c("date")]
  }
  # merge back
  TSF <- TSF[,setdiff(colnames(TSF), "factorscore")]
  TSF <- merge.x(TSF, TSF_core, by = c("date","stockID"))
  # remove sector column if its not from the input
  if(!identical(sectorAttr, "existing") & !is.null(sectorAttr)){
    TSF <- TSF[,setdiff(colnames(TSF), "sector")]
  }
  return(TSF)
}



#' refinePar_default
#' 
#' @param type Could be "none","scale","scale_sec","reg","reg_glm_sec","reg_glm_cap"
#' @param sectorAttr could be a sectorAttr list, or NULL(means no grouping by sector).
#' @param log TRUE or FALSE
#' @param regLists a \bold{factorLists}
#' @export
#' @author Han.Qian
refinePar_default <- function(type=c("none","scale","scale_sec","reg","reg_glm_sec","reg_glm_cap"), 
                              sectorAttr=defaultSectorAttr("ind"),
                              log=FALSE,
                              regLists=list(fl_cap(log = TRUE,var="float_cap",datasrc = "memory"))
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
                        sectorAttr=NULL,
                        regLists=NULL),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
    )
  }else if(type=="scale_sec"){
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
                        regWgt=NULL),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
    )
  }else if(type=="reg_glm_sec"){
    re <- list(outlier=list(method = "boxplot",
                            par=1.5,
                            sectorAttr= NULL),
               std=list(method = "reg",
                        log=log, 
                        sectorAttr=sectorAttr,
                        regLists=regLists,
                        regWgt="sector"),
               na=list(method = "median", 
                       sectorAttr=sectorAttr)
               )
  }else if(type=="reg_glm_cap"){
    re <- list(outlier=list(method = "boxplot",
                            par=1.5,
                            sectorAttr= NULL),
               std=list(method = "reg",
                        log=log, 
                        sectorAttr=sectorAttr,
                        regLists=regLists,
                        regWgt="cap"),
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
                         std_regWgt,
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
  if(!missing(std_regWgt)){
    refinePar$std$regWgt <- std_regWgt
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
                    regWgt = refinePar$std$regWgt)
  
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
#' refinePar_lists <- list(refinePar_default(type = "none"),
#'                         refinePar_default(type = "reg"),
#'                         refinePar_default(type = "scale"))
#' mTSFR <- factor_refine_MF(rawTSFR, refinePar_lists)
#' MF.chart.Ngroup.spread(mTSFR)
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


#' @rdname factor_refine
#' @export
MF.factor_refine <- function(mTSF,refinePar=refinePar_default(),drop_sector=TRUE){
  TSFs <- mTSF2TSFs(mTSF)
  TSFs_refine <- plyr::llply(TSFs,factor_refine,refinePar=refinePar,drop_sector=drop_sector)
  mTSF_refine <- TSFs2mTSF(TSFs_refine)
  return(mTSF_refine)
}








# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  factor orthogan functions ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' @title factor orthogan
#' @name factor_orthogan_name
#' @rdname factor_orthogan
#' @export
factor_orthogon_single <- function(TSF,y,x,sectorAttr=defaultSectorAttr(),regType=c('lm','glm'),res_adjust=FALSE){
  regType <- match.arg(regType)
  cols <- colnames(TSF)
  fname <- guess_factorNames(TSF,no_factorname="glm_wgt",is_factorname = "factorscore",silence=TRUE)
  
  if(length(fname)==1 && is.null(sectorAttr)){
    stop('NO x variable!')
  }
  if(!(y %in% fname)){
    stop('y not in TSF!')
  }
  if(missing(x)){
    x <- setdiff(fname,y)
  }
  if(!is.null(sectorAttr)){
    TSF <- getSectorID(TSF,sectorAttr = sectorAttr,fillNA = TRUE)
  }
  
  if(is.null(sectorAttr)){
    resd <- lm_NPeriod(TSF,y,x,lmtype=regType,res_adjust = res_adjust)
  }else{
    resd <- lm_NPeriod(TSF,y,x,secIN = TRUE,lmtype=regType,res_adjust = res_adjust)
  }
  
  resd <- resd$resd
  re <- TSF[,cols]
  re[,y] <- resd$res
  return(re)
}



#' @rdname factor_orthogan 
#' @export
#' @examples 
#' tb_symm <- factor_orthogon(mtsf,method = "symm")
#' tb_reg <- factor_orthogon(mtsf,method = "reg")
#' MF.chart.Fct_corr(mtsf)
#' MF.chart.Fct_corr(tb_symm)
#' MF.chart.Fct_corr(tb_reg)
factor_orthogon <- function(TSF, method = c("symm","reg"),
                            forder, sectorAttr=NULL, regType=c('lm','glm')){
  method <- match.arg(method)
  
  if(method=="reg"){
    regType <- match.arg(regType)
    cols <- colnames(TSF)
    fname <- guess_factorNames(TSF,is_factorname = NULL,silence=TRUE)
    if(missing(forder)){
      forder <- fname
    }
    if(is.numeric(forder)){
      forder <- fname[forder]
    }
    if(!is.null(sectorAttr)){
      TSF <- getSectorID(TSF,sectorAttr = sectorAttr,fillNA = TRUE)
    }
    sectorAttr_ <- if(is.null(sectorAttr)) NULL else "existing"
    if(!is.null(sectorAttr)){ # forder[1]
      TSF <- factor_orthogon_single(TSF, y = forder[1], x=NULL,sectorAttr = "existing",regType=regType)
    }
    for(j in 2:length(forder)){ # forder[2:length]
      TSF <- factor_orthogon_single(TSF, y = forder[j], x=forder[1:(j-1)],sectorAttr = sectorAttr_,regType=regType)
    }
    return(TSF[,cols])
    
  } else if(method == "symm"){
    # one period func
    subfun <- function(TSF, method = method, fnames, rest_cols){
      F_mat <- as.matrix(TSF[,fnames])
      D_sd_mat <- diag(sqrt(diag(cov(F_mat))))
      
      N <- nrow(F_mat)
      M_mat <- t(F_mat) %*% F_mat / N
      decomposed_result <- eigen(M_mat, symmetric = TRUE)
      
      U_mat <- decomposed_result$vectors
      D_mat <- diag(decomposed_result$values)
      D_mat_star <- diag((decomposed_result$values)^(-1/2))
      
      S_mat <- U_mat %*% D_mat_star %*% t(U_mat) %*% D_sd_mat
      
      # output : F_new
      F_mat_new <- F_mat %*% S_mat
      F_mat_new <- as.data.frame(F_mat_new)
      colnames(F_mat_new) <- fnames
      TSF_new <- cbind(TSF[,rest_cols], F_mat_new)
      return(TSF_new)
    }
    # processing
    fnames <- guess_factorNames(TSF,is_factorname = NULL,silence=TRUE)
    fnames <- subset(fnames, substr(fnames, 1, 2) != "ES")
    rest_cols <- setdiff(colnames(TSF), fnames)
    TSF <- dplyr::group_by(TSF, date)
    result <- dplyr::do(.data = TSF, subfun(., method = method, fnames = fnames, rest_cols = rest_cols))
    # output
    result <- result[,colnames(TSF)]
    result <- as.data.frame(result)
    return(result)
  }
}
#' @rdname factor_orthogan 
#' @export
#' @examples 
#' factor_orthogon_transMat(mtsf)
factor_orthogon_transMat <- function(TSF){
  # one period func
  subfun <- function(TSF, method = method, fnames, rest_cols){
    F_mat <- as.matrix(TSF[,fnames])
    D_sd_mat <- diag(sqrt(diag(cov(F_mat))))
    
    N <- nrow(F_mat)
    M_mat <- t(F_mat) %*% F_mat / N
    decomposed_result <- eigen(M_mat, symmetric = TRUE)
    
    U_mat <- decomposed_result$vectors
    D_mat <- diag(decomposed_result$values)
    D_mat_star <- diag((decomposed_result$values)^(-1/2))
    
    S_mat <- U_mat %*% D_mat_star %*% t(U_mat) %*% D_sd_mat
    
    # output : S_mat
    S_mat <- as.data.frame(S_mat)
    colnames(S_mat) <- fnames
    S_mat$sum <- rowSums(S_mat)
    S_mat$fname <- fnames
    return(S_mat)
  }
  # processing
  fnames <- guess_factorNames(TSF,is_factorname = NULL,silence=TRUE)
  fnames <- subset(fnames, substr(fnames, 1, 2) != "ES")
  rest_cols <- setdiff(colnames(TSF), fnames)
  TSF <- dplyr::group_by(TSF, date)
  result <- dplyr::do(.data = TSF, subfun(., method = method, fnames = fnames, rest_cols = rest_cols))
  # output
  result <- as.data.frame(result)
  return(result)
}





# if lmtpye=='glm', data must include 'glm_wgt' column.
#' lm_NPeriod
#' @export
lm_NPeriod <- function(data,y,x,lmtype=c('lm','glm'),secIN=FALSE,res_adjust=FALSE,silence=TRUE){
  check.colnames(data,c('date','stockID'))
  lmtype <- match.arg(lmtype)
  
  TS <- data[,c('date','stockID')]
  data <- data[rowSums(is.na(data[,c(x,y),drop=FALSE]))==0,] # remove NA
  if(!silence && nrow(data)<nrow(TS)){
    warning("NAs found in x or y part!")
  }
  
  if(secIN){
    data$sector <- as.factor(data$sector)
    fml <- formula(paste(y," ~ ", paste(c(x,"sector"), collapse= "+"),"-1",sep=''))
  }else{
    fml <- formula(paste(y," ~ ", paste(x, collapse= "+"),sep=''))
  }
  
  if(lmtype=='lm'){
    models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = . ,na.action = "na.exclude"))
  }else{
    check.colnames(data,"glm_wgt")
    models <- data %>% dplyr::group_by(date) %>% dplyr::do(mod = lm(fml, data = .,weights=glm_wgt ,na.action = "na.exclude"))
  }
  
  rsq <- dplyr::summarise(models,date=date,rsq = summary(mod)$r.squared)
  coef <- models %>% broom::tidy(mod)
  resd <- models %>% broom::augment(mod)
  if(lmtype == "glm" & res_adjust){
    resd$.resid <- sqrt(resd$X.weights.) * resd$.resid
  }
  resd <- cbind(data[,c('date','stockID')],resd[,c('.fitted','.resid')])
  colnames(resd) <- c('date','stockID','fitted','res')
  resd <- merge.x(TS,resd,by=c('date','stockID'))
  
  rsq <- as.data.frame(rsq)
  coef <- as.data.frame(coef)
  resd <- as.data.frame(resd)
  return(list(rsq=rsq,coef=coef,resd=resd))
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  TS removing functions ------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
#' remove suspension stock from TS
#'
#' @param nearby a integer vector. default is 1L, which means remove the suspending of the next tradingday. see detail in \code{\link{trday.nearby}}.
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
#' @param nearby a integer vector. default is 1L, which means remove the price_limited stocks of the next tradingday.
#' @param lim a vector of length 2.
#' @param priceType "close" or "open".
#' @examples
#' RebDates <- getRebDates(as.Date('2013-03-17'),as.Date('2016-04-17'),'month')
#' TS <- getTS(RebDates,'EI000985')
#' re <- rm_priceLimit(TS)
#' re1 <- rm_priceLimit(TS,nearby=-1:1)
#' re2 <- rm_priceLimit(TS,lim=c(-Inf,10)) # remove limit-up
#' @export
rm_priceLimit <- function(TS,nearby=1L,lim=c(-10, 10),priceType=c("close","open"),
                          datasrc=defaultDataSRC()){
  TS_ <- is_priceLimit(TS=TS,nearby = nearby,lim = lim, datasrc = datasrc)
  TS <- TS[!TS_$overlim,]
  return(TS)
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
    RSQLite::dbWriteTable(con,"temp_table",TS_,overwrite=TRUE,row.names=FALSE)
    qr <- "select y.*,q.SecuAbbr 'is_st' from temp_table y left join QT_DailyQuote q on y.date=q.TradingDay and y.stockID=q.ID"
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



#' is_delist
#'
#' judeg whether stocks to be delisted
#' @export
is_delist <- function(TS,nearby=months(2),datelist,stockID, drop,datasrc='jy'){
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
  if(datasrc=='jy'){
    stocks <- substr(unique(TS$stockID),3,8)
    
    #get delist date
    qr <- paste("SELECT 'EQ'+s.SecuCode 'stockID',convert(varchar,ChangeDate,112) 'delistdate'
                FROM LC_ListStatus l
                INNER join SecuMain s on l.InnerCode=s.InnerCode and s.SecuCategory=1
                where l.ChangeType=4 and l.SecuMarket in (83,90)
                and s.SecuCode in",brkQT(stocks))
    re <- queryAndClose.odbc(db.jy(),qr,as.is = TRUE)
    if(nrow(re)>0){
      re <- transform(re,delistdate=intdate2r(delistdate))
      TS <- TS %>% dplyr::left_join(re,by='stockID') %>%
        dplyr::mutate(tmpdate=trday.offset(date,nearby),delist=ifelse(is.na(delistdate),FALSE,delistdate<=tmpdate)) %>%
        dplyr::select(-tmpdate,-delistdate)
    }
  }
  
  if(drop){
    return(TS$delist)
  }else{
    return(TS)
  }
}

