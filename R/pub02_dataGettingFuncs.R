


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================
# ====================  Tinysoft related utility functions ====================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================
#' tsInclude
#'
#' Series of functions of connect/login Tinysoft server, and remote call funcions of Tinysoft.
#' @param funchar a charactor string of tinysoft script
#' @param pars a list of funchar's parametres
#' @param syspars a list of tinysoft system parametres.(including:StockID CurrentDate Cycle bRate RateDay Precision)
#' @return tsInclude: include and load the external dll;
#' @return tsRequre: test if tinysoft connected, if not, connect it.
#' @author ruifei.yin
#' @export
#' @examples
#' tsInclude()
#' tsConnect()
#' tsLogined()
#' tsRemoteExecute("return close();",list(StockID="SZ000002",CurrentDate=rdate2ts(as.Date("2014-11-11"))))
#' # use ldply/laply/llply to transform the result from list to comfortable form
#' ll <- tsRemoteExecute("return Array(('a':1,'b':4),('a':2,'b':5),('a':3,'b':6));")
#' plyr::ldply(ll,as.data.frame)
#' plyr::laply(ll,as.array)
#' plyr::llply(ll,unlist)
#' tsRemoteCallFunc("close",,list(StockID="SZ000002"))
#' tsRemoteCallFunc("rand",list(2,3))
#' tsDisconnect()
tsInclude <- function(os = R.Version()$arch){
  source(paste(.libPaths()[1],"/QDataGet/tslr/tslr.R",sep = ""))
  tsLoad(os)
}

#' @rdname tsInclude
#' @export
tsLoad <- function(os = R.Version()$arch){
  libpath <- .libPaths()[1]
  if(os == "i386"){
    dyn.load(paste(libpath,"/QDataGet/tslr/i386/tslr.dll",sep = ""))
  }else if(os == "x86_64"){
    dyn.load(paste(libpath,"/QDataGet/tslr/x64/tslr.dll",sep = ""))
  }
}

#' @export
#' @rdname tsInclude
tsRequire <- function(){
  if(!exists("tsLogined")){
    tsInclude()
  }
  if(!is.loaded("tslConnectServer")){
    tsLoad()
  }
  if(tsLogined()==0){
    tsConnect()
  }
}

#' rdate2ts
#'
#' transform the R date to tinysoft date value
#' @param rdate a vector with \bold{Date} class
#' @param GAP.R2TS the constant of R to tinysoft date-transformating 
#' @return a vector containing the tinysoft date value transformed from the R date
#' @seealso \code{\link{tsdate2r}}
#' @author Ruifei.Yin
#' @export
rdate2ts <- function(rdate, GAP.R2TS=25569){
  tsdate <- rdate + GAP.R2TS
  tsdate <- as.numeric(tsdate)
  return(tsdate)
}

#' tsdate2r
#'
#' transform the tinysoft date value to R date
#' @param tsdate a vector containing the tinysoft date value
#' @param GAP.R2TS the constant of R to tinysoft date-transformating 
#' @return a vector with \bold{Date} class, transformed from the tinysoft date value
#' @seealso \code{\link{rdate2ts}}
#' @author Ruifei.Yin
#' @export
tsdate2r <- function(tsdate, GAP.R2TS=25569){
  rdate <- tsdate - GAP.R2TS
  rdate <- as.Date(rdate,origin="1970-01-01")
  return(rdate)
}








#' ts.wss
#' 
#' realize the "stock-data-expert" functions in Tinysoft.Given the stocklist, get specific variables of the stocks from Tinysoft.
#' @param stocks a vector of stockID.
#' @param funchar expression to get variables from tinysoft,a character string, usually copyed from tinysoft "stock-data-expert". If you want to specify the rptDate by param \code{rptDate}, the expression should be converted simply by replaceing the specified reportdate in the stock-data-expert expression by \code{'Rdate'}. e.g. convert \code{Last12MData(20091231,46002)} to \code{Last12MData(Rdate,46002)}.
#' @param varname vector of charactor string
#' @param rptDate a specified rptDate, with class of Date. Could be missing when unnessasry. See examples for more detail.
#' @param Time a Date object, giving the pn_date() in tinysoft
#' @param Rate a integer,giving the type of rights adjustment, could be one of 0(no adjustment),1(geometric adjustment),2(simple adjustment),3
#' @param RateDay a integer,giving the base date of right adjustment,could be one of 0(the last trading day),-1(the IPO date),or a tinysoft date integer(eg. \code{rdate2ts(as.Date("2010-01-02"))})
#' @param adjust_yoy a logic.  If TRUE, param \code{Time} will be set to \code{rptDate.deadline(rptDate)}, the financial index before(not including) \code{rptDate} will be adjusted, the financial index after \code{rptDate} will not be adjusted. 
#' @return a dataframe,with cols:stockID,stockName,and the returned variables.
#' @note you can get different financial index by set param Time and param adjust_yoy.
#' If you set param \code{Time} to 1900-01-01, all the financial index returned will not be adjusted; If you set param \code{Time} to \code{Sys.Date()}, all the financial index returned will be adjusted; See example for detail.
#' @export
#' @family stockDataExpert functions
#' @examples 
#' stocks <- c("EQ600011","EQ000631","EQ000004")
#' funchar1 <- '"eps",reportofall(9900000,20121231),
#'    "zyywlrzzl",reportofall(9900601,20121231),
#'    "yszk",report(44009,20121231),
#'    "isFinanceCompany",IsFCompany_(),
#'    "close",close(),
#'    "rtn(%)(19890705,20120705)",StockZf(32694,41095),
#'    "Ndayrtn(%)(N=10)",StockZf2(10),
#'    "floatMV(20110926)",StockMarketValue(40812)'
#' re1 <- ts.wss(stocks,funchar1)
#' 
#' # -- specify the rptDate
#' funchar2 <- '"eps",reportofall(9900000,Rdate),
#'    "zyywlrzzl",reportofall(9900601,Rdate),
#'    "yszk",report(44009,Rdate),
#'    "isFinanceCompany",IsFCompany_(),
#'    "close",close(),
#'    "rtn(%)(19890705,20120705)",StockZf(32694,41095),
#'    "Ndayrtn(%)(N=10)",StockZf2(10),
#'    "floatMV(20110926)",StockMarketValue(40812)'
#' re2 <- ts.wss(stocks,funchar2,rptDate=as.Date('2012-12-31'))
#' 
#' all.equal(re1,re2)  # TRUE
#' 
#' # -- getting financial index (adjust or not)
#' ts.wss("EQ000027",'"G_NP_Q",LastQuarterData(Rdate,9900604,0)',as.Date("2007-03-31"),Time=as.Date("1900-06-29")) # 31.8 (07_adj/06_adj-1)
#' ts.wss("EQ000027",'"G_NP_Q",LastQuarterData(Rdate,9900604,0)',as.Date("2007-03-31"),Time=Sys.Date()) # 229.16 (07_unadj/06_unadj-1)
#' ts.wss("EQ000027",'"G_NP_Q",LastQuarterData(Rdate,9900604,0)',as.Date("2007-03-31"),adjust_yoy=TRUE) # 49 (07_unadj/06_adj-1) This is the correct one!
ts.wss <- function(stocks,funchar,varname,rptDate,Time=Sys.Date(),Rate=1,RateDay=0, 
                   adjust_yoy=FALSE){   
  stocks <- stockID2stockID(stocks,from="local",to="ts")
  if(missing(rptDate)) {
    rptDate <- 0
  } else {
    if(adjust_yoy){
      Time <- rptDate.deadline(rptDate)
    } 
    rptDate <- rdate2int(rptDate)
  }
  Time=rdate2ts(Time)
  syspars <- list(CurrentDate=Time,bRate=Rate,RateDay=RateDay)
  stocks.str <- paste(stocks,collapse=";")  
  str <- paste('Rdate:=',rptDate,';\n',
               'return Query("","',
               stocks.str,               
               '",True,"","stockID",DefaultStockID(),',
               funchar,
               ');',
               sep="")
  tsRequire()
  re <- tsRemoteExecute(str,syspars)
  re <- plyr::ldply(re,as.data.frame)
  re$stockID <- stockID2stockID(re$stockID,from="ts",to="local")
  if(!missing(varname)){
    re <- renameCol(re,colnames(re)[-1],varname)
  }
  return(re)
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ==================    tradingday related       =============
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' memory.load
#' 
#' load frequently used data into memory, including \code{.tradingdays}, \code{.QT_sus_res}, ...
#' @param reload a logic. If reload the memory data?
#' @param datasrc
#' @return return NULL, but load some data into memory.
#' @export
#' @author Ruifei.Yin
memory.load <- function(reload=FALSE){  
  if(!exists(".tradingdays") || reload){
    if(reload){
      cat("[memory data reloading]\n")
    } else {
      cat("[memory data loading]\n")
    }  
    
    # -- the market trading days  
    cat("  Loading '.tradingdays' ... \n")
    qr1 <- "select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1"
    tradingdays <- queryAndClose.dbi(db.local("main"),qr1)[[1]] 
    .tradingdays <<- intdate2r(tradingdays)
    
    con_qt <- db.local("qt")
    con_main <- db.local("main")
    # - QT_sus_res
    cat("  Loading '.QT_sus_res' ... \n")
    QT_sus_res <- dbReadTable(con_qt,"QT_sus_res")
    QT_sus_res <- data.table::data.table(QT_sus_res,key = "stockID")
    .QT_sus_res <<- QT_sus_res[is.na(res),res:=99990101
                               ][,`:=`(sus=intdate2r(sus),res=intdate2r(res))
                                 ][,-("updateDate"),with=FALSE
                                   ]
    # -- the market size data frame
    cat("  Loading '.marketsize' ... \n")
    .marketsize <<- dbReadTable(con_qt, "QT_Size")
    .marketsize$date <<- intdate2r(.marketsize$date)
    
    # - LC_ExgIndustry
    cat("  Loading '.LC_ExgIndustry' ... \n")
    LC_ExgIndustry <- dbReadTable(con_main,"LC_ExgIndustry")
    LC_ExgIndustry <- data.table::data.table(LC_ExgIndustry,key = "stockID")
    .LC_ExgIndustry <<- LC_ExgIndustry[is.na(OutDate),OutDate:=99990101
                               ][,`:=`(InDate=intdate2r(InDate),OutDate=intdate2r(OutDate))]
    
    dbDisconnect(con_qt)
    dbDisconnect(con_main)
  }  
}




#' TS.sus_res
#' 
#' get the suspension and resumption data
#' @param TS
#' @param datasrc
#' @return a data.frame with cols: "sus" and "res"
#' @export
#' @author Ruifei.yin
#' @examples
#' TS <- getTS(getRebDates(as.Date("2007-01-01"),as.Date("2016-01-01"),rebFreq = "week"),indexID = "EI000300")
#' microbenchmark::microbenchmark(re <- TS.sus_res(TS,datasrc="memory"),re1 <- TS.sus_res(TS,datasrc = "local"),times = 10)  # 100 VS. 3000
TS.sus_res <- function(TS,datasrc="memory"){
  TS_ <- TS[,c("date","stockID")]
  if(datasrc == "memory"){
    memory.load()
    TS_ <- data.table::data.table(TS_)
    re <- .QT_sus_res[TS_,.(date,stockID,x.sus,x.res),on=.(stockID,sus<=date,res>date)]
    re <- renameCol(re,c("x.sus","x.res"),c("sus","res"))
    re <- as.data.frame(re)
  } else if(datasrc == "local"){
    TS_$date <- rdate2int(TS_$date)
    con <- db.local("qt")
    qr <- paste(
      "select date,a.stockID,sus,res
      from yrf_tmp as a left join QT_sus_res as b
      on a.stockID=b.stockID and sus<=date and (res>date or res is null)"  )
    dbWriteTable(con,name="yrf_tmp",value=TS_[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
    re <- dplyr::mutate(re,date=intdate2r(date),sus=intdate2r(sus),res=intdate2r(res))
  }
  re <- merge.x(TS,re,by=c("date","stockID"),mult = "first")
  return(re)
}

#' trday.get
#' 
#' get the trading date series from begT to endT
#' @param begT a Date object.The default is as.Date("1990-12-19")
#' @param endT a Date object.The default is Sys.Date()
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @param datasrc
#' @return a vector of class Date
#' @export
#' @author Ruifei.yin
#' @examples
#' re <- trday.get()
#' re <- trday.get(as.Date("2012-01-01"),as.Date("2012-09-30"))
#' re <- trday.get(as.Date("2012-07-01"),as.Date("2013-03-30"),stockID="EQ000527")
#' system.time(replicate(100,ii <- trday.get(datasrc="local"))) # 14.09
#' system.time(replicate(100,ii <- trday.get(datasrc="memory"))) # 0.39
#' system.time(replicate(100,ii <- trday.get(datasrc="local",stockID="EQ000527"))) # 16.38
#' system.time(replicate(100,ii <- trday.get(datasrc="memory",stockID="EQ000527"))) # 1.25
trday.get <- function(begT=as.Date("1990-12-19"),endT=Sys.Date(),
                          stockID=NULL,
                          datasrc="memory"){    
  
  # get the market trading days
  if(datasrc %in% c("quant","local")){
    begT <- max(begT,as.Date("1990-12-19"))
    begT <- rdate2int(begT)
    endT <- rdate2int(endT)
    qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begT,"and",endT)    
    if(datasrc=="quant"){
      trday <- queryAndClose.odbc(db.quant(),qr)
    } else if(datasrc=="local"){
      trday <- queryAndClose.dbi(db.local("main"),qr)
    }     
    re <- trday[[1]]
    re <- intdate2r(re)
  } else if (datasrc=="memory") {
    memory.load()
    begT <- max(begT,as.Date("1990-12-19"))
    re <- get(".tradingdays")
    re <- re[re >= begT & re <= endT]
  }  
  
  # get the stock's trading days
  if(!is.null(stockID)){
    TS <- expand.grid(date=re, stockID=stockID)
    sus_res <- TS.sus_res(TS,datasrc = datasrc)
    isTradingday <- is.na(sus_res$sus)
    re <- re[isTradingday]
  }  
  
  re <- re[order(re)] 
  return(re)
}








#' trday.is
#' 
#' Identify if the date is trading day or not
#' @param datelist a vector of class Date
#' @param stockID a character string or NULL If NULL, the market trading days, other wise the trading days of specific stock.
#' @param TS
#' @param drop
#' @return a logical vecter with elements TRUE or FALSE depending on whether its argument is an tradingday
#' @export
#' @author Ruifei.Yin
#' @examples
#' datelist <- seq(from=as.Date("2013-01-21"),to=as.Date("2013-01-30"),by="day")
#' trday.is(datelist)
#' trday.is(datelist,stockID="EQ000527")
#' trday.is(TS=TS)
trday.is <- function(datelist,stockID=NULL,TS,
                     drop){
  if(is.null(stockID) & missing(TS)){ # the market tradingday
    mindt <- min(datelist)
    maxdt <- max(datelist)
    tradingday <- trday.get(begT=mindt,endT=maxdt,stockID=NULL)
    re <- datelist %in% tradingday
    return(re)
  } 
  
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
  sus_res <- TS.sus_res(TS)
  isTradingday <- trday.is(sus_res$date,stockID=NULL) & is.na(sus_res$sus)
  
  if(drop){
    re <- isTradingday
  }else {
    re <- cbind(TS,isTradingday)
  }
  return(re)
}






#' trday.nearest
#'
#' get the nearest tradingday. 
#' @param datelist a vector of class Date
#' @param dir a integer. Indicating forward or backward to find, if \code{datelist} is not tradingday. -1 for backward, 1 for forward.
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @return a vector of class Date, the value of which is the nearest tradingday before/after the \code{datelist} if \code{datelist} is not a tradingday, otherwise,the \code{datelist} itself. 
#' @author Ruifei.Yin
#' @export
#' @examples
#' datelist <- as.Date(c("2012-07-21","2012-07-22","2012-07-23","2013-01-30"))
#' trday.nearest(datelist)
#' trday.nearest(datelist, dir = 1)
#' trday.nearest(datelist, dir = 1, stockID="EQ000527")
trday.nearest <- function(datelist, dir=-1L, stockID=NULL, TS,
                          drop){ 
  if(is.null(stockID) & missing(TS)){ # the market tradingday
    tradingdays <- trday.get(endT=Sys.Date()+365, stockID=NULL)
    if(dir == -1L){
      re <- tradingdays[findInterval(datelist, tradingdays)]  
    } else if(dir == 1L){
      re <- tradingdays[findInterval.rightClosed(datelist, tradingdays)+1]
    } else {
      stop("unsupported \"dir\" argument!")
    }
    re <- as.Date(re,origin="1970-01-01")
    return(re)
  } 
  
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
  sus_res <- TS.sus_res(TS)
  if(dir == -1L){
    re <- dplyr::mutate(sus_res,nearest=ifelse(is.na(sus),
                                               trday.nearest(date,dir = -1L),
                                               trday.nearby(sus,by=-1,stockID=NULL)))
  } else if(dir == 1L){
    re <- dplyr::mutate(sus_res,nearest=ifelse(is.na(sus),
                                               trday.nearest(date,dir = 1L),
                                               res))
  } else {
    stop("unsupported \"dir\" argument!")
  }
  re$nearest <- as.Date(re$nearest,origin="1970-01-01")
  if(drop){
    re <- re$nearest
  }else {
    re <- dplyr::select(re,-sus,-res)
  }
  return(re)
  
}




#' trday.nearby
#' 
#' get the nearby tradingday by shifting forward or backward.If the argument datelist is not a tradingday,the tradingday nearest by it will be firstly find by function \code{\link{trday.nearest}}.
#' @param datelist a vector of class Date
#' @param by a integer giving the lagging days.If negetive,get the earlyer tradingday,if positive,get the later tradingday. 
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @param dir 1L or -1L. see \code{\link{trday.nearest}}
#' @return a vector of trading days of class Date
#' @export
#' @author Ruifei.Yin
#' @examples
#' (datelist <- as.Date(c("2012-07-21","2012-07-22","2012-07-23","2013-01-30")))
#' trday.nearby(datelist,-20) # the tradingday 20 days earlyer than datelist
#' trday.nearby(datelist,20) # the tradingday 20 days later than datelist 
#' trday.nearby(datelist,20,stockID="EQ000527") 
trday.nearby <- function(datelist,by, stockID=NULL,
                         dir=if(by>0) 1L else -1L,
                         TS,
                         drop){
  if(is.null(stockID) & missing(TS)){ # the market tradingday
    trdingday.all <- trday.get(endT=Sys.Date()+365, stockID=NULL)   
    trdlist <- trdingday.all[findInterval(datelist, trdingday.all)] # get the nearest tradingday
    lag.trdingday.all <- xts::lag.xts(trdingday.all, -by)
    idx <- match(trdlist, trdingday.all)
    re <- lag.trdingday.all[idx]
    return(re)
  }
  
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
  # get the market's nearby tradingday
  nearby_market <- trday.nearby(TS$date,by=by,stockID=NULL) 
  # get the stock's nearest tradingday
  nearby <- trday.nearest(TS=data.frame(date=nearby_market,stockID=TS$stockID),dir = dir,drop = TRUE)
  
  if(drop){
    re <- nearby
  } else {
    re <- cbind(TS,nearby)
  }
  return(re)
}


#' trday.offset 
#' 
#' offset the datelist by months,quarters,ect. then get the nearest tradingday
#' @param datelist a vector of class Date
#' @param by a period object. See detail in package \code{lubridate}.
#' @param dir 1L or -1L. if the result date is not trdingday, get the forward nearest tradingday or the backward tradingday? See detail in \link{trday.nearest}
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @return a vector of trading days of class Date
#' @export
#' @author Ruifei.Yin
#' @examples
#' (datelist <- as.Date(c("2012-07-21","2012-07-22","2012-07-23","2013-01-30")))
#' trday.offset(datelist,months(1)) # the tradingdays 1 month after the datelist
#' trday.offset(datelist,months(-1)) # the tradingdays 1 month before the datelist
#' trday.offset(datelist,years(1))
#' trday.offset(datelist,months(-1),stockID="EQ000527") 
trday.offset <- function(datelist,by=months(1),stockID=NULL,
                         dir=if(as.numeric(by)>0) 1L else -1L, 
                         TS,
                         drop){
  if(is.null(stockID) & missing(TS)){ # the market tradingday
    if (any(c(by@.Data, by@minute, by@hour, by@day) != 0)){
      re <- datelist + by
    } else { # if handles month and years, should use %m+%
      re <- datelist %m+% by
    }
    re <- trday.nearest(re, dir=dir, stockID=NULL)
    return(re)
  } 
  
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
  # get the market's offset tradingday
  offset_market <- trday.offset(TS$date,by=by,dir=dir,stockID=NULL) 
  # get the stock's offset tradingday
  offset <- trday.nearest(TS=data.frame(date=offset_market,stockID=TS$stockID),dir = dir,drop = TRUE)
  
  if(drop){
    re <- offset
  } else {
    re <- cbind(TS,offset)
  }
  return(re)
}



#' trday.count 
#' 
#' get the count of the trading days between \code{begT} and \code{endT}.
#' @param begT a Date object.The default is as.Date("1990-12-19")
#' @param endT a Date object.The default is Sys.Date()
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @return a integer 
#' @export
#' @author Ruifei.Yin
#' @examples
#' trday.count(as.Date("2012-01-01"),as.Date("2012-09-30"))
#' trday.count(as.Date("2012-01-01"),as.Date("2012-09-30"),stockID="EQ000527")
trday.count <- function(begT=as.Date("1990-12-19"), endT=Sys.Date(), stockID=NULL){
  trdate <- trday.get(begT,endT,stockID=stockID)
  re <- length(trdate)
  return(re)
}


#' trday.IPO
#' @param stockID a vector
#' @return a vector
#' @export
#' @family SecuMain functions
trday.IPO <- function(stockID,datasrc=defaultDataSRC()){
  qr <- paste("select ListedDate,ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  re <- tmpdat[match(stockID,tmpdat[,2]),1]  
  re <- intdate2r(re)
  return(re)  
}

#' trday.unlist
#' @param stockID a vector
#' @return a vector
#' @export
#' @family SecuMain functions
trday.unlist <- function(stockID,datasrc="jy"){
  
  if(datasrc=="jy"){
    stocks <- substr(unique(stockID),3,8)
    #get delist date
    qr <- paste("SELECT 'EQ'+s.SecuCode 'stockID',convert(varchar,ChangeDate,112) 'delistdate'
              FROM LC_ListStatus l
              INNER join SecuMain s on l.InnerCode=s.InnerCode and s.SecuCategory=1
              where l.ChangeType=4 and l.SecuMarket in (83,90)
              and s.SecuCode in",brkQT(stocks))
    tmpdat <- queryAndClose.odbc(db.jy(),qr,as.is = TRUE)
    re <- tmpdat[match(stockID,tmpdat[,1]),2]  
    re <- intdate2r(re)
  }
  
  return(re)  
}


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ====================    SecuMain related        ==============
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' tradeCode2stockID
#' 
#' Covert the tradeCode to stockID given specific secucategory.
#' @param tradeCode Vector of character,giving the stock trading code. eg. \code{c("600001","600002","000300")}
#' @param secuCate integer,giving the secuCategory.(1 for A equity,4 for index,...)
#' @param IDsrc a charactor string, could be one of "local","jy","ts","wind",etc.
#' @return a vector,return the stockID of specific datascr.
#' @export
#' @family SecuMain functions
#' @author Ruifei.Yin
#' @examples
#' tradeCode2stockID(c("600001","600002"),secuCate=1)
#' tradeCode2stockID(c("000001","000300"),secuCate=4)
tradeCode2stockID <- function(tradeCode, secuCate=1, IDsrc="local",
                              datasrc=defaultDataSRC()){
  id_var <- switch(IDsrc,
                   local="ID",
                   quant="ID",
                   jy="InnerCode",
                   ts="StockID_TS",
                   wind="StockID_wind")
  qr <- paste("select secucode,",id_var,"from SecuMain where secucategory=",secuCate)
  
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr,as.is=1)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  }  
  
  stockID <- tmpdat[match(tradeCode,tmpdat[,1]),2]    
  return(stockID)
}


intCode2tradeCode <- function(){
  #   718-->"000718"
}



#' stockID2tradeCode
#' 
#' Covert the stockID to tradeCode.
#' @param stockID a vector of stockID of specific datascr.
#' @param IDsrc  a charactor string, could be one of "local","jy","ts","wind",etc.
#' @return a vector of character,giving the stock trading code. eg. \code{c("600001","600002","000300")}
#' @export
#' @family SecuMain functions
#' @author Ruifei.Yin
#' @examples
#' stockID2tradeCode(c("EI000001","EQ000030"),IDsrc="local")
#' stockID2tradeCode(c("SZ000001","SH000030"),IDsrc="ts")
stockID2tradeCode <- function(stockID,IDsrc="local",
                              datasrc=defaultDataSRC()){
  id_var <- switch(IDsrc,
                   local="ID",
                   quant="ID",
                   jy="InnerCode",
                   ts="StockID_TS",
                   wind="StockID_wind")
  qr <- paste("select secucode,",id_var,"from SecuMain")
  
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr,as.is=1)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  
  tradeCode <- tmpdat[match(stockID,tmpdat[,2]),1]  
  return(tradeCode)
}

#' stockID2stockID
#' 
#' Covert the stockID to another type of stockID.
#' @param stockID a vector of specific type
#' @param to a character string, giving the type of stockID convered to. Could be one of "local","ts","jy","wind".
#' @param from a character string, giving the type of stockID convered from. Could be one of "local","ts","jy","wind".
#' @return a vector,return the stockID of specific datascr.
#' @export
#' @family SecuMain functions
#' @author Ruifei.Yin
#' @examples
#' stockID2stockID(c("EI000001","EI000300"),to="jy",from="local")
#' stockID2stockID(c("SH000001","SH000300"),to="jy",from="ts")
stockID2stockID <- function(stockID, from, to,
                            datasrc=defaultDataSRC()){  
  id_from <- switch(from,
                    local="ID",
                    quant="ID",
                    jy="InnerCode",
                    ts="StockID_TS",
                    wind="StockID_wind")
  id_to <- switch(to,
                  local="ID",
                  quant="ID",
                  jy="InnerCode",
                  ts="StockID_TS",
                  wind="StockID_wind")
  qr <- paste("select",id_from,",",id_to,"from SecuMain")
  
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  
  stockID_to <- tmpdat[match(stockID,tmpdat[,1]),2]
  return(stockID_to)
}


#' stockID2name
#' @param stockID a vector
#' @return a vector
#' @export
#' @family SecuMain functions
#' @examples
#' stockID2name("EQ000527")
stockID2name <- function(stockID,datasrc=defaultDataSRC()){
  qr <- paste("select SecuAbbr,ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  re <- tmpdat[match(stockID,tmpdat[,2]),1]  
  return(re)  
}



#' stockName2ID
#' @param name a characoter vector
#' @return a dataframe with cols: 'SecuAbbr', 'ID'
#' @export
#' @family SecuMain functions
#' @examples
#' stockName2ID("ST")
stockName2ID <- function(name, datasrc=defaultDataSRC()){
  qr <- paste("select SecuAbbr, ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  }   
  re <- tmpdat[grep(name, gsub("\\s*","",tmpdat$SecuAbbr)), ]  
  return(re) 
}

#' SecuMarket
#' @param stockID a vector
#' @return a vector
#' @export
#' @family SecuMain functions
SecuMarket <- function(stockID,datasrc=defaultDataSRC()){
  qr <- paste("select SecuMarket,ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  re <- tmpdat[match(stockID,tmpdat[,2]),1]  
  return(re)  
}
#' SecuCategory
#' @param stockID a vector
#' @return a vector
#' @export
#' @family SecuMain functions
SecuCategory <- function(stockID,datasrc=defaultDataSRC()){
  qr <- paste("select SecuCategory,ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- queryAndClose.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- queryAndClose.dbi(db.local("main"),query=qr)
  } 
  re <- tmpdat[match(stockID,tmpdat[,2]),1]  
  return(re)  
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    sector components & sectorID      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============




#' getComps
#'
#' get the components of the specific index, sector or plate on certain days.
#' @param ID a character string. The ID of the index, sector or plate. Could be a single-ID-code(eg. "EI000300","ES09440000",...) or a more complicated express containing some set operations and ID-codes(eg. "setdiff(union(EI000300,EI000905),ES09440000)")
#' @param endT a vector of class \code{Date}. IF missing, then get the latest components.
#' @param drop if drop the field of date and return a vector when endT is length 1 ?
#' @param datasrc
#' @return If \code{endT} is missing or is length 1 and \code{drop} is TRUE, a vector of stockID of the components; else a dataframe, with cols: "date" and "stockID"
#' @export
#' @family getComps functions
#' @examples
#' re1 <- getComps("EI000300") # same as getIndexComp("EI000300")
#' ID <- "setdiff(union(EI000300,EI399006),ES09440000)"
#' re2 <- getComps(ID)
#' re3 <- getComps(ID,endT=as.Date(c("2011-12-31","2012-12-31")))
#' more examples:
#' # CSI300 ex. financial servive sector
#' getComps("setdiff(EI000300,ES09440000)")
#' # CSI300 and financial servive sector
#' getComps("intersect(EI000300,ES09440000)")
#' # not drop
#' getComps("EI000300",drop=FALSE)
getComps <- function(ID, endT=Sys.Date(), drop=TRUE, datasrc=defaultDataSRC()){  
  IDs <- gsub("union|intersect|setdiff|[()]","",ID)
  IDs <- strsplit(IDs,split=",")[[1]]
  IDs <- unique(IDs[IDs!=""])  
  IDs_index <- IDs[substring(IDs,1,2)=="EI"]
  IDs_sector <- IDs[substring(IDs,1,2)=="ES"]
  IDs_plate <- IDs[substring(IDs,1,2)=="EP"]  
  
  if(length(IDs_index) > 0){
    comp_index <- lapply(IDs_index, FUN=getIndexComp, endT=endT, drop=FALSE, datasrc=datasrc)
    names(comp_index) <- IDs_index
  } else {
    comp_index <- list()
  }
  if(length(IDs_sector) > 0){
    comp_sector <- lapply(IDs_sector, FUN=getSectorComp, endT=endT, drop=FALSE, datasrc=datasrc)
    names(comp_sector) <- IDs_sector
  } else {
    comp_sector <- list()
  }
  if(length(IDs_plate) > 0){
    comp_plate <- lapply(IDs_plate, FUN=getPlateComp, endT=endT, drop=FALSE, datasrc=datasrc)
    names(comp_plate) <- IDs_plate
  } else {
    comp_plate <- list()
  }
  comps <- c(comp_index,comp_sector,comp_plate)  
  
  subfun <- function(endT0){
    comps0 <- lapply(comps,function(x) x[x$date==endT0,"stockID"] )
    dat0 <- with(comps0,eval(parse(text=ID)))
    dat0 <- data.frame(date=endT0,stockID=dat0,stringsAsFactors=FALSE)
    return(dat0)    
  }  
  # cat("Function getComps: getting the components ....\n")
  re <- plyr::ldply(endT, subfun)
  
  if(length(endT)==1 && drop==TRUE){
    return(re$stockID)
  } else {
    return(re)
  }
}


#' getIndexComp
#'
#' get the components of the specific index on certain day.
#' @param indexID the stockID of the index
#' @param endT a vector of class \code{Date}. IF missing, then get the latest components.
#' @param drop if drop the field of date and return a vector when endT is length 1 ?
#' @param datasrc
#' @return If \code{endT} is missing or is length 1 and \code{drop} is TRUE, a vector of stockID of the components; else a dataframe, with cols: "date" and "stockID"
#' @export
#' @examples 
#' tmp <- getIndexComp("EI000300") # get the latest components, a vector
#' tmp <- getIndexComp("EI000300",drop=FALSE) # get the latest components, a dataframe
#' tmp <- getIndexComp("EI000300",as.Date("2012-12-31")) # get the components on single day,a vector
#' tmp <- getIndexComp("EI000300",as.Date(c("2011-12-31","2012-12-31"))) # get the components on multi-days, a data frame
#' tmp <- getIndexComp("EI000300",as.Date(c("2005-12-31","2012-12-31"))) # get the components before index's pubdate, a data frame
getIndexComp <- function(indexID, endT=Sys.Date(), drop=TRUE, datasrc=defaultDataSRC()){
  pubdate <- trday.IPO(indexID)
  
  endTdf <- data.frame(dateori=endT,date=endT)
  if(indexID!='EI000985' && (!is.na(pubdate)) && min(endT)<pubdate){
    warning(paste("min(endT):",min(endT)," is earlier than index's published date:",pubdate,". The early components would be approximated!",sep=""))
    endT[endT<pubdate] <- pubdate
    endTdf <- transform(endTdf,date=endT)
    endT <- unique(endT)
  }
  
  if(datasrc %in% c("quant","local")){   
    endT <- rdate2int(endT)
    tmpdat <- data.frame(endT=endT)    
    qr <- paste("SELECT a.endT as date, b.SecuID as stockID from yrf_tmp a, LC_IndexComponent b
                where b.IndexID=", QT(indexID), 
                "and InDate<=endT and (OutDate>endT or OutDate IS NULL)")      
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con, sqtable="yrf_tmp", errors=FALSE)
      sqlSave(con, dat=tmpdat, tablename="yrf_tmp", safer=FALSE, rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re$date <- intdate2r(re$date) 
    
  } 
  if(datasrc=="ts"){
    endT <- rdate2ts(endT)
    indexID <- stockID2stockID(indexID,to="ts",from="local")
    subfun <- function(endT0){  
      tsRequire()
      stocks <- tsRemoteCallFunc("GetBKbyDate",list(indexID,endT0))
      stocks <- as.vector(as.matrix(stocks))
      stocks <- stockID2stockID(stocks,to="local",from="ts")
      dat0 <- data.frame(date=tsdate2r(endT0),stockID=stocks,stringsAsFactors=FALSE)
      return(dat0)
    }
    re <- plyr::ldply(endT,subfun)  
  }
  
  endT <- endTdf$dateori
  
  if(indexID!='EI000985' && (!is.na(pubdate)) && min(endT)<pubdate){
    re <- endTdf %>% dplyr::full_join(re,by='date') %>% dplyr::select(-date) %>% dplyr::rename(date=dateori)
    ipo <- data.frame(stockID=unique(re$stockID),stringsAsFactors = FALSE)
    ipo$ipoday <- trday.IPO(ipo$stockID)
    re <- re %>% dplyr::left_join(ipo,by='stockID') %>% dplyr::mutate(gap=date-ipoday) %>% 
      dplyr::filter(gap>=90) %>% dplyr::select(-ipoday,-gap)
  }
  
  re <- dplyr::arrange(re,date,stockID)
  
  if(length(endT)==1 && drop==TRUE){
    return(re$stockID)
  } else {
    return(re)
  }
}



#' getIndexCompWgt
#'
#' get the components and wgts of the specific index on certain day.
#' @param indexID the stockID of the index
#' @param endT a vector of class \code{Date}. IF missing, then get the latest components.
#' @param datasrc
#' @return a dataframe, with cols: "date", "stockID","wgt".
#' @export
#' @family getComps functions
#' @examples 
#' tmp <- getIndexCompWgt("EI000300") # get the latest components
#' tmp <- getIndexCompWgt("EI000985",as.Date("2012-12-31")) # get the components on single day
#' tmp <- getIndexCompWgt("EI000300",as.Date(c("2011-12-31","2012-12-31"))) # get the components on multi-days
getIndexCompWgt <- function(indexID="EI000300",endT,datasrc=defaultDataSRC()){
  
  if(missing(endT)) endT <- trday.nearby(Sys.Date(),by=-1)  # if endT missing, get the nearest wgt data.  
  endT <- trday.nearest(endT)    # if endT is not tradingday, get the nearest trading days.
  
  trday.nearbyinDB <- function(indexID,endT,datasrc){ 
    # sometimes, the index-component-weight in database is not daily data, this function is used to get nearest date on which the weight data is available.
    daymat <- data.frame(oldday=endT)
    endT <- rdate2int(endT)
    qr <- paste(
      "SELECT DISTINCT EndDate
      FROM LC_IndexComponentsWeight 
      where IndexID=",QT(indexID),"order by EndDate"
    )
    if(datasrc=="quant"){
      con <- db.quant()
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    } else if (datasrc=="jy"){
      con <- db.jy()
      indexID <- stockID2stockID(indexID,to="jy",from="local")  
      qr <- paste(
        "SELECT DISTINCT convert(varchar,EndDate,112) 'EndDate'
        FROM LC_IndexComponentsWeight 
        where IndexCode=",QT(indexID),"order by EndDate"
      )
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    }
    if(nrow(re)==0){
      return(0)
    }
    if(min(endT)<min(re$EndDate)){
      return(0)
    } else{
      endT<- re[findInterval(endT,re$EndDate),]
      daymat$newday <- endT
      return(daymat)
    }
  }
  daymat <- trday.nearbyinDB(indexID,endT,datasrc)
  
  if(is.numeric(daymat)){ # -- calulate the estimated wgt by 'free_foat_MV'
    warning("There is no wgt data in table 'LC_IndexComponentsWeight', calulate the estimated wgt by 'free_foat_MV'.")
    # TS <- getTS(endT,indexID)
    TS <- getIndexComp(indexID = indexID ,endT = endT, drop = FALSE)
    TSF <- gf.free_float_sharesMV(TS)
    re <- plyr::ddply(TSF,"date",transform,wgt=factorscore/sum(factorscore,na.rm=TRUE))
    re <- re[,c("date","stockID","wgt" )]
    return(re)
  }
  endT <- unique(daymat$newday)
  
  
  if(datasrc %in% c("quant","local")){    
    tmpdat <- data.frame(endT=endT)    
    qr <- paste("SELECT a.endT as date, b.SecuID as stockID, Weight/100 as wgt 
                from yrf_tmp a, LC_IndexComponentsWeight b
                where b.IndexID=", QT(indexID), 
                "and a.endT=b.EndDate")      
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con, sqtable="yrf_tmp", errors=FALSE)
      sqlSave(con, dat=tmpdat, tablename="yrf_tmp", safer=FALSE, rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re=merge(re,daymat,by.x="date",by.y ="newday" )
    re=re[,c("oldday","stockID","wgt")]
    colnames(re)<-c("date","stockID","wgt")
  }
  
  if(datasrc=="jy"){
    indexID <- stockID2stockID(indexID,to="jy",from="local")     
    subfun <- function(endT0){
      qr <- paste(
        "SELECT InnerCode as stockID, Weight/100 as wgt
        FROM LC_IndexComponentsWeight as a  
        where IndexCode=",QT(indexID) ,"and a.EndDate=",QT(endT0)
      )
      dat <- queryAndClose.odbc(db.jy(),query=qr)            
      if(nrow(dat)==0){
        warning(paste("weight data is missing on",endT0,"!"))      
      } else {
        dat <- data.frame(date=intdate2r(endT0),dat,stringsAsFactors=FALSE)
      }    
      return(dat)
    }  
    cat("Function getIndexCompWgt: getting the component wgts ....\n")
    re <- plyr::ldply(endT,subfun,.progress="text")    
    re$stockID <- stockID2stockID(re$stockID,to="local",from="jy")    
  }
  
  re <- dplyr::arrange(re,date,stockID) 
  return(re)
}



#' getSectorComp
#'
#' get the components of the specific sector on certain day.
#' @param sectorID ID of the sector. eg. "ES09440000" for "financial service" the 1st level sector of SHENWAN; "ES09440100" for "bank"  the 2nd level sector of SHENWAN.  Get details with \code{CT_industryList()}
#' @param endT a vector of class \code{Date}. IF missing, then get the latest components.
#' @param drop if drop the field of date and return a vector when endT is length 1 ?
#' @param datasrc
#' @return If \code{endT} is missing or is length 1 and \code{drop} is TRUE, a vector of stockID of the components; else a dataframe, with cols: "date" and "stockID"
#' @export
#' @family getComps functions
#' @examples 
#' tmp <- getSectorComp("ES09440000") # get the latest components, a vector
#' tmp <- getSectorComp("ES09440000",as.Date("2012-12-31")) # get the components on single day,a vector
#' tmp <- getSectorComp("ES09440000",as.Date(c("2011-12-31","2012-12-31"))) # get the components on multi-days, a data frame
getSectorComp <- function(sectorID, endT=Sys.Date(), drop=TRUE, datasrc=defaultDataSRC()){
  
  level <- CT_industryList(ID=sectorID)$Level
  sectorVar <- paste("Code",level,sep="")  
  
  if(datasrc %in% c("quant","local")){    
    endT <- rdate2int(endT)
    tmpdat <- data.frame(endT=endT)    
    qr <- paste("SELECT a.endT as date, b.stockID as stockID from yrf_tmp a, LC_ExgIndustry b
                where ",sectorVar,"=", QT(sectorID), 
                "and InDate<=endT and (OutDate>endT or OutDate IS NULL)")      
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con, sqtable="yrf_tmp", errors=FALSE)
      sqlSave(con, dat=tmpdat, tablename="yrf_tmp", safer=FALSE, rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re$date <- intdate2r(re$date)
  }   
  re <- dplyr::arrange(re,date,stockID)
  if(length(endT)==1 && drop==TRUE){
    return(re$stockID)
  } else {
    return(re)
  }  
}


#' @family getComps functions
getPlateComp <- function(plateID,endT,drop,datasrc){
  
}



#' sectorID2name
#' @export
sectorID2name <- function(sectorID){
  tmpdat <- CT_industryList(ID=sectorID)
  tmpdat <- tmpdat[,c("IndustryID","IndustryName")]
  re <- tmpdat[match(sectorID,tmpdat[,1]),2]
  return(re)
}
plateID2name <- function(plateID){
  
}


#' sectorID2indexID
#' @export
#' @examples 
#' sctID <- getSectorID(stockID = "EQ000001",drop=TRUE)
#' sectorID2indexID(sctID)
sectorID2indexID <- function(sectorID,std=24){
  qr <-paste("select A.IndustryCode 'sector',B.SecuCode
             from LC_CorrIndexIndustry A,SecuMain B
             where A.IndexCode = B.InnerCode and A.IndexState in (1,2) and A.IndustryStandard=",std)
  tmpdat <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors =FALSE)
  if(std==24){
    tmpdat$sector <- paste0("ES33",tmpdat$sector)
  }else if(std==9){
    tmpdat$sector <- paste0("ES09",tmpdat$sector)
  }else if(std==3){
    tmpdat$sector <- paste0("ES03",tmpdat$sector)
  }
  
  re <- tmpdat[match(sectorID,tmpdat[,1]),2]  
  if(std %in% c(9,24)){
    re <- paste0("EI",re)
  }
  return(re)
}


#' stockID2indexID
#' @export
#' @examples 
#' stockID2indexID(stockID = "EQ000001")
stockID2indexID <- function(TS, stockID, withsector = FALSE){
  re <- getSectorID(TS = TS, stockID = stockID)
  re$indexID <- sectorID2indexID(re$sector)
  if(!withsector){
    re <- dplyr::select(re, -sector)
  }
  return(re)
}




#' getSectorID
#'
#' get the sectorID of the stocks on specific dates.
#' @param TS  a \bold{TS} object
#' @param stockID a vector of stockID
#' @param endT a vector of Date
#' @param sectorAttr a list(See more in \code{\link{defaultSectorAttr}}) or NULL,or "existing". 
#' @param ret a charactor string,could be one of "ID" or "name",indicating sectorID or sectorName returned.
#' @param drop a logical. Shoud the \code{TS} be exculded in the result?
#' @param fillNA
#' @param ungroup
#' @param datasrc
#' @return a data.frame,with the same cols of TS,added by "\code{sector}". Or a vector if \code{drop} is TRUE. You can get more sector infomation by \code{\link{CT_industryList}}
#' @note param TS and combination of stockID and endT should  at least have one and only have one. The combination of vector stockID and endT could be different length, which abide by the recycling rule.
#' @author Ruifei.Yin
#' @export
#' @examples
#' # - with TS
#' TS <- getTS(getRebDates(as.Date('2007-03-17'),as.Date('2012-05-20')),'EI000300')
#' getSectorID(TS)
#' # - one stock, multiple dates
#' getSectorID(stockID="EQ000001", endT=as.Date(c("2010-01-01","2012-01-01","2013-01-01")))
#' # - one date, multiple stocks
#' getSectorID(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2010-01-01"))
#' # - get 'ZHONGXIN' sector
#' getSectorID(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2010-01-01"), sectorAttr=list(3,1), ret="name")
#' getSectorID(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2010-01-01"), sectorAttr=list(3,2), ret="name")
#' # -- combined sectorAttr
#' test <- getSectorID(TS, sectorAttr= defaultSectorAttr("fct",fct_level = 5))
#' test <- getSectorID(TS, sectorAttr= defaultSectorAttr("ind_fct",ind_std = 33,ind_level = 1,fct_level = 5))
#' test <- getSectorID(TS, sectorAttr= defaultSectorAttr("ind_fct",ind_std = c(336,33),ind_level = 1,fct_level = 5))
#' test <- getSectorID(TS, sectorAttr= defaultSectorAttr("fct",fct_std = buildFactorLists_lcfs(c("F000001","F000006"),factorRefine = refinePar_default("none",NULL)),fct_level = 5))
#' factorList1 <- buildFactorList(factorFun = "gf_cap",factorRefine=refinePar_default("scale",NULL))
#' test2 <- getSectorID(TS, sectorAttr= list(std=list(factorList1,33),level=list(5,1)))
#' # - speed compares (20 VS. 200)
#' microbenchmark::microbenchmark(re <- getSectorID(TS,datasrc="memory"),re1 <- getSectorID(TS,datasrc = "local"),times = 10)
getSectorID <- function(TS, stockID, endT=Sys.Date(),
                        sectorAttr=defaultSectorAttr(),
                        ret=c("ID","name"),
                        drop=FALSE,
                        fillNA=FALSE,
                        ungroup=NULL,
                        datasrc="memory"){
  
  if(identical(sectorAttr,"existing") | is.null(sectorAttr)){
    return(TS)
  }
  # arguments checking
  ret <- match.arg(ret)
  if (missing(TS) && missing(stockID)) {
    stop("Param TS and combination of stockID and endT should at least have one!")
  }
  if (!missing(TS) && !missing(stockID)) {
    stop("Param TS and combination of stockID and endT should only have one!")
  }
  if (missing(TS)){
    TS <- expand.grid(date=endT, stockID=stockID)
  }
  names(sectorAttr) <- c("std","level")
  if(length(sectorAttr$std) != length(sectorAttr$level)) stop("The argument of sectorAttr is not complete.")
  if("sector" %in% colnames(TS)){
    warning('There is already a "sector" field in TS, it will be overwritten!')
    TS$sector <- NULL
  }
  
  # looping
  loop <- length(sectorAttr$std)
  for( i in 1:loop) {
    
    if(is.numeric(sectorAttr$std[[i]])){ # by industrys
      sectorSTD <- sectorAttr$std[[i]] 
      level <- sectorAttr$level[[i]]
      sectorvar <- if (ret=="ID") paste("Code",level,sep="") else paste("Name",level,sep="")
      TS_ <- TS[,c("date","stockID")]
      
      if(datasrc %in% c("quant","local")){
        TS_$date <- rdate2int(TS_$date)
        qr <- paste(
          "select date,a.stockID,",sectorvar,"as sector_
          from yrf_tmp as a left join LC_ExgIndustry as b
          on a.stockID=b.stockID and InDate<=date and (OutDate>date or OutDate is null)
          where b.Standard=",sectorSTD
        )
        if(datasrc=="quant"){
          con <- db.quant()
          sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
          sqlSave(con,dat=TS_,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)
          re <- sqlQuery(con,query=qr)
          odbcClose(con)
        } else if (datasrc=="local"){
          con <- db.local("main")
          dbWriteTable(con,name="yrf_tmp",value=TS_,row.names = FALSE,overwrite = TRUE)
          re <- dbGetQuery(con,qr)
          dbDisconnect(con)
        }
        re$date <- intdate2r(re$date)
        TS <- merge.x(TS,re,by=c("date","stockID"))
      } else if (datasrc=="memory"){
        memory.load()
        TS_ <- data.table::data.table(TS_)
        LC_ExgIndustry <- .LC_ExgIndustry[Standard==sectorSTD]
        re <- LC_ExgIndustry[TS_,c("date","stockID",paste("x.",sectorvar,sep="")),on=.(stockID,InDate<=date,OutDate>date),with=FALSE]
        re <- renameCol(re,paste("x.",sectorvar,sep=""),"sector_")
        re <- as.data.frame(re)
        TS <- merge.x(TS,re,by=c("date","stockID"))
      }
      if(fillNA){
        TS$sector_ <- sector_NA_fill(sector = TS$sector_, sectorAttr = list(std = sectorSTD, level = level))
      }
    } else { # by factors
      factorList <- sectorAttr$std[[i]]
      level <- sectorAttr$level[[i]]
      tmpTS <- TS[,c("date","stockID")]
      tmpTSF <- getTSF(TS = tmpTS, FactorList = factorList)
      tmpTSF <- dplyr::group_by(tmpTSF, date)
      tmpTSF <- dplyr::mutate(tmpTSF,sector_=cut(rank(-factorscore,na.last="keep"),breaks =level,labels=FALSE))
      tmpTSF <- dplyr::select(tmpTSF,-factorscore)
      TS <- merge.x(TS,tmpTSF, by=c("date","stockID"))
    }
    
    # join together
    if(i==1L){
      TS <-  renameCol(TS,"sector_","sector")
    } else {
      TS$sector <- paste(TS$sector, TS$sector_, sep="_")
      TS <- dplyr::select(TS,-sector_)
    }
  }
  
  # ungroup
  if(!is.null(ungroup)){
    TS <- sector_ungroup(TS,N=ungroup)
  }
  
  # return
  if(drop){
    return(TS[,"sector"])
  } else {
    return(TS)
  }
}




#' is component of specific sector or index?
#'
#' @param TS 
#' @param stockID 
#' @param endT 
#' @param sectorID a character string. The ID of the index, sector or plate. See detail in \code{\link{getComps}}
#' @param drop 
#' @param datasrc 
#' @return a dataframe or a vector(if drop==TRUE) 
#' @export
#' @examples
#' is_component(stockID = c("EQ000001","EQ300089"),sectorID = "EI000300",drop=TRUE)
#' is_component(stockID = c("EQ000001","EQ300089"),sectorID = "ES09440000")
#' TS <- getTS(as.Date(c("2014-01-01","2016-01-01")),indexID = "EI000906")
#' re <- is_component(TS,sectorID = "EI000300")
is_component <- function(TS, stockID, endT=Sys.Date(),
                        sectorID,
                        drop=FALSE,
                        datasrc=defaultDataSRC()){
  if (missing(TS) && missing(stockID)) {
    stop("Param TS and combination of stockID and endT should at least have one!")
  }
  if (!missing(TS) && !missing(stockID)) {
    stop("Param TS and combination of stockID and endT should only have one!")
  }
  if (missing(TS)){
    TS <- expand.grid(date=endT, stockID=stockID)
  }
  indexComp <- getComps(sectorID,unique(TS$date),drop = FALSE,datasrc = datasrc)
  indexComp$is_comp <- 1L
  TSF <- merge.x(TS,indexComp,by=c('date','stockID'))
  TSF[is.na(TSF$is_comp),"is_comp"] <- 0
  
  if(drop){
    return(TSF[,"is_comp"])
  } else {
    return(TSF)
  }
  
}





#' deal with the NA value of sectorID
#' 
#' replace the NA value of sectorID with an "OTHER" sector
#' @param sector a charactor vector of sectorID
#' @param sectorAttr
#' @export
sector_NA_fill <- function(sector, sectorAttr=defaultSectorAttr()){
  Standard=c( 3,      3,      3,      9,      9,      9,      9,      9,      33,   33, 33, 336)
  Level=c(        1,      2,      3,      1,      2,      3,      98,   99,   1,      2,      3,  1)
  IndustryID=c(       'ES0370',  'ES037010',      'ES03701010',  'ES09510000',  'ES09510100',  'ES09510101',  'ES0951000098',         'ES0951000099',      'ES33510000','ES33510100','ES33510101', 'ES6')
  replace_sec_value <- IndustryID[Standard==sectorAttr[[1]]&Level==sectorAttr[[2]]]
  sector[is.na(sector)] <- replace_sec_value
  return(sector)
}


# inner-func
# Turn two-stage group ID to one-stage when group members is less than a certain number.
sector_ungroup <- function(TSS,N=10){
  if(stringr::str_detect(TSS[1,"sector"],'_') && substr(TSS$sector,1,2)=="ES"){
    nsector <- TSS %>% group_by(date,sector) %>% summarise(num=n()) %>% ungroup()
    if(any(nsector$num<N)){
      nsector <- tidyr::separate(nsector,'sector',c("ind","fct"),sep="_",remove=FALSE)
      nsector <- nsector %>% group_by(date,ind) %>% mutate(minnum=min(num)) %>% ungroup()
      nsector <- transform(nsector,
                           sectornew=ifelse(minnum<N,ind,stringr::str_c(ind,fct,sep="_")))
      TSS <- dplyr::left_join(TSS,nsector[,c("date","sector","sectornew")],by=c('date','sector'))
      TSS <- transform(TSS,sector=sectornew,sectornew=NULL)
    }
  }
  return(TSS)
}



#' @rdname getSectorID
#' @export
gf_sector <- function(TS, sectorAttr) {
  TSS <- getSectorID(TS,sectorAttr = sectorAttr,fillNA = TRUE)
  re <- cast_sector(TSS)
  return(re)
}

#' @rdname getSectorID
#' @param TSS a dataframe, with cols: date,stockID,and some dummy variables of sectors.
#' @export
cast_sector <- function(TSS){
  check.TSS(TSS)
  TSS$.tmp <- 1
  re <- reshape2::dcast(TSS,date+stockID~sector,fill=0,value.var = '.tmp')
  TSS$.tmp <- NULL
  re <- merge.x(TSS,re,by = c("date","stockID"))
  return(re)
}

#' check.colnames_sectorfs
#' 
#' @export
check.colnames_sectorfs <- function(data){
  check.colnames(data,"sector")
  cols <- colnames(data)
  if(!any(substr(cols,1,2)=="ES")){
    stop("the data must contain the sector-factors!")
  }
}




#' defaultSectorAttr
#' 
#' get the sectorAttr list. 
#' @param type Currently supporting types : ind, fct, ind_fct, fct_ind
#' @param ind_std vector of integer.
#' @param ind_level vector of integer
#' @param fct_std a \bold{FactorLists} object
#' @param fct_level vector of integer. The number of fct splitted groups.
#' @return A \bold{sectorAttr} object. A list with two items: 
#' \itemize{
#' \item std: a list( or a vector, if sector standards only include industry, not include factors) of sector standard;
#' \item level: a vector of sector level
#' }
#' @export
#' @examples 
#' defaultSectorAttr()
#' defaultSectorAttr("ind",ind_std=c(3,33))
#' defaultSectorAttr("fct") 
#' defaultSectorAttr("ind_fct")
#' defaultSectorAttr("fct_ind")
#' defaultSectorAttr("ind_fct",fct_level=5)
#' defaultSectorAttr("ind_fct",fct_std = RFactorModel::buildFactorLists_lcfs(c("F000001","F000006")),fct_level=c(2,3))
defaultSectorAttr <- function(type = c("ind","fct","ind_fct","fct_ind"), 
                              ind_std = 33, 
                              ind_level=1,
                              fct_std = list(fl_cap()), 
                              fct_level = 3){
  
  type <- match.arg(type)
  if(length(ind_std)>length(ind_level)){
    ind_level <- rep(ind_level,length(ind_std))
  }
  if(length(fct_std)>length(fct_level)){
    fct_level <- rep(fct_level,length(fct_std))
  }
  if(type == "ind"){
    re <- list(std = ind_std, level = ind_level)
  } else if(type =="fct"){
    re <- list(std = fct_std, level = fct_level)
  } else if(type == "ind_fct"){
    re <- list(std = c(as.list(ind_std), fct_std), level = c(ind_level, fct_level))
  } else if (type == "fct_ind"){
    re <- list(std = c(fct_std, as.list(ind_std)), level = c(fct_level, ind_level))
  }
  return(re)
}



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Constant value infomation      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' CT_SystemConst
#' @export
#' @examples
#' CT_SystemConst(LB=1081)
CT_SystemConst <- function(LB,datasrc=defaultDataSRC()){
  qr <- "select * from CT_SystemConst"
  if(datasrc=="local"){
    re <- queryAndClose.dbi(db.local("main"),qr)
  } else if(datasrc=="quant"){
    re <- queryAndClose.odbc(db.quant(),qr)
  } else if(datasrc=="jy"){
    re <- queryAndClose.odbc(db.jy(),qr)[,c("LB","LBMC", "DM","MS")]
  }
  
  if(!missing(LB)){
    re <- re[re$LB==LB,]
  }
  return(re)
}


#' CT_SecuCategory
#' @export
#' @examples
#' CT_SecuCategory(DM=c(1,4))
#' CT_SecuCategory(MS="xxx")
#' CT_SecuCategory()
CT_SecuCategory <- function(DM,MS){
  re <- CT_SystemConst(LB=1177)
  if(!missing(DM)){
    re <- re[re$DM %in% DM, ]
  }
  if(!missing(MS)){
    re <- re[grep(MS,re$MS),]
  }
  return(re)
}
#' CT_SecuMarket
#' @export
#' @examples
#' CT_SecuMarket(DM=c(83,90))
#' CT_SecuMarket(MS="xxx")
#' CT_SecuMarket()
CT_SecuMarket <- function(DM,MS){
  re <- CT_SystemConst(LB=201)
  if(!missing(DM)){
    re <- re[re$DM %in% DM, ]
  }
  if(!missing(MS)){
    re <- re[grep(MS,re$MS),]
  }
  return(re)
}

#' CT_sectorSTD
#' @export
#' @examples
#' CT_sectorSTD(DM=9)
#' CT_sectorSTD(DM=c(3,9))
#' CT_sectorSTD(MS="xxx")
#' CT_sectorSTD()
CT_sectorSTD <- function(DM,MS){
  re <- CT_SystemConst(LB=1081)
  if(!missing(DM)){
    re <- re[re$DM %in% DM, ]
  }
  if(!missing(MS)){
    re <- re[grep(MS,re$MS),]
  }
  return(re)
}

#' CT_industryList
#' @export
#' @examples
#' re <- CT_industryList(std=c(9,3))
#' CT_industryList(std=9,level=1)
#' CT_industryList(ID=c("ES0310","ES0312"))
#' CT_industryList(name="SHENWAN")
CT_industryList <- function(std,level,ID,name,
                            datasrc=defaultDataSRC()){
  
  qr <- "select * from CT_IndustryList"
  if(datasrc=="local"){
    re <- queryAndClose.dbi(db.local("main"),qr)
  } else if(datasrc=="quant"){
    re <- queryAndClose.odbc(db.quant(),qr)
  }
  
  if(!missing(std)){
    re <- re[re$Standard %in% std, ]
  }
  if(!missing(level)){
    re <- re[re$Level %in% level, ]
  }
  if(!missing(ID)){
    re <- re[re$IndustryID %in% ID, ]
  }
  if(!missing(name)){
    re <- re[grep(name,re$IndustryName),]
  }
  return(re)
}


#' CT_TechVars
#' @export
CT_TechVars <- function(datasrc,secuCate,tableName,vars){
  # -- Only local database allowed!
  re <- queryAndClose.dbi(db.local("main"),"select * from CT_TechVars")
  if(! missing(datasrc)){
    re <- re[re$datasrc==datasrc,]
  }
  if(! missing(secuCate)){
    re <- re[re$secuCate==secuCate,]
  }
  if(! missing(tableName)){
    re <- re[re$tableName==tableName,]
  }
  if(! missing(vars)){
    re <- re[re$varName %in% vars,]
  }
  return(re)
}

#' CT_FactorLists
#' @export
CT_FactorLists <- function(factorID,factorName,factorType){
  # -- Only local database allowed!
  re <- queryAndClose.dbi(db.local("fs"),"select * from CT_FactorLists")
  if(! missing(factorID)){
    re <- re[match(factorID,re[,1]),]
  }
  if(! missing(factorName)){
    re <- re[re$factorName %in% factorName,]
  }
  if(! missing(factorType)){
    re <- re[re$factorType %in% factorType,]
  }
  return(re)
}


#' factorID2name
#' @export
factorID2name <- function(factorID){
 re <- CT_FactorLists(factorID)$factorName
 return(re)
}


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Future related      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' getIFlastfirstday
#' 
#' get the first,last and open dates of all IFs.
#' 
#' Note that the "firstday" is the date when the current-month IF switching,while the "openday" is the date the IF IPOing.
#' @param begM beginning month,a yearmon object.
#' @param endM ending month,a yearmon object.
#' @return a dataframe,with cols: date(of class Date),IF01(of class character) and firstday,lastday,openday(of class Date)
#' @export
#' @author Ruifei.Yin
#' @examples
#' getIFlastfirstday()
getIFlastfirstday <- function(begM=zoo::as.yearmon("2010-05"),endM=zoo::as.yearmon(Sys.Date())){
  dt <- seq(from=zoo::as.Date(zoo::as.yearmon("2010-05")),to=Sys.Date()+1/12,by="month")  
  IF01 <- paste("IF",substr(as.character(dt),3,4),substr(as.character(dt),6,7),sep="")
  # ---- lastday
  lastday0 <- as.Date(timeDate::timeNthNdayInMonth(dt,5,3))
  lastday <- as.Date(ifelse(trday.is(lastday0)|(lastday0>=Sys.Date()),lastday0,trday.nearby(lastday0,1))) 
  # ---- firstday
  lastday_lag <-lag.m(lastday,1,TRUE)   
  firstday <- as.Date(ifelse(dt!=as.Date("2010-05-01"),trday.nearby(lastday_lag,1),as.Date("2010-04-16")))
  firstday <- as.Date(ifelse(lastday_lag>=Sys.Date()&(!is.na(lastday_lag)),lastday_lag+3,firstday)) # this is a very tricky method!  
  # ---- openday
  openday <- vector(length=length(dt))
  for(ii in 1:length(dt)){
    mm <-month(dt[ii]) 
    if(mm %in% c(3,6,9,12)){
      if(ii<=7){
        openday[ii] <-as.Date("2010-04-16")
      } else {
        openday[ii] <-firstday[ii-7]
      } 
    } else {
      if(ii<=1){
        openday[ii] <-as.Date("2010-04-16")
      } else {
        openday[ii] <-firstday[ii-1]
      }
    }
  }
  openday <- as.Date(openday)
  # ---- merge and subset
  re <- data.frame(date=dt,IF01,firstday,lastday,openday,stringsAsFactors=FALSE)
  begM <- max(zoo::as.yearmon("2010-05"),begM)
  endM <- min(zoo::as.yearmon(Sys.Date())+1/12,endM)
  re <- re[re$date>=as.Date(begM)&re$date<=as.Date(endM),]
  return(re)
}


#' getIFlist
#' 
#' get the list(the 4 future at one time) of codes of all CSI300 index futures and the first and last day of every current-month future.
#' @param begM beginning month,a yearmon object.
#' @param endM ending month,a yearmon object.
#' @return a dataframe,with cols: date,IF01,IF02,IF03,IF04,firstday,lastday,openday
#' @export
#' @author Ruifei.Yin
#' @examples
#' getIFlist()
getIFlist <- function(begM=zoo::as.yearmon("2010-05"),endM=zoo::as.yearmon(Sys.Date())){
  begM <- max(zoo::as.yearmon("2010-05"),begM)
  endM <- min(zoo::as.yearmon(Sys.Date())+1/12,endM)
  d1 <- seq(from=as.Date(begM),to=as.Date(endM),by="month")
  d2 <- d1+months(1)
  d3 <- vector(length=length(d1))
  for(i in 1:length(d1)){
    mm <-month(d1[i]) 
    if(mm %in% c(1,4,7,10)){d3[i] <-d1[i]+months(2)}
    if(mm %in% c(2,5,8,11)){d3[i] <-d1[i]+months(4)}
    if(mm %in% c(3,6,9,12)){d3[i] <-d1[i]+months(3)}
  }
  d3 <- as.Date(d3)
  d4 <- d3+months(3)
  IF01 <- paste("IF",substr(as.character(d1),3,4),substr(as.character(d1),6,7),sep="")
  IF02 <- paste("IF",substr(as.character(d2),3,4),substr(as.character(d2),6,7),sep="")
  IF03 <- paste("IF",substr(as.character(d3),3,4),substr(as.character(d3),6,7),sep="")
  IF04 <- paste("IF",substr(as.character(d4),3,4),substr(as.character(d4),6,7),sep="")
  IFlist <- data.frame(date=d1,IF01,IF02,IF03,IF04,stringsAsFactors=FALSE)
  IFlastfirstday <- getIFlastfirstday(begM,endM)
  re <- merge(IFlist,IFlastfirstday)
  return(re)
}

#' getIFcontinuousCode
#' 
#' get the continuous index future code on every trading day
#' @param begT
#' @param endT 
#' @return a datafame with cols:tradingday,IF00,IF01,IF02,IF03,IF04,firstday,lastday...
#' @export
#' @author Ruifei.Yin
#' @examples
#' begT <- as.Date("2010-04-16")
#' endT <- Sys.Date()
#' getIFcontinuousCode()
getIFcontinuousCode <- function(begT=as.Date("2010-04-16"),endT=Sys.Date()){
  tradingday <- data.frame(tradingday=trday.get(begT,endT),stringsAsFactors=FALSE)
  IFlist <- getIFlist() 
  sqlchar1 <- "select * from tradingday as a ,IFlist as b 
  where a.tradingday <= b.lastday and a.tradingday >= b.firstday"
  re <- sqldf(sqlchar1) 
  return(re)
}



#' getIFrtn
#' 
#' get index future daily return series
#' @param code the code of the future(eg."IF00","IF01","IF02"...)
#' @param begT an object of class "Date"
#' @param endT an object of class "Date"
#' @param adj a logical,with default FALSE. if true, the daily return of future will be aligned with the trading time(starting from 9:30 and ending to 15:30) of CSI300 index.
#' @return the daily return series of class xts.
#' @export
#' @author Ruifei.Yin
#' @examples
#' begT <- as.Date("2011-01-01")
#' endT <- as.Date("2011-02-01")
#' rtn <- getIFrtn("IF00",begT,endT)
#' rtn.adj <- getIFrtn("IF00",begT,endT,adj=TRUE)
getIFrtn <- function(code,begT,endT,adj=FALSE){
  qt <- getQuote_ts(code,begT,endT,variables=c("price","yclose"),Cycle="cy_15m()")
  rtn <- qt$price/qt$yclose-1
  time <- qt$date
  time.adj<- time
  for(ii in 1:length(time)){
    if(lubridate::hour(time[ii])==15 & lubridate::minute(time[ii])==15){
      time.adj[ii] <- time[ii+1]-3600
    }
  }
  if(adj){
    rtn.xts <- xts(rtn,time.adj)
  } else {
    rtn.xts <- xts(rtn,time)
  }  
  rtn.daily <- aggregate(rtn.xts,by=zoo::as.Date,PerformanceAnalytics::Return.cumulative)
  colnames(rtn.daily) <- code
  return(rtn.daily)
}







# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Mutual Fund related      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' mutual fund stats functions
#'
#' @name MF_funcs
#' @examples
#' #---MF_getQuote--------------------------------------------------------------
#' fundID <- c("519983.OF","519987.OF")
#' begT <- as.Date("2016-01-05")
#' # -- get one variable from wind
#' variables <- "NAV_adj_return1"
#' re <- MF_getQuote(fundID = fundID, begT = begT, variables = variables)
#' # -- get multiple variables from wind
#' variables <- "nav,NAV_adj,NAV_adj_return1"
#' re <- MF_getQuote(fundID = fundID, begT = begT, variables = variables)
#' # -- get one variable from jy
#' variables <- "NVDailyGrowthRate"
#' re <- MF_getQuote(fundID = fundID, variables = variables,datasrc = 'jy')
#' variables <- "UnitNV,NVDailyGrowthRate"
#' re <- MF_getQuote(fundID = fundID, variables = variables,datasrc = 'jy')
#' 
#' #---MF_getStockPort----------------------------------------------------------
#' fundID <- c("519983.OF","519987.OF")
#' re <- MF_getStockPort(fundID,as.Date("2016-09-30"),mode="top10",datasrc = "jy")
#' 
#' #---MF_Turnover_annual-------------------------------------------------------
#' begrptDate <- as.Date("2015-06-30")
#' endrptDate <- as.Date("2016-12-31")
#' fundID <- c("519983.OF","519987.OF")
#' re <- MF_Turnover_annual(fundID, begrptDate, endrptDate)
#' 
#' #---MF_setupday-------------------------------------------------------
#' re <- MF_setupday(fundID)
#' 
#' #---MF_nav_stat----------------------------------------------------------
#' # -- get one fund's nav stats from set up date.
#' mfstat <- MF_nav_stat(fundID='100038.OF')
#' # -- get one fund's yearly nav stats from set up date.
#' mfstat <- MF_nav_stat(fundID='100038.OF',freq='year')
#' # -- get mutiple funds' nav stats from specified date.
#' fundID <- c('162411.OF','501018.OF')
#' begT <- as.Date('2016-01-04')
#' endT <- as.Date('2017-12-31')
#' mfstat <- MF_nav_stat(fundID=fundID,begT=begT,endT=endT)
#' # -- define benchmark by yourself.
#' fundID <- c('000978.OF','000877.OF')
#' bmk <- c('EI000905','EI000300')
#' mfstat <- MF_nav_stat(fundID=fundID,bmk=bmk,datasrc='jy')
#' # -- pass raw data by yourself.
#' mfstat <- MF_nav_stat(fundnav=fundnav)
NULL



#' \code{MF_getQuote} get fund's quote from multiple data source.
#' 
#' @rdname MF_funcs
#' @param variables see examples.
#' @export
MF_getQuote <- function(fundID,begT,endT,variables="NAV_adj_return1",datasrc = c("wind","jy","ts"),NAfill=FALSE){
  datasrc <- match.arg(datasrc)
  
  if(missing(begT)){
    begT <- MF_setupday(fundID,datasrc = datasrc)
  }
  if(missing(endT)){
    endT <- trday.nearby(Sys.Date(),-1)
  }
  fundinfo <- data.frame(fundID=fundID,begT=begT,endT=endT,stringsAsFactors = FALSE)
  
  if(datasrc == "jy"){
    qr <- paste("select convert(varchar,f.EndDate,112) 'date',
                s.SecuCode+'.OF' 'fundID',",variables,
                "from MF_NetValue f,SecuMain s
                where f.InnerCode=s.InnerCode
                and s.SecuCode in",brkQT(stringr::str_replace_all(fundID,'.OF','')),
                " order by f.EndDate,s.SecuCode")
    fundts <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors = FALSE)
    fundts <- fundts %>% dplyr::mutate(date=intdate2r(date))
    tradingdays <- trday.get(min(fundts$date),max(fundts$date)) #remove untrading days
    fundts <- fundts %>% dplyr::filter(date %in% tradingdays) %>% 
      dplyr::left_join(fundinfo,by='fundID') %>% 
      dplyr::filter(date>=begT,date<=endT) %>% dplyr::select(-begT,-endT)
    
  }else if(datasrc == "ts"){
    #
  }else if(datasrc == "wind"){
    # NAV_adj_return1 : fu quan dan wei jing zhi zeng zhang lv (in % unit)
    # NAV_adj: fu quan dan wei jing zhi, houfuquan
    require(WindR)
    w.start(showmenu = FALSE)
    
    fundts <- data.frame()
    for(i in 1:nrow(fundinfo)){
      fundts_ <- w.wsd(fundinfo$fundID[i],variables,fundinfo$begT[i],fundinfo$endT[i])[[2]]
      fundts_ <- fundts_ %>% dplyr::rename(date=DATETIME) %>% dplyr::mutate(fundID=fundinfo$fundID[i]) %>% 
        dplyr::select(date,fundID,dplyr::everything())
      fundts <- rbind(fundts,fundts_)
    }
  }
  
  if(NAfill){
    fundts[is.na(fundts)] <- 0
  }
  return(fundts)
}






#' \code{MF_getStockPort} get fund's stock portfolio from financial reports.
#' 
#' @rdname MF_funcs
#' @export
MF_getStockPort <- function(fundID,rptDate,mode=c("all","top10"),datasrc = c("jy","ts","wind")){
  #variables=c("date","stockID","wgt")
  datasrc <- match.arg(datasrc)
  mode <- match.arg(mode)
  if(datasrc == "jy"){
    fundID <- substr(fundID,1,6)
    fundIDqr <- paste(fundID,collapse = "','")
    if(mode == "all"){
      sheetname <- "MF_StockPortfolioDetail"
    }else if(mode == "top10"){
      sheetname <- "MF_KeyStockPortfolio"
    }
    qr <- paste0("select convert(varchar(8),A.ReportDate,112) rptDate, A.RatioInNV wgt, B.SecuCode fundID, C.SecuCode stockID
                 from JYDB.dbo.",sheetname," A,
                 JYDB.dbo.SecuMain B, JYDB.dbo.SecuMain C
                 where A.InnerCode = B.InnerCode
                 and A.StockInnerCode = C.InnerCode
                 and B.SecuCode in ('",fundIDqr,"')")
    tmpdat <- queryAndClose.odbc(db.jy(),qr)
    tmpdat$stockID <- paste0('EQ',substr(tmpdat$stockID + 1000000,2,7))
    tmpdat$fundID <- paste0(substr(tmpdat$fundID + 1000000,2,7),".OF")
    tmpdat$rptDate <- intdate2r(tmpdat$rptDate)
    tmpdat <- tmpdat[tmpdat$rptDate %in% rptDate,]
  }
  re <- tmpdat[,c("fundID","rptDate","stockID","wgt")]
  re <- renameCol(re,"rptDate","date")
  rownames(re) <- NULL
  return(re)
}


#' \code{MF_Turnover_annual} get fund's annual turnover rate.
#' 
#' @rdname MF_funcs
#' @export
MF_Turnover_annual <- function(fundID,begrptDate,endrptDate){
  
  # buy in and sell out db
  fundID <- substr(fundID,1,6)
  fundIDqr <- paste(fundID, collapse="','")
  qr <- paste0("select  B.SecuCode fundID, convert(varchar(8),A.ReportDate,112) rptDate,
               A.BuyingCost+A.SellingIncome value
               from JYDB.dbo.MF_FundTradeInfo A,
               JYDB.dbo.SecuMain B
               where A.InnerCode = B.InnerCode
               and B.SecuCode in ('",fundIDqr,"')")
  tmpdat <- queryAndClose.odbc(db.jy(),qr)
  tmpdat$rptDate <- intdate2r(tmpdat$rptDate)
  tmpdat$fundID <- substr(tmpdat$fundID + 1000000, 2, 7)
  # mkt_value db
  qr2 <- paste0("select convert(varchar(8),A.ReportDate,112) rptDate,
                A.MarketValue mkt_cap, B.SecuCode fundID
                from JYDB.dbo.MF_StockPortfolioDetail A,
                JYDB.dbo.SecuMain B
                where A.InnerCode = B.InnerCode
                and B.SecuCode in ('",fundIDqr,"')")
  tmpdat2 <- queryAndClose.odbc(db.jy(), qr2)
  tmpdat2 <- dplyr::group_by(tmpdat2, rptDate, fundID)
  tmpdat2 <- dplyr::summarise(tmpdat2, mkt_sum = sum(mkt_cap))
  tmpdat2$rptDate <- intdate2r(tmpdat2$rptDate)
  tmpdat2$fundID <- substr(tmpdat2$fundID + 1000000, 2, 7)
  # computing process
  finalre <- data.frame()
  for( i in 1:length(fundID)){
    # numerator
    tmpdat_ <- tmpdat[tmpdat$fundID == fundID[i],]
    tmpdat_ <- dplyr::arrange(tmpdat_, rptDate)
    tmpdat_ <- subset(tmpdat_, rptDate >= begrptDate & rptDate <= endrptDate)
    ind_ <- substr(tmpdat_$rptDate,6,10) == "12-31"
    ind_[length(ind_)] <- TRUE
    if(ind_[1] == FALSE){
      ind_[1] <- TRUE
      tmpdat_$value[1] <- tmpdat_$value[1]*(-1)
    }else if(ind_[1] == TRUE){
      ind_[1] <- FALSE
    }
    subre_ <- tmpdat_[ind_,]
    nominator_ <- sum(subre_$value)
    # denominator
    tmpdat2_ <- tmpdat2[tmpdat2$fundID == fundID[i],]
    tmpdat2_ <- subset(tmpdat2_, rptDate > begrptDate & rptDate <= endrptDate)
    denominator_ <- mean(tmpdat2_$mkt_sum)
    # years
    yy_ <- nrow(tmpdat2_)*0.5
    # output
    re_ <- nominator_/yy_/2/denominator_
    finalre_ <- data.frame("fundID" = fundID[i], "turnover_ann" = re_)
    finalre <- rbind(finalre, finalre_)
  }
  # output
  finalre$fundID <- paste0(finalre$fundID,".OF")
  return(finalre)
}

#' \code{MF_setupday} get fund's set up date.
#' 
#' @rdname MF_funcs
#' @export
MF_setupday <- function(fundID,datasrc = c('jy','wind')){
  datasrc <- match.arg(datasrc)
  if(datasrc=='wind'){
    require(WindR)
    w.start(showmenu = FALSE)
    f_setup_date <- w.wss(fundID,'fund_setupdate')[[2]]
    f_setup_date <- f_setup_date %>% dplyr::mutate(FUND_SETUPDATE=w.asDateTime(FUND_SETUPDATE,asdate = TRUE))
    re <- f_setup_date[match(fundID,f_setup_date[,'CODE']),2]  
    
  }else if(datasrc=='jy'){
    qr <- paste("select s.SecuCode+'.OF' 'fundID',
                convert(varchar,f.EstablishmentDate,112) 'begT'  
                from MF_FundArchives f,SecuMain s
                where f.InnerCode=s.InnerCode
                and s.SecuCode in ",brkQT(stringr::str_replace_all(fundID,'.OF','')))
    fundinfo <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors = FALSE)
    fundinfo <- transform(fundinfo,begT=intdate2r(begT))
    re <- fundinfo[match(fundID,fundinfo[,'fundID']),2]  
  }
  return(re)  
}


#' \code{MF_nav_stat} get fund's nav stats.
#' 
#' @rdname MF_funcs
#' @param fundID is vector of fundID.
#' @param begT is a vector of begin date,can be missing,if missing,then fund's set up date will be begT
#' @param endT is end date,can be missing,if missing,then the nearest trading day for \bold{today} will be endT.
#' @param freq is fund's nav statistic frequency,default value is \code{NULL},same as \code{\link[QUtility]{rtn.periods}}.
#' @param scale is number of periods in a year,for daily return default value is 250,for monthly return default value is 12.
#' @param datasrc
#' @param fundnav is a data frame with four columns,contains \code{date},\code{fundID},\code{nav_rtn},\code{bmk_rtn}.fundnav can be missing,if missing,this function with get data from specified data source.
#' @export
MF_nav_stat <- function(fundID,begT,endT,bmk,freq=NULL,scale=250,datasrc=c('wind','jy'),fundnav){
  
  if(missing(fundnav)){
    datasrc <- match.arg(datasrc)
    
    #get begT and endT
    if(missing(endT)){
      endT <- trday.nearby(Sys.Date(),-1)
    }
    setupday <- MF_setupday(fundID,datasrc = datasrc)
    
    if(missing(begT)){
      f_info <- data.frame(fundID=fundID,begT=setupday,endT=endT,stringsAsFactors = FALSE)
    }else{
      f_info <- data.frame(fundID=fundID,begT=begT,endT=endT,tmpday=setupday,stringsAsFactors = FALSE)
      f_info <- f_info %>% dplyr::mutate(begT=ifelse(begT<tmpday,tmpday,begT)) %>% 
        dplyr::mutate(begT=as.Date(begT,origin = "1970-01-01"))%>% dplyr::select(-tmpday)
    }
    
    
    #get fund's nav return
    if(datasrc=='wind'){
      fundnav <- MF_getQuote(fundID = f_info$fundID, begT = f_info$begT, endT = f_info$endT, variables = "NAV_adj_return1",datasrc = 'wind',NAfill = TRUE)
      fundnav <- fundnav %>% dplyr::rename(nav_rtn=NAV_ADJ_RETURN1) %>% dplyr::mutate(nav_rtn=nav_rtn/100)
    }else if(datasrc=='jy'){
      fundnav <- MF_getQuote(fundID = f_info$fundID, begT = f_info$begT, endT = f_info$endT, variables = "NVDailyGrowthRate",datasrc = 'jy',NAfill = TRUE)
      fundnav <- fundnav %>% dplyr::rename(nav_rtn=NVDailyGrowthRate)
    }
    
    #get benchmark's pct_chg
    result <- data.frame()
    if(missing(bmk)){
      require(WindR)
      w.start(showmenu = FALSE)
      bmk <- w.wss(f_info$fundID,'fund_benchindexcode')[[2]]
      f_info <- transform(f_info,benchindexcode=bmk$FUND_BENCHINDEXCODE)
      bmkqt <- data.frame()
      for(i in 1:nrow(f_info)){
        bmkqt_ <-w.wsd(f_info$benchindexcode[i],"pct_chg",f_info$begT[i],f_info$endT[i])[[2]]
        bmkqt_ <- bmkqt_ %>% dplyr::rename(date=DATETIME,bmk_rtn=PCT_CHG) %>% 
          dplyr::mutate(fundID=f_info$fundID[i],bmk_rtn=bmk_rtn/100) %>% dplyr::select(date,fundID,bmk_rtn)
        bmkqt <- rbind(bmkqt,bmkqt_)
      }
      fundnav <- fundnav %>% dplyr::left_join(bmkqt,by=c('date','fundID'))
      
    }else{
      f_info <- f_info %>% dplyr::mutate(benchindexcode=bmk)
      bmkqt <- getIndexQuote(bmk,begT = min(f_info$begT),endT=max(f_info$endT),variables = 'pct_chg',datasrc = 'jy')
      bmkqt <- bmkqt %>% dplyr::rename(benchindexcode=stockID,bmk_rtn=pct_chg) %>% 
        dplyr::mutate(benchindexcode=as.character(benchindexcode))
      fundnav <- fundnav %>% dplyr::left_join(f_info[,c('fundID','benchindexcode')],by='fundID') %>% 
        dplyr::left_join(bmkqt,by=c('date','benchindexcode')) %>% dplyr::select(-benchindexcode)
    }
  }
  
  #stats inner function
  navstats_innerfunc <- function(fnav,scale){
    fnav <- fnav %>% dplyr::mutate(exc_rtn=nav_rtn-bmk_rtn)
    fundstat <- fnav %>% dplyr::group_by(fundID) %>%
      dplyr::summarise(nyear=as.numeric(max(date)-min(date))/365,
                       rtn=prod(1+nav_rtn)-1,rtn_ann=(1+rtn)^(1/nyear)-1,
                       bench=prod(1+bmk_rtn)-1,bench_ann=(1+bench)^(1/nyear)-1,
                       alpha=rtn-bench,alpha_ann=rtn_ann-bench_ann,
                       hitratio=sum(exc_rtn>0)/n(),
                       bias=mean(abs(exc_rtn)),
                       TE=sqrt(sum(exc_rtn^2)/(n()-1)) * sqrt(scale),
                       alphaIR=alpha_ann/TE,
                       rtn_min=min(exc_rtn),
                       rtn_max=max(exc_rtn)) %>% dplyr::ungroup()
    
    #get two dates
    sdate <- fnav %>% dplyr::select(date,fundID,exc_rtn) %>%
      dplyr::left_join(fundstat[,c('fundID','rtn_min','rtn_max')],by='fundID')
    sdate_ <- sdate %>% dplyr::filter(exc_rtn==rtn_max) %>% dplyr::group_by(fundID) %>%
      dplyr::summarise(rtn_maxdate=min(date)) %>% dplyr::ungroup()
    sdate <- sdate %>% dplyr::filter(exc_rtn==rtn_min) %>% dplyr::group_by(fundID) %>%
      dplyr::summarise(rtn_mindate=min(date)) %>% dplyr::ungroup() %>% dplyr::left_join(sdate_,by='fundID')
    fundstat <- fundstat %>% dplyr::left_join(sdate,by='fundID')
    
    #get max drawdown
    maxDD <- split(fnav,fnav$fundID)
    maxDDinnerfunc <- function(df,varname='exc_rtn'){
      df <- reshape2::dcast(df,date~fundID,value.var = varname,fill = 0)
      fnames <- colnames(df)[-1]
      df <- xts::xts(df[,-1],order.by = df[,1])
      df <- PerformanceAnalytics::table.Drawdowns(df,1)
      df <- df[,c("Depth","From","Trough")]
      colnames(df) <- c('alphamaxDD','maxDDbegT','maxDDendT')
      return(df)
    }
    maxDD <- plyr::ldply(maxDD,maxDDinnerfunc,.id = 'fundID')
    maxDD <- maxDD %>% dplyr::mutate(fundID=as.character(fundID))
    
    fundstat <- fundstat %>% dplyr::left_join(maxDD,by='fundID')
    fundstat <- as.data.frame(fundstat)
    return(fundstat)
  }
  
  #stats
  if(is.null(freq)){
    fundstat <- navstats_innerfunc(fundnav,scale=scale)
  }else{
    fundnav <- fundnav %>% dplyr::mutate(date_break=as.Date(cut.Date2(date,breaks =freq)))
    fundnav <- split(fundnav,fundnav$date_break)
    fundstat <- plyr::ldply(fundnav,navstats_innerfunc,scale=scale,.id = 'date')
    fundstat <- fundstat %>% dplyr::mutate(date=as.Date(date))%>% dplyr::arrange(date,fundID)
  }
  return(fundstat)
}







# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    rptDate related      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

# --------------------  ~~ rptDate funcs ----------------

#' rptDate.is
#' 
#' Is it a proper report date?
#' @param rptDate a vector of rptDate , with class Date.
#' @return a logical vector with the same length of rptDate.
#' @export
#' @author Ruifei.Yin
#' @seealso \code{\link{check.rptDate}}
#' @examples
#' rptDate.is(ymd(c(20100103,20141231,20130630)))
#' rptDate.is(ymd(c(20100630,20141231,20130930)))
#' rptDate.is(ymd(c(20100630,20141231,20130930)),"h")
rptDate.is <- function(rptDate,freq=c("q","y","h")){
  freq <- match.arg(freq)
  md <- 100*lubridate::month(rptDate) + lubridate::day(rptDate)
  if(freq=="q"){
    re <- md %in% c(331,630,930,1231)
  } else if(freq=="h"){
    re <- md %in% c(630,1231)
  } else {
    re <- md %in% c(1231)
  }
  return(re)
}

#' check.rptDate
#' 
#' Is it a proper rptDate object? If TRUE, return NULL; else return an error.
#' @param rptDate a vector of rptDate , with class Date.
#' @export
#' @author Ruifei.Yin
#' @seealso \code{\link{rptDate.is}}
#' @examples
#' check.rptDate(ymd(c(20100103,20141231,20130630)))
#' check.rptDate(ymd(c(20100630,20141231,20130930)))
check.rptDate <- function(rptDate){
  rptDate <- na.omit(rptDate) # omit the NA
  re <- rptDate.is(rptDate)
  re_all <- all(re)
  if(!re_all) {
    warning("rptDate is not proper report date!") 
    cat("Following is not proper rptDate:\n")
    print(rptDate[!re])
  } 
}


#' rptDate.yoy
#' @param rptDate a vector of rptDate , with class Date.
#' @return a vector of yoy rptDate.
#' @export
#' @author Ruifei.Yin
#' @examples
#' rptDate.yoy(lubridate::ymd(c(20100630,20141231,20130930)))
rptDate.yoy <- function(rptDate){
  check.rptDate(rptDate)
  re <- rptDate - lubridate::years(1)
  re <- as.Date(re,tz="")
  return(re)
}

#' rptDate.qoq
#' @param rptDate a vector of rptDate , with class Date.
#' @return a vector of qoq rptDate.
#' @export
#' @author Ruifei.Yin
#' @examples
#' rptDate.qoq(lubridate::ymd(c(20100630,20141231,20130930)))
rptDate.qoq <- function(rptDate){
  check.rptDate(rptDate)
  re <- rptDate %m-% months(3)
  re <- lubridate::ceiling_date(re,unit="month") - lubridate::days(1)
  re <- as.Date(re,tz="")
  return(re)
}



#' rptDate.offset
#' @param rptDate a vector of rptDate , with class Date.
#' @param by a vector of integer
#' @param freq charactor string: "y" or "q".
#' @return a vector or a dataframe according to the length of rptDate and by.
#' @export
#' @author Ruifei.Yin
#' @examples
#' # multi rptDate and 1 by:
#' rptDate.offset(as.Date(c("2016-03-31","2016-06-30","2016-09-30")),-1,"q")
#' rptDate.qoq(as.Date(c("2016-03-31","2016-06-30","2016-09-30")))
#' rptDate.offset(as.Date(c("2016-03-31","2016-06-30","2016-09-30")),-1,"y")
#' rptDate.yoy(as.Date(c("2016-03-31","2016-06-30","2016-09-30")))
#' # 1 rptDate and multi by:
#' rptDate.offset(as.Date("2016-03-31"),-2:0,"q")
#' # multi rptDate and multi by:
#' rptDate.offset(as.Date(c("2016-03-31","2016-06-30","2016-09-30")),-2:0,"q")
rptDate.offset <- function(rptDate,by,freq){
  check.rptDate(rptDate)
  if(length(rptDate)>1 && length(by)>1){
    for(i in 1:length(by)){
      re_ <- rptDate.offset(rptDate = rptDate, by=by[i], freq=freq)
      re_ <- data.frame(re_)
      if(i==1L){
        re <- re_
      } else{
        re <- cbind(re,re_)
      }
    }
    colnames(re) <- paste(freq,by,sep = "")
  } else {
    if(freq=="y"){
      re <- rptDate + lubridate::years(by)
    }else if(freq=="q"){
      re <- rptDate %m+% months(by*3)
      re <- lubridate::ceiling_date(re,unit="month") - lubridate::days(1)
    }else if(freq=="h"){
      re <- rptDate %m+% months(by*6)
      re <- lubridate::ceiling_date(re,unit="month") - lubridate::days(1)
    }else{
      stop("uncorrect freq!")
    }
    re <- as.Date(re,tz="")
  }
  return(re)
}



#' rptDate.deadline
#' return the deadline of rptDate.
#' @param rptDate a vector of rptDate, with class Date.
#' @return a vector of Date
#' @export
#' @author Ruifei.Yin
#' @examples
#' rptDate.deadline(ymd(c(20100630,20141231,20130930)))
rptDate.deadline <- function(rptDate){
  check.rptDate(rptDate)
  q <- lubridate::quarter(rptDate)
  y <- lubridate::year(rptDate)  
  re <- ISOdate(y,4,30,tz="")
  re[q==2] <- ISOdate(y[q==2], 8, 31, tz="")
  re[q==3] <- ISOdate(y[q==3], 10, 31, tz="")
  re[q==4] <- ISOdate(y[q==4]+1, 4, 30, tz="")
  re <- as.Date(re,tz="")
  return(re)
}

#' rptDate.yearrpt
#' @param datelist a vector of date , with class Date.
#' @return a vector of yearly rptDate.
#' @export
#' @author Ruifei.Yin
#' @examples
#' rptDate.nearest(as.Date(c("2015-01-11","2016-06-21","2016-12-31")),"q",1)
#' rptDate.nearest(as.Date(c("2015-01-11","2016-06-21","2016-12-31")),"y",-1)
rptDate.nearest <- function(datelist,freq=c("q","y","h"),dir=-1L){
  freq <- match.arg(freq)
  if(freq=="y"){
    if(dir==-1L){
      re <- dplyr::if_else(rptDate.is(datelist,freq = "y"),datelist,
                           lubridate::floor_date(datelist,unit="year") - lubridate::days(1))
    } else {
      re <- lubridate::ceiling_date(datelist,unit="year") - lubridate::days(1)
    }
  } else if(freq=="q"){
    if(dir==-1L){
      re <- dplyr::if_else(rptDate.is(datelist,freq = "q"),datelist,
                           lubridate::floor_date(datelist,unit="quarter") - lubridate::days(1))
    } else {
      re <- lubridate::ceiling_date(datelist,unit="quarter") - lubridate::days(1)
    }
  } else if(freq=="h"){
    if(dir==-1L){
      re <- dplyr::if_else(rptDate.is(datelist,freq = "h"),datelist,
                           lubridate::floor_date(datelist,unit="halfyear") - lubridate::days(1))
    } else {
      re <- lubridate::ceiling_date(datelist,unit="halfyear") - lubridate::days(1)
    }
  }
  re <- as.Date(re,tz="")
  return(re)
}





#' rptDate.yearrpt
#' @param begT
#' @param endT
#' @param freq
#' @param dir 1L,-1L,or 0L
#' @return a vector of yearly rptDate.
#' @export
#' @author Ruifei.Yin
#' @examples
#' rptDate.get(as.Date("2011-02-06"),as.Date("2011-10-23"),freq="h",dir=1)
#' rptDate.get(as.Date("2011-12-31"),as.Date("2013-06-30"),freq="q",dir=0)
rptDate.get <- function(begT, endT, freq=c("q","y","h"), dir=0L){
  freq <- match.arg(freq)
  datelist <- c(seq(begT,endT,by='month'),endT)
  if(dir==-1L){
    re <- rptDate.nearest(datelist = datelist, freq = freq, dir = -1L)
  }else if(dir==1L){
    re <- rptDate.nearest(datelist = datelist, freq = freq, dir = 1L)
  }else{
    re <- rptDate.nearest(datelist = datelist, freq = freq, dir = -1L)
    re <- re[re>=begT & re<=endT]
  }
  re <- unique(re)
  return(re)
}



#' rptDate.publ
#'
#' @export
#' @author Aming.Tao
#' @examples
#' rptTS <- getrptTS(begT=as.Date("2016-02-06"),endT=as.Date("2017-8-23"),univ='EI000300')
#' re <- rptDate.publ(rptTS)
#' rptDate <- rptDate.get(as.Date("2011-02-06"),as.Date("2013-10-23"))
#' re <- rptDate.publ(rptDate=rptDate,stockID=c("EQ000001","EQ000002"))
rptDate.publ <- function(rptTS,rptDate,stockID,datasrc=defaultDataSRC()){
  if (missing(rptTS) && any(missing(stockID),missing(rptDate))) {
    stop("Param rptTS and combination of stockID and rptDate should at least have one!")
  }
  if (!missing(rptTS) && !all(missing(stockID),missing(rptDate))) {
    stop("Param rptTS and combination of stockID and rptDate should only have one!")
  }
  if (missing(rptTS)){
    rptTS <- expand.grid(rptDate=rptDate, stockID=stockID,stringsAsFactors = FALSE)
  }
  check.rptTS(rptTS)
  
  if(datasrc=="quant"){
    rptTS <- transform(rptTS,rptDate=rdate2int(rptDate))
    con <- db.quant()
    qr <- paste("select a.*,b.PublDate from yrf_tmp a left join LC_RptDate b on a.stockID=b.stockID and a.rptDate=b.EndDate")
    sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
    sqlSave(con,dat=rptTS,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
    re <- sqlQuery(con,query=qr)
    odbcClose(con)
    re <- transform(re,rptDate=intdate2r(rptDate))
  }else if(datasrc=="local"){
    rptTS <- transform(rptTS,rptDate=rdate2int(rptDate))
    con <- db.local("main")
    dbWriteTable(con,name="yrf_tmp",value=rptTS,row.names = FALSE,overwrite = TRUE)
    qr <- paste("select a.*,b.PublDate from yrf_tmp a left join LC_RptDate b on a.stockID=b.stockID and a.rptDate=b.EndDate")
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
    re <- transform(re,rptDate=intdate2r(rptDate))
  }else if(datasrc=="ts"){
    re <- rptTS.getFin_ts(rptTS,'"PublDate",report(128006,RDate)')
  }
  re <- transform(re,PublDate=intdate2r(PublDate))
  return(re)
}




# --------------------  ~~ rptTS funcs ----------------
#' getrptTS
#' @param begT
#' @param endT
#' @param freq
#' @param dir 1L,-1L,or 0L
#' @param univ
#' @param rptDates
#' @param stocks
#' @return a \bold{rptTS} object.a dataframe,with cols:
#'   \itemize{
#'   \item date: the rptDates
#'   \item stockID: the stockID 
#'   }.IF rptDates or stocks is not missing then use them directly, else get rptDates and stocks by \code{\link{rptDate.get}} and \code{\link{getComps}}. 
#' @export
#' @author Ruifei.Yin
#' @examples
#' re1 <- getrptTS(begT=as.Date("2011-02-06"),endT=as.Date("2011-10-23"),univ="EI000300")
#' re2 <- getrptTS(rptDates=as.Date(c("2016-03-31","2016-09-30")),univ="EI000300")
#' re3 <- getrptTS(rptDates=as.Date(c("2016-03-31","2016-09-30")),stocks=c("EQ000001","EQ000002"))
#' re4 <- getrptTS(begT=as.Date("2011-02-06"),endT=as.Date("2011-10-23"),stocks=c("EQ000001","EQ000002"))
getrptTS <- function(begT,endT,freq=c("q","y","h"),dir=0,univ,rptDates,stocks){
  freq <- match.arg(freq)
  if(missing(rptDates)){
    rptDates <- rptDate.get(begT = begT, endT = endT, freq = freq, dir = dir)
  }
  check.rptDate(rptDates)
  if(missing(stocks)){
    re <- getComps(ID=univ,endT=rptDates,drop=FALSE)
    re <- renameCol(re,"date","rptDate")
  } else {
    re <- expand.grid(rptDate=rptDates,stockID=stocks)
  }
  return(re)
}






#' rptTS.getFin
#' 
#' get financtial indicators via \bold{rptTS} through WindR, or TinySoft. 
#' @rdname rptTS.getFin
#' @name rptTS.getFin
#' @aliases rptTS.getFin_windR 
#' @param rptTS a \bold{rptTS} object. a dataframe with cols:"rptDate","stockID"
#' @param field character strting or a vector of character string, giving the windfields. eg.  "OPEN,CLOSE,HIGH" or c("OPEN","CLOSE","HIGH")
#' @param varname vector of charactor string
#' @param ... other arguments except \code{rptDate} in \code{w.wss} 
#' @return a dataframe with the same length with rptTS, but added by some other financial indicator fields.
#' @export
#' @seealso \code{\link[WindR]{w.wss}}
#' @author Ruifei.Yin
#' @examples
#' rptTS <- getrptTS(begT=as.Date("2011-02-06"),endT=as.Date("2011-10-23"),stocks=c("EQ000001","EQ000002"))
#' # rptTS.getFin_windR
#' re <- rptTS.getFin_windR(rptTS,"np_belongto_parcomsh",options ="rptType=1")
rptTS.getFin_windR <- function(rptTS, field, varname, ...){  
  check.rptTS(rptTS)
  require(WindR)
  if(!w.isconnected()){
    w.start(showmenu=FALSE)
  }  
  rptTS2 <- transform(rptTS, stockID_wind=stockID2stockID(stockID,from="local",to="wind"), stringsAsFactors=FALSE)
  Dts <- unique(rptTS$rptDate)  
  Dts <- na.omit(Dts)
  df <- data.frame()
  for(Dt in Dts){    
    Dt <- as.Date(Dt,origin = "1970-01-01")
    codes <- rptTS2[rptTS2$rptDate==Dt, "stockID_wind", drop=TRUE]
    w_out <- w.wss(codes=codes, fields=field, paste('rptDate=',rdate2int(Dt)), ...)
    if(!w_out$ErrorCode==0){
      stop(paste('Error in w.wss running! rptDate =',rdate2int(Dt),'; errorcode=',w_out$ErrorCode))
    } 
    out <- data.frame(rptDate=Dt, w_out$Data)    
    if(dim(df)[1]==0) {
      df <- out
    } else {
      df <- rbind(df,out)
    }    
  }
  
  df$stockID <- stockID2stockID(df$CODE,"wind","local")
  re <- merge.x(rptTS,df,by=c("rptDate","stockID"))
  re$CODE <- NULL
  if(!missing(varname)){
    re <- renameCol(re,setdiff(names(re),names(rptTS)), varname)
  }
  return(re)
}





#' @rdname rptTS.getFin
#' @param funchar expression to get variables from tinysoft,a character string, usually copyed from tinysoft "stock-data-expert" and then replace the specified reportdate in the stock-data-expert expression by \code{'Rdate'}. e.g. convert \code{Last12MData(20091231,46002)} to \code{Last12MData(Rdate,46002)}.
#' @param ... other arguments in \code{ts.wss}.
#' @export
#' @seealso \code{\link{ts.wss}}
#' @examples
#' # rptTS.getFin_ts
#' re2 <- rptTS.getFin_ts(rptTS,'"np_belongto_parcomsh",report(46078,RDate)')
#' multi_funchar <- '"eps",reportofall(9900000,RDate),
#'    "zyywlrzzl",reportofall(9900601,RDate),
#'    "yszk",report(44009,RDate)'
#' re3 <- rptTS.getFin_ts(rptTS,multi_funchar)
rptTS.getFin_ts <- function(rptTS, funchar,varname, ...){
  check.rptTS(rptTS)
  Dts <- unique(rptTS$rptDate) 
  Dts <- na.omit(Dts)
  df <- data.frame()
  for(Dt in Dts){ 
    Dt <- as.Date(Dt,origin = "1970-01-01")
    codes <- rptTS[rptTS$rptDate==Dt, "stockID", drop=TRUE]
    ts_out <- ts.wss(stocks=codes, funchar=funchar,varname = varname, rptDate=Dt, adjust_yoy=TRUE, ...)     
    out <- data.frame(rptDate=Dt, ts_out)    
    if(dim(df)[1]==0) {
      df <- out
    } else {
      df <- rbind(df,out)
    }
  }
  re <- merge.x(rptTS,df,by=c("rptDate","stockID"))
  re$stockName <- NULL
  return(re)
}

#' @rdname rptTS.getFin
#' @export
#' @examples
#' # rptTS.getFinSeri_ts
#' FinSeri <- rptTS.getFinSeri_ts(rptTS,12,"q",'"np_belongto_parcomsh",report(46078,RDate)')
#' Finseri2 <- rptTS.getFinSeri_ts(rptTS,3,"y",multi_funchar)
rptTS.getFinSeri_ts <- function(rptTS, N, freq, funchar,varname, ...){
  check.rptTS(rptTS)
  # get rptTS_seri
  rptDate_df <- rptDate.offset(rptTS$rptDate, by=-N:0, freq = freq)
  rptTS_seri <- cbind(rptTS[,c("stockID","rptDate")],rptDate_df)
  rptTS_seri <- reshape2::melt(rptTS_seri,id.vars=c("stockID","rptDate"),variable.name="lagN",value.name="lag_rptDate")
  # remove repdate before IPO
  rptTS_seri$ipoDate <- trday.IPO(rptTS_seri$stockID) 
  rptTS_seri <- dplyr::filter(rptTS_seri,lag_rptDate>=ipoDate) 
  rptTS_seri <- dplyr::arrange(rptTS_seri,stockID,rptDate,desc(lag_rptDate))
  # get the financial index data
  rptTS_uniq <- unique(rptTS_seri[,c("stockID","lag_rptDate")])
  rptTS_uniq <- renameCol(rptTS_uniq,"lag_rptDate","rptDate")
  rptTS_uniq <- rptTS.getFin_ts(rptTS=rptTS_uniq,funchar=funchar, varname = varname, ...)
  rptTS_uniq <- renameCol(rptTS_uniq,"rptDate","lag_rptDate")
  re <- merge.x(rptTS_seri,rptTS_uniq,by =c("stockID","lag_rptDate"))
  return(re)
}



#' calcFinStat
#' 
#' @export
#' @examples
#' # calcFinStat
#' FinStat <- calcFinStat(FinSeri,"mean")
calcFinStat <- function(FinSeri,stat=c('mean','sum','slope','slope/mean','slope/growthsd','sd','mean/sd'),fname,rm_N){
  if(missing(fname)){
    fname <- guess_factorNames(FinSeri,no_factorname = c("stockID", "rptDate","lagN","lag_rptDate","ipoDate"),is_factorname = "factorscore",silence = TRUE)
  }
  # melt & group_by
  FinSeri <- reshape2::melt(FinSeri,measure.vars=fname,variable.name = "fname", value.name = "value")
  FinSeri <- dplyr::group_by(FinSeri,fname,stockID,rptDate)
  if(!missing(rm_N)){ # remove the  too short seri
    FinSeri <- FinSeri %>% dplyr::filter(n() > rm_N)
  }
  
  # get rptTS_stat
  if(stat=="mean"){
    rptTS_stat <- dplyr::summarise(FinSeri,value=mean(value,na.rm = TRUE))
  } else if (stat=="sum"){
    rptTS_stat <- dplyr::summarise(FinSeri,value=sum(value,na.rm = TRUE))
  } else if (stat=="sd"){
    rptTS_stat <- dplyr::summarise(FinSeri,value=sd(value,na.rm = TRUE))
  } else if (stat=="mean/sd"){
    rptTS_stat <- dplyr::summarise(FinSeri,value=mean(value,na.rm = TRUE)/sd(value,na.rm = TRUE))
  } else if (stat %in% c("slope","slope/mean","slope/growthsd")){
    FinSeri$lagN <- as.integer(substr(FinSeri$lagN,2,1000))
    if(stat=="slope"){
      rptTS_stat <- dplyr::do(FinSeri,mod = lm(value ~ lagN, data = .))
      rptTS_stat <- broom::tidy(rptTS_stat,mod)
      rptTS_stat <- rptTS_stat[rptTS_stat$term=='lagN',c("fname","stockID","rptDate","estimate")]
      rptTS_stat <- renameCol(rptTS_stat,"estimate","value")
    }else if(stat %in% c("slope/mean","slope/growthsd")){
      if(stat=="slope/mean"){
        rptTS_stat1 <- FinSeri %>% dplyr::summarise(value2=mean(value,na.rm = TRUE))
      }else{
        FinSeri <- FinSeri %>% dplyr::mutate(value2=value/dplyr::lead(value)-1)
        FinSeri <- FinSeri %>% dplyr::filter(!is.na(value2))
        rptTS_stat1 <- dplyr::summarise(FinSeri,value2=sd(value2,na.rm = TRUE))
      }
      
      rptTS_stat2 <- dplyr::do(FinSeri,mod = lm(value ~ lagN, data = .))
      rptTS_stat2 <- broom::tidy(rptTS_stat2,mod)
      rptTS_stat2 <- rptTS_stat2[rptTS_stat2$term=='lagN',c("fname","stockID","rptDate","estimate")]
      rptTS_stat <- dplyr::left_join(rptTS_stat1,rptTS_stat2,by=c("fname","stockID","rptDate"))
      rptTS_stat <- transform(rptTS_stat,value=estimate/value2,estimate=NULL,value2=NULL)
    }
  }
  # cast
  re <- reshape2::dcast(rptTS_stat,stockID+rptDate~fname)
  return(re)
}

#' rptTS.getFinStat_ts
#' 
#' @export
#' @examples
#' # rptTS.getFinStat_ts
#' FinStat <- rptTS.getFinStat_ts(rptTS,12,"q",'"np_belongto_parcomsh",report(46078,RDate)',stat="mean")
rptTS.getFinStat_ts <- function(rptTS, N, freq, funchar, varname, 
                                stat=c('mean','sum','slope','slope/mean','slope/growthsd','sd','mean/sd'),
                                rm_N, ...){
  stat <- match.arg(stat)
  check.rptTS(rptTS)
  FinSeri <- rptTS.getFinSeri_ts(rptTS = rptTS,N = N,freq = freq,funchar = funchar, varname = varname, ...)
  rptTS_stat <- calcFinStat(FinSeri=FinSeri,stat = stat,fname = varname, rm_N = rm_N)
  re <- merge.x(rptTS,rptTS_stat,by=c("stockID","rptDate"))
  return(re)
}



# --------------------  ~~ TS.getFin_by_rptTS ----------------
#' getrptDate_newest
#' 
#' get the newest rptDate  of the stocks on specific dates.
#' @param TS  a \bold{TS} object
#' @param stockID a vector of stockID
#' @param endT a vector of Date
#' @param mult a character string. Could be one of "last","first","all". IF a listed company publish more than one financial reports in a single day, which one should be returned? the newest one(the default value), the earliest one or all of them? See example for dedail.
#' @param drop a logical. Shoud the \code{TS} be exculded in the result?
#' @param datasrc
#' @return a data.frame,with the same cols of TS,added by "\code{rptDate}". Or a vector if \code{drop} is TRUE. 
#' @note param TS and combination of stockID and endT should  at least have one and only have one. The combination of vector stockID and endT could be different length, which abide by the recycling rule.
#' @author Ruifei.Yin
#' @export
#' @examples
#' # - with TS
#' TS <- getTS(getRebDates(as.Date('2007-03-17'),as.Date('2012-05-20'),rebFreq="year"),'EI000300')
#' getrptDate_newest(TS)
#' # - one stock, multiple dates
#' getrptDate_newest(stockID="EQ000001", endT=seq(from=as.Date("2010-12-31"),to=as.Date("2011-12-31"),by="month"))
#' # - one date, multiple stocks
#' getrptDate_newest(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2003-04-30"))
#' # - multi="all"
#' getrptDate_newest(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2003-04-30"),mult="all")
#' # - multi="first"
#' getrptDate_newest(stockID=c("EQ000001","EQ000002","EQ000004"), endT=as.Date("2003-04-30"),mult="first")
getrptDate_newest <- function(TS,stockID,endT=Sys.Date(),freq=c("q","y","h"),
                              mult=c("last","first","all"),
                              drop=FALSE,
                              datasrc=defaultDataSRC()){
  freq <- match.arg(freq)
  mult <- match.arg(mult)
  if (missing(TS) && missing(stockID)) {
    stop("Param TS and combination of stockID and endT should at least have one!")
  }
  if (!missing(TS) && !missing(stockID)) {
    stop("Param TS and combination of stockID and endT should only have one!")
  }
  if (missing(TS)){
    TS <- data.frame(date=endT, stockID=stockID) 
  }   
  check.TS(TS)
  
  if(datasrc %in% c("quant","local")){
    TS_new <- TS
    TS_new$date <- rdate2int(TS_new$date)
    qr <- paste(
      "select date, a.stockID, EndDate as rptDate
      from yrf_tmp as a left join LC_RptDate as b
      on a.stockID=b.stockID and PublDate<=date and (PublDate_next>date or PublDate_next is null)
      order by date, a.stockID, rptDate"      
    )
    
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=TS_new[,c("date","stockID")],tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      dbWriteTable(con,name="yrf_tmp",value=TS_new[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    
    re <- transform(re, date=intdate2r(date), rptDate=intdate2r(rptDate)) 
    if(freq %in% c("y","h")){ # get the newest yearly report or halfyearly report
      re$rptDate <- rptDate.nearest(re$rptDate,freq = freq,dir = -1L)
    }
    re <- merge.x(TS,re,by=c("date","stockID"),mult=mult)
  } 
  
  if(drop){
    return(re[,"reptDate"])
  } else {
    return(re)
  }    
  
}




#' TS.getFin_by_rptTS
#' 
#' get financial factorscore via rptTS 
#' @param TS
#' @param fun a function or a non-empty character string naming the function to be called, which  get the financtial indicators from \bold{rptTS}. Note that the function must contain a param of 'rptTS' .
#' @param ... optional arguments except rptTS of fun.
#' @return a dataframe with the same length with TS, but added by some other financial indicator fields.
#' @export
#' @seealso \code{\link{rptTS.getFin_windR}}, \code{\link{rptTS.getFin_ts}}
#' @author Ruifei.Yin
#' @examples 
#' TS <- Model.TS(modelPar.univ(indexID="ES09440000"))
#' re <- TS.getFin_by_rptTS(TS,fun="rptTS.getFin_windR",field="np_belongto_parcomsh","rptType=1")
#' re2 <- TS.getFin_by_rptTS(TS,fun="rptTS.getFin_ts",funchar='"np_belongto_parcomsh",report(46078,RDate)')
#' # -- Following is a speed comparison of three different methods to get financial factorscores:
#' 
#' TS <- Model.TS(setmodelPar.time(modelPar.default(),begT=as.Date("2007-12-01"),endT=as.Date("2014-05-01")))
#' system.time(re.wind <- TS.getFin_by_rptTS(TS,fun="rptTS.getFin_windR",field="np_belongto_parcomsh","rptType=1")) # 73.69
#' system.time(re.ts_dir <- TS.getFin_ts(TS,funchar='report(46078,RDate)',varname="np_belongto_parcomsh")) # 12.49
#' system.time(re.ts_rpt <- TS.getFin_by_rptTS(TS,fun="rptTS.getFin_ts",funchar='"np_belongto_parcomsh",report(46078,RDate)')) # 6.29
TS.getFin_by_rptTS <- function(TS, fun, ...){
  check.TS(TS)  
  TS <- getrptDate_newest(TS,mult="last")
  rptTS <- unique(TS[, c("rptDate","stockID")])
  rptTSF <- do.call(fun, list(rptTS=rptTS, ...))
  TSF <- merge.x(TS, rptTSF, by=c("rptDate","stockID"))
  return(TSF)
}


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Others      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



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
  } else {
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





#' @export
getTech <- function(TS, 
                    variables = select.list(CT_TechVars(datasrc=datasrc,secuCate="EQ",tableName=tableName)[["varName"]],graphics=TRUE,multiple=TRUE), 
                    tableName="QT_DailyQuote2",
                    datelist,stockID, 
                    drop,
                    datasrc=defaultDataSRC()){
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
    missing_TS <- TRUE
  } else {
    missing_TS <- FALSE
  }
  
  re <- TS.getTech(TS=TS,variables = variables,tableName = tableName,datasrc = datasrc)
  
  if(missing(drop)){
    drop <- if(missing_TS&length(variables)==1) TRUE else FALSE
  }
  if(drop){
    return(re[,variables])
  }else{
    return(re)
  }
}

#' @export
getTech_ts <- function(TS,funchar,varname=funchar, Rate=1, RateDay=0,
                       datelist,stockID, 
                       drop){
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)&length(funchar)==1) TRUE else FALSE
  }
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  result <- TS.getTech_ts(TS=TS,funchar = funchar,varname = varname,Rate = Rate,RateDay = RateDay)
  
  if(drop){
    return(result[,varname])
  }else{
    return(result)
  }
}

#' @export
getFin_ts <- function(TS,funchar,varname=funchar,Rate=1,RateDay=0,
                      datelist,stockID, 
                      drop){
  if (missing(TS) && any(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should at least have one!")
  }
  if (!missing(TS) && !all(missing(stockID),missing(datelist))) {
    stop("Param TS and combination of stockID and datelist should only have one!")
  }
  if(missing(drop)){
    drop <- if(missing(TS)&length(funchar)==1) TRUE else FALSE
  }
  if (missing(TS)){
    TS <- expand.grid(date=datelist, stockID=stockID)
  }
  
  result <- TS.getFin_ts(TS=TS,funchar = funchar,varname = varname,Rate = Rate,RateDay = RateDay)
  
  if(drop){
    return(result[,varname])
  }else{
    return(result)
  }  
}



#' TS.getTech
#' @export 
TS.getTech <- function(TS, 
                       variables = select.list(CT_TechVars(datasrc=datasrc,secuCate="EQ",tableName=tableName)[["varName"]],graphics=TRUE,multiple=TRUE), 
                       tableName="QT_DailyQuote2",
                       datasrc=defaultDataSRC()){
  check.TS(TS)  
  tmpdat <- transform(TS[,c("stockID","date")], date = rdate2int(date))
  tmpdat$PK_ <- 1:NROW(tmpdat)  
  vars <- CT_TechVars(datasrc=datasrc,secuCate="EQ",tableName=tableName,vars=variables) 
  if(NROW(vars)==0){
    stop(paste("No fields matched in table",QT(tableName),"in datasrc",QT(datasrc),"!"))
  }
  vars <- paste(vars$func,"as",QT(vars$varName), collapse=", ")      
  qr <- paste("select a.*,",vars,"from yrf_tmp a left join",tableName ,"b on a.stockID=b.ID and a.date=b.TradingDay") 
  
  if(datasrc=="quant"){
    con <- db.quant()
    sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
    sqlSave(con,dat=tmpdat,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
    re <- sqlQuery(con,query=qr)
    odbcClose(con)
  } else if (datasrc=="local"){
    con <- if(tableName =="QT_FactorScore") db.local("fs") else db.local("qt")
    dbWriteTable(con,name="yrf_tmp",value=tmpdat,row.names = FALSE,overwrite = TRUE)
    if(tableName %in% c("QT_DailyQuote2","QT_DailyQuote","QT_FactorScore")){
      dbExecute(con, 'CREATE INDEX [IX_yrf_tmp] ON [yrf_tmp]([date],[stockID]);')
    }
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
  }  
  re <- dplyr::arrange(re,PK_)[,variables,drop=FALSE]
  re <- cbind(TS,re)    
  
  return(re)
 
}


#' TS.getTech_ts
#'
#' get technical factors throug a tinysoft expression, which could be a simple expresion, e.g 'close()','StockZf2(20)','BBIBOLL_v(11,6)',..,or more complicated expression, e.g. 'close()/open()', 'StockZf2(10)-StockZf2(20)'...
#' @param TS a \bold{TS} object
#' @param funchar a vector of character,the tinysoft function with the arguments. e.g \code{"BBIBOLL_v(11,6)"}
#' @param varname character string, giving the name of the returned variables
#' @return A \bold{TSF} object
#' @note Note that the tinysoft function must contain ONLY two(no more!) system parameters: pn_stock() and pn_date() ,which is specifyed by the two fields in the TS respectively.
#' @author Ruifei.Yin
#' @export
#' @examples 
#' TS <- getTS(getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17')),'EI000300')
#'  p1 <- 11
#'  p2 <- 6
#'  funchar <- paste('BBIBOLL_v(',p1,',',p2,')',sep='')
#'  TSF <- TS.getTech_ts(TS,funchar)
#'  TSF <- TS.getTech_ts(TS, funchar="StockAveHsl2(20)/StockAveHsl2(60)", varname="avgTurnover_1M3M")
#'  funchar <- c('BBIBOLL_v(11,6)','StockAveHsl2(20)/StockAveHsl2(60)')
#'  TSF2 <- TS.getTech_ts(TS, funchar)
TS.getTech_ts <- function(TS,funchar,varname=funchar, Rate=1, RateDay=0){
  check.TS(TS)
  
  tmpfile <- TS
  tmpfile$stockID <- stockID2stockID(tmpfile$stockID,from="local",to="ts")
  tmpfile$date <- as.character(tmpfile$date)
  
  tmpcsv <- tempfile(fileext=".csv")
  tmpcsv2 <- stringr::str_replace_all(tmpcsv,'\\\\',"\\\\\\\\")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  
  subqr <- ""
  for( i in 1:length(funchar)){
    subqr <- paste0(subqr, '  factorexp[',i-1,'] := &"',funchar[i],'";  ')
  }
  subqr2 <- ""
  if(!is.null(Rate)){
    subqr2 <- paste0(subqr2, ' SetSysParam(pn_rate(), ',Rate,'); ')
  }
  if(!is.null(RateDay)){
    subqr2 <- paste0(subqr2, ' SetSysParam(pn_rateday(), ',RateDay,'); ')
  }
  len.funchar <- length(funchar)-1
  
  qrstr <- paste0('oV:=BackUpSystemParameters();
                  SetSysParam(pn_cycle(),cy_day());
                  ',subqr2,'
                  rdo2 importfile(ftcsv(),"","',tmpcsv2,'",timestockframe);
                  factorexp := array();
                  ',subqr,'
                  result:=array();
                  for i:=0 to length(timestockframe)-1 do
                  begin
                  SetSysParam(pn_stock(),timestockframe[i]["stockID"]);
                  SetSysParam(pn_date(),strtodate(timestockframe[i]["date"]));
                  for j:= 0 to ',len.funchar,' do
                  begin
                  factorvalue:=eval(factorexp[j]);
                  result[i][j]:=factorvalue;
                  end;
                  end;
                  RestoreSystemParameters(oV);
                  return result;')
  tsRequire()
  fct <- tsRemoteExecute(qrstr)
  fct <- plyr::ldply(fct, unlist)
  colnames(fct) <- varname 
  result <- cbind(TS,fct)
  return(result)  
}




#' TS.getFin_ts
#'
#' get financial factors throug a tinysoft expression, which could be simple or more complicated. 
#'
#' Note that the tinysoft function must contain ONLY two system parameters(no more!):pn_stock() and pn_date() ,which is supplyed from the two fields in the TS respectively.Also,the function must contain a expression of 'Rdate:=NewReportDateOfEndT2(sp_time())' to get the newest Rdate of sp_time.
#' @param TS a \bold{TS} object
#' @param funchar a vector of character string, giving a tinysoft expression to get the financtial indicators of the stock. The expression can be made simply by replaceing the specified reportdate in the stock-data-expert expression by \code{'Rdate'}. e.g. change \code{Last12MData(20091231,46002)} to \code{Last12MData(Rdate,46002)}.
#' @param varname character string, giving the name of the returned variables
#' @return A \bold{TSF} object,a dataframe, containing at least cols:\bold{date}(with class of Date),\bold{stockID},\bold{varname},\bold{Rdate} 
#' @note Note that the tinysoft function must contain ONLY two system parameters(no more!):pn_stock() and pn_date() ,which is supplyed from the two fields in the TS respectively. Also,the function must contain a expression of 'Rdate:=NewReportDateOfEndT2(sp_time())' to get the newest Rdate of sp_time.
#' @author Ruifei.Yin
#' @export
#' @examples
#' TS <- getTS(getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17')),'EI000300')
#' TSF <- TS.getFin_ts(TS,"ReportOfAll(9900416,Rdate)")
#' TSF2 <- TS.getFin_ts(TS,"GrowthOfNReport(@@LastQuarterData(DefaultRepID(),9900003,0),Rdate,3,1)")
#' TSF3 <- TS.getFin_ts(TS,"StockAveHsl2(20)+reportofall(9900003,Rdate)")
#' funchar <- c("ReportOfAll(9900416,Rdate)","StockAveHsl2(20)+reportofall(9900003,Rdate)")
#' TSF4 <- TS.getFin_ts(TS, funchar)
TS.getFin_ts <- function(TS,funchar,varname=funchar,Rate=1,RateDay=0){
  check.TS(TS)
  
  tmpfile <- TS
  tmpfile$stockID <- stockID2stockID(tmpfile$stockID,from="local",to="ts")
  tmpfile$date <- as.character(tmpfile$date)
  tmpcsv <- tempfile(fileext=".csv")
  tmpcsv2 <- stringr::str_replace_all(tmpcsv,'\\\\',"\\\\\\\\")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  
  subqr <- ""
  for( i in 1:length(funchar)){
    subqr <- paste0(subqr, '  factorexp[',i-1,'] := &"',funchar[i],'";  ')
  }
  subqr2 <- ""
  if(!is.null(Rate)){
    subqr2 <- paste0(subqr2, ' SetSysParam(pn_rate(), ',Rate,'); ')
  }
  if(!is.null(RateDay)){
    subqr2 <- paste0(subqr2, ' SetSysParam(pn_rateday(), ',RateDay,'); ')
  }
  len.funchar <- length(funchar)-1
  
  qrstr <- paste0('oV:=BackUpSystemParameters();
                  SetSysParam(pn_cycle(),cy_day());  
                  ',subqr2,'  
                  rdo2 importfile(ftcsv(),"","',tmpcsv2,'",timestockframe);
                  factorexp := array();  
                  ',subqr,'
                  result:=array(); 
                  for i:=0 to length(timestockframe)-1 do
                  begin 
                  SetSysParam(pn_stock(),timestockframe[i]["stockID"]); 
                  SetSysParam(pn_date(),strtodate(timestockframe[i]["date"]));
                  Rdate:=NewReportDateOfEndT2(sp_time()); 
                  for j:=0 to ',len.funchar,' do
                  begin 
                  factorvalue:=eval(factorexp[j]); 
                  result[i][j]:=factorvalue; 
                  end; 
                  result[i]["Rdate"]:=Rdate; 
                  end; 
                  RestoreSystemParameters(oV); 
                  return result;')
  tsRequire()
  fct <- tsRemoteExecute(qrstr)
  fct <- plyr::ldply(fct,as.data.frame)
  colnames(fct) <- c(varname, "RDate")
  result <- cbind(TS,fct)
  return(result)  
}






#' getQuote_ts
#' 
#' get Quote series of some stocks in certain period
#' @param stocks a vector of charactor
#' @param begT an object of class "Date"
#' @param endT an object of class "Date"
#' @param variables a vector of charactor,elements of which could be stockID,stockName,date,price,open,high,low,vol,amount,yclose,sectional_yclose,cjbs,...
#' @param Cycle a charactor string,eg."cy_day()","cy_30m()","cy_month()",...
#' @param Rate a integer,giving the type of rights adjustment, could be one of 0(no adjustment),1(geometric adjustment),2(simple adjustment),3 
#' @param RateDay a integer,giving the base date of right adjustment,could be one of 0(the last trading day),-1(the IPO date),or a tinysoft date integer(eg.\code{rdate2ts(as.Date("2010-01-02"))})
#' @param melt a logical. If FALSE(default), the style of result is "stockID+date~variable";If TRUE, the quote data will be melted(see the examples for details).
#' @param split
#' @param splitNbin
#' @return the quote data, a data frame object with cols:stockID,stockName,date(\bold{of class POSIXct}) and the elements the param \code{variable} containing
#' @export
#' @author Ruifei.Yin
#' @examples
#' stocks <- c("SZ002001","SZ002002")
#' begT <- as.Date("2011-01-01")
#' endT <- as.Date("2011-02-01")
#' variables <- c("price","open")
#' qt.asis <- getQuote_ts(stocks,begT,endT,variables)
#' qt.melt <- getQuote_ts(stocks,begT,endT,variables,melt=TRUE)
getQuote_ts <- function(stocks,begT,endT,variables,Cycle="cy_day()",Rate=0,RateDay=0,melt=FALSE,
                        split = if(length(stocks) > splitNbin) TRUE else FALSE,
                        splitNbin = 50){
  #     stocks <- c("SZ002001","SZ002002")
  #     begT <- as.Date("2008-01-01")
  #     endT <- as.Date("2011-02-01")
  #     variables <- c("price","yclose")
  #     Cycle="cy_day()"
  #     Rate=0
  #     RateDay=0     
  subfun <- function(stocks,begT,endT,variables,Cycle,Rate,RateDay,melt){
    stockID <- paste(stocks,collapse=",")
    begT <- rdate2ts(begT)
    endT <- rdate2ts(endT)
    variables <- paste('["',variables,'"]',sep="",collapse=",")  
    tsRequire()
    qt <- tsRemoteCallFunc("getQuote",list(stockID,begT,endT,variables,Cycle,Rate,RateDay))
    qt <- plyr::ldply(qt,as.data.frame)
    qt$date <- as.POSIXct(qt$date,tz="")
    if(melt){
      qt <- reshape2::melt(qt,id.vars=c("stockID","date"))
    }
    return(qt)
  }
  if(!split){
    re <- subfun(stocks,begT,endT,variables,Cycle,Rate,RateDay,melt)
  } else {    
    Ngroup <- length(stocks) %/% splitNbin +1  
    Ngroup <-  if(Ngroup>1) Ngroup else Ngroup+1
    bby <- cut(1:length(stocks),Ngroup,labels=FALSE)
    warning(paste('Possibly dealing with a big quote data. The process is split to ',Ngroup,'groups.'))
    for(i in 1:Ngroup){
      #     i <- 2
      substocks <- stocks[bby==i]
      cat("Dealing with ",i," of ",Ngroup,"groups ... \n")
      gc()
      qt <- subfun(substocks,begT,endT,variables,Cycle,Rate,RateDay,melt)
      if(i==1L){
        re <- qt
      } else {
        re <- rbind(re,qt)
      }
    } 
  }
  return(re) 
}




#' getQuote
#' 
#' get Quote series of stocks, indexs, futures in certain period
#' @aliases getQuote getIndexQuote getIFquote
#' @param stocks a charactor vector of stockID or indexID.
#' @param begT an object of class "Date"
#' @param endT an object of class "Date"
#' @param variables a vector of charactor. Call funtion \code{CT_TechVars(secuCate="EQ")} to get all available variables of equity, funtion \code{CT_TechVars(secuCate="EI")} of equity index, and funtion \code{CT_TechVars(secuCate="IF")} of index future.
#' @param melt a logical. If FALSE(default), the style of result is "stockID+date~variable";If TRUE, the quote data will be melted(see the examples for details).
#' @param split
#' @param splitNbin
#' @param tableName character string, giving the table name from which get data.
#' @return the quote data, a data frame object with cols:stockID,date and the elements the param \code{variable} containing. If melt is TRUE, data frame with cols:stockID,date,variable,value
#' @export
#' @author Ruifei.Yin
#' @examples
#' # get stock's quote
#' stocks <- c("EQ002001","EQ002002")
#' begT <- as.Date("2007-01-01")
#' endT <- as.Date("2013-02-01")
#' variables <- c("close","pct_chg","vwap")
#' re <- getQuote(stocks,begT,endT,variables,datasrc="local")
#' re <- getQuote(stocks,begT,endT,variables,melt=TRUE,datasrc="local")
#' stocks <- getIndexComp("EI000300")
#' system.time(uu2 <- getQuote(stocks,begT,endT,variables,datasrc="local",split=FALSE))
#' system.time(uu1 <- getQuote(stocks,begT,endT,variables,datasrc="local",split=TRUE))
getQuote <- function(stocks, begT=as.Date("1990-12-19"), endT=Sys.Date(), 
                     variables = select.list(CT_TechVars(datasrc=datasrc,secuCate="EQ",tableName=tableName)[["varName"]],graphics=TRUE,multiple=TRUE), 
                     melt=FALSE,
                     split = if(length(stocks) > splitNbin) TRUE else FALSE,
                     splitNbin = 300,
                     tableName = "QT_DailyQuote",
                     datasrc=defaultDataSRC()){
  
  begT <- rdate2int(begT)
  endT <- rdate2int(endT)  
  if(begT>endT){
    stop(paste("begT ",begT,"is later then endT ",endT))
  }
  techVars <- CT_TechVars(datasrc=datasrc,secuCate="EQ",tableName=tableName,vars=variables)  
  if(NROW(techVars)==0){
    stop(paste("No fields matched in table", QT(tableName),"in datasrc",QT(datasrc),"!"))
  }  
  
  subfun <- function(stocks,melt){
    stocks_char <- paste("(",paste(QT(stocks),collapse=","),")",sep="") 
    vars <- techVars
    vars <- paste(vars$func,"as",QT(vars$varName), collapse=", ")      
    querychar <- paste("select ID as stockID,TradingDay as date,",vars,"from",tableName ,"where ID in", stocks_char, "and TradingDay >=", begT, "and TradingDay <=" ,endT)  
    
    if(datasrc=="quant"){  
      qt <- queryAndClose.odbc(db.quant(),querychar)
    } else if(datasrc=="local"){      
      qt <- queryAndClose.dbi(db.local("qt"),querychar)
    }
    
    qt$date <- intdate2r(qt$date)
    if(melt){
      qt <- reshape2::melt(qt,id.vars=c("stockID","date"))
    }
    return(qt)
  } 
  
  
  if(!split){
    re <- subfun(stocks,melt)
  } else {
    Ngroup <- length(stocks) %/% splitNbin +1  
    #     Ngroup <-  if(Ngroup>1) Ngroup else Ngroup+1
    bby <- cut(1:length(stocks),Ngroup,labels=FALSE)
    warning(paste('Possibly dealing with a big data. The process is splited to ',Ngroup,'groups.'))
    for(i in 1:Ngroup){
      substocks <- stocks[bby==i]
      cat("Dealing with ",i," of ",Ngroup,"groups ... \n")
      gc()
      qt <- subfun(substocks,melt)
      if(i==1L){
        re <- qt
      } else {
        re <- rbind(re,qt)
      }
    }
  } 
  
  rslt <- re 
  
  return(rslt)  
}



#' @rdname getQuote
#' @export
#' @examples
#' # get index quote
#' indexs <- c("EI000001","EI000300")
#' begT <- as.Date("2015-01-01")
#' endT <- as.Date("2016-08-01")
#' variables <- c("close","pct_chg")
#' re <- getIndexQuote(indexs,begT,endT,variables,datasrc="local")
#' re <- getIndexQuote(indexs,begT,endT,variables,datasrc="jy")
getIndexQuote <- function(stocks, 
                          begT=as.Date("1990-12-19"), endT=Sys.Date(), 
                          variables = select.list(CT_TechVars(datasrc=datasrc,secuCate="EI",tableName="QT_IndexQuote")[["varName"]],graphics=TRUE,multiple=TRUE), 
                          melt=FALSE,
                          datasrc=defaultDataSRC()){
  begT <- rdate2int(begT)
  endT <- rdate2int(endT)
  vars <- CT_TechVars(datasrc=datasrc,secuCate="EI",tableName="QT_IndexQuote",vars=variables)   
  if(NROW(vars)==0){
    stop(paste("No fields matched in table 'QT_IndexQuote' in datasrc",QT(datasrc),"!"))
  }  
  
  subfun <- function(stocks,melt){
    stocks_char <- paste("(",paste(QT(stocks),collapse=","),")",sep="")    
    vars <- paste(vars$func,"as",QT(vars$varName), collapse=", ")      
    querychar <- paste("select ID as stockID,TradingDay as date,",vars,"from QT_IndexQuote where ID in", stocks_char, "and TradingDay >=", begT, "and TradingDay <=" ,endT) 
    
    if(datasrc=="quant"){  
      con <- db.quant()
      qt <- queryAndClose.odbc(db.quant(),querychar)
    } else if(datasrc=="local"){      
      qt <- queryAndClose.dbi(db.local("main"),querychar)
    }else if(datasrc=="jy"){
      stocks_char <- paste("(",paste(QT(substr(stocks,3,8)),collapse=","),")",sep="")  
      begT <- intdate2r(begT)
      endT <- intdate2r(endT)
      querychar <- paste("SELECT 'EI'+s.SecuCode 'stockID',CONVERT(varchar,TradingDay,112) 'date',",
                  vars," FROM QT_IndexQuote q,SecuMain s
                  where q.InnerCode=s.InnerCode and s.SecuCode in",stocks_char,
                  " and q.TradingDay>=",QT(begT)," and q.TradingDay<=",QT(endT))
      qt <- queryAndClose.odbc(db.jy(),querychar)
      qt <- dplyr::arrange(qt,stockID,date)
    }
    
    qt$date <- intdate2r(qt$date)
    if(melt){
      qt <- reshape2::melt(qt,id.vars=c("stockID","date"))
    }
    return(qt)
  }    
  re <- subfun(stocks,melt)   
  return(re)  
}





#' getPeriodrtn
#'
#' get the period return of the stocks
#' @param SP a \bold{SP} object ('stock*period'): a data frame with cols: "stockID","begT","endT"(with class of Date).
#' @param stockID a vector of stockID
#' @param begT a vector of Date
#' @param endT a vector of Date
#' @param tradeType a character string("close","nextavg","nextopen"),indicating the trading type.
#' @param drop a logical. If the \code{SP} shoud be exculded in the result?
#' @param datasrc
#' @return a data frame, with cols of SP and "periodrtn". Or a vector if \code{drop} is TRUE.
#' @note param SP and combination of stockID, begT and endT should  at least have one and only have one. The combination of vector stockID, begT and endT could be different length, which abide by the recycling rule.
#' @author Ruifei.Yin
#' @export
#' @examples
#' # -- with SP
#' getPeriodrtn(data.frame(stockID="EQ601313",begT=as.Date("2012-01-20"),endT=as.Date("2012-09-27")))
#' getPeriodrtn(data.frame(stockID="EQ601313",begT=as.Date("2012-01-20"),endT=as.Date("2012-09-27")),tradeType="nextopen")
#' TS <- getTS(getRebDates(as.Date('2007-03-17'),as.Date('2012-05-20')),'EI000300')
#' TS <- renameCol(TS,"date","begT")
#' SP <- data.frame(TS,endT=trday.offset(TS$begT,months(1)))
#' system.time(re <- getPeriodrtn(SP,datasrc="ts"))  # 8.40
#' system.time(re1 <- getPeriodrtn(SP,datasrc="local"))  # 1.94
#' system.time(re <- getPeriodrtn(SP,tradeType = "nextavg",datasrc="ts")) # 10.80
#' system.time(re1 <- getPeriodrtn(SP,tradeType = "nextavg",datasrc="local")) # 16.7
#' 
#' #-- with combination of vector stockID, begT and endT
#' getPeriodrtn(stockID="EQ000001",begT=as.Date("2012-01-01"), endT=as.Date("2012-01-01")+c(10,20,30))
#' getPeriodrtn(stockID=c("EQ000001","EQ000002","EQ000004"),begT=as.Date("2012-01-01"), endT=as.Date("2013-01-01"))
getPeriodrtn <- function(SP, stockID, begT, endT,
                         tradeType=c("close","nextavg","nextopen"),drop=FALSE,
                         datasrc=defaultDataSRC()){
  tradeType <- match.arg(tradeType)
  if (missing(SP) && any(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should at least have one!")
  }
  if (!missing(SP) && !all(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should only have one!")
  }
  if (missing(SP)){
    SP <- data.frame(stockID=stockID,begT=begT,endT=endT) 
  }   
  check.SP(SP)
  if("periodrtn" %in% names(SP)){
    warning("Column 'periodrtn' is already exist. It will be drop!")
    SP$periodrtn <- NULL
  }
  
  
  if(datasrc=="ts"){
    tmpdat <- transform(SP, stockID = stockID2stockID(stockID,from="local",to="ts"),
                        begT = as.character(begT),
                        endT = as.character(endT))
    tmpcsv <- tempfile(fileext=".csv")
    write.csv(tmpdat,tmpcsv,row.names=FALSE,quote=FALSE,fileEncoding="GB2312")
    tsRequire()
    periodrtn <- tsRemoteCallFunc("getperiodrtn",list(tmpcsv,tradeType))
    periodrtn <- plyr::laply(periodrtn,as.array)
    re <- cbind(SP, periodrtn)
  } 
  
  if(datasrc %in% c("quant","local")){
    # -- deal with the trading days
    if(tradeType=="close"){
      tmpdat <- transform(SP[,c("stockID","begT","endT")], begT = trday.nearest(begT), endT = trday.nearest(endT))
      tmpdat <- transform(tmpdat, begT = rdate2int(begT), endT = rdate2int(endT))
      tmpdat$PK_ <- 1:NROW(tmpdat) 
    } else {
      begT_ <- trday.nearby(TS = dplyr::select(SP,date=begT,stockID),by = 1,drop = TRUE)
      endT_ <- trday.nearby(TS = dplyr::select(SP,date=endT,stockID),by = 1,drop = TRUE)
      tmpdat <- data.frame(stockID=SP$stockID,begT=begT_,endT=endT_)
      tmpdat <- transform(tmpdat, begT = rdate2int(begT), endT = rdate2int(endT))
      tmpdat$PK_ <- 1:NROW(tmpdat)
    }         
    # -- query
    if(tradeType=="close"){
      qr <- "select a.*,b.RRClosePrice as P0,c.RRClosePrice as P1
      from yrf_tmp a left join QT_DailyQuote2 b
      on a.begT=b.TradingDay and a.stockID=b.ID  
      left join QT_DailyQuote2 as c
      on a.endT=c.TradingDay and a.stockID=c.ID"
    } else if(tradeType=="nextopen"){
      qr <- "select a.*,(b.OpenPrice*b.RRFactor) as P0,(c.OpenPrice*c.RRFactor) as P1
      from yrf_tmp a left join QT_DailyQuote2 b
      on a.begT=b.TradingDay and a.stockID=b.ID
      left join QT_DailyQuote2 as c
      on a.endT=c.TradingDay and a.stockID=c.ID"
    } else if(tradeType=="nextavg"){
      qr <- "select a.*,(b.TurnoverValue/b.TurnoverVolume*b.RRFactor) as P0,
      (c.TurnoverValue/c.TurnoverVolume*c.RRFactor) as P1
      from yrf_tmp a left join QT_DailyQuote2 b
      on a.begT=b.TradingDay and a.stockID=b.ID
      left join QT_DailyQuote2 as c
      on a.endT=c.TradingDay and a.stockID=c.ID"
    }   
    # -- fetch
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=tmpdat,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)  
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("qt")
      dbWriteTable(con,name="yrf_tmp",value=tmpdat,row.names = FALSE,overwrite = TRUE)
      dbExecute(con, 'CREATE INDEX [IX_yrf_tmp] ON [yrf_tmp]([begT],[stockID]);')
      dbExecute(con, 'CREATE INDEX [IX2_yrf_tmp] ON [yrf_tmp]([endT],[stockID]);')
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re <- dplyr::arrange(re,PK_)[,c("P0","P1")]
    re <- re$P1/re$P0-1
    re <- cbind(SP,periodrtn=re)
  }  
  if(drop){
    return(re[,"periodrtn"])
  } else {
    return(re)
  }  
}



#' @rdname getPeriodrtn
#' @export
#' @examples 
#' # -- getPeriodrtn_EI
#' getPeriodrtn_EI(stockID="EI000300",begT=as.Date("2012-01-01"), endT=as.Date("2012-01-01")+c(10,20,30))
getPeriodrtn_EI <- function(SP, stockID, begT, endT, drop=FALSE,
                         datasrc=defaultDataSRC()){
  if (missing(SP) && any(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should at least have one!")
  }
  if (!missing(SP) && !all(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should only have one!")
  }
  if (missing(SP)){
    SP <- data.frame(stockID=stockID,begT=begT,endT=endT) 
  }   
  check.SP(SP)
  
  if(datasrc=="ts"){
    tmpdat <- transform(SP, stockID = stockID2stockID(stockID,from="local",to="ts"),
                        begT = as.character(begT),
                        endT = as.character(endT))
    tmpcsv <- tempfile(fileext=".csv")
    write.csv(tmpdat,tmpcsv,row.names=FALSE,quote=FALSE,fileEncoding="GB2312")
    tsRequire()
    periodrtn <- tsRemoteCallFunc("getperiodrtn",list(tmpcsv,"close"))
    periodrtn <- plyr::laply(periodrtn,as.array)
    re <- cbind(SP, periodrtn)
  } 
  
  if(datasrc %in% c("quant","local")){
    # -- deal with the trading days
    if(TRUE){
      tmpdat <- transform(SP[,c("stockID","begT","endT")], begT = trday.nearest(begT), endT = trday.nearest(endT))
      tmpdat <- transform(tmpdat, begT = rdate2int(begT), endT = rdate2int(endT))
      tmpdat$PK_ <- 1:NROW(tmpdat) 
    }          
    # -- query
    if(TRUE){
      qr <- "select a.*,b.ClosePrice as P0,c.ClosePrice as P1
      from yrf_tmp a left join QT_IndexQuote b
      on a.begT=b.TradingDay and a.stockID=b.ID  
      left join QT_IndexQuote as c
      on a.endT=c.TradingDay and a.stockID=c.ID"
    }   
    # -- fetch
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=tmpdat,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)  
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local("main")
      dbWriteTable(con,name="yrf_tmp",value=tmpdat,row.names = FALSE,overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re <- dplyr::arrange(re,PK_)[,c("P0","P1")]
    re <- re$P1/re$P0-1
    re <- cbind(SP,periodrtn=re)
    }  
  if(drop){
    return(re[,"periodrtn"])
  } else {
    return(re)
  }  
}

#' @rdname getPeriodrtn
#' @export
#' @examples 
#' # -- getPeriodrtn_FU
#' getPeriodrtn_FU(stockID="FUIF00",begT=as.Date("2012-01-01"), endT=as.Date("2012-01-01")+c(10,20,30))
getPeriodrtn_FU <- function(SP, stockID, begT, endT, drop=FALSE,
                            datasrc="ts"){
  if (missing(SP) && any(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should at least have one!")
  }
  if (!missing(SP) && !all(missing(stockID),missing(begT),missing(endT))) {
    stop("Param SP and combination of stockID, begT and endT should only have one!")
  }
  if (missing(SP)){
    SP <- data.frame(stockID=stockID,begT=begT,endT=endT) 
  }   
  check.SP(SP)
  
  if(datasrc=="ts"){# todo
    # tmpdat <- transform(SP, stockID = stockID2stockID(stockID,from="local",to="ts"),
    #                     begT = as.character(begT),
    #                     endT = as.character(endT))
    # tmpcsv <- tempfile(fileext=".csv")
    # write.csv(tmpdat,tmpcsv,row.names=FALSE,quote=FALSE,fileEncoding="GB2312")
    # periodrtn <- tsRemoteCallFunc("getperiodrtn",list(tmpcsv,"close"))
    # periodrtn <- plyr::laply(periodrtn,as.array)
    # re <- cbind(SP, periodrtn)
  } 
  
  if(datasrc %in% c("quant","local")){
    
  }  
  if(drop){
    return(re[,"periodrtn"])
  } else {
    return(re)
  }  
}



#' getTradeList
#' 
#' @param port_ini a dataframe with cols: "stockID" (with EQ format),"amount".
#' @param port_obj a dataframe with cols: "stockID","wgt"
#' @param money_obj a numeric
#' @param splitN a integer
#' @return a list with 3 items: tradeList_N,tradeList_remain,summaryTable
#' @export
#' @author Han.Qian
getTradeList <- function(port_ini, port_obj, money_obj, splitN=1, outputstyle = c("hs")){
  
  # get the cashflow and tradelist from the S_combine
  make_tradelist_cashflow <- function(S_combine, lastday, style){
    # calculate cash flow
    S_money <- S_combine
    date <- rep(lastday, length(S_money$stockID))
    TS_money <- data.frame(date, "stockID" = S_money$stockID)
    TS_money <- TS.getTech_ts(TS_money, funchar = "close()", varname = "close")
    S_money$close <- TS_money$close
    S_money$total <- S_money$diff * S_money$close
    buy <- -sum(S_money$total[S_money$dir == 1])
    sell <- sum(S_money$total[S_money$dir == 2])
    net <- sell + buy
    S_summary <- data.frame(buy, sell, net)
    # make trade list
    if(style == "hs"){
      res <- S_combine
      res$amount <- res$diff
      res$commandPrice <- 0
      res$priceMode <- 4
      # res$mktCode <- getmktCode(res$stockID, format = "local")
      secuMarket <- SecuMarket(res$stockID) 
      res$mktCode <- ifelse(secuMarket==90L,2L,1L) # 1:SH, 2:SZ
      res$stockID <- stockID2tradeCode(res$stockID, IDsrc = "local")
      res <- dplyr::select(res, 
                           stockID,
                           dir,
                           amount,
                           commandPrice,
                           priceMode,
                           mktCode)
    }
    reslist <- list(res, S_summary)
    return(reslist)
  }
  
  
  check.colnames(port_ini, c("stockID","amount"))
  # if(sum(port_ini$wgt) != 1){stop("The weight does not sum up to 1.")} #does not work. Reason unknown.
  check.colnames(port_obj, c("stockID","wgt"))
  match.arg(outputstyle)
  
  # find nearest trading day 
  today <- Sys.Date()
  if(trday.is(today)){
    lastday <- trday.nearby(today, by = -1)
  }else{
    lastday <- trday.nearest(today, dir = -1)
  }
  # contruct TS_new
  date <- rep(lastday, length(port_obj$stockID))
  TS_new <- data.frame(date, "stockID" = port_obj$stockID)
  TS_new <- TS.getTech_ts(TS = TS_new, funchar = "close()",varname = "close")
  TS_new$amount <- port_obj$wgt*money_obj/TS_new$close
  # organize S_old and S_new
  S_old <- data.frame("stockID" = port_ini$stockID, "amount_old" = port_ini$amount)
  S_new <- data.frame("stockID" = TS_new$stockID, "amount_new" = TS_new$amount)
  # merge
  S_combine <- merge(S_old, S_new, by = "stockID", all = TRUE)
  S_combine$amount_old[is.na(S_combine$amount_old)] <- 0
  S_combine$amount_new[is.na(S_combine$amount_new)] <- 0
  S_combine$diff <- (S_combine$amount_new - S_combine$amount_old)
  S_combine$diff <- S_combine$diff / 100
  S_combine$diff[S_combine$amount_new != 0] <- round(S_combine$diff[S_combine$amount_new != 0]) 
  S_combine <- subset(S_combine, diff != 0)
  S_combine$dir <- (S_combine$diff < 0) + 1L
  S_combine$diff <- abs(S_combine$diff)
  if(splitN > 1){
    S_combine_N <- S_combine
    S_combine_N$diff <- S_combine_N$diff %/% splitN
    S_combine_N$diff <- S_combine_N$diff * 100
    S_combine_remain <- S_combine
    S_combine_remain$diff <- S_combine_remain$diff %% splitN
    S_combine_remain$diff <- S_combine_remain$diff * 100
    S_combine_remain <- subset(S_combine_remain, diff != 0)
    if(nrow(S_combine_remain) == 0){
      S_combine_remain <- NULL
    }
  }else{
    S_combine_N <- S_combine
    S_combine_N$diff <- S_combine_N$diff * 100
    S_combine_remain <- NULL
  }
  
  # get the cashflow and tradelist from the S_combine
  reslist_N <- make_tradelist_cashflow(S_combine_N, lastday, outputstyle)
  if(!is.null(S_combine_remain)){
    reslist_remain <- make_tradelist_cashflow(S_combine_remain, lastday, outputstyle)
  }else{
    reslist_remain <- NULL
  }
  # return:
  tradeList_N <- reslist_N[[1]]
  split <- reslist_N[[2]]
  if(!is.null(S_combine_remain)){
    tradeList_remain <- reslist_remain[[1]]
    total <- reslist_N[[2]]*splitN + reslist_remain[[2]]
  }else{
    tradeList_remain <- NULL
    total <- reslist_N[[2]]*splitN
  }
  summaryTable <- data.frame(splitN, 
                             "buy_total" = total$buy, 
                             "sell_total" = total$sell, 
                             "turnover" = (total$sell-total$buy)/money_obj/2, 
                             "net_total" = total$net, 
                             "buy_split" = split$buy, 
                             "sell_split" = split$sell, 
                             "net_split" = split$net)
  finalres <- list("tradeList_N" = tradeList_N, "tradeList_remain" = tradeList_remain, "summaryTable" = summaryTable)
  return(finalres)
}


# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    gf.xx functions     =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' gf_cap
#' @param bc_lambda NULL, "auto", or a specific numeric.
#' @export
#' @examples
#' system.time(re <- gf_cap(ts,datasrc = "local"))
#' system.time(re2 <- gf_cap(ts,datasrc = "memory"))
gf_cap <- function(TS,
                   log=FALSE,
                   bc_lambda = NULL,
                   var=c("float_cap","mkt_cap","free_cap"),
                   na_fill=TRUE,
                   varname="factorscore",
                   datasrc="memory"){
  var <- match.arg(var)
  # shrink columns, merge back later
  TS_core <- TS[,c("date","stockID")]
  # retrieve data
  if(datasrc=="local"){
    if(var=="free_cap"){
      re <- gf.free_float_sharesMV(TS_core)
      re <- renameCol(re,"factorscore","cap")
    } else {
      re <- getTech(TS_core,variables=var)
      re <- renameCol(re,var,"cap")
      re$cap <- re$cap/10000  # 100 million ("yi")
    }
  } else if(datasrc=="memory"){
    memory.load()
    TS_core_DT <- data.table(TS_core, key = c("stockID","date"))
    marketsize_DT <- data.table(.marketsize, key = c("stockID","date"))
    re <- marketsize_DT[TS_core_DT, roll = TRUE]
    re <- re[, c("date","stockID",var), with = FALSE]
    colnames(re) <- c("date","stockID","cap")
    re <- as.data.frame(re)
  }
  
  if(log){
    re$cap <- log(re$cap)
  }
  if(!is.null(bc_lambda)){
    re <- factor_bcPower(re, fname = "cap", lambda = bc_lambda)
  }
  if(na_fill){
    re[is.na(re$cap),"cap"] <- median(re$cap, na.rm = TRUE)
  }
  re <- renameCol(re,"cap",varname)
  # merge back
  re <- merge.x(TS, re, by = c("date","stockID"))
  return(re)
}


#' fl_cap
#' @export
fl_cap <- function(log=FALSE,bc_lambda=NULL,var="float_cap",na_fill=TRUE,datasrc="memory"){
  re <- list(
    factorFun = "gf_cap",
    factorPar = list(log=log, bc_lambda=bc_lambda, var=var, na_fill=na_fill,datasrc=datasrc),
    factorDir = 1 ,
    factorRefine = list(
      outlier=list(method = "none", par=NULL, sectorAttr= NULL),
      std=list(method = "none", log=FALSE, sectorAttr=NULL, regLists=NULL),
      na=list(method = "none", sectorAttr=NULL)
    ),   
    factorName = if(log) "ln_cap" else "cap",
    factorID = "",
    factorType = "",
    factorDesc = ""
  )
  return(re)
}


#' gf.free_float_shares
#' @export
gf.free_float_shares <- function(TS){
  
  qr <- "select b.date, b.stockID,a.freeShares as 'factorscore'
  from QT_FreeShares a, yrf_tmp b
  where a.rowid=(
  select rowid from QT_FreeShares
  where stockID=b.stockID and date<=b.date
  order by date desc limit 1)"
  
  con <- db.local("main")
  TS$date <- rdate2int(TS$date)
  dbWriteTable(con,name="yrf_tmp",value=TS,row.names = FALSE,overwrite = TRUE)
  re <- dbGetQuery(con,qr)
  re <- merge.x(TS,re,by=c("date","stockID"))
  re <- transform(re, date=intdate2r(date))
  dbDisconnect(con)
  return(re)
}


#' gf.free_float_sharesMV
#' @export
gf.free_float_sharesMV <- function(TS){
  ffs <- gf.free_float_shares(TS)
  close <- getTech(TS,variables='close')
  re <- merge.x(ffs,close,by=c('date','stockID'))
  re$factorscore <- re$factorscore*re$close
  re <- re[,c("date","stockID","factorscore")]
  return(re)
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    transplanted inner functions     =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

# copy from RFactorModel
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


