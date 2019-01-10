



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================
# ===================== Database Operation  ===========================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================

#' defaultDataSRC
#' 
#' get the default datasrc. You can reset the default value by eg. \code{options(datasrc="quant")}
#' @return a character string, the value of the default datasrc. 
#' @export
#' @examples
#' # -- get the default datasrc
#' defaultDataSRC()
#' # -- reset 
#' options(datasrc="quant")
#' # -- reget
#' defaultDataSRC()
defaultDataSRC <- function(){
  getOption("datasrc",default="local")
}

#' origin_sql
#' @export
origin_sql <- function(){
  return("1970-01-01")
}


#' Database connection
#' 
#' connect database wind, quant, cs, jy, local, ...
#' @rdname db.connection
#' @return a database connection.
#' @export
db.local <- function(dbname = "QTlite",dbpath="D:/sqlitedb"){
  driver = DBI::dbDriver("SQLite")
  dbname <- paste(paste(dbpath,dbname,sep = "/"),".db",sep="")
  dbConnect(driver, dbname = dbname)
}
#' @rdname db.connection
#' @export
db.quant <- function(uid = "wsread",pwd = "wsread"){
  odbcConnect("jyquant", uid = uid, pwd = pwd)
}
#' @rdname db.connection
#' @export
db.cs <- function(){
  odbcConnect("csdb", uid = "wsread",pwd = "wsread")
}
#' @rdname db.connection
#' @export
db.jy <- function(){
  odbcConnect("jy", uid = "jyread",pwd = "jyread")
}
#' @rdname db.connection
#' @export
db.wind <- function(){
  odbcConnect("wind", uid = "wsread",pwd = "wsread")
}

# db.lite <- function(){ # connect SQLite by ODBC
#   odbcConnect("lite")
# }




#' queryAndClose.odbc
#'
#' read data from a ODBC data source with a query
#' @param db a ODBC database object
#' @param query a character string,indicating the query to execute 
#' @return a dataframe
#' @author Ruifei.Yin
#' @export
#' @examples 
#' queryAndClose.odbc(db.quant(),"select top 10 * from QT_DailyQuote")
queryAndClose.odbc <- function (db, query, as.is=FALSE, ...) {
  table = sqlQuery(db, query, as.is = as.is, ...)
  odbcClose(db)
  return(table) 
}


#' queryAndClose.dbi
#'
#' read data from a DBI data source with a query
#' @param db a DBI data source object
#' @param query a character string,indicating the query to execute 
#' @return a dataframe
#' @author Ruifei.Yin
#' @export
#' @examples 
#' queryAndClose.dbi(db.local("qt"),"select * from QT_DailyQuote limit 10")
queryAndClose.dbi <- function (db, query, ...) {
  table = dbGetQuery(db, query, ...)
  dbDisconnect(db)
  return(table) 
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================
# ===================== Database Updating  ===========================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================


# --------------------  ~~ from 'jyquant' ----------------
#' lcdb.updatetime
#' 
#' get the updatetime of tables in lcdb
#' @return a datafrme, with cols: "table", "updatetime".
#' @export
lcdb.updatetime <- function () {
  con_main <- db.local("main")
  con_fs <- db.local("fs")
  con_qt <- db.local("qt")
  updatetime <- c(
    dbGetQuery(con_main,"select max(EndDate) from LC_IndexComponentsWeight")[[1]],
    dbGetQuery(con_main,"select max(TradingDay) from QT_IndexQuote")[[1]]        ,
    dbGetQuery(con_qt,"select max(TradingDay) from QT_DailyQuote")[[1]]       ,
    dbGetQuery(con_fs,"select max(TradingDay) from QT_FactorScore")[[1]]       ,
    dbGetQuery(con_main,"select max(PublDate) from LC_RptDate")[[1]]             ,
    dbGetQuery(con_main,"select max(InfoPublDate) from LC_PerformanceGrowth")[[1]],
    dbGetQuery(con_main,"select max(date) from QT_FreeShares")[[1]],
    dbGetQuery(con_qt,"select max(updateDate) from QT_sus_res")[[1]]
  )
  table <- c(
    "LC_IndexComponentsWeight",
    "QT_IndexQuote",
    "QT_DailyQuote",
    "QT_FactorScore",
    "LC_RptDate",
    "LC_PerformanceGrowth",
    "QT_FreeShares",
    "QT_sus_res"
  )
  dbDisconnect(con_main)
  dbDisconnect(con_fs)
  dbDisconnect(con_qt)
  return(data.frame(table,updatetime))
}




#' update the local database
#' @return NULL
#' @export
lcdb.update <- function(){
  lcdb.update.SecuMain()                 ;  message("lcdb.update.SecuMain()... Done ");
  lcdb.update.QT_DailyQuote()           ;  message("lcdb.update.QT_DailyQuote()... Done ");
  lcdb.update.QT_TradingDay()            ;  message("lcdb.update.QT_TradingDay()... Done");
  lcdb.update.QT_sus_res()               ;  message("lcdb.update.QT_sus_res()... Done");
  lcdb.update.CT_SystemConst()           ;  message("lcdb.update.CT_SystemConst()... Done");
  lcdb.update.CT_IndustryList()          ;  message("lcdb.update.CT_IndustryList()... Done");
  
  lcdb.update.LC_ExgIndustry()           ;  message("lcdb.update.LC_ExgIndustry()... Done");
  lcdb.fix.swindustry()                  ;  message("lcdb.fix.swindustry()... Done");
  lcdb.fix.ezindustry()                  ;  message("lcdb.fix.ezindustry()... Done");
  
  lcdb.update.LC_IndexComponent()        ;  message("lcdb.update.LC_IndexComponent()... Done ");
  lcdb.add.LC_IndexComponent("EI000985") ;  message("lcdb.add.LC_IndexComponent('EI000985')... Done ");
  
  lcdb.update.LC_IndexComponentsWeight() ;  message("lcdb.update.LC_IndexComponentsWeight()... Done");
  
  lcdb.update.QT_IndexQuote()            ;  message("lcdb.update.QT_IndexQuote()... Done ");
  lcdb.update.IndexQuote_000985E()       ;  message("lcdb.update.IndexQuote_000985E()... Done ");
  
  lcdb.update.LC_RptDate()               ;  message("lcdb.update.LC_RptDate()... Done ");
  lcdb.update.LC_PerformanceGrowth()     ;  message("lcdb.update.LC_PerformanceGrowth()... Done ");
  lcdb.update.QT_FreeShares()            ;  message("lcdb.update.QT_FreeShares()... Done ");
  lcdb.update.QT_Size()                  ;  message("lcdb.update.QT_Size()... Done ");
  # lcdb.update.QT_Rf()                    ;  message("lcdb.update.QT_Rf()... Done ");
  lcdb.update.QT_FactorScore(type = "alpha")    ;  message("lcdb.update.QT_FactorScore(alpha)... Done ");
  lcdb.update.QT_FactorScore(type = "risk")     ;  message("lcdb.update.QT_FactorScore(risk)... Done ");
  lcdb.update.barra_basic()              ;  message("lcdb.update.QT_barra_basic()... Done ");
  lcdb.update.barra_adv()                ;  message("lcdb.update.QT_barra_adv()... Done ");
  
}

#' @rdname lcdb.update
#' @export
lcdb.update.SecuMain <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from SecuMain", as.is=4)
  con <- db.local("main")
  dbExecute(con,"delete from SecuMain")
  dbWriteTable(con,"SecuMain",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.QT_TradingDay <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from QT_TradingDay")
  con <- db.local("main")
  dbExecute(con,"delete from QT_TradingDay")
  dbWriteTable(con,"QT_TradingDay",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.CT_SystemConst <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from CT_SystemConst")
  con <- db.local("main")
  dbExecute(con,"delete from CT_SystemConst")
  dbWriteTable(con,"CT_SystemConst",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.CT_IndustryList <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from CT_IndustryList")
  con <- db.local("main")
  dbExecute(con,"delete from CT_IndustryList")
  dbWriteTable(con,"CT_IndustryList",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.LC_ExgIndustry <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from LC_ExgIndustry")
  con <- db.local("main")
  dbExecute(con,"delete from LC_ExgIndustry")
  dbWriteTable(con,"LC_ExgIndustry",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.LC_IndexComponent <- function(){
  tb.from <- queryAndClose.odbc(db.quant(),query="select * from LC_IndexComponent")
  con <- db.local("main")
  dbExecute(con,"delete from LC_IndexComponent")
  dbWriteTable(con,"LC_IndexComponent",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
#' @param begT a numeric date. if missing, '\code{max(EndDate)}' in the lcdb.
#' @param endT a numeric date. if missing, 99990101.
#' @aliases indexID a vector of indexID. if missing, all the index in server database.
#' @examples 
#' lcdb.update.LC_IndexComponentsWeight() # update all the indexs up to date
#' lcdb.update.LC_IndexComponentsWeight(20060101,20060330) # update all the indexs in given period
#' lcdb.update.LC_IndexComponentsWeight(19000101,99990101,"EI000905") # update all the data of given index
#' lcdb.update.LC_IndexComponentsWeight(20060101,20060330,"EI000905")
lcdb.update.LC_IndexComponentsWeight <- function(begT,endT,IndexID){  
  con <- db.local("main")
  if(TRUE){
    if(missing(begT)){
      begT <- dbGetQuery(con,"select max(EndDate) from LC_IndexComponentsWeight")[[1]]
    }
    begT_filt <- paste("EndDate >=",begT)
    if(missing(endT)){
      endT <- 99990101
    }
    endT_filt <- paste("EndDate < ",endT)
    if(missing(IndexID)){
      pool_filt <- "1>0"
    } else{
      pool_filt <- paste("IndexID in",brkQT(IndexID))
    }
  }
  tb.from <- queryAndClose.odbc(db.quant(),query=paste("select * from LC_IndexComponentsWeight where",begT_filt,"and",endT_filt,"and",pool_filt))  
  if(NROW(tb.from)==0){
    return()
  }
  dbExecute(con,paste("delete from LC_IndexComponentsWeight where",begT_filt,"and",endT_filt,"and",pool_filt))
  dbWriteTable(con,"LC_IndexComponentsWeight",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}






#' @rdname lcdb.update
#' @export
lcdb.update.QT_IndexQuote <- function(begT,endT,IndexID,datasrc=c("quant","jy")){
  datasrc <- match.arg(datasrc)
  con <- db.local("main")
  if(TRUE){
    if(missing(begT)){
      if(missing(IndexID)){
        begT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
      } else {
        begT <- dbGetQuery(con,"select min(TradingDay) from QT_IndexQuote")[[1]]
      }
    }
    begT_filt <- paste("TradingDay >=",begT)
    if(missing(endT)){
      if(missing(IndexID)){
        endT <- 99990101
      } else {
        endT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
      }
    }
    endT_filt <- paste("TradingDay < ",endT)
    if(missing(IndexID)){
      pool_filt <- "1>0"
    } else{
      pool_filt <- paste("ID in",brkQT(IndexID))
    }
  }
  
  if(datasrc=='quant'){
    tb.from <- queryAndClose.odbc(db.quant(),query=paste("select * from QT_IndexQuote where ",begT_filt,"and",endT_filt,"and",pool_filt))
  }else if(datasrc=='jy'){
    begT_filt_ <- paste("TradingDay >=",QT(intdate2r(begT)))
    endT_filt_ <- paste("TradingDay < ",QT(intdate2r(endT)))
    IndexID_ <- stringr::str_replace(IndexID,'EI','')
    pool_filt_ <- paste("SecuCode in",brkQT(IndexID_))
    qr <- paste("SELECT q.InnerCode,
                year(TradingDay)*10000+month(TradingDay)*100+day(TradingDay) 'TradingDay',
                PrevClosePrice,OpenPrice,HighPrice,LowPrice,ClosePrice,TurnoverVolume,
                TurnoverValue,TurnoverDeals,ChangePCT,NegotiableMV,
                (case when PrevClosePrice is not null and PrevClosePrice <> 0 then ClosePrice/PrevClosePrice-1 else null end) 'DailyReturn',
                'EI'+s.SecuCode 'ID'
                FROM QT_IndexQuote q,SecuMain s
                where q.InnerCode=s.InnerCode and ",begT_filt_,"and",endT_filt_,"and",pool_filt_)
    tb.from <- queryAndClose.odbc(db.jy(),query=qr) 
  }
  
  if(NROW(tb.from)==0){
    return()
  }
  dbExecute(con,paste("delete from QT_IndexQuote where",begT_filt,"and",endT_filt,"and",pool_filt))
  dbWriteTable(con,"QT_IndexQuote",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con) 
}



#' @rdname lcdb.update
#' @export
lcdb.update.QT_DailyQuote <- function(begT,endT,stockID,loopFreq="100 year"){
  con <- db.local("qt")
  if(TRUE){
    if(missing(begT)){
      if(missing(stockID)){
        begT <- dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote")[[1]]
      } else{
        begT <- dbGetQuery(con,"select min(TradingDay) from QT_DailyQuote")[[1]]
      }
    }
    if(missing(endT)){
      if(missing(stockID)){
        endT <- 99990101
      } else {
        endT <- dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote")[[1]]
      }
    }
    if(missing(stockID)){
      pool_filt <- "1>0"
    } else{
      pool_filt <- paste("ID in",brkQT(stockID))
    }
  }
  
  endT <- min(intdate2r(endT), Sys.Date())
  dates <- c(seq(intdate2r(begT), endT ,by = loopFreq), endT)
  dates <- rdate2int(dates)
  for(ii in 1:(length(dates)-1)){
    message(paste("lcdb.update.QT_DailyQuote: updating to ",dates[ii+1],"..."))
    begT_filt <- paste("TradingDay >=",dates[ii])
    endT_filt <- paste("TradingDay < ",dates[ii+1])
    
    tb.from <- queryAndClose.odbc(db.quant(),query=paste("select * from QT_DailyQuote where ",begT_filt,"and",endT_filt,"and",pool_filt))
    if(NROW(tb.from)==0){
      return()
    }
    dbExecute(con,paste("delete from QT_DailyQuote where",begT_filt,"and",endT_filt,"and",pool_filt))
    dbWriteTable(con,"QT_DailyQuote",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
    gc()
  }
 
  dbDisconnect(con)
}

#' @rdname lcdb.update
#' @export
lcdb.update.LC_RptDate <- function(begT,endT,stockID){  
  con <- db.local("main")
  
  if(missing(begT)){
    begT <- 10000101
  }
  if(missing(endT)){
    endT <- 99990101
  }
  
  if(missing(stockID)){
    pool_filt <- ""
  } else{
    pool_filt <- paste(" and stockID in",brkQT(stockID))
  }
  
  qr <- paste("select * from LC_RptDate where PublDate >=",begT,"and PublDate <=",endT,pool_filt)
  tb.from <- queryAndClose.odbc(db.quant(),qr)  
  if(NROW(tb.from)==0){
    dbDisconnect(con)
    return()
  }
  qr <- paste("delete from LC_RptDate where PublDate >=",begT,"and PublDate<=",endT,pool_filt)
  dbExecute(con,qr)
  dbWriteTable(con,"LC_RptDate",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}



#' @rdname lcdb.update
#' @export
lcdb.update.LC_PerformanceGrowth <- function(begT,endT){
  con <- db.local("main")
  if(missing(begT)){
    begT <- dbGetQuery(con,"select max(InfoPublDate) from LC_PerformanceGrowth")[[1]]
  } 
  if(missing(endT)){
    endT <- 99990101
  }
  tb.from <- queryAndClose.odbc(db.quant(),query=paste("select * from LC_PerformanceGrowth where InfoPublDate>=",begT,"and InfoPublDate<=",endT))
  if(NROW(tb.from)==0){
    return()
  }
  dbExecute(con,paste("delete from LC_PerformanceGrowth where InfoPublDate >=",begT,"and InfoPublDate<=",endT))
  dbWriteTable(con,"LC_PerformanceGrowth",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}


# --------------------  ~~ fixing ----------------

#' add a index to local database from JY database
#'
#'
#' @author Andrew Dow
#' @param indexID is index code,such as EI000300
#' @return nothing
#' @examples
#' lcdb.add.LC_IndexComponent(indexID="EI801003")
#' lcdb.add.LC_IndexComponent(indexID="EI000985")
#' @export
lcdb.add.LC_IndexComponent <- function(indexID){
  qr1 <- paste("select ID,InnerCode,CompanyCode,'EI'+SecuCode 'SecuCode',SecuAbbr,
               SecuMarket,ListedSector,ListedState,JSID 'UpdateTime',
               SecuCode 'StockID_TS',SecuCategory,
               convert(varchar,ListedDate,112) 'ListedDate',SecuCode 'StockID_wind'
               from SecuMain WHERE SecuCode=",QT(substr(indexID,3,8)),
               " and SecuCategory=4",sep='')
  indexInfo <- queryAndClose.odbc(db.jy(),qr1,stringsAsFactors=FALSE)
  
  qr2 <- paste("SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
               convert(varchar(8),l.InDate,112) 'InDate',
               convert(varchar(8),l.OutDate,112) 'OutDate',l.Flag,l.XGRQ 'UpdateTime'
               FROM LC_IndexComponent l inner join SecuMain s1
               on l.IndexInnerCode=s1.InnerCode and s1.SecuCode=",QT(substr(indexID,3,8)),
               " left join SecuMain s2 on l.SecuInnerCode=s2.InnerCode",
               " where s2.SecuCode like '3%' or s2.SecuCode like '6%' or s2.SecuCode like '0%'")
  indexComp <- queryAndClose.odbc(db.jy(),qr2,stringsAsFactors=FALSE)
  
  if(indexID=='EI000985'){
    changeDate <- as.Date('2011-08-02')
    
    indexInfo <- transform(indexInfo,ID=indexID,
                           SecuCode=substr(SecuCode,3,8),
                           StockID_TS='SH000985',
                           StockID_wind='000985.SH')
    
    #part 2 update local LC_IndexComponent
    qr <- paste("SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
                convert(varchar(8),l.InDate,112) 'InDate',
                convert(varchar(8),l.OutDate,112) 'OutDate',
                convert(varchar(8),s2.ListedDate,112) 'IPODate'
                FROM LC_IndexComponent l
                inner join SecuMain s1 on l.IndexInnerCode=s1.InnerCode and s1.SecuCode='801003'
                left join SecuMain s2 on l.SecuInnerCode=s2.InnerCode
                where (s2.SecuCode like '3%' or s2.SecuCode like '6%' or s2.SecuCode like '0%')
                and l.InDate<",QT(changeDate))
    re <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
    
    if(TRUE){ # -- 801003
      tmp <- transform(re,InDate=intdate2r(InDate),
                       OutDate=intdate2r(OutDate),
                       IPODate=intdate2r(IPODate)+90)
      tmp[tmp$InDate<tmp$IPODate,'InDate'] <- tmp[tmp$InDate<tmp$IPODate,'IPODate']
      tmp <- tmp[tmp$InDate < changeDate,c("SecuID","InDate","OutDate")]
      tmp[is.na(tmp$OutDate),'OutDate'] <- changeDate
      tmp[tmp$OutDate>changeDate,'OutDate'] <- changeDate
      
      qr <- paste("select 'EQ'+s.SecuCode 'SecuID',
                  case when st.SpecialTradeType in(2,4,6) then convert(varchar(8),st.SpecialTradeTime,112)
                  else NULL end 'InDate',
                  case when st.SpecialTradeType in(1,3,5) then convert(varchar(8),st.SpecialTradeTime,112)
                  else NULL end 'OutDate'
                  from LC_SpecialTrade st,SecuMain s
                  where st.InnerCode=s.InnerCode and s.SecuCategory=1
                  and st.SpecialTradeTime<",QT(changeDate),
                  " and st.SpecialTradeType in(1,2,3,4,5,6)
                  and (s.SecuCode like '3%' or s.SecuCode like '6%' or s.SecuCode like '0%')
                  order by s.SecuCode,st.SpecialTradeTime")
      st <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
      st <- transform(st,InDate=intdate2r(InDate),
                      OutDate=intdate2r(OutDate))
      st[is.na(st$OutDate),'OutDate'] <- changeDate
      
      tmp <- rbind(tmp[,c("SecuID","InDate","OutDate")],st)
      tmp <- reshape2::melt(tmp,id=c('SecuID'))
      tmp <- na.omit(tmp)
      tmp <- unique(tmp)
      tmp <- dplyr::arrange(tmp,SecuID,value)
      
      tmp$flag <- c(1)
      for(i in 2: nrow(tmp)){
        if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i-1]=='InDate' && tmp$variable[i]=='InDate'){
          tmp$flag[i-1] <- 0
        }else if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i-1]=='OutDate' && tmp$variable[i]=='OutDate'){
          tmp$flag[i] <- 0
        }else{
          next
        }
      }
      tmp <- tmp[tmp$flag==1,c("SecuID","variable","value")]
      tmp <- cbind(tmp[tmp$variable=='InDate',c("SecuID","value")],
                   tmp[tmp$variable=='OutDate',"value"])
      colnames(tmp) <- c("SecuID","InDate","OutDate")
      tmp <- transform(tmp,IndexID='EI000985',
                       Flag=0,
                       UpdateTime=Sys.time(),
                       InDate=rdate2int(InDate),
                       OutDate=rdate2int(OutDate))
      tmp <- tmp[,c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    }
    indexComp <- rbind(indexComp,tmp)
    
  }else{
    #part 1 update local SecuMain
    indexInfo <- transform(indexInfo,ID=indexID,
                           SecuCode=substr(SecuCode,3,8),
                           StockID_TS=ifelse(is.na(stockID2stockID(indexID,'local','ts')),substr(indexID,3,8),
                                             stockID2stockID(indexID,'local','ts')),
                           StockID_wind=ifelse(is.na(stockID2stockID(indexID,'local','wind')),substr(indexID,3,8),
                                               stockID2stockID(indexID,'local','wind')))
  }
  con <- db.local("main")
  dbExecute(con,paste("delete from SecuMain where ID=",QT(indexID),sep=''))
  dbExecute(con,paste("delete from LC_IndexComponent where IndexID=",QT(indexID),sep=''))
  
  dbWriteTable(con,"SecuMain",indexInfo,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,"LC_IndexComponent",indexComp,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}


#' fix shenwan new industry rule
#'
#' Due to the SHENWAN inustry standard changing in 2014, fix local database's shenwan industry rule's bug and make the rule keep consistent. The new industry standard is 33.  Update 3 related local tables:CT_SystemConst,CT_IndustryList and LC_ExgIndusry.  
#' @rdname lcdb.update
#' @author Andrew Dow
#' @return nothing.
#' @examples
#' lcdb.fix.swindustry()
#' @export
lcdb.fix.swindustry <- function(){
  
  #get raw data
  qr <- "SELECT 'EQ'+s.SecuCode 'stockID',l.CompanyCode,l.FirstIndustryCode 'Code1',l.FirstIndustryName 'Name1',
  l.SecondIndustryCode 'Code2',l.SecondIndustryName 'Name2',l.ThirdIndustryCode 'Code3',
  l.ThirdIndustryName 'Name3',convert(varchar, l.InfoPublDate, 112) 'InDate',
  convert(varchar, l.CancelDate, 112) 'OutDate',l.InfoSource,l.Standard,l.Industry,
  l.IfPerformed 'Flag',convert(float,l.XGRQ) 'UpdateTime',
  convert(varchar, s.ListedDate, 112) 'IPODate'
  FROM LC_ExgIndustry l,SecuMain s
  where l.CompanyCode=s.CompanyCode and s.SecuCategory=1
  and s.SecuMarket in(83,90) and l.Standard in(9,24)
  order by l.Standard,l.InfoPublDate"
  re <- queryAndClose.odbc(db.jy(),qr,as.is = TRUE)
  re <- re %>% filter(substr(stockID,1,3) %in% c('EQ6','EQ3','EQ0'),!is.na(IPODate)) %>% 
    mutate(InDate=as.integer(InDate),OutDate=as.integer(OutDate),UpdateTime=as.double(UpdateTime),IPODate=as.integer(IPODate))
  
  #use standard 24 data directly
  sw24use <- re %>% filter(Standard==24) %>% dplyr::select(-IPODate)
  
  #use standard 9 data before standard 24 published date
  sw9use <- re %>% filter(Standard==9,InDate<20140101,IPODate<20140101)
  sw9use[is.na(sw9use$OutDate) | sw9use$OutDate>20140101,'OutDate'] <- 20140101
  sw9use <- sw9use %>% mutate(Flag=2,unlistDate=trday.unlist(stockID)) %>% mutate(unlistDate=rdate2int(unlistDate))
  sw9use <- sw9use[is.na(sw9use$unlistDate) | sw9use$InDate<sw9use$unlistDate,] # remove Indate> unlistdate
  
  # remove outdate> unlistdate
  sw9use <- sw9use %>% mutate(OutDate=ifelse(!is.na(sw9use$unlistDate) & sw9use$OutDate>sw9use$unlistDate,unlistDate,OutDate)) %>% 
    dplyr::select(-IPODate,-unlistDate) %>% 
    dplyr::rename(OldCode1=Code1,OldName1=Name1,OldCode2=Code2,OldName2=Name2,OldCode3=Code3,OldName3=Name3)
  
  #convert old industry to new industry
  sw24tmp <- sw24use[sw24use$InDate==20140101,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")]
  sw9part1 <- sw9use[sw9use$OutDate==20140101,]
  sw9part1 <- dplyr::left_join(sw9part1,sw24tmp,by='stockID')
  
  #get industry match table
  indmatch <- unique(sw9part1[,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")])
  indmatch <- plyr::ddply(indmatch,~OldName3,plyr::mutate,n=length(OldName3))
  indmatch <- indmatch[indmatch$n==1,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  sw9part1 <- sw9part1[,colnames(sw24use)]
  
  sw9part2 <- sw9use[sw9use$OutDate<20140101,]
  sw9part2 <- dplyr::left_join(sw9part2,indmatch,by=c("OldCode1","OldName1",
                                                      "OldCode2","OldName2",
                                                      "OldCode3","OldName3"))
  sw9part3 <- sw9part2 %>% filter(is.na(Code1)) %>% dplyr::select(-Code1,-Name1,-Code2,-Name2,-Code3,-Name3)
  sw9part2 <- sw9part2[!is.na(sw9part2$Code1),colnames(sw24use)]
  
  sw9part3 <- dplyr::left_join(sw9part3,sw24tmp,by='stockID')
  sw9part3 <- sw9part3[,colnames(sw24use)]
  
  sw9use <- rbind(sw9part1,sw9part2,sw9part3)
  
  #fill na to zonghe industry
  zhcn <- unique(sw24use[sw24use$Code1==510000,'Name1'])
  sw9use[is.na(sw9use$Code1),c("Name1","Name2","Name3")] <- zhcn
  sw9use[is.na(sw9use$Code1),"Code1"] <-510000
  sw9use[is.na(sw9use$Code2),"Code2"] <-510100
  sw9use[is.na(sw9use$Code3),"Code3"] <-510101
  
  sw33 <- rbind(sw9use,sw24use)
  sw33 <- transform(sw33,Standard=33,
                    Code1=paste('ES33',Code1,sep = ''),
                    Code2=paste('ES33',Code2,sep = ''),
                    Code3=paste('ES33',Code3,sep = ''),
                    Code99=c(NA),
                    Name99=c(NA),
                    Code98=c(NA),
                    Name98=c(NA))
  sw33 <- dplyr::arrange(sw33,stockID,InDate)
  
  #deal with abnormal condition
  #1 outdate<=indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),TRUE,sw33$OutDate>sw33$InDate),]
  #2 one stock has two null outdate
  tmp <- sw33 %>% dplyr::group_by(stockID) %>% 
    dplyr::summarise(NANum=sum(is.na(OutDate))) %>% 
    dplyr::ungroup() %>% dplyr::filter(NANum>1)
  
  if(nrow(tmp)>0){
    tmp <- tmp$stockID
    sw33tmp <- sw33[(sw33$stockID %in% tmp) & is.na(sw33$OutDate),]
    sw33 <- sw33[!((sw33$stockID %in% tmp) & is.na(sw33$OutDate)),]
    sw33tmp <- sw33tmp %>% dplyr::group_by(stockID) %>% dplyr::filter(InDate==min(InDate)) %>% dplyr::ungroup()
    sw33 <- rbind(sw33,sw33tmp)
    sw33 <- dplyr::arrange(sw33,stockID,InDate)
  }
  
  #3 indate[i+1]!=outdate[i]
  sw33$tmpstockID <- c(sw33$stockID[1],sw33$stockID[1:(nrow(sw33)-1)])
  sw33$tmpOutDate <- c(NA,sw33$OutDate[1:(nrow(sw33)-1)])
  sw33$InDate <- ifelse(ifelse(is.na(sw33$tmpOutDate),FALSE,sw33$stockID==sw33$tmpstockID & sw33$InDate!=sw33$tmpOutDate),
                        sw33$tmpOutDate,sw33$InDate)
  sw33 <- subset(sw33,select=-c(tmpstockID,tmpOutDate))
  # 4 duplicate indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),TRUE,sw33$OutDate>sw33$InDate),]
  sw33[!is.na(sw33$OutDate) & sw33$Flag==1,'Flag'] <- 2
  
  # update local database CT_IndustryList
  qr <- "SELECT Standard,Classification 'Level','ES33'+IndustryCode 'IndustryID'
  ,IndustryName,SectorCode 'Alias','ES33'+FirstIndustryCode 'Code1'
  ,FirstIndustryName 'Name1','ES33'+SecondIndustryCode 'Code2'
  ,SecondIndustryName 'Name2','ES33'+ThirdIndustryCode 'Code3'
  ,ThirdIndustryName 'Name3',convert(float,UpdateTime) 'UpdateTime'
  FROM CT_IndustryType where Standard=24"
  indCon <- queryAndClose.odbc(db.jy(),qr,as.is = TRUE)
  indCon <- transform(indCon,Standard=33,UpdateTime=as.double(UpdateTime))
  indCon[is.na(indCon$Name2),'Code2'] <- NA
  indCon[is.na(indCon$Name3),'Code3'] <- NA
  
  # update local database CT_SystemConst
  syscon <- queryAndClose.odbc(db.jy(),"select top 1 LB, LBMC, DM ,MS  from CT_SystemConst where LB=1081",as.is = TRUE)
  syscon <- transform(syscon,DM=33, MS="SHENWAN2014fixed")
  
  # update...
  con <- db.local("main")
  res <- dbSendQuery(con,"delete  from LC_ExgIndustry where Standard=33")
  dbClearResult(res)
  res <- dbSendQuery(con,"delete  from CT_IndustryList where Standard=33")
  dbClearResult(res)
  res <- dbSendQuery(con,"delete  from CT_SystemConst where LB=1081 and DM=33")
  dbClearResult(res)
  
  dbWriteTable(con,'LC_ExgIndustry',sw33,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_IndustryList',indCon,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_SystemConst',syscon,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
  # return('Done!')
}




#' @rdname lcdb.update
#' @author Qian Han
#' @return nothing.
#' @examples
#' lcdb.fix.ezindustry()
#' @export
lcdb.fix.ezindustry <- function(){
  con <- db.local("main")
  # Sec
  seclist <- list()
  namelist <- list()
  seclist[[1]] <- c("ES33110000","ES33210000","ES33220000","ES33230000","ES33240000")
  namelist[[1]] <- "BigCycle" 
  seclist[[2]] <- c("ES33480000","ES33490000","ES33430000")
  namelist[[2]] <- "FinRealEstate"
  seclist[[3]] <- c("ES33710000","ES33720000","ES33730000","ES33270000")
  namelist[[3]] <- "TMT"
  seclist[[4]] <- c("ES33280000","ES33330000","ES33340000","ES33350000","ES33460000","ES33370000","ES33450000")
  namelist[[4]] <- "Comsumption"
  seclist[[5]] <- c("ES33360000","ES33630000","ES33640000","ES33610000","ES33620000","ES33650000")
  namelist[[5]] <- "Manufacturing"
  seclist[[6]] <- c("ES33420000","ES33410000","ES33510000")
  namelist[[6]] <- "Others"
  
  # LC_ExgIndustry
  qr <- paste(" select * from LC_ExgIndustry
              where Standard = 33")
  tmpdat <- DBI::dbGetQuery(con, qr)
  for(i in 1:nrow(tmpdat)){
    for(j in 1:6){
      if(tmpdat$Code1[i] %in% seclist[[j]]){
        tmpdat$Code1[i] <- paste0("ES",j)
        tmpdat$Name1[i] <- namelist[[j]]
      }
    }
  }
  tmpdat$Standard <- 336
  
  # CT_SystemConst
  tmpdat2 <- DBI::dbReadTable(con, "CT_SystemConst")
  tmpdat2 <- subset(tmpdat2, LB == 1081 & DM == 33)
  tmpdat2$DM <- 336
  tmpdat2$MS <- "6EasyIndustryCategory"
  
  # CT_IndustryList
  qr3 <- " select * from CT_IndustryList 
  where Standard = 33
  and Level = 1"
  tmpdat3 <- DBI::dbGetQuery(con, qr3)
  tmpdat3 <- tmpdat3[1:6,]
  tmpdat3$Standard <- 336
  tmpdat3$IndustryID <- paste0("ES",1:6)
  tmpdat3$IndustryName <- unlist(namelist)
  tmpdat3$Code1 <- tmpdat3$IndustryID
  tmpdat3$Name1 <- tmpdat3$IndustryName
  
  # Update into LCDB
  dbExecute(con,"delete from LC_ExgIndustry where Standard=336")
  dbExecute(con,"delete from CT_IndustryList where Standard=336")
  dbExecute(con,"delete from CT_SystemConst where LB=1081 and DM=336")
  dbWriteTable(con,'LC_ExgIndustry',tmpdat,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_SystemConst',tmpdat2,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_IndustryList',tmpdat3,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
  return('Done!')
}





#' lcdb.update.IndexQuote_000985E
#'
#' @examples
#' lcdb.update.IndexQuote_000985E()
#' @export
lcdb.update.IndexQuote_000985E <- function(begT,endT){
  con <- db.local("main")
  con_qt <- db.local("qt")
  
  if(TRUE){
    if(missing(begT)){
      begT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote where ID='EI000985E'")[[1]]
    }
    if(missing(endT)){
      endT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
    }
  }
  
  if(begT==endT){
    return('Alread up to date!')
  } else if(begT>endT){
    stop("please update the 'QT_IndexQuote' table firstly!")
  } else {
    init_data <- dbGetQuery(con,paste("select * from QT_IndexQuote where ID='EI000985E' and TradingDay=",begT))
    begT <- intdate2r(begT)
    endT <- intdate2r(endT)
    begT <- trday.nearby(begT,by=1)  # -- one day after
    # TS <- getTS(begT,indexID = 'EI000985')
    TS <- getIndexComp(indexID = 'EI000985',endT = begT,drop=FALSE)
    # tmp.dates <- getRebDates(begT,endT,rebFreq = 'day')
    tmp.dates <- trday.get(begT,endT)
    
    message('calculating',rdate2int(min(tmp.dates)),"~",rdate2int(max(tmp.dates)),'...')
    qr <- paste("select TradingDay,ID,DailyReturn from QT_DailyQuote
                where TradingDay>=",rdate2int(min(tmp.dates))," and TradingDay<=",rdate2int(max(tmp.dates)))
    quotedf <- dbGetQuery(con_qt,qr)
    quotedf$TradingDay <- intdate2r(quotedf$TradingDay)
    quotedf <- quotedf[quotedf$ID %in% TS$stockID,]
    
    index <- quotedf %>% dplyr::group_by(TradingDay) %>%
      dplyr::summarise(DailyReturn = mean(DailyReturn, na.rm = TRUE))
    
    
    tmp <- xts::xts(index$DailyReturn, order.by = index$TradingDay)
    tmp <- WealthIndex(tmp)
    close <- data.frame(TradingDay=zoo::index(tmp),close=zoo::coredata(tmp)*init_data$ClosePrice,row.names =NULL)
    colnames(close) <- c('TradingDay','ClosePrice')
    index <- merge(index,close,by='TradingDay')
    index <- transform(index,TradingDay=rdate2int(TradingDay),
                       InnerCode=c(1000985),
                       PrevClosePrice=c(NA,index$ClosePrice[-(nrow(index))]),
                       OpenPrice=c(NA),
                       HighPrice=c(NA),
                       LowPrice=c(NA),
                       TurnoverVolume=c(NA),
                       TurnoverValue=c(NA),
                       TurnoverDeals=c(NA),
                       ChangePCT=DailyReturn*100,
                       NegotiableMV=c(NA),
                       UpdateTime=c(Sys.Date()),
                       ID=c('EI000985E'))
    index <- index[,c("InnerCode","TradingDay","PrevClosePrice","OpenPrice","HighPrice",
                      "LowPrice","ClosePrice","TurnoverVolume","TurnoverValue","TurnoverDeals",
                      "ChangePCT","NegotiableMV","UpdateTime","DailyReturn","ID")]
    index$PrevClosePrice[1] <- init_data$ClosePrice
    dbExecute(con,paste("delete from QT_IndexQuote where ID='EI000985E' and TradingDay >=",rdate2int(begT),"and TradingDay <=",rdate2int(endT)))
    dbWriteTable(con,'QT_IndexQuote',index,overwrite=FALSE,append=TRUE,row.names=FALSE)
  }
  
  dbDisconnect(con_qt)
  dbDisconnect(con)
  return('Done!')
  
}









#' lcdb.update.QT_FreeShares
#'
#' update QT_FreeShares through Wind API.
#' @examples
#' lcdb.update.QT_FreeShares()
#' @export
lcdb.update.QT_FreeShares <-  function(begT,endT,Freq='week') {
  con <- db.local("main")
  re <- dbReadTable(con,'QT_FreeShares')
  
  if(missing(begT)){
    begT <- intdate2r(max(re$date))
    begT <- rdate2int(trday.nearby(begT,1))
  }
  if(missing(endT)){
    endT <- rdate2int(trday.nearest(Sys.Date()-1))
  }
  
  if(begT<endT){
    dates <- getRebDates(intdate2r(begT),intdate2r(endT),rebFreq = Freq)
    require(WindR)
    WindR::w.start(showmenu = FALSE)
    for(i in 1:length(dates)){
      TS <- w.wset('sectorconstituent',date=dates[i],sectorid='a001010100000000')[[2]]
      float_shares_ <- WindR::w.wss(TS$wind_code,'free_float_shares',tradeDate=dates[i])[[2]]
      float_shares_ <- cbind(data.frame(date=dates[i]),float_shares_)
      if(i==1){
        float_shares <- float_shares_
      }else{
        float_shares <- rbind(float_shares,float_shares_)
      }
    }
    colnames(float_shares) <- c("date","stockID","freeShares")
    float_shares <- transform(float_shares,
                              date=rdate2int(date),
                              stockID=stringr::str_c("EQ",substr(stockID,1,6)),
                              freeShares=freeShares/1e8)
    float_shares <- rbind(float_shares,re[re$date>=begT & re$date<=endT,])
    float_shares <- float_shares %>% group_by(stockID,freeShares) %>% summarise(date=min(date)) %>% dplyr::ungroup()
    float_shares <- float_shares[,c("date","stockID","freeShares")]
    
    re_ <- re %>% dplyr::filter(date<begT | date>endT) %>% arrange(stockID,desc(date)) %>% group_by(stockID) %>% slice(1) %>% dplyr::ungroup()
    re_ <- dplyr::rename(re_,dateold=date,freeSharesold=freeShares)
    float_shares <- dplyr::left_join(float_shares,re_,by='stockID')
    float_shares <- rbind(float_shares %>% dplyr::filter(!is.na(freeSharesold)) %>% dplyr::filter(date!=dateold & freeShares!=freeSharesold),
                          float_shares %>% dplyr::filter(is.na(freeSharesold)))
    float_shares <- float_shares[,c("date","stockID","freeShares")]
    float_shares <- arrange(float_shares,date,stockID)
    dbExecute(con,paste("delete from QT_FreeShares where date >=",begT,"and date<=",endT))
    dbWriteTable(con,'QT_FreeShares',float_shares,overwrite=FALSE,append=TRUE,row.names=FALSE)
    
  }
  dbDisconnect(con)
}






# --------------------  ~~ QT_sus_res ----------------
#' lcdb.update.QT_sus_res
#' 
#' @export
#' @rdname  lcdb.update.QT_sus_res
#' @examples 
#' #-- initiate:
#' lcdb.init.QT_sus_res(19901231,19950630)
#' #-- update:
#' dates <- c(seq(as.Date("1998-12-31"),to = Sys.Date(),by = "year"),Sys.Date())
#' dates <- rdate2int(dates)
#' for(date in dates){
#'   message(paste("updating to ",date,"..."))
#'   lcdb.update.QT_sus_res(endT=date)
#' }
#' #-- fix the bugs
#' bugs <- lcdb.update.QT_sus_res_bugsFinding()
#' lcdb.update.QT_sus_res(stockID=bugs)
lcdb.init.QT_sus_res <- function(begT=19901231,endT=99990101){
  con <- db.local("qt")
  
  if(dbExistsTable(con,"QT_sus_res")){dbRemoveTable(con,"QT_sus_res")}
  message("lcdb.init QT_sus_res ... ");
  dbExecute(con,'CREATE TABLE QT_sus_res 
            ( "stockID" TEXT,
            "sus" INTEGER,
            "res" INTEGER,
            "updateDate" INTEGER 
            );')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_sus_res] ON [QT_sus_res] ([stockID], [sus]);')
  
  begT_filt <- paste("TradingDay >=",begT)
  endT_filt <- paste("TradingDay <= ",endT)
  updateDate <- dbGetQuery(con,paste("select max(TradingDay) from QT_DailyQuote where",endT_filt))[[1]]
  
  loops <- dbGetQuery(con,"select distinct ID from QT_DailyQuote")[[1]]
  # loops <- "EQ603520"
  
  TB_sus_res <- data.frame()
  for (ii in 1:length(loops)){
    # ii <- 1
    stockID_ <- loops[ii]
    message(paste(stockID_," "),appendLF = FALSE)
    QTstock <- dbGetQuery(con,paste("select ID, TradingDay, TurnoverVolume from QT_DailyQuote where ID=",QT(stockID_),"and ",begT_filt,"and",endT_filt, "order by TradingDay"))
    QTstock <- dplyr::mutate(QTstock,Vol_lag=lag(TurnoverVolume))
    QTstock <- dplyr::mutate(QTstock,sus_res=ifelse(TurnoverVolume<1 & Vol_lag>=1, "s",  # - suspend
                                                    ifelse(Vol_lag<1 & TurnoverVolume>=1, "r", # - resumption 
                                                           NA))) # - nothing
    sus <- dplyr::filter(QTstock,sus_res=="s")$TradingDay
    res <- dplyr::filter(QTstock,sus_res=="r")$TradingDay
    if(length(sus)+length(res) > 0){
      if(length(res)>0 & sus[1] > res[1]){
        res_lag <- res[1]  # -- res_lag
        res <- res[-1]
      } 
      if(length(sus)>length(res)){
        res <- c(res,NA)
      }
      S_R <- data.frame(stockID=stockID_,sus=sus,res=res,updateDate=updateDate)
      TB_sus_res <- rbind(TB_sus_res,S_R)
    }
  }
  
  dbWriteTable(con,"QT_sus_res",TB_sus_res,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}

#' @export
#' @rdname  lcdb.update.QT_sus_res
lcdb.update.QT_sus_res <- function(endT,stockID){
  con <- db.local("qt")
  if(missing(stockID)){
    begT <- dbGetQuery(con,"select max(updateDate) from QT_sus_res")[[1]]
  }else{
    begT <- 19901231
  }
  if(missing(endT)){
    if(missing(stockID)){
      endT <- 99990101
    } else {
      endT <- dbGetQuery(con,"select max(updateDate) from QT_sus_res")[[1]]
    }
  }
  begT_filt <- paste("TradingDay >=",begT)
  endT_filt <- paste("TradingDay <= ",endT)
  if(missing(stockID)){
    pool_filt <- "1>0"
  } else{
    pool_filt <- paste("ID in",brkQT(stockID))
  }
  
  if(endT<=begT){
    stop("Can not update this table in the midst!")
  }
  QTdata <- dbGetQuery(con,paste("select ID, TradingDay, TurnoverVolume from QT_DailyQuote where ",begT_filt,"and",endT_filt,"and",pool_filt, "order by ID,TradingDay"))
  if(dim(QTdata)[1]<1){
    return()
  }
  
  updateDate <- max(QTdata$TradingDay)
  
  loops <- unique(QTdata$ID)
  TB_sus_res <- data.frame()
  for (ii in 1:length(loops)){
    # ii <- 1
    stockID_ <- loops[ii]
    # message(stockID_) # --
    QTstock <- dplyr::filter(QTdata,ID==stockID_)
    QTstock <- dplyr::mutate(QTstock,Vol_lag=lag(TurnoverVolume)) # this step will trim the QTdata of begT just right to avoid the overlapping
    QTstock <- dplyr::mutate(QTstock,sus_res=ifelse(TurnoverVolume<1 & Vol_lag>=1, "s",  # - suspend
                                                    ifelse(Vol_lag<1 & TurnoverVolume>=1, "r", # - resumption 
                                                           NA))) # - nothing
    sus <- dplyr::filter(QTstock,sus_res=="s")$TradingDay
    res <- dplyr::filter(QTstock,sus_res=="r")$TradingDay
    
    if(length(sus)==0 & length(res) == 0){
      next
    } else if(length(sus)==0){ # length(sus)==0 & length(res)==1
      dbExecute(con,paste("UPDATE QT_sus_res 
                           SET res = ",res,
                           "WHERE stockID=",QT(stockID_),
                           "and sus=(select max(sus) from QT_sus_res where stockID=",QT(stockID_),")"))
    } else if(length(res)==0) {# length(sus)==1 & length(res)==0
      res <- NA
      S_R <- data.frame(stockID=stockID_,sus=sus,res=res,updateDate=updateDate)
      TB_sus_res <- rbind(TB_sus_res,S_R)
    } else {
      if(sus[1] > res[1]){
        res_lag <- res[1]  # -- res_lag
        dbExecute(con,paste("UPDATE QT_sus_res 
                              SET res = ",res_lag,
                             "WHERE stockID=",QT(stockID_),
                             "and sus=(select max(sus) from QT_sus_res where stockID=",QT(stockID_),")"))
        res <- res[-1]
      } 
      if(length(sus)>length(res)){
        res <- c(res,NA)
      }
      S_R <- data.frame(stockID=stockID_,sus=sus,res=res,updateDate=updateDate)
      TB_sus_res <- rbind(TB_sus_res,S_R)
    }
    
  }
  
  if(!missing(stockID)){
    dbExecute(con,paste("delete from QT_sus_res where stockID in",brkQT(loops)))
  }
  
  dbWriteTable(con,"QT_sus_res",TB_sus_res,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}

#' @export
#' @rdname  lcdb.update.QT_sus_res
lcdb.update.QT_sus_res_bugsFinding <- function(){
  con <- db.local("qt")
  bugsdata <- dbGetQuery(con,"select * from (select * from QT_sus_res where res is null) a ,
                         (select stockID as 'ID', max(sus) as 'sus_max' from QT_sus_res group by stockID) m
                         where a.stockID=m.ID")
  bugs <- dplyr::filter(bugsdata,sus!=sus_max)$stockID
  dbDisconnect(con)
  return(bugs)
}

# --------------------  ~~ QT_Size ----------------
#' @export
lcdb.init.QT_Size <- function(){
  con <- db.local("qt")
  if(dbExistsTable(con,"QT_Size")){
    dbRemoveTable(con,"QT_Size")
  }
  message("lcdb.init QT_Size ... ");
  dbExecute(con,'CREATE TABLE "QT_Size" (
            "date" INTEGER,
            "stockID" TEXT,
            "mkt_cap" REAL,
            "float_cap" REAL,
            "free_cap" REAL
  );')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_size] ON [QT_Size] ([date], [stockID]);')
  dbDisconnect(con)
}

#' @export
lcdb.update.QT_Size <- function(begT, endT){
  
  # RebDates
  if(missing(begT)){
    begT <- queryAndClose.dbi(db.local("qt"),"select max(date) from QT_Size")[[1]]
    if(is.na(begT)){ # EMPTY TABLE
      begT <- queryAndClose.dbi(db.local("qt"),"select min(TradingDay) from QT_DailyQuote")[[1]]
    }
  }
  
  if(missing(endT)){
    endT <- queryAndClose.dbi(db.local("qt"),"select max(TradingDay) from QT_DailyQuote")[[1]]
  }
  begT <- intdate2r(begT)
  endT <- intdate2r(endT)
  if(begT >= endT){
    return("Done.")
  }else{
    # checking the connection of the date sequence
    rebdates <- getRebDates(begT, endT, rebFreq = "week")
    rebdates_index <- cut.Date2(rebdates, breaks = "week")
    if(rebdates_index[1] == rebdates_index[2]){
      rebdates <- rebdates[-1]
    }
    
    # get TS (A shares)
    # divide into groups
    yearlist <- lubridate::year(rebdates)
    yearlist_unique <- unique(yearlist)
    for(i in 1:length(yearlist_unique)){
      year_ <- yearlist_unique[i]
      message(year_)
      rebdates_ <- rebdates[yearlist == year_]
      rebdates_qr <- paste0("(",paste(rdate2int(rebdates_), collapse = ","),")")
      ts_ <- queryAndClose.dbi(db.local("qt"),
                               paste0("SELECT TradingDay, ID from QT_DailyQuote
                                      WHERE TradingDay in ", rebdates_qr))
      if(i == 1L){
        ts <- ts_
      }else{
        ts <- rbind(ts, ts_)
      }
    }
    colnames(ts) <- c("date","stockID")
    ts <- dplyr::arrange(ts, date, stockID)
    ts$date <- intdate2r(ts$date)
    
    # get GF_CAP
    tsf <- gf_cap(ts,log=FALSE,bc_lambda = NULL,
                  var="mkt_cap",na_fill=FALSE,varname="mkt_cap",
                  datasrc="local")
    tsf <- gf_cap(tsf,log=FALSE,bc_lambda = NULL,
                  var="float_cap",na_fill=FALSE,varname="float_cap",
                  datasrc="local")
    tsf <- gf_cap(tsf,log=FALSE,bc_lambda = NULL,
                  var="free_cap",na_fill=FALSE,varname="free_cap",
                  datasrc="local")
    
    # output
    tsf$date <- rdate2int(tsf$date)
    
    con <- db.local("qt")
    dbExecute(con,paste("delete from QT_Size where date >=", rdate2int(min(rebdates)),"and  date <=", rdate2int(max(rebdates))))
    dbWriteTable(con,'QT_Size',tsf,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbDisconnect(con)
    return("Done.")
  }
}

# --------------------  ~~ FactorScore ----------------

#' lcdb.update.QT_FactorScore
#' 
#' update \bold{all} factorscores in table CT_FactorLists (see \code{CT_FactorLists()}).
#' 
#' @param begT the begin date of the updating
#' @param endT the end date of the updating
#' @param stockID a vector of stockID
#' @export
#' @seealso \code{\link{lcfs.update}}
#' @examples
#' # update factors on certain time
#' lcdb.update.QT_FactorScore(20130322,20130330)
#' # update factors of certain stocks
#' lcdb.update.QT_FactorScore(20130322,20130330,c("EQ000001","EQ000002")) 
#' # update factors on certin time, of certain stocks
#' lcdb.update.QT_FactorScore(20130322,20130330,c("EQ000001","EQ000002"))
lcdb.update.QT_FactorScore <- function(begT,endT,stockID,loopFreq="month",type = c("alpha","risk")){
  
  type <- match.arg(type)
  if(type == "alpha"){
    con_fs <- db.local("fs")
    tableName_char <- "QT_FactorScore"
  }else if(type == "risk"){
    con_fs <- db.local("fs_r")
    tableName_char <- "QT_FactorScore_R"
  }
  
  con_qt <- db.local("qt")
  
  if(TRUE){
    if(missing(begT)){
      if(missing(stockID)){
        begT <- dbGetQuery(con_fs,paste("select max(TradingDay) from",tableName_char))[[1]]
      } else {
        begT <- dbGetQuery(con_fs,paste("select min(TradingDay) from",tableName_char))[[1]]
      }
    }
    if(missing(endT)){
      if(missing(stockID)){
        endT <- 99990101
      } else {
        endT <- dbGetQuery(con_fs,paste("select max(TradingDay) from",tableName_char))[[1]]
      }
    }
    if(missing(stockID)){
      pool_filt <- "1>0"
    } else {
      pool_filt <- paste("ID in",brkQT(stockID))
    }
  }
  
  endT <- min(intdate2r(endT), Sys.Date())
  dates <- c(seq(intdate2r(begT), endT ,by = loopFreq), endT)
  dates <- rdate2int(dates)
  for(ii in 1:(length(dates)-1)){
    message(paste("lcdb.update.",tableName_char,": updating to ",dates[ii+1],"..."))
    begT_filt <- paste("TradingDay >=",dates[ii])
    endT_filt <- paste("TradingDay < ",dates[ii+1])
    TS <- dbGetQuery(con_qt, paste("select TradingDay as date, ID as stockID from QT_DailyQuote where ",begT_filt,"and",endT_filt,"and",pool_filt))
    if(NROW(TS)==0) {
      return()
    }
    TS <- transform(TS,date=intdate2r(date))
    TS <- dplyr::arrange(TS,date,stockID)
    
    require(QFactorGet)
    factorLists <- CT_FactorLists(type = type)
    for(i in 1:NROW(factorLists)){
      factorName <- factorLists[i,"factorName"]
      factorID <- factorLists[i,"factorID"]
      factorFun <- factorLists[i,"factorFun"]
      factorPar <- factorLists[i,"factorPar"]
      message("Factor",factorName,"getting ...")
      subTSF <- getRawFactor(TS=TS,factorFun=factorFun,factorPar=factorPar)
      subTSF <- renameCol(subTSF,src="factorscore",tgt=factorID)
      if(i==1L){
        re <- subTSF[,c("date","stockID",factorID)]
      } else {
        re <- merge(re,subTSF[,c("date","stockID",factorID)],by=c("date","stockID"))
      }
    }
    re <- renameCol(re,c("date","stockID"),c("TradingDay","ID"))
    re$TradingDay <- rdate2int(re$TradingDay)
    
    if(TRUE){ # add extra fields, and reorder the fields to fix the order of target table. 
      targetfield <- dbListFields(con_fs,tableName_char)
      extrfield <- setdiff(targetfield,names(re))
      extrdat <- as.data.frame(matrix(NA,NROW(re),length(extrfield)))
      names(extrdat) <- extrfield
      re <- cbind(re,extrdat)
      re <- re[targetfield]
    }
    
    dbExecute(con_fs,paste("delete from",tableName_char,"where",begT_filt,"and",endT_filt,"and",pool_filt))
    dbWriteTable(con_fs,name = tableName_char,value = re,overwrite=FALSE,append=TRUE,row.names=FALSE)
    gc()
  }
  
  dbDisconnect(con_fs)
  dbDisconnect(con_qt)
}









#' lcfs.update
#' 
#' update \bold{one} specific factorscore.
#' 
#' @param factorID a single charactor of factorID
#' @param begT the begin date of the updating
#' @param endT the end date of the updating
#' @param stockID a vector of stockID
#' @export
#' @seealso \code{\link{lcdb.update.QT_FactorScore}}, \code{\link{lcfs.add}}
#' @examples
#' # update a factorscore on all the time, of all the stocks
#' lcfs.update("F000008")
#' # update a factor on certain time
#' lcfs.update("F000008",20130322,20130330)
#' # update a factor of certain stocks
#' lcfs.update("F000008",20130322,20130330,c("EQ000001","EQ000002")) 
#' # update a factorscore on certin time, of certain stocks
#' lcfs.update("F000008",20130322,20130330,c("EQ000001","EQ000002"))
lcfs.update <- function(factorID,begT,endT,stockID,
                        splitNbin="month"){
  if(substr(factorID,1,1) == "F"){
    con <- db.local("fs")
    tableName_char <- "QT_FactorScore"
  }else if(substr(factorID,1,1) == "R"){
    con <- db.local("fs_r")
    tableName_char <- "QT_FactorScore_R"
  }
  
  if(missing(begT)){
    begT <- dbGetQuery(con,paste("select min(TradingDay) from", tableName_char))[[1]]
  }
  if(missing(endT)){
    endT <- dbGetQuery(con,paste("select max(TradingDay) from", tableName_char))[[1]]
  }
  
  if(missing(stockID)){
    pool_filt <- "1>0"
  } else{
    pool_filt <- paste("ID in",brkQT(stockID))
  }
  
  factorFun <- CT_FactorLists(factorID = factorID)$factorFun
  factorPar <- CT_FactorLists(factorID = factorID)$factorPar
  
  loopT <- rdate2int(trday.get(intdate2r(begT),intdate2r(endT)))
  loopT.L <- split(loopT,cut(intdate2r(loopT),splitNbin))
  
  subfun <- function(Ti){
    message(paste(" ",min(Ti),"to",max(Ti)," ..."))
    dates <- paste(Ti,collapse=",")
    TS <- dbGetQuery(con,paste("select TradingDay as date, ID as stockID from",tableName_char,"where TradingDay in (",dates,") and",pool_filt))
    TS$date <- intdate2r(TS$date)    
    TSF <- getRawFactor(TS,factorFun,factorPar)
    TSF$date <- rdate2int(TSF$date)
    TSF <- renameCol(TSF,src="factorscore",tgt=factorID)
    
    for(Tij in Ti){ # update the factorscore day by day.
      #     Tij <- Ti[1]
      # message(paste(" ",Tij))
      dbWriteTable(con,"temp_table",TSF[TSF$date==Tij,],overwrite=TRUE,append=FALSE,row.names=FALSE)
      if(tableName_char == "QT_FactorScore"){
        qr <- paste("UPDATE QT_FactorScore
                  SET ",factorID,"= (SELECT ",factorID," FROM temp_table WHERE temp_table.stockID =QT_FactorScore.ID) 
                    WHERE QT_FactorScore.ID = (SELECT stockID FROM temp_table WHERE temp_table.stockID =QT_FactorScore.ID)
                    and QT_FactorScore.TradingDay =",Tij)
      }else if(tableName_char == "QT_FactorScore_R"){
        qr <- paste("UPDATE QT_FactorScore_R
                  SET ",factorID,"= (SELECT ",factorID," FROM temp_table WHERE temp_table.stockID =QT_FactorScore_R.ID) 
                    WHERE QT_FactorScore_R.ID = (SELECT stockID FROM temp_table WHERE temp_table.stockID =QT_FactorScore_R.ID)
                    and QT_FactorScore_R.TradingDay =",Tij)
      }
      res <- dbSendQuery(con,qr)
      dbClearResult(res)  
    }   
    gc()
  }  
  
  message(paste("Function lcfs.update: updateing factor score of",factorID,".... "))
  plyr::l_ply(loopT.L, subfun, .progress = plyr::progress_text(style=3))   
  dbDisconnect(con)
}



#' lcfs.add
#' 
#' add/update \bold{one} factorscore column in local sqlite table \code{"QT_FactorScore"}. On the same time, correspondingly, add/update a record into table \code{"CT_FactorLists"} and table \code{"CT_TechVars"}.
#' 
#' @param factorFun a character string naming the function to get the factor scores
#' @param factorPar a character string, containing the parameters of the \code{factorFun}. Note that unlike in \code{\link{getTSF}}, here the factorPar could not be a list, because it need to be written into database.
#' @param factorDir a integer,should be 1 or -1 (1 for the positive factor,-1 for the negative one). \bold{Note that} the \code{factorDir} here is only used to write a record into table \code{"CT_FactorLists"}, not used when getting \code{TSF}. So that the factorscore in table \code{"QT_FactorScore"} is kept \bold{"raw", without adding the dirrection infomation}.
#' @param factorID a character string
#' @param factorName a character string. IF missing, then take a default name by function \code{default.factorName}. 
#' @param factorType a character string
#' @param factorDesc a character string
#' @param splitNbin a character of interval specification(see \code{\link{cut.Date}} for detail). Specify the time interval when looping of getting the \code{TSF} object.
#' @return Write data into the local sqlite database, returning NULL.
#' @seealso \code{\link{getTSF}},\code{\link{modelPar.factor}}, \code{\link{lcdb.update.QT_FactorScore}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' system.time(lcfs.add(factorFun="gf.F_rank_chg",factorPar="lag=60,con_type=\"1,2\"", factorDir=1, factorID="F000999"))
lcfs.add <- function(factorFun, 
                     factorPar="", 
                     factorDir,
                     factorID,                      
                     factorName = default.factorName(factorFun,factorPar,factorDir),                      
                     factorType = "", 
                     factorDesc = "",
                     splitNbin = "month"){
  #
  if(substr(factorID,1,1) == "F"){
    con_fs <- db.local("fs")
    tableName_char <- "QT_FactorScore"
  }else if(substr(factorID,1,1) == "R"){
    con_fs <- db.local("fs_r")
    tableName_char <- "QT_FactorScore_R"
  }
  #
  if(factorID %in% CT_FactorLists()$factorID) {
    is_overwrite <- select.list(choices=c("OK","CANCEL"),preselect="CANCEL",title=paste("Warning!\nThe factor",factorID,"has already exist!\nDo you want to overwrite it?"),graphics=FALSE)
    if(is_overwrite == "CANCEL") return(invisible(NULL))
  } 
  con_main <- db.local("main")
  # insert or replace a row to table 'CT_FactorLists' 
  if(!is.character(factorPar)){
    stop("The 'factorPar' must be a character!")
  }
  qr1 <- paste("replace into CT_FactorLists_R
               (factorID, factorName, factorFun, factorPar, factorDir, factorType, factorDesc )
               values
               (
               ",QT(factorID),",
               ",QT(factorName),",
               ",QT(factorFun),",
               ",QT(factorPar),",
               ",QT(factorDir),",
               ",QT(factorType),",
               ",QT(factorDesc),"
               ) ")
  dbExecute(con_fs,qr1)
  
  # insert or replace a row to table 'CT_TechVars'  
  qr2 <- paste("replace into CT_TechVars
               (datasrc, secuCate, varName, func, tableName)
               values
               (
               'local',
               'EQ',
               ",QT(factorID),",
               ",QT(factorID),",
               'QT_FactorScore'
               ) ")
  dbExecute(con_main,qr2)
  
  # add 1 colume to table 'QT_FactorScore' 
  tryCatch(dbExecute(con_fs,paste("ALTER TABLE",tableName_char,"ADD COLUMN ",factorID,"float(0, 4)")),
           error=function(e) { print("RS-DBI driver: (error in statement: duplicate column name)") })
  
  dbDisconnect(con_fs)
  dbDisconnect(con_main)
  
  # update
  lcfs.update(factorID = factorID,splitNbin = splitNbin)
  
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================
# ===================== Database Initiation  ===========================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================

# --------------------  ~~ main ----------------
#' @rdname lcdb.init
#' @export
lcdb.init.IndexQuote_000985E <- function(begT=20050101){
  con <- db.local("main")
  dbExecute(con,"delete from QT_IndexQuote where ID='EI000985E'")
  
  endT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
  endT <- intdate2r(endT)
  begT <- intdate2r(begT)
  dates <- trday.get(begT,endT)
  dates <- as.Date(unique(cut.Date2(dates,"month")))
  TS <- getIndexComp(indexID = 'EI000985',endT = dates, drop = FALSE)
  
  con_qt <- db.local("qt")
  index <- data.frame()
  for(i in 1:(length(dates)-1)){
    # tmp.dates <- getRebDates(dates[i],dates[i+1],rebFreq = 'day')
    tmp.dates <- trday.get(dates[i],dates[i+1])
    tmp.dates <- tmp.dates[-length(tmp.dates)]
    message(rdate2int(max(tmp.dates)))
    qr <- paste("select TradingDay,ID,DailyReturn from QT_DailyQuote
                  where TradingDay>=",rdate2int(min(tmp.dates))," and TradingDay<=",rdate2int(max(tmp.dates)))
    quotedf <- dbGetQuery(con_qt,qr)
    quotedf$TradingDay <- intdate2r(quotedf$TradingDay)
    tmp.TS <- TS[TS$date==dates[i],]
    quotedf <- quotedf[quotedf$ID %in% tmp.TS$stockID,]
    
    tmp <- quotedf %>% dplyr::group_by(TradingDay) %>%
      dplyr::summarise(DailyReturn = mean(DailyReturn, na.rm = TRUE))
    index <- rbind(index,tmp)
  }
  
  tmp <- xts::xts(index$DailyReturn,order.by = index$TradingDay)
  tmp <- WealthIndex(tmp)
  close <- data.frame(TradingDay=zoo::index(tmp),close=zoo::coredata(tmp)*1000,row.names =NULL)
  colnames(close) <- c('TradingDay','ClosePrice')
  index <- merge(index,close,by='TradingDay')
  index <- transform(index,TradingDay=rdate2int(TradingDay),
                     InnerCode=c(1000985),
                     PrevClosePrice=c(NA,index$ClosePrice[-(nrow(index))]),
                     OpenPrice=c(NA),
                     HighPrice=c(NA),
                     LowPrice=c(NA),
                     TurnoverVolume=c(NA),
                     TurnoverValue=c(NA),
                     TurnoverDeals=c(NA),
                     ChangePCT=DailyReturn*100,
                     NegotiableMV=c(NA),
                     UpdateTime=c(Sys.Date()),
                     ID=c('EI000985E'))
  index <- index[,c("InnerCode","TradingDay","PrevClosePrice","OpenPrice","HighPrice",
                    "LowPrice","ClosePrice","TurnoverVolume","TurnoverValue","TurnoverDeals",
                    "ChangePCT","NegotiableMV","UpdateTime","DailyReturn","ID")]
  dbWriteTable(con,'QT_IndexQuote',index,overwrite=FALSE,append=TRUE,row.names=FALSE)
  
  dbDisconnect(con_qt)
  dbDisconnect(con)
}



#' @rdname lcdb.init
#' @export
lcdb.init.IndexQuote_000985 <- function(begT=19900101){
  con <- db.local("main")
  windCode='000985.CSI'
  indexCode <- paste('EI',substr(windCode,1,6),sep = '')
  dbExecute(con, paste("delete from QT_IndexQuote where ID=",QT(indexCode)))
  
  endT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
  endT <- intdate2r(endT)
  begT <- intdate2r(begT)
  require(WindR)
  WindR::w.start(showmenu = FALSE)
  index <- w.wsd(windCode,"pre_close,open,high,low,close,volume,amt,dealnum,pct_chg",begT,endT)[[2]]
  colnames(index) <- c("TradingDay","PrevClosePrice","OpenPrice","HighPrice", "LowPrice",
                       "ClosePrice","TurnoverVolume","TurnoverValue","TurnoverDeals","ChangePCT")
  
  #get innercode
  qr <- paste("SELECT InnerCode FROM SecuMain
                where SecuCode=",QT(substr(windCode,1,6))," and SecuCategory=4")
  innercode <- queryAndClose.odbc(db.jy(),qr)[[1]]
  
  index <- transform(index,TradingDay=rdate2int(TradingDay),
                     InnerCode=c(innercode),
                     DailyReturn=ChangePCT/100,
                     NegotiableMV=c(NA),
                     UpdateTime=c(Sys.Date()),
                     ID=c(indexCode))
  index <- index[,c("InnerCode","TradingDay","PrevClosePrice","OpenPrice","HighPrice",
                    "LowPrice","ClosePrice","TurnoverVolume","TurnoverValue","TurnoverDeals",
                    "ChangePCT","NegotiableMV","UpdateTime","DailyReturn","ID")]
  dbWriteTable(con,'QT_IndexQuote',index,overwrite=FALSE,append=TRUE,row.names=FALSE)
  
  dbDisconnect(con)
}


#' @export
#' @rdname lcdb.init
lcdb.init.QT_FreeShares <- function(filename="D:/sqlitedb/QT_FreeShares.csv"){
  re <- read.csv(filename,stringsAsFactors = FALSE)
  con <- db.local("main")
  if(dbExistsTable(con,"QT_FreeShares")){dbRemoveTable(con,"QT_FreeShares")}
  message("lcdb.init QT_FreeShares ... ");
  dbExecute(con,'CREATE TABLE "QT_FreeShares" (
            "date" INTEGER,
            "stockID" TEXT,
            "freeShares" REAL
  );')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_Freeshares] ON [QT_FreeShares] ([date], [stockID]);')
  dbWriteTable(con,'QT_FreeShares',re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @export
#' @rdname lcdb.init
lcdb.init.CT_TechVars <- function(filename="D:/sqlitedb/CT_TechVars.csv"){
  re <- read.csv(filename,stringsAsFactors = FALSE)
  con <- db.local("main")
  if(dbExistsTable(con,"CT_TechVars")){dbRemoveTable(con,"CT_TechVars")}
  message("lcdb.init CT_TechVars ... ");
  dbExecute(con,'CREATE TABLE [CT_TechVars] (
  [datasrc] TEXT, 
            [secuCate] TEXT, 
            [varName] TEXT, 
            [func] TEXT, 
            [tableName] TEXT);')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_CT_TechVars] ON [CT_TechVars] ([datasrc], [tableName], [secuCate], [varName]);')
  dbWriteTable(con,'CT_TechVars',re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}

#' lcdb.init
#' 
#' initialize all the tables in sqlitdb
#' @rdname lcdb.init
#' @examples 
#' # A correct throughout process of initialize the whole local database:
#' # 1. expert 3 csv tables:
#' lcdb.export2csv("main","CT_TechVars")
#' lcdb.export2csv("main","QT_FreeShares")
#' lcdb.export2csv("fs","CT_FactorLists")
#' # 2. build 3 empty sqlite files: qt.db, fs.db, main.db
#' # 3. initialize 3 database files, in proper order:
#' lcdb.init_qt()
#' lcdb.init_main()
#' lcdb.init_fs()
#' @export
lcdb.init_main <- function(begT=19900101,endT=99990101){
  
  con <- db.local("main")
  
  if(dbExistsTable(con,"SecuMain")){dbRemoveTable(con,"SecuMain")}
  message("lcdb.init SecuMain ... ");
  dbExecute(con,'CREATE TABLE "SecuMain" (
  "ID" TEXT,
  "InnerCode" INTEGER,
  "CompanyCode" INTEGER,
  "SecuCode" TEXT,
  "SecuAbbr" TEXT,
  "SecuMarket" INTEGER,
  "ListedSector" INTEGER,
  "ListedState" INTEGER,
  "UpdateTime" REAL,
  "StockID_TS" TEXT,
  "SecuCategory" INTEGER,
  "ListedDate" INTEGER,
  "StockID_wind" TEXT
  ); ')
  dbExecute(con,'CREATE UNIQUE INDEX IX_SecuMain ON SecuMain (ID);')
  lcdb.update.SecuMain()
  
  
  if(dbExistsTable(con,"QT_TradingDay")){dbRemoveTable(con,"QT_TradingDay")}
  message("lcdb.init QT_TradingDay ... ");
  dbExecute(con,'CREATE TABLE "QT_TradingDay" (
    "TradingDate" INTEGER,
    "IfTradingDay" INTEGER,
    "SecuMarket" INTEGER,
    "IfWeekEnd" INTEGER,
    "IfMonthEnd" INTEGER,
    "IfQuarterEnd" INTEGER,
    "IfYearEnd" INTEGER
  );')
  dbExecute(con,'CREATE UNIQUE INDEX IX_QT_TradingDay ON QT_TradingDay (TradingDate, SecuMarket);')
  lcdb.update.QT_TradingDay()
  
  
  if(dbExistsTable(con,"CT_IndustryList")){dbRemoveTable(con,"CT_IndustryList")}
  message("lcdb.init CT_IndustryList ... ");
  dbExecute(con,'CREATE TABLE "CT_IndustryList" (
  "Standard" INTEGER,
             "Level" INTEGER,
             "IndustryID" TEXT,
             "IndustryName" TEXT,
             "Alias" INTEGER,
             "Code1" TEXT,
             "Name1" TEXT,
             "Code2" TEXT,
             "Name2" TEXT,
             "Code3" TEXT,
             "Name3" TEXT,
             "UpdateTime" REAL
  ); ')
  dbExecute(con,'CREATE UNIQUE INDEX IX_CT_IndustryList ON CT_IndustryList (Standard, IndustryID);')
  lcdb.update.CT_IndustryList()
  
  
  if(dbExistsTable(con,"CT_SystemConst")){dbRemoveTable(con,"CT_SystemConst")}
  message("lcdb.init CT_SystemConst ... ");
  dbExecute(con,'CREATE TABLE "CT_SystemConst" (
  "LB" INTEGER,
             "LBMC" TEXT,
             "DM" INTEGER,
             "MS" TEXT
  );')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_CT_SystemConst] ON [CT_SystemConst] ([LB], [DM]);')
  lcdb.update.CT_SystemConst()
  
  
  lcdb.init.CT_TechVars()
  
  
  if(dbExistsTable(con,"LC_ExgIndustry")){dbRemoveTable(con,"LC_ExgIndustry")}
  message("lcdb.init LC_ExgIndustry ... ");
  dbExecute(con,'CREATE TABLE "LC_ExgIndustry" (
  "stockID" TEXT,
             "CompanyCode" INTEGER,
             "Code1" TEXT,
             "Name1" TEXT,
             "Code2" TEXT,
             "Name2" TEXT,
             "Code3" TEXT,
             "Name3" TEXT,
             "InDate" INTEGER,
             "OutDate" INTEGER,
             "InfoSource" TEXT,
             "Standard" INTEGER,
             "Industry" INTEGER,
             "Flag" INTEGER,
             "UpdateTime" REAL,
             "Code99" TEXT,
             "Name99" TEXT,
             "Code98" TEXT,
             "Name98" TEXT
  );')
  dbExecute(con,'CREATE UNIQUE INDEX IX_LC_ExgIndustry ON LC_ExgIndustry (Standard, stockID, InDate);')
  lcdb.update.LC_ExgIndustry()
  lcdb.fix.swindustry()
  lcdb.fix.ezindustry()
  
  
  if(dbExistsTable(con,"LC_IndexComponent")){dbRemoveTable(con,"LC_IndexComponent")}
  message("lcdb.init LC_IndexComponent ... ");
  dbExecute(con,'CREATE TABLE "LC_IndexComponent" (
  "IndexID" TEXT,
             "SecuID" TEXT,
             "InDate" INTEGER,
             "OutDate" INTEGER,
             "Flag" INTEGER,
             "UpdateTime" REAL
  );')
  dbExecute(con,'CREATE UNIQUE INDEX IX_LC_IndexComponent ON LC_IndexComponent (IndexID, InDate,SecuID);')
  lcdb.update.LC_IndexComponent()
  lcdb.add.LC_IndexComponent("EI000985")
  
  
  if(dbExistsTable(con,"LC_IndexComponentsWeight")){dbRemoveTable(con,"LC_IndexComponentsWeight")}
  message("lcdb.init LC_IndexComponentsWeight ... ");
  dbExecute(con,'CREATE TABLE LC_IndexComponentsWeight 
  ( IndexID TEXT,
    SecuID TEXT,
    EndDate INTEGER,
    Weight REAL,
    UpdateTime REAL 
  );')
  dbExecute(con,'CREATE UNIQUE INDEX IX_LC_IndexComponentsWeight ON LC_IndexComponentsWeight ([IndexID], [EndDate], [SecuID]);')
  lcdb.update.LC_IndexComponentsWeight(begT = begT, endT = endT)
  
  
  if(dbExistsTable(con,"LC_PerformanceGrowth")){dbRemoveTable(con,"LC_PerformanceGrowth")}
  message("lcdb.init LC_PerformanceGrowth ... ");
  dbExecute(con,'CREATE TABLE [LC_PerformanceGrowth] (
            [stockID] varchar(10) NOT NULL, 
            [CompanyCode] int NOT NULL, 
            [InfoPublDate] int NOT NULL, 
            [EndDate] int NOT NULL, 
            [PeriodMark] int NOT NULL, 
            [src] varchar(10) NOT NULL, 
            [NP] money, 
            [NP_LYCP] money, 
            [NP_YOY] decimal(18, 6), 
            [OperatingRevenue] money, 
            [OR_LYCP] money, 
            [OR_YOY] decimal(18, 6), 
            [ForcastType] INT, 
            [UpdateTime] datetime NOT NULL,
            [id] NOT NULL,
            [Mark] INT);')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_LC_PerformanceGrowth] ON [LC_PerformanceGrowth] ([stockID], [PeriodMark], [InfoPublDate], [EndDate], [src], [Mark]);')
  dbExecute(con,'CREATE INDEX [IX_LC_PerformanceGrowth2] ON [LC_PerformanceGrowth]  ([id]);')
  lcdb.update.LC_PerformanceGrowth(begT = begT, endT = endT)
  
  
  
  if(dbExistsTable(con,"LC_RptDate")){dbRemoveTable(con,"LC_RptDate")}
  message("lcdb.init LC_RptDate ... ");
  dbExecute(con,'CREATE TABLE LC_RptDate (
  stockID varchar(10)  NOT NULL,
             CompanyCode int NOT NULL,
             EndDate int NOT NULL,
             PublDate int NOT NULL,
             UpdateTime datetime NOT NULL
  );')
  dbExecute(con,'CREATE UNIQUE INDEX [IX_LC_RptDate] ON [LC_RptDate] ([stockID], [PublDate], [EndDate]);')
  lcdb.update.LC_RptDate(begT = begT, endT = endT)
  
  
  lcdb.init.QT_FreeShares()
  lcdb.update.QT_FreeShares(endT = endT)
  
  
  if(dbExistsTable(con,"QT_IndexQuote")){dbRemoveTable(con,"QT_IndexQuote")}
  message("lcdb.init QT_IndexQuote ... ");
  dbExecute(con,'CREATE TABLE QT_IndexQuote 
  ( InnerCode INTEGER,
  	TradingDay INTEGER,
  	PrevClosePrice REAL,
  	OpenPrice REAL,
  	HighPrice REAL,
  	LowPrice REAL,
  	ClosePrice REAL,
  	TurnoverVolume REAL,
  	TurnoverValue REAL,
  	TurnoverDeals INTEGER,
  	ChangePCT REAL,
  	NegotiableMV REAL,
  	UpdateTime REAL,
  	DailyReturn REAL,
  	ID TEXT 
  );')
  dbExecute(con,'CREATE UNIQUE INDEX IX_QT_IndexQuote ON QT_IndexQuote (ID,TradingDay);')
  lcdb.update.QT_IndexQuote(begT = begT, endT = endT)
  lcdb.init.IndexQuote_000985(begT = begT)
  lcdb.init.IndexQuote_000985E(begT = begT)
  
  
  dbDisconnect(con)
}


# --------------------  ~~ qt ----------------
#' @export
#' @rdname lcdb.init
lcdb.init_qt <- function(){
  lcdb.init.QT_DailyQuote()
  # QT_sus_res
  lcdb.init.QT_sus_res()
  bugs <- lcdb.update.QT_sus_res_bugsFinding()
  if(length(bugs)>0){
    message("\n QT_sus_res_bugsFinding:", bugs)
    lcdb.update.QT_sus_res(stockID=bugs)
  }
  # QT_Size
  lcdb.init.QT_Size()
  lcdb.update.QT_Size()
}



#' @export
#' @rdname lcdb.init
lcdb.init.QT_DailyQuote <- function(begT=19900101,endT=99990101){
  begT_filt <- paste("TradingDay >=",begT)
  endT_filt <- paste("TradingDay < ",endT)
  
  con <- db.local("qt")
  if(dbExistsTable(con,"QT_DailyQuote")){dbRemoveTable(con,"QT_DailyQuote")}
  message("lcdb.init QT_DailyQuote ... ");
  
  dbExecute(con,"
             CREATE TABLE QT_DailyQuote (
             ID varchar(10)  NOT NULL,
             InnerCode int NOT NULL,
             TradingDay int NOT NULL,
             PrevClosePrice smallmoney NULL,
             OpenPrice smallmoney NULL,
             HighPrice smallmoney NULL,
             LowPrice smallmoney NULL,
             ClosePrice smallmoney NULL,
             TurnoverVolume decimal(20, 0) NULL,
             TurnoverValue money NULL,
             DailyReturn float NULL,
             STStatus smallint NULL,
             SecuAbbr varchar(10)  NULL,
             RRFactor float NULL,
             RRClosePrice smallmoney NULL,
             TotalShares decimal(20, 4) NULL,
             NonRestrictedShares decimal(20, 4) NULL
  );
             ")
  dbExecute(con,'CREATE UNIQUE INDEX IX_QT_DailyQuote ON QT_DailyQuote ([TradingDay] ,[InnerCode]);')
  dbExecute(con,'CREATE INDEX [IX_QT_DailyQuote_2] ON [QT_DailyQuote]([TradingDay],[ID]);')
  dbExecute(con,'CREATE INDEX [IX_QT_DailyQuote_3] ON [QT_DailyQuote]([ID] );')
  # dbExecute(con,'CREATE INDEX [IX_QT_DailyQuote_4] on QT_DailyQuote ([InnerCode]);')
  
  all.days <- queryAndClose.odbc(db.quant(),paste("select distinct TradingDay from QT_DailyQuote","where",begT_filt,"and",endT_filt))[[1]]
  all.days <- all.days[order(all.days)]
  subfun <- function(day0){
    message(paste(" ",day0),appendLF = FALSE)
    data0 <- dbGetQuery(con,paste("select * from QT_DailyQuote where TradingDay=",day0))
    dbWriteTable(con, "QT_DailyQuote", data0, overwrite = FALSE, append=TRUE,row.names = FALSE)
    gc()    
  }
  plyr::l_ply(all.days, subfun, .progress = plyr::progress_text(style=3))  
  dbDisconnect(con)
}



# --------------------  ~~ fs ----------------
#' @export
#' @rdname lcdb.init
lcdb.init.CT_FactorLists <- function(filename, type = c("alpha","risk")){
  
  type <- match.arg(type)
  re <- read.csv(filename,stringsAsFactors = FALSE)
  if(type == "alpha"){
    con <- db.local("fs")
    if(dbExistsTable(con,"CT_FactorLists")){dbRemoveTable(con,"CT_FactorLists")}
    message("lcdb.init CT_FactorLists ... ");
    dbExecute(con,'CREATE TABLE [CT_FactorLists] (
              [factorID] TEXT NOT NULL, 
              [factorName] TEXT NOT NULL, 
              [factorFun] TEXT, 
              [factorPar] TEXT, 
              [factorDir] INT DEFAULT 1, 
              [factorType] TEXT, 
              [factorDesc] TEXT);')
    dbExecute(con,'CREATE UNIQUE INDEX [IX_CT_FactorLists] ON [CT_FactorLists] ([factorID]);')
    dbWriteTable(con,'CT_FactorLists',re,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbDisconnect(con)
  }else if(type == "risk"){
    con <- db.local("fs_r")
    if(dbExistsTable(con,"CT_FactorLists_R")){dbRemoveTable(con,"CT_FactorLists_R")}
    message("lcdb.init CT_FactorLists_R ... ");
    dbExecute(con,'CREATE TABLE [CT_FactorLists_R] (
              [factorID] TEXT NOT NULL, 
              [factorName] TEXT NOT NULL, 
              [factorFun] TEXT, 
              [factorPar] TEXT, 
              [factorDir] INT DEFAULT 1, 
              [factorType] TEXT, 
              [factorDesc] TEXT);')
    dbExecute(con,'CREATE UNIQUE INDEX [IX_CT_FactorLists_R] ON [CT_FactorLists_R] ([factorID]);')
    re <- data.frame("factorID" = "R000001",
                     "factorName" = "lncap",
                     "factorFun" = "gr.lncap",
                     "factorPar" = "",
                     "factorDir" = 1L,
                     "factorType" = NA,
                     "factorDesc" = NA)
    dbWriteTable(con,'CT_FactorLists_R',re,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbDisconnect(con)
  }
  return("Done")
}

#' @export
#' @rdname lcdb.init
lcdb.init.QT_FactorScore <- function(begT=20050104,endT=99990101, type = c("alpha","risk")){ 
  type <- match.arg(type)
  if(type == "alpha"){
    con <- db.local("fs")
    if(dbExistsTable(con,"QT_FactorScore")){dbRemoveTable(con,"QT_FactorScore")}
    message("lcdb.init QT_FactorScore ... ");
    IDs <- dbGetQuery(con,"select factorID from CT_FactorLists order by factorID")[[1]]
    IDs <- paste(IDs," float(0, 4)")
    IDs <- paste(IDs,collapse = ",")
    char_createtable <- paste('CREATE TABLE [QT_FactorScore] (
                              [ID] varchar(10) NOT NULL, 
                              [TradingDay] int NOT NULL,',
                              IDs,
                              ')'
    )
    dbExecute(con,char_createtable)
    dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_FactorScore] ON [QT_FactorScore] ([TradingDay] DESC, [ID]);')
    dbExecute(con,'CREATE INDEX [IX_QT_FactorScore_ID] ON [QT_FactorScore] ([ID]);')
    
    lcdb.update.QT_FactorScore(begT,endT,loopFreq = "month",type = "alpha")
    dbDisconnect(con)
  }else if(type == "risk"){
    con <- db.local("fs_r")
    if(dbExistsTable(con,"QT_FactorScore_R")){dbRemoveTable(con,"QT_FactorScore_R")}
    message("lcdb.init QT_FactorScore_R ... ");
    IDs <- dbGetQuery(con,"select factorID from CT_FactorLists_R order by factorID")[[1]]
    if(length(IDs) == 0){
      char_createtable <- paste('CREATE TABLE [QT_FactorScore_R] (
                                [ID] varchar(10) NOT NULL, 
                                [TradingDay] int NOT NULL)')
      dbExecute(con,char_createtable)
      dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_FactorScore_R] ON [QT_FactorScore_R] ([TradingDay] DESC, [ID]);')
      dbExecute(con,'CREATE INDEX [IX_QT_FactorScore_R_ID] ON [QT_FactorScore_R] ([ID]);')
      
    }else{
      IDs <- paste(IDs," float(0, 4)")
      IDs <- paste(IDs,collapse = ",")
      char_createtable <- paste('CREATE TABLE [QT_FactorScore_R] (
                                [ID] varchar(10) NOT NULL, 
                                [TradingDay] int NOT NULL,',
                                IDs,
                                ')'
      )
      dbExecute(con,char_createtable)
      dbExecute(con,'CREATE UNIQUE INDEX [IX_QT_FactorScore_R] ON [QT_FactorScore_R] ([TradingDay] DESC, [ID]);')
      dbExecute(con,'CREATE INDEX [IX_QT_FactorScore_R_ID] ON [QT_FactorScore_R] ([ID]);')
      lcdb.update.QT_FactorScore(begT,endT,loopFreq = "month",type = "risk")
      
    }
    dbDisconnect(con)
  }
  return("Done.")
}

#' @export
#' @rdname lcdb.init
lcdb.init_fs <- function(type=c("alpha","risk")){
  type <- match.arg(type)
  if(type == "alpha"){
    lcdb.init.CT_FactorLists(filename = "D:/sqlitedb/CT_FactorLists.csv", type = "alpha")
    lcdb.init.QT_FactorScore(type = "alpha")
  } else {
    lcdb.init.CT_FactorLists(filename = "D:/sqlitedb/CT_FactorLists_r.csv", type = "risk")
    lcdb.init.QT_FactorScore(type = "risk")
  }
}



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================
# ===================== Table Exporting  ===========================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ======================

#' lcdb.export2csv
#' 
#' @export
#' @examples 
#' lcdb.export2csv("main","CT_TechVars")
#' lcdb.export2csv("main","QT_FreeShares")
#' lcdb.export2csv("fs","CT_FactorLists")
#' lcdb.export2csv("fs_r","CT_FactorLists_r")
lcdb.export2csv <- function(dbname,tablename,path="D:/sqlitedb"){
  con <- db.local(dbname)
  tb <- dbReadTable(con,tablename)
  filename <- file.path(path,paste(tablename,".csv",sep = ""))
  write.csv(tb,filename,row.names = FALSE)
}
