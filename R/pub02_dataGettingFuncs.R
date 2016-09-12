





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
db.local <- function(dbname = "D:/sqlitedb/QTlite.db"){
  driver = DBI::dbDriver("SQLite")
  dbConnect(driver, dbname = dbname)
}
#' @rdname db.connection
#' @export
db.quant <- function(){
  odbcConnect("jyquant", uid = "wsread",pwd = "wsread")
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




#' read.db.odbc
#'
#' read data from a ODBC data source with a query
#' @param db a ODBC database object
#' @param query a character string,indicating the query to execute 
#' @return a dataframe
#' @author Ruifei.Yin
#' @export
#' @examples 
#' read.db.odbc(db.quant(),"select top 10 * from QT_DailyQuote")
read.db.odbc <- function (db, query, as.is=FALSE, ...) {
  table = sqlQuery(db, query, as.is = as.is, ...)
  odbcClose(db)
  return(table) 
}


#' read.db.dbi
#'
#' read data from a DBI data source with a query
#' @param db a DBI data source object
#' @param query a character string,indicating the query to execute 
#' @return a dataframe
#' @author Ruifei.Yin
#' @export
#' @examples 
#' read.db.dbi(db.local(),"select * from QT_DailyQuote limit 10")
read.db.dbi <- function (db, query, ...) {
  table = dbGetQuery(db, query, ...)
  dbDisconnect(db)
  return(table) 
}



#' lcdb.updatetime
#' 
#' get the updatetime of tables in lcdb
#' @return a datafrme, with cols: "table", "updatetime".
#' @export
lcdb.updatetime <- function () {
  con <- db.local()
  updatetime <- c(    
    rdate2int(as.Date(as.POSIXct(dbGetQuery(con,"select max(updateTime) from SecuMain")[[1]],origin = origin_sql()),tz="")),
    rdate2int(as.Date(as.POSIXct(dbGetQuery(con,"select max(updateTime) from CT_IndustryList")[[1]],origin = origin_sql()),tz="")),
    rdate2int(as.Date(as.POSIXct(dbGetQuery(con,"select max(updateTime) from LC_ExgIndustry")[[1]],origin = origin_sql()),tz="")),
    rdate2int(as.Date(as.POSIXct(dbGetQuery(con,"select max(updateTime) from LC_IndexComponent")[[1]],origin =origin_sql()),tz="")),
    dbGetQuery(con,"select max(EndDate) from LC_IndexComponentsWeight")[[1]],
    dbGetQuery(con,"select max(TradingDay) from QT_UnTradingDay")[[1]]      ,
    dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]        ,
    dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote")[[1]]        ,
    dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote2")[[1]]       ,
    dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]       ,
    dbGetQuery(con,"select max(PublDate) from LC_RptDate")[[1]]             ,
    dbGetQuery(con,"select max(InfoPublDate) from LC_PerformanceGrowth")[[1]]             
    )
  table <- c(
    "SecuMain",
    "CT_IndustryList",
    "LC_ExgIndustry",
    "LC_IndexComponent",
    "LC_IndexComponentsWeight",
    "QT_UnTradingDay",
    "QT_IndexQuote",
    "QT_DailyQuote",
    "QT_DailyQuote2",
    "QT_FactorScore",
    "LC_RptDate",
    "LC_PerformanceGrowth"
    )
  dbDisconnect(con)
  return(data.frame(table,updatetime))
}



#' update the local database
#' @return NULL
#' @export
lcdb.update <- function(){
  lcdb.update.SecuMain()                 ;  cat("lcdb.update.SecuMain()... Done \n");
  lcdb.update.QT_TradingDay()            ;  cat("lcdb.update.QT_TradingDay()... Done\n");
  lcdb.update.CT_SystemConst()           ;  cat("lcdb.update.CT_SystemConst()... Done\n");
  lcdb.update.CT_IndustryList()          ;  cat("lcdb.update.CT_IndustryList()... Done\n");
  lcdb.update.LC_ExgIndustry()           ;  cat("lcdb.update.LC_ExgIndustry()... Done\n");
  lcdb.fix.swindustry()                  ;  cat("lcdb.fix.swindustry()... Done\n");
  lcdb.update.LC_IndexComponent()        ;  cat("lcdb.update.LC_IndexComponent()... Done \n");
  lcdb.add.LC_indexComponent("EI000985") ;  cat("lcdb.add.LC_indexComponent()... Done \n");
  lcdb.update.LC_IndexComponentsWeight() ;  cat("lcdb.update.LC_IndexComponentsWeight()... Done\n");
  lcdb.update.QT_UnTradingDay()          ;  cat("lcdb.update.QT_UnTradingDay()... Done \n");
  lcdb.update.QT_IndexQuote()            ;  cat("lcdb.update.QT_IndexQuote()... Done \n");
  lcdb.update.QT_DailyQuote()            ;  cat("lcdb.update.QT_DailyQuote()... Done \n");
  lcdb.update.QT_DailyQuote2()           ;  cat("lcdb.update.QT_DailyQuote2()... Done \n");
  lcdb.update.LC_RptDate()               ;  cat("lcdb.update.LC_RptDate()... Done \n");
  lcdb.update.LC_PerformanceGrowth()     ;  cat("lcdb.update.LC_PerformanceGrowth()... Done \n");
  lcfs.update()                          ;  cat("lcfs.update()... Done \n");
  
}

#' @rdname lcdb.update
#' @export
lcdb.update.SecuMain <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from SecuMain", as.is=4)
  con <- db.local()
  dbWriteTable(con,"SecuMain",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE UNIQUE INDEX IX_SecuMain ON SecuMain (ID)")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.QT_TradingDay <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from QT_TradingDay")
  con <- db.local()
  dbWriteTable(con,"QT_TradingDay",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE UNIQUE  INDEX IX_QT_TradingDay ON QT_TradingDay (TradingDate, SecuMarket)")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.CT_SystemConst <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from CT_SystemConst")
  con <- db.local()
  dbWriteTable(con,"CT_SystemConst",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE UNIQUE INDEX [IX_CT_SystemConst] ON [CT_SystemConst] ([LB], [DM])")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.CT_IndustryList <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from CT_IndustryList")
  con <- db.local()
  dbWriteTable(con,"CT_IndustryList",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE UNIQUE INDEX IX_CT_IndustryList ON CT_IndustryList (Standard, IndustryID)")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.LC_ExgIndustry <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from LC_ExgIndustry")
  con <- db.local()
  #   dbSendQuery(con,"delete from LC_ExgIndustry")
  dbWriteTable(con,"LC_ExgIndustry",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE unique INDEX IX_LC_ExgIndustry ON LC_ExgIndustry (Standard, stockID, InDate)")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.LC_IndexComponent <- function(){
  tb.from <- read.db.odbc(db.quant(),query="select * from LC_IndexComponent")
  con <- db.local()
  #   dbSendQuery(con,"delete from LC_IndexComponent")
  dbWriteTable(con,"LC_IndexComponent",tb.from,overwrite=TRUE,append=FALSE,row.names=FALSE)
  res <- dbSendQuery(con,"CREATE unique INDEX IX_LC_IndexComponent ON LC_IndexComponent (IndexID, InDate,SecuID)")
  dbClearResult(res)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.LC_IndexComponentsWeight <- function(){  
  con <- db.local()
  begT <- dbGetQuery(con,"select max(EndDate) from LC_IndexComponentsWeight")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from LC_IndexComponentsWeight where EndDate >",begT))  
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"LC_IndexComponentsWeight",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}


#' add new security to local database. 
#' @param IndexID
#' @return NULL
#' @export
#' @examples
#' lcdb.add.LC_IndexComponentsWeight(c("EI000905","EI000906"))
lcdb.add.LC_IndexComponentsWeight <- function(IndexID){
  # IndexID <- c("EI000905","EI000906")
  
  con <- db.local()
  IDs <- paste(QT(IndexID),collapse = ",")
  IDs <- paste("(",IDs,")")
  endT <- dbGetQuery(con,"select max(EndDate) from LC_IndexComponentsWeight")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from LC_IndexComponentsWeight where EndDate <=",endT,"and IndexID in ",IDs))  
  if(NROW(tb.from)==0){
    return()
  }    
  dbSendQuery(con,paste("delete from LC_IndexComponentsWeight where IndexID in ",IDs))  
  dbWriteTable(con,"LC_IndexComponentsWeight",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}

#' @rdname lcdb.update
#' @export
lcdb.update.QT_UnTradingDay <- function(){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(TradingDay) from QT_UnTradingDay")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from QT_UnTradingDay where TradingDay >",begT))  
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"QT_UnTradingDay",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)  
}
#' @rdname lcdb.update
#' @export
lcdb.update.QT_IndexQuote <- function(){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(TradingDay) from QT_IndexQuote")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from QT_IndexQuote where TradingDay >",begT)) 
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"QT_IndexQuote",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con) 
}
#' @rdname lcdb.update
#' @export
lcdb.update.QT_DailyQuote <- function(){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from QT_DailyQuote where TradingDay >",begT))
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"QT_DailyQuote",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}
#' @rdname lcdb.update
#' @export
lcdb.update.QT_DailyQuote2 <- function(){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(TradingDay) from QT_DailyQuote2")[[1]]
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from QT_DailyQuote where TradingDay >",begT))
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"QT_DailyQuote2",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}

#' @rdname lcdb.update
#' @export
lcdb.update.LC_RptDate <- function(){  
  con <- db.local()
  begT <- dbGetQuery(con,"select max(PublDate) from LC_RptDate")[[1]]
  #   begT <- 19900101
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from LC_RptDate where PublDate >",begT))  
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"LC_RptDate",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}



#' @rdname lcdb.update
#' @export
lcdb.update.LC_PerformanceGrowth <- function(){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(InfoPublDate) from LC_PerformanceGrowth")[[1]]
   # begT <- 19900101
  tb.from <- read.db.odbc(db.quant(),query=paste("select * from LC_PerformanceGrowth where InfoPublDate>",begT))
  if(NROW(tb.from)==0){
    return()
  }
  dbWriteTable(con,"LC_PerformanceGrowth",tb.from,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}



#' add a index to local database from JY database
#'
#'
#' @author Andrew Dow
#' @param indexID is index code,such as EI000300
#' @return nothing
#' @examples
#' lcdb.add.LC_indexComponent(indexID="EI801003")
#' lcdb.add.LC_indexComponent(indexID="EI000985")
#' @export
lcdb.add.LC_indexComponent <- function(indexID){
  #check whether the index in local db
  if(indexID=='EI000985'){
    con <- db.local()
    qr <- paste("select * from SecuMain where ID="
                ,QT(indexID),sep="")
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
    if(nrow(re)>0) return("Already in local database!")
    
    #part 1 update local SecuMain
    con <- db.jy()
    qr <- paste("select ID,InnerCode,CompanyCode,SecuCode,SecuAbbr,SecuMarket,
                ListedSector,ListedState,JSID 'UpdateTime',SecuCode 'StockID_TS',
                SecuCategory,ListedDate,SecuCode 'StockID_wind'
                from SecuMain WHERE SecuCode=",
                QT(substr(indexID,3,8)),
                " and SecuCategory=4")
    indexInfo <- sqlQuery(con,qr)
    indexInfo <- transform(indexInfo,ID=indexID,
                           SecuCode='000985',
                           StockID_TS='SH000985',
                           StockID_wind='000985.SH')
    
    #part 2 update local LC_IndexComponent
    qr <- "SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
    convert(varchar(8),l.InDate,112) 'InDate',convert(varchar(8),l.OutDate,112) 'OutDate',
    l.Flag,l.XGRQ 'UpdateTime',convert(varchar(8),s2.ListedDate,112) 'IPODate'
    FROM LC_IndexComponent l
    inner join SecuMain s1 on l.IndexInnerCode=s1.InnerCode and s1.SecuCode in('801003','000985')
    LEFT join SecuMain s2 on l.SecuInnerCode=s2.InnerCode
    order by s1.SecuCode,l.InDate"
    re <- sqlQuery(con,qr,stringsAsFactors=F)
    indexComp <- re[re$IndexID=='EI000985',c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    
    tmp <- re[re$IndexID=='EI801003' & re$InDate<20110802,]
    tmp <- tmp[substr(tmp$SecuID,1,3) %in% c('EQ0','EQ3','EQ6'),]
    tmp <- transform(tmp,InDate=intdate2r(InDate),
                     OutDate=intdate2r(OutDate),
                     IPODate=intdate2r(IPODate))
    tmp[(tmp$InDate-tmp$IPODate) < 90,'InDate'] <- trday.offset(tmp[(tmp$InDate-tmp$IPODate) < 90,'IPODate'],by = months(3))
    tmp <- tmp[tmp$InDate < as.Date('2011-08-02'),]
    tmp <- tmp[,c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    
    qr <- "select 'EQ'+s.SecuCode 'SecuID',st.SpecialTradeType,
    ct.MS,convert(varchar(8),st.SpecialTradeTime,112) 'SpecialTradeTime'
    from JYDB.dbo.LC_SpecialTrade st,JYDB.dbo.SecuMain s,JYDB.dbo.CT_SystemConst ct
    where st.InnerCode=s.InnerCode and SecuCategory=1
    and st.SpecialTradeType=ct.DM and ct.LB=1185 and st.SpecialTradeType in(1,2,5,6)
    order by s.SecuCode"
    st <- sqlQuery(con,qr,stringsAsFactors=F)
    odbcCloseAll()
    st <- st[substr(st$SecuID,1,3) %in% c('EQ0','EQ3','EQ6'),]
    st <- st[st$SpecialTradeTime<20110802,]
    st$InDate <- ifelse(st$SpecialTradeType %in% c(2,6),st$SpecialTradeTime,NA)
    st$OutDate <- ifelse(st$SpecialTradeType %in% c(1,5),st$SpecialTradeTime,NA)
    st$InDate <- intdate2r(st$InDate)
    st$OutDate <- intdate2r(st$OutDate)
    st <- st[,c("SecuID","InDate","OutDate")]
    
    tmp <- rbind(tmp[,c("SecuID","InDate","OutDate")],st)
    tmp <- reshape2::melt(tmp,id=c('SecuID'))
    tmp <- tmp[!(is.na(tmp$value) & tmp$variable=='InDate'),]
    tmp <- unique(tmp)
    tmp[is.na(tmp$value),'value'] <- as.Date('2100-01-01')
    tmp <- dplyr::arrange(tmp,SecuID,value)
    
    tmp$flag <- c(1)
    for(i in 2: nrow(tmp)){
      if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i]==tmp$variable[i-1] && tmp$variable[i]=='InDate'){
        tmp$flag[i-1] <- 0
      }else if(tmp$SecuID[i]==tmp$SecuID[i-1] && tmp$variable[i]==tmp$variable[i-1] && tmp$variable[i]=='OutDate'){
        tmp$flag[i] <- 0
      }else{
        next
      }
    }
    tmp <- tmp[tmp$flag==1,c("SecuID","variable","value")]
    tmp <- dplyr::arrange(tmp,SecuID,value)
    tmp1 <- tmp[tmp$variable=='InDate',]
    tmp2 <- tmp[tmp$variable=='OutDate',]
    tmp <- cbind(tmp1[,c("SecuID","value")],tmp2[,"value"])
    colnames(tmp) <- c("SecuID","InDate","OutDate")
    tmp[tmp$OutDate>as.Date('2011-08-02'),'OutDate'] <- as.Date('2011-08-02')
    tmp$IndexID <- 'EI000985'
    tmp$Flag <- 0
    tmp$UpdateTime <- Sys.time()
    tmp$InDate <- rdate2int(tmp$InDate)
    tmp$OutDate <- rdate2int(tmp$OutDate )
    tmp <- tmp[,c("IndexID","SecuID","InDate","OutDate","Flag","UpdateTime")]
    
    indexComp <- rbind(indexComp,tmp)
    con <- db.local()
    dbWriteTable(con,"SecuMain",indexInfo,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbWriteTable(con,"LC_IndexComponent",indexComp,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbDisconnect(con)
  }else{
    con <- db.local()
    qr <- paste("select * from SecuMain where ID="
                ,QT(indexID))
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
    if(nrow(re)>0) return("Already in local database!")
    
    #part 1 update local SecuMain
    con <- db.jy()
    qr <- paste("select ID,InnerCode,CompanyCode,SecuCode,SecuAbbr,SecuMarket,
                ListedSector,ListedState,JSID 'UpdateTime',SecuCode 'StockID_TS',
                SecuCategory,ListedDate,SecuCode 'StockID_wind'
                from SecuMain WHERE SecuCode=",
                QT(substr(indexID,3,8)),
                " and SecuCategory=4",sep='')
    indexInfo <- sqlQuery(con,qr)
    indexInfo <- transform(indexInfo,ID=indexID,
                           StockID_TS=ifelse(is.na(stockID2stockID(indexID,'local','ts')),substr(indexID,3,8),
                                             stockID2stockID(indexID,'local','ts')),
                           StockID_wind=ifelse(is.na(stockID2stockID(indexID,'local','wind')),substr(indexID,3,8),
                                               stockID2stockID(indexID,'local','wind')))
    
    #part 2 update local LC_IndexComponent
    qr <- paste("SELECT 'EI'+s1.SecuCode 'IndexID','EQ'+s2.SecuCode 'SecuID',
                convert(varchar(8),l.InDate,112) 'InDate',
                convert(varchar(8),l.OutDate,112) 'OutDate',l.Flag,l.XGRQ 'UpdateTime'
                FROM LC_IndexComponent l inner join SecuMain s1
                on l.IndexInnerCode=s1.InnerCode and s1.SecuCode=",
                QT(substr(indexID,3,8))," LEFT join JYDB.dbo.SecuMain s2
                on l.SecuInnerCode=s2.InnerCode")
    indexComp <- sqlQuery(con,qr)
    odbcCloseAll()
    
    con <- db.local()
    dbWriteTable(con,"SecuMain",indexInfo,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbWriteTable(con,"LC_IndexComponent",indexComp,overwrite=FALSE,append=TRUE,row.names=FALSE)
    dbDisconnect(con)
  }

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
  con <- db.jy()
  qr <- "SELECT 'EQ'+s.SecuCode 'stockID',l.CompanyCode,l.FirstIndustryCode 'Code1',l.FirstIndustryName 'Name1',
  l.SecondIndustryCode 'Code2',l.SecondIndustryName 'Name2',l.ThirdIndustryCode 'Code3',
  l.ThirdIndustryName 'Name3',convert(varchar, l.InfoPublDate, 112) 'InDate',
  convert(varchar, l.CancelDate, 112) 'OutDate',l.InfoSource,l.Standard,l.Industry,
  l.IfPerformed 'Flag',l.XGRQ 'UpdateTime'
  FROM [JYDB].[dbo].[LC_ExgIndustry] l,JYDB.dbo.SecuMain s
  where l.CompanyCode=s.CompanyCode and s.SecuCategory=1
  and s.SecuMarket in(83,90) and l.Standard in(9,24)"
  re <- sqlQuery(con,qr,stringsAsFactors=F)
  re <- re[substr(re$stockID,1,3) %in% c('EQ6','EQ3','EQ0'),]
  re <- re[ifelse(is.na(re$OutDate),T,re$OutDate!=re$InDate),] # remove indate==outdate wrong data
  
  sw24use <- re[(re$InDate>20140101) & (re$Standard==24),]
  sw9use <- re[(re$InDate<20140101) & (re$Standard==9),]
  sw24tmp <- re[(re$InDate==20140101) & (re$Standard==24),]
  sw9tmp <- sw9use[is.na(sw9use$OutDate) | sw9use$OutDate>20140101,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")]
  colnames(sw9tmp) <- c("stockID","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")
  hashtable <- merge(sw24tmp,sw9tmp,by='stockID',all.x=T)
  hashtable <- hashtable[,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  hashtable <- unique(hashtable)
  hashtable <- plyr::ddply(hashtable,~OldName3,plyr::mutate,n=length(OldName3))
  hashtable <- hashtable[hashtable$n==1,c("Code1","Name1","Code2","Name2","Code3","Name3","OldCode1","OldName1","OldCode2","OldName2","OldCode3","OldName3")]
  
  sw9use <- plyr::rename(sw9use,replace=c("Code1"="OldCode1",
                                          "Name1"="OldName1",
                                          "Code2"="OldCode2",
                                          "Name2"="OldName2",
                                          "Code3"="OldCode3",
                                          "Name3"="OldName3"))
  sw9use <- merge(sw9use,hashtable,by=c("OldCode1","OldName1",
                                        "OldCode2","OldName2",
                                        "OldCode3","OldName3"),all.x=T)
  sw9use <- sw9use[,c("stockID","CompanyCode","Code1","Name1","Code2","Name2",
                      "Code3","Name3","InDate","OutDate","InfoSource","Standard",
                      "Industry","Flag","UpdateTime","OldCode1","OldName1","OldCode2",
                      "OldName2","OldCode3","OldName3")]
  tmp <- sw9use[is.na(sw9use$Code1),c("stockID","CompanyCode","InDate","OutDate","InfoSource","Standard",
                                      "Industry","Flag","UpdateTime","OldCode1","OldName1","OldCode2",
                                      "OldName2","OldCode3","OldName3")]
  sw9use <- sw9use[!is.na(sw9use$Code1),c("stockID","CompanyCode","Code1","Name1","Code2","Name2",
                                          "Code3","Name3","InDate","OutDate","InfoSource","Standard",
                                          "Industry","Flag","UpdateTime")]
  
  tmp <- dplyr::arrange(tmp,stockID,InDate)
  tmp <-merge(tmp,sw24tmp[,c("stockID","Code1","Name1","Code2","Name2","Code3","Name3")],by='stockID',all.x=T)
  zhcn <- unique(sw24use[sw24use$Code1==510000,'Name1'])
  tmp[is.na(tmp$Code1),c("Name1","Name2","Name3")] <- zhcn
  tmp[is.na(tmp$Code1),"Code1"] <-510000
  tmp[is.na(tmp$Code2),"Code3"] <-510100
  tmp[is.na(tmp$Code3),"Code3"] <-510101
  tmp <- tmp[,c("stockID","CompanyCode","Code1","Name1","Code2","Name2",
                "Code3","Name3","InDate","OutDate","InfoSource","Standard",
                "Industry","Flag","UpdateTime")]
  sw9use <- rbind(sw9use,tmp)
  
  sw33 <- rbind(sw9use,sw24use)
  sw33$Standard <- 33
  sw33$Code1 <- paste('ES33',sw33$Code1,sep = '')
  sw33$Code2 <- paste('ES33',sw33$Code2,sep = '')
  sw33$Code3 <- paste('ES33',sw33$Code3,sep = '')
  sw33$Code99 <- c(NA)
  sw33$Name99 <- c(NA)
  sw33$Code98 <- c(NA)
  sw33$Name98 <- c(NA)
  sw33 <- dplyr::arrange(sw33,stockID,InDate)
  
  #deal with abnormal condition
  #1 outdate<=indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),T,sw33$OutDate>sw33$InDate),]
  #2 one stock has two null outdate
  tmp <- plyr::ddply(sw33,'stockID',plyr::summarise,NANum=sum(is.na(OutDate)))
  tmp <- c(tmp[tmp$NANum>1,'stockID'])
  sw33tmp <- sw33[sw33$stockID %in% tmp,]
  sw33 <- sw33[!(sw33$stockID %in% tmp),]
  if(nrow(sw33tmp)>0){
    for(i in 1:(nrow(sw33tmp)-1)){
      if(sw33tmp$stockID[i]==sw33tmp$stockID[i+1] && is.na(sw33tmp$OutDate[i])) sw33tmp$OutDate[i] <- sw33tmp$InDate[i+1]
    }
  }
  sw33 <- rbind(sw33,sw33tmp)
  sw33 <- dplyr::arrange(sw33,stockID,InDate)
  #3 indate[i+1]!=outdate[i]
  sw33$tmpstockID <- c(NA,sw33$stockID[1:(nrow(sw33)-1)])
  sw33$tmpOutDate <- c(NA,sw33$OutDate[1:(nrow(sw33)-1)])
  sw33$InDate <- ifelse(ifelse(is.na(sw33$tmpstockID) | is.na(sw33$tmpOutDate),FALSE,sw33$stockID==sw33$tmpstockID & sw33$InDate!=sw33$tmpOutDate),
                        sw33$tmpOutDate,sw33$InDate)
  sw33 <- subset(sw33,select=-c(tmpstockID,tmpOutDate))
  # 4 duplicate indate
  sw33 <- sw33[ifelse(is.na(sw33$OutDate),T,sw33$OutDate>sw33$InDate),]
  sw33[!is.na(sw33$OutDate) & sw33$Flag==1,'Flag'] <- 2
  
  # update local database CT_IndustryList
  qr <- "SELECT Standard,Classification 'level','ES33'+IndustryCode 'IndustryID'
  ,IndustryName,SectorCode 'Alias','ES33'+FirstIndustryCode 'Code1'
  ,FirstIndustryName 'Name1','ES33'+SecondIndustryCode 'Code2'
  ,SecondIndustryName 'Name2','ES33'+ThirdIndustryCode 'Code3'
  ,ThirdIndustryName 'Name3',UpdateTime
  FROM CT_IndustryType where Standard=24"
  indCon <- sqlQuery(con,qr,stringsAsFactors=F)
  indCon$Standard <- 33
  indCon[is.na(indCon$Name2),'Code2'] <- NA
  indCon[is.na(indCon$Name3),'Code3'] <- NA
  
  # update local database CT_SystemConst
  syscon <- sqlQuery(con,"select top 1 LB, LBMC, DM ,MS  from CT_SystemConst where LB=1081",stringsAsFactors=F)
  syscon$DM <- 33
  syscon$MS <- "SHENWAN2014fixed"
  odbcCloseAll()
  
  # update...
  con <- db.local()
  res <- dbSendQuery(con,"delete  from LC_ExgIndustry where Standard=33;
                          delete  from CT_IndustryList where Standard=33;
                          delete  from CT_SystemConst where DM=33")
  dbClearResult(res)
  dbWriteTable(con,'LC_ExgIndustry',sw33,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_IndustryList',indCon,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbWriteTable(con,'CT_SystemConst',syscon,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
  # return('Done!')
}



#' @rdname lcdb.update
#' @export
lcfs.update <- function(endT=Sys.Date()){
  con <- db.local()
  begT <- dbGetQuery(con,"select max(TradingDay) from QT_FactorScore")[[1]]
  endT <- rdate2int(endT)
  TS <- dbGetQuery(con,paste("select TradingDay as date, ID as stockID from QT_DailyQuote2 where TradingDay > ",begT," and TradingDay <=",endT))
  if(NROW(TS)==0) {
    return()
  }
  TS <- transform(TS,date=intdate2r(date))
  
  factorLists <- CT_FactorLists()
  for(i in 1:NROW(factorLists)){
    factorName <- factorLists[i,"factorName"]
    factorID <- factorLists[i,"factorID"]
    factorFun <- factorLists[i,"factorFun"]
    factorPar <- factorLists[i,"factorPar"]    
    cat("Factor",factorName,"getting ...\n")
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
    targetfield <- dbListFields(con,"QT_FactorScore")
    extrfield <- setdiff(targetfield,names(re))
    extrdat <- as.data.frame(matrix(NA,NROW(re),length(extrfield)))
    names(extrdat) <- extrfield
    re <- cbind(re,extrdat)
    re <- re[targetfield]
  }
  
  dbWriteTable(con,"QT_FactorScore",re,overwrite=FALSE,append=TRUE,row.names=FALSE)
  dbDisconnect(con)
}



# This function comes from package RFactorModel
getRawFactor <- function (TS,factorFun,factorPar) {
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
}

# This function comes from package RFactorModel
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



lcdb.init.QT_DailyQuote <- function(){  
  con <- db.local()
  dbSendQuery(con,"
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
              CREATE UNIQUE INDEX [IX_QT_DailyQuote] ON [QT_DailyQuote]([ID] ,[TradingDay] DESC);
              CREATE INDEX IX_QT_DailyQuote_TradingDay ON QT_DailyQuote (TradingDay);
              ")  
  all.stocks <- read.db.odbc(db.quant(),"select distinct ID from QT_DailyQuote")[[1]]
  
  subfun <- function(stock0){
    #   stock0 <- "EQ000001"
    cat(paste(" ",stock0))
    data0<- read.db.odbc(db.quant(),paste("select * from QT_DailyQuote where ID=",QT(stock0)))
    dbWriteTable(con, "QT_DailyQuote", data0, overwrite = FALSE, append=TRUE,row.names = FALSE)
    gc()    
  }
  cat("Function lite.QT_DailyQuote: getting the quote ....\n")
  l_ply(all.stocks, subfun, .progress = progress_text(style=3))  
  dbDisconnect(con)
}

lcdb.init.QT_DailyQuote2 <- function(){  
  dbSendQuery(con,"CREATE UNIQUE INDEX IX_QT_DailyQuote2 ON QT_DailyQuote2 (TradingDay desc ,ID)")
  con <- db.local()
  dbSendQuery(con,"
              CREATE TABLE 'QT_DailyQuote2' (
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
              CREATE UNIQUE INDEX IX_QT_DailyQuote2 ON QT_DailyQuote2 (TradingDay DESC, ID);            
              CREATE INDEX IX_QT_DailyQuote2_ID on QT_DailyQuote2 (ID);
              ")    
  all.days <- read.db.odbc(db.quant(),"select distinct TradingDay from QT_DailyQuote")[[1]]
  all.days <- all.days[order(-all.days)]
  
  subfun <- function(day0){
    cat(paste(" ",day0))
    data0 <- dbGetQuery(con,paste("select * from QT_DailyQuote where TradingDay=",day0))
    dbWriteTable(con, "QT_DailyQuote2", data0, overwrite = FALSE, append=TRUE,row.names = FALSE)
    gc()    
  }
  cat("Function lite.QT_DailyQuote: getting the quote ....\n")
  l_ply(all.days, subfun, .progress = progress_text(style=3))  
  dbDisconnect(con)
}




#' lcfs.add
#' 
#' add/update a factorscore column in local sqlite table \code{"QT_DailyQuote"}. On the same time, correspondingly, add/update a record into table \code{"CT_FactorLists"} and table \code{"CT_TechVars"}.
#' @param factorFun a character string naming the function to get the factor scores
#' @param factorPar a character string, containing the parameters of the \code{factorFun}. Note that unlike in \code{\link{getTSF}}, here the factorPar could not be a list, because it need to be written into database.
#' @param factorDir a integer,should be 1 or -1 (1 for the positive factor,-1 for the negative one). \bold{Note that} the \code{factorDir} here is only used to write a record into table \code{"CT_FactorLists"}, not used when getting \code{TSF}. So that the factorscore in table \code{"QT_DailyQuote"} is kept \bold{"raw", without adding the dirrection infomation}.
#' @param factorID a character string
#' @param factorName a character string. IF missing, then take a default name by function \code{default.factorName}. 
#' @param factorType a character string
#' @param factorDesc a character string
#' @param begT the begin date of the adding/updating
#' @param endT the end date of the adding/updating
#' @param splitNbin a character of interval specification(see \code{\link{cut.Date}} for detail). Specify the time interval when looping of getting the \code{TSF} object.
#' @return Write data into the local sqlite database, returning NULL.
#' @seealso \code{\link{getTSF}},\code{\link{modelPar.factor}}, \code{\link{lcfs.update}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' system.time(lcfs.add(factorFun="gf.F_rank_chg",factorPar="lag=60,con_type=\"1,2\"", factorDir=1, factorID="F000009"))
lcfs.add <- function(factorFun, 
                     factorPar="", 
                     factorDir = 1,
                     factorID,                      
                     factorName = default.factorName(factorFun,factorPar,factorDir),                      
                     factorType = "", 
                     factorDesc = "",
                     begT = as.Date("1990-01-01"), endT = Sys.Date(),
                     splitNbin = "month"){   
  if(factorID %in% CT_FactorLists()$factorID) {
    is_overwrite <- select.list(choices=c("OK","CANCEL"),preselect="CANCEL",title=paste("Warning!\nThe factor",factorID,"has already exist!\nDo you want to overwrite it?"),graphics=FALSE)
    if(is_overwrite == "CANCEL") return(invisible(NULL))
  } 
  con <- db.local()
  # insert or replace a row to table 'CT_FactorLists' 
  if(!is.character(factorPar)){
    stop("The 'factorPar' must be a character!")
  }
  qr1 <- paste("replace into CT_FactorLists
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
  dbSendQuery(con,qr1)
  
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
  dbSendQuery(con,qr2)
  
  # add 1 colume to table 'QT_FactorScore' , looping day by day
  tryCatch(dbSendQuery(con,paste("ALTER TABLE QT_FactorScore ADD COLUMN ",factorID,"float(0, 4)")),
           error=function(e) { print("RS-DBI driver: (error in statement: duplicate column name)") })
  loopT <- dbGetQuery(con,"select distinct tradingday from QT_FactorScore order by tradingday")[[1]]
  loopT <- loopT[loopT>=rdate2int(begT) & loopT<=rdate2int(endT)]    
  loopT.L <- split(loopT,cut(intdate2r(loopT),splitNbin))
  
  subfun <- function(Ti){
    cat(paste(" ",min(Ti),"to",max(Ti)," ...\n"))
    dates <- paste(Ti,collapse=",")
    TS <- dbGetQuery(con,paste("select TradingDay as date, ID as stockID from QT_FactorScore where TradingDay in (",dates,")"))
    TS$date <- intdate2r(TS$date)    
    TSF <- getRawFactor(TS,factorFun,factorPar)
    TSF$date <- rdate2int(TSF$date)
    TSF <- renameCol(TSF,src="factorscore",tgt=factorID)
    
    for(Tij in Ti){ # update the factorscore day by day.
      #     Tij <- Ti[1]
      # cat(paste(" ",Tij))
      dbWriteTable(con,"yrf_tmp",TSF[TSF$date==Tij,],overwrite=TRUE,append=FALSE,row.names=FALSE)
      qr <- paste("UPDATE QT_FactorScore
                  SET ",factorID,"= (SELECT ",factorID," FROM yrf_tmp WHERE yrf_tmp.stockID =QT_FactorScore.ID) 
                  WHERE QT_FactorScore.ID = (SELECT stockID FROM yrf_tmp WHERE yrf_tmp.stockID =QT_FactorScore.ID)
                  and QT_FactorScore.TradingDay =",Tij)
      res <- dbSendQuery(con,qr)
      dbClearResult(res)  
    }   
    gc()
  }  
  
  cat(paste("Function lcfs.add: updateing factor score of",factorID,".... \n"))
  plyr::l_ply(loopT.L, subfun, .progress = plyr::progress_text(style=3))   
  dbDisconnect(con)
}




#' gf_lcfs
#' @export
gf_lcfs <- function(TS,factorID){
  re <- TS.getTech(TS,variables=factorID,tableName="QT_FactorScore")
  re <- renameCol(re,factorID,"factorscore")
  return(re)
}



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================
# ====================  Tinysoft related utility functions ====================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================
#' tsInclude
#'
#' Series of functions of connect/login Tinysoft server, and remote call funcions of Tinysoft.
#' @param funchar a charactor string of tinysoft script
#' @param pars a list of funchar's parametres
#' @param syspars a list of tinysoft system parametres.(including:StockID CurrentDate Cycle bRate RateDay Precision)
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
tsInclude <- function(){
  source(paste(.libPaths()[1],"/QDataGet/tslr/tslr.R",sep = ""))
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
ts.wss <- function(stocks,funchar,rptDate,Time=Sys.Date(),Rate=1,RateDay=0, 
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
  syspars <- list(CurrentDate=time,bRate=Rate,RateDay=RateDay)
  stocks.str <- paste(stocks,collapse=";")  
  str <- paste('Rdate:=',rptDate,';\n',
               'return Query("","',
               stocks.str,               
               '",True,"","stockID",DefaultStockID(),',
               funchar,
               ');',
               sep="")
  re <- tsRemoteExecute(str,syspars)
  re <- plyr::ldply(re,as.data.frame)
  re$stockID <- stockID2stockID(re$stockID,from="ts",to="local")
  return(re)
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ==================    tradingday related       =============
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' memory.load
#' 
#' load frequently used data into memory, including \code{.tradingdays}, \code{.QT_UnTradingDay}, ...
#' @param reload a logic. If reload the memory data?
#' @param datasrc
#' @return return NULL, but load some data into memory.
#' @export
#' @author Ruifei.Yin
memory.load <- function(reload=FALSE, datasrc=defaultDataSRC()){  
  if(!exists(".tradingdays") || reload){
    if(reload){
      cat("[memory data reloading]\n")
    } else {
      cat("[memory data loading]\n")
    }  
    
    # -- the market trading days  
    cat("  Loading the market trading days: '.tradingdays' ... \n")
    qr1 <- "select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1"
    if(datasrc=="local"){
      tradingdays <- read.db.dbi(db.local(),qr1)[[1]]
    } else if(datasrc=="quant"){
      tradingdays <- read.db.odbc(db.quant(),qr1)[[1]]
    }  
    .tradingdays <<- intdate2r(tradingdays)
    
    # -- the stock's untrading days
    cat("  Loading the stocks' untrading days: '.QT_UnTradingDay' ... \n")
    qr2 <- "select * from QT_UnTradingDay"
    if(datasrc=="local"){
      QT_UnTradingDay <- read.db.dbi(db.local(),qr2)
    } else if(datasrc=="quant") {
      QT_UnTradingDay <- read.db.odbc(db.quant(),qr2)
    }
    .QT_UnTradingDay <<- data.table::data.table(transform(QT_UnTradingDay, TradingDay = intdate2r(TradingDay)),key="ID")    
  }  
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
                          datasrc=defaultDataSRC()){    
  
  # get the market trading days
  if(datasrc %in% c("quant","local")){
    begT <- max(begT,as.Date("1990-12-19"))
    begT <- rdate2int(begT)
    endT <- rdate2int(endT)
    qr <- paste("select TradingDate from QT_TradingDay where SecuMarket=83 and IfTradingDay=1 and TradingDate between ",begT,"and",endT)    
    if(datasrc=="quant"){
      trday <- read.db.odbc(db.quant(),qr)
    } else if(datasrc=="local"){
      trday <- read.db.dbi(db.local(),qr)
    }     
    re <- trday[[1]]
    re <- intdate2r(re)
  } else if (datasrc=="memory") {
    begT <- max(begT,as.Date("1990-12-19"))
    if(!exists(".tradingdays")){
      stop("The object of '.tradingdays' is not exist in the memory! Please load it, with 'memory.load()'!")
    }
    re <- get(".tradingdays")
    re <- re[re >= begT & re <= endT]
  }  
  
  # get the stock's trading days
  if(!is.null(stockID)){
    if(datasrc %in% c("quant","local")){
      qr <- paste("select * from QT_UnTradingDay where ID =",QT(stockID))
      if(datasrc=="quant"){
        untrday <- read.db.odbc(db.quant(),qr)
      } else if(datasrc=="local"){
        untrday <- read.db.dbi(db.local(),qr)
      }       
      untrday <- untrday[["TradingDay"]]
      untrday <- intdate2r(untrday)      
    } else if (datasrc=="memory"){
      if(!exists(".QT_UnTradingDay")){
        stop("The object of '.QT_UnTradingDay' is not exist in the memory! Please load it, with 'memory.load()'!")
      }
      untrday <-  .QT_UnTradingDay[stockID,]$TradingDay
    } 
    re <- as.Date(setdiff(re,untrday),origin="1970-01-01")
  }    
  
  re <- re[order(re)] 
  return(re)
}





# stocks <- unique(.QT_UnTradingDay$ID)
# system.time(
#   sapply(stocks,function(stockID){
#     tmp <- read.db.dbi(db.local(),paste("select TradingDay from QT_UnTradingDay where ID =",QT(stockID),sep=""))
#   })
# ) # 20.61
# system.time(
#   sapply(stocks,function(stockID){
#     tmp <- .QT_UnTradingDay[stockID,TradingDay]
#   })
# ) # 16.13





#' trday.is
#' 
#' Identify if the date is trading day or not
#' @param datelist a vector of class Date
#' @param stockID a character string or NULL If NULL, the market trading days, other wise the trading days of specific stock.
#' @return a logical vecter with elements TRUE or FALSE depending on whether its argument is an tradingday
#' @export
#' @author Ruifei.Yin
#' @examples
#' datelist <- seq(from=as.Date("2013-01-21"),to=as.Date("2013-01-30"),by="day")
#' trday.is(datelist)
#' trday.is(datelist,stockID="EQ000527")
trday.is <- function(datelist,stockID=NULL){
  mindt <- min(datelist)
  maxdt <- max(datelist)
  tradingday <- trday.get(begT=mindt,endT=maxdt,stockID=stockID)
  re <- datelist %in% tradingday
  return(re)
}


#' trday.nearest
#'
#' get the nearest tradingday. 
#' @param datelist a vector of class Date
#' @param dir a integer. Indicating forward or backward to find, if \code{datelist} is not tradingday. 1 for forward, -1 for backward.
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @return a vector of class Date, the value of which is the nearest tradingday before/after the \code{datelist} if \code{datelist} is not a tradingday, otherwise,the \code{datelist} itself. 
#' @author Ruifei.Yin
#' @export
#' @examples
#' datelist <- as.Date(c("2012-07-21","2012-07-22","2012-07-23","2013-01-30"))
#' trday.nearest(datelist)
#' trday.nearest(datelist, dir = -1)
#' trday.nearest(datelist, dir = -1, stockID="EQ000527")
trday.nearest <- function(datelist, dir=1, stockID=NULL){ 
  tradingdays <- trday.get(endT=Sys.Date()+365, stockID=stockID)
  if(dir==1){
    re <- tradingdays[findInterval(datelist, tradingdays)]  
  } else {
    re <- tradingdays[findInterval.rightClosed(datelist, tradingdays)+1]
  }  
  re <- as.Date(re,origin="1970-01-01")
  return(re)
}






#' trday.nearby
#' 
#' get the nearby tradingday by shifting forward or backward.If the argument datelist is not a tradingday,the tradingday nearest by it will be firstly find by function \code{\link{trday.nearest}}.
#' @param datelist a vector of class Date
#' @param by a integer giving the lagging days.If positive,get the earlyer tradingday,if negetive,get the later tradingday. 
#' @param stockID a character string or null. If null, the market trading days, other wise the trading days of specific stock.
#' @return a vector of trading days of class Date
#' @export
#' @author Ruifei.Yin
#' @examples
#' (datelist <- as.Date(c("2012-07-21","2012-07-22","2012-07-23","2013-01-30")))
#' trday.nearby(datelist,20) # the tradingday 20 days earlyer than datelist
#' trday.nearby(datelist,-20) # the tradingday 20 days later than datelist 
#' trday.nearby(datelist,-20,stockID="EQ000527") 
trday.nearby <- function(datelist,by=1, stockID=NULL){
  trdingday.all <- trday.get(endT=Sys.Date()+365, stockID=stockID)   
  trdlist <- trdingday.all[findInterval(datelist, trdingday.all)] # get the nearest tradingday
  lag.trdingday.all <- xts::lag.xts(trdingday.all, by)
  idx <- match(trdlist, trdingday.all)
  re <- lag.trdingday.all[idx]
  return(re)
}



trday.nearest_TS <- function(TS){
  
}

trday.next_TS <- function(TS){
  
}


#' trday.offset 
#' 
#' offset the datelist by months,quarters,ect. then get the nearest tradingday
#' @param datelist a vector of class Date
#' @param by a period object. See detail in package \code{lubridate}.
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
trday.offset <- function(datelist,by=months(1), stockID=NULL){
  re <- datelist + by 
  if (any(c(by@.Data, by@minute, by@hour, by@day) != 0)){
    re[is.na(re)] <- datelist[is.na(re)] + by
  } else { # if handles month and years, should use %m+% 
    re[is.na(re)] <- datelist[is.na(re)] %m+% by
  }
  re <- trday.nearest(re, dir=1, stockID=stockID)
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

trday.first <- function(stockID,datasrc){
  
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
    tmpdat <- read.db.odbc(db.quant(),query=qr,as.is=1)
  } else if(datasrc=="local") {
    tmpdat <- read.db.dbi(db.local(),query=qr)
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
    tmpdat <- read.db.odbc(db.quant(),query=qr,as.is=1)
  } else if(datasrc=="local") {
    tmpdat <- read.db.dbi(db.local(),query=qr)
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
    tmpdat <- read.db.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- read.db.dbi(db.local(),query=qr)
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
stockID2name <- function(stockID,IDsrc="local",
                         datasrc=defaultDataSRC()){
  id_var <- switch(IDsrc,
                   local="ID",
                   quant="ID",
                   jy="InnerCode",
                   ts="StockID_TS",
                   wind="StockID_wind")
  qr <- paste("select SecuAbbr,",id_var,"from SecuMain")
  
  if(datasrc=="quant"){
    tmpdat <- read.db.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- read.db.dbi(db.local(),query=qr)
  } 
  
  re <- tmpdat[match(stockID,tmpdat[,2]),1]  
  return(re)  
}

#' stockName2ID
#' @return a dataframe with cols: 'SecuAbbr', 'ID'
#' @export
#' @family SecuMain functions
#' @examples
#' stockName2ID("ST")
stockName2ID <- function(name, datasrc=defaultDataSRC()){
  qr <- paste("select SecuAbbr, ID from SecuMain")
  if(datasrc=="quant"){
    tmpdat <- read.db.odbc(db.quant(),query=qr)
  } else if(datasrc=="local") {
    tmpdat <- read.db.dbi(db.local(),query=qr)
  }   
  re <- tmpdat[grep(name, gsub("\\s*","",tmpdat$SecuAbbr)), ]  
  return(re) 
}


# get secucategory
getSecuCate <- function(stockID){
  
}

secumain <- function(){
  
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    sector components & sectorID      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============




#' getComps
#'
#' get the components of the specific index, sector or plate on certain days.
#' @param sectorIDs a character string. The ID of the index, sector or plate. Could be a single-ID-code(eg. "EI000300","ES09440000",...) or a more complicated express containing some set operations and ID-codes(eg. "setdiff(union(EI000300,EI000905),ES09440000)")
#' @param endT a vector of class \code{Date}. IF missing, then get the latest components.
#' @param drop if drop the field of date and return a vector when endT is length 1 ?
#' @param datasrc
#' @return If \code{endT} is missing or is length 1 and \code{drop} is TRUE, a vector of stockID of the components; else a dataframe, with cols: "date" and "stockID"
#' @export
#' @family getComps functions
#' @examples
#' re1 <- getComps("EI000300") # same as getIndexComp("EI000300")
#' sectorIDs <- "setdiff(union(EI000300,EI399006),ES09440000)"
#' re2 <- getComps(sectorIDs)
#' re3 <- getComps(sectorIDs,endT=as.Date(c("2011-12-31","2012-12-31")))
#' more examples:
#' # CSI300 ex. financial servive sector
#' getComps("setdiff(EI000300,ES09440000)")
#' # CSI300 and financial servive sector
#' getComps("intersect(EI000300,ES09440000)")
#' # not drop
#' getComps("EI000300",drop=FALSE)
getComps <- function(sectorIDs, endT=Sys.Date(), drop=TRUE, datasrc=defaultDataSRC()){  
  IDs <- gsub("union|intersect|setdiff|[()]","",sectorIDs)
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
    dat0 <- with(comps0,eval(parse(text=sectorIDs)))
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
getIndexComp <- function(indexID, endT=Sys.Date(), drop=TRUE, datasrc=defaultDataSRC()){
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
      con <- db.local()
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re$date <- intdate2r(re$date) 
    re <- dplyr::arrange(re,date)
  } 
  if(datasrc=="ts"){
    endT <- rdate2ts(endT)
    indexID <- stockID2stockID(indexID,to="ts",from="local")
    subfun <- function(endT0){  
      stocks <- tsRemoteCallFunc("GetBKbyDate",list(indexID,endT0))
      stocks <- as.vector(as.matrix(stocks))
      stocks <- stockID2stockID(stocks,to="local",from="ts")
      dat0 <- data.frame(date=tsdate2r(endT0),stockID=stocks,stringsAsFactors=FALSE)
      return(dat0)
    }
    re <- plyr::ldply(endT,subfun)  
  }
  
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
#' tmp <- getIndexCompWgt("EI000300",as.Date("2012-12-31")) # get the components on single day
#' tmp <- getIndexCompWgt("EI000300",as.Date(c("2011-12-31","2012-12-31"))) # get the components on multi-days
getIndexCompWgt <- function(indexID="EI000300",endT,datasrc=defaultDataSRC()){
  
  if(missing(endT)) endT <- trday.nearby(Sys.Date(),by=1)  # if endT missing, get the nearest wgt data.  
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
      con <- db.local()
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    } else if (datasrc=="jy"){
      con <- db.jy()
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
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
  if(is.numeric(daymat)) stop("date out of range")
  endT <- daymat$newday
  
  
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
      con <- db.local()
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re=merge(re,daymat,by.x="date",by.y ="newday" )
    re=re[,c("oldday","stockID","wgt")]
    colnames(re)<-c("date","stockID","wgt")
    re <- dplyr::arrange(re,date)     
  }
  
  if(datasrc=="jy"){
    indexID <- stockID2stockID(indexID,to="jy",from="local")     
    subfun <- function(endT0){
      qr <- paste(
        "SELECT InnerCode as stockID, Weight/100 as wgt
        FROM LC_IndexComponentsWeight as a  
        where IndexCode=",QT(indexID) ,"and a.EndDate=",QT(endT0)
      )
      dat <- read.db.odbc(db.jy(),query=qr)            
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
      con <- db.local()
      dbWriteTable(con, name="yrf_tmp", value=tmpdat, row.names = FALSE, overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re$date <- intdate2r(re$date)  
    re <- dplyr::arrange(re,date)
  }   
  
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


#' getSectorID
#'
#' get the sectorID of the stocks on specific dates.
#' @param TS  a \bold{TS} object
#' @param stockID a vector of stockID
#' @param endT a vector of Date
#' @param sectorAttr a list with two elements: \code{std}, a integer ID number, indicating the sector standard(9 for SHENWAN sector,3 for ZHONGXIN sector.See more by \code{\link{CT_sectorSTD}}; \code{level}, a integer, giving the level of sector,the default is 1.
#' @param ret a charactor string,could be one of "ID" or "name",indicating sectorID or sectorName returned.
#' @param exclude.TS a logical. Shoud the \code{TS} be inculded in the result?
#' @param datasrc
#' @return a data.frame,with the same cols of TS,added by "\code{sector}". Or a vector if \code{exclude.TS} is TRUE. You can get more sector infomation by \code{\link{CT_industryList}}
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
getSectorID <- function(TS, stockID, endT=Sys.Date(),
                        sectorAttr=defaultSectorAttr(),ret=c("ID","name"),exclude.TS=FALSE,
                        datasrc=defaultDataSRC()){
  ret <- match.arg(ret)
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
  sectorSTD <- sectorAttr[[1]]
  level <- sectorAttr[[2]]
  
  if("sector" %in% colnames(TS)){
    warning('There is already a "sector" field in TS, it will be overwritten!')
    TS$sector <- NULL    
  }  
  
  if(datasrc %in% c("quant","local")){
    TS$date <- rdate2int(TS$date)
    sectorvar <- if (ret=="ID") paste("Code",level,sep="") else paste("Name",level,sep="")    
    qr <- paste(
      "select date,a.stockID,",sectorvar,"as sector
      from yrf_tmp as a left join LC_ExgIndustry as b
      on a.stockID=b.stockID and InDate<=date and (OutDate>date or OutDate is null)
      where b.Standard=",sectorSTD
    )
    
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=TS[,c("date","stockID")],tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local()
      dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }   
    
    re <- merge.x(TS,re,by=c("date","stockID"))
    re$date <- intdate2r(re$date)
  }     
  
  if(exclude.TS){
    return(re[,"sector"])
  } else {
    return(re)
  }  
}

#' deal with the NA value of sectorID
#' 
#' replace the NA value of sectorID with an "OTHER" sector
#' @param TSS a dataframe with a "sector" colume
#' @param sectorAttr
#' @export
sectorNA_fill <- function(TSS,sectorAttr=defaultSectorAttr()){
  Standard=c(	3,	3,	3,	9,	9,	9,	9,	9,	33,	33,	33)
  Level=c(	1,	2,	3,	1,	2,	3,	98,	99,	1,	2,	3)
  IndustryID=c(	'ES0370',	'ES037010',	'ES03701010',	'ES09510000',	'ES09510100',	'ES09510101',	'ES0951000098',	'ES0951000099',	'ES33510000')
  rp <- IndustryID[Standard==sectorAttr[[1]]&Level==sectorAttr[[2]]]
  TSS[is.na(TSS$sector),'sector'] <- rp
  return(TSS)
}





#' defaultSectorAttr
#' 
#' get the default sectorAttr. You can reset the default value by eg. \code{options(sectorAttr=list(std=3,level=2))}
#' @return a list, the value of the default sectorAttr
#' @export
#' @examples
#' # -- get the default sectorAttr
#' defaultSectorAttr()
#' # -- reset 
#' options(sectorAttr=list(std=3,level=2))
#' # -- reget
#' defaultSectorAttr()
defaultSectorAttr <- function(){
  getOption("sectorAttr",default=list(std=33,level=1))
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
    re <- read.db.dbi(db.local(),qr)
  } else if(datasrc=="quant"){
    re <- read.db.odbc(db.quant(),qr)
  } else if(datasrc=="jy"){
    re <- read.db.odbc(db.jy(),qr)[,c("LB","LBMC", "DM","MS")]
  }
  
  if(!missing(LB)){
    re <- re[re$LB==LB,]
  }
  return(re)
}

#' CT_sectorSTD
#' @export
#' @examples
#' CT_sectorSTD(DM=9)
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
    re <- read.db.dbi(db.local(),qr)
  } else if(datasrc=="quant"){
    re <- read.db.odbc(db.quant(),qr)
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
  re <- read.db.dbi(db.local(),"select * from CT_TechVars")
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
  re <- read.db.dbi(db.local(),"select * from CT_FactorLists")
  if(! missing(factorID)){
    re <- re[re$factorID %in% factorID,]
  }
  if(! missing(factorName)){
    re <- re[re$factorName %in% factorName,]
  }
  if(! missing(factorType)){
    re <- re[re$factorType %in% factorType,]
  }
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
  lastday <- as.Date(ifelse(trday.is(lastday0)|(lastday0>=Sys.Date()),lastday0,trday.nearby(lastday0,-1))) 
  # ---- firstday
  lastday_lag <-lag.m(lastday,1,TRUE)   
  firstday <- as.Date(ifelse(dt!=as.Date("2010-05-01"),trday.nearby(lastday_lag,-1),as.Date("2010-04-16")))
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
  qt <- getQuote_ts(code,begT,endT,variables=c("price","yclose"),cycle="cy_15m()")
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
# ===============    rptDate related      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============



#' getrptDate_newest
#' 
#' get the newest rptDate  of the stocks on specific dates.
#' @param TS  a \bold{TS} object
#' @param stockID a vector of stockID
#' @param endT a vector of Date
#' @param mult a character string. Could be one of "last","first","all". IF a listed company publish more than one financial reports, which one should be returned? the newest one(the default value), the earliest one or all of them?
#' @param exclude.TS a logical. Shoud the \code{TS} be inculded in the result?
#' @param datasrc
#' @return a data.frame,with the same cols of TS,added by "\code{rptDate}". Or a vector if \code{exclude.TS} is TRUE. 
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
getrptDate_newest <- function(TS,stockID,endT=Sys.Date(),mult=c("last","first","all"),
                              exclude.TS=FALSE,
                              datasrc=defaultDataSRC()){
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
    TS$date <- rdate2int(TS$date)
    qr <- paste(
      "select date, a.stockID, EndDate as rptDate
      from yrf_tmp as a left join LC_RptDate as b
      on a.stockID=b.stockID and PublDate<=date and (PublDate_next>date or PublDate_next is null)
      order by date, a.stockID, rptDate"      
    )
    
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=TS[,c("date","stockID")],tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local()
      dbWriteTable(con,name="yrf_tmp",value=TS[,c("date","stockID")],row.names = FALSE,overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    
    re <- merge.x(TS,re,by=c("date","stockID"),mult=mult)
    re <- transform(re, date=intdate2r(date), rptDate=intdate2r(rptDate))      
  } 
  
  if(exclude.TS){
    return(re[,"reptDate"])
  } else {
    return(re)
  }    
  
}

getrptDate_newestYear <- function(TS,stockID,endT){
  
}



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Others      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============




#' TS.getFin_rptTS
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
#' re <- TS.getFin_rptTS(TS,fun="rptTS.getFin_windR",field="np_belongto_parcomsh","rptType=1")
#' re2 <- TS.getFin_rptTS(TS,fun="rptTS.getFin_ts",funchar='"np_belongto_parcomsh",report(46078,RDate)')
#' # -- Following is a speed comparison of three different methods to get financial factorscores:
#' 
#' TS <- Model.TS(setmodelPar.time(modelPar.default(),begT=as.Date("2007-12-01"),endT=as.Date("2014-05-01")))
#' system.time(re.wind <- TS.getFin_rptTS(TS,fun="rptTS.getFin_windR",field="np_belongto_parcomsh","rptType=1")) # 73.69
#' system.time(re.ts_dir <- TS.getFin_ts(TS,funchar='report(46078,RDate)',varname="np_belongto_parcomsh")) # 12.49
#' system.time(re.ts_rpt <- TS.getFin_rptTS(TS,fun="rptTS.getFin_ts",funchar='"np_belongto_parcomsh",report(46078,RDate)')) # 6.29
TS.getFin_rptTS <- function(TS, fun, ...){
  check.TS(TS)  
  TS <- getrptDate_newest(TS,mult="last")
  rptTS <- unique(TS[, c("rptDate","stockID")])
  rptTSF <- do.call(fun, list(rptTS=rptTS, ...))
  TSF <- merge.x(TS, rptTSF, by=c("rptDate","stockID"))
  return(TSF)
}



#' rptTS.getFin
#' 
#' get financtial indicators via \bold{rptTS} through WindR, or TinySoft. 
#' @rdname rptTS.getFin
#' @name rptTS.getFin
#' @aliases rptTS.getFin_windR 
#' @param rptTS a \bold{rptTS} object. a dataframe with cols:"rptDate","stockID"
#' @param field character strting or a vector of character string, giving the windfields. eg.  "OPEN,CLOSE,HIGH" or c("OPEN","CLOSE","HIGH")
#' @param ... other arguments except \code{rptDate} in \code{w.wss} 
#' @return a dataframe with the same length with rptTS, but added by some other financial indicator fields.
#' @export
#' @seealso \code{\link[WindR]{w.wss}}
#' @author Ruifei.Yin
#' @examples
#' TS <- Model.TS(modelPar.univ(indexID="ES09440000"))
#' TS <- getrptDate_newest(TS,mult="last")
#' rptTS <- unique(TS[, c("rptDate","stockID")])
#' # rptTS.getFin_windR
#' re <- rptTS.getFin_windR(rptTS,"np_belongto_parcomsh","rptType=1")
rptTS.getFin_windR <- function(rptTS, field, ...){  
  # field <- 'eps_ttm,eps_diluted,netprofitmargin'
  #browser()
  check.rptTS(rptTS)
  if(!w.isconnected()){
    w.start(showmenu=FALSE)
  }  
  rptTS2 <- transform(rptTS, stockID_wind=stockID2stockID(stockID,from="local",to="wind"), stringsAsFactors=FALSE)
  Dts <- unique(rptTS$rptDate)  
  Dts <- na.omit(Dts)
  df <- data.frame()
  for(Dt in Dts){    
    # Dt <- Dts[1]   
    #browser()
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
rptTS.getFin_ts <- function(rptTS, funchar, ...){
  check.rptTS(rptTS)
  Dts <- unique(rptTS$rptDate) 
  Dts <- na.omit(Dts)
  df <- data.frame()
  for(Dt in Dts){    
    # Dt <- Dts[1]
    #browser()
    Dt <- as.Date(Dt,origin = "1970-01-01")
    codes <- rptTS[rptTS$rptDate==Dt, "stockID", drop=TRUE]
    ts_out <- ts.wss(stocks=codes, funchar=funchar, rptDate=Dt, adjust_yoy=TRUE, ...)     
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






#' TS.getTech
#' @export
TS.getTech <- function(TS, variables, exclude.TS=FALSE,
                       tableName="QT_DailyQuote",
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
    con <- db.local()
    dbWriteTable(con,name="yrf_tmp",value=tmpdat,row.names = FALSE,overwrite = TRUE)
    re <- dbGetQuery(con,qr)
    dbDisconnect(con)
  }  
  re <- dplyr::arrange(re,PK_)[,variables,drop=FALSE]
  re <- cbind(TS,re)    
  
  if(exclude.TS){
    return(re[,variables,drop=FALSE])
  } else {
    return(re)
  }  
}




#' TS.getTech_ts
#'
#' get technical factors throug a tinysoft expression, which could be a simple expresion, e.g 'close()','StockZf2(20)','BBIBOLL_v(11,6)',..,or more complicated expression, e.g. 'close()/open()', 'StockZf2(10)-StockZf2(20)'...
#' @param TS a \bold{TS} object
#' @param funchar a character,the tinysoft function with the arguments. e.g \code{"BBIBOLL_v(11,6)"}
#' @param varname character string, giving the name of the factor
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
TS.getTech_ts <- function(TS,funchar,varname="factorscore"){
  #       funchar <- 'BBIBOLL_v(11,6)'  
  check.TS(TS)
  TS$stockID <- stockID2stockID(TS$stockID,from="local",to="ts")
  tmpfile <- TS
  tmpfile$date <- as.character(tmpfile$date)
  tmpcsv <- tempfile(fileext=".csv")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  fct <- tsRemoteCallFunc('getfactor_tech_general',list(tmpcsv,funchar))
  fct <- plyr::laply(fct,as.array)
  result <- cbind(TS,fct)
  result <- renameCol(result,"fct",varname)
  result$stockID <- stockID2stockID(result$stockID,from="ts",to="local")
  return(result)  
}




#' TS.getFin_ts
#'
#' get financial factors throug a tinysoft expression, which could be simple or more complicated. 
#'
#' Note that the tinysoft function must contain ONLY two system parameters(no more!):pn_stock() and pn_date() ,which is supplyed from the two fields in the TS respectively.Also,the function must contain a expression of 'Rdate:=NewReportDateOfEndT2(sp_time())' to get the newest Rdate of sp_time.
#' @param TS a \bold{TS} object
#' @param funchar character string, giving a tinysoft expression to get the financtial indicators of the stock. The expression can be made simply by replaceing the specified reportdate in the stock-data-expert expression by \code{'Rdate'}. e.g. change \code{Last12MData(20091231,46002)} to \code{Last12MData(Rdate,46002)}.
#' @param varname character string, giving the name of the factor
#' @return A \bold{TSF} object,a dataframe, containing at least cols:\bold{date}(with class of Date),\bold{stockID},\bold{varname},\bold{Rdate} 
#' @note Note that the tinysoft function must contain ONLY two system parameters(no more!):pn_stock() and pn_date() ,which is supplyed from the two fields in the TS respectively. Also,the function must contain a expression of 'Rdate:=NewReportDateOfEndT2(sp_time())' to get the newest Rdate of sp_time.
#' @author Ruifei.Yin
#' @export
#' @examples
#' TS <- getTS(getRebDates(as.Date('2011-03-17'),as.Date('2012-04-17')),'EI000300')
#' TSF <- TS.getFin_ts(TS,"ReportOfAll(9900416,Rdate)")
#' TSF2 <- TS.getFin_ts(TS,"GrowthOfNReport(@@LastQuarterData(DefaultRepID(),9900003,0),Rdate,3,1)")
#' TSF3 <- TS.getFin_ts(TS,"StockAveHsl2(20)+reportofall(9900003,Rdate)")
TS.getFin_ts <- function(TS,funchar,varname="factorscore"){
  #     funchar <- "ReportOfAll(9900416,Rdate)"
  check.TS(TS)
  TS$stockID <- stockID2stockID(TS$stockID,from="local",to="ts")
  tmpfile <- TS
  tmpfile$date <- as.character(tmpfile$date)
  tmpcsv <- tempfile(fileext=".csv")
  write.csv(tmpfile,tmpcsv,row.names=FALSE,quote=FALSE)
  fct <- tsRemoteCallFunc('getfactor_fin_general',list(tmpcsv,funchar))
  fct <- plyr::ldply(fct,as.data.frame)
  result <- cbind(TS,fct)
  result <- renameCol(result,"factorscore",varname)
  result$stockID <- stockID2stockID(result$stockID,from="ts",to="local")
  return(result)  
}


#' getQuote_ts
#' 
#' get Quote series of some stocks in certain period
#' @param stocks a vector of charactor
#' @param begT an object of class "Date"
#' @param endT an object of class "Date"
#' @param variables a vector of charactor,elements of which could be stockID,stockName,date,price,open,high,low,vol,amount,yclose,sectional_yclose,cjbs,...
#' @param cycle a charactor string,eg."cy_day()","cy_30m()","cy_month()",...
#' @param rate a integer,giving the type of rights adjustment, could be one of 0(no adjustment),1(geometric adjustment),2(simple adjustment),3 
#' @param rateday a integer,giving the base date of right adjustment,could be one of 0(the last trading day),-1(the IPO date),or a tinysoft date integer(eg.\code{rdate2ts(as.Date("2010-01-02"))})
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
getQuote_ts <- function(stocks,begT,endT,variables,cycle="cy_day()",rate=0,rateday=0,melt=FALSE,
                        split = if(length(stocks) > splitNbin) TRUE else FALSE,
                        splitNbin = 50){
  #     stocks <- c("SZ002001","SZ002002")
  #     begT <- as.Date("2008-01-01")
  #     endT <- as.Date("2011-02-01")
  #     variables <- c("price","yclose")
  #     cycle="cy_day()"
  #     rate=0
  #     rateday=0     
  subfun <- function(stocks,begT,endT,variables,cycle,rate,rateday,melt){
    stockID <- paste(stocks,collapse=",")
    begT <- rdate2ts(begT)
    endT <- rdate2ts(endT)
    variables <- paste('["',variables,'"]',sep="",collapse=",")  
    qt <- tsRemoteCallFunc("getQuote",list(stockID,begT,endT,variables,cycle,rate,rateday))
    qt <- plyr::ldply(qt,as.data.frame)
    qt$date <- as.POSIXct(qt$date,tz="")
    if(melt){
      qt <- reshape2::melt(qt,id.vars=c("stockID","date"))
    }
    return(qt)
  }
  if(!split){
    re <- subfun(stocks,begT,endT,variables,cycle,rate,rateday,melt)
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
      qt <- subfun(substocks,begT,endT,variables,cycle,rate,rateday,melt)
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
      qt <- read.db.odbc(db.quant(),querychar)
    } else if(datasrc=="local"){      
      qt <- read.db.dbi(db.local(),querychar)
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
    warning(paste('Possibly dealing with a big data. The process is split to ',Ngroup,'groups.'))
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
      qt <- read.db.odbc(db.quant(),querychar)
    } else if(datasrc=="local"){      
      qt <- read.db.dbi(db.local(),querychar)
    }else if(datasrc=="jy"){
      stocks_char <- paste("(",paste(QT(substr(stocks,3,8)),collapse=","),")",sep="")  
      begT <- intdate2r(begT)
      endT <- intdate2r(endT)
      querychar <- paste("SELECT 'EI'+s.SecuCode 'stockID',CONVERT(varchar,TradingDay,112) 'date',",
                  vars," FROM QT_IndexQuote q,SecuMain s
                  where q.InnerCode=s.InnerCode and s.SecuCode in",stocks_char,
                  " and q.TradingDay>=",QT(begT)," and q.TradingDay<=",QT(endT))
      qt <- sqlQuery(db.jy(),querychar)
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
#' @param exclude.SP a logical. If the \code{SP} shoud be inculded in the result?
#' @param datasrc
#' @return a data frame, with cols of SP and "periodrtn". Or a vector if \code{exclude.SP} is TRUE.
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
#' system.time(re <- getPeriodrtn(SP,"nextavg",datasrc="ts")) # 10.80
#' system.time(re1 <- getPeriodrtn(SP,"nextavg",datasrc="local")) # 16.7
#' 
#' #-- with combination of vector stockID, begT and endT
#' getPeriodrtn(stockID="EQ000001",begT=as.Date("2012-01-01"), endT=as.Date("2012-01-01")+c(10,20,30))
#' getPeriodrtn(stockID=c("EQ000001","EQ000002","EQ000004"),begT=as.Date("2012-01-01"), endT=as.Date("2013-01-01"))
getPeriodrtn <- function(SP, stockID, begT, endT,
                         tradeType=c("close","nextavg","nextopen"),exclude.SP=FALSE,
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
  
  if(datasrc=="ts"){
    tmpdat <- transform(SP, stockID = stockID2stockID(stockID,from="local",to="ts"),
                        begT = as.character(begT),
                        endT = as.character(endT))
    tmpcsv <- tempfile(fileext=".csv")
    write.csv(tmpdat,tmpcsv,row.names=FALSE,quote=FALSE,fileEncoding="GB2312")
    periodrtn <- tsRemoteCallFunc("getperiodrtn",list(tmpcsv,tradeType))
    periodrtn <- plyr::laply(periodrtn,as.array)
    re <- cbind(SP, periodrtn)
  } 
  
  if(datasrc %in% c("quant","local")){
    memory.load()  # - load memory data for faster speed
    ov <- defaultDataSRC()    
    options(datasrc="memory")
    
    if(tradeType=="close"){
      tmpdat <- transform(SP[,c("stockID","begT","endT")], begT = trday.nearest(begT), endT = trday.nearest(endT))
      tmpdat <- transform(tmpdat, begT = rdate2int(begT), endT = rdate2int(endT))
      tmpdat$PK_ <- 1:NROW(tmpdat) 
    } else {
      tmpdat <- SP[,c("stockID","begT","endT")]
      tmpdat$PK_ <- 1:NROW(tmpdat)      
      tmpdat <- plyr::ddply(tmpdat,"stockID",
                      function(df){
                        transform(df,
                                  begT=trday.nearby(begT,-1,stockID=df[1,"stockID"]),
                                  endT=trday.nearby(endT,-1,stockID=df[1,"stockID"])) 
                      }
      )      
      tmpdat <- transform(tmpdat, begT = rdate2int(begT), endT = rdate2int(endT))
    }         
    
    if(tradeType=="close"){
      qr <- "select a.*,b.RRClosePrice as P0,c.RRClosePrice as P1
      from yrf_tmp a left join QT_DailyQuote b
      on a.stockID=b.ID and a.begT=b.TradingDay
      left join QT_DailyQuote as c
      on a.stockID=c.ID and a.endT=c.TradingDay"
    } else if(tradeType=="nextopen"){
      qr <- "select a.*,(b.OpenPrice*b.RRFactor) as P0,(c.OpenPrice*c.RRFactor) as P1
      from yrf_tmp a left join QT_DailyQuote b
      on a.stockID=b.ID and a.begT=b.TradingDay
      left join QT_DailyQuote as c
      on a.stockID=c.ID and a.endT=c.TradingDay"
    } else if(tradeType=="nextavg"){
      qr <- "select a.*,(b.TurnoverValue/b.TurnoverVolume*b.RRFactor) as P0,
      (c.TurnoverValue/c.TurnoverVolume*c.RRFactor) as P1
      from yrf_tmp a left join QT_DailyQuote b
      on a.stockID=b.ID and a.begT=b.TradingDay
      left join QT_DailyQuote as c
      on a.stockID=c.ID and a.endT=c.TradingDay"
    }    
    
    if(datasrc=="quant"){
      con <- db.quant()
      sqlDrop(con,sqtable="yrf_tmp",errors=FALSE)
      sqlSave(con,dat=tmpdat,tablename="yrf_tmp",safer=FALSE,rownames=FALSE)    
      re <- sqlQuery(con,query=qr)
      odbcClose(con)
    } else if (datasrc=="local"){
      con <- db.local()
      dbWriteTable(con,name="yrf_tmp",value=tmpdat,row.names = FALSE,overwrite = TRUE)
      re <- dbGetQuery(con,qr)
      dbDisconnect(con)
    }  
    re <- dplyr::arrange(re,PK_)[,c("P0","P1")]
    re <- re$P1/re$P0-1
    re <- cbind(SP,periodrtn=re)
    options(datasrc=ov)  
  }  
  if(exclude.SP){
    return(re[,"periodrtn"])
  } else {
    return(re)
  }  
}





