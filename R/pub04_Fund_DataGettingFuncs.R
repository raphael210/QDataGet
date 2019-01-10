

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===============    Mutual Fund related      =========
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


# ---------  ************ Andrew's ************    -----------

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
#' #---MF_basicinfo-------------------------------------------------------
#' #get fund's fundname and setupday
#' re <- MF_basicinfo(fundID)
#' #get setupday only
#' re <- MF_basicinfo(fundID,info='setupday')
#' 
#' #---MF_rtn_table----------------------------------------------------------
#' # -- get one fund's nav stats from set up date.
#' mfstat <- MF_rtn_table(fundID='100038.OF')
#' # -- get one fund's yearly nav stats from set up date.
#' mfstat <- MF_rtn_table(fundID='100038.OF',freq='year')
#' # -- get mutiple funds' nav stats from specified date.
#' fundID <- c('162411.OF','501018.OF')
#' begT <- as.Date('2016-01-04')
#' endT <- as.Date('2017-12-31')
#' mfstat <- MF_rtn_table(fundID=fundID,begT=begT,endT=endT)
#' # -- define benchmark by yourself.
#' fundID <- c('000978.OF','000877.OF')
#' bmk <- c('EI000905','EI000300')
#' mfstat <- MF_rtn_table(fundID=fundID,bmk=bmk)
NULL



#' \code{MF_getQuote} get fund's quote from multiple data source.
#' 
#' @rdname MF_funcs
#' @param variables see examples.
#' @export
MF_getQuote <- function(fundID,begT,endT,variables="NAV_adj_return1",datasrc = c("wind","jy","ts"),NAfill=FALSE){
  datasrc <- match.arg(datasrc)
  
  if(missing(begT)){
    begT <- MF_basicinfo(fundID,info='setupday',datasrc = datasrc)
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


#' \code{MF_rtn_table} get fund's nav stats.
#' 
#' @rdname MF_funcs
#' @param fundID is vector of fundID.
#' @param begT is a vector of begin date,can be missing,if missing,then fund's set up date will be begT
#' @param endT is end date,can be missing,if missing,then the nearest trading day for \bold{today} will be endT.
#' @param freq is fund's nav statistic frequency,default value is \code{NULL},same as \code{\link[QUtility]{rtn.periods}}.
#' @param scale is number of periods in a year,for daily return default value is 250,for monthly return default value is 12.
#' @export
MF_rtn_table <- function(fundID,begT,endT,bmk=NULL,freq=NULL,scale=250,datasrc=c('jy','wind')){
  datasrc <- match.arg(datasrc)
  fundnav <- MF_rtn_get(fundID,begT,endT,bmk,datasrc)
  
  #stats inner function
  navstats_innerfunc <- function(fnav,scale){
    fundstat <- fnav %>% dplyr::group_by(fundID,fundName) %>%
      dplyr::summarise(nyear=as.numeric(max(date)-min(date))/365,
                       rtn=prod(1+nav_rtn)-1,rtn_ann=(1+rtn)^(1/nyear)-1,
                       bench=prod(1+bmk_rtn)-1,bench_ann=(1+bench)^(1/nyear)-1,
                       alpha=rtn-bench,alpha_ann=rtn_ann-bench_ann,
                       bias=mean(abs(exc_rtn)),
                       TE=sqrt(sum(exc_rtn^2)/(n()-1)) * sqrt(scale),
                       alphaIR=alpha_ann/TE) %>% dplyr::ungroup()
    
    # #get two dates
    # sdate <- fnav %>% dplyr::select(date,fundID,exc_rtn) %>%
    #   dplyr::left_join(fundstat[,c('fundID','rtn_min','rtn_max')],by='fundID')
    # sdate_ <- sdate %>% dplyr::filter(exc_rtn==rtn_max) %>% dplyr::group_by(fundID) %>%
    #   dplyr::summarise(rtn_maxdate=min(date)) %>% dplyr::ungroup()
    # sdate <- sdate %>% dplyr::filter(exc_rtn==rtn_min) %>% dplyr::group_by(fundID) %>%
    #   dplyr::summarise(rtn_mindate=min(date)) %>% dplyr::ungroup() %>% dplyr::left_join(sdate_,by='fundID')
    # fundstat <- fundstat %>% dplyr::left_join(sdate,by='fundID')
    
    #get max drawdown
    maxDD <- split(fnav,fnav$fundID)
    maxDD_hitratio <- function(df,varname='exc_rtn'){
      df <- reshape2::dcast(df,date~fundID,value.var = varname,fill = 0)
      df <- xts::xts(df[,-1],order.by = df[,1])
      dd <- PerformanceAnalytics::table.Drawdowns(df,1)
      dd <- dd[,c("Depth","From","Trough")]
      colnames(dd) <- c('alphamaxDD','maxDDbegT','maxDDendT')
      if(periodicity_Ndays(df)<28){
        hit <- rtn.periods(df,'month')
        hit <- hit[1:(nrow(hit)-2),]
        hratio <- length(hit[hit>0])/length(hit)
      }else{
        hratio <- hitRatio(df)
      }
      hitratio <- data.frame(hitratio=hratio,row.names = NULL)
      re <- cbind(hitratio,dd)
      return(re)
    }
    maxDD <- plyr::ldply(maxDD,maxDD_hitratio,.id = 'fundID')
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



#' \code{MF_rtn_plot} get funds' hedge nav plot
#' 
#' @rdname MF_funcs
#' @export
#' @examples
#' #---MF_navrtn_plot----------------------------------------------------------
#' #all hs300 enchanced funds
#' fundID <- c("000478.OF","002311.OF","002510.OF","002906.OF","003986.OF","161017.OF")
#' begT <- as.Date('2018-01-02')
#' bmk <- 'EI000905'
#' MF_rtn_plot(fundID,begT,bmk=bmk)
#' # set begT as setupdate+
#' begT <- MF_basicinfo(fundID,info = 'setupday')+months(1)
#' MF_rtn_plot(fundID,begT,bmk=bmk)
#' # hs300 and zz500 enhanced funds
#' fundID <- c("000478.OF","002311.OF","310318.OF","002906.OF","003986.OF","161017.OF","100038.OF","163407.OF")
#' MF_rtn_plot(fundID) #bmk can be missing
MF_rtn_plot <- function(fundID,begT,endT,bmk=NULL,datasrc=c('jy','wind'),
                        plottype=c('all','excess'),
                        wrap_sacles=c("free","free_x","free_y","fixed")){
  datasrc <- match.arg(datasrc)
  plottype <- match.arg(plottype)
  wrap_sacles <- match.arg(wrap_sacles)
  
  fundnav <- MF_rtn_get(fundID,begT,endT,bmk,datasrc)
  
  fundnav <- fundnav %>% dplyr::select(-fundID) %>% 
    tidyr::gather(key = "rtn_type", value = "rtn",-date,-fundName) %>% 
    dplyr::arrange(fundName,rtn_type,date) %>%
    dplyr::group_by(fundName,rtn_type) %>%
    dplyr::mutate(nav=cumprod(1+rtn)) %>%
    dplyr::ungroup()
  
  suppressPackageStartupMessages(require(ggplot2))
  if(plottype=='excess'){
    fundnav <- fundnav %>% filter(rtn_type=='exc_rtn')
  }
  re <- ggplot(fundnav,aes(x=date,y=nav,color=rtn_type))+geom_line()
  if(length(fundID)>1){
    re <- re + facet_wrap(~fundName,scales = wrap_sacles)
  }
  return(re)
}

#' \code{MF_rtn_get} get fund's nav return
#' 
#' @rdname MF_funcs
#' @export
MF_rtn_get <- function(fundID,begT,endT,bmk=NULL,datasrc=c('jy','wind')){
  datasrc <- match.arg(datasrc)
  
  #get begT and endT
  f_info <- MF_basicinfo(fundID,datasrc = datasrc)
  if(missing(endT)){
    endT <- trday.nearby(Sys.Date(),-1)
  }
  
  if(missing(begT)){
    f_info <- f_info %>% dplyr::mutate(begT=setupday)
  }else{
    f_info <- f_info %>% dplyr::mutate(begT=begT) %>% dplyr::mutate(begT=ifelse(begT<setupday,setupday,begT)) %>% 
      dplyr::mutate(begT=as.Date(begT,origin = "1970-01-01"))
  }
  f_info <- f_info %>% mutate(endT=endT)
  
  #get benchmark
  if(!is.null(bmk)){
    f_info <- f_info %>% dplyr::mutate(benchindex=bmk)
  }
  bmkindex <- na.omit(unique(f_info$benchindex))
  
  #get fund's nav return
  bmkqt <- data.frame(stringsAsFactors = FALSE)
  if(datasrc=='wind'){
    fundnav <- MF_getQuote(fundID = f_info$fundID, begT = f_info$begT, endT = f_info$endT, variables = "NAV_adj_return1",datasrc = 'wind',NAfill = TRUE)
    fundnav <- fundnav %>% dplyr::rename(nav_rtn=NAV_ADJ_RETURN1) %>% dplyr::mutate(nav_rtn=nav_rtn/100)
    
    if(length(bmkindex)>0){
      for(i in 1:length(bmkindex)){
        indexID_ <- bmkindex[i]
        if(substr(indexID_,1,2)=='EI'){
          indexID_ <- stockID2stockID(indexID_,from = 'local',to = 'wind')
        }
        bmkqt_ <- w.wsd(indexID_,"pct_chg",min(f_info$begT),max(f_info$endT))[[2]]
        bmkqt_ <- bmkqt_ %>% dplyr::rename(date=DATETIME,bmk_rtn=PCT_CHG) %>% 
          dplyr::mutate(benchindex=bmkindex[i],bmk_rtn=bmk_rtn/100) %>% dplyr::select(date,benchindex,bmk_rtn)
        bmkqt <- rbind(bmkqt,bmkqt_)
      }
    }
    
  }else if(datasrc=='jy'){
    fundnav <- MF_getQuote(fundID = f_info$fundID, begT = f_info$begT, endT = f_info$endT, variables = "NVDailyGrowthRate",datasrc = 'jy',NAfill = TRUE)
    fundnav <- fundnav %>% dplyr::rename(nav_rtn=NVDailyGrowthRate)
    
    if(length(bmkindex)>0){
      bmkqt <- getIndexQuote(bmkindex,begT = min(f_info$begT),endT=max(f_info$endT),variables = 'pct_chg',datasrc = 'jy')
      if(nrow(bmkqt)>0){
        bmkqt <- bmkqt %>% dplyr::rename(benchindex=stockID,bmk_rtn=pct_chg) %>% 
          dplyr::mutate(benchindex=as.character(benchindex)) %>% dplyr::select(date,benchindex,bmk_rtn)
      }
      
    }
  }
  
  f_info <- f_info %>% dplyr::select(fundID,fundName,benchindex)
  fundnav <- fundnav %>% dplyr::left_join(f_info,by='fundID')
  if(nrow(bmkqt)>0){
    fundnav <- fundnav %>% dplyr::left_join(bmkqt,by=c('date','benchindex')) %>% 
      mutate(bmk_rtn=ifelse(is.na(bmk_rtn),0,bmk_rtn),exc_rtn=nav_rtn-bmk_rtn)
  }else{
    fundnav <- fundnav %>% mutate(bmk_rtn=0,exc_rtn=nav_rtn-bmk_rtn)
  }
  fundnav <- fundnav %>% dplyr::select(date,fundID,fundName,nav_rtn,bmk_rtn,exc_rtn)
  fundnav <- as.data.frame(fundnav)
  return(fundnav)
}



#' \code{MF_basicinfo} get fund's basic info
#' 
#' @rdname MF_funcs
#' @export
MF_basicinfo <- function(fundID,info=c('fundName','setupday','benchindex'),datasrc = c('jy','wind')){
  datasrc <- match.arg(datasrc)
  if(datasrc=='wind'){
    require(WindR)
    w.start(showmenu = FALSE)
    fundinfo <- w.wss(fundID,'sec_name,fund_setupdate,fund_trackindexcode')[[2]]
    fundinfo <- fundinfo %>%
      dplyr::mutate(FUND_SETUPDATE=w.asDateTime(FUND_SETUPDATE,asdate = TRUE)) %>%
      dplyr::rename(fundID=CODE,fundName=SEC_NAME,setupday=FUND_SETUPDATE,benchindex=FUND_TRACKINDEXCODE)
  }else if(datasrc=='jy'){
    qr <- paste("select s.SecuCode+'.OF' 'fundID',s.SecuAbbr 'fundName',
                convert(varchar,f.EstablishmentDate,112) 'setupday','EI'+s2.SecuCode 'benchindex'
                from MF_FundArchives f
                inner join SecuMain s on f.InnerCode=s.InnerCode and s.SecuCode in ",brkQT(stringr::str_replace_all(fundID,'.OF','')),"
                left join MF_InvestTargetCriterion mf 
                on f.InnerCode=mf.InnerCode and mf.IfExecuted=1 and mf.TracedIndexCode is not null and mf.MinimumInvestRatio>=0.8 and mf.InvestTarget=90
                left join SecuMain s2 on mf.TracedIndexCode=s2.InnerCode")
    fundinfo <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors = FALSE)
    fundinfo <- transform(fundinfo,setupday=intdate2r(setupday))
  }
  
  funddf <- data.frame(fundID=fundID,stringsAsFactors = FALSE)
  re <- funddf %>% dplyr::left_join(fundinfo,by='fundID')
  if(length(info)==1){
    if(info=='fundName'){
      re <- re$fundName
    }else if(info=='setupday'){
      re <- re$setupday
    }else if(info=='benchindex'){
      re <- re$benchindex
    }
    
  }
  return(re)
}








#' MF_getTF
#'
#' \code{\bold{MF_getTF}} get mutual funds in RebDates.
#' @rdname mutualfund_func
#' @param fundtype \code{equity} means quity fund,\code{aggr_allo} means aggressive allocation fund.
#' @export
#' @examples
#' RebDates <- getRebDates(as.Date('2010-01-01'),as.Date('2018-06-30'),'quarter')
#' TF <- MF_getTF(RebDates) #get all quity funds
#' TF <- MF_getTF(RebDates,fundtype='aggr_allo') #get all aggressive allocation funds
MF_getTF <- function(RebDates,fundtype=c('equity','aggr_allo','index','indexenhance'),rm=c('namewithC','initperiod'),N=90){
  fundtype <- match.arg(fundtype)
  
  qr <- "select s.SecuCode+'.OF' 'fundID',s.SecuAbbr 'fundName',
  convert(varchar,f.EstablishmentDate,112) 'setupday',
  convert(varchar,f.ExpireDate,112) 'ExpireDate',
  f.Type,c1.MS 'TypeCN',
  f.InvestmentType,c2.MS 'InvestmentTypeCN',
  f.InvestStyle,c3.MS 'InvestStyleCN',
  f.FundTypeCode,c4.MS 'FundTypeCodeCN'
  from MF_FundArchives f
  inner join SecuMain s on f.InnerCode=s.InnerCode
  INNER join CT_SystemConst c1 on f.Type=c1.DM and c1.DM in(1,2,3,4,6,7,8) and c1.LB=1210
  INNER join CT_SystemConst c2 on f.InvestmentType=c2.DM and c2.LB=1094
  INNER join CT_SystemConst c3 on f.InvestStyle=c3.DM and c3.LB=1093
  INNER join CT_SystemConst c4 on f.FundTypeCode=c4.DM and c4.LB=1249 and c4.DM IN(1101,1103)
  where f.EstablishmentDate is not NULL and f.FundNature=1
  order by fundID"
  fundinfo <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors = FALSE)
  fundinfo <- data.table::as.data.table(fundinfo)
  if(fundtype=='equity'){
    fundsub <- fundinfo[FundTypeCode==1101][!(InvestmentType %in% c(7,8))][!(Type %in% c(1,6,7))][,.(fundID,fundName,setupday,ExpireDate)]
  }else if(fundtype=='aggr_allo'){
    fundsub <- fundinfo[FundTypeCode==1103][!(InvestmentType %in% c(7,8))][!(Type %in% c(1,6,7))][InvestStyle==10][,.(fundID,fundName,setupday,ExpireDate)]
  }else if(fundtype=='index'){
    fundsub <- fundinfo[InvestmentType==7 & Type!=6,.(fundID,fundName,setupday,ExpireDate)]
  }else if(fundtype=='indexenhance'){
    fundsub <- fundinfo[InvestmentType==8 & Type!=6,.(fundID,fundName,setupday,ExpireDate)]
  }
  fundsub[is.na(ExpireDate),ExpireDate:=99990101][,`:=`(setupday=intdate2r(setupday),
                                                        ExpireDate=intdate2r(ExpireDate))]
  
  RebDatesdt <- data.table::data.table(date=RebDates)
  re <- fundsub[RebDatesdt,.(i.date,x.fundID,x.fundName,x.setupday,x.ExpireDate),on=.(setupday<=date,ExpireDate>date),allow.cartesian=TRUE]
  data.table::setnames(re,c("date","fundID","fundName","setupday","ExpireDate"))
  data.table::setorder(re, date,setupday)
  
  if('initperiod' %in% rm){
    re <- re[(date-setupday)>=N]
  }
  re <- as.data.frame(re)
  
  if('namewithC' %in% rm){
    re <- MF_TF_reshape(re,type='rm_NameC')
  }
  re <- MF_TF_reshape(re,type='rp_IDJ')
  re <- re[,c("date","fundID","fundName")]
  return(re)
}



#' MF_TF_reshape
#'
#' \code{\bold{MF_TF_reshape}} reshape a \code{TF} object.
#' @param type \code{rp_IDJ} means replace fundID's last four characters which is 'J.OF', \code{rm_NameC} means remove fundName last character which is C, \code{rm_quantfund} means remove quantative funds.
#' @rdname mutualfund_func
#' @export
MF_TF_reshape <- function(TF,type=c('rp_IDJ','rm_NameC','rm_quantfund')){
  type <- match.arg(type)
  
  if(type=='rp_IDJ'){
    TF$fundID <- stringr::str_replace(TF$fundID,'J.OF','.OF')
  }else if(type=='rm_NameC'){
    TF <- TF %>% dplyr::mutate(lastL=stringr::str_sub(fundName,-1,-1),torm=(lastL %in% LETTERS) & lastL!='A') %>%
      dplyr::filter(!torm) %>%
      dplyr::select(-lastL,-torm) %>%
      as.data.frame()
  }else if(type=='rm_quantfund'){
    alphaf <- TF_info_fromwind(endT = max(TF$date),fundtype = 'alpha',addsize = FALSE)
    TF <- TF %>% dplyr::filter(!(fundID %in% alphaf$fundID)) %>% as.data.frame()
  }
  
  return(TF)
}


#' MF_CT_nonalphafund
#'
#' \code{\bold{MF_CT_nonalphafund}} export const non-alpha fundIDs.
#' @rdname mutualfund_func
#' @export
MF_CT_nonalphafund <- function(mfadd=NULL,mfrm=NULL){
  fundID <- c('377010.OF','519712.OF','000082.OF','002562.OF','630005.OF',
              '519212.OF','165531.OF','165532.OF','168501.OF','165532.OF',
              '005244.OF','501046.OF','165531.OF','001907.OF','004674.OF')
  if(!is.null(mfadd)){
    fundID <- union(fundID,mfadd)
  }
  if(!is.null(mfrm)){
    fundID <- setdiff(fundID,mfrm)
  }
  return(fundID)
}


#' MF_getAssetPort
#'
#' @rdname mutualfund_func
#' @export
MF_getAssetPort <- function(fundID,rptDate,datasrc="jy",TF){
  
  if(datasrc == "jy"){
    if(missing(TF)){
      fundIDqr <- brkQT(substr(fundID,1,6))
    }else{
      fundIDqr <- brkQT(substr(unique(TF$fundID),1,6))
    }
    
    qr <- paste("select B.SecuCode+'.OF' 'fundID',
                convert(varchar(8),A.ReportDate,112) 'rptDate',
                convert(varchar(8),A.InfoPublDate,112) 'pubDate',
                A.RatioInNV 'wgt'
                from MF_AssetAllocation A
                left join SecuMain B on A.InnerCode = B.InnerCode
                where B.SecuCode in ",fundIDqr," and A.AssetTypeCode='10020'
                order by fundID,rptDate")
    tmpdat <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
    tmpdat <- transform(tmpdat,rptDate=intdate2r(rptDate),
                        pubDate=intdate2r(pubDate))
    
    if(missing(TF)){
      if(!missing(rptDate)){
        tmpdat <- tmpdat[tmpdat$rptDate %in% rptDate,]
      }
    }else{
      tmpdat <- data.table::as.data.table(tmpdat)
      data.table::setnames(tmpdat,'pubDate','date')
      TF <- data.table::as.data.table(TF)
      
      tmpdat <- tmpdat[TF,.(i.date,i.fundID,x.rptDate,x.date,x.wgt),on=.(fundID,date),roll=TRUE]
      tmpdat <- as.data.frame(tmpdat)
      colnames(tmpdat) <- c("date","fundID","rptDate","pubDate","wgt")
    }
  }
  return(tmpdat)
}



#' MF_getTFSW
#'
#' \code{\bold{MF_getTFSW}} get TF with stockID and wgt
#' @rdname mutualfund_func
#' @param fill default value is \code{TRUE},if \code{TRUE} then fill quarter data with nearest half year report.
#' @export
#' @examples
#' ################~~MF_getTFSW demo~~###################################
#' TFSW <- MF_getTFSW(TF)
#' TFSW <- MF_getTFSW(TF,fill=FALSE)
MF_getTFSW <- function(TF,fill=TRUE){
  
  #get stock portfolio from reports
  fundIDs <- unique(TF$fundID)
  rptDates <- rptDate.nearest(unique(TF$date))
  rptDates <- c(rptDate.offset(min(rptDates),-2:-1,'q'),rptDates)# add two report date before
  qtrstock <- MF_getStockPort(fundIDs,rptDates,mode = 'top10') #quarterly report
  yearstock <- MF_getStockPort(fundIDs,rptDates,mode = 'all') #yearly and half yearly report
  
  #rbind two types data
  allstock <- rbind(data.frame(qtrstock,rptType='quarter',stringsAsFactors = FALSE),
                    data.frame(yearstock,rptType='year',stringsAsFactors = FALSE))
  allstock <- data.table::as.data.table(allstock)
  
  #deal with wrong data
  allstock <- allstock[pubDate>rptDate]
  
  #merge TF with report dates
  rptdt <- unique(allstock[,.(rptType,fundID,rptDate,pubDate)])
  data.table::setnames(rptdt,"pubDate","date")
  
  ## part1 deal with quarterly report
  TF <- data.table::as.data.table(TF)
  TFrpt <- rptdt[rptType=='quarter'][TF,.(i.date,i.fundID,x.rptDate,x.date),on=.(fundID,date),roll=TRUE,nomatch=0]
  colnames(TFrpt) <- c( "date","fundID","rptDate_Q","pubDate_Q")
  
  ## part2 deal with yearly report
  TFrpt <- rptdt[rptType=='year'][TFrpt,.(i.date,i.fundID,i.rptDate_Q,i.pubDate_Q,x.rptDate,x.date),on=.(fundID,date),roll=TRUE]
  colnames(TFrpt) <- c( "date","fundID","rptDate_Q","pubDate_Q","rptDate_Y","pubDate_Y")
  
  # join with TF
  TFrpt[,rptType:=ifelse(!is.na(rptDate_Y) & rptDate_Y==rptDate_Q,'year','quarter')]
  
  TFrpt[,`:=`(rptDate=ifelse(rptType=='quarter',rptDate_Q,rptDate_Y),
              pubDate=ifelse(rptType=='quarter',pubDate_Q,pubDate_Y))][,`:=`(rptDate=as.Date(rptDate,origin='1970-01-01'),
                                                                             pubDate=as.Date(pubDate,origin='1970-01-01'))]
  
  TFSWspan <- TFrpt[allstock,on=.(fundID,rptDate,pubDate,rptType),nomatch=0]
  TFSWspan[,wgt:=round(wgt,4)]
  if(fill){
    
    #to fill
    TFSW_2 <- TFSWspan[rptType=='quarter' & !is.na(rptDate_Y)]
    
    #get CSRC sector ID
    rptTS <- unique(TFSW_2[,.(rptDate,pubDate,stockID)])
    data.table::setnames(rptTS,'pubDate','date')
    data.table::setorder(rptTS,rptDate,date,stockID)
    rptTSS <- getCSRCsector(rptTS)
    data.table::setnames(rptTSS,'date','pubDate')
    
    TFSW_2 <- rptTSS[TFSW_2,on=.(rptDate,pubDate,stockID)]
    TFsecwgttop10 <- TFSW_2[,.(top10wgt=sum(wgt)),by=.(date,fundID,rptDate,pubDate,secCode,secName)]
    
    # fund's sector wgt
    TFsecwgt <- MF_getindustry(fundIDs,rptDates)  # fund's CSRC sector wgt from reports
    TFsecwgt <- data.table::as.data.table(TFsecwgt)
    TFsecwgt[,Standard:=NULL]
    
    #non top 10 stocks sector wgt
    TFsecwgt <- TFsecwgttop10[TFsecwgt,on=.(fundID,rptDate,pubDate,secCode,secName)]
    data.table::setorder(TFsecwgt,fundID,rptDate,date,na.last = TRUE)
    TFsecwgt[, date:=zoo::na.locf(date, na.rm = FALSE), by=c("fundID","rptDate")][,top10wgt:=ifelse(is.na(top10wgt),0,top10wgt)]
    TFsecwgt <- TFsecwgt[!is.na(date) & !is.na(wgt)]
    TFsecwgt <- TFsecwgt[,reswgt:=wgt-top10wgt][reswgt>0.01][,.(date,fundID,rptDate,pubDate,secCode,secName,reswgt)]
    
    #fill stock from yearly report
    yearstock <- data.table::as.data.table(yearstock)
    data.table::setnames(yearstock,c("rptDate","pubDate"),c("rptDate_Y","pubDate_Y"))
    
    resstock <- unique(TFsecwgt[,.(date,fundID,rptDate,pubDate)])
    resstock <- TFrpt[,.(fundID,rptDate_Y,pubDate_Y,rptDate,pubDate)][resstock,on=.(fundID,rptDate,pubDate),nomatch=0]
    resstock <- resstock[yearstock,on=.(fundID,rptDate_Y,pubDate_Y),nomatch=0,allow.cartesian=TRUE]
    resstock <- resstock[,ran:=order(-wgt),by=.(date,fundID,rptDate)][ran>10 & wgt>0][,ran:=NULL]
    
    rptTS <- distinct(resstock[,.(pubDate_Y,rptDate_Y,stockID)])
    colnames(rptTS) <- c('date','rptDate','stockID')
    rptTSS <- getCSRCsector(rptTS)
    rptTSS[,Standard:=NULL]
    colnames(rptTSS) <- c("rptDate_Y","pubDate_Y","stockID","secCode","secName")
    resstock <- rptTSS[resstock,on=.(rptDate_Y,pubDate_Y,stockID)]
    resstock[,secwgt:=sum(wgt),by=c('date','fundID','rptDate','secCode','secName')]
    resstock <- TFsecwgt[resstock,on=.(date,fundID,rptDate,pubDate,secCode,secName),nomatch=0]
    resstock[,`:=`(wgt=round(wgt*reswgt/secwgt,4),
                   filltag=TRUE,
                   rptType='quarter')]
    resstock <- resstock[,.(date,fundID,rptDate,pubDate,rptType,stockID,wgt,filltag)]
    
    TFSW <- TFSWspan[,.(date,fundID,rptDate,pubDate,rptType,stockID,wgt)][,filltag:=FALSE]
    TFSW <- rbind(TFSW,resstock)
    TFSW <- TFSW[order(date,fundID,rptDate,-wgt)]
    TFSW <- as.data.frame(TFSW)
  }else{
    TFSW <- as.data.frame(TFSWspan[,.(date,fundID,rptDate,pubDate,rptType,stockID,wgt)])
  }
  
  return(TFSW)
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
    
    if(mode == "all"){
      sheetname <- "MF_StockPortfolioDetail"
    }else if(mode == "top10"){
      sheetname <- "MF_KeyStockPortfolio"
    }
    
    fundIDqr <- brkQT(substr(fundID,1,6))
    
    if(!missing(rptDate)){
      rptDateqr <-paste(" and A.ReportDate in",brkQT(rptDate))
    }else{
      rptDateqr <- " "
    }
    
    qr <- paste("select B.SecuCode+'.OF' 'fundID',
                 convert(varchar(8),A.ReportDate,112) 'rptDate',
                 convert(varchar(8),A.InfoPublDate,112) 'pubDate',
                 'EQ'+C.SecuCode 'stockID',
                 A.RatioInNV 'wgt'
                 from ",sheetname," A
                 left join SecuMain B on A.InnerCode = B.InnerCode
                 inner join SecuMain C on A.StockInnerCode = C.InnerCode and C.SecuCode is not NULL and C.SecuCode not LIKE '99%'
                 where A.RatioInNV is not NULL and B.SecuCode in ",fundIDqr,rptDateqr)
    tmpdat <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
    tmpdat <- tmpdat %>% dplyr::mutate(rptDate=intdate2r(rptDate),pubDate=intdate2r(pubDate)) %>% 
      dplyr::arrange(fundID,rptDate,desc(wgt))
    
    #fill NA with non NA value
    if(any(is.na(tmpdat$pubDate))){
      tmpdat <- tmpdat %>% dplyr::group_by(fundID,rptDate) %>% 
        dplyr::mutate(pubDate=ifelse(is.na(pubDate),max(pubDate,na.rm = TRUE),pubDate)) %>% 
        dplyr::ungroup() %>% 
        filter(!is.infinite(pubDate)) %>% 
        mutate(pubDate=as.Date(pubDate,origin="1970-01-01")) %>% 
        as.data.frame()
    }
    
    #deal with one report date have two or more pubdates
    nprtday <-  tmpdat %>% group_by(fundID,rptDate,pubDate) %>% summarise(nstock=n()) %>% ungroup() %>% 
      group_by(fundID,rptDate) %>% mutate(nrpt=n())
    if(any(nprtday$nrpt>1)){
      nprtday <- nprtday %>% filter(nstock==max(nstock)) %>% ungroup() %>% select(fundID,rptDate,pubDate)
      tmpdat <- tmpdat %>% mutate(pubDate=NULL) %>% left_join(nprtday,by=c('fundID','rptDate')) %>% 
        select(fundID,rptDate,pubDate,stockID,wgt) %>% as.data.frame()
    }
  }
  return(tmpdat)
}


#' \code{MF_getStockPort} get fund's industry allocation from financial reports.
#' 
#' @rdname MF_funcs
#' @export
MF_getindustry <- function(fundID,rptDate,datasrc = c("jy","ts","wind")){
  #variables=c("date","stockID","wgt")
  datasrc <- match.arg(datasrc)
  if(datasrc == "jy"){
    fundIDqr <- brkQT(substr(fundID,1,6))
    
    if(!missing(rptDate)){
      rptDateqr <-paste(" and A.ReportDate in",brkQT(rptDate))
    }else{
      rptDateqr <- " "
    }
    
    qr <- paste("select B.SecuCode+'.OF' 'fundID',
                 convert(varchar(8),A.ReportDate,112) 'rptDate',
                 convert(varchar(8),A.InfoPublDate,112) 'pubDate',
                 A.InduStandard 'Standard',A.InduDiscCode 'secCode',A.IndustryName 'secName',A.RatioInNV 'wgt'
                 from MF_InvestIndustry A
                 left join SecuMain B on A.InnerCode = B.InnerCode
                 where B.SecuCode in ",fundIDqr,rptDateqr," order by fundID,rptDate,IndustryName")
    tmpdat <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
    tmpdat <- tmpdat %>% dplyr::mutate(rptDate=intdate2r(rptDate),pubDate=intdate2r(pubDate)) %>% 
      dplyr::arrange(fundID,rptDate,desc(wgt)) %>% dplyr::filter(!is.na(secCode))
  }
  return(tmpdat)
}



getCSRCsector <- function(rptTS){
  qr <- "select 'EQ'+b.SecuCode 'stockID',
  a.Standard,a.FirstIndustryCode 'secCode',a.FirstIndustryName 'secName',a.IfPerformed,
  convert(varchar(8),a.InfoPublDate,112) 'pubDate',
  convert(varchar(8),a.CancelDate,112) 'canDate'
  from LC_ExgIndustry a
  inner join SecuMain b on a.CompanyCode=b.CompanyCode and b.SecuCategory=1 and b.ListedSector in(1,2,6)
  inner join CT_SystemConst c on c.LB=1081 and a.Standard=c.DM
  where a.Standard in(1,22)
  order by b.SecuCode,a.InfoPublDate"
  re <- queryAndClose.odbc(db.jy(),qr,stringsAsFactors=FALSE)
  re <- data.table::as.data.table(re)
  re[,canDate:=ifelse(is.na(canDate),99990101,canDate)][,`:=`(canDate=intdate2r(canDate),
                                                              pubDate=intdate2r(pubDate))]
  
  rptTS <- rptTS[,Standard:=ifelse(rptDate<=as.Date('2012-12-31'),1,22)]
  rptTS <- re[rptTS,.(i.rptDate,i.date,i.stockID,x.Standard,x.secCode,x.secName),on=.(Standard,stockID,pubDate<=date,canDate>date)]
  colnames(rptTS) <- c("rptDate","date","stockID","Standard","secCode","secName" )
  return(rptTS)
}

TF_info_fromwind <- function(endT=Sys.Date(),fundtype=c('index','enhanced','alpha','hedge'),addsize=TRUE){
  fundtype <- match.arg(fundtype)
  
  if(!WindR::w.isconnected()){
    require(WindR)
    WindR::w.start(showmenu = FALSE)
  }
  if(fundtype=='index'){
    secID <- '2001010102000000'
  }else if(fundtype=='enhanced'){
    secID <- '2001010103000000'
  }else if(fundtype=='alpha'){
    secID <- '1000023322000000'
  }else if(fundtype=='hedge'){
    secID <- '1000010420000000'
  }
  
  mf <- WindR::w.wset('sectorconstituent',date=max(endT),sectorid=secID)[[2]]
  mf <- mf %>% dplyr::mutate(CODE=NULL,date=NULL) %>%
    dplyr::rename(fundID=wind_code,fundName=sec_name)
  mf <- MF_TF_reshape(mf,type='rm_NameC')
  
  addinfo <- WindR::w.wss(mf$fundID,'fund_setupdate,fund_trackindexcode,fund_corp_fundmanagementcompany',tradeDate=max(endT))[[2]]
  mf <- addinfo %>% dplyr::rename(fundID=CODE,setupDate=FUND_SETUPDATE,indexID=FUND_TRACKINDEXCODE,company=FUND_CORP_FUNDMANAGEMENTCOMPANY) %>%
    dplyr::filter(setupDate>0) %>%
    dplyr::mutate(setupDate=w.asDateTime(setupDate,asdate = TRUE),indexID=ifelse(indexID=='NaN',NA,indexID)) %>%
    dplyr::right_join(mf,by='fundID')
  
  mf <- data.table::as.data.table(mf)
  endTdt <- data.table::data.table(date=endT)
  re <- mf[endTdt,.(i.date,x.fundID,x.fundName,x.setupDate,x.indexID,x.company),on=.(setupDate<=date),allow.cartesian=TRUE]
  data.table::setnames(re,c("date","fundID","fundName","setupDate","indexID","company"))
  data.table::setorder(re, date,setupDate)
  
  if(fundtype=='alpha'){
    re <- re[!(fundID %in% MF_CT_nonalphafund())]
  }
  re <- as.data.frame(re)
  
  if(addsize){
    dates <- unique(re$date)
    size <- data.frame(stringsAsFactors = FALSE)
    for(i in 1:length(dates)){
      addinfo <- WindR::w.wss(re[re$date==dates[i],'fundID'],'netasset_total','unit=1',tradeDate=dates[i])[[2]]
      addinfo <- addinfo %>% dplyr::rename(fundID=CODE,Asset=NETASSET_TOTAL) %>%
        dplyr::mutate(date=dates[i],Asset=Asset/1e8)
      size <- rbind(size,addinfo)
    }
    re <- re %>% dplyr::left_join(size,by=c('date','fundID'))
  }
  return(re)
}






# ---------  ************ Mazi's ************    -----------


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
