
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================
# ====================  Tinysoft Connection functions ====================
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx =====================


#' tsConnect
#'
#' connect and login Tinysoft server
#' @author ruifei.yin
#' @export
tsConnect <- function(user="gtrycs",psw="gtrycs888888"){
  # libpath <- .libPaths()[1]
  # match.arg(arg = os,choices = c("win32","win64"))
  # if(os == "win32"){
  #   dyn.load(paste(libpath,"/QDataGet/tslr/i386/tslr.dll",sep = ""))
  # }else if(os == "win64"){
  #   dyn.load(paste(libpath,"/QDataGet/tslr/x64/tslr.dll",sep = ""))
  # }
  a <- .External("tslConnectServer","tsl.tinysoft.com.cn",443,NULL) # connect the server. return 0 if connect successfully.
  b <- .External("tslLoginServer",user,psw) # login the server
  # c <- .External("tslSetComputeBitsOption",0)# compute automatically
  f <- .External("tslLogined")# return 1 if logined already
  if (f!=1L) stop(paste("Tinysoft Login failed! Error message is:",f)) else cat("Tinysoft logined.\n")
}

#' @rdname tsConnect
#' @export
tsLogined <- function(){
  .External("tslLogined")
}
#' @rdname tsConnect
#' @export
tsDisconnect <- function(){
  a <- .External("tslDisconnect")
  cat("Tinysoft disconnected.")
}
#' @rdname tsConnect
#' @param funchar a charactor string of tinysoft script
#' @param syspars a list of tinysoft system parametres.(including:StockID CurrentDate Cycle bRate RateDay Precision)
#' @return a scalar or a list
#' @export
#' @examples
#' tsRemoteExecute("return close();",list(StockID="SZ000002",CurrentDate=rdate2ts(as.Date("2014-11-11"))))
#' # use ldply/laply/llply to transform the result from list to comfortable form
#' ll <- tsRemoteExecute("return Array(('a':1,'b':4),('a':2,'b':5),('a':3,'b':6));")
#' plyr::ldply(ll,unlist)
#' plyr::laply(ll,unlist)
#' plyr::llply(ll,unlist)
tsRemoteExecute <- function(funchar,syspars=NULL){
  a <- .External("tslRemoteExecute",funchar,syspars)
  suscess <- a[[1]]
  if (suscess==0L) {
    return (a[[2]])
  } else {
    stop(a[[3]])
  }
}

#' @rdname tsConnect
#' @param funchar a charactor string of tinysoft functions
#' @param pars a list of funchar's parametres
#' @param syspars a list of tinysoft system parametres(including:StockID CurrentDate Cycle bRate RateDay Precision)
#' @export
#' @examples
#' tsRemoteCallFunc("close",,list(StockID="SZ000002"))
#' tsRemoteCallFunc("rand",list(2,3))
tsRemoteCallFunc <- function(funchar,pars=NULL,syspars=NULL){
  a <- .External("tslRemoteCallFunc",funchar,pars,syspars)
  suscess <- a[[1]]
  if (suscess==0L) {
    return (a[[2]])
  } else {
    stop(a[[3]])
  }
}





