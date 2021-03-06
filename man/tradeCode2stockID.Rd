% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pub02_dataGettingFuncs.R
\name{tradeCode2stockID}
\alias{tradeCode2stockID}
\title{tradeCode2stockID}
\usage{
tradeCode2stockID(tradeCode, secuCate = 1, IDsrc = "local",
  datasrc = defaultDataSRC())
}
\arguments{
\item{tradeCode}{Vector of character,giving the stock trading code. eg. \code{c("600001","600002","000300")}}

\item{secuCate}{integer,giving the secuCategory.(1 for A equity,4 for index,...)}

\item{IDsrc}{a charactor string, could be one of "local","jy","ts","wind",etc.}
}
\value{
a vector,return the stockID of specific datascr.
}
\description{
Covert the tradeCode to stockID given specific secucategory.
}
\examples{
tradeCode2stockID(c("600001","600002"),secuCate=1)
tradeCode2stockID(c("000001","000300"),secuCate=4)
}
\seealso{
Other SecuMain functions: \code{\link{SecuCategory}},
  \code{\link{SecuMarket}}, \code{\link{stockID2name}},
  \code{\link{stockID2stockID}},
  \code{\link{stockID2tradeCode}},
  \code{\link{stockName2ID}}, \code{\link{trday.IPO}},
  \code{\link{trday.unlist}}
}
\author{
Ruifei.Yin
}
