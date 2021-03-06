% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct01_frameBuildingFuncs.R
\name{TSF2TSFs_byfactor}
\alias{TSF2TSFs_byfactor}
\title{TSF2TSFs_byfactor}
\usage{
TSF2TSFs_byfactor(TSF, ftype = c("size", "self", "vol", "amt", "sector",
  "indexcomp", "date", "findate"), N = 5, freq = "year",
  includeTSF = TRUE, sectorAttr = defaultSectorAttr())
}
\arguments{
\item{ftype}{support size,trading volume,trading amount,industry,important index's component(000300.SH,000905.SH,000852.SH) and date.}

\item{N}{split into N group,default value is 5.}

\item{includeTSF}{whether add raw \code{TSF} to \code{TSFs} list,default value is \code{TRUE}.}
}
\description{
split a TSF or a TSFR into TSFs or TSFRs by factor,such as size,vol...
}
\examples{
#####split by size into 3 group####
TSFs <- TSF2TSFs_byfactor(tsf,N=3)

#####split by itself into 3 group####
TSFs <- TSF2TSFs_byfactor(tsf,ftype='self',N=3)

#####split by trading volumn of last 20 days ####
TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='vol')

#####split by sector ####
TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='sector')
TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='sector',sectorAttr=defaultSectorAttr('fct')) #self defined sector

#####split by date ####
TSFRs <- TSF2TSFs_byfactor(tsfr,ftype='date',freq='2 years')

}
\author{
Andrew Dow
}
