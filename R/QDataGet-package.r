#' QDataGet
#'
#' @name QDataGet
#' @docType package
#' @import QUtility  RSQLite WindR
#' @importFrom zoo zoo as.zoo
#' @importFrom xts as.xts xts 
#' @importFrom data.table data.table :=
#' @importFrom lubridate %m+% %m-%
#' @importFrom dplyr %>% group_by do summarise arrange slice ungroup mutate
#' @importFrom DBI dbDriver dbGetQuery dbSendQuery dbClearResult dbWriteTable dbReadTable dbRemoveTable dbConnect dbDisconnect dbListFields dbListTables dbExistsTable
#' @importFrom RODBC odbcConnect odbcDriverConnect odbcClose odbcCloseAll sqlQuery sqlDrop sqlSave
NULL
