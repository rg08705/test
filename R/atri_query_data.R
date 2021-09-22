#' Read ATRI query data
#' 
#' @param protocol The protocol name (e.g. 'a345-test-1').
#' @param collapsed default is TRUE, which returns the most recent transaction of the query ID (only 1 row per query ID). With collapsed=FALSE, returns all historical query transactions.
#' @param datalake default is TRUE, which returns data from the latest data extract from the data lake s3 archive topic. 
#' With datalake=FALSE, returns the current data using the public API (/public/api/v1/query/queries).
#' @return A data.frame of the query data. 
#' 
#' @seealso \code{\link{atri_api}}
#' @examples
#' \donttest{
#'    query_data <- atri_query_data("a345-test-1")
#'    query_data_all_transactions <- atri_query_data("a345-test-1", collapsed=FALSE)
#'    query_data_live <- atri_query_data("a345-test-1",datalake=FALSE)
#' }
#' @importFrom magrittr "%>%"
#' @export
atri_query_data <- function(protocol, collapsed=TRUE, datalake=TRUE) {
  if (datalake) {
    temp<-tempfile()
    resp<-atri_api(protocol, path="/docs/s3_archive/data_lake/download/edc/query_data/latest/query_list.csv", direct=FALSE, file = paste0(temp,".csv"))
    output<-read.csv(paste0(temp,".csv"))
  } else {
    output<-atri_api(protocol,path="/query/queries?output_format=csv",direct=FALSE)
  }
  if (collapsed) {
    output<-output %>% dplyr::group_by(id) %>% dplyr::filter(ts_create == max(ts_create))
  }
  return(output)
}