#' Read ATRI CRF data
#' 
#' @param protocol The protocol name (e.g. 'a345-test-1').
#' @param crf The table name (e.g. 'registry').
#' @param datalake default is TRUE, which returns data from the latest data extract from the data lake s3 archive topic. 
#' With datalake=FALSE, returns the current data using the public API (/public/api/v1/crfs/data/<crf>).
#' @param wide default is TRUE, which returns data with one record per row (one column per field). With wide=FALSE, 
#' returns data in the raw, long format (one row per field).
#' @param translated default is TRUE, which returns translated data. Only applicable if wide=TRUE
#' @return A data.frame of the crf data. 
#' 
#' @seealso \code{\link{atri_api}}
#' @examples
#' \donttest{
#'    registry_wide <- atri_crf("a345-test-1", "registry")
#'    registry_long <- atri_crf("a345-test-1", "registry",wide=F)
#'    registry_untranslated <- atri_crf("a345-test-1", "registry",translated=F)
#'    registry_live <- atri_crf("a345-test-1", "registry",datalake=F)
#' }
#' @importFrom magrittr "%>%"
#' @export
atri_crf <- function(protocol, crf, datalake=TRUE, wide=TRUE, translated=TRUE) {
  #check if crf is listed in crf list
  crf<-tolower(crf)
  crf_list<-atri_api(protocol = protocol, path = "/crfs/")
  if(!crf %in% crf_list$name){
    stop(crf, ' not in list of CRF names for ', protocol)
  }
  if (datalake) {
    path<-paste0("/docs/s3_archive/data_lake/download/edc/crf_data/latest/",crf,".csv")
    temp<-tempfile()
    resp<-atri_api(protocol, path=path,file=paste0(temp,".csv"))
    dd<-read.csv(paste0(temp,".csv"))
  } else {
    path<-paste0("/public/api/v1/crfs/data/",crf,"/?output_format=csv")
    dd<-atri_api(protocol,path=path,direct=T)
  }
  if(nrow(dd)>0){
    # substring timestamp metadata fields to format below
    # strptime("2016-09-08T18:54:14.418Z", format="%Y-%m-%dT%H:%M:%OSZ")
    dd$ts_create<-as.POSIXct(strptime(dd$ts_create, format="%Y-%m-%d %T"))
    dd$ts_last_modify<-as.POSIXct(strptime(dd$ts_last_modify, format="%Y-%m-%d %T"))
    if(wide){
      atts <- dplyr::filter(dd, !duplicated(dd_field.name)) %>%
        dplyr::select(dd_field.name, dd_revision_field.label, dd_revision_field.unit)
      if(translated){
        ddw <- dplyr::select(dd, subject_event_crf.id, dd_revision_field.translated_value, dd_field.name) %>%
          tidyr::spread(dd_field.name, dd_revision_field.translated_value)
      } else {
        ddw <- dplyr::select(dd, subject_event_crf.id, dd_revision_field.value, dd_field.name) %>%
          tidyr::spread(dd_field.name, dd_revision_field.value)      
      }
      ddw.id <- dplyr::filter(dd, !duplicated(subject_event_crf.id)) %>%
        dplyr::select(subject_event_crf.id:has_unresolved_error)
      dd <- dplyr::full_join(ddw.id, ddw, by='subject_event_crf.id')
      # write and read back as csv to obtain typical classes
      temp<-tempfile()
      utils::write.csv(dd, file=temp, row.names=FALSE)
      dd <- read.csv(file=temp, as.is=TRUE, stringsAsFactors=FALSE, check.names = FALSE)
      for(cc in atts$dd_field.name){
        subs <- subset(atts, dd_field.name==cc)
        if(!is.na(subs$dd_revision_field.label)){
          Hmisc::label(dd[, cc]) <- subs$dd_revision_field.label
        } 
        if(!is.na(subs$dd_revision_field.unit)){
          dd[, cc] <- Hmisc:::`units<-.default`(dd[, cc], subs$dd_revision_field.unit)
        }
      }
    }
    #uppercase values of boolean fields
    dd$event.is_unscheduled <- toupper(dd$event.is_unscheduled) 
    dd$has_unresolved_error <- toupper(dd$has_unresolved_error) 
    return(dd)
  }else{return(NULL)} #if no data exists yet, NULL
}