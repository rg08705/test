#' Read ATRI study data
#' 
#' @param protocol The protocol name (e.g. 'a345-test-1').
#' @param data The study data source name. See below for examples for each data source. Additionally, allowable text input can be the data export label (e.g.'Participant list'), the relative public api pathway (e.g. '/subjects'), or the data lake filename (e.g. 'participant_list.csv' or 'participant_list').
#' @param datalake default is TRUE, which returns data from the latest data extract from the data lake s3 archive topic. 
#' With datalake=FALSE, returns the current data using the public API (/public/api/v1/<relative API path>).
#' @return A data.frame of the study data. 
#' 
#' @seealso \code{\link{atri_api}}
#' @examples
#' \donttest{
#' #--Participant data--#
#' #Participant list
#' subjects<-atri_study_data(protocol="a345-test-1",data="subjects")
#' #Participant Attribute Value list
#' subject_attr<-atri_study_data(protocol="a345-test-1",data="subject_attr")
#' #Participant Attribute Datadic list
#' subject_attr_dd<-atri_study_data(protocol="a345-test-1",data="subject_attr_dd")
#' 
#' #--Site data--#
#' #Site list
#' sites<-atri_study_data("a345-test-1","sites")
#' #Site Attribute Value list
#' site_attr<-atri_study_data("a345-test-1","site_attr")
#' #Site Attribute Datadic list
#' site_attr_dd<-atri_study_data("a345-test-1","site_attr_dd")
#' 
#' #--Study attribute data--#
#' #Study Attribute Datadic list
#' study_attr_dd<-atri_study_data("a345-test-1","study_attr_dd")
#' 
#' #--CRF metadata--#
#' #eCRF list
#' crfs<-atri_study_data("a345-test-1","crfs")
#' #Monitor Review list
#' monitor_review<-atri_study_data("a345-test-1","monitor_review")
#' #Form Lock list
#' form_lock<-atri_study_data("a345-test-1","form_lock")
#' 
#' #--Event/Schedule of events data--#
#' #Event list
#' events<-atri_study_data("a345-test-1","events")
#' #Event Attribute Datadic list
#' event_attr_dd<-atri_study_data("a345-test-1","event_attr_dd")
#' #Schedule of Events
#' tracks<-atri_study_data("a345-test-1","tracks")
#' #Subject Track list
#' subject_track<-atri_study_data("a345-test-1","subject_track")
#' 
#' #--Schedule of events by track code--#
#' #If datalake=TRUE, must use datalake filelabel or filename
#' soe_a3_default<-atri_study_data("a345-test-1","schedule_of_events_a3_default")
#' soe_a4_default<-atri_study_data("a345-test-1","schedule_of_events_a4_default.csv")
#' #If datalake=FALSE, must use API relative path
#' soe_a3_default<-atri_study_data("a345-test-1","/tracks/a3_default")
#' 
#' #--Alternate examples for 'data' parameter input--#
#' #Data export label
#' subject_track.1<-atri_study_data("a345-test-1",data="Subject Track list")
#' #Relative API path
#' subject_track.2<-atri_study_data("a345-test-1",data="/subject/track/list")
#' #Data lake filename
#' subject_track.3<-atri_study_data("a345-test-1",data="subject_track_list.csv")
#' subject_track.4<-atri_study_data("a345-test-1",data="subject_track_list")
#' 
#' #--Live data pull--#
#' subject_track_live<-atri_study_data("a345-test-1","subject_track", datalake=F)
#' }
#' @export
atri_study_data <- function(protocol, data, datalake=TRUE) {
  d<-tolower(data)
  d<-ifelse(substr(d,1,1)=='/', sub("^.","",d),d)
  #Subject APIs
  if (d %in% c("subjects","participant_list.csv","participant_list","participant list")){
    dl_api<-"edc/study_data/latest/participant_list.csv"
    pub_api<-"/subjects?output_format=csv"
  } else if (d %in% c("subjects/attribute_values","subject_attribute_value_list.csv",
                         "subject_attribute_value_list","subject_attr","participant attribute value list")) {
    dl_api<-"edc/study_data/latest/subject_attribute_value_list.csv"
    pub_api<-"/subjects/attribute_values?output_format=csv"
  } else if (d %in% c("/attribute_mng/subject/list","subject_attribute_datadic.csv",
                         "subject_attribute_datadic.csv","subject_attr_dd","participant attribute datadic list")) {
    dl_api<-"edc/study_data/latest/subject_attribute_datadic.csv"
    pub_api<-"/attribute_mng/subject/list?output_format=csv"
  #Site APIs
  } else if (d %in% c("sites","site_list.csv","site_list","site list")){
    dl_api<-"edc/study_data/latest/site_list.csv"
    pub_api<-"/sites?output_format=csv"
  } else if (d %in% c("site/attribute_values","site_attribute_value_list.csv",
                         "site_attribute_value_list","site_attr","site attribute value list")) {
    dl_api<-"edc/study_data/latest/site_attribute_value_list.csv"
    pub_api<-"/site/attribute_values?output_format=csv"
  } else if (d %in% c("/attribute_mng/site/list","site_attribute_datadic.csv",
                         "site_attribute_datadic","site_attr_dd","site atrribute datadic list")) {
    dl_api<-"edc/study_data/latest/site_attribute_datadic.csv"
    pub_api<-"/attribute_mng/site/list?output_format=csv"
  #Study APIs
  } else if (d %in% c("/attribute_mng/study/list","study_attribute_datadic.csv",
                         "study_attribute_datadic","study_attr_dd","study atrribute datadic list")) {
    dl_api<-"edc/study_data/latest/study_attribute_datadic.csv"
    pub_api<-"/attribute_mng/study/list?output_format=csv"
  #CRF APIs
  } else if (d %in% c("crfs","ecrf_list.csv","ecrf_list","ecrf list")){
    dl_api<-"edc/study_data/latest/ecrf_list.csv"
    pub_api<-"/crfs?output_format=csv"
  } else if (d %in% c("monitor","monitor_review_list.csv",
                         "monitor_review_list","monitor_review","monitor review list")) {
    dl_api<-"edc/crf_metadata/latest/monitor_review_list.csv"
    pub_api<-"/monitor?output_format=csv"
  } else if (d %in% c("lock","form_lock_list.csv",
                         "form_lock_list","form_lock","form lock list")) {
    dl_api<-"edc/crf_metadata/latest/form_lock_list.csv"
    pub_api<-"/lock?output_format=csv"
  #Event APIs
  } else if (d %in% c("events","event_list.csv","event_list","event list")){
    dl_api<-"edc/study_data/latest/event_list.csv"
    pub_api<-"/events?output_format=csv"
  } else if (d %in% c("attribute_mng/event/list","event_attribute_datadic.csv",
                         "event_attribute_datadic","event_attr_dd","event atrribute datadic list")) {
    dl_api<-"edc/study_data/latest/event_attribute_datadic.csv"
    pub_api<-"/attribute_mng/event/list?output_format=csv"
  } else if (d %in% c("tracks","schedule_of_events_list.csv",
                         "schedule_of_events_list","soes","schedule of events")){
    dl_api<-"edc/study_data/latest/schedule_of_events_list.csv"
    pub_api<-"/tracks?output_format=csv"
  } else if (d %in% c("subject/track/list","subject_track_list.csv",
                         "subject_track_list","subject_track","subject track list")){
    dl_api<-"edc/study_data/latest/subject_track_list.csv"
    pub_api<-"/subject/track/list?output_format=csv"
  } else {
    d<-gsub('.csv','',d,fixed=T)
    files.list<-atri_api(protocol,path=paste0('/docs/s3_archive/data_lake/items/edc/study_data/latest/?format=json&q=',utils::URLencode(d)))
    files.list<-files.list[grepl(paste0(d,".csv"),files.list$label,fixed=T),]
    if (nrow(files.list)==1 & datalake==T){#if datalake=T and pathway not hardcoded above, check if data parameter matches a datalake file
      dl_api<-files.list$label
    } else if (substr(data,1,1)=='/'&datalake==F){#if datalake=F and data starts with '/' then attempt to use the pathway
      pub_api<-paste0(data,"?output_format=csv")
    } else {
      stop(paste0(data," pathway not set up. Check documentation or use atri_api function to obtain data source."))}
  }
  if (datalake) {
    temp<-tempfile()
    resp<-atri_api(protocol, path=paste0("/docs/s3_archive/data_lake/download/",
                                         dl_api), direct=FALSE, file = paste0(temp,".csv"))
    output<-read.csv(paste0(temp,".csv"))
  } else {
    output<-atri_api(protocol,path=pub_api,direct=FALSE)
  }
  return(output)
}