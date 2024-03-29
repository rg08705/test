% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atri_api.R
\name{atri_api}
\alias{atri_api}
\title{Source or download data using ATRI portal api}
\usage{
atri_api(protocol, path, direct = FALSE, file = NA, filter = NULL, sort = NULL)
}
\arguments{
\item{protocol:}{The protocol name (e.g. 'a345').}

\item{path:}{The relative path of the public api. See API documentation for details:
https://atrihub.atlassian.net/wiki/spaces/SDS/pages/1089962604/SDS+-+Public+Data+Export+APIs
and examples.}

\item{direct:}{Default is FALSE (appends protocol URL and '/public/api/v1' to path), set TRUE to append path directly the protocol url.}

\item{file:}{(optional) Filename to download the API response result.}

\item{filter:}{(optional; only applies to getting list of items using the Docs API) Subsets list of files or folders that matches the characters with the item's 'label' (and 'description' for topics and files in standard topics) NOTE: string entered is NOT case-sensitive and NOT fixed to match exact string).}

\item{sort:}{(optional; only applies to getting list of items using the Docs API) Sorts the list of files or folders by the allowable character parameter entered. Allowable parameters for standard topics include: label, code, ts_last_modify, ts_create, ts_file_last_modify, file_size, file_type. Allowable parameters for s3 topics include: label, last_modified, size. Descending order can be prefixed with '-' (e.g. '-label').}
}
\value{
Data frame of API request result. If file is downloaded, atri_api object containing: response- response from http url; url- a string of complete url; protocol- a string of input protocol; filename- name of downloaded file.
}
\description{
A wrapper to source EDC data, get list of items in Docs topics, or download Docs files using the ATRI portal api
}
\examples{
\donttest{
protocol<- "a345"
#-----CRF Data (consider using atri_crf instead)-----#
registry_meta_data <- atri_api(protocol = protocol, path = "/crfs/data/registry/")

#-----Study Meta Data (consider using atri_study_data instead)-----#
#-CRF List - List of CRFs-#
crfs <- atri_api(protocol = protocol, path = "/crfs/")
#-Site List - List of all sites-#
sites_json <- atri_api(protocol = protocol, path = "/sites/")
sites_csv <- atri_api(protocol = protocol, path = "/sites?output_format=csv")
#-Subject List-#
subjects <- atri_api(protocol = protocol, path = "/subjects/")
subjects999 <- atri_api(protocol = protocol, path = "/subjects?site_code=999")
#-Event List - List of active events-#
events <- atri_api(protocol = protocol, path="/events/")
#-Event Track List - Schedule of event tracks-#
tracks <- atri_api(protocol=protocol, path="/tracks/")

#-----Query Data (consider using atri_qeury_data instead)-----#)
query <- atri_api(protocol=protocol, path="/query/queries?output_format=csv")
#-Store Query as a csv-#
queryresp <- atri_api(protocol=protocol, path="/query/queries?output_format=csv",
 file = "query.csv")

#-----Docs-----#
#-Topic List - List of docs topics user has access to-#
topics <- atri_api(protocol="a345-test-1", path="/docs/topics/")
#-File List under a Standard Topic-#
cogstatefiles <- atri_api(protocol="a345-test-1", path="/docs/topics/transfer-cogstate/files?format=json")
#-Download file from file list of a standard topic-#
cogstatefiles <- atri_api(protocol="a345-test-1", path="/docs/topics/transfer-cogstate/files?format=json")
atri_api(protocol="a345-test-1", path=cogstatefiles$latest_version.download_url[1], direct = TRUE, file = cogstatefiles$label[1])

#-Filter file list to match a string of characters-#
reports_saama<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", filter='saama data')
#-Sort item list-#
reports_ascending<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", sort='label')
reports_descending<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", sort='-label')

#-File or Folder List under a S3 topic (S3_archive and S3_topic)-#
cogstatefiles_latest<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/latest/')
cogstatefiles_archived<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/archived/')
#-Download file from file list of a S3 topic-#
cogstatefiles<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/latest/')
atri_api(protocol="a345-test-1", path=paste0("/docs/s3_archive/data_lake/download/",cogstatefiles$label[1]), direct = FALSE, file = 'cogstatefile1.csv')
}
}
\seealso{
\code{\link{atri_crf}}; \code{\link{atri_docs_download}}; \code{\link{atri_study_data}}; \code{\link{atri_query_data}}
}
