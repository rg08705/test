#' Source or download data using ATRI portal api
#'
#' A wrapper to source EDC data, get list of items in Docs topics, or download Docs files using the ATRI portal api
#'
#' @param protocol: The protocol name (e.g. 'a345').
#' @param path: The relative path of the public api. See API documentation for details:
#' https://atrihub.atlassian.net/wiki/spaces/SDS/pages/1089962604/SDS+-+Public+Data+Export+APIs
#' and examples.
#' @param direct: Default is FALSE (appends protocol URL and '/public/api/v1' to path), set TRUE to append path directly the protocol url.
#' @param file: (optional) Filename to download the API response result.
#' @param filter: (optional; only applies to getting list of items using the Docs API) Subsets list of files or folders that matches the characters with the item's 'label' (and 'description' for topics and files in standard topics) NOTE: string entered is NOT case-sensitive and NOT fixed to match exact string).
#' @param sort: (optional; only applies to getting list of items using the Docs API) Sorts the list of files or folders by the allowable character parameter entered. Allowable parameters for standard topics include: label, code, ts_last_modify, ts_create, ts_file_last_modify, file_size, file_type. Allowable parameters for s3 topics include: label, last_modified, size. Descending order can be prefixed with '-' (e.g. '-label').
#' @return Data frame of API request result. If file is downloaded, atri_api object containing: response- response from http url; url- a string of complete url; protocol- a string of input protocol; filename- name of downloaded file.
#' @seealso \code{\link{atri_crf}}; \code{\link{atri_docs_download}}; \code{\link{atri_study_data}}; \code{\link{atri_query_data}}
#' @examples
#' \donttest{
#' protocol<- "a345"
#' #-----CRF Data (consider using atri_crf instead)-----#
#' registry_meta_data <- atri_api(protocol = protocol, path = "/crfs/data/registry/")
#'
#' #-----Study Meta Data (consider using atri_study_data instead)-----#
#' #-CRF List - List of CRFs-#
#' crfs <- atri_api(protocol = protocol, path = "/crfs/")
#' #-Site List - List of all sites-#
#' sites_json <- atri_api(protocol = protocol, path = "/sites/")
#' sites_csv <- atri_api(protocol = protocol, path = "/sites?output_format=csv")
#' #-Subject List-#
#' subjects <- atri_api(protocol = protocol, path = "/subjects/")
#' subjects999 <- atri_api(protocol = protocol, path = "/subjects?site_code=999")
#' #-Event List - List of active events-#
#' events <- atri_api(protocol = protocol, path="/events/")
#' #-Event Track List - Schedule of event tracks-#
#' tracks <- atri_api(protocol=protocol, path="/tracks/")
#' 
#' #-----Query Data (consider using atri_qeury_data instead)-----#)
#' query <- atri_api(protocol=protocol, path="/query/queries?output_format=csv")
#' #-Store Query as a csv-#
#' queryresp <- atri_api(protocol=protocol, path="/query/queries?output_format=csv",
#'  file = "query.csv")
#'
#' #-----Docs-----#
#' #-Topic List - List of docs topics user has access to-#
#' topics <- atri_api(protocol="a345-test-1", path="/docs/topics/")
#' #-File List under a Standard Topic-#
#' cogstatefiles <- atri_api(protocol="a345-test-1", path="/docs/topics/transfer-cogstate/files?format=json")
#' #-Download file from file list of a standard topic-#
#' cogstatefiles <- atri_api(protocol="a345-test-1", path="/docs/topics/transfer-cogstate/files?format=json")
#' atri_api(protocol="a345-test-1", path=cogstatefiles$latest_version.download_url[1], direct = TRUE, file = cogstatefiles$label[1])
#' 
#' #-Filter file list to match a string of characters-#
#' reports_saama<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", filter='saama data')
#' #-Sort item list-#
#' reports_ascending<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", sort='label')
#' reports_descending<-atri_api(protocol="a345-test-1", path="/docs/topics/study-management-reports/files?format=json", sort='-label')
#' 
#' #-File or Folder List under a S3 topic (S3_archive and S3_topic)-#
#' cogstatefiles_latest<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/latest/')
#' cogstatefiles_archived<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/archived/')
#' #-Download file from file list of a S3 topic-#
#' cogstatefiles<-atri_api(protocol="a345-test-1", path='/docs/s3_archive/data_lake/items/transfers/cogstate/inbound/assessment/latest/')
#' atri_api(protocol="a345-test-1", path=paste0("/docs/s3_archive/data_lake/download/",cogstatefiles$label[1]), direct = FALSE, file = 'cogstatefile1.csv')
#' }
#' @export

atri_api <- function(protocol, path, direct=FALSE, file=NA, filter=NULL, sort=NULL) {
  protocol_url <- paste0("https://", protocol, ".atrihub.org")
  if (direct) {
    url <- paste0(protocol_url, path)
  } else {
    url <- paste0(protocol_url, "/public/api/v1", path)
  }
  if (!grepl("?", url, fixed=TRUE)) {#will ensure '?' is added to url to allow parameters
    url <- paste0(url, "?")
  }
  if (!is.null(filter)) {
    url <- paste0(url, "&q=",utils::URLencode(filter))
  }
  if (!is.null(sort)) {
    url <- paste0(url, "&sort=",sort)
  }
  if (is.na(file)) {
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Token", grab_token(protocol))))
  } else {
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Token", grab_token(protocol))),
                      httr::write_disk(file, overwrite = TRUE))
    if(resp$status_code >= 400) {#if unsuccessful, will try request once more with authorization included in URL
      resp<- httr::GET(resp$url, httr::write_disk(file, overwrite = TRUE))
    }
  }
  httr::warn_for_status(resp)
  if(resp$status_code >= 400) #if unsuccessful, print error
    stop(paste0("invalid http status code:", resp$status_code,
                ", check token, connection, api documentation and ", url))
  if (!grepl('output_format=csv', url) & is.na(file)) {
    if (httr::http_type(resp) != "application/json") {
      stop(paste0("API did not return json, check api documentation and ", url), call. = FALSE)
    }
  }
  if (is.na(file)) {
    x<-structure(list(response = resp, url = url, protocol = protocol),
                 class = "atri_api")
    #as.data.frame resp result
    content <- httr::content(x$response, "text")
    if (grepl("output_format=csv", x$url)) {
      con <- textConnection(content)
      output_data <- as.data.frame(utils::read.csv(con))
      close(con)
    } else {
      api <- jsonlite::fromJSON(content, flatten = TRUE)
      if (grepl("/crfs/", x$url)) {
        output_data <- api
      } else {
        classes <- sapply(api$data, class)
        for (i in colnames(api$data)) {
          if (classes[i] %in% "list") {
            if(length(api$data[[i]]) > 0 && is.data.frame(api$data[[i]][[1]])) {
              dd <- dplyr::bind_rows(api$data[[i]])
              colnames(dd) <- paste0(i, ".", colnames(dd))
              api$data <- dplyr::bind_cols(api$data, dd)
              api$data[[i]] <- NULL
            }
          }
        }
        output_data <- as.data.frame(api$data)
      }
    }
    return(output_data)
  } else (
    structure(list(response = resp, url = url, protocol = protocol, filename=file),
              class = "atri_api")
  )
}
