#' Upload files to ATRI standard docs topics
#' 
#' A wrapper to upload file to ATRI portal standard docs topics
#' 
#' @param protocol: The protocol name (e.g. 'a345-test-1').
#' @param topic_code: specify a topic_code to upload file into it.
#' @param file_code: use file_code to overwrite a file in a topic_code. No file_code is needed when
#' upload a new file.
#' @param file: a string text for local file location.
#' @param file_label: a string text for file label.
#' @param file_description: a string text for file description.
#' @param reason_for_change: a string text to indicate reason for change.
#' @param site_code: a string text to indicate the site topic code. Required for site-restricted topics.
#' @return atri_api object contains:
#' response- response from http url;
#' url- a string of complete url;
#' protocol- a string of input protocol.
#' @seealso \code{\link{atri_api}}
#' @examples
#' \donttest{
#' # upload new file
#' write.csv('test initial upload',file='test.csv')
#' new_upload_resp <- atri_docs_upload(protocol = "a345-test-1", 
#'   topic_code = "report-configuration", 
#'   file = "test.csv", 
#'   file_label = "Test upload function file", 
#'   file_description = "Test upload function")
#' # upload new file to site topic
#' new_upload_site_topic <- atri_docs_upload(protocol = "a345-test-1", 
#'   topic_code = "site-folder-test", 
#'   file = "test.csv", 
#'   file_label = "Test upload function file", 
#'   file_description = "Test upload function",
#'   site_code="999")
#'  
#' # replace a file
#' write.csv('test re-upload',file='retest.csv')
#' topicfiles <- atri_api(protocol='a345-test-1', path='/docs/topics/report-configuration/files?format=json')
#' re_upload_resp <- atri_docs_upload(protocol = "a345-test-1", 
#'   topic_code = "report-configuration", 
#'   file_code = topicfiles[topicfiles$label %in% "Test upload function file", "code"],
#'   file = "retest.csv", 
#'   reason_for_change = "test re-uploads")
#' # replace file to site topic
#' topicfiles <- atri_api(protocol='a345-test-1', path='/docs/topics/site-folder-test/files?format=json&site_code=999')
#' re_upload_site_topic <- atri_docs_upload(protocol = "a345-test-1", 
#'   topic_code = "site-folder-test", 
#'   file_code = topicfiles[topicfiles$label %in% "Test upload function file", "code"],
#'   file = "retest.csv", 
#'   reason_for_change = "test re-uploads",
#'   site_code="999")
#' }
#' @export

atri_docs_upload <- function(protocol, 
                           topic_code = NULL, 
                           file_code = NULL, 
                           file = NULL, 
                           file_label = NULL, 
                           file_description = NULL,
                           reason_for_change = NULL,
                           site_code = NULL) {
  token <- grab_token(protocol)
  source_file <- httr::upload_file(file)
  protocol_url <- paste0("https://", protocol, ".atrihub.org")
  if (is.null(topic_code) & is.null(file_code)) {
    stop("topic_code and file_code can not both be NULL.")
  } else if (! is.null(topic_code) & is.null(file_code)) {
    # provide only topic_code to upload newfile.
    url <- paste0(protocol_url, "/public/api/v1", "/docs/topics/", topic_code, "/files")
    if (is.null(file_label) | is.null(file_description)) {
      stop("file_label and file_description can not be NULL when upload new file")
    }
    api.body <-list(topic_code = topic_code, 
                label = file_label, 
                description = file_description, 
                topic_code = topic_code, 
                source_file = source_file)
    if (!is.null(site_code)) {
      url <- paste0(url, "?site_code=",site_code)
      api.body <- c(api.body,list(site_code=site_code))
    }
    resp <- httr::POST(url, 
                       body = api.body, 
                       httr::add_headers(Authorization = paste("Token", token)))
  } else if (! is.null(topic_code) & ! is.null(file_code)) {
    # check if file_code is under the topic_code
    topic_url <- paste0(protocol_url, "/public/api/v1", "/docs/topics/", topic_code, "/files/")
    if (!is.null(site_code)) {
      topicfiles <- atri_api(protocol=protocol, path=paste0('/docs/topics/', topic_code, "/files?format=json&site_code=",site_code))
    } else {
      topicfiles <- atri_api(protocol=protocol, path=paste0('/docs/topics/', topic_code, "/files?format=json"))
    }
    if (! file_code %in% topicfiles$code) {
      stop(paste0(file_code, " is not found in topic_code:", topic_code))
    }
    if (is.null(reason_for_change)) {
      stop("reason_for_change can not be NULL, when replace existing file")
    }
    url <- paste0(protocol_url, "/public/api/v1", "/docs/files/", file_code) 
    api.body <- list(file_code = file_code,
                source_file = source_file,
                reason_for_change = reason_for_change, 
                topic_code = topic_code)
    if (!is.null(site_code)) {
      url <- paste0(url, "?site_code=",site_code)
      api.body <- c(api.body,list(site_code=site_code))
    }
    resp <- httr::POST(url, 
                       body = api.body, 
                       httr::add_headers(Authorization = paste("Token", token)))
  }
  httr::warn_for_status(resp)
  if(resp$status_code >= 400) 
    stop(paste0("invalid http status code, check token, connection and ", url))
  structure(
    list(response = resp, 
         url = url, 
         protocol = protocol),
    class = "atri_api"
  )
}