% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atri_docs_upload.R
\name{atri_docs_upload}
\alias{atri_docs_upload}
\title{Upload files to ATRI standard docs topics}
\usage{
atri_docs_upload(
  protocol,
  topic_code = NULL,
  file_code = NULL,
  file = NULL,
  file_label = NULL,
  file_description = NULL,
  reason_for_change = NULL,
  site_code = NULL
)
}
\arguments{
\item{protocol:}{The protocol name (e.g. 'a345-test-1').}

\item{topic_code:}{specify a topic_code to upload file into it.}

\item{file_code:}{use file_code to overwrite a file in a topic_code. No file_code is needed when
upload a new file.}

\item{file:}{a string text for local file location.}

\item{file_label:}{a string text for file label.}

\item{file_description:}{a string text for file description.}

\item{reason_for_change:}{a string text to indicate reason for change.}

\item{site_code:}{a string text to indicate the site topic code. Required for site-restricted topics.}
}
\value{
atri_api object contains:
response- response from http url;
url- a string of complete url;
protocol- a string of input protocol.
}
\description{
A wrapper to upload file to ATRI portal standard docs topics
}
\examples{
\donttest{
# upload new file
write.csv('test initial upload',file='test.csv')
new_upload_resp <- atri_docs_upload(protocol = "a345-test-1", 
  topic_code = "report-configuration", 
  file = "test.csv", 
  file_label = "Test upload function file", 
  file_description = "Test upload function")
# upload new file to site topic
new_upload_site_topic <- atri_docs_upload(protocol = "a345-test-1", 
  topic_code = "site-folder-test", 
  file = "test.csv", 
  file_label = "Test upload function file", 
  file_description = "Test upload function",
  site_code="999")
 
# replace a file
write.csv('test re-upload',file='retest.csv')
topicfiles <- atri_api(protocol='a345-test-1', path='/docs/topics/report-configuration/files?format=json')
re_upload_resp <- atri_docs_upload(protocol = "a345-test-1", 
  topic_code = "report-configuration", 
  file_code = topicfiles[topicfiles$label \%in\% "Test upload function file", "code"],
  file = "retest.csv", 
  reason_for_change = "test re-uploads")
# replace file to site topic
topicfiles <- atri_api(protocol='a345-test-1', path='/docs/topics/site-folder-test/files?format=json&site_code=999')
re_upload_site_topic <- atri_docs_upload(protocol = "a345-test-1", 
  topic_code = "site-folder-test", 
  file_code = topicfiles[topicfiles$label \%in\% "Test upload function file", "code"],
  file = "retest.csv", 
  reason_for_change = "test re-uploads",
  site_code="999")
}
}
\seealso{
\code{\link{atri_api}}
}
