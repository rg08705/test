#' Grab protocol token
#'
#' grab access token from Rauth file for a protocol
#'
#' @param protocol The protocol name (e.g. 'test-adni3').
#' @return a string of protocol access token
#' @examples
#' \donttest{
#'   grab_token("test-adni3")
#' }
#' @export

grab_token <- function(protocol) {
  token <- utils::read.csv("~/.Rauth", row.names = 1, as.is = TRUE)[tolower(protocol), "token"]
  return(token)
}
