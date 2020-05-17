logon_body <- function(username, password, authentication_type){
  body<-'<attrs xmlns="http://www.sap.com/rws/bip">
  <attr name="password" type="string">%s</attr>
  <attr name="auth" type="string" possibilities="secEnterprise,secLDAP,secWinAD,secSAPR3">%s</attr>
  <attr name="userName" type="string">%s</attr>
  </attrs>'
  return(sprintf(body, password, authentication_type, username))
}


get_token <- function(){
  token <- Sys.getenv("x_sap_logontoken")
  names(token)<-"X-SAP-LogonToken"
  return(token)
}

unnest_df <- function(x) {
  y <- do.call(data.frame, c(x, list(stringsAsFactors=FALSE)))
  if("data.frame" %in% unlist(lapply(y, class))){
    y<-unnest_df(y)
  }
  return(y)
}

#' @title Logs into SAP Business Objects
#' @description Logs into SAP Business Objects and saves the SAP Business Objects API token into the System Environment
#' @param domain SAP Business Objects domain to log into
#' @param username SAP Business Objects username
#' @param password SAP Business Objects password
#' @param authentication_type Athentication Type to logon into SAP Business Objects. Defaults to \code{secLDAP}. Must be one of the following:
#' \itemize{
#' \item secLDAP
#' \item secEnterprise
#' \item secWinAD
#' \item secSAPR3
#' }
#' @return Does not return any object, only a success message, as SAP API token is saved into the environment to work with further functions.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @section Warning: Please be careful when dealing with API keys and other
#'   secrets & tokens - keep them private and do not publish them.
#' @examples
#' \dontrun{
#' log_on(domain="YOUR_DOMAIN",
#'        username="YOUR_USERNAME",
#'        password="YOUR_PASSWORD",
#'        authentication_type="secLDAP")
#' }
#' @export

log_on <- function(domain, username, password, authentication_type="secLDAP"){
  # GET SAP Cookie
  url <- httr::modify_url(domain, path = list("biprws", "logon", "long"))
  cookie_request <- httr::GET(url)
  cookies <- as.character(cookie_request$cookies$value)
  names(cookies) <- cookie_request$cookies$name
  body <- logon_body(username, password, authentication_type)
  
  # POST request to obtain SAP Token
  get_token_request <- httr::POST(url, 
                            body = body,
                            httr::content_type("application/xml"),
                            httr::add_headers(c("Accept"="application/json")),
                            httr::set_cookies(cookies))
  # Ensure request is in json format
  if (httr::http_type(get_token_request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(get_token_request)) {
    stop(sprintf("Error Code %s - %s",
                 get_token_request$status_code,
                 httr::http_status(get_token_request$status_code)$message),
         call. = FALSE)
  }
  get_token_request$cookies
  # Set token to environment variable
  token<-c("X-SAP-LogonToken"=get_token_request$headers[["x-sap-logontoken"]])
  Sys.setenv("x_sap_logontoken"=token)
  if(get_token_request$status_code==200){
    message("Logon process succesfull. SAP Logon Token saved into system environment.")
  }
}



#' @title Logs off from the SAP Business Objects
#' @description Logs off from the SAP Business Objects session and removes the SAP Business Objects API token from the System Environment
#' @param domain SAP Business Objects domain to log off from
#' @return Does not return any object, only a success message, as SAP API token is removed from the environment.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @section Warning: Please be careful when dealing with API keys and other
#'   secrets & tokens - keep them private and do not publish them.
#' @examples
#' \dontrun{
#' log_off(domain="YOUR_DOMAIN")
#' }
#' @export

log_off <- function(domain){
  url <- httr::modify_url(domain, path = list("biprws", "logoff"))
  log_off_request <- httr::POST(url, 
                                body = body,
                                httr::content_type("application/xml"),
                                httr::add_headers(get_token(),
                                                  c("Accept"="application/json")))
  if(log_off_request$status_code==200){
    Sys.unsetenv("x_sap_logontoken")
    message("Logoff process succesfull. SAP Logon Token deleted from system environment.")
  }
}


#' @title Retrieve Business Objects document schedules
#' @description Retrieves the schedules for a specific document ID and returns a tidy \code{data.frame} with detailed information.
#' @param domain SAP Business Objects domain
#' @param document_id Document ID of the SAP Business Objects Document
#' @return Returns a tidy \code{data.frame} with detailed information about the documents' schedules.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @examples
#' \dontrun{
#' get_document_schedules(domain="YOUR_DOMAIN",
#'                        document_id="YOUR_DOCUMENT_ID")
#' }
#' @export

get_document_schedules <- function(domain, document_id){
  url <- httr::modify_url(domain,
                          path = list("biprws",
                                      "raylight",
                                      "v1",
                                      "documents",
                                      document_id,
                                      "schedules"))
  get_document_schedules <- httr::GET(url, httr::add_headers(get_token(),
                                                             c("Accept"="application/json")))
  if (httr::http_type(get_document_schedules) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  #Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(get_document_schedules, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  df <- unnest_df(content$schedules$schedule)
  col_names <- colnames(df)
  col_names <- gsub("^X\\.", "", col_names)
  col_names <- gsub("\\.\\.", "", col_names)
  colnames(df) <- col_names
  return(df)
}