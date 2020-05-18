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
  # Build URL
  url <- httr::modify_url(domain, path = list("biprws", "logon", "long"))
  # GET SAP Cookie
  cookie_request <- httr::GET(url)
  cookies <- as.character(cookie_request$cookies$value)
  names(cookies) <- cookie_request$cookies$name
  body <- logon_body(username, password, authentication_type)
  
  # POST request to obtain SAP Token
  get_token_request <- httr::POST(url, 
                            body = body,
                            httr::content_type_xml(),
                            httr::accept_json(),
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
  # Build URL
  url <- httr::modify_url(domain, path = list("biprws", "logoff"))
  # POST request
  request <- httr::POST(url, 
                        httr::content_type_xml(),
                        httr::accept_json(),
                        httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  if(request$status_code==200){
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
#' get_bo_document_schedules(domain="YOUR_DOMAIN",
#'                           document_id="YOUR_DOCUMENT_ID")
#' }
#' @export

get_bo_document_schedules <- function(domain, document_id){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws",
                                      "raylight",
                                      "v1",
                                      "documents",
                                      document_id,
                                      "schedules"))
  # GET request
  request <- httr::GET(url,
                       httr::accept_json(),
                       httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  #Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  df <- unnest_df(content$schedules$schedule)
  col_names <- colnames(df)
  col_names <- gsub("^X\\.", "", col_names)
  col_names <- gsub("\\.\\.", "", col_names)
  colnames(df) <- col_names
  return(df)
}

#' @title Retrieve Business Objects document' schedule details
#' @description Retrieves the details of a specific schedule ID of a SAP Business Objects document with information about document parameters & frequency.
#' @param domain SAP Business Objects domain
#' @param document_id Document ID of the SAP Business Objects Document
#' @param schedule_id Schedule ID of the specific SAP Business Objects Document. Can be obtained through the \code{get_bo_document_schedules()} function.
#' @return Returns details about the SAP Business Objects schedule.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @examples
#' \dontrun{
#' get_bo_schedule_details(domain="YOUR_DOMAIN",
#'                         document_id="YOUR_DOCUMENT_ID",
#'                         schedule_id="YOUR_SCHEDULE_ID")
#' }
#' @export

get_bo_schedule_details <- function(domain, document_id, schedule_id){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws",
                                      "raylight",
                                      "v1",
                                      "documents",
                                      document_id,
                                      "schedules",
                                      schedule_id))
  # GET request
  request <- httr::GET(url,
                       httr::accept_json(),
                       httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  # Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  return(content$schedule)
}


#' @title Retrieving the Connections
#' @description Retrieves the list of SAP Business Objects connections stored in the CMS repository that the end-user has access to.
#' @param domain SAP Business Objects domain
#' @param type Indicates the connection type to be retrieved. Values are:
#' \itemize{
#' \item \code{"Relational"}
#' \item \code{"FlattenedOlap"}
#' \item \code{"Olap"}
#' \item \code{"DataFederator"}
#' }
#' This parameter is optional. If not specified, all types of connections are retrieved.
#' @param offset Indicates the position in the list, from which connections are returned. 
#' It must be greater than or equal to 0. The default value is  \code{0}. This parameter is optional.
#' @param limit indicates the number of connections that you can list on one page. Its range is 1 to 50.
#'  This parameter is optional. The default value is \code{50}.
#' @return Returns a tidy \code{data.frame} of SAP Business Objects connections
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/cc73a43eae76475291d35e1c3ef5451d.html}
#' @examples
#' \dontrun{
#' get_bo_connections(domain="YOUR_DOMAIN")
#' }
#' @export

get_bo_connections <- function(domain, type=NULL, offset=0, limit=50){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws",
                                      "raylight",
                                      "v1",
                                      "connections"),
                          query=list(type=type,
                                     offset=offset,
                                     limit=limit))
  # GET request
  request <- httr::GET(url,
                       httr::accept_json(),
                       httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  # Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  return(content$connections$connection)
}

#' @title Getting the Configuration parameters
#' @description Retrieves information on default configuration details declared on the WACS server, such as the default formats or color palettes.
#' @param domain SAP Business Objects domain
#' @param type This parameter is obligatory. By default set to \code{"functions"}.
#' Indicates the type from which to be retrieved. Other possible values are:
#' \itemize{
#' \item \code{"functions"}
#' \item \code{"charsets"}
#' \item \code{"visualizations"}
#' \item \code{"palettes"}
#' \item \code{"formats"}
#' \item \code{"fontmappings"}
#' \item \code{"operators"}
#' \item \code{"skins"}
#' }
#' @return Returns a tidy \code{data.frame} of SAP Business Objects configuration details
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/0c4ef646601e4c1eaf9a2571091e33d5.html}
#' @examples
#' \dontrun{
#' get_bo_configuration(domain="YOUR_DOMAIN", type="charsets")
#' get_bo_configuration(domain="YOUR_DOMAIN", type="functions")
#' }
#' @export

get_bo_configuration <- function(domain, type="functions"){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws", "raylight" , "v1",
                                      "configuration",
                                      type))
  # GET request
  request <- httr::GET(url,
                       httr::accept_json(),
                       httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  # Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  return(content[[1]][[1]])
}


#' @title Getting the List of SAP Business Objects universes
#' @description Gets the list of UNX and UNV universes stored in the CMS repository.
#' @param domain SAP Business Objects domain
#' @param offset Indicates the position in the list, from which universes are returned.
#'  It must be greater than or equal to 0. The default value is 0. This parameter is optional. The default value is \code{0}.
#' @param limit  Indicates the number of universes that you can list on one page. The range is from 1 to 50.
#'  The default value is 10. This parameter is optional. The default value is \code{50}.
#' @return Returns a tidy \code{data.frame} of SAP Business Objects Universes details
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/ec558f026fdb101497906a7cb0e91070.html}
#' @examples
#' \dontrun{
#' get_bo_universes(domain="YOUR_DOMAIN")
#' }
#' @export

get_bo_universes <- function(domain, offset=0, limit=50){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws",
                                      "raylight",
                                      "v1",
                                      "universes"),
                          query=list(offset=offset,
                                     limit=limit))
  # Get query
  request <- httr::GET(url,
                       httr::accept_json(),
                       httr::add_headers(get_token()))
  # Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }
  # Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  return(content$universes$universe)
}
