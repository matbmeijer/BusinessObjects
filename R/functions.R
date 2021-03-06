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
#' @param authentication_type Authentication Type to logon into SAP Business Objects. Defaults to \code{secLDAP}. Must be one of the following:
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
  }else if(get_token_request$status_code==200){
    message("Logon process succesfull. SAP Logon Token will be saved into system environment.")
  }
  # Set token to environment variable
  token<-c("X-SAP-LogonToken"=get_token_request$headers[["x-sap-logontoken"]])
  Sys.setenv("x_sap_logontoken"=token)
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
  # Stop if errors
  if (httr::http_error(request)) {
    stop(sprintf("Error Code %s - %s",
                 request$status_code,
                 httr::http_status(request$status_code)$message),
         call. = FALSE)
  }else if(request$status_code==200){
    Sys.unsetenv("x_sap_logontoken")
    message("Logoff process succesfull. SAP Logon Token deleted from system environment.")
  }
}

#' @title Retrieve Business Objects document details
#' @description Retrieves Gets the details of a Web Intelligence document and returns a tidy \code{data.frame} with detailed information.
#' @param domain SAP Business Objects domain
#' @param document_id Document ID of the SAP Business Objects Document
#' @param tracker_document_id Optional parameter. Identifier of a reference document for track-data feature.
#' Must be provided only when the document state is unused.
#' @return Returns a tidy \code{data.frame} with detailed information about the document.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/ec5627ba6fdb101497906a7cb0e91070.html}
#' @examples
#' \dontrun{
#' get_bo_document_details(domain="YOUR_DOMAIN",
#'                        document_id="YOUR_DOCUMENT_ID")
#' }
#' @export

get_bo_document_details <- function(domain, document_id, tracker_document_id=NULL){
  # Build URL
  url <- httr::modify_url(domain,
                          path = c("biprws/raylight/v1",
                                   "documents",
                                   document_id),
                          query=list("trackerDocumentId"=tracker_document_id))
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
  
  
  df <- unnest_df(data.frame(content$document[lengths(content$document)>0], stringsAsFactors = FALSE))
  df$updated <- as.POSIXct(df$updated, format = "%Y-%m-%dT%H:%M:%S")
  return(df)
}

#' @title Retrieve Business Objects document schedules
#' @description Retrieves the schedules for a specific document ID and returns a tidy \code{data.frame} with detailed information.
#' @param domain SAP Business Objects domain
#' @param document_id Document ID of the SAP Business Objects Document
#' @return Returns a tidy \code{data.frame} with detailed information about the documents' schedules.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/7da1d59c6f701014aaab767bb0e91070.html}
#' @examples
#' \dontrun{
#' get_bo_document_schedules(domain="YOUR_DOMAIN",
#'                           document_id="YOUR_DOCUMENT_ID")
#' }
#' @export

get_bo_document_schedules <- function(domain, document_id){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws/raylight",
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
  df$updated <- as.POSIXct(df$updated, format = "%Y-%m-%dT%H:%M:%S")
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
                          path = list("biprws/raylight/v1",
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
#' @return Returns a tidy \code{data.frame} of SAP Business Objects Universes
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

#' @title Getting the details of a SAP Business Objects' universe
#' @description Gets the details of a UNX or UNV universe referenced by its ID.
#' @param domain SAP Business Objects domain
#' @param universe_id Universe ID from which to obtain the details
#' @param aggregated Is an optional, boolean/logical parameter that indicates if the outline must be aggregated. Default value is \code{FALSE}.
#' @return Returns the SAP Business Objects Universes details
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/ec5584596fdb101497906a7cb0e91070.html}
#' @examples
#' \dontrun{
#' get_bo_universe_details(domain="YOUR_DOMAIN",
#'                         universe_id=12345)
#' }
#' @export

get_bo_universe_details <- function(domain, universe_id, aggregated=FALSE){
  # Build URL
  url <- httr::modify_url(domain,
                          path = list("biprws/raylight/v1",
                                      "universes", universe_id),
                          query=list(aggregated=tolower(aggregated)))
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
                                              encoding = "UTF-8"))
  return(content$universe)
}


#' @title Upload an Excel File to SAP Business Objects
#' @description Uploads and stores a Microsoft Excel file to the CMS repository.
#' @param domain SAP Business Objects domain 
#' @param excel_file_path Location of the Excel file. Must include directory, file name and file extension. 
#' Must be a Microsoft Excel 2003 or Microsoft Excel 2007 format file.
#' @param folder_id SAP Business Objects folder ID to which the Microsoft Excel file should be stored.
#' @return Returns a message stating the success or failure of the request.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/cb615e388c784ef193c1873455e7ae39.html}
#' @examples
#' \dontrun{
#' upload_excel_to_bo(domain = "YOUR_DOMAIN",
#'                    excel_file_path = "/Users/YOurUsername/Downloads/example_file.xlsx",
#'                    folder_id = 123456
#' )
#' }
#' @export

upload_excel_to_bo <- function(domain, excel_file_path, folder_id){
  # Build url
  url <- httr::modify_url(domain, path = c("biprws/raylight/v1", "spreadsheets"))
  # Build form data
  file_info <-list(spreadsheet=list(
    name=basename(excel_file_path),
    folderId=folder_id))
  # Create body
  body <- list(
    attachmentInfos = curl::form_data(jsonlite::toJSON(file_info, auto_unbox=TRUE), type = "application/json"),
    attachmentContent = httr::upload_file(excel_file_path)
  )
  # POST request
  request <- httr::POST(url = url, body = body, encode="multipart",
                        httr::accept_json(),
                        httr::add_headers("Content-Type" = "multipart/form-data", get_token())
  )
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
  # Read content
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  # Message
  message(content$success$message)
}


#' @title Update an Excel File from SAP Business Objects
#' @description Updates an already-stored Microsoft Excel file in the CMS repository.
#' The current file and the one to upload must have the same file structure (columns number, names, and order).
#' @param domain SAP Business Objects domain 
#' @param excel_file_path Location of the Excel file. Must include directory, file name and file extension. 
#' Must be a Microsoft Excel 2003 or Microsoft Excel 2007 format file.
#' @param spreadsheet_id ID of the Excel file in the CMS repository.
#' @return Returns a message stating the success or failure of the request.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/58f583a7643e48cf944cf554eb961f5b/4.2/en-US/cb615e388c784ef193c1873455e7ae39.html}
#' @examples
#' \dontrun{
#' update_bo_excel_file(domain = "YOUR_DOMAIN",
#'                    excel_file_path = "/Users/YOurUsername/Downloads/example_file.xlsx",
#'                    spreadsheet_id = 123456
#' )
#' }
#' @export

update_bo_excel_file <- function(domain, excel_file_path, spreadsheet_id){
  # Build url
  url <- httr::modify_url(domain, path = c("biprws/raylight/v1", "spreadsheets", spreadsheet_id))
  # Create body
  body <- list(
    attachmentContent = httr::upload_file(excel_file_path)
  )
  # PUT request
  request <- httr::PUT(url = url, body = body, encode="multipart",
                       httr::accept_json(),
                       httr::add_headers("Content-Type" = "multipart/form-data", get_token())
  )
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
  # Read content
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  # Message
  message(content$success$message)
}


#' @title Retrieve all the children elements of a folder
#' @description Retrieves all the children ressources with name, description, id and type from a given Folder ID.
#' The current file and the one to upload must have the same file structure (columns number, names, and order).
#' @param domain SAP Business Objects domain 
#' @param folder_id Folder ID in the CMS repository of the folder to retrieve its children from (Equals to the \code{SI_ID} variable).
#' @param type Optional parameter to define the type of elements to retrieve. Must be a single element (Equals to the \code{SI_KIND} variable).
#' Some of the types of the elements to retrieve are the following (incomplete list)
#' \itemize{
#' \item \code{"InfoView"}
#' \item \code{"Webi"}
#' \item \code{"Publication"}
#' \item \code{"Folder"}
#' }
#' @param page For more than 50 elements within a folder pagination is necessary. Use this parameter to control pagination.
#' @param pageSize Page size of the number of ressources to obtain. Maximum is 50, defaults to 50.
#' @return Returns a tidy \code{data.frame} of all the children elements within the folder 
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @references \url{https://help.sap.com/viewer/db6a17c0d1214fd6971de66ea0122378/4.2.3/en-US/45aa8bae6e041014910aba7db0e91070.html}
#' @examples
#' \dontrun{
#' get_bo_folder_ressources(domain = "YOUR_DOMAIN",
#'                          folder_id = 12345,
#'                          type = Webi
#' )
#' }
#' @export

get_bo_folder_ressources <- function(domain, folder_id, type=NULL, page=NULL, pageSize=50){
  # Build url
  url <- httr::modify_url(domain, path = c("/biprws/infostore",folder_id , "children"),
                          query=list(type=type, page=page, pageSize=pageSize))
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
  # Read content
  content <- jsonlite::fromJSON(httr::content(request, "text",
                                              encoding = "UTF-8"),
                                simplifyDataFrame = TRUE)
  # Message
  return(content$entries)
}