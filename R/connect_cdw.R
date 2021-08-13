#' Connect to the CDW (or another odbc connection)
#'
#' @param odbc_name a character string of the name of the odbc connection that you want to connect to
#' @param name_to_check a character string of the object you want to check. Recommended to set this to the same name you're currently assigning to, in order to ensure that you recreate a new connection, but with the same name.
#' @param username the username to use with the odbc connection. It's recommended to save your username and password as environment variables in your .RProfile
#' @param password the password to use with the odbc connection. It's recommended to save your username and password as environment variables in your .RProfile
#'
#' @return a database connection. If name_to_check exists and is valid, then the name_to_check object is returned. If name_to_check is an invalid database connection (ie. DBI::dbIsValid(name_to_check) == FALSE), or if name_to_check does not exist, a new connection is created and returned. It is recommended to use the same name in name_to_check as whatever object you're assigning the value of this function to.
#' @export

connect_cdw  <- function( odbc_name = 'CDW2_64', name_to_check = 'cdw', username = Sys.getenv('CDW_USR'), password = Sys.getenv("CDW_PWD")   ) {

  if( !(exists(name_to_check))  ) {
    DBI::dbConnect(odbc::odbc(), odbc_name, uid = username, pwd = password, timeout = 10)
  } else if( !DBI::dbIsValid(get(name_to_check)) ) {
    DBI::dbConnect(odbc::odbc(), odbc_name, uid = username, pwd = password, timeout = 10)
  } else {
    get(name_to_check)
  }

}
