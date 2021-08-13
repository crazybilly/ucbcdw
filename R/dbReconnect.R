#' Is tbl a database table (or is it local)
#'
#' @param tblname an object name (not a string) to test
#'
#' @return a logical value indicating whether or not the object is a tbl_sql, ie. TRUE means it's a database tbl, as opposed to a local tbl
#' @export
is_db_tbl  <- function(tblname) {

  get(tblname) %>%
    class() %>%
    str_detect("tbl_sql") %>%
    any()
}


dbMakeTableObj  <- function(tblname, con = cdw) {

  x  <- tbl(con, in_schema("CDW", str_to_upper(tblname)) )

  assign(tblname, x, env = rlang::global_env())

}

#' Reconnect a database table
#'
#' @param thetbl a tbl_sql to reconnect, or a character string of the object name. Note that when thetbl is specified as a string, and the connection has already been disconnected, the function cannot reconnect.
#' @param con a database connection to use
#' @param assign_global a logical value determining whether or not the reconnected table should be written invisibly back to the global environment or should be returned.
#'
#' @return if assign_global is TRUE, then returns TRUE invisibly. Otherwise, it returns the reconnected table
#' @export
#'
dbRemakeTbl <- function(thetbl, con = cdw, assign_global = T) {

  if(any(class(thetbl) == 'character')) {

    thetbl  <- get(thetbl)
    thename  <- thetbl

  } else {

    thename  <- deparse(substitute(thetbl))

  }


  the_query  <- sql_render(thetbl)

  new_obj <- tbl(con, the_query)

  if(assign_global) {


    assign(thename, new_obj, env = rlang::global_env())
    invisible(TRUE)


  } else {
    new_obj
  }

}


# should eventually add some sort of way to specify which connection to reconnect
#   but is dependent on figuring out how to connect tbl_sql with their parent connection

#' dbReconnect
#'
#' @description reconnect to a database and reinitialize all the tables. NOTE: doesn't work right if you have more than one active database connection.
#'
#' @param con a existing database connection to reconnect to
#' @param reconnect_tables a logical value specifying whether all database tables should be reconncted using con. If you have more than one database connection, you should set this to FALSE, as dbReconnect() has not way to know which connection you used to create a table and will try to use con to recreate all your tbl_sql object with con.
#' @param reconnect_all_tbls_with_this_con a logical value used to override warnings about multiple database connections. FALSE by default, meaning this function will error out if you have multiple database connections and try to reconnect tables. Set to TRUE to override the warning and reconnect all tables with the connection specified in con.
#'
#' @details This function writes to the global environment, overwriting an existing database connection with a valid one, and if reocnnect_tables = TRUE, reconnecting all tbl_sql objects with the newly written, valid connection. Note that this can be really problematic if you have more than one active database connection--there's not a good way to know which connetion was used for each table, so the function will error out if you have more than one database connection and are trying to reconnec all the tables.
#'
#' @return returns TRUE invisibly
#'
#' @export
dbReconnect  <- function(con = cdw, reconnect_tables = TRUE, reconnect_all_tbls_with_this_con = FALSE)  {

  n_oracle_connections  <- ls() %>%
    # loops the global environment looking for
    map_lgl(~ any(get(.x) %>% class %>% map_lgl(~ any(.x == 'Oracle')) )) %>%
    sum

  if(n_oracle_connections > 1 & reconnect_tables) {
    stop("You have more than one valid database connection and I don't know which connection feeds each dervied tbl_sql object. Use reconnect_all_tbls_with_this_con = TRUE to override and continue ")
  }



  # get a list of current tbl_sql objects and their underlying queries
  all_objs_str  <- ls(envir = rlang::global_env())
  all_objs_class  <- map(all_objs_str, ~get(.x) %>% class)
  tablesi  <- map_lgl(all_objs_class, ~ any(str_detect(.x, "tbl_sql")) )
  dbtables <- all_objs_str[tablesi]
  tblqueries  <- map(dbtables, ~ get(.x) %>%  sql_render)


  # get info about the database connection
  con_name  <- deparse(substitute(con))

  if(!exists(con_name)) {
    stop("Please enter an existing database connection.")
  }

  con_source  <- con@info$sourcename


  # reconnect the database and reassign back to the global environment
  DBI::dbDisconnect(con)

  newcon  <- DBI::dbConnect(odbc::odbc(), con_source, uid = Sys.getenv("CDW_USR"), pwd = Sys.getenv("CDW_PWD"), timeout = 10)
  assign(con_name, newcon, env = rlang::global_env())


  # reconnect all the database tables
  if(reconnect_tables) {


    map2(tblqueries, dbtables, ~ tbl(newcon, .x) %>% assign(.y, ., env = rlang::global_env()) )

  }



  # need to figure out how to match tables up with db connections when you have more than one connection

  invisible(TRUE)

}


