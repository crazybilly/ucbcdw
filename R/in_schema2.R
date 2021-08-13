
#' In Schema2
#'
#' @description a convenience wrapper around \link[dbplyr]{in_schema} that capitalizes the schema and table names. Apparently, some combination of recent versions of ODBC and/or R are case-sensitive with regard to table names (this was not the case prior to October 2020). So, this just capitalizes the table names so you don't have to fight it.
#'
#'
#' @param schema a database schema name
#' @param table the name of a database table
#'
#' @export
#'
in_schema2  <- function(schema, table) {

  schema2  <- stringr::str_to_upper(schema)
  table2   <- stringr::str_to_upper(table)

  dbplyr::in_schema(schema2, table2)

}
