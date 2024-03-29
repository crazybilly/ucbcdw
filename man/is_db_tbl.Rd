% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbReconnect.R
\name{is_db_tbl}
\alias{is_db_tbl}
\title{Is tbl a database table (or is it local)}
\usage{
is_db_tbl(tblname)
}
\arguments{
\item{tblname}{an object, or a character string of an object's name, to test}
}
\value{
a logical value indicating whether or not the object is a tbl_sql, ie. TRUE means it's a database tbl, as opposed to a local tbl. If tblname is a string (as opposed the actual object itself), the function will get(tblname) and test that.
}
\description{
Is tbl a database table (or is it local)
}
