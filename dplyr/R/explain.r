#' Show sql and query plans.
#'
#' Any queries run inside this function will automatically be explained:
#' displaying information about which indexes are used to optimise the query.
#' This requires a little bit of knowledge about how \code{EXPLAIN} works for
#' your database, but is very useful for diagnosing performance problems.
#'
#' @param code code to run. All sql queries executed during the running of the
#'   code will be shown and explained.
#' @param tbl an sql-based table to explain.
#' @export
#' @examples
#' \donttest{
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'
#' batting <- tbl(lahman_sqlite(), "Batting")
#'
#' # Note that you have to do something that actually triggers a query
#' # inside the explain function
#' explain_sql(nrow(batting))
#' explain_sql(nrow(batting))
#'
#' # nrow requires two queries the first time because it's the same as dim(x)[1]
#' # but the results are cached
#'
#' show_sql(head(batting))
#' explain_sql(head(batting))
#'
#' # If you just want to understand the sql for a tbl, use explain
#' explain(batting)
#'
#' # The batting database has indices on all ID variables:
#' # SQLite automatically picks the most restrictive index
#' explain(filter(batting, lgID == "NL" & yearID == 2000))
#'
#' # OR's will use multiple indexes
#' explain(filter(batting, lgID == "NL" | yearID == 2000))
#' }
#' }
explain_sql <- function(code) {
  old <- options(dplyr.explain_sql = TRUE, dplyr.show_sql = TRUE)
  on.exit(options(old))

  code
}

#' @export
#' @rdname explain_sql
show_sql <- function(code) {
  old <- options(dplyr.show_sql = TRUE)
  on.exit(options(old))

  code
}

#' @export
#' @rdname explain_sql
explain <- function(tbl) {
  force(tbl)
  message(
    "<SQL>\n", tbl$query$sql,
    "\n\n",
    "<PLAN>\n", qry_explain(tbl$query$con, tbl$query$sql)
  )

  invisible(NULL)
}
