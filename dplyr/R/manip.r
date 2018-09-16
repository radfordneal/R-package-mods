#' Data manipulation functions.
#'
#' These five functions form the backbone of dplyr. They are all S3 generic
#' functions with methods for each individual data type. All functions work
#' exactly the same way: the first argument is the tbl, and the
#' subsequence arguments are interpreted in the context of that tbl.
#'
#' @section Manipulation functions:
#'
#' The five key data manipulation functions are:
#'
#' \itemize{
#'   \item filter: return only a subset of the rows. If multiple conditions are
#'     supplied they are combined with \code{&}.
#'   \item select: return only a subset of the columns. If multiple columns are
#'     supplied they are all used.
#'   \item arrange: reorder the rows. Multiple inputs are ordered from left-to-
#'    right.
#'   \item mutate: add new columns. Multiple inputs create multiple columns.
#'   \item summarise: reduce each group to a single row. Multiple inputs create
#'     multiple output summaries.
#' }
#'
#' These are all made significantly more useful when applied by group,
#' as with \code{\link{group_by}}
#'
#' @section Tbls:
#'
#' dplyr comes with three built-in tbls.  Read the help for the
#' manip methods of that class to get more details:
#'
#' \itemize{
#'   \item data.frame: \link{manip_df}
#'   \item data.table: \link{manip_dt}
#'   \item SQLite: \code{\link{src_sqlite}}
#'   \item PostgreSQL: \code{\link{src_postgres}}
#'   \item MySQL: \code{\link{src_mysql}}
#' }
#'
#' @section Output:
#'
#' Generally, manipulation functions will return an output object of the
#' same type as their input. The exceptions are:
#'
#' \itemize{
#'    \item \code{summarise} will return an ungrouped source
#'    \item remote sources (like databases) will typically return a local
#'      source from at least \code{summarise} and \code{mutate}
#' }
#' @name manip
#' @param .data a tbl
#' @param ... variables interpreted in the context of that data frame.
#' @examples
#' filter(mtcars, cyl == 8)
#' select(mtcars, mpg, cyl, hp:vs)
#' arrange(mtcars, cyl, disp)
#' mutate(mtcars, displ_l = disp / 61.0237)
#' summarise(mtcars, mean(disp))
#' summarise(group_by(mtcars, cyl), mean(disp))
NULL

#' @rdname manip
#' @export
filter <- function(.data, ...) UseMethod("filter")

#' @rdname manip
#' @export
summarise <- function(.data, ...) UseMethod("summarise")

#' @rdname manip
#' @export
mutate <- function(.data, ...) UseMethod("mutate")

#' @rdname manip
#' @export
arrange <- function(.data, ...) UseMethod("arrange")

#' @rdname manip
#' @export
select <- function(.data, ...) UseMethod("select")

#' The number of observations in the current group.
#' 
#' This function is implemented special for each data source and can only
#' be used from within \code{\link{summarise}}.
#' 
#' @export
#' @examples
#' if (require("hflights")) {
#' carriers <- group_by(hflights, UniqueCarrier)
#' summarise(carriers, n())
#' }
n <- function() {
  stop("This function should not be called directly")
}
