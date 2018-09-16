library("testthat")
library("dplyr")

library("Lahman")
lahman_sqlite()
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  lahman_postgres()  
}

options(warn=1)

test_package("dplyr")
