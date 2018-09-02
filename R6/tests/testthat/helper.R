expect_no_error <- function(expr) {
  err <- FALSE
  tryCatch(force(expr),
    error = function(e) {
      err <<- TRUE
    }
  )

  # Changed for pqR to allow testthat that lacks 'expect'.
  # Was:  expect(!err, "Expected no error, but had error.")

  expect_false(err)

  invisible(NULL)
}