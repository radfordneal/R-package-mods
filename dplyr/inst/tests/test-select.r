context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)


test_that("two selects equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %.% select(l:s) %.% select(n:o),
    ref = select(df, n:o))
})

test_that("select does not loose grouping (#147)", {
  df <- tbl_df(data.frame(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8)))
  grouped <- df %.% group_by(a) %.% select(a, b, x)
  
  expect_equal(groups(grouped), list(quote(a)))
})

