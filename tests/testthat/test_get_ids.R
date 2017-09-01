
library(entrezquery)
context("getids")

test_that("character vector is returned", {
  expect_equal(class(get_ids('expression profiling by high throughput sequencing[DataSet Type]', db = 'gds', retmax = 4)), "character")
})

