

test_that("prp", {
  library(tidyfst)
  library(data.table)
  data.frame(Journal = LETTERS[c(rep(1,9),2)],
             CiteCount = c(rep(1,9),2)) -> journal_table

  expect_equal(
    prp(journal_table,group = "Journal",index = "CiteCount") %>%
      setDF(),
    data.frame(
      stringsAsFactors = FALSE,
      Journal = c("A", "B"),
      total_no = c(9L, 1L),
      prp = c(20, 10)
    )
  )
})
