


test_that("siie", {
  library(tidyfst)
  library(data.table)
  data.frame(Journal = LETTERS[c(rep(1,9),2)],
             CiteCount = c(rep(1,9),2)) -> journal_table

  expect_equal(
    siie(journal_table,group = "Journal",index = "CiteCount") %>%
      setDF(),
    data.frame(
      stringsAsFactors = FALSE,
      Journal = c("A", "B"),
      superior_no = c(0L, 1L),
      total_no = c(9L, 1L),
      sii = c(0, 1),
      sie = c(0, 1)
    )
  )
})



