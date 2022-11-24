

test_that("p_sie", {
  library(tidyfst)
  data.frame(Journal = LETTERS[c(rep(1,9),2)],
             CiteCount = c(rep(1,9),2)) -> journal_table

  expect_equal(
    p_sie(journal_table,group = "Journal",index = "CiteCount",p = c(10,100),
          to_compare = c("A","B")),
    data.table(
      Journal = c("A", "B", "A", "B"),
      p = c(10, 10, 100, 100),
      sie = c(0, 1, 1, 1)
    )
  )
})


