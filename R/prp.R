
#' @title Calculation of Paper Rank Percentile (PRP)
#' @description Paper rank percentile refers to the journals' average ranking of papers
#'   within the field. If Journal A has a PRP of 90, it means the papers in Journal A
#'   has an average ranking of 90 out of 100. This metric could be extended to measure
#'   other entities such as institutes and countries.
#' @param df A data.frame containing at least two columns (namely the group and the
#'   index of each individual).
#' @param group The group avariable. In the context of our paper,
#'  this could be the name or ISSN of a journal.
#' @param index The indicator of individuals. In the context of our paper,
#'   this could be citation index of papers.
#' @return A data.table with 3 columns, with the group, total number in the group
#'   (total_no) and PRP.
#' @references Huang, TY., Yang, L. Superior identification index: Quantifying the capability of academic journals to recognize good research. Scientometrics 127, 4023–4043 (2022). https://doi.org/10.1007/s11192-022-04372-z
#' @importFrom data.table ":=" as.data.table setnames setorder rbindlist
#' @examples
#'
#'  set.seed(19960822)
#'  nr_of_rows = 1e4
#'  data.frame(
#'    Id = 1:1e4,
#'    Journal = sample(LETTERS,nr_of_rows,replace = TRUE),
#'    CiteCount = sample(1:100,nr_of_rows,replace = TRUE)
#'  ) -> journal_table
#'
#'  prp(journal_table,group = "Journal",index = "CiteCount")
#'
#' @export

prp = \(df,group,index){
  df = as.data.table(df)
  setnames(df,old = c(group,index),new = c("group","index"))
  df = df[,rank := rank(-index,ties.method = "min")][
    ,.(total_no = .N,rank = mean(rank)*100/nrow(df)),by = group]
  setnames(df,old = c("group","rank"),new = c(group,"prp"))[order(-prp)]
}



