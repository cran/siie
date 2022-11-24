
#' @title Calculation of SII and SIE
#' @description Calculate SII (Superior Identification Index) and
#'   SIE (Superior Identification Efficiency) for each group with individual values.
#'   In the context of the paper, we have citation counts of papers from different journals.
#'   This function could calculate SII and SIE for each journal within the field.
#' @param df A data.frame containing at least two columns (namely the group and the
#'   index of each individual).
#' @param group The group avariable. In the context of our paper,
#'  this could be the name or ISSN of a journal.
#' @param index The indicator of individuals. In the context of our paper,
#'   this could be citation index of papers.
#' @param p Cutoff of superior. Defaults to 10, meaning top 10 percent individuals are
#'   regarded as superior.
#' @return A data.table with 5 columns, with the group, superior number (superior_no),
#'   total number in the group (total_no), SII (sii) and SIE (sie).
#' @details In the context, SII indicates how well a journal could identify the
#'   top papers (superior research), whereas SIE quantifies the efficiency of
#'   a journal to identify the top papers.
#' @references Huang, TY., Yang, L. Superior identification index: Quantifying the capability of academic journals to recognize good research. Scientometrics 127, 4023–4043 (2022). https://doi.org/10.1007/s11192-022-04372-z
#' @importFrom stats quantile
#' @examples
#'
#' set.seed(19960822)
#' nr_of_rows = 1e4
#' data.frame(
#'   Id = 1:1e4,
#'   Journal = sample(LETTERS,nr_of_rows,replace = TRUE),
#'   CiteCount = sample(1:100,nr_of_rows,replace = TRUE)
#' ) -> journal_table
#' siie(journal_table,group = "Journal",index = "CiteCount")
#'
#' @export

siie = \(df,group,index,p = 10){
  df = as.data.table(df)
  setnames(df,old = c(group,index),new = c("group","index"))
  quantile(df$index,1 - p/100) -> cutoff
  df[index >= cutoff,.(superior_no = .N),by = group]  -> s_no

  df[,.(total_no = .N),by = group] -> g_no
  setnames(
    merge(s_no,g_no,by = "group",all = TRUE)[is.na(superior_no),superior_no:=0][
      ,`:=`(sii = superior_no/sum(superior_no),sie = superior_no/total_no)],
    old = "group",new = group
  )[]
}

globalVariables(c(".",".SD",".N","superior_no","total_no"))




