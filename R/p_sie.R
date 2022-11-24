

#' @title Get data for p-SIE curve
#' @description Calculate data for p-SIE curve.
#' @param df A data.frame containing at least two columns (namely the group and the
#'   index of each individual).
#' @param group The group avariable. In the context of our paper,
#'  this could be the name or ISSN of a journal.
#' @param index The indicator of individuals. In the context of our paper,
#'   this could be citation index of papers.
#' @param p A group of p (Cutoff of superior). Defaults to integers from 1 to 100.
#' @param to_compare Which groups to compare with.
#' @return A data.table with 3 columns, with the group, p and the according SIE.
#' @references Huang, TY., Yang, L. Superior identification index: Quantifying the capability of academic journals to recognize good research. Scientometrics 127, 4023–4043 (2022). https://doi.org/10.1007/s11192-022-04372-z
#' @examples
#' library(ggplot2)
#' library(tidyfst)
#'
#' set.seed(19960822)
#' nr_of_rows = 1e4
#' data.frame(
#'   Id = 1:1e4,
#'   Journal = sample(LETTERS,nr_of_rows,replace = TRUE),
#'   CiteCount = sample(1:100,nr_of_rows,replace = TRUE)
#' ) -> journal_table
#'
#' p_sie(journal_table,group = "Journal",
#'       index = "CiteCount",to_compare = c("A","B","C")) -> p_sie_df
#'
#' p_sie_df
#'
#' p_sie_df %>%
#'   ggplot(aes(p/100,sie,color = Journal)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_abline(slope = 1,linetype = "dashed") +
#'   scale_x_continuous(labels = tidyfst::percent) +
#'   scale_y_continuous(labels = tidyfst::percent) +
#'   labs(x = "p",y = "SIE") +
#'   theme_bw()

#' @export
p_sie = \(df,group,index,p = 1:100,to_compare){
  rbindlist(
    lapply(p,\(x){
      siie(df,group = group,index = index,p = x) [,p:=x]
    })
  ) -> dt
  dt[dt[[group]] %in% to_compare,.SD,.SDcols = c(group,"p","sie")]
}

#' @import data.table

