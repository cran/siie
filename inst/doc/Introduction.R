## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(19960822)
nr_of_rows = 1e4
data.frame(
  Id = 1:1e4,
  Journal = sample(LETTERS,nr_of_rows,replace = TRUE),
  CiteCount = sample(1:100,nr_of_rows,replace = TRUE)
) -> journal_table

## -----------------------------------------------------------------------------
library(siie)
library(tidyfst)

journal_table %>% siie(group = "Journal",index = "CiteCount")

## ----eval=FALSE---------------------------------------------------------------
#  journal_table %>% siie(group = "Journal",index = "CiteCount",p = 1)

## -----------------------------------------------------------------------------
prp(journal_table,group = "Journal",index = "CiteCount")

## ----out.width="90%"----------------------------------------------------------
library(ggplot2)

p_sie(journal_table,group = "Journal",
      index = "CiteCount",to_compare = c("A","B","C")) -> p_sie_df

p_sie_df

p_sie_df %>%
  ggplot(aes(p/100,sie,color = Journal)) +
  geom_point() +
  geom_line() +
  geom_abline(slope = 1,linetype = "dashed") +
  scale_x_continuous(labels = tidyfst::percent) +
  scale_y_continuous(labels = tidyfst::percent) +
  labs(x = "p",y = "SIE") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.3),
        legend.background = element_rect(size=0.5,
                                         color = "black",linetype="solid"))

