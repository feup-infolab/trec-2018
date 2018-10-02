if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2"
)

doc_id_title_url <- read.csv("output/wapo-id_title_url-dictionary.csv.gz", fill = F)

runs <- lapply(c("data/feup-run1.res.gz", "data/feup-run2.res.gz"), function(filename) {
  top_10 <- setNames(read.table(filename, header = F, sep = " "),
                     c("topic", "q0", "doc_id", "rank", "score", "run_id"))
  top_10 <- lapply(split(top_10, top_10$topic), head, 10)
  top_10
})