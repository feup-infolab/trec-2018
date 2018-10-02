if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "xml2",
  "stringr"
)

doc_id_title_url <- read.csv("output/wapo-id_title_url-dictionary.csv.gz", fill = F)

topics <- read_xml("data/2018-test-topics.xml")
topics <- xml_text(xml_find_all(topics, "//*[self::num or self::title]"))
topics <- split(str_trim(topics), sort(rep(c(1:50), 2)))
topics <- do.call(rbind, lapply(topics, function(t) data.frame(topic=str_sub(t[1], 9), query=t[2])))

runs <- lapply(c("data/feup-run1.res.gz", "data/feup-run2.res.gz"), function(filename) {
  top_10 <- setNames(read.table(filename, header = F, sep = " "),
                     c("topic", "q0", "id", "rank", "score", "run_id"))
  top_10 <- lapply(split(top_10, top_10$topic), head, 10)
  top_10 <- lapply(top_10, function(d) {
    df <- merge(d, doc_id_title_url, by="id", all.x = T)
    df <- merge(df, topics, by="topic", all.x = T)
    df[order(df$rank), c("topic", "query", "rank", "score", "id", "title", "url")]
  })
  top_10 <- do.call(rbind, top_10)
  top_10
})
names(runs) <- c("feup-run1", "feup-run2")

for (run_id in names(runs)) {
  filename <- sprintf("output/%s-top_10-info.csv", run_id)
  write.csv(runs[[run_id]], filename, row.names = F)
}