if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2"
)

load_metric <- function(filename, metric, all=F) {
  df <- read.csv(textConnection(gsub("\\s+", ",", readLines(filename))), header = F, stringsAsFactors = F)
  names(df) <- c("metric", "topic", "value")
  if (all) {
    df <- df[df$metric == metric & df$topic == "all", ]
  } else {
    df <- df[df$metric == metric & df$topic != "all", ]
  }
  df$value <- as.numeric(df$value)
  df
}

load_metric_for_runs <- function(run_ids, metric, all=F) {
  do.call(rbind, lapply(run_ids, function(run_id) {
    cbind(run=run_id, load_metric(paste("data/", run_id, ".eval.gz", sep=""), metric, all = all))
  }))
}

run_ids <- paste("feup-run", 1:8, sep="")
avg_map <- load_metric_for_runs(run_ids, "map", all = T)
avg_p_10 <- load_metric_for_runs(run_ids, "P_10", all = T)

map <- load_metric_for_runs(run_ids, "map", all = F)

wilcox <- rbind(
  data.frame(
    run1="feup-run1",
    run2="feup-run3",
    p_value=wilcox.test(map[map$run == "feup-run1", "value"], map[map$run == "feup-run3", "value"])$p.value
  ),
  data.frame(
    run1="feup-run1",
    run2="feup-run6",
    p_value=wilcox.test(map[map$run == "feup-run1", "value"], map[map$run == "feup-run6", "value"])$p.value
  )
)

top_map <- head(map[order(map$value, decreasing = T), ], 12)
zero_shuffled_map <- head(map[map$value == 0, ][sample(1:length(which(map$value == 0))), ], 10)