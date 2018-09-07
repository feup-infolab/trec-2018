if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  proxy,
  logging
)

basicConfig()

#
# Load document profiles
#

dp_r1_bin <- read.table(
  "data/run1-document_profile-binary_weights-with_pruning.tsv.gz",
  header = T,
  row.names = 1)

dp_r1_real <- read.table(
  "data/run1-document_profile-real_weights-without_pruning.tsv.gz",
  header = T,
  row.names = 1)

dp_r2_bin <- read.table(
  "data/run2-document_profile-binary_weights-with_pruning-without_keywords-top_100_results.tsv.gz",
  header = T,
  row.names = 1)
dp_r2_bin <- dp_r2_bin[, -which(colnames(dp_r2_bin) == "Language")]

#
# Load runs and their document profile configuration
#
# run1 and run2 are the base runs and didn't use any document profile,
# so we measure diversity based on all available features.
#

base_runs <- list(
  run1=setNames(
    read.table("data/feup-run1.res.gz", header = F, sep = " "),
    c("topic", "q0", "doc_id", "rank", "score", "run_id")),
  run2=setNames(
    read.table("data/feup-run2.res.gz", header = F, sep = " "),
    c("topic", "q0", "doc_id", "rank", "score", "run_id"))
)

dp_runs <- list(
  run3=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "Keywords",
      "NamedEntities",
      "SentimentAnalysis",
      "ReadingComplexity",
      "EmotionCategories"
    ),
    binary_weights=TRUE,
    base_run_id="run1"
  ),
  
  run4=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "SentimentAnalysis",
      "ReadingComplexity"
    ),
    binary_weights=TRUE,
    limit=100,
    base_run_id="run2"
  ),
  
  run5=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "Keywords",
      "NamedEntities",
      "SentimentAnalysis",
      "ReadingComplexity",
      "EmotionCategories"
    ),
    binary_weights=TRUE,
    limit=Inf,
    base_run_id="run1"
  ),
  
  run6=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "SentimentAnalysis",
      "ReadingComplexity",
      "EmotionCategories"
    ),
    binary_weights=TRUE,
    limit=100,
    base_run_id="run2"
  ),
  
  run7=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "SentimentAnalysis",
      "ReadingComplexity"
    ),
    binary_weights=TRUE,
    limit=Inf,
    base_run_id="run1"
  ),
  
  run8=list(
    data=setNames(
      read.table("data/feup-run3.res.gz", header = F, sep = " "),
      c("topic", "q0", "doc_id", "rank", "score", "run_id")),
    features=c(
      "NamedEntities",
      "SentimentAnalysis",
      "ReadingComplexity",
      "EmotionCategories"
    ),
    binary_weights=FALSE,
    limit=Inf,
    base_run_id="run1"
  )
)

#
# Calculate weighted diversities
#

cache_dist <- list()

weighted_diversity <- function(base_run_id, data, features, binary_weights, limit=Inf) {
  cache_key <- ""

  if (binary_weights) {
    if (base_run_id == "run1") {
      dp <- dp_r1_bin
    } else {
      dp <- dp_r2_bin
    }
    cache_key <- paste(cache_key, base_run_id, "bin", sep = "_")
  } else {
    dp <- dp_r1_real
    cache_key <- paste(cache_key, base_run_id, "real", sep = "_")
  }
  
  cache_key <- paste(cache_key, paste(sort(features), collapse = "_"), sep = "_")
  
  if (is.null(cache_dist[[cache_key]])) {
    loginfo("Computing cosine distance matrix for %s", cache_key)
    cache_dist[[cache_key]] <- as.matrix(1 - round(proxy::simil(dp, method = "cosine"), 10))
  }
  
  per_topic <- split(data, data$topic)
  
  diversity_per_topic <- sapply(per_topic, function(res) {
    res <- head(res, limit)
    N <- nrow(res)
    weights <- seq(1, 0, -1/(N-1))[-N]
    sum(sapply(1:nrow(res), function(i) {
      doc_id <- as.character(res[i, "doc_id"])
      print(doc_id)
      weights[i] * median(cache_dist[[cache_key]][
        doc_id, -which(colnames(cache_dist[[cache_key]]) == doc_id)])
    }))
  })
  
  median(diversity_per_topic)
}

dp_runs_diversity <- lapply(names(dp_runs), function(run_id) {
  loginfo("Computing weighted diversity for %s", run_id)
  do.call(weighted_diversity, dp_runs[[run_id]])
})