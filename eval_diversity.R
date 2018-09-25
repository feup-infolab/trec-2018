if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  proxy,
  logging,
  memoise
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
    limit=Inf,
    base_run_id="run1"
  ),
  
  run4=list(
    data=setNames(
      read.table("data/feup-run4.res.gz", header = F, sep = " "),
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
      read.table("data/feup-run5.res.gz", header = F, sep = " "),
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
      read.table("data/feup-run6.res.gz", header = F, sep = " "),
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
      read.table("data/feup-run7.res.gz", header = F, sep = " "),
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
      read.table("data/feup-run8.res.gz", header = F, sep = " "),
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

get_dp_dist <- function(base_run_id, features, binary_weights) {
  loginfo(
    "Computing cosine distance matrix for base_run_id = %s, features = %s, binary_weights = %s",
    base_run_id, paste(features, collapse = ", "), as.character(binary_weights))

  if (binary_weights) {
    if (base_run_id == "run1") {
      dp <- dp_r1_bin
    } else {
      dp <- dp_r2_bin
    }
  } else {
    dp <- dp_r1_real
  }
  
  dp <- dp[, grep(sprintf("^(%s)", paste(features, collapse = "|")), colnames(dp))]

  as.matrix(1 - round(proxy::simil(dp, method = "cosine"), 10))
}
get_dp_dist <- memoise(get_dp_dist, cache = cache_filesystem("output/dp_dist_cache"))

weighted_geometric_mean <- function(x, w) exp(sum(w * log(x)) / sum(w))

weighted_diversity <- function(base_run_id, data, features, binary_weights, limit=Inf) {
  dp_dist <- get_dp_dist(base_run_id, sort(features), binary_weights)

  per_topic <- split(data, data$topic)
  
  diversity_per_topic <- sapply(per_topic, function(res) {
    res <- head(res, limit)
    N <- nrow(res)
    #weights <- seq(1, 0, -1/(N-1))[-N]
    weights <- 1 / seq(1, N)
    weights <- weights / norm(as.matrix(weights))

    sum(sapply(1:(N-1), function(i) {
      doc_id <- as.character(res[i, "doc_id"])
      if (doc_id %in% colnames(dp_dist)) {
        weights[i] * mean(dp_dist[doc_id, colnames(dp_dist) %in% res$doc_id & colnames(dp_dist) != doc_id])
        #weighted_geometric_mean(..., weights)
      } else {
        logwarn("No document profile found for document %s, skipping", doc_id)
        0
      }
    }))
  })

  list(vector=diversity_per_topic, mean=mean(diversity_per_topic))
}

dp_runs_diversity <- lapply(names(dp_runs), function(run_id) {
  loginfo("Computing weighted diversity for %s", run_id)
  do.call(weighted_diversity, dp_runs[[run_id]])
})
names(dp_runs_diversity) <- names(dp_runs)

dp_runs_diversity_base_compare <- lapply(names(dp_runs), function(run_id) {
  base_run_id <- dp_runs[[run_id]]$base_run_id

  base_diversity <- weighted_diversity(
    base_run_id,
    base_runs[[base_run_id]],
    features = sort(dp_runs[[run_id]]$features),
    binary_weights = dp_runs[[run_id]]$binary_weights,
    limit = dp_runs[[run_id]]$limit)

  c(
    base=base_diversity$mean,
    rerank=dp_runs_diversity[[run_id]]$mean,
    wilcox=wilcox.test(base_diversity$vector, dp_runs_diversity[[run_id]]$vector)$p.value)
})
dp_runs_diversity_base_compare <- do.call(rbind, setNames(dp_runs_diversity_base_compare, names(dp_runs)))
dp_runs_diversity_base_compare
