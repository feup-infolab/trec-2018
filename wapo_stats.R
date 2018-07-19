if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2",
  "rjson",
  "logging",
  "plyr",
  "reshape2",
  "stringr"
)

basicConfig()

BATCH_SIZE <- 10000
BIN_WIDTH <- 4000
BINS <- 10

get_document_length <- function(filepath) {
  res <- data.frame(doc_id=character(0), doc_len=integer(0), par_len=integer(0))
  con = file(filepath, "r")
  count <- 0

  while (TRUE) {
    lines = readLines(con, n = BATCH_SIZE)

    if (length(lines) == 0) {
      break
    }

    doc_lens <- lapply(lines, function(line) {
      json <- fromJSON(line)

      pars <- lapply(json$contents, function(c) {
        if (length(c$subtype) > 0 && c$subtype == 'paragraph') {
          gsub('<.*?>', '', c$content)
        } else {
          NULL
        }
      })

      pars <- pars[!sapply(pars, is.null)]

      doc_id = json$id
      doc_len <- nchar(do.call(paste, pars))
      par_len <- nchar(do.call(paste, head(pars, 3)))

      if (length(doc_id) < 1 || length(doc_len) < 1 || length(par_len) < 1) {
        return(data.frame(doc_id=character(0), doc_len=character(0), par_len=character(0)))
      }

      data.frame(doc_id=json$id, doc_len=doc_len, par_len=par_len)
    })

    res <- rbind.fill(c(list(res), doc_lens))

    count <- count + length(lines)
    loginfo("%d documents processed", count)
    #break
  }

  close(con)
  res
}

generate_csv <- function(doc_length, doc_length_path) {
  #dir.create("output/", showWarnings = F)
  #doc_length_path <- c("output/", filename)
  write.csv(doc_length, gzfile(doc_length_path), row.names = F)
}

read_csv <- function(doc_length_path) {
  res <- read.csv(file=gzfile(doc_length_path), header=TRUE, sep=",")
  res
}

plot_data <- function(doc_length) {
  doc_length <- doc_length[order(doc_length$doc_len), ]
  doc_length$n <- 1:nrow(doc_length)
  doc_length <- melt(doc_length, id.vars=c("doc_id", "n"))
  
  ggplot(doc_length, aes(x=n, y=value, color=variable)) +
    geom_smooth() +
    scale_color_discrete("Part", labels=c("Full content", "First three paragraphs")) +
    xlab("Documents") +
    ylab("Length") +
    theme(legend.position="top")
}

read_features_file <- function(features_file_path) {
  res <- read.csv(file=gzfile(features_file_path), header=TRUE, sep="\t")
  res
}

plot_sentiment <- function(features) {
  features <- features[!(is.na(features$SentimentAnalysis) | features$SentimentAnalysis=="null"), ]
  ggplot(features, aes(x=features$SentimentAnalysis, fill=features$SentimentAnalysis)) +
    geom_bar() +
    scale_fill_discrete("Sentiment") +
    xlab("Sentiment") +
    ylab("Documents") +
    theme(legend.position="none")
}

plot_readability <- function(features) {
  features <- features[!(is.na(features$ReadingComplexity) | features$ReadingComplexity=="null"), ]
  features$ReadingComplexity <- sub("~¨-\\*.*$", "", features$ReadingComplexity)
  positions <- factor(features$ReadingComplexity, levels <- c("5th_grade", "6th_grade", "7th_grade",  "8th_9th_grade", "10th_12th_grade", "College", "College_graduate"))
  ggplot(features, aes(x=positions, fill=positions)) +
    #ggplot(features, aes(x=sub("~¨-\\*.*$", "", features$ReadingComplexity), fill=sub("~¨-\\*.*$", "", features$ReadingComplexity))) +
    geom_bar() +
    scale_fill_brewer("Levels") +
    xlab("Levels") +
    ylab("Documents") +
    theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))#axis.text=element_text(size=4))
}
#plot_readability(features)

plot_named_entities <- function(features) {
  features <- features[!(is.na(features$NamedEntities) | features$NamedEntities=="null"), ]
  entities <- as.data.frame(table(unlist(str_split(features$NamedEntities, "\\|")), dnn = list("entity")), responseName = "freq")
  entities <- head(entities[order(entities$freq, decreasing = TRUE), ], 20)
  ggplot(entities, aes(x=reorder(entity, freq), y=freq, fill = freq)) + 
  geom_bar(stat="identity") +
  scale_fill_gradient(low = "blue", high = "blue") + 
  xlab("Entities") +
  ylab("Documents") +
  theme(legend.position="none") +
  coord_flip() 
}
#plot_named_entities(features)

plot_emotions_ocurrences <- function(features) {
  features <- features[!(is.na(features$EmotionCategories) | features$EmotionCategories=="null"), ]
  #features$EmotionCategoriesName <- sub("~¨-\\*.*$", "", features$EmotionCategories)
  features$EmotionCategoriesValue <- gsub("\\]", "", gsub("\\[", "", gsub("^.*~¨-\\*", "", features$EmotionCategories)))
  features$EmotionCategoriesList <- gsub(", ", "|", gsub(";[^\\,]*", "", features$EmotionCategoriesValue))
  emotlist <- as.data.frame(table(unlist(str_split(features$EmotionCategoriesList, "\\|")), dnn = list("emot")), responseName = "freq")
  ggplot(emotlist, aes(x=reorder(emot, freq), y=freq, fill = freq)) + 
    geom_bar(stat="identity") +
    scale_fill_gradient(low = "blue", high = "blue") + 
    xlab("Emotions") +
    ylab("Documents") +
    theme(legend.position="none") +
    coord_flip() 
}
#plot_emotions_ocurrences(features)


plot_emotions_weight <- function(features) {
  features <- features[!(is.na(features$EmotionCategories) | features$EmotionCategories=="null"), ]
  #features$EmotionCategoriesName <- sub("~¨-\\*.*$", "", features$EmotionCategories)
  features$EmotionCategoriesValue <- gsub("\\]", "", gsub("\\[", "", gsub("^.*~¨-\\*", "", features$EmotionCategories)))
  features$EmotionCategoriesList <- gsub(", ", "|", features$EmotionCategoriesValue)
  emotlist <- as.data.frame(table(unlist(str_split(features$EmotionCategoriesList, "\\|")), dnn = list("emot")), responseName = "freq")
  emotlist
}
#plot_emotions_weight(features)

#loginfo("Getting document length and first-three-paragraphs length")
#doc_length <- get_document_length("~/Downloads/WashingtonPost.v2/data/TREC_Washington_Post_collection.v2.jl")

#loginfo("Saving result")
#generate_csv(doc_length = doc_length, doc_length_path = "output/wapo-doc_length-all.csv.gz")

#loginfo("Getting document length from file")
#doc_length <- read_csv(doc_length_path = "~/gitprojects/wapo-analytics/output/wapo-doc_length-all.csv.gz")

#loginfo("Draw graph from data")
#plot_data(doc_length = doc_length)

#loginfo("Getting features from file")
#features <- read_features_file("~/Descargas/feat-small.tsv")
#features <- read_features_file("~/Downloads/features-basic6-trec-notext-p1.tsv.gz")

#loginfo("Draw graph sentiment")
#plot_sentiment(features)

#plot_readability(features)

#plot_named_entities(features)
