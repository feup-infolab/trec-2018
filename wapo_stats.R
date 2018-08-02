if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2",
  "rjson",
  "logging",
  "plyr",
  "reshape2",
  "stringr",
  "lubridate",
  "proxy"
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

generate_emotions_weight <- function(features) {
  features <- features[!(is.na(features$EmotionCategories) | features$EmotionCategories=="null"), ]
  #features$EmotionCategoriesName <- sub("~¨-\\*.*$", "", features$EmotionCategories)
  features$EmotionCategoriesValue <- gsub("\\]", "", gsub("\\[", "", gsub("^.*~¨-\\*", "", features$EmotionCategories)))
  features$EmotionCategoriesList <- gsub(", ", "|", features$EmotionCategoriesValue)
  emotlist <- as.data.frame(table(unlist(str_split(features$EmotionCategoriesList, "\\|")), dnn = list("emot")), responseName = "freq")
  emotlist$Label <- gsub(";.*$", "", emotlist$emot)
  emotlist$Weight <- as.numeric(gsub("^.*;", "", emotlist$emot))
  emotlist$emot <- NULL
  emotlist$freq <- NULL
  emotlist
}

generate_emotions_weight_aggr <- function(features) {
  emotlist <- generate_emotions_weight(features)
  aggr <- aggregate(emotlist$Weight, by=list(Category=emotlist$Label), FUN=sum)
  colnames(aggr) <- c("Emotions", "Weight")
  aggr <- aggr[order(aggr$Weight, decreasing = TRUE), ]
  aggr
}

plot_emotions_weight <- function(features) {
  aggr <- generate_emotions_weight(features)
  ggplot(aggr, aes(x=reorder(Emotions, Weight), y=Weight, fill = Weight)) + 
    geom_bar(stat="identity") +
    scale_fill_gradient(low = "blue", high = "blue") + 
    xlab("Emotions") +
    ylab("Weight") +
    theme(legend.position="none") +
    coord_flip() 
}
#plot_emotions_weight(features)

plot_emotion_distribution_weight <- function(features, emot) {
  aggr <- generate_emotions_weight(features)
  aggr <- aggr[aggr$Label == emot, ]
  ggplot(aggr, aes(x=aggr$Weight)) + geom_histogram(bins=5)
}
#plot_emotion_distribution_weight(features, "self-pride")

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

temp <- function() {
  #time <- read.csv(file="~/Downloads/features-nent_time-notext.tsv", header=TRUE, sep="\t")
  #extra <- read.csv(file="~/Downloads/trec-extra-date.tsv", header=TRUE, sep="\t")
  #total <- merge(time,extra,by="id")
  #write.table(total,file="~/Downloads/feat-date-nent_time.tsv",sep="\t",quote = F,row.names = F)
  total <- read.csv(file="~/Downloads/feat-date-nent_time.tsv", header=TRUE, sep="\t")
  total$date <- as.Date(as.POSIXct(total$published_date/1000, origin="1970-01-01"))
  total$year <- year(as.POSIXct(total$published_date/1000, origin="1970-01-01"))
  total$month <- month(as.POSIXct(total$published_date/1000, origin="1970-01-01"))
  total$day <- day(as.POSIXct(total$published_date/1000, origin="1970-01-01"))
  total$month_abb <- month.abb[month(as.POSIXct(1325376562000/1000, origin="1970-01-01"))]
  total$weekday <- weekdays(as.Date(as.POSIXct(total$published_date/1000, origin="1970-01-01")))
  total$season <- getSeason(as.Date(as.POSIXct(total$published_date/1000, origin="1970-01-01")))
  total$week <- lubridate::isoweek(ymd(as.Date(as.POSIXct(total$published_date/1000, origin="1970-01-01"))))
  write.table(total,file="~/Downloads/trec-dates-all.tsv",sep="\t",quote = F,row.names = F)
  total
}
#total <- temp()
#head(total)

LAMBDA <- 0.85

# Show concrete columns (by name) of dataframe
#features_df[, c("id","SentimentAnalysis")] 

# Delete column of dataframe
#r1[, -1]

# Vector similarity
#simil(matrix(c(1,2,3,  1,2,3), nrow=2, byrow=T), method="cosine")

features_df <- read.csv(file="~/Downloads/features5-matrix.tsv", header=TRUE, sep="\t")
#+temp try less features
features_df <- features_df[, c("id","SentimentAnalysis", "ReadingComplexity")]
r1 <- read.csv(file="~/Downloads/feup-run1.res", header=FALSE, sep=" ")
maxS <- function(di, dJ, features_df) {
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <- c("id", "sim")
  for (dj in dJ) {
    sim <- simil(matrix(c(features_df[which(features_df$id == di), -1],  features_df[which(features_df$id == dj), -1]), nrow=2, byrow=T), method="cosine")
    df[nrow(df)+1,] <- c(dj, sim)
  }
  as.numeric(max(df$sim))
}
rerank <- function(features_df, r1) {
  r1$norm_score <- r1$V5/max(r1$V5)
  r1split <- split(r1, r1$V1)
  
  s_rerank <- NULL
  for (topic in as.character(unique(r1$V1))) {
    s <- NULL
    rs <- r1split[[topic]]
  
    s <- rbind(s, head(rs,1))
    rs <- rs[-1,]
  
    while(nrow(rs) > 0) {
      df <- data.frame(matrix(ncol = 2, nrow = 0))
      colnames(df) <- c("id", "mmr")
      for (rs_doc in rs$V3) {
        mmr =  LAMBDA * (rs[which(rs$V3 == rs_doc),]$norm_score - (1-LAMBDA)*maxS(rs_doc, s$V3, features_df))
        df[nrow(df)+1,] <- c(rs_doc, mmr)
      }
      s <- rbind(s, rs[which(rs$V3 == df[which.max(df$mmr),]$id),])
      rs <- rs[-which(rs$V3 ==  df[which.max(df$mmr),]$id),]
    }
    s_rerank <- rbind(s_rerank, s)
  }
  s_rerank
}
r3_rerank <- rerank(features_df, r1)
write.table(r3_rerank,file="~/Downloads/r3_rerank.tsv",sep=" ",quote = F,row.names = F)

#write.csv(total, "~/Downloads/feat-date-nent_time.tsv", row.names = F, sep="\t")

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
