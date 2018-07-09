if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2",
  "rjson",
  "logging",
  "plyr"
)

basicConfig()

BATCH_SIZE <- 10000

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
    break
  }

  close(con)
  res
}

loginfo("Getting document length and first-three-paragraphs length")
doc_length <- get_document_length("~/Downloads/WashingtonPost.v2/data/TREC_Washington_Post_collection.v2.jl")

dir.create("output/", showWarnings = F)
doc_length_path <- "output/wapo-doc_length.csv.gz"
loginfo("Saving result to %s", doc_length_path)
write.csv(doc_length, gzfile(doc_length_path), row.names = F)