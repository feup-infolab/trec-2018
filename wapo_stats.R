if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "ggplot2",
  "rjson"
)

get_document_length <- function(filepath) {
  df <- data.frame(doc_id=character(0), doc_len=integer(0), par_len=integer(0))
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)

    if (length(line) == 0) {
      break
    }

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
    
    if (length(doc_id) < 1 || length(doc_len) < 1 || length(par_len) < 1) next
    
    df <- rbind(df, data.frame(doc_id=json$id, doc_len=doc_len, par_len=par_len))
  }
  
  close(con)
  df
}

doc_length <- get_document_length("~/Downloads/WashingtonPost.v2/data/TREC_Washington_Post_collection.v2.jl")