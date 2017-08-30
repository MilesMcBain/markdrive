push_pre_process <- function(md_doc){
   content <- readLines(md_doc$local_md) %>%
     gsub(pattern = "(?<=^)\\s*(?=$)", replacement = "\n<br>\n", x = ., perl = TRUE)
   processed_doc <- file.path(".",".markdrive",paste0(md_doc$name,"_processed",".md"))
   writeLines(content, processed_doc)
   processed_doc
}
