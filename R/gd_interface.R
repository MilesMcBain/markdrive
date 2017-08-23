#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

gdoc_checkout <- function(filename, path = NULL){
  message("Searching your Google documents...\n")
  file_matches <- googledrive::drive_find(pattern = filename,
                                          type = "document")
  if(nrow(file_matches) > 8){
    message("[gdoc_checkout] Found too many matches for filename, use a more specific patern")
  } else if(nrow(file_matches) > 1){
    match_index <- menu(title = "Choose a google doc to fetch:",
                        choices = c(file$name, "None of the above."))
  }
  else if(nrow(file_matches) == 1){
    match_index = 1
  }
  else{
    stop("No files matching: ", filename, " were found.")
  }
  ##Create output dir
  if(!dir.exists("./.rmarkdrive")) dir.create("./.rmarkdrive")
  remote_doc <-
    file_matches[match_index,] %>%
    googledrive::drive_download(path = file.path(".",".rmarkdrive",
                                               file_matches$name[[1]]))
  message("Converting file to markdown with pandoc...")
  ##Pandoc to md
  md_name =
  system(command = paste0("pandoc -f docx -t markdown -o \"", remote_doc$name,".md\"",
         " \"", remote_doc$local_path, "\""))
  remote_doc$local_rmd = paste0(remote_doc$name,".md")
  message("  * Created ./",remote_doc$local_rmd)
  saveRDS(object = remote_doc,
          file = file.path(".",".rmarkdrive",paste0(file_matches$name[[1]],".Rds")))
  remote_doc
}

gdoc_push <- function(file){}

gdoc_md2gd <- function(filename, remote_name = NULL){}
