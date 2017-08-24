#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

gdoc_checkout <- function(filename){
  message("Searching your Google documents...\n")
  file_matches <- googledrive::drive_find(pattern = filename,
                                          type = "document")
  if(nrow(file_matches) > 8){
    message("[gdoc_checkout] Found too many matches for filename, use a more specific patern")
  } else if(nrow(file_matches) > 1){
    match_index <- menu(title = "Choose a google doc to fetch:",
                        choices = c(file$name, "None of the above."))
    ## TODO: Handle choice "None of the above!
  }
  else if(nrow(file_matches) == 1){
    match_index = 1
  }
  else{
    stop("No files matching: ", filename, " were found.")
  }
  ##Create output dir
  if(!dir.exists("./.markdrive")) dir.create("./.markdrive")
  remote_doc <-
    file_matches[match_index,] %>%
    googledrive::drive_download(path = file.path(".",".markdrive",
                                               file_matches$name[[1]]),
                                overwrite = TRUE)
  message("Converting file to markdown with pandoc...")
  ##Pandoc to md
  system(command = paste0("pandoc -f docx -t markdown -o \"", remote_doc$name,".md\"",
         " \"", remote_doc$local_path, "\""))
  remote_doc$local_md = paste0(remote_doc$name,".md")
  class(remote_doc) <- c(class(remote_doc), "markdown_doc")
  message("  * Created ./",remote_doc$local_md)
  saveRDS(object = remote_doc,
          file = file.path(".",".markdrive",paste0(file_matches$name[[1]],".Rds")))
  invisible(remote_doc)
}

gdoc_push <- function(filename){
  if(googledrive::is_dribble(filename)){
    googledrive::confirm_single_file(filename)
    stopifnot(inherits(filename,"markdown_doc"))
    file_to_push <- filename
  }else{
    #it's an .rds file name (or part thereof)
    controlled_files_list <- list.files(path = file.path(".",".markdrive"),
                                  pattern = ".*\\.RDS",
                                  ignore.case = TRUE,
                                  full.names = TRUE
                                  )
    stopifnot(length(controlled_files_list) > 0)
    matching_controlled_files <-
      controlled_files_list[grepl(pattern = filename, x = controlled_files_list)]
    controlled_files <- lapply(matching_controlled_files, readRDS)

    if(length(matching_controlled_files) > 1){
      file_choices <- lapply(controlled_files, `[`, "local_md")
      chosen_file <- menu(choices = file_choices,
                          title = "Choose a file to push to Googledocs:")
      ## TODO: Give a none of the above option and handle

      controlled_files <- controlled_files[chosen_file,]
    }
    file_to_push <- controlled_files[[1]]
  }

  #Pandoc the file_to_push
  system(command = paste0("pandoc -f markdown -t html -o \"",
                          file.path(".",".markdrive",paste0(file_to_push$name,".html")),
                          "\" \"", file_to_push$local_md, "\""))

  googledrive::drive_update(file = file_to_push,
                            media = file.path(".",".markdrive",paste0(file_to_push$name,".html")))
}
