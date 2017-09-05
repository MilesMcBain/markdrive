#' Checkout a google document in Markdown format
#'
#' Given a filename this funciton will search for a matching file on your Google drive.
#' If more than one match is found you will get a menu to choose from. If too many maches are
#' found the function will stop.
#'
#' Once the target file has been identified it will be downloaded in docx format into a hidden
#' folder `.markdrive/` created under the current working directory. Pandoc is used to convert the
#' docx to .md and the result is placed in the current working directory. File metadata mapping
#' the .md to the remote source is saved in the markdrive folder.
#'
#'
#' @param filename a complete or partial filename to be searched for on your Google drive.
#'
#' @return a googledrive dribble augmented with a new column `local_md` and new class "markdown_doc"
#' @export
#'
#' @examples \dontrun{
#'  gdoc_checkout("GOT")
#'  #Check out a file matching "GOT", e.g. "my GOT theory"
#' }
gdoc_checkout <- function(filename){
  message("Searching your Google documents...\n")
  file_matches <- googledrive::drive_find(pattern = filename,
                                          type = "document")
  if(nrow(file_matches) > 8){
    stop("[gdoc_checkout] Found too many matches for filename, use a more specific patern")
  } else if(nrow(file_matches) > 1){
    match_index <- menu(title = "Choose a google doc to fetch:",
                        choices = c(file$name, "None of the above."))
    #If you chose None of the above:
    if(match_index == (length(file$name) + 1) ){
      return(NULL)
    }
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
  system(command = paste0("pandoc -f docx --track-changes=all -t markdown -o \"", remote_doc$name,".md\"",
         " \"", remote_doc$local_path, "\""))
  remote_doc$local_md = paste0(remote_doc$name,".md")
  class(remote_doc) <- c(class(remote_doc), "markdown_doc")
  message("  * Created ./",remote_doc$local_md)
  saveRDS(object = remote_doc,
          file = file.path(".",".markdrive",paste0(file_matches$name[[1]],".Rds")))
  invisible(remote_doc)
}

#' Push a markdrive controlled markdown file back to Google drive as an update to
#' the source Google doc.
#'
#' Given a filename, this funciton will search for previously checked out files that
#' match. A selection menu is output if there is more than 1 match.
#'
#' Once the file to be pushed is identified, the md is converted to html with pandoc and
#' uploaded to Google drive as the new 'media' for the checked out file.
#'
#' @param filename a complete or partial filename to be searched for in the `.markdrive/` folder under
#' the current working directory
#' @param preserve_empty_lines If TRUE empty whitespace lines are preserved in the translation to Google doc.
#' This is most likely what you want if you use empty lines in your online document. If you want to write PURE markdown,
#' and handle this yourself with <br> etc, Set to FALSE.
#'
#' @return a fresh handle to the updated file.
#' @export
#'
#' @examples /dontrun{
#' gdoc_push("GOT")
#' #Pushes the local file "my GOT theory.md" as an update to the remote Google doc
#' "my GOT theory"
#' }
gdoc_push <- function(filename, preserve_empty_lines = TRUE){
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

      if(nrow(file_choices) > 8){
          stop("[gdoc_push] Found too many matches for filename, use a more specific patern")
      }

      chosen_file <- menu(choices = c(file_choices, "None of the above."),
                          title = "Choose a file to push to Googledocs:")

      #if you chose none of the above:
      if(chosen_file == length(file_choices) + 1){
        return(NULL)
      }


      controlled_files <- controlled_files[chosen_file,]
    }
    file_to_push <- controlled_files[[1]]
  }

  # pre process the file to push
  # can optionally disable whitespace preservation
  file_to_push_pp <- if(preserve_empty_lines){
    push_pre_process(file_to_push)
  } else{
    file_to_push$local_md
  }
  file_to_push_html <- file.path(".",".markdrive",paste0(file_to_push$name,".html"))

  #Pandoc the file_to_push
  system(command = paste0("pandoc -f markdown -t html -o \"",
                          file_to_push_html,
                          "\" \"", file_to_push_pp, "\""))

  googledrive::drive_update(file = file_to_push,
                            media = file_to_push_html)

}

#' Create a Google Doc from a local md or Rmd
#'
#' @param filename The path to the local file
#' @param gdoc_name A name to be given to the Google doc, if required.
#' @param gdoc_path A folder to place the Google doc in, if required.
#'
#' @return A handle to the created file.
#' @export
gdoc_render <- function(filename, gdoc_name = NULL, gdoc_path = NULL){
    if(is.null(gdoc_name)){
      gdoc_name <- rmarkdown::yaml_front_matter(filename)$title
      if(is.null(gdoc_name)){
          gdoc_name <-  tail(unlist(strsplit(x = filename, split = "/|\\\\")),1)
      }
    }
  #render to md
  #rmarkdown::render(filename, output_format = rmarkdown::md_document())

  remote_handle <- googledrive::drive_upload(
      media = rmarkdown::render(filename,                                                                   ),
                              type = "document",
                              path = gdoc_path,
                              name = gdoc_name)
    invisible(remote_handle)
}

render_to_gdoc <- function(){
    source_tab <- rstudioapi::getSourceEditorContext()
    if(source_tab$path == ''){
      stop("Please save the current source before rendering to GoogleDrive")
    }
    remote_file <- gdoc_render(source_tab$path)
    browseURL(remote_file$drive_resource[[1]]$webViewLink)
    invisible(NULL)
}
