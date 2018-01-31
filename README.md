# markdrive
Edit Google docs in Markdown with a little help from #rstats

# Usage

`gdoc_checkout(filename = "GOT")` Will search your Google drive for Google docs with "GOT" in the name and prompt to download one. After download it will be converted to .md for editing. Let's say the file that was downloaded was `my_GOT_theory.docx`, `my_GOT_theory.md` will be created in the working dir. 

`gdoc_push(filename = "GOT")` Will push a markdown file matching the name pattern back to Google drive and update the source document. You could also supply a `dribble` output from `gdoc_checkout`. It updates the google doc via a html conversion along the way.

`gdoc_render(filename = "./test.Rmd")` will render an .md or .Rmd file to html and push it to Google Drive as a google doc. This package includes an Rstudio addin that will do this for the currently active source tab.



Checkout to get the markdown file. Push to "save" your edits to Google drive.
