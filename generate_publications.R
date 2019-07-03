bibtex_2academic <- function(bibfile,
                             outfold,
                             abstract = FALSE,
                             overwrite = FALSE) {
    require(RefManageR)
    require(dplyr)
    require(stringr)
    require(anytime)
    
    # Import the bibtex file and convert to data.frame
    mypubs   <-
        ReadBib(bibfile, check = "warn", .Encoding = "UTF-8")
    # create a function which populates the md template based on the info
    # about a publication
    for (i in 1:length(mypubs)) {
        x <- mypubs[[i]]
        x$title <- gsub("\\}", "", gsub("\\{", "", x$title))
        if (!is.null(x$year)) {
            if (is.null(x$month)) {
                x$date <- paste0(x$year, "-01")
                
            } else{
                x$month <- match(x$month, tolower(month.abb))
                x$date <- paste0(x$year, "-", x$month)
            }
        } else {
            x$date <- "1989-01"
        }
        entryname <- paste(
            x$date,
            x$title %>%
                str_replace_all(fixed(" "), "_") %>%
                str_remove_all(fixed(":")) %>%
                str_remove_all(fixed(",")) %>%
                str_sub(1, 20),
            sep = "_"
        )
        filename <- paste0("index.md")
        if (!file.exists(file.path(outfold, entryname)) |
            overwrite) {
            dir.create(file.path(outfold, entryname), showWarnings = FALSE)
            WriteBib(x, file = file.path(file.path(outfold, entryname), "cite.bib"))
            fileConn <-
                file.path(file.path(outfold, entryname), filename)
            write("---", fileConn)
            # Title and date
            write(paste0("title: \"", x$title, "\""),
                  fileConn,
                  append = T)
            write(paste0("date: \"", anydate(x$date), "\""),
                  fileConn,
                  append = T)
            # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
            auth_hugo <-
                sapply(x$author, function(x) {
                    stringi::stri_trans_general(as.character(x), "latin-ascii")
                })
            write("authors:", fileConn, append = T)
            write(paste0("- ", auth_hugo), fileConn, append = T)
            # Publication type. Legend:
            # 0 = Uncategorized, 1 = Conference paper, 2 = Journal article
            # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book section
            if (x$bibtype == "Article" |
                x$bibtype == "Article in Press") {
                x$pubtype <- 2
            } else if (x$bibtype == "InProceedings" |
                       x$bibtype == "Proceedings" |
                       x$bibtype == "Conference" |
                       x$bibtype == "Conference Paper") {
                x$pubtype <- 1
            } else if (x$bibtype == "MastersThesis" |
                       x$bibtype == "PhdThesis") {
                x$pubtype <- 3
            } else if (x$bibtype == "Manual" |
                       x$bibtype == "TechReport") {
                x$pubtype <- 4
            } else if (x$bibtype == "Book") {
                x$pubtype <- 5
            } else if (x$bibtype == "InCollection" |
                       x$bibtype == "InBook") {
                x$pubtype <- 6
            } else if (x$bibtype == "Misc") {
                x$pubtype <- 0
            } else{
                x$pubtype <- 0
            }
            write("publication_types:", fileConn, append = T)
            write(paste0("- ", x$pubtype), fileConn, append = T)
            if (!is.null(x$journal)) {
                publication <- x$journal
                
                if (!is.null(x$volume))
                    publication <-
                        paste0(publication, ", (", x$volume, ")")
                if (!is.null(x$number))
                    publication <-
                        paste0(publication, ", ", x$number)
                if (!is.null(x$pages))
                    publication <-
                        paste0(publication, ", _pp. ", x$pages, "_")
                if (!is.null(x$doi))
                    publication <-
                        paste0(publication,
                               ", ",
                               paste0("https://doi.org/",  x$doi))
                write(paste0("publication: \"", publication, "\""),
                      fileConn,
                      append = T)
                write(
                    paste0("publication_short: \"", publication, "\""),
                    fileConn,
                    append = T
                )
            }
            if (!is.null(x$doi)) {
                write(paste0("doi: \"", x$doi, "\""),
                      fileConn,
                      append = T)
            }
            if (abstract & !is.null(x$abstract)) {
                x$abstract <- gsub("\\\\%","%", x$abstract)
                write(paste0("abstract: \"", x$abstract, "\""),
                      fileConn,
                      append = T)
            } else {
                write("abstract: \"\"", fileConn, append = T)
            }
            write(paste0("abstract_short: \"", "\""),
                  fileConn,
                  append = T)
            write(paste0("featured: false"),
                  fileConn,
                  append = T)
            # other possible fields are kept empty. They can be customized later by
            # editing the created md
            
            write("image_preview: \"\"", fileConn, append = T)
            write("projects: []", fileConn, append = T)
            write("tags: []", fileConn, append = T)
            #links
            write("url_pdf: \"\"", fileConn, append = T)
            write("url_preprint: \"\"", fileConn, append = T)
            write("url_code: \"\"", fileConn, append = T)
            write("url_dataset: \"\"", fileConn, append = T)
            write("url_project: \"\"", fileConn, append = T)
            write("url_slides: \"\"", fileConn, append = T)
            write("url_video: \"\"", fileConn, append = T)
            write("url_poster: \"\"", fileConn, append = T)
            if (!is.null(x$url)) {
                write(paste0("url_source: \"", x$url, "\""),
                      fileConn,
                      append = T)
            } else{
                write("url_source: \"\"", fileConn, append = T)
            }
            #other stuff
            write("math: true", fileConn, append = T)
            write("highlight: true", fileConn, append = T)
            write("---", fileConn, append = T)
        }
    }
}

