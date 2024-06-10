
# TO DO -------------------------------------------------------------------

## Change or remove silly headings: "Setting Up", "Well Done", "Next Steps" etc.

# Setup -------------------------------------------------------------------


if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}

library(magrittr)

#https://community.rstudio.com/t/is-there-a-way-to-extract-the-names-of-all-functions-in-an-r-script/51905

get_calls <- function(filepath) {
  code <- parse(filepath)
  tokens <- as.list(code)
  calls <- c()
  while (TRUE) {
    any_unpacked <- FALSE
    for (ii in seq_along(tokens)) {
      part <- tokens[[ii]]
      # Calls always have the function name as the first element
      if (is.call(part)) {
        fun_token <- part[[1]]
        calls <- c(calls, deparse(fun_token))
      }
      # Expressions have a length
      if (length(part) > 1) {
        tokens[[ii]] <- as.list(part)
        any_unpacked <- TRUE
      }
    }
    tokens <- unlist(tokens)
    if (!any_unpacked) break
  }
  unique(calls)
}

## Get file paths to all qmds in tutorials folder and onward

all_qmds_path <- "tutorials/docs"
# all_qmds_path <- "tutorials/archive_autumn_2023" #for generating the archive quickref
all_qmds <- list.files(path = all_qmds_path, pattern = "qmd", recursive = TRUE)

## don't include any unrendered docs

all_qmds <- all_qmds[!grepl("^_.*", all_qmds)]

# Create Functions Index --------------------------------------------------

## Extract all code from tutorial docs and save in new scripts in tutorials/index folder

### Put function index files in fx folder!

path_fx_index <- file.path(getwd(), "tutorials/index/fx")

if(!dir.exists(path_fx_index)){
  dir.create(path_fx_index)
}

purrr::map(
  .x = file.path(all_qmds_path, all_qmds),
  .f = ~knitr::purl(.x, output = file.path(path_fx_index, 
                                           gsub(".*/(.*)", "\\1", gsub("qmd", "R", .x))))
)

## Clean up errorful/non-parseable code and replace file

clean_code <- function(this_file){
  
  this_code <- readLines(this_file)
  
  ## Drop blank lines and comments
  #this_code <- this_text[this_text != "" & !grepl("^#", this_text)]
  
  keep_these <- purrr::map(
    .x = this_code,
    .f = ~class(try(parse(text = .x), silent = TRUE))
  ) |> 
    unlist() |> 
    grep(pattern = "expression", x = _)
  
  writeLines(this_code[keep_these], con = this_file)
}

purrr::map(
  .x = list.files(path = path_fx_index, pattern = ".R$", recursive = TRUE, full.names = TRUE),
  .f = clean_code
)

## Get out all function calls

get_title <- function(file_path){
  text <- readLines(file_path)
  gsub('title: \\\"(.*)\\\"', "\\1", grep("title:", text, value = TRUE)[1])
}

all_fxs <- list.files(path = path_fx_index, pattern = ".R$", 
           recursive = TRUE, full.names = TRUE) %>% 
  purrr::set_names(nm = unlist(purrr::map(file.path(all_qmds_path, all_qmds), get_title))) |> 
  purrr::map(
  .x = _,
  .f = get_calls
) |> 
  purrr::map(
    .x = _,
    .f = sort
  )

## DELETES all the .R files (otherwise if the file names change the whole thing breaks)
unlink(list.files(path = path_fx_index, pattern = ".R$",
                  recursive = TRUE, full.names = TRUE))

all_fxs_tib <- all_fxs |> tibble::enframe() |> tidyr::unnest(cols = value)

fxs_tib <- all_fxs_tib |>
  dplyr::filter(grepl("^[[:alpha:]]", value) & !(grepl("^:", value))) |> 
  dplyr::mutate(
    value = paste0(value, "()")
  ) |>
  dplyr::rowwise() |> 
  dplyr::mutate(
    link = downlit::autolink(value)
  )

## Returns package name - from https://stackoverflow.com/questions/44696431/how-to-get-the-package-name-of-a-function-in-r
## Edited to return the FIRST result (because it doesn't really matter I guess!)

findAllFun <- function(f) {     
  h <- help.search(paste0("^",f,"$"),agrep=FALSE)     
  h$matches[,"Package"] }


fxs_tib <- fxs_tib |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    value = dplyr::case_when(
      is.na(link) ~ paste0(findAllFun(value)[1], "::", value),
      .default = value
    ), 
    link = downlit::autolink(value)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(!is.na(link))

fxs_tib_wide <- fxs_tib |>
  dplyr::mutate(
    value = gsub(".*::(.*)", "\\1", value)
  ) |> 
  dplyr::group_by(value) |> 
  dplyr::reframe(
    tut = paste(unlist(name), collapse = "<br>"),
    link = link
  ) |> 
  dplyr::filter(!duplicated(value))

## Rearrange and convert to kable
  
fxs_tab <- fxs_tib_wide |> 
  dplyr::arrange(value) |> 
  dplyr::select(value, link, tut) |> 
  kableExtra::kbl(
    col.names = c("Function Name", "Link to Help Documentation", "Used In..."),
    format = "html",
    escape = FALSE
    ) |> 
  kableExtra::kable_styling()


# Create Topics Index -----------------------------------------------------

path_topics_index <- file.path(getwd(), "tutorials/index/topics")

if(!dir.exists(path_topics_index)){
  dir.create(path_topics_index)
}

purrr::map(
  .x = file.path(all_qmds_path, all_qmds),
  .f = ~knitr::purl(.x, documentation = 2,
                    output = file.path(path_topics_index, 
                                           gsub(".*/(.*)", "\\1", gsub(".qmd", "_topics.R", .x))))
)

clean_topics <- function(this_file){
  
  this_code <- readLines(this_file)
  
  this_text <- this_code[grepl("#' #{2,3} ", this_code)]
  
  tibble::tibble(
    text = this_text,
    heading_level = nchar(gsub("#' (#{2,}) .*", "\\1", text)),
    heading_text = gsub("#' #{2,} (.*)", "\\1", text)
  ) |> 
    dplyr::select(-text)
  
  # gsub("#' #{1,} (.*)", "\\1", this_code[grepl("#' #{2,3} ", this_code)])
}

titles_key <- file.path(all_qmds_path, all_qmds) |> 
  purrr::set_names(nm = unlist(purrr::map(file.path(all_qmds_path, all_qmds), get_title)))

topics <- list.files(path = path_topics_index, pattern = "_topics.R$", recursive = TRUE, full.names = TRUE) |> 
  purrr::set_names(nm = all_qmds) |> 
  purrr::map(
    .x = _,
    .f = clean_topics
  ) |> 
  tibble::enframe() |> 
  tidyr::unnest(cols = value)

## DELETES all the .R files (otherwise if the file names change the whole thing breaks)
unlink(list.files(path = path_topics_index, pattern = ".R$",
                  recursive = TRUE, full.names = TRUE))

exclude_headings <- c("Overview", "Basic Structure", "Setting Up", "Next Steps", "Well Done!")

topics_tab <- topics |> 
  dplyr::filter(!(heading_text %in% exclude_headings)) |> 
  dplyr::mutate(
    link = paste0("[", heading_text, "](tutorials/docs/", name, "#", gsub(" ", "-", tolower(heading_text)),")")
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    tut = unique(names(titles_key[which(grepl(name, titles_key))]))
    )

## Restructure

topics_tab <- topics_tab |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    heading_group = dplyr::if_else(heading_level == 2,
                                   1, 0),
    heading_group = cumsum(heading_group)
  ) |> 
  tidyr::pivot_wider(
    names_from = heading_level, values_from = link, id_cols = c(tut, heading_group),
    values_fn = ~ paste0(.x, collapse = ", ")
  ) |> 
  ## Remove and keep in two columns; replace NAs with ""
  # dplyr::mutate(
  #   link = paste0(`2`, ": ", `3`)
  # ) |> 
  dplyr::select(tut, `2`, `3`) |> 
  dplyr::rename_with(~c("tut", "topic", "sub_topic")) |> 
  dplyr::mutate(
    sub_topic = tidyr::replace_na(sub_topic, "")
  )

# topics_indents <- which(topics_tab$heading_level > 2)

topics_pack <- topics_tab |> 
  dplyr::count(tut) |> 
  tibble::deframe()

topics_kbl <- topics_tab |> 
  dplyr::select(topic, sub_topic) %>%
  kableExtra::kbl(
    format = "html",
    col.names = stringr::str_to_title(gsub("_", "-", colnames(.))),
    booktabs = TRUE
  )

topics_kbl <- topics_kbl |> 
  # kableExtra::add_indent(positions = topics_indents) |> 
  kableExtra::pack_rows(index = topics_pack)  |> 
  kableExtra::kable_styling(bootstrap_options = "condensed") |> 
  kableExtra::row_spec(row = 0, align = "c") |> 
  kableExtra::kable_styling()

# Create Index Page -------------------------------------------------------

writeLines(
  c("---",
  "title: 'Quick Reference'",
  "---",
  "",
  "## Index of Functions",
  "",
  "Looking for a function you can't quite remember how to use? You're in the right place!",
  "The table below is arranged alphabetically by function name, and the linked full name (including relevant package calls) will take you to the help documentation.",
  "",
  fxs_tab,
  "",
  "## Index of Topics",
  "",
  "If you're looking for a particular section of a tutorial, use this handy summary to jump straight to the section you want.",
  "",
  topics_kbl),
  file.path("quick_ref.qmd")
)


### Somwhat misguided/overwrought attempt to produce code that could be used to install all pkgs
### Come back to this, maybe.

# pkgs <- fxs_tib |> 
#   tidyr::separate_wider_delim(cols = value, names = c("pkg", "fx"), 
#                               delim = "::", too_few = "align_end") |> 
#   dplyr::pull(pkg) |> 
#   na.omit() |> 
#   unique()
# 
# used_pkgs <- tibble::tibble(
#   pkgs = pkgs,
#   cran = pkgs %in% available.packages(),
#   tvs = pkgs %in% tidyverse::tidyverse_packages()
# ) |> 
#   arrange(pkgs)
# 
# quote_commas <- function(x){
#   paste0("'", x, "'", sep = ",", collapse = "") |> 
#     gsub("(.*),$", "\\1", x = _)
# }
# 
# install_chunk <- function(x, how = "cran"){
#   paste("```{r}", "\n",
#         ifelse(how == "cran", "install.packages", "githubinstall::githubinstall"),
#         "(c(", quote_commas(x), "))", "\n",
#         "```", sep = "")
# }
# 
# install_chunk("rcanvas", how = "github")
# 
# install_all_pkgs <- install_chunk(quote_commas())
# 
# used_pkgs
# 
# pkgs <- c(used_pkgs$pkgs, "not_this_one")
# 
# tryCatch(install.packages(pkgs[i]), 
#          error = tryCatch(githubinstall::githubinstall(pkgs[i]),
#                           error = 
#                           finally = stop("Can't find that package on CRAN or Github, sorry!"))
#            )
# 
# for(i in seq_along(pkgs)){
#     try_cran <- try()
#     if(inherits(try_cran, "try-error")){
#       try_github <- try)
#       if(inherits(try_github, "try-error")){
#         return("Can't find that package, sorry!")
#       }
#     }
#   }

## Get out function name only, and restructure with duplicates on the same row