
# TO DO -------------------------------------------------------------------

## Change or remove silly headings: "Setting Up", "Well Done", "Next Steps" etc.

# Setup -------------------------------------------------------------------


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

all_qmds <- list.files(path = "tutorials/psychrlogy", pattern = "qmd", recursive = TRUE)


# Create Functions Index --------------------------------------------------

## Extract all code from tutorial docs and save in new scripts in tutorials/index folder

### Put function index files in fx folder!

purrr::map(
  .x = file.path("tutorials/psychrlogy", all_qmds),
  .f = ~knitr::purl(.x, output = file.path(getwd(), "tutorials/index/fx", 
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
  .x = list.files(path = "tutorials/index/fx", pattern = ".R$", recursive = TRUE, full.names = TRUE),
  .f = clean_code
)

## Get out all function calls

get_title <- function(file_path){
  text <- readLines(file_path)
  gsub('title: \\\"(.*)\\\"', "\\1", grep("title:", text, value = TRUE)[1])
}

all_fxs <- list.files(path = "tutorials/index/fx", pattern = ".R$", 
           recursive = TRUE, full.names = TRUE) %>% 
  purrr::set_names(nm = unlist(purrr::map(file.path("tutorials/psychrlogy", all_qmds), get_title))) |> 
  purrr::map(
  .x = _,
  .f = get_calls
) |> 
  purrr::map(
    .x = _,
    .f = sort
  )

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

purrr::map(
  .x = file.path("tutorials/psychrlogy", all_qmds),
  .f = ~knitr::purl(.x, documentation =2,
                    output = file.path(getwd(), "tutorials/index/topics", 
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

titles_key <- list.files(path = "tutorials/psychrlogy", pattern = ".qmd$", 
                         recursive = TRUE, full.names = TRUE) |> 
  purrr::set_names(nm = unlist(purrr::map(file.path("tutorials/psychrlogy", all_qmds), get_title)))

topics <- list.files(path = "tutorials/index/topics", pattern = "_topics.R$", recursive = TRUE, full.names = TRUE) |> 
  purrr::set_names(nm = all_qmds) |> 
  purrr::map(
    .x = _,
    .f = clean_topics
  ) |> 
  tibble::enframe() |> 
  tidyr::unnest(cols = value)

topics_tab <- topics |> 
  dplyr::filter(!(heading_text %in% c("Overview", "Basic Structure"))) |> 
  dplyr::mutate(
    link = paste0("[", heading_text, "](tutorials/psychrlogy/", name, "#", gsub(" ", "-", tolower(heading_text)),")")
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    tut = unique(names(titles_key[which(grepl(name, titles_key))]))
    )

topics_indents <- which(topics_tab$heading_level > 2)

topics_kbl <- topics_tab |> 
  dplyr::select(tut, link) |> 
  kableExtra::kbl(
    col.names = c("Tutorial", "Jump to Section"),
    format = "html"
  )

topics_kbl <- topics_kbl |> 
  kableExtra::add_indent(positions = topics_indents, target_cols = 2) |> 
  kableExtra::collapse_rows(columns = 1, valign = "middle") |> 
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