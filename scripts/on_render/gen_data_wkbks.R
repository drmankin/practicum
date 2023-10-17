if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}

## Datasets

data_path <- here::here("data")

data_tab <- tibble::tibble(
  data_file =list.files(data_path, pattern = "data.csv"),
  link = paste0("<a href='https://raw.githubusercontent.com/drmankin/practicum/master/data/", 
                data_file, "' download='", data_file, "'> Download ", data_file, " </a>")
  )|> 
  dplyr::arrange(data_file) |> 
  kableExtra::kbl(
    col.names = c("Filename", "Download"),
    format = "html",
    escape = FALSE
  ) |> 
  kableExtra::kable_styling()
  
## Workbooks

wkbk_path <- here::here("workbooks")

list.files(path = here::here(), pattern = "workbook.qmd", 
           recursive = TRUE, full.names = TRUE)

get_title <- function(file_path){
  text <- readLines(file_path)
  gsub('title: \\\"(.*)\\\"', "\\1", grep("title:", text, value = TRUE)[1])
}

wkbk_tab <- tibble::tibble(
  data_file = list.files(wkbk_path, pattern = "workbook.qmd"),
  link = paste0("<a href='https://raw.githubusercontent.com/drmankin/practicum/master/workbooks/'", 
                data_file, "' download='", data_file, "'>Download ", data_file, "</a>"),
  full_path = list.files(path = here::here(), pattern = "workbook.qmd", 
                                 recursive = TRUE, full.names = TRUE)
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    title = get_title(full_path)
  ) |> 
  dplyr::select(title, link) |> 
  kableExtra::kbl(
    col.names = c("Tutorial", "Download"),
    format = "html",
    escape = FALSE
  ) |> 
  kableExtra::kable_styling()


# Create Page -------------------------------------------------------------

writeLines(
  c("---",
    "title: 'Data and Workbooks'",
    "---",
    "\n",
    "## Datasets",
    "\n",
    "Download and save datasets to use for tutorial tasks here.",
    "\n",
    data_tab,
    "\n",
    "## Workbooks",
    "",
    "Download and save workbook Qmd files to complete the tutorial tasks in.",
    "",
    wkbk_tab),
  file.path("data_workbooks.qmd")
)
