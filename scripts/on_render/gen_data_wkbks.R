if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}

## Datasets

data_path <- here::here("data")

data_info <- tibble::tribble(
  ~data_file, ~source, ~cite, ~notes,
  "syn_data.csv", "Mealor et al., 2016", "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0155483", "Dataset publicly available",
  "anx_data.csv", "Terry, Lea, & Field (in prep)", "https://osf.io/8jk5v/", "Dataset shared for teaching purposes"
) |> 
  dplyr::mutate(
    cite_link = paste0("<a href='", cite, "'>", source, "</a>")
  ) |> 
  dplyr::select(data_file, cite_link, notes)

data_tab <- tibble::tibble(
  data_file =list.files(data_path, pattern = "data.csv"),
  link = paste0("<a href='https://raw.githubusercontent.com/drmankin/practicum/master/data/", 
                data_file, "'>Link</a>")
  )|> 
  dplyr::arrange(data_file)

data_tab <- data_info |>
  dplyr::left_join(data_tab) |> 
  kableExtra::kbl(
    col.names = c("Filename", "Citation/Source", "Comments", "URL"),
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
    "Either copy the link to use in `readr::read_csv()`, or save the data at the link to a .csv file to read in.",
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
