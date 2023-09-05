# 
# file_path <- file.path(getwd(), "tutorials", "01_fundRmentals", "03_lm.qmd")
# 
# text <- readLines(file_path)
# 
# ind_divs <- grep("^:::", text)
# 
# ind_ex <- grep("callout-note.*?Exercise", text)
# 
# start_ind <- ind_ex + 1
# end_ind <- ind_divs[which(ind_divs %in% ind_ex) + 1] -1
# 
# # text[start_ind:end_ind]
# 
# # purrr::pmap(
# #   .l = list(
# #     start_ind,
# #     end_ind,
# #     text),
# #   .f = ~ text[start_ind:end_ind]
# #   )
# 
# # http://adv-r.had.co.nz/Subsetting.html#subassignment

library(magrittr)

## Get file paths to all qmds in tutorials folder and onward

all_qmds_path <- "tutorials/psychrlogy"
all_qmds <- list.files(path = all_qmds_path, pattern = "qmd", recursive = TRUE)

## Create new folder
path_wkbk <- file.path(getwd(), "tutorials/workbooks")

if(!dir.exists(path_wkbk)){
  dir.create(path_wkbk)
}

this_file <- file.path(all_qmds_path, all_qmds[1])

qmd_lines <-  tibble::tibble(
  lines = readLines(this_file)
)


## Identify the YAML
qmd_lines <- qmd_lines |>
  dplyr::mutate(
    is_yaml = stringr::str_detect(lines, "---"),
    ## for is_yaml, increment by 1 each time. So the first is_yaml = TRUE is 1, then 2, and so on, otherwise 0
    is_yaml = dplyr::if_else(is_yaml, cumsum(is_yaml), as.integer(0)),
    ## checks whether the row number is less than or equal to the row number of the case
    ## where is_yaml is 2 (ie the closing ---).
    ## End result: all rows of the YAML header are TRUE, otherwise FALSE
    is_yaml = dplyr::row_number() <= which(is_yaml == 2)
  )

## Identify headers

qmd_lines <- qmd_lines |> 
  dplyr::mutate(
    is_heading = grepl("#{2,}", lines)
  )

## Identify solutions callouts

qmd_lines <- qmd_lines |> 
  dplyr::mutate(
    sol_start = grepl('title="Solution"', lines),
    sol_start_index = dplyr::if_else(sol_start, cumsum(sol_start), as.integer(NA)),
    co_end = grepl("^:::$", lines),
    co_end_index = dplyr::if_else(co_end, cumsum(co_end), as.integer(NA))
  ) |> 
  tidyr::fill(sol_start_index) |> 
  tidyr::fill(co_end_index, .direction = "up") |> 
  tidyr::fill(co_end_index) |> 
  dplyr::group_by(sol_start_index) |> 
  dplyr::mutate(
    sol_index = dplyr::case_when(co_end_index == min(co_end_index) ~ sol_start_index, .default = as.integer(0)),
    is_sol = dplyr::if_else(sol_index > 0, TRUE, FALSE)
  ) |> 
  dplyr::ungroup()

## Identify exercise text

qmd_lines <- qmd_lines |> 
  dplyr::mutate(
    ex_start = grepl('title="Exercise', lines),
    ex_start_index = dplyr::if_else(ex_start, cumsum(ex_start), as.integer(NA))
  ) |> 
  tidyr::fill(ex_start_index) |> 
  dplyr::group_by(ex_start_index) |> 
  dplyr::mutate(
    ex_index = dplyr::case_when(co_end_index == min(co_end_index) ~ ex_start_index, .default = as.integer(0)),
    is_ex = dplyr::if_else(ex_index > 0 & !sol_index, TRUE, FALSE),
    is_ex_text = dplyr::if_else(is_ex & !(startsWith(lines, ":::")), TRUE, FALSE)
  ) |> 
  dplyr::ungroup()

## Identify code chunks

qmd_lines <- qmd_lines |>
  dplyr::mutate(
    end_fence = lines == "```",
    start_fence = grepl("^```.*r.*", lines),
    is_fence = end_fence == 1 | start_fence == 1,
    fence_index_start = dplyr::if_else(start_fence, cumsum(start_fence), as.integer(NA)),
    fence_index_end = dplyr::if_else(end_fence, cumsum(end_fence), as.integer(NA))
  ) |>
  tidyr::fill(fence_index_start) |>
  tidyr::fill(fence_index_end, .direction = "up") |>
  dplyr::mutate(
    code_chunk_index = dplyr::case_when(fence_index_start == fence_index_end ~ fence_index_start, TRUE ~ as.integer(0)),
    is_code = dplyr::if_else(code_chunk_index == 0, FALSE, TRUE)
  )

View(qmd_lines)

qmd_lines |> 
  dplyr::filter(!is_code) |> 
  dplyr::filter(is_heading | is_yaml | is_ex_text) |> 
  dplyr::pull(lines) |> 
  writeLines(file.path(path_wkbk, "workbook_text.qmd"))

## Add new lines before/after headings
## Add code chunks after text
