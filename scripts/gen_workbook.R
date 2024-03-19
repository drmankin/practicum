
## Set up output folder
out <- here::here("workbooks")

## Create new folder
if(!dir.exists(out)){
  dir.create(out)
}

## Get file paths to all qmds in tutorials folder and onward

all_qmds_path <- "workshops/dissertations"
all_qmds <- list.files(path = all_qmds_path, pattern = "qmd", recursive = TRUE)

which_file <- all_qmds[1]

this_file <- file.path(all_qmds_path, grep(which_file, all_qmds, value = TRUE))

gen_workbook <- function(this_file, out){
  
  
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
      sol_start = grepl('title="Solution.*"', lines),
      sol_start_index = dplyr::if_else(sol_start, cumsum(sol_start), as.integer(NA)),
      co_end = grepl("^:{3,4}$", lines),
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
    # tidyr::fill(ex_start_index, .direction = "up") |>  #trying to fix the weird numbering
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
    dplyr::group_by(fence_index_start) |> 
    dplyr::mutate(
      code_chunk_index = dplyr::case_when(fence_index_end == min(fence_index_end, na.rm = TRUE) ~ fence_index_start, .default = as.integer(0)),
      is_code = dplyr::if_else(code_chunk_index == 0, FALSE, TRUE)
    )
  
  this_page <- sub(".qmd", "", this_file)
  
  viewer_text <- c("## Open the Tutorial\n\nUse the following code chunks to open the accompanying tutorial.",
                   paste0("### Open in RStudio Viewer\n\n```{r}\nrstudioapi::viewer('https://r-training.netlify.app/", this_page, "')\n```", collapse = ""),
                   paste0("### Open in a New Browser Tab\n\n```{r}\nutils::browseURL('https://r-training.netlify.app/", this_page, "')\n```", collapse = "")
  ) |> 
    paste0(collapse="\n\n")
  
  qmd_lines <- qmd_lines |> 
    dplyr::filter(!is_code | is.na(is_code)) |> 
    dplyr::filter(is_heading | is_yaml | is_ex_text) |> 
    dplyr::mutate(
      lines = dplyr::case_when(
        is_heading ~ paste0("\n", lines, "\n"),
        is_yaml & cumsum(is_yaml) == sum(is_yaml) ~ paste0("embed-resources: true\neditor: visual\n---\n\n", viewer_text, "\n\n"),
        .default = lines)
    ) |>
    dplyr::group_by(ex_index) |> 
    dplyr::mutate(
      lines = dplyr::case_when(
        ex_index != 0 & dplyr::row_number() == length(ex_index) ~ paste0(lines, "\n```{r}\n\n```\n\n"),
        .default = lines
      )
    ) |> 
    dplyr::ungroup()
  
  qmd_lines |>  dplyr::pull(lines) |>
    writeLines(file.path(out, paste0(gsub(".*/(.*)", "\\1", this_page), "_workbook.qmd")))
}

## Generate a single workbook
gen_workbook(this_file, out)
