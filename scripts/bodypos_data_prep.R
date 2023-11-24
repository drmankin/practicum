library(tidyverse)

bp_data <- here::here("data/bodypos_data.sav") |> 
  haven::read_sav()

bp_data <- bp_data |> 
  dplyr::rename_with(~gsub(pattern = ".*?_(.*)", replacement = "\\1", x = tolower(.x))) |> 
  dplyr::rename(choice_perc = healthy100) |> 
  dplyr::select(1:61, condition, choice_perc)

bp_data <- bp_data |> 
  dplyr::mutate(
    across(c(gender, age, instagram, condition),
           labelled::to_factor)
  ) |> 
  dplyr::relocate(c(condition, choice_perc), .after = instagram)

saveRDS(bp_data, here::here("data/bp_data.rds"))


# Codebook ----------------------------------------------------------------

## Demographic variables
demo_vars <- bp_data |> 
  dplyr::select(recordeddate:choice_perc) |> 
  names()

demo_bp_data <- bp_data |> 
  dplyr::select(all_of(demo_vars)) 

demo_bp_cb <- demo_bp_data |> 
  sjlabelled::get_labels() |> 
  tibble::enframe() |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    values = unlist(value) |> paste0(collapse = ", ")
  ) |> 
  dplyr::select(-value) |> 
  dplyr::left_join(demo_bp_data |> 
                     sjlabelled::get_label() |> 
                     tibble::enframe())

## Add in data type
demo_bp_cb <- demo_bp_data |> 
  dplyr::summarise(
    across(everything(), class)
  ) |> 
  dplyr::mutate(
    recordeddate = "date"
  ) |> 
  dplyr::distinct() |> 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "name",
                      values_to = "type") |> 
  dplyr::right_join(demo_bp_cb)

# do_tidy_select <- function(df, expression){
#   expression <- rlang::enquo(expression)
#   expression <- tidyselect::eval_select(expression, data = df)
#   rlang::set_names(df[expression], names(expression))
# }

demo_bp_cb <- demo_bp_data |> 
  dplyr::summarise(
    across(where(is.numeric),
           ~paste0("Range: ",
                   min(.x, na.rm = TRUE), " - ",
                   max(.x, na.rm = TRUE)))
  ) |> 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "name",
                      values_to = "values") |> 
  dplyr::full_join(demo_bp_cb, by = join_by(name)) |> 
  dplyr::mutate(
    values = dplyr::coalesce(values.x, values.y),
    value = stringr::str_to_sentence(value),
    value = ifelse(value == "", "Whether participants viewed a body-positivity post featuring an average-sized model, larger model, or a control image about travel", value)
  ) |> 
  dplyr::select(-contains(".")) |> 
  dplyr::arrange(factor(name, levels = demo_vars))

readr::write_csv(demo_bp_cb, here::here("data/bp_demo_codebook.csv"))

## Subscales
sub_vars <- bp_data |> 
  dplyr::select(!any_of(demo_vars)) |> 
  names() |> 
  gsub("(.*?)[0-9]+", "\\1", x = _) |> 
  unique()

sub_bp_data <- bp_data |> 
  dplyr::select(!any_of(demo_vars))

scale_ref <- tibble::tribble(
  ~scale, ~scale_full,
  "PANAS", "Positive and Negative Affect Scale",
  "BSS", "Body Satisfaction Scale (State)",
  "SAC", "Social Comparison",
  "BAS", "Body Appreciation Scale (Trait)",
  "PACSR", "Physical Appearance Comparison Scale, Revised"
)

sub_bp_cb <- sub_bp_data |> 
  sjlabelled::get_labels() |> 
  tibble::enframe() |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    values = unlist(value) |> paste0(collapse = ", ")
  ) |> 
  dplyr::select(-value) |> 
  dplyr::right_join(sub_bp_data |> 
                     sjlabelled::get_label() |> 
                     tibble::enframe()) |> 
  tidyr::separate_wider_regex(
    cols = name,
    c("name_pref" = "^[[:alpha:]]*", "item_number" = "[0-9]*")) |> 
  dplyr::mutate(
      subscale = dplyr::case_when(
        name_pref == "panas" & item_number %in% c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19) ~ "Positive",
        name_pref == "panas" ~ "Negative",
        .default = "Single scale"
      ),
      scale = toupper(name_pref)
    ) |> 
  dplyr::summarise(
    .by = c(name_pref, subscale),
    across(c(scale, values),
           ~paste(unique(.x))),
    items = paste0(item_number, collapse = ", "),
    value = ifelse(name_pref %in% c("panas", "bss"), paste0(value, collapse = ", "),
                   paste0(value, collapse = "</br>"))
  ) |> 
  dplyr::select(name_pref, scale, subscale, items, value, values) |> 
  dplyr::distinct()

sub_bp_cb <- scale_ref |> 
  dplyr::left_join(sub_bp_cb)

readr::write_csv(sub_bp_cb, here::here("data/bp_sub_codebook.csv"))

