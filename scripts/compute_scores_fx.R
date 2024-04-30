
these_vars <- grep("stars|rmars|sticsa", names(anx_data), value = TRUE) |> 
  gsub("(.*)_*?[0-9]+?", "\\1", x = _) |> 
  gsub("(.*)_$", "\\1", x = _) |> 
  unique()

test_vars <- these_vars[1:3]

create_subscale

col_string = "stars_test"
df <- anx_data

compute_mean_score <- function(df, col_string){
  
  df <- df |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      mean_score = mean(
        dplyr::c_across(
          dplyr::starts_with(
            col_string
          )
        ),
        na.rm = TRUE
      )
    ) 
  
  df <- dplyr::rename_with(df, ~paste0(col_string,"_mean"), "mean_score")
  
  return(df)
  
}

anx_data |> 
  compute_mean_score("stars_test")



purrr::(
  .x = anx_data,
  .y = test_vars,
  .f = ~compute_mean_score(.x, .y)
)


create_subscales <- function(df, col_string){
  
  sub_name <- paste0(col_string, "_score")
  
  sub_df <- df |> 
    dplyr::select(starts_with(col_string)) |> 
    dplyr::mutate(
      sub_mean = mean(c_across(everything()), na.rm = TRUE)
    ) |> 
    dplyr::rename_with(~print(sub_name), "sub_mean")
  
  dplyr::bind_cols(df, dplyr::select(sub_df, all_of(sub_name)))
}

create_subscales(anx_data, "stars_help")

purrr::map2(
  anx_data,
  test_vars,
  create_subscales
)

anx_data %>% 
  dplyr::bind_cols(
    purrr::map_dfc(.x = as.list(test_vars), 
                   .f = ~ .y %>% 
                     dplyr::rowwise() %>% 
                     dplyr::transmute(!!str_c("mean_", .x) := mean(c_across(starts_with(.x)), na.rm = TRUE)),
                   .y = .)
  ) |> 
  View()

