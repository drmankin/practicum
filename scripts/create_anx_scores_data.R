library(tidyverse)

df <- readr::read_csv(here::here("data/anx_data.csv"))


## Extract common portions of variable names without numbers
subscales <- gsub("((stars|rmars|sticsa)_[a-z|_]*?)(_)?[0-9]+", "\\1", 
     grep("stars|rmars|sticsa", names(df), value = TRUE)) |> 
  unique() |>
  as.list()


df_scores <- df %>% 
  bind_cols(
    map_dfc(.x = subscales, 
            .f = ~ .y %>% 
              rowwise() %>% 
              transmute(!!str_c(.x, "_score") := mean(c_across(starts_with(.x)), na.rm = TRUE)),
            .y = .)
  )

mcq_sum <-  gsub("(.*)_[1-5]", "\\1", 
                 grep("mcq.*[1-5]", names(df), value = TRUE)) |> 
  unique() |>
  as.list()

df_scores <- df_scores %>% 
  bind_cols(
    map_dfc(.x = mcq_sum, 
            .f = ~ .y %>% 
              rowwise() %>% 
              transmute(!!str_c(.x, "_score") := sum(c_across(starts_with(.x)))),
            .y = .)
  )

df_scores <- df_scores |> 
  dplyr::mutate(
    mcq_score = dplyr::coalesce(mcq_stats_score, mcq_maths_score)
  )

df_scores <- df_scores |> 
  dplyr::select(matches("^[a-z]+$")|matches(str_c(unlist(subscales), "_score")), mcq_score)

readr::write_csv(df_scores, here::here("data/anx_scores_data.csv"))
