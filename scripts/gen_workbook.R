
file_path <- file.path(getwd(), "tutorials", "01_fundRmentals", "03_lm.qmd")

text <- readLines(file_path)

ind_divs <- grep("^:::", text)

ind_ex <- grep("callout-note.*?Exercise", text)

start_ind <- ind_ex + 1
end_ind <- ind_divs[which(ind_divs %in% ind_ex) + 1] -1

# text[start_ind:end_ind]

# purrr::pmap(
#   .l = list(
#     start_ind,
#     end_ind,
#     text),
#   .f = ~ text[start_ind:end_ind]
#   )

# http://adv-r.had.co.nz/Subsetting.html#subassignment