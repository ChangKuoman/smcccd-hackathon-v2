data_cleaning <- function(df) {
  df %>% 
    clean_names() %>% ### janitor
    mutate(across(c(kda, w_rate, pick_rate, role_perc, ban_perc), as.numeric)) %>%
    mutate(
      name = str_to_title(name),
      role = str_to_title(role)
    )
}