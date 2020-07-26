sourceCpp("exponential_decay_analytical.cpp")

computeHistory <- function(df, taus, mixed_state, mixed_value){
  purrr::map(taus, ~compute_history_analytical(data.frame(df), ., mixed_state, mixed_value)) %>%
    dplyr::bind_rows() %>%
    data.frame()
}


