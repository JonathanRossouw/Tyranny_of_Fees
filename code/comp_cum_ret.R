# Comparative return from different fees
comp_cum_ret <- \(gg){
    gg %>% group_by(bp) %>%
        filter(date == last(date)) %>%
        ungroup() %>%
        mutate(CP_top = slice_max(arrange(., bp), 1)$CP) %>%
        mutate(prop = paste0(round(-(CP_top - CP)/CP_top*100, 1), "%")) %>%
        slice(2:n())
}
