### Cumulative Returns Function

cum_ret_func <- \(dat, bp){
    idx <- dat %>%
        mutate(YM = format(date, "%Y%B")) %>%
        arrange(date) %>%
        group_by(YM) %>% filter(date == last(date)) %>%
        ungroup() %>%
        mutate(ret = (TRI/lag(TRI) - 1) - ((bp/10000)/12)) %>% select(date,
                                                                      ret) %>%
        arrange(date) %>%
        mutate(Rets = coalesce(ret, 0)) %>%
        mutate(CP = cumprod(1 + Rets)) %>%
        mutate(bp = as.factor(paste0("Return - ", as.character(bp), " bps"))) %>%
        ungroup()
}