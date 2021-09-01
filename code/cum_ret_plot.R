# Plotting function for cumulative returns

cum_ret_plot <- \(gg, sum_gg){

    gg  %>% arrange(bp) %>%
        ggplot() +
        theme_bw() +
        # Plot lines
        geom_line(data = . %>% filter(bp %in% "Return - 0 bps"), aes(date, CP), color = "darkgreen", alpha = 0.8) +
        geom_line(data = . %>% filter(!bp %in% "Return - 0 bps"), aes(date, CP, colour = bp), alpha = 0.6, show.legend = TRUE) +
        # Chnage Labels
        labs(title = "Fee Impact of Cumulated Wealth",
             subtitle = paste0("Base Return: FTSE JSE All Share Index | Start Date: January ", min(gg$date) %>% year()), y = "Cumulative Returns", x = "", colour = "Type") +
        # Change y axis
        scale_y_continuous(n.breaks = 5) +
        # Change x axis
        scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 years") +
        # Set x axis to vertical, set size
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 8), plot.subtitle = element_text(size = 8),
              axis.title.y = element_text(size = 8),
              legend.text = element_text(size = 6), legend.title = element_text(size = 8)
        ) +
        # Change colours and legend
        scale_color_manual(breaks = c("Return - 10 bps", "Return - 50 bps", "Return - 100 bps", "Return - 200 bps", "Return - 250 bps", "Return - 350 bps")
                           , values = c("coral", "gold", "seagreen1", "cyan", "slateblue1", "violet"))+
        # Include cumulative performance
        geom_text(data = sum_gg %>% group_by(bp) %>% mutate(date = date %m+% months(8)), aes(x = date, y = CP, label = prop), color = "maroon", size = ggpts(7)) +
        # Include label for different performances
        geom_label(data = data.frame(date = min(gg$date) %>% as.Date() , y = max(gg$CP)*0.8)
                   , aes(x = date, y = y, label = glue("R1m invested in January ", min(gg$date) %>% year() , "\n \u2022 10 bps: R", round(sum_gg[1, "CP"] %>% pull(), 2), "\n \u2022 250 bps: R", round(sum_gg[5, "CP"] %>% pull(), 2), "\n \u2022 350 bps: R", round(sum_gg[6, "CP"] %>% pull(), 2))),
                   nudge_x = difftime(max(gg$date) %>% as.Date, min(gg$date) %>% as.Date(), units = "days")[[1]] * 0.2, color = "dark green", size = ggpts(8))

}

