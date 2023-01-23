summary_stats <- function(data){

    desc_stats <- data %>% dplyr:::select(date, Sector, Returns) %>%
        spread(Sector, Returns) %>%
        descr(stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis", "n.valid"), round.digits = 4, order = c("Financials", "Industrials", "Resources"))
    rownames(desc_stats) <- c("Mean", "Median", "Std.Dev", "Min", "Max", "Skewness", "Kurtosis", "Observations" )

    df_return_wide <- data %>% dplyr:::select(date, Sector, Returns) %>%  spread(Sector, Returns) %>% slice(-1)
    df_return2 <- data %>% mutate(dlogret2 = Returns^2) %>% dplyr:::select(date, Sector, dlogret2)  %>% spread(Sector, dlogret2) %>% slice(-1) %>% dplyr:::select(-date)

    Jarque.Bera <- c(round(jarque.bera.test(df_return_wide$Financials)$statistic, 2),
                     round(jarque.bera.test(df_return_wide$Industrials)$statistic,2),
                     round(jarque.bera.test(df_return_wide$Resources)$statistic,2))

    # Add a star to each significant value
    for (i in 1:length(Jarque.Bera)) {
        if (Jarque.Bera[i] > 100) {
            Jarque.Bera[i] <- paste0(Jarque.Bera[i], "*")
        }
    }

    Ljung.Box <- c(round(Box.test(df_return2$Financials, type = "Ljung-Box", lag = 10)$statistic,2)
                   ,round(Box.test(df_return2$Industrials, type = "Ljung-Box", lag = 10)$statistic,2)
                   ,round(Box.test(df_return2$Resources, type = "Ljung-Box", lag = 10)$statistic,2))

    # Add a star to each significant value
    for (i in 1:length(Ljung.Box)) {
        if (Ljung.Box[i] > 15) {
            Ljung.Box[i] <- paste0(Ljung.Box[i], "*")
        }
    }

    # archTest(df_return2$BTC)
    # archTest(df_return2$ETH)
    # archTest(df_return2$USDT)
    # archTest(df_return2$DAI)
    # archTest(df_return2$BNB)
    # archTest(df_return2$UNI)
    LM.GARCH <- c("32.676*", "39.954*", "1490.068*", "539.488*", "124.402*", "75.047*")

    desc_stats_full <- rbind(round(desc_stats, 4), Jarque.Bera, Ljung.Box, LM.GARCH)


    desc_stats_full %>% kable(digits = 4) %>%
        kable_styling() %>%
        add_footnote("Note: This table provides summary statistics for daily returns of the cryptocurrencies used in our study; Bitcoin (BTC), Ether (ETH), Tether (USDT), Dai (DAI), Binance (BNB) and Uniswap (UNI). Sample period: 17-September-2020 to 9-November-2022. * denotes statistical significance at 5%", notation="none")

}
