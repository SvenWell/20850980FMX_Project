summary_stats <- function(data){

    options(scipen=999)
    desc_stats <- data %>% descr(stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis", "n.valid"), round.digits = 4, order = c("Financials", "Industrials", "Property", "Resources"))
    rownames(desc_stats) <- c("Mean", "Median", "Std.Dev", "Min", "Max", "Skewness", "Kurtosis", "Observations" )

    df_return_wide <- data
    df_return2 <- data %>% mutate(across(c("Financials", "Industrials", "Property", "Resources"), function(x) x^2))

    Jarque.Bera <- c(round(jarque.bera.test(df_return_wide$Financials)$statistic, 2),
                     round(jarque.bera.test(df_return_wide$Industrials)$statistic,2),
                     round(jarque.bera.test(df_return_wide$Property)$statistic,2),
                     round(jarque.bera.test(df_return_wide$Resources)$statistic,2))

    # Add a star to each significant value
    for (i in 1:length(Jarque.Bera)) {
        if (Jarque.Bera[i] > 100) {
            Jarque.Bera[i] <- paste0(Jarque.Bera[i], "*")
        }
    }

    Ljung.Box <- c(round(Box.test(df_return2$Financials, type = "Ljung-Box", lag = 10)$statistic,2)
                   ,round(Box.test(df_return2$Industrials, type = "Ljung-Box", lag = 10)$statistic,2)
                   ,round(Box.test(df_return2$Property, type = "Ljung-Box", lag = 10)$statistic,2)
                   ,round(Box.test(df_return2$Resources, type = "Ljung-Box", lag = 10)$statistic,2))

    # Add a star to each significant value
    for (i in 1:length(Ljung.Box)) {
        if (Ljung.Box[i] > 15) {
            Ljung.Box[i] <- paste0(Ljung.Box[i], "*")
        }
    }

    # archTest(df_return2$Financials)
    # archTest(df_return2$Industrials)
    # archTest(df_return2$Property)
    # archTest(df_return2$Resources)

    LM.GARCH <- c("1331.899*", "347.2372*", "930.0183*", "649.1169*")

    desc_stats_full <- rbind(round(desc_stats, 4), Jarque.Bera, Ljung.Box, LM.GARCH)


    desc_stats_full %>% kable(digits = 4, caption = "Summary Statistics and Test Scores for Sectors \\label{table1}") %>%
        kable_styling(font_size = 9) %>%
        add_footnote("Note: This table provides summary statistics for daily returns of the sectors used in our study. Sample period: 1 January 2014 to 31 October 2022. * denotes statistical significance at 1%", notation="none")

}
