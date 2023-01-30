portfolio_return_function_alsi <- function(data = data, sector = "", index = ""){
    stocks <- data

    if(!sector == ""){
        stocks <- data %>% filter(Sector %in% sector) %>%
            group_by(date) %>%
            mutate(J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!index == ""){
        stocks <- data %>% filter(Index_Name %in% index) %>%
            group_by(date) %>%
            mutate(J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!sector == "" & !index == ""){
        stocks <- data %>%
            group_by(date) %>%
            mutate(J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }

    # First we get the weights of the ALSI and SWIX into xts format
    j200_weights <- stocks %>% dplyr:::select(date, Tickers, J200) %>% spread(Tickers, J200) %>% tbl_xts()

    # Then we get the returns for all the Tickers into xts format
    stock_returns <- stocks %>% dplyr:::select(date, Tickers, Return) %>% spread(Tickers, Return)

    # Forcing the NA values to 0
    # We can do this as NA weights are just 0
    j200_weights[is.na(j200_weights)] <- 0

    # For NA's in returns it is safer to impute values for the NA's
    stock_returns <- impute_missing_returns(stock_returns, impute_returns_method = "Drawn_Distribution_Collective")
    stock_returns <- stock_returns %>% tbl_xts()


    J200_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns,
                                                   weights = j200_weights, lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)

    # Need to clean and save the data

    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl()

    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl()

    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl()


    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))

    # Let's bind all of these together now:
    df_port_return_J200 <-
        left_join(stocks %>% dplyr:::select(date, Tickers, Return),
                  J200_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))


    # Calculate Portfolio Returns:
    df_Portf_J200 <- df_port_return_J200 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)


    output <- df_Portf_J200 %>% rename(J200 = PortfolioReturn) %>%
        pivot_longer("J200", names_to = "Portfolio", values_to = "Returns")
    output
}
