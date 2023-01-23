portfolio_return_function <- function(data = data, sector = "", index = ""){
    stocks <- data

    if(!sector == ""){
        stocks <- data %>% filter(Sector %in% sector) %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!index == ""){
        stocks <- data %>% filter(Index_Name %in% index) %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!sector == "" & !index == ""){
        stocks <- data %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }

    # First we get the weights of the ALSI and SWIX into xts format
    j200_weights <- stocks %>% dplyr:::select(date, Tickers, J200) %>% spread(Tickers, J200) %>% tbl_xts()
    j400_weights <- stocks %>% dplyr:::select(date, Tickers, J400) %>% spread(Tickers, J400) %>% tbl_xts()

    # Then we get the returns for all the Tickers into xts format
    stock_returns <- stocks %>% dplyr:::select(date, Tickers, Return) %>% spread(Tickers, Return)

    # Forcing the NA values to 0
    # We can do this as NA weights are just 0
    j200_weights[is.na(j200_weights)] <- 0
    j400_weights[is.na(j400_weights)] <- 0

    # For NA's in returns it is safer to impute values for the NA's
    stock_returns <- impute_missing_returns(stock_returns, impute_returns_method = "Drawn_Distribution_Collective")
    stock_returns <- stock_returns %>% tbl_xts()


    J200_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns,
                                                   weights = j200_weights, lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)

    J400_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns,
                                                   weights = j400_weights,
                                                   lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)

    # Need to clean and save the data

    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl()

    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl()

    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl()

    # Clean and save portfolio returns and weights:
    J400_Contribution <- J400_RetPort$"contribution" %>% xts_tbl()

    J400_BPWeight <- J400_RetPort$"BOP.Weight" %>% xts_tbl()

    J400_BPValue <- J400_RetPort$"BOP.Value" %>% xts_tbl()

    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))
    names(J400_Contribution) <- c("date", names(J400_RetPort$"contribution"))
    names(J400_BPWeight) <- c("date", names(J400_RetPort$"BOP.Weight"))
    names(J400_BPValue) <- c("date", names(J400_RetPort$"BOP.Value"))


    # Let's bind all of these together now:
    df_port_return_J200 <-
        left_join(stocks %>% dplyr:::select(date, Tickers, Return),
                  J200_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J200_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    df_port_return_J400 <-
        left_join(stocks %>% dplyr:::select(date, Tickers, Return),
                  J400_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J400_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(., J400_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    # Calculate Portfolio Returns:
    df_Portf_J200 <- df_port_return_J200 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    # Calculate Portfolio Returns:
    df_Portf_J400 <- df_port_return_J400 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    output <- left_join(df_Portf_J200 %>% rename(J200 = PortfolioReturn),
                        df_Portf_J400 %>%  rename(J400 = PortfolioReturn),
                        by = "date") %>%
        pivot_longer(c("J200", "J400"), names_to = "Portfolio", values_to = "Returns")
    output
}
