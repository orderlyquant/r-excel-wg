get_prices_and_returns <- function(tkr, from, to) {

  tbl <- tidyquant::tq_get(tkr, from = from, to = to)

  tbl <- tbl %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(adjusted_lag = dplyr::lag(adjusted)) %>%
    dplyr::mutate(return = (adjusted / adjusted_lag) - 1) %>%
    dplyr::mutate(return = ifelse(is.na(return), 0, return)) %>%
    dplyr::ungroup()

  return(tbl)

}

calc_dr <- function(pd) {

  # this date convention works with tq_get, which returns
  # pricing up until the previous day
  today <- Sys.Date()
  dplyr::case_when(
    pd == "qtd" ~ c(lubridate::floor_date(today, "quarter") - 1, today),
    pd == "ytd" ~ c(lubridate::floor_date(today, "year") - 1, today),
    pd == "ttm" ~ c(today - lubridate::years(1), today)
  )
}

build_summary_tbl <- function(df) {
  return_stats_tbl <- df %>%
    filter(symbol != benchmark) %>%
    select(symbol, date, return) %>%
    left_join(
      df %>%
        filter(symbol == benchmark) %>%
        select(date, return),
      by = "date",
      suffix = c("", "_bm")
    ) %>%
    group_by(symbol) %>%
    nest() %>%
    mutate(
      capm = map(data, ~tq_performance(..1, Ra = return, Rb = return_bm, performance_fun = table.CAPM)),
      downside = map(data, ~tq_performance(..1, Ra = return, Rb = return_bm, performance_fun = table.DownsideRisk))
    )


  capm_tbl <- return_stats_tbl %>%
    select(symbol, capm) %>%
    unnest(cols = capm) %>%
    select(
      symbol, Alpha, Beta, `Beta+`, `Beta-`, Correlation
    ) %>%
    mutate(
      Alpha = (1 + Alpha)^252 - 1    # approx. annualize
    )

  downside_tbl <- return_stats_tbl %>%
    select(symbol, downside) %>%
    unnest(cols = downside) %>%
    select(symbol, VaR = `HistoricalVaR(95%)`, dd = MaximumDrawdown)

  perf_tbl <-
    df %>%
    filter(symbol != benchmark) %>%
    group_by(symbol) %>%
    summarize(
      period_return = prod(1+return)-1,
      vol = sd(return) * sqrt(252)
    )

  stat_tbl <- perf_tbl %>%
    left_join(capm_tbl, by = "symbol") %>%
    left_join(downside_tbl, by = "symbol") %>%
    relocate(Correlation, .after = dd)

  return(stat_tbl)
}


build_return_plot <- function(tbl) {

  tbl %>%
    mutate(
      cum_return = cumprod(1 + return) - 1
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = date, y = cum_return)
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = NULL, y = NULL) +
    tidyquant::theme_tq()

}

build_drawdown_plot <- function(tbl) {

  tbl %>%
    tk_zoo(select = "return", date_var = "date") %>%
    PerformanceAnalytics::Drawdowns() %>%
    tk_tbl() %>%
    set_names(c("date", "drawdown")) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = date, y = drawdown)
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = NULL, y = NULL) +
    tidyquant::theme_tq()
}


security_report <- function(sym, ret_plot, dd_plot) {
  list(
    XLConnectXTRA::make_reportable(
      sym, "symbol", incl_header = FALSE
    ),
    XLConnectXTRA::make_reportable(
      ret_plot, "return_plot", dims = c(3, 7), os = FALSE, scale = 1
    ),
    XLConnectXTRA::make_reportable(
      dd_plot, "drawdown_plot", dims = c(3, 7), os = FALSE, scale = 1
    )
  )

}
