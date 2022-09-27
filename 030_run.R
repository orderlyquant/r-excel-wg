
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

library(tidyquant)
library(timetk)

library(GGally)
library(glue)

library(XLConnect)
library(XLConnectXTRA)

source("010_funcs.R")



# report specifics --------------------------------------------------------

securities <- c("AAPL", "MSFT", "NVDA", "GOOGL", "META")
benchmark <- "SPY"

all_symbols <- c(securities, benchmark)

dr <- calc_dr("ttm")

rpt_period <- glue("Period: {dr[1]} to {dr[2]}")


# retrieve security data --------------------------------------------------

sec_return_long <- get_prices_and_returns(
  all_symbols,
  dr[1], dr[2]
) %>%
  mutate(symbol = factor(symbol, all_symbols))


sec_return_wide <- sec_return_long %>%
  select(symbol, date, return) %>%
  pivot_wider(names_from = symbol, values_from = return)



# group work --------------------------------------------------------------



corr_plot <- sec_return_wide %>%
  select(-date) %>%
  ggcorr(
    method = c("pairwise.complete.obs", "pearson"),
    label = TRUE,
    label_round = 2,
    limits = FALSE,
    midpoint = 0.5
  )

stat_tbl <- build_summary_tbl(sec_return_long)

group_report_list <- list(
  XLConnectXTRA::make_reportable(
    rpt_period, "period", incl_header = FALSE
  ),
  XLConnectXTRA::make_reportable(
    stat_tbl, "stat_tbl", incl_header = FALSE
  ),
  XLConnectXTRA::make_reportable(
    corr_plot, "corr_plot", dims = c(4, 4), os = FALSE, scale = 1.5
  )
)


# security work -----------------------------------------------------------

sec_tbl <- sec_return_long %>%
  group_by(symbol) %>%
  nest() %>%
  mutate(
    return_plot = map(data, build_return_plot),
    drawdown_plot = map(data, build_drawdown_plot)
  ) %>%
  mutate(
    report = pmap(list(symbol, return_plot, drawdown_plot), security_report)
  )


# reporting ---------------------------------------------------------------

template_file <- "report-template.xlsx"
output_file <- "sample-report.xlsx"

report_obj <- XLConnectXTRA::load_xl_template(template_name = template_file)


# GROUP - single tab
XLConnectXTRA::report_reportables(
  xl_obj = report_obj,
  tpl_name = "group_report",
  instance_name = "Summary",
  rpt_list = group_report_list
)

# SECURITY - multiple tabs
sec_tbl %>%
  mutate(symbol = as.character(symbol)) %>%
  select(symbol, report) %>%
  pmap(
    ~XLConnectXTRA::report_reportables(
      xl_obj = report_obj,
      tpl_name = "security_report",
      instance_name = ..1,
      rpt_list = ..2
    )
  )



# save and cleanup --------------------------------------------------------

XLConnectXTRA::cleanup_template(report_obj)

XLConnect::saveWorkbook(
  report_obj$wb,
  file = output_file
)

rm(report_obj);XLConnect::xlcFreeMemory()


