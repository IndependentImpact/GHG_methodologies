% Generated manually
\name{aggregate_monitoring_periods}
\alias{aggregate_monitoring_periods}
\title{Aggregate monitoring periods for ACM0001}
\usage{aggregate_monitoring_periods(monitoring_data)}
\arguments{
  \item{monitoring_data}{Tibble or data frame containing landfill gas monitoring observations with ACM0001-required columns.}
}
\value{A tibble summarising each monitoring period with emissions calculations.}
\description{Summarises landfill gas monitoring data into period-level totals that follow the ACM0001 baseline, project, leakage, and emission reduction workflow.}
