% Generated manually
\name{calculate_baseline_emissions_acm0001}
\alias{calculate_baseline_emissions_acm0001}
\title{Calculate ACM0001 baseline emissions}
\usage{calculate_baseline_emissions_acm0001(methane_generation_m3, baseline_capture_efficiency = 0, methane_density_t_per_m3 = 0.000716, gwp_ch4 = 28)}
\arguments{
  \item{methane_generation_m3}{Methane generated during the period in cubic metres.}
  \item{baseline_capture_efficiency}{Fraction of methane captured in the baseline scenario.}
  \item{methane_density_t_per_m3}{Methane density in tonnes per cubic metre.}
  \item{gwp_ch4}{Global warming potential for methane.}
}
\value{Numeric vector of baseline emissions in tonnes CO2e.}
\description{Estimates baseline greenhouse gas emissions assuming methane not destroyed by the project is released to the atmosphere.}
