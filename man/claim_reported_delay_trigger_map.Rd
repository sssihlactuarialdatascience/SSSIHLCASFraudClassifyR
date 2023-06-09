% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{claim_reported_delay_trigger_map}
\alias{claim_reported_delay_trigger_map}
\title{claim reporting delay based fraud trigger}
\usage{
claim_reported_delay_trigger_map(
  claims_file,
  treatment_end_date_field,
  claim_reported_date_field,
  claim_delay_days
)
}
\arguments{
\item{claims_file}{data set containing claim-level information}

\item{treatment_end_date_field}{column in the claims_file that contains the ending date of the treatment}

\item{claim_reported_date_field}{column in the claims_file that contains the claim reported date of the policy}

\item{claim_delay_days}{a user input that sets a permissible time period within which a claim should be reported}
}
\value{
input claims_file with claims_delay_flag column
}
\description{
For each policy there is a treatment start date and treatment end date. The claim should be reported within the permissible days after treatment end date(discharge). If the claim reported date is outside the permissible limit it will raise a flag
}
\examples{

library(dplyr)

data(claims_data_sample)

claim_reported_delay_trigger_map(
claims_file = claims_data_sample,
treatment_end_date_field = "treatment_end_date",
claim_reported_date_field = "claim_reported_date",
claim_delay_days = 15) \%>\%
select(c("treatment_end_date", "claim_reported_date", "claim_reported_delay_flag"))

}
