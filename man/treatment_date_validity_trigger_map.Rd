% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{treatment_date_validity_trigger_map}
\alias{treatment_date_validity_trigger_map}
\title{treatment date validity-based fraud trigger}
\usage{
treatment_date_validity_trigger_map(
  claims_file,
  treatment_start_date_field,
  policy_commencement_date_field,
  policy_termination_date_field
)
}
\arguments{
\item{claims_file}{data set containing claim-level information}

\item{treatment_start_date_field}{column in the claims_file that contains the starting date of the treatment}

\item{policy_commencement_date_field}{column in the claims_file that contains the commencement date of the policy}

\item{policy_termination_date_field}{column in the claims_file that contains the termination date of the policy}
}
\value{
input claims_file with treatment_date_validity_flag column
}
\description{
For each policy there is a commencement and termination date within which the claim event(treatment) should occur. If the treatment date is outside these dates it will raise a flag
}
\examples{

library(dplyr)

data(claims_data_sample)

treatment_date_validity_trigger_map(
claims_file = claims_data_sample,
treatment_start_date_field = "treatment_start_date",
policy_commencement_date_field = "policy_commencement_date",
policy_termination_date_field = "policy_termination_date") \%>\%
select(c("treatment_start_date", "policy_commencement_date", "treatment_date_validity_flag"))

}
