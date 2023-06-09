% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_gender.R
\name{gender_trigger_map}
\alias{gender_trigger_map}
\title{Gender-based fraud trigger}
\usage{
gender_trigger_map(
  claims_file,
  gender_column_name,
  triggers_file,
  procedure_code,
  gender_trigger
)
}
\arguments{
\item{claims_file}{data set containing claim-level information}

\item{gender_column_name}{column in the claims_file that contains the gender of the policy holder}

\item{triggers_file}{file containing the gender specification for each procedure code}

\item{procedure_code}{column in the triggers_file that contains the code/identifier for the medical procedure}

\item{gender_trigger}{column in the triggers_file that contains the specification of gender for each medical procedure}
}
\value{
input claims_file with gender_flag column
}
\description{
Some procedures are gender specific. This function creates a red flag if a procedure is performed for a claimant for a wrong gender. For example gynacologic procedure for a male claimant will raise a redflag.
             The gender should be specified consistently in the claims_file and the triggers_file for this function to work.
}
\examples{

library(dplyr)

data(claims_data_sample)
data(trigger_data)

gender_trigger_map(
claims_file = claims_data_sample,
gender_column_name = "gender_code",
triggers_file = trigger_data,
procedure_code = "primary_procedure_code",
gender_trigger = "gender_trigger") \%>\%
group_by(gender_code)\%>\%
summarise(gender_flag = mean(gender_flag))

}
