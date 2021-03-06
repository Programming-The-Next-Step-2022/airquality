% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{current_aq_df}
\alias{current_aq_df}
\title{Current Airquality Data}
\usage{
current_aq_df(city = "Amsterdam", country = "NL")
}
\arguments{
\item{city}{City (in quotation marks)}

\item{country}{Country Code (according to ISO 3166; e.g.: NL for Netherlands; in quotation marks)}
}
\value{
The airquality index (aqi) and the subcomponents (co, no, no2, o3, so2, pm2_5, pm10, nh3) in a list object.
}
\description{
With this function you can get the current air quality data of any town.
}
\details{
The airquality index ranges from 1 = Good to 5 = Very Poor.
}
\examples{
current_aq_df("Amsterdam", "NL")

}
