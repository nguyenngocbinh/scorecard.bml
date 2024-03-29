% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/woe_cal_df.R
\name{woe_cal_df}
\alias{woe_cal_df}
\title{woe_cal_df}
\usage{
woe_cal_df(data, woe_table)
}
\arguments{
\item{data}{The data is to be computed.}

\item{woe_table}{The table is calculated woe}
}
\value{
The woe data
\itemize{
  \item woe_data. The data is computed woe.
}
}
\description{
Compute the woe_cal that shows the Weights Of Evidence (WOE) for each group and respeective Information Values (IVs).
}
\details{
For a given actual for a Binary Y variable and a categorical X variable stored as factor, the WOE table is generated with calculated WOE's and IV's
}
\examples{
data('hmeq')
hmeq1 <- hmeq[,c("bad", "reason", "job")]
woe_table <- woe_cal_table(df = hmeq1, y = "bad")
df_woe <- woe_cal_df(data = hmeq, woe_table = woe_table)
}
\author{
Nguyen Ngoc Binh \email{nguyenngocbinhneu@gmail.com}
}
