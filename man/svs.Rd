% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svs.R
\name{svs}
\alias{svs}
\title{Compute personal values from SVS-57 data}
\usage{
svs(
  df,
  items = NULL,
  compute = "all",
  correction = TRUE,
  na.rm = TRUE
)
}
\arguments{
\item{df}{A data frame containing the raw responses for the SVS items.
If "items" is not provided, it must have exactly 57 columns, and their order must
correspond to the SVS items.}

\item{items}{An optional vector containing the names or indices of the columns that correspond
to the SVS items. Must be exactly 57 items. If NULL, the function assumes the items
are the only columns given in the "df" parameter.}

\item{compute}{Character. Indicates which personal values scores to compute and return.
Possible values are "all" (default; computes all first and second-order personal values scores),
"ten.values" (computes the ten basic personal values scores),
 "four.higher" (computes the four main higher-order values scores),
 "two.foci" (computes two higher-order values scores: self-focused values that regulate personal interests and characteristics, and
    other-focused values that regulate social relationships),
    "two.anxiety" (computes two higher-order values based on their relation to anxiety:
    self-protective values aimed at coping with anxiety due to uncertainty in the social and physical world, and
    growth-oriented or self-expansive values that express anxiety-free motivations).}

\item{correction}{Logical. When TRUE, the scores are corrected for individual differences
in the use of the response scale. Default is TRUE.}

\item{na.rm}{Logical. When TRUE, NAs are ignored in calculations; when FALSE,
NAs are preserved and will affect calculations. Default is TRUE.}
}
\value{
A data frame with computed values. If both "df" and "items" parameters are provided,
        the returned data frame includes the original data with the calculations appended as new columns.
}
\description{
This function computes personal values scores and different higher-order
dimensions related to Schwartz's theoretical framework based on the 57 items
of the Schwartz Value Survey (Schwartz, 1992).
By default, the function applies the recommended statistical adjustment
(MRAT centering) as suggested by the authors, to correct for
individual differences in response styles thus enhancing the interpretative
validity of the scores.
}
\note{
Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net,
      giuseppe.corbelli@uniroma1.it
}
\examples{
persval::svs(df = data.frame(
  svs1 = c(3, 1, 4), svs2 = c(2, 5, 3), svs3 = c(1, 5, 2), svs4 = c(4, 3, 5),
  svs5 = c(5, 2, 1), svs6 = c(3, 4, 2), svs7 = c(1, 2, 4), svs8 = c(3, 1, 5),
  svs9 = c(2, 4, 1), svs10 = c(5, 3, 2), svs11 = c(1, 4, 3), svs12 = c(2, 1, 5),
  svs13 = c(3, 5, 4), svs14 = c(1, 2, 3), svs15 = c(4, 5, 1), svs16 = c(2, 3, 4),
  svs17 = c(5, 1, 2), svs18 = c(3, 4, 1), svs19 = c(2, 3, NA), svs20 = c(1, 3, 4),
  svs21 = c(2, 5, 1), svs22 = c(4, 1, 5), svs23 = c(3, 4, 2), svs24 = c(5, 1, 3),
  svs25 = c(4, 2, 5), svs26 = c(1, 3, 2), svs27 = c(5, 4, 1), svs28 = c(2, 1, 4),
  svs29 = c(3, 5, 2), svs30 = c(1, 4, 3), svs31 = c(2, 3, 5), svs32 = c(4, 1, NA),
  svs33 = c(3, 5, 4), svs34 = c(1, 2, 3), svs35 = c(4, 1, 5), svs36 = c(2, 3, 4),
  svs37 = c(5, 2, 1), svs38 = c(4, 3, 2), svs39 = c(1, 5, 3), svs40 = c(2, 4, 1),
  svs41 = c(5, 1, 2), svs42 = c(3, 2, 4), svs43 = c(2, 5, 3), svs44 = c(4, 1, 5),
  svs45 = c(3, 2, 4), svs46 = c(1, 5, 3), svs47 = c(4, 3, 2), svs48 = c(5, 1, 2),
  svs49 = c(3, 4, 1), svs50 = c(2, 5, NA), svs51 = c(1, 4, 3), svs52 = c(2, 1, 5),
  svs53 = c(3, 5, 4), svs54 = c(1, 2, 3), svs55 = c(4, 5, 1), svs56 = c(2, 3, 4),
  svs57 = c(5, 1, 2)),
correction = TRUE,
compute = "all",
na.rm = TRUE)

}
