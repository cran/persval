% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tenvalplot.R
\name{tenvalplot}
\alias{tenvalplot}
\title{Plot spider chart for ten specific personal values}
\usage{
tenvalplot(
   df,
   valueMap,
   instrument,
   corrected = TRUE,
   standdev = TRUE,
   na.rm = TRUE
)
}
\arguments{
\item{df}{A dataframe containing computed values for the ten basic human values.}

\item{valueMap}{A named vector where names correspond to the ten personal values:
Conformity ("CO"), Tradition ("TR"), Benevolence ("BE"), Achievement ("AC"),
Power ("PO"), Security ("SE"), Stimulation ("ST"), Self-direction ("SD"),
Universalism ("UN"), Hedonism ("HE"). Vector values correspond to the column names
in the dataframe.}

\item{instrument}{Character string specifying the type of instrument used.
Supported instruments: "svs", "pvq40", "pvq21", "twivi", "tivi".}

\item{corrected}{Logical. Indicates if data are ipsatized; if FALSE, data are given as raw scores (variable ranges depending on the type of instrument used). Default is TRUE.}

\item{standdev}{Logical. When TRUE, plots mean ± SD lines; when FALSE, plots only the mean. Default is TRUE.}

\item{na.rm}{Logical. When TRUE, NAs are ignored; when FALSE, NAs are preserved and will affect calculations. Default is TRUE.}
}
\value{
A spider chart visualizing the ten basic human values scores.
}
\description{
This function generates a spider chart based on the Basic Human Values.
It requires exactly ten value categories corresponding to the 10 predefined
personal values. This visualization helps in understanding the individual
or average profile of personal values.
}
\examples{
df <- data.frame(
  conf_comp = sample(1:6, 3, replace = TRUE),
  trad_comp = sample(1:6, 3, replace = TRUE),
  bene_comp = sample(1:6, 3, replace = TRUE),
  achie_comp = sample(1:6, 3, replace = TRUE),
  power_comp = sample(1:6, 3, replace = TRUE),
  sec_comp = sample(1:6, 3, replace = TRUE),
  stim_comp = sample(1:6, 3, replace = TRUE),
  selfdir_comp = sample(1:6, 3, replace = TRUE),
  univ_comp = sample(1:6, 3, replace = TRUE),
  hedo_comp = sample(1:6, 3, replace = TRUE))
valueMap <- c(CO = "conf_comp", TR = "trad_comp",
              BE = "bene_comp", AC = "achie_comp",
              PO = "power_comp", SE = "sec_comp",
              ST = "stim_comp", SD = "selfdir_comp",
              UN = "univ_comp", HE = "hedo_comp")
persval::tenvalplot(df = df, valueMap = valueMap, instrument = "pvq40",
corrected = FALSE)
}
