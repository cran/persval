#' Plot spider chart for ten specific personal values
#'
#' This function generates a spider chart based on the Basic Human Values.
#' It requires exactly ten value categories corresponding to the 10 predefined
#' personal values. This visualization helps in understanding the individual
#' or average profile of personal values.
#'
#' @usage
#' tenvalplot(
#'    df,
#'    valueMap,
#'    instrument,
#'    corrected = TRUE,
#'    standdev = TRUE,
#'    na.rm = TRUE
#' )
#'
#' @importFrom fmsb radarchart
#' @importFrom stats setNames sd
#' @importFrom grDevices rgb
#' @importFrom graphics title
#' @param df A dataframe containing computed values for the ten basic human values.
#' @param valueMap A named vector where names correspond to the ten personal values:
#'        Conformity ("CO"), Tradition ("TR"), Benevolence ("BE"), Achievement ("AC"),
#'        Power ("PO"), Security ("SE"), Stimulation ("ST"), Self-direction ("SD"),
#'        Universalism ("UN"), Hedonism ("HE"). Vector values correspond to the column names
#'        in the dataframe.
#' @param instrument Character string specifying the type of instrument used.
#'        Supported instruments: "svs", "pvq40", "pvq21", "twivi", "tivi".
#' @param corrected Logical. Indicates if data are ipsatized; if FALSE, data are given as raw scores (variable ranges depending on the type of instrument used). Default is TRUE.
#' @param standdev Logical. When TRUE, plots mean ± SD lines; when FALSE, plots only the mean. Default is TRUE.
#' @param na.rm Logical. When TRUE, NAs are ignored; when FALSE, NAs are preserved and will affect calculations. Default is TRUE.
#' @return A spider chart visualizing the ten basic human values scores.
#' @examples
#' df <- data.frame(
#'   conf_comp = sample(1:6, 3, replace = TRUE),
#'   trad_comp = sample(1:6, 3, replace = TRUE),
#'   bene_comp = sample(1:6, 3, replace = TRUE),
#'   achie_comp = sample(1:6, 3, replace = TRUE),
#'   power_comp = sample(1:6, 3, replace = TRUE),
#'   sec_comp = sample(1:6, 3, replace = TRUE),
#'   stim_comp = sample(1:6, 3, replace = TRUE),
#'   selfdir_comp = sample(1:6, 3, replace = TRUE),
#'   univ_comp = sample(1:6, 3, replace = TRUE),
#'   hedo_comp = sample(1:6, 3, replace = TRUE))
#' valueMap <- c(CO = "conf_comp", TR = "trad_comp",
#'               BE = "bene_comp", AC = "achie_comp",
#'               PO = "power_comp", SE = "sec_comp",
#'               ST = "stim_comp", SD = "selfdir_comp",
#'               UN = "univ_comp", HE = "hedo_comp")
#' persval::tenvalplot(df = df, valueMap = valueMap, instrument = "pvq40",
#' corrected = FALSE)
#' @export

tenvalplot <- function(df, valueMap, instrument, corrected = TRUE, standdev = TRUE, na.rm = TRUE) {
  specificValues <- c("SD", "ST", "HE", "AC", "PO", "SE", "TR", "CO", "BE", "UN")

  if (!identical(sort(names(valueMap)), sort(specificValues))) {
    stop("Please provide exactly ten value mappings, one for each personal value.")
  }

  if (!all(valueMap %in% names(df))) {
    stop("One or more specified columns not found in the dataframe.")
  }

  orderedValueMap <- setNames(valueMap[match(specificValues, names(valueMap))], specificValues)
  valueMeans <- colMeans(df[, orderedValueMap, drop = FALSE], na.rm = na.rm)

  if (instrument == "svs") {
    if (corrected && any(valueMeans > 7.2 | valueMeans < -7.2)) {
      stop("Values exceed the expected range of ipsatized scores for the given instrument. If you provided raw scores, please set corrected = FALSE.")
    }
    if (!corrected && any(valueMeans > 7 | valueMeans < -1)) {
      stop("Values exceed the expected range of raw scores for the given instrument. If you provided ipsatized scores, please set corrected = TRUE.")
    }
  }
  if (instrument %in% c("pvq40", "pvq21", "twivi", "tivi")) {
    if (corrected && any(valueMeans > 4.5 | valueMeans < -4.5)) {
      stop("Values exceed the expected range of ipsatized scores for the given instrument. If you provided raw scores, please set corrected = FALSE.")
    }
    if (!corrected && any(valueMeans > 6 | valueMeans < 1)) {
      stop("Values exceed the expected range of raw scores for the given instrument. If you provided ipsatized scores, please set corrected = TRUE.")
    }
  }

  if (instrument %in% c("pvq40", "pvq21", "twivi", "tivi")) {
    radar_min <- if (corrected) -4.5 else 1
    radar_max <- if (corrected) 4.5 else 6
    axis_labels <- if (corrected) c(-4.5, -2.25, 0, 2.25, 4.5) else c(1, 2.25, 3.5, 4.75, 6)
  } else if (instrument == "svs") {
    radar_min <- if (corrected) -7.2 else -1
    radar_max <- if (corrected) 7.2 else 7
    axis_labels <- if (corrected) c(-7.2, -3.6, 0, 3.6, 7.2) else c(-1, 1, 3, 5, 7)
  } else {
    stop("Unsupported instrument type.")
  }

  radarData <- data.frame(t(valueMeans))
  colnames(radarData) <- specificValues
  radarData <- rbind(
    rep(radar_max, length(specificValues)),
    rep(radar_min, length(specificValues)),
    radarData
  )

  if (standdev && nrow(df) > 1) {
    valueSDs <- apply(df[, orderedValueMap, drop = FALSE], 2, sd, na.rm = na.rm)
    #valueSEs <- valueSDs / sqrt(nrow(df))
    radarData <- rbind(radarData, radarData[3, ] - valueSDs, radarData[3, ] + valueSDs)
    colori_linea <- c("black", "darkgray", "gray")
    colori_riempimento <- c(rgb(0, 0, 0, 0.15), rgb(0, 0, 0, 0.15), rgb(0, 0, 0, 0.15))
    tipi_linea <- c(1, 0, 0)
    larghezza_linea <- c(1.8, 1, 1)
    tipo_punti <- c(16, 32, 32)
  } else {
    colori_linea <- "black"
    colori_riempimento <- rgb(0, 0, 0, 0.15)
    tipi_linea <- 1
    larghezza_linea <- 1.8
    tipo_punti <- 16
  }

  fmsb::radarchart(df = radarData,
                   axistype = 1,
                   caxislabels = axis_labels,
                   palcex = 0.8,
                   pcol = colori_linea,
                   pfcol = colori_riempimento,
                   plty = tipi_linea,
                   plwd = larghezza_linea,
                   pty = tipo_punti,
                   vlcex = 0.8)

  title(main = "Spider Chart: Ten Basic Human Values")
}
