#' Plot spider chart for four higher-order personal values
#'
#' This function generates a spider chart based on four higher-order Basic Human Values:
#' Openness to Change, Self-Enhancement, Conservation, and Self-Transcendence.
#' This visualization helps in understanding the profile of higher-order values for individuals or groups.
#'
#' @usage
#' fourvalplot(
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
#' @param df A dataframe containing computed values for the four higher-order personal values.
#' @param valueMap A named vector where names are the four higher-order personal values:
#'        Openness to Change ("OPEN"), Self-enhancement ("SELFENH"), Conservation ("CONS"),
#'        Self-transcendence ("SELFTRANS"). Vector values correspond to the column names in the dataframe.
#' @param instrument Character string specifying the type of instrument used.
#'        Supported instruments: "svs", "pvq40", "pvq21", "twivi", "tivi".
#' @param corrected Logical. Indicates if data are ipsatized; if FALSE, data are given as raw scores (variable ranges depending on the type of instrument used). Default is TRUE.
#' @param standdev Logical. When TRUE, plots mean ± SD lines; when FALSE, plots only the mean. Default is TRUE.
#' @param na.rm Logical. When TRUE, NAs are ignored; when FALSE, NAs are preserved and will affect calculations. Default is TRUE.
#' @return A spider chart visualizing the four higher-order values scores.
#' @examples
#' df <- data.frame(
#'   open_comp = sample(-4.5:+4.5, 3, replace = TRUE),
#'   selfenh_comp = sample(-4.5:+4.5, 3, replace = TRUE),
#'   cons_comp = sample(-4.5:+4.5, 3, replace = TRUE),
#'   selftrans_comp = sample(-4.5:+4.5, 3, replace = TRUE))
#' valueMap <- c(OPEN = "open_comp", SELFENH = "selfenh_comp",
#'               CONS = "cons_comp", SELFTRANS = "selftrans_comp")
#' persval::fourvalplot(df = df, valueMap = valueMap, instrument = "pvq40")
#' @export

fourvalplot <- function(df, valueMap, instrument, corrected = TRUE, standdev = TRUE, na.rm = TRUE) {
  specificValues <- c("OPEN", "SELFENH", "CONS", "SELFTRANS")

  if (!identical(sort(names(valueMap)), sort(specificValues))) {
    stop("Please provide exactly four value mappings, one for each higher-order personal value.")
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

  title(main = "Spider Chart: Four Higher Order Values")
}
