#' Multiple variables to unique date variable
#' `r lifecycle::badge("experimental")`
#'
#' This function converts multiple date-related variables (year, quarter, month, day)
#' into a single date variable within a data frame. It handles various date formats
#' and frequencies, offering flexibility in how dates are represented in the input data.
#'
#' @param tbl A data frame or tibble.
#' @param year The position or name of the year variable.
#' @param quarter The position or name of the quarter variable.
#' @param month The position or name of the month variable.
#' @param day The position or name of the day variable.
#' @param date A variable name or position containing a date-like format.
#' @param drop_vars Logical, indicating whether to drop the original date component variables. Default is TRUE.
#' @param clean_names Logical, indicating whether to clean variable names using `janitor::make_clean_names()`. Default is FALSE.
#' @param date_format The date format of the `date` variable (if provided). Default is "%d-%m-%y".
#' @param origin The base date for converting numeric dates. Default is "1900-01-01".
#' @param .round Specifies how to round the resulting date: "end", "middle", or "start" of the period. Default is "end".
#'
#' @return A tibble with the computed date variable.
#' @export
#'
#' @examples
#' \dontrun{
#'   tbl <- data.frame(
#'     year = rep("2021", 12),
#'     month = month.name,
#'     day = sample(1:3, 12, TRUE),
#'     value = sample(100:1000, 12, TRUE)
#'   )
#'
#'   dts_vars_to_date(tbl, year = 1, month = 2, day = 3)
#'
#'   # With quarters
#'   tbl <- data.frame(
#'     year = rep("2021", 12),
#'     quarter = sample(c("Q1", "Q2", "Q3", "Q4"), 12, TRUE),
#'     value = sample(100:1000, 12, TRUE)
#'   )
#'
#'   dts_vars_to_date(tbl, year = 1, quarter = 2)
#' }
dts_vars_to_date <- function(
    tbl, year = NULL, quarter = NULL, month = NULL,
    day = NULL, date = NULL, drop_vars = TRUE,
    clean_names = FALSE, date_format = "%d-%m-%y",
    origin = "1900-01-01", .round = c("end", "middle", "start")) {
  # . <- NULL
  date_end <- NULL
  .round_step <- 1
  if (length(.round) > 1) {
    .round <- .round[[1]]
  }
  if (clean_names) {
    names(tbl) <- janitor::make_clean_names(names(tbl))
  }

  if (length(names(tbl)) != length(unique(names(tbl)))) {
    stop("Not unique names. Make clean names or set `clean_names = TRUE`.")
  }

  if (!is.null(day)) {
    day <- get_pos(day, names(tbl))
    dayn <- names(tbl)[day]

    if (sum(is.na(tbl[[dayn]])) > 0) {
      stop(paste0("Do not pass NAs in the variable '", dayn, "'"))
    }
  } else {
    day <- FALSE
  }

  if (!is.null(month)) {
    month <- get_pos(month, names(tbl))
    monthn <- names(tbl)[month]

    if (sum(is.na(tbl[[monthn]])) > 0) {
      stop(paste0("Do not pass NAs in the variable '", monthn, "'"))
    }

    tbl <- numeric_month(tbl, monthn)
  } else {
    month <- FALSE
  }

  if (!is.null(quarter)) {
    quarter <- get_pos(quarter, names(tbl))
    quartern <- names(tbl)[quarter]

    if (sum(is.na(tbl[[quartern]])) > 0) {
      stop(paste0("Do not pass NAs in the variable '", quartern, "'"))
    }

    tbl <- numeric_quarter(tbl, quartern)
    tbl[[quartern]] <- tbl[[quartern]] * 3 - 2
  } else {
    quarter <- FALSE
  }

  if (!is.null(year)) {
    year <- get_pos(year, names(tbl))
    yearn <- names(tbl)[year]

    if (sum(is.na(tbl[[yearn]])) > 0) {
      stop(paste0("Do not pass NAs in the variable '", yearn, "'"))
    }
  } else {
    year <- FALSE
  }

  if (!is.null(date)) {
    date <- get_pos(date, names(tbl))
    daten <- names(tbl)[date]

    if (sum(is.na(tbl[[daten]])) > 0) {
      stop(paste0("Do not pass NAs in the variable '", daten, "'"))
    }
  } else {
    date <- FALSE
  }

  if (day) {
    if (any(is.null(year), !year)) {
      stop("You need to indicate the variable 'year'")
    }
    if (any(is.null(month), !month)) {
      stop("You need to indicate the variable 'month'")
    }

    tbl$date <- as.Date(
      paste0(tbl[[yearn]], "-", tbl[[monthn]], "-", tbl[[dayn]])
    )
    tbl <- tbl %>%
      dplyr::relocate(date)

    if (drop_vars) {
      tbl[, yearn] <- NULL
      tbl[, monthn] <- NULL
      tbl[, dayn] <- NULL
    }

    year <- FALSE
    month <- FALSE
    day <- FALSE
    .round_step <- NULL
  }

  if (month) {
    if (!year) {
      stop("You need to indicate the variable 'year'")
    }
    tbl$date <- paste0(tbl[[yearn]], "-", tbl[[monthn]], "-", "01")
    tbl$date <- as.Date(tbl$date)
    # tbl$date <- lubridate::ceiling_date(tbl$date, unit = "month")
    # tbl$date <- lubridate::add_with_rollback(tbl$date, lubridate::days(-1))

    if (drop_vars) {
      tbl[, yearn] <- NULL
      tbl[, monthn] <- NULL
    }
    year <- FALSE
    month <- FALSE
    quarter <- FALSE
  }

  if (quarter) {
    if (!year) {
      stop("You need to indicate the variable 'year'")
    }

    tbl <- dts_vars_to_date(
      tbl,
      year = year,
      month = quarter,
      drop_vars = drop_vars,
      .round = NULL
    )
    tbl[[quartern]] <- lubridate::quarter(tbl$date)

    if (drop_vars) {
      tbl[, yearn] <- NULL
      tbl[, quartern] <- NULL
    }
    year <- FALSE
    quarter <- FALSE
    .round_step <- 3
  }

  if (date) {
    nombres <- names(tbl)
    nombres[nombres == daten] <- "date"
    names(tbl) <- nombres
    tbl <- clean_date(tbl, date_format, origin)
    nombres[nombres == "date"] <- daten
    names(tbl) <- nombres
    .round_step <- NULL
  }

  if (!is.null(.round)) {
    if (!is.null(.round_step)) {
      tbl <- tbl %>%
        dplyr::mutate(
          date_end = lubridate::add_with_rollback(date, months(.round_step)),
          date_end = lubridate::add_with_rollback(date_end, lubridate::days(-1))
        )
      if (.round == "end") {
        tbl$date <- tbl$date_end
      } else if (.round == "middle") {
        tbl <- tbl %>%
          dplyr::mutate(date = date + (date_end - date) / 2)
        lubridate::day(tbl$date) <- 15
      }
      tbl$date_end <- NULL
    }
  }

  tbl %>%
    dplyr::relocate(date)
}



get_pos <- function(arg, names) {
  if (is.character(arg)) {
    arg <- match(arg, names)
  }
  arg
}


# Convert a month variable to a numeric format
#
# @param tbl a data.frame with the month variable
# @param month the name or position of the month variable
#
# @return tbl a new data.frame with the month variable in numeric format
#
#
# @examples
# \dontrun{
# numeric_month(tbl, "month")
# }
numeric_month <- function(tbl, month) {
  if (is.character(utils::type.convert(tbl[[month]], as.is = TRUE))) {
    tbl[[month]] <- stringr::str_remove_all(tbl[[month]], stringr::regex("[^a-zA-Z]"))
    tbl[[month]] <- stringr::str_trim(tbl[[month]])
    tbl[[month]] <- stringr::str_to_title(stringr::str_sub(tbl[[month]], 1, 3))
    tbl[[month]][tbl[[month]] == "Ene"] <- "Jan"
    tbl[[month]][tbl[[month]] == "Abr"] <- "Apr"
    tbl[[month]][tbl[[month]] == "Ago"] <- "Aug"
    tbl[[month]][tbl[[month]] == "Dic"] <- "Dec"

    tbl[[month]] <- match(tbl[[month]], month.abb)
  }
  tbl
}


# Convert a quarter variable to a numeric format
#
# @param tbl a data.frame with the quarter variable
# @param quarter the name or position of the quarter variable
#
# @return tbl a new data.frame with the quarter variable in numeric format
#
#
# @examples
# \dontrun{
# numeric_quarter(tbl, "quarter")
# }
numeric_quarter <- function(tbl, quarter) {
  . <- NULL
  qq <- c(
    "1" = 1, "2" = 2, "3" = 3, "4" = 4,
    "1" = "01", "2" = "02", "3" = "03", "4" = "04",
    "1" = "I", "2" = "II", "3" = "III", "4" = "IV",
    "1" = "Q1", "2" = "Q2", "3" = "Q3", "4" = "Q4",
    "1" = "T1", "2" = "T2", "3" = "T3", "4" = "T4",
    "1" = "em", "2" = "aj", "3" = "js", "4" = "od",
    "2" = "ej", "3" = "es", "4" = "ed",
    "1" = "jm", "2" = "aj", "3" = "js", "4" = "od",
    "2" = "jj", "4" = "jd"
  )

  qncharmean <- mean(nchar(tbl[[quarter]]))

  if (qncharmean > 2) {
    tidyr::separate(tbl, col = quarter, into = c("V1", "V2")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_sub(.x, 1, 1))) %>%
      {
        paste0(.[["V1"]], .[["V2"]])
      } %>%
      tolower() -> tbl[, quarter]
  }
  tbl[[quarter]] <- as.numeric(names(qq)[match(tbl[[quarter]], qq)])

  tbl
}


# Refactor
clean_date <- function(tbl, date_format, origin) {
  .eviews <- tbl |>
    dplyr::pull(date) |>
    stringr::str_detect(stringr::regex("[0-9]{2,4}[M|Q][0-9]{2}")) |>
    mean()
  if (.eviews >= 0.95) {
    .qtr <- tbl |>
      dplyr::pull(date) |>
      stringr::str_detect(stringr::regex("[0-9]{2,4}[Q][0-9]{2}")) |>
      mean()
    if (.qtr >= 0.95) {
      tbl <- tbl |>
        tidyr::separate(date, c("year", "quarter"), "Q", convert = TRUE) |>
        dts_vars_to_date(year = "year", quarter = "quarter")
    } else {
      tbl <- tbl |>
        tidyr::separate(date, c("year", "month"), "M", convert = TRUE) |>
        dts_vars_to_date(year = "year", month = "month")
      print(tbl)
    }
    return(tbl)
  }
  tbl <- tbl %>%
    dplyr::mutate(
      date = dplyr::case_when(
        is.na(as.numeric(date)) ~ stringr::str_remove_all(stringr::str_remove_all(date, "\\."), "\\*"),
        TRUE ~ date
      ),
      date = dplyr::case_when(
        stringr::str_detect(tolower(date), "enero") ~ stringr::str_replace(tolower(date), "enero", "jan"),
        stringr::str_detect(tolower(date), "ene") ~ stringr::str_replace(tolower(date), "ene", "jan"),
        stringr::str_detect(tolower(date), "abril") ~ stringr::str_replace(tolower(date), "abril", "apr"),
        stringr::str_detect(tolower(date), "abr") ~ stringr::str_replace(tolower(date), "abr", "apr"),
        stringr::str_detect(tolower(date), "agosto") ~ stringr::str_replace(tolower(date), "agosto", "aug"),
        stringr::str_detect(tolower(date), "ago") ~ stringr::str_replace(tolower(date), "ago", "aug"),
        stringr::str_detect(tolower(date), "diciembre") ~ stringr::str_replace(tolower(date), "diciembre", "dec"),
        stringr::str_detect(tolower(date), "dic") ~ stringr::str_replace(tolower(date), "dic", "dec"),
        TRUE ~ date
      ),
      date = dplyr::case_when(
        stringr::str_detect(
          date,
          stringr::regex("[a-z]{3}-")
        ) ~ stringr::str_replace(
          date,
          stringr::regex("[a-z]{3}"),
          paste0(
            match(
              stringr::str_to_title(stringr::str_extract(date, "[a-z]{3}")),
              month.abb
            )
          )
        ),
        TRUE ~ date
      )
    )
  if (date_format %in% c("%d-%m-%y", "%m-%y")) {
    tbl <- tbl %>%
      dplyr::mutate(
        date = dplyr::case_when(
          stringr::str_count(date, "-") == 1 & stringr::str_starts(date, "[0-9]") ~ paste0(1, "-", date),
          TRUE ~ date
        ),
        date = dplyr::case_when(
          stringr::str_count(date, "-") == 2 ~ as.character(difftime(as.Date(date, format = date_format), as.Date(origin), units = "days")),
          TRUE ~ date
        )
      )
  }
  tbl %>%
    dplyr::mutate(date = as.Date(as.numeric(date), origin = origin))
}
