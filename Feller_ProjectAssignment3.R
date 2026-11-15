## ESCI620 Nationwide tidal data ##

getwd()

setwd("C:/Users/alora/OneDrive/Desktop/School/ESCI620/Data")

install.packages("httr2")

## download_NOAA_data ##

library(httr2)

datasets <- c(
  ExtremeWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/.json?name=extremewaterlevels",
  PeakWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/.json?name=peakwaterlevels",
  SeaLevelTrends = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/sealvltrends.json",
  SeaLevelRiseProjections = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/slr_projections.json",
  SeaLevelRiseProjectionOffsets = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/slr_projectionOffsets.json",
  TopTenWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/toptenwaterlevels.json"
)

dir.create("noaa_data", showWarnings = FALSE)

for (dataset_name in names(datasets)) {
  url <- datasets[[dataset_name]]
  file_path <- file.path("noaa_data", paste0(dataset_name, ".json"))

  tryCatch({
    response <- request(url) |> req_perform()
    writeBin(resp_body_raw(response), file_path)
    message("Downloaded: ", file_path)
  }, error = function(e) {
    message("Could not download ", dataset_name, ": ", conditionMessage(e))
  })
}

## Error- httr2 not running correctly#

datasets <- c(
  ExtremeWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/.json?name=extremewaterlevels",
  PeakWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/.json?name=peakwaterlevels",
  SeaLevelTrends = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/sealvltrends.json",
  SeaLevelRiseProjections = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/slr_projections.json",
  SeaLevelRiseProjectionOffsets = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/slr_projectionOffsets.json",
  TopTenWaterLevels = "https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/toptenwaterlevels.json"
)

dir.create("noaa_data", showWarnings = FALSE)

for (dataset_name in names(datasets)) {
  file_path <- file.path("noaa_data", paste0(dataset_name, ".json"))

  tryCatch({
    status <- download.file(
      url = datasets[[dataset_name]],
      destfile = file_path,
      mode = "wb",
      quiet = FALSE
    )

    if (status == 0 && file.exists(file_path) && file.info(file_path)$size > 0) {
      message("Downloaded: ", file_path)
    } else {
      message("Download may have failed: ", dataset_name)
    }
  }, error = function(e) {
    message("Could not download ", dataset_name, ": ", conditionMessage(e))
  })
}

## explore json files ##
install.packages("jsonlite")

# Read all downloaded JSON files
files <- list.files(
  "noaa_data",
  pattern = "\\.json$",
  full.names = TRUE
)

if (!length(files)) stop("No JSON files found in the noaa_data folder.")

noaa_json <- setNames(
  lapply(files, jsonlite::fromJSON, simplifyVector = TRUE),
  basename(files)
)

# Inspect the structure of each file without printing all its contents
for (file_name in names(noaa_json)) {
  cat("\n---", file_name, "---\n")
  cat("Top-level fields:", paste(names(noaa_json[[file_name]]), collapse = ", "), "\n")
  str(noaa_json[[file_name]], max.level = 2, give.attr = FALSE)
}

# Find data frames nested anywhere within each JSON object
collect_tables <- function(x, path = "root") {
  if (is.data.frame(x)) {
    return(setNames(list(x), path))
  }

  result <- list()
  if (is.list(x) && length(x)) {
    element_names <- names(x)

    for (i in seq_along(x)) {
      label <- if (!is.null(element_names) && nzchar(element_names[i])) {
        element_names[i]
      } else {
        as.character(i)
      }

      result <- c(result, collect_tables(x[[i]], paste(path, label, sep = "$")))
    }
  }

  result
}

noaa_tables <- lapply(noaa_json, collect_tables)

# See which tables were found and their dimensions
for (file_name in names(noaa_tables)) {
  cat("\n---", file_name, "---\n")
  if (!length(noaa_tables[[file_name]])) {
    cat("No tabular data frame found; inspect this file with str().\n")
  } else {
    print(data.frame(
      table = names(noaa_tables[[file_name]]),
      rows = vapply(noaa_tables[[file_name]], nrow, integer(1)),
      columns = vapply(noaa_tables[[file_name]], ncol, integer(1))
    ))
  }
}

# View table
# Check for table names

names(noaa_tables[["SeaLevelTrends.json"]])

# view table
trends <- noaa_tables[["SeaLevelTrends.json"]][[1]]
View(trends)
head(trends)
names(trends)

#table quality check
assess_table <- function(x) {
  blank_count <- vapply(x, function(col) {
    if (is.character(col)) sum(trimws(col) == "", na.rm = TRUE) else 0L
  }, integer(1))

  result <- data.frame(
    column = names(x),
    type = vapply(x, function(col) paste(class(col), collapse = "/"), character(1)),
    missing_NA = vapply(x, function(col) sum(is.na(col)), integer(1)),
    blank_strings = blank_count,
    stringsAsFactors = FALSE
  )

  result$missing_percent <- round(
    100 * (result$missing_NA + result$blank_strings) / nrow(x),
    2
  )

  list(
    dimensions = dim(x),
    duplicate_rows = sum(duplicated(x)),
    missing_by_column = result,
    numeric_summary = summary(x[vapply(x, is.numeric, logical(1))])
  )
}

qc <- assess_table(trends)
qc$dimensions
qc$duplicate_rows
qc$missing_by_column
qc$numeric_summary

# check possible ranges
variables <- c(
  "trend",
  "seasonalAverage",
  "y2000_offset",
  "latitude",
  "longitude"
)

# Check that the columns exist
setdiff(variables, names(trends))

# Summarize each variable
range_summary <- do.call(rbind, lapply(variables, function(column) {
  x <- trends[[column]]
  x <- suppressWarnings(as.numeric(as.character(x)))
  valid <- x[is.finite(x)]

  if (length(valid) == 0) {
    return(data.frame(
      variable = column,
      n_valid = 0,
      n_missing = length(x),
      min = NA_real_,
      q1 = NA_real_,
      median = NA_real_,
      mean = NA_real_,
      q3 = NA_real_,
      max = NA_real_,
      n_distinct = 0
    ))
  }

  q <- quantile(valid, c(0.25, 0.5, 0.75), names = FALSE)

  data.frame(
    variable = column,
    n_valid = length(valid),
    n_missing = sum(!is.finite(x)),
    min = min(valid),
    q1 = q[1],
    median = q[2],
    mean = mean(valid),
    q3 = q[3],
    max = max(valid),
    n_distinct = length(unique(valid))
  )
}))

print(range_summary)

## DATA INSPECTION ##

# check variables
variables <- c(
  "trend",
  "seasonalAverage",
  "y2000_offset",
  "latitude",
  "longitude"
)

# stop early if a requested column is absent
missing_columns <- setdiff(c("stationId", variables), names(trends))
if (length(missing_columns) > 0) {
  stop("Missing column(s): ", paste(missing_columns, collapse = ", "))
}

# convert selected values to numeric for checks
numeric_data <- as.data.frame(lapply(trends[variables], function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}))

# Impossible pysical coordinates: latitude must be [-90,90],
# longitude must be [-180,180] in this datasets's apparent convention.
impossible_coordinates <- which(
  !is.finite(numeric_data$latitude) |
  !is.finite(numeric_data$longitude) |
  numeric_data$latitude < -90 | numeric_data$latitude > 90 |
  numeric_data$longitude < -180 | numeric_data$longitude > 180
)

if (length(impossible_coordinates)) {
  print(data.frame(
    row = impossible_coordinates,
    stationId = trends$stationId[impossible_coordinates],
    latitude = numeric_data$latitude[impossible_coordinates],
    longitude = numeric_data$longitude[impossible_coordinates]
  ))
} else {
  message("No physically impossible or non-finite coordinates found.")
}

# Statistical outliers: flag values beyond 1.5*IQR OR with
# an absolute modified MAD z-score greater than 3.5.
outlier_results <- list()

for (variable in variables) {
  x <- numeric_data[[variable]]
  valid <- is.finite(x)

  if (sum(valid) < 4 || length(unique(x[valid])) < 2) {
    next  # Not enough variation/data for a useful outlier check
  }

  q <- quantile(x[valid], c(0.25, 0.75), names = FALSE)
  iqr_value <- q[2] - q[1]
  lower_iqr <- q[1] - 1.5 * iqr_value
  upper_iqr <- q[2] + 1.5 * iqr_value
  iqr_flag <- valid & (x < lower_iqr | x > upper_iqr)

  median_value <- median(x[valid])
  mad_value <- mad(x[valid], constant = 1)
  if (mad_value > 0) {
    modified_z <- 0.6745 * (x - median_value) / mad_value
    mad_flag <- valid & abs(modified_z) > 3.5
  } else {
    modified_z <- rep(NA_real_, length(x))
    mad_flag <- rep(FALSE, length(x))
  }

  flagged <- which(iqr_flag | mad_flag)

  if (length(flagged)) {
    outlier_results[[variable]] <- data.frame(
      row = flagged,
      stationId = trends$stationId[flagged],
      variable = variable,
      value = x[flagged],
      iqr_flag = iqr_flag[flagged],
      mad_flag = mad_flag[flagged]
    )
  }
}

outliers <- if (length(outlier_results)) {
  do.call(rbind, outlier_results)
} else {
  data.frame(
    row = integer(),
    stationId = character(),
    variable = character(),
    value = numeric(),
    iqr_flag = logical(),
    mad_flag = logical()
  )
}

print(outliers)

# Flag records outside a rough contiguous-U.S. bonding box. 
# this is a geographic scope check not and impossible value check.
outside_conus <- which(
  numeric_data$latitude < 24 | numeric_data$latitude > 50 |
  numeric_data$longitude < -126 | numeric_data$longitude > -66
)

if (length(outside_conus)) {
  print(data.frame(
    row = outside_conus,
    stationId = trends$stationId[outside_conus],
    latitude = numeric_data$latitude[outside_conus],
    longitude = numeric_data$longitude[outside_conus]
  ))
}
 
# make a working copy; this code does not change 'trends'
dat <- trends

# 1. Exact duplicate rows
exact_dupes <- which(duplicated(dat) | duplicated(dat, fromLast = TRUE))

cat("Exact duplicate rows:", length(exact_dupes), "\n")
if (length(exact_dupes)) {
  print(dat[exact_dupes, , drop = FALSE])
}

# 2. Repeated station IDs (if stationId exists)
if ("stationId" %in% names(dat)) {
  station_id <- trimws(as.character(dat$stationId))
  repeated_ids <- unique(station_id[!is.na(station_id) &
                                      duplicated(station_id)])

  cat("\nStation IDs appearing more than once:",
      length(repeated_ids), "\n")

  if (length(repeated_ids)) {
    print(dat[station_id %in% repeated_ids, , drop = FALSE])
  }
}

# 3. Text fields: detect leading/trailing spaces and case/spacing variants
text_cols <- names(dat)[vapply(dat, function(x) {
  is.character(x) || is.factor(x)
}, logical(1))]

for (column in text_cols) {
  raw <- as.character(dat[[column]])
  present <- !is.na(raw)

  # Values with leading or trailing whitespace
  whitespace_rows <- which(present & raw != trimws(raw))
  if (length(whitespace_rows)) {
    cat("\nWhitespace issue in", column, ":\n")
    print(data.frame(
      row = whitespace_rows,
      original = raw[whitespace_rows],
      trimmed = trimws(raw[whitespace_rows])
    ))
  }

# Different raw spellings that become identical after trimming and
  # converting to lowercase (e.g. "mm/year" vs "MM/year").
  normalized <- tolower(trimws(raw))
  groups <- split(raw[present], normalized[present])
  variants <- groups[vapply(groups, function(x) {
    length(unique(x)) > 1
  }, logical(1))]

  if (length(variants)) {
    cat("\nCase/spacing variants in", column, ":\n")
    print(lapply(variants, unique))
  }
}

# 4. Unit fields: list distinct values and counts for review
unit_cols <- grep("unit", names(dat), ignore.case = TRUE, value = TRUE)

for (column in unit_cols) {
  cat("\nUnit values in", column, ":\n")
  print(sort(table(trimws(as.character(dat[[column]])),
                   useNA = "ifany"), decreasing = TRUE))
}

# 5. Date fields: inspect raw formats and flag values not matching
# common date patterns. This reports format patterns; it doesn't alter dates.
date_cols <- grep("date", names(dat), ignore.case = TRUE, value = TRUE)

date_pattern <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return("missing/blank")
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return("YYYY-MM-DD")
  if (grepl("^\\d{4}/\\d{2}/\\d{2}$", x)) return("YYYY/MM/DD")
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) return("MM/DD/YYYY or DD/MM/YYYY")
  if (grepl("^\\d{2}-\\d{2}-\\d{4}$", x)) return("DD-MM-YYYY or MM-DD-YYYY")
  return("other format")
}

for (column in date_cols) {
  raw <- as.character(dat[[column]])
  formats <- vapply(raw, date_pattern, character(1))

  cat("\nDate format counts in", column, ":\n")
  print(table(formats, useNA = "ifany"))

  unusual <- which(formats == "other format")
  if (length(unusual)) {
    cat("Values to inspect:\n")
    print(data.frame(row = unusual, value = raw[unusual]))
  }
}

# See affected rows in "startDate0" and "endDate)"
for (column in c("startDate0", "endDate0")) {
  x <- as.character(trends[[column]])
  blank <- !is.na(x) & trimws(x) == ""

  cat(
    "\n", column,
    "\nNA values:", sum(is.na(x)),
    "\nBlank/whitespace values:", sum(blank),
    "\n"
  )
}

# Show rows where either field is missing or blank
x_start <- as.character(trends$startDate0)
x_end   <- as.character(trends$endDate0)

missing_rows <- which(
  is.na(x_start) | trimws(x_start) == "" |
  is.na(x_end)   | trimws(x_end) == ""
)

trends[missing_rows, c("stationId", "startDate0", "endDate0")]

## Clean NOAA trends data
clean <- trends  # Preserve `trends` as the original data

count_missing <- function(x) {
  is_na <- is.na(x)
  is_blank <- rep(FALSE, length(x))

  if (is.character(x) || is.factor(x)) {
    value <- trimws(as.character(x))
    is_blank <- !is_na & value == ""
  }

  c(
    na_count = sum(is_na),
    blank_count = sum(is_blank),
    total_missing = sum(is_na | is_blank)
  )
}

missing_report <- as.data.frame(t(vapply(
  trends,
  count_missing,
  numeric(3)
)))

missing_report$column <- rownames(missing_report)
rownames(missing_report) <- NULL

missing_report <- missing_report[
  , c("column", "na_count", "blank_count", "total_missing")
]

print(missing_report)

# Flag missing optional date fields; do not impute them
for (column in c("startDate0", "endDate0")) {
  if (column %in% names(clean)) {
    value <- trimws(as.character(clean[[column]]))
    clean[[paste0(column, "_missing")]] <- is.na(value) | value == ""
  }
}

# Flag coordinates outside the rough bounding box for review only.
# This is NOT a definitive CONUS boundary, so do not exclude these rows yet.
clean$outside_rough_conus_box <-
  clean$latitude < 24 | clean$latitude > 50 |
  clean$longitude < -126 | clean$longitude > -66

review_geo <- clean[clean$outside_rough_conus_box, , drop = FALSE]
print(review_geo[, intersect(
  c("stationId", "stationName", "latitude", "longitude"),
  names(review_geo)
)])

# Confirm unit labels; keep the unit metadata with the measurements
for (column in grep("unit", names(clean), ignore.case = TRUE, value = TRUE)) {
  cat("\n", column, ":\n")
  print(table(clean[[column]], useNA = "ifany"))
}

# Reconfirm duplicate checks; no rows are removed
cat("\nExact duplicate rows:", sum(duplicated(clean)), "\n")
if ("stationId" %in% names(clean)) {
  cat("Repeated station IDs:",
      sum(duplicated(clean$stationId) & !is.na(clean$stationId)), "\n")
}

