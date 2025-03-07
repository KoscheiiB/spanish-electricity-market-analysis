# ============================================================================
# OMIE MARKET CONCENTRATION ANALYSIS
# ============================================================================
# This script analyzes the concentration of the Spanish electricity market
# using data from OMIE (Operador del Mercado Ibérico de Energía) for the years
# 2020-2024, focusing on both purchase and production data for Spain.
#
# The analysis calculates various concentration indices:
# - Static: R(1), R(2), R(3), HHI, HTI, EI, EXP, CCI(1), CCI(2), CCI(3)
# - Dynamic: Instability Index between consecutive years
#
# ============================================================================

# ============================================================================
# 1. SETUP AND CONFIGURATION
# ============================================================================

# Clear the environment
rm(list = ls())

# Load required packages
required_packages <- c("logger", "readxl", "ggplot2", "here")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Configure logger
log_formatter(formatter_paste)
log_threshold(INFO)

# Create directories
# PROJECT_DIR <- getwd()
PROJECT_DIR <- here()
DATA_DIR <- file.path(PROJECT_DIR, "data")
RESULTS_DIR <- file.path(PROJECT_DIR, "results")

for (dir in c(DATA_DIR, RESULTS_DIR)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    log_info(paste("Created directory:", dir))
  }
}

# Analysis parameters
YEARS <- 2020:2024
REGION <- "spain"
TYPES <- c("purchase", "production")
FORMAT <- "XLS"

# Mapping for region and type codes
REGION_MAP <- list("spain" = 1)
TYPE_MAP <- list(
  "purchase" = "UADQ",
  "production" = "UPROD"
)

# ============================================================================
# 2. DATA DOWNLOAD AND IMPORT FUNCTIONS
# ============================================================================

#' Generate OMIE filename based on parameters
#'
#' @param year Numeric year
#' @param type Character data type
#' @param region Character region
#' @param format Character file format
#' @return Character filename
#'
generate_omie_filename <- function(year, type, region, format) {
  region_code <- REGION_MAP[[region]]
  type_code <- TYPE_MAP[[type]]

  sprintf(
    "ANU_CUOTA_EMP_PHFC_%s_%d_01_01_%d_31_12_%d.%s",
    type_code, region_code, year, year, format
  )
}

#' Download OMIE Energy Market Data
#'
#' @param year Numeric, year for which data is requested
#' @param type Character, "purchase" or "production"
#' @param region Character, "spain"
#' @param format Character, "XLS"
#' @param save_dir Character, directory to save the file
#'
#' @return Character, path to the downloaded file or NULL if download failed
#'
download_omie_data <- function(year, type, region, format, save_dir) {
  # Validate inputs
  if (!is.numeric(year) || year < 2020 || year > 2024) {
    log_error("Year must be a number between 2020 and 2024")
    return(NULL)
  }

  region_code <- REGION_MAP[[region]]
  if (is.null(region_code)) {
    log_error("Region must be 'spain'")
    return(NULL)
  }

  type_code <- TYPE_MAP[[type]]
  if (is.null(type_code)) {
    log_error("Type must be either 'purchase' or 'production'")
    return(NULL)
  }

  format <- toupper(format)
  format_url <- ifelse(format == "XLS", "XLV", "TXT")

  # Construct the URL
  base_url <- "https://www.omie.es/sites/default/files/dados"
  file_name <- generate_omie_filename(year, type, region, format)
  url <- sprintf("%s/AGNO_%d/ANUAL/%s/%s", base_url, year, format_url, file_name)

  # Create the output file path
  output_file <- file.path(save_dir, file_name)

  # Download the file
  tryCatch(
    {
      log_info(paste("Downloading", type, "data for", region, "(", year, ") in", format, "format"))
      utils::download.file(url, output_file, mode = "wb", quiet = TRUE)
      log_info(paste("File saved to:", output_file))
      return(output_file)
    },
    error = function(e) {
      log_error(paste("Download failed:", e$message))
      return(NULL)
    }
  )
}

#' Import OMIE data from XLS file
#'
#' @param file_path Path to XLS file
#' @return Data frame or NULL if import failed
#'
import_omie_xls <- function(file_path) {
  tryCatch(
    {
      # Skip the header rows (typically 4)
      df <- readxl::read_excel(file_path, skip = 4)

      # Ensure all columns are properly converted
      df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

      # Normalize column names
      names(df) <- gsub("\\s+", "_", tolower(names(df)))

      # Check for expected columns
      if (ncol(df) < 3) {
        log_error("Expected at least 3 columns in data")
        return(NULL)
      }

      # Standard column mapping
      # Usually:
      # - Column 1: company code
      # - Column 2: energy value
      # - Column 3: market share (%)

      # Rename columns to standardized names
      if (ncol(df) >= 3) {
        standard_names <- c("codigo", "energia", "cuota_energia")
        if (ncol(df) > 3) {
          standard_names <- c(standard_names, paste0("col", 4:ncol(df)))
        }
        names(df)[1:min(3, ncol(df))] <- standard_names[1:min(3, ncol(df))]
      }

      log_info(paste("Imported data with columns:", paste(names(df), collapse = ", ")))
      return(df)
    },
    error = function(e) {
      log_error(paste("Error importing XLS file", file_path, ":", e$message))
      return(NULL)
    }
  )
}

#' Import OMIE data for all years and types
#'
#' @param years Vector of years to import
#' @param region Region to import
#' @param types Vector of data types to import
#' @param format Preferred format for import
#' @param data_dir Directory where files are stored
#' @param auto_download Whether to download missing files
#'
#' @return A list containing data frames for each year and type
#'
import_all_omie_data <- function(years, region, types, format, data_dir, auto_download = TRUE) {
  # Initialize results list
  all_data <- list()

  # Loop through all combinations
  for (year in years) {
    for (type in types) {
      # Generate key for this combination
      key <- paste(year, region, type, sep = "_")

      # Check if file exists
      file_name <- generate_omie_filename(year, type, region, format)
      file_path <- file.path(data_dir, file_name)

      if (!file.exists(file_path) && auto_download) {
        log_info(paste("File not found for", key, "- Downloading..."))
        file_path <- download_omie_data(year, type, region, format, data_dir)
        if (is.null(file_path)) {
          log_error(paste("Unable to download data for", key))
          next
        }
      } else if (!file.exists(file_path)) {
        log_error(paste("File not found for", key, "and auto_download is disabled"))
        next
      }

      # Import the file
      df <- import_omie_xls(file_path)

      if (!is.null(df)) {
        # Add to results list with key
        all_data[[key]] <- df
        log_info(paste("Successfully imported data for", key))
      }
    }
  }

  return(all_data)
}

# ============================================================================
# 3. CONCENTRATION ANALYSIS FUNCTIONS
# ============================================================================

#' Calculate Market Concentration Indices
#'
#' @param df Data frame containing market share data
#' @param share_column Name of the column containing market shares (%)
#' @param name_column Name of the column containing company names
#'
#' @return A list containing calculated concentration indices
#'
calculate_concentration_indices <- function(df, share_column = "cuota_energia", name_column = "codigo") {
  # Check if the share column exists
  if (!share_column %in% names(df)) {
    log_error(paste("Share column", share_column, "not found in data frame"))
    return(NULL)
  }

  # Extract market shares and ensure they're numeric
  shares <- df[[share_column]]

  # Convert to numeric if needed
  if (!is.numeric(shares)) {
    shares <- as.numeric(gsub(",", ".", shares)) # Replace comma decimal separators
  }

  # Get company names/codes
  if (name_column %in% names(df)) {
    company_names <- df[[name_column]]
  } else {
    company_names <- paste("Company", 1:length(shares))
  }

  # Create a data frame with shares and names
  companies_df <- data.frame(
    name = company_names,
    share = shares,
    stringsAsFactors = FALSE
  )

  # Remove NA values
  companies_df <- companies_df[!is.na(companies_df$share), ]

  # Filter out rows with "Total" or "TOTAL" as company name
  # This prevents summary rows from being treated as companies
  companies_df <- companies_df[!grepl("^[Tt][Oo][Tt][Aa][Ll]", companies_df$name), ]

  # Check if we have enough data
  if (nrow(companies_df) == 0) {
    log_error("No valid market share data found")
    return(NULL)
  }

  # Convert percentage to proportion
  companies_df$share <- companies_df$share / 100

  # Ensure shares sum to 1 (sometimes there are rounding errors)
  companies_df$share <- companies_df$share / sum(companies_df$share)

  # Sort shares in descending order
  companies_df <- companies_df[order(companies_df$share, decreasing = TRUE), ]

  # Extract sorted shares and names
  shares <- companies_df$share
  sorted_names <- companies_df$name

  # Number of firms
  n <- length(shares)

  # Calculate indices

  # R(k) Concentration ratios for k=1,2,3
  r_values <- list()
  top_companies <- list()
  for (k in 1:3) {
    if (k <= n) {
      r_values[[paste0("R", k)]] <- sum(shares[1:k])
      top_companies[[paste0("R", k)]] <- sorted_names[1:k]
    } else {
      r_values[[paste0("R", k)]] <- sum(shares)
      top_companies[[paste0("R", k)]] <- sorted_names
    }
  }

  # Herfindahl-Hirschman Index (HHI)
  hhi <- sum(shares^2)

  # Hall-Tideman Index (HTI)
  hti <- 1 / (2 * sum(seq_along(shares) * shares) - 1)

  # Entropy Index (EI)
  # Avoid log(0) by filtering out zeros
  nonzero_shares <- shares[shares > 0]
  ei <- -sum(nonzero_shares * log(nonzero_shares))

  # Exponential Index (EXP)
  exp_idx <- prod(shares^shares)

  # Comprehensive Concentration Index (CCI) for k=1,2,3
  cci_values <- list()
  for (k in 1:3) {
    if (k < n) {
      cci_values[[paste0("CCI", k)]] <- sum(shares[1:k]) +
        sum(shares[(k + 1):n]^2 * (1 + (1 - shares[(k + 1):n])))
    } else {
      cci_values[[paste0("CCI", k)]] <- sum(shares)
    }
  }

  # Return all indices
  return(list(
    R = r_values,
    top_companies = top_companies,
    HHI = hhi,
    HTI = hti,
    EI = ei,
    EXP = exp_idx,
    CCI = cci_values,
    n_firms = n
  ))
}

#' Calculate Instability Index Between Two Periods
#'
#' @param df1 Data frame for period 1
#' @param df2 Data frame for period 2
#' @param id_column Column identifying firms in both periods
#' @param share_column Column containing market shares
#'
#' @return Instability index value
#'
calculate_instability_index <- function(df1, df2, id_column = "codigo", share_column = "cuota_energia") {
  # Check if the required columns exist in both dataframes
  if (!id_column %in% names(df1) || !id_column %in% names(df2) ||
    !share_column %in% names(df1) || !share_column %in% names(df2)) {
    log_error("Required columns missing for instability index calculation")
    return(NA)
  }

  # Ensure shares are numeric
  df1[[share_column]] <- as.numeric(gsub(",", ".", as.character(df1[[share_column]])))
  df2[[share_column]] <- as.numeric(gsub(",", ".", as.character(df2[[share_column]])))

  # Convert to proportions
  df1[[share_column]] <- df1[[share_column]] / 100
  df2[[share_column]] <- df2[[share_column]] / 100

  # Create a unified set of all firm IDs
  all_ids <- unique(c(df1[[id_column]], df2[[id_column]]))

  # Create data frames with all firms, filling in zeros for missing values
  shares1 <- data.frame(
    id = all_ids,
    share1 = 0
  )
  shares1$share1[match(df1[[id_column]], all_ids)] <- df1[[share_column]]

  shares2 <- data.frame(
    id = all_ids,
    share2 = 0
  )
  shares2$share2[match(df2[[id_column]], all_ids)] <- df2[[share_column]]

  # Merge the two periods
  merged <- merge(shares1, shares2, by = "id")

  # Calculate instability index
  instability <- 0.5 * sum(abs(merged$share1 - merged$share2))

  return(instability)
}

#' Run Market Concentration Analysis on OMIE Data
#'
#' @param data_list List of data frames with OMIE data
#'
#' @return A list containing analysis results
#'
analyze_market_concentration <- function(data_list) {
  if (length(data_list) == 0) {
    log_error("Analysis cannot continue - no data provided")
    return(NULL)
  }

  # Results container
  results <- list()

  # Calculate static concentration indices for each dataset
  for (key in names(data_list)) {
    df <- data_list[[key]]

    # Calculate indices
    indices <- calculate_concentration_indices(df)

    if (!is.null(indices)) {
      # Store results
      results[[key]] <- indices

      # Log results
      log_info(paste("Calculated concentration indices for", key))
      log_info(paste(
        "HHI:", round(indices$HHI, 4),
        "R1:", round(indices$R$R1, 4),
        "N firms:", indices$n_firms
      ))
    }
  }

  # Calculate dynamic (instability) indices between consecutive years
  years <- YEARS

  for (i in 1:(length(years) - 1)) {
    year1 <- years[i]
    year2 <- years[i + 1]

    for (type in TYPES) {
      # Get data for both years
      key1 <- paste(year1, REGION, type, sep = "_")
      key2 <- paste(year2, REGION, type, sep = "_")

      if (key1 %in% names(data_list) && key2 %in% names(data_list)) {
        # Calculate instability index
        instability <- calculate_instability_index(data_list[[key1]], data_list[[key2]])

        # Store result
        instab_key <- paste(year1, year2, REGION, type, "instability", sep = "_")
        results[[instab_key]] <- instability

        log_info(paste(
          "Instability index between", year1, "-", year2,
          "for", REGION, type, ":", round(instability, 4)
        ))
      }
    }
  }

  return(results)
}

# ============================================================================
# 4. VISUALIZATION FUNCTIONS
# ============================================================================

#' Create and save concentration index plots
#'
#' @param results Results list from market concentration analysis
#' @param save_dir Directory to save plots
#'
#' @return NULL (plots are saved to files)
#'
create_concentration_plots <- function(results, save_dir) {
  # Extract data for plots
  years <- YEARS

  # Create a better x-axis for plots
  x_labels <- as.character(years)

  # ================ Static Concentration Indices ================

  # HHI Plot data
  purchase_hhi <- sapply(years, function(y) {
    key <- paste(y, REGION, "purchase", sep = "_")
    if (key %in% names(results)) results[[key]]$HHI else NA
  })

  production_hhi <- sapply(years, function(y) {
    key <- paste(y, REGION, "production", sep = "_")
    if (key %in% names(results)) results[[key]]$HHI else NA
  })

  # R1, R2, R3 Plot data
  purchase_company_names <- list()
  production_company_names <- list()

  for (r_idx in 1:3) {
    r_key <- paste0("R", r_idx)

    # Set up variables to store data
    assign(paste0("purchase_r", r_idx), sapply(years, function(y) {
      key <- paste(y, REGION, "purchase", sep = "_")
      if (key %in% names(results) && r_key %in% names(results[[key]]$R)) {
        purchase_company_names[[as.character(y)]] <- results[[key]]$top_companies[[r_key]]
        return(results[[key]]$R[[r_key]])
      } else {
        return(NA)
      }
    }))

    assign(paste0("production_r", r_idx), sapply(years, function(y) {
      key <- paste(y, REGION, "production", sep = "_")
      if (key %in% names(results) && r_key %in% names(results[[key]]$R)) {
        production_company_names[[as.character(y)]] <- results[[key]]$top_companies[[r_key]]
        return(results[[key]]$R[[r_key]])
      } else {
        return(NA)
      }
    }))
  }

  # HTI Plot data
  purchase_hti <- sapply(years, function(y) {
    key <- paste(y, REGION, "purchase", sep = "_")
    if (key %in% names(results)) results[[key]]$HTI else NA
  })

  production_hti <- sapply(years, function(y) {
    key <- paste(y, REGION, "production", sep = "_")
    if (key %in% names(results)) results[[key]]$HTI else NA
  })

  # EI Plot data
  purchase_ei <- sapply(years, function(y) {
    key <- paste(y, REGION, "purchase", sep = "_")
    if (key %in% names(results)) results[[key]]$EI else NA
  })

  production_ei <- sapply(years, function(y) {
    key <- paste(y, REGION, "production", sep = "_")
    if (key %in% names(results)) results[[key]]$EI else NA
  })

  # EXP Plot data
  purchase_exp <- sapply(years, function(y) {
    key <- paste(y, REGION, "purchase", sep = "_")
    if (key %in% names(results)) results[[key]]$EXP else NA
  })

  production_exp <- sapply(years, function(y) {
    key <- paste(y, REGION, "production", sep = "_")
    if (key %in% names(results)) results[[key]]$EXP else NA
  })

  # CCI Plot data
  for (cci_idx in 1:3) {
    cci_key <- paste0("CCI", cci_idx)

    assign(paste0("purchase_cci", cci_idx), sapply(years, function(y) {
      key <- paste(y, REGION, "purchase", sep = "_")
      if (key %in% names(results) && cci_key %in% names(results[[key]]$CCI)) {
        return(results[[key]]$CCI[[cci_key]])
      } else {
        return(NA)
      }
    }))

    assign(paste0("production_cci", cci_idx), sapply(years, function(y) {
      key <- paste(y, REGION, "production", sep = "_")
      if (key %in% names(results) && cci_key %in% names(results[[key]]$CCI)) {
        return(results[[key]]$CCI[[cci_key]])
      } else {
        return(NA)
      }
    }))
  }

  # Function to create a standard line plot for concentration indices
  create_line_plot <- function(purchase_data, production_data, title, y_label, filename) {
    png(file.path(save_dir, filename), width = 1000, height = 600)
    par(mar = c(5, 4, 4, 8) + 0.1) # Increase right margin for notes

    plot(years, purchase_data,
      type = "l", col = "blue", lwd = 2,
      main = paste(title, "for Spanish Electricity Market (2020-2024)"),
      xlab = "Year", ylab = y_label,
      ylim = c(
        min(c(purchase_data, production_data), na.rm = TRUE) * 0.95,
        max(c(purchase_data, production_data), na.rm = TRUE) * 1.05
      ),
      xaxt = "n" # No x-axis labels initially
    )

    # Add custom x-axis labels
    axis(1, at = years, labels = x_labels)

    lines(years, production_data, col = "red", lty = 2, lwd = 2)
    points(years, purchase_data, col = "blue", pch = 19)
    points(years, production_data, col = "red", pch = 17)
    grid()

    legend("topright",
      legend = c("Purchase", "Production"),
      col = c("blue", "red"), lty = c(1, 2), pch = c(19, 17), lwd = c(2, 2)
    )

    dev.off()
  }

  # Function to create R(k) plots without company names text area
  create_r_plot <- function(k, purchase_data, production_data) {
    r_label <- paste0("R(", k, ")")
    filename <- paste0("r", k, "_plot.png")

    png(file.path(save_dir, filename), width = 1000, height = 600)

    plot(years, purchase_data,
      type = "l", col = "blue", lwd = 2,
      main = paste(r_label, "Index - Market Share of Top", k, "Firms (2020-2024)"),
      xlab = "Year", ylab = r_label,
      ylim = c(
        min(c(purchase_data, production_data), na.rm = TRUE) * 0.95,
        max(c(purchase_data, production_data), na.rm = TRUE) * 1.05
      ),
      xaxt = "n"
    )

    # Add custom x-axis labels
    axis(1, at = years, labels = x_labels)

    lines(years, production_data, col = "red", lty = 2, lwd = 2)
    points(years, purchase_data, col = "blue", pch = 19)
    points(years, production_data, col = "red", pch = 17)
    grid()

    legend("topright",
      legend = c("Purchase", "Production"),
      col = c("blue", "red"), lty = c(1, 2), pch = c(19, 17), lwd = c(2, 2)
    )

    dev.off()
  }

  # Create all the static concentration index plots
  create_line_plot(purchase_hhi, production_hhi, "HHI Index", "HHI", "hhi_plot.png")
  create_line_plot(purchase_hti, production_hti, "HTI Index", "HTI", "hti_plot.png")
  create_line_plot(purchase_ei, production_ei, "Entropy Index", "EI", "ei_plot.png")
  create_line_plot(purchase_exp, production_exp, "Exponential Index", "EXP", "exp_plot.png")

  # Create R(k) plots with company names
  create_r_plot(1, purchase_r1, production_r1)
  create_r_plot(2, purchase_r2, production_r2)
  create_r_plot(3, purchase_r3, production_r3)

  # Create CCI plots
  create_line_plot(purchase_cci1, production_cci1, "CCI(1) Index", "CCI(1)", "cci1_plot.png")
  create_line_plot(purchase_cci2, production_cci2, "CCI(2) Index", "CCI(2)", "cci2_plot.png")
  create_line_plot(purchase_cci3, production_cci3, "CCI(3) Index", "CCI(3)", "cci3_plot.png")

  # ================ Dynamic Concentration Index (Instability) ================

  # Instability Plot data
  year_pairs <- c()
  for (i in 1:(length(years) - 1)) {
    year_pairs <- c(year_pairs, paste(years[i], years[i + 1], sep = "-"))
  }

  purchase_instability <- sapply(1:(length(years) - 1), function(i) {
    key <- paste(years[i], years[i + 1], REGION, "purchase", "instability", sep = "_")
    if (key %in% names(results)) results[[key]] else NA
  })

  production_instability <- sapply(1:(length(years) - 1), function(i) {
    key <- paste(years[i], years[i + 1], REGION, "production", "instability", sep = "_")
    if (key %in% names(results)) results[[key]] else NA
  })

  # Create Instability Plot with better formatting
  png(file.path(save_dir, "instability_plot.png"), width = 1000, height = 600)

  # Calculate the y-axis limits with some padding
  y_max <- max(c(purchase_instability, production_instability), na.rm = TRUE) * 1.1

  barplot_data <- rbind(purchase_instability, production_instability)
  bp <- barplot(barplot_data,
    beside = TRUE, col = c("blue", "red"),
    names.arg = year_pairs,
    main = "Market Instability Between Years",
    ylab = "Instability Index",
    ylim = c(0, y_max),
    cex.names = 0.9
  )

  # Add values on top of bars
  text(
    x = bp, y = barplot_data + y_max / 50,
    labels = round(barplot_data, 3),
    cex = 0.8, pos = 3
  )

  # Add gridlines
  abline(h = seq(0, y_max, by = 0.05), col = "lightgray", lty = 3)

  legend("topright",
    legend = c("Purchase", "Production"),
    fill = c("blue", "red")
  )

  # Add interpretation note
  mtext("Note: Higher values indicate greater market instability (0 = complete stability, 1 = maximum instability)",
    side = 1, line = 4, cex = 0.8
  )

  dev.off()

  log_info("Created and saved all concentration plots")
}

#' Create a summary data frame from results
#'
#' @param results Results list from market concentration analysis
#'
#' @return A list containing two data frames: one for static indices and one for dynamic indices
#'
create_results_summary <- function(results) {
  # Create two separate data frames for static and dynamic indices

  # 1. Static indices dataframe
  static_keys <- grep("instability", names(results), invert = TRUE, value = TRUE)
  if (length(static_keys) > 0) {
    # Initialize vectors
    years <- c()
    types <- c()
    hhi <- c()
    hti <- c()
    ei <- c()
    exp_idx <- c()
    r1 <- c()
    r2 <- c()
    r3 <- c()
    cci1 <- c()
    cci2 <- c()
    cci3 <- c()
    n_firms <- c()

    # For company identifiers
    top1_companies <- c()
    top2_companies <- c()
    top3_companies <- c()

    # Extract metrics from static results
    for (key in static_keys) {
      parts <- strsplit(key, "_")[[1]]
      years <- c(years, as.numeric(parts[1]))
      types <- c(types, parts[3])

      res <- results[[key]]

      # Add all static concentration indices
      hhi <- c(hhi, res$HHI)
      hti <- c(hti, res$HTI)
      ei <- c(ei, res$EI)
      exp_idx <- c(exp_idx, res$EXP)

      # R values
      r1 <- c(r1, res$R$R1)
      r2 <- c(r2, ifelse("R2" %in% names(res$R), res$R$R2, NA))
      r3 <- c(r3, ifelse("R3" %in% names(res$R), res$R$R3, NA))

      # CCI values
      cci1 <- c(cci1, ifelse("CCI1" %in% names(res$CCI), res$CCI$CCI1, NA))
      cci2 <- c(cci2, ifelse("CCI2" %in% names(res$CCI), res$CCI$CCI2, NA))
      cci3 <- c(cci3, ifelse("CCI3" %in% names(res$CCI), res$CCI$CCI3, NA))

      n_firms <- c(n_firms, res$n_firms)

      # Add company identifiers
      if ("top_companies" %in% names(res)) {
        # R1 - Top company
        if ("R1" %in% names(res$top_companies) && length(res$top_companies$R1) > 0) {
          top1_companies <- c(top1_companies, res$top_companies$R1[1])
        } else {
          top1_companies <- c(top1_companies, NA)
        }

        # R2 - Top 2 companies (get the 2nd one)
        if ("R2" %in% names(res$top_companies) && length(res$top_companies$R2) >= 2) {
          top2_companies <- c(top2_companies, paste(res$top_companies$R2[1:2], collapse = ", "))
        } else {
          top2_companies <- c(top2_companies, NA)
        }

        # R3 - Top 3 companies (get the 3rd one)
        if ("R3" %in% names(res$top_companies) && length(res$top_companies$R3) >= 3) {
          top3_companies <- c(top3_companies, paste(res$top_companies$R3[1:3], collapse = ", "))
        } else {
          top3_companies <- c(top3_companies, NA)
        }
      } else {
        top1_companies <- c(top1_companies, NA)
        top2_companies <- c(top2_companies, NA)
        top3_companies <- c(top3_companies, NA)
      }
    }

    # Create data frame for static indices
    static_df <- data.frame(
      Year = years,
      Type = types,
      HHI = hhi,
      HTI = hti,
      EI = ei,
      EXP = exp_idx,
      R1 = r1,
      R2 = r2,
      R3 = r3,
      CCI1 = cci1,
      CCI2 = cci2,
      CCI3 = cci3,
      N_Firms = n_firms,
      Top1_Company = top1_companies,
      Top2_Companies = top2_companies,
      Top3_Companies = top3_companies,
      stringsAsFactors = FALSE
    )

    # Sort by year and type
    static_df <- static_df[order(static_df$Year, static_df$Type), ]
  } else {
    static_df <- data.frame()
  }

  # 2. Dynamic indices dataframe
  dynamic_keys <- grep("instability", names(results), value = TRUE)
  if (length(dynamic_keys) > 0) {
    # Initialize vectors
    year_pairs <- c()
    types <- c()
    instability <- c()

    # Extract metrics from dynamic results
    for (key in dynamic_keys) {
      parts <- strsplit(key, "_")[[1]]
      year_pairs <- c(year_pairs, paste(parts[1], "-", parts[2], sep = ""))
      types <- c(types, parts[4])
      instability <- c(instability, results[[key]])
    }

    # Create data frame for dynamic indices
    dynamic_df <- data.frame(
      Year_Pair = year_pairs,
      Type = types,
      Instability = instability,
      stringsAsFactors = FALSE
    )

    # Sort by year pair and type
    dynamic_df <- dynamic_df[order(dynamic_df$Year_Pair, dynamic_df$Type), ]
  } else {
    dynamic_df <- data.frame()
  }

  # Return both data frames as a list
  return(list(
    static = static_df,
    dynamic = dynamic_df
  ))
}

# ============================================================================
# 5. MAIN ANALYSIS PIPELINE
# ============================================================================

#' Run the complete analysis
#'
#' @param auto_download Whether to download missing files
#' @param save_results Whether to save results to files
#'
#' @return Analysis results
#'
run_analysis <- function(auto_download = TRUE, save_results = TRUE) {
  # Step 1: Import all data
  log_info("Step 1: Importing OMIE data...")
  data_list <- import_all_omie_data(YEARS, REGION, TYPES, FORMAT, DATA_DIR, auto_download)

  if (length(data_list) == 0) {
    log_error("Analysis cannot continue - no data imported")
    return(NULL)
  }

  log_info(paste("Successfully imported", length(data_list), "datasets"))

  # Step 2: Run market concentration analysis
  log_info("Step 2: Running market concentration analysis...")
  results <- analyze_market_concentration(data_list)

  if (is.null(results) || length(results) == 0) {
    log_error("Analysis failed - no results generated")
    return(NULL)
  }

  log_info(paste("Analysis complete -", length(results), "results generated"))

  # Step 3: Create visualizations
  if (save_results) {
    log_info("Step 3: Creating visualizations...")
    create_concentration_plots(results, RESULTS_DIR)

    # Save results to RData file
    save(results, file = file.path(RESULTS_DIR, "market_concentration_results.RData"))
    log_info("Saved results to RData file")

    # Create and save summary CSVs
    summary_data <- create_results_summary(results)

    # Save static indices summary
    if (nrow(summary_data$static) > 0) {
      write.csv(summary_data$static, file.path(RESULTS_DIR, "static_concentration_summary.csv"), row.names = FALSE)
      log_info("Saved static concentration indices summary to CSV file")
    }

    # Save dynamic indices summary
    if (nrow(summary_data$dynamic) > 0) {
      write.csv(summary_data$dynamic, file.path(RESULTS_DIR, "dynamic_instability_summary.csv"), row.names = FALSE)
      log_info("Saved dynamic instability indices summary to CSV file")
    }
  }

  return(results)
}

# ============================================================================
# 6. RUN THE ANALYSIS
# ============================================================================

# Set up a log file
log_file <- file.path(RESULTS_DIR, paste0("omie_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_appender(appender_file(log_file))
log_info(paste("Starting OMIE market analysis. Log file:", log_file))

# Run the analysis
results <- run_analysis(auto_download = TRUE, save_results = TRUE)

if (!is.null(results)) {
  log_info("Analysis completed successfully!")
} else {
  log_error("Analysis failed.")
}
