# ============================================================================
# Factors: Earnings Persistence, Dividend Yield, Idiosyncratic Skewness
# ============================================================================

#################################################################################
### 1. Libraries ------------------------------------------------------------ ###
#################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

required_pkgs <- c(
  "tidyverse", "purrr", "dplyr", "stats", "moments", "knitr", "tidyr", "forcats", "ggplot2", "Rblpapi", 
  "lubridate", "quantmod", "zoo"
)

# Loading and installing
installed_pkgs <- rownames(installed.packages())
missing_pkgs <- setdiff(required_pkgs, installed_pkgs)

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

invisible(lapply(required_pkgs, function(pkg) {
  library(pkg, character.only = TRUE)
}))

message("All required packages are now loaded:")
message(paste(required_pkgs, collapse = ", "))


#################################################################################################
### 2. Loading data, preparation ------------------------------------------------------------ ###
#################################################################################################


msci_tickers <- c(
#  "MXIN Index",   # India
#  "MXCN Index",   # China
#  "MXEG Index",   # Egypt
#  "MXZA Index",   # South Africa
  "MXKR Index",   # South Korea
  "TAMSCI Index", # Taiwan
#  "MXTH Index",   # Thailand
  "MXCZ Index",   # Czech Republic
  "MXPL Index",   # Poland
#  "MXAR Index",   # Argentina
#  "MXCL Index",   # Chile
  "MXGR Index",   # Greece (legacy code)
  "MXTR Index",   # Turkey
#  "MXCA Index",   # Canada
#  "MXUS Index",   # USA
  "MXFR Index",   # France
  "MXDE Index",   # Germany
  "MXIL Index",   # Israel
  "MXIT Index",   # Italy
  "MXSE Index",   # Sweden
  "MXES Index",   # Spain
  "MXCH Index",   # Switzerland
  "MXGB Index",   # United Kingdom
  "MXAU Index",   # Australia
  "MXHK Index",   # Hong Kong
  "MXJP Index",   # Japan
  "MXSG Index",   # Singapore
  
  "MXAU Index",   # Australia
  "MXNZ Index",   # New Zealand
  "MXEU Index",   # Europe
  "MXSA Index",   # Saudi Arabia
  "MXJO Index",   # Jordan
  "MXKW Index",   # Kuwait
  "MXQA Index",   # Qatar
  "MXBH Index",   # Bahrain
  "MXAE Index",   # UAE
  "MXOM Index",   # Oman
  "MXRO Index",   # Romania
  "MXNL Index",   # Netherlands
  "MXBE Index",   # Belgium
  "MXPT Index",   # Portugal
  "MXHU Index",   # Hungary
  "MXAT Index",   # Austria
  "MXIE Index",   # Ireland
  "MXFI Index",   # Finland
  "MXNO Index",   # Norway
  "MXDK Index",   # Denmark
  "MXRS Index",   # Serbia
  "MXBAH Index",  # Bosnia
  "MXEST Index"  # Estonia 
  
)
countries <- c(
#  "India",
#  "China",
#  "Egypt",
#  "South Africa",
  "South Korea",
  "Taiwan",
#  "Thailand",
  "Czech Republic",
  "Poland",
#  "Argentina",
#  "Chile",
  "Greece",
  "Turkey",
#  "Canada",
#  "USA",
  "France",
  "Germany",
  "Israel",
  "Italy",
  "Sweden",
  "Spain",
  "Switzerland",
  "United Kingdom",
  "Australia",
  "Hong Kong",
  "Japan",
  "Singapore",
  "Australia",
  "New Zealand",
  "Europe",
  "Saudi Arabia",
  "Jordan",
  "Kuwait","Qatar",
  "Bahrain",
  "UAE",
  "Oman",
  "Romania",
  "Netherlands",
  "Belgium",
  "Portugal",
  "Hungary",
  "Austria",
  "Ireland",
  "Finland",
  "Norway",
  "Denmark",
  "Serbia",
  "Bosnia",
  "Estonia"
)



bdh_list <- bdh(
  securities = msci_tickers,
  fields     = c("PX_LAST", "IN001","IN004" ),
  start.date = Sys.Date() - years(40),
  end.date   = Sys.Date(),
  options = c("periodicitySelection" = "MONTHLY")
)


bdh_list <- map(bdh_list, ~ {
  if (is.data.frame(.x)) {
    rename(.x,
           EPS = IN001,
           PE  = IN004
    )
  } else .x
})


###############################################################################
### 3. Metrics ------------------------------------------------------------ ###
###############################################################################

# required return in decimal
r_required = 0.1

bdh_list <- lapply(bdh_list, function(df) {
  if (is.data.frame(df) && "PE" %in% names(df)) {
    ## Earnings Yield
    df$EarningsYield <- 1 / df$PE
    # Z-score
    mu_ey = mean(df$EarningsYield, na.rm = TRUE)
    sd_ey = sd(df$EarningsYield, na.rm = TRUE)
    df$EY_z_score = (df$EarningsYield - mu_ey) / sd_ey
    
    ## EPS Z-score
    mu_eps = mean(df$EPS, na.rm = TRUE)
    sd_eps = sd(df$EPS, na.rm = TRUE)
    df$EPS_z_score = (df$EPS - mu_eps) / sd_eps
    
    ## P/E Z-score
    # P/E z-score relative to its own history
    mu = mean(df$PE, na.rm = TRUE)
    sd = sd(df$PE, na.rm = TRUE)
    df$PE_z_score = (df$PE - mu) / sd
  }
  df
})

head(bdh_list[[1]], 5)

##########################################################################################
### 3. Summary statistics ------------------------------------------------------------ ###
##########################################################################################

# metrics to summarise
metrics <- c(
  "PX_LAST",           
  "EPS",               
  "PE",                
  "EarningsYield",     
  "EY_z_score",
  "EPS_z_score",
  "PE_z_score"
)

# build summary stats
summary_stats <- imap_dfr(bdh_list, ~ {
  df      <- .x
  country <- .y
  
  df %>%
    summarise(across(
      all_of(metrics),
      list(
        mean     = ~ mean(.   , na.rm = TRUE),
        median   = ~ median(. , na.rm = TRUE),
        sd       = ~ sd(.     , na.rm = TRUE),
        min      = ~ min(.    , na.rm = TRUE),
        max      = ~ max(.    , na.rm = TRUE),
        skewness = ~ skewness(. , na.rm = TRUE),
        kurtosis = ~ kurtosis(. , na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    mutate(Country = country) %>%
    select(Country, everything())
})

# view
print(summary_stats)

# render nicely
kable(summary_stats, digits = 3)

# write an excel file
write.csv(summary_stats, "valuation_summary_stats.csv", row.names = FALSE)


#############################################################################
### 4. Plots ------------------------------------------------------------ ###
#############################################################################


#### -------------------------------------------------
#### Box-and-Whisker and Violin plots of Z-scores ####
# 1a) Gather all z‐scores into one long data.frame
z_long <- imap_dfr(bdh_list, ~ {
  df <- .x
  if (!is.data.frame(df)) return(NULL)
  df %>%
    select(EPS_z_score, PE_z_score, EY_z_score) %>%
    pivot_longer(everything(),
                 names_to  = "Metric",
                 values_to = "Z") %>%
    mutate(Country = .y)
})

# 1b) Boxplot (swap geom_boxplot() for geom_violin(trim=FALSE) to get violins)
ggplot(z_long, aes(x = Country, y = Z, fill = Country)) +
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  coord_flip() +
  labs(
    title = "Distribution of Valuation & Earnings Z-Scores by Country",
    x     = NULL,
    y     = "Z-Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(z_long, aes(x = Country, y = Z, fill = Country)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  coord_flip() +
  labs(
    title = "Distribution of Valuation & Earnings Z-Scores by Country",
    x     = NULL,
    y     = "Z-Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#### -------------------------------------------------
#### Heatmap of mean Z-scores ####

# 2a) Compute mean Z per country × metric
heatmap_df <- z_long %>%
  group_by(Country, Metric) %>%
  summarise(MeanZ = mean(Z, na.rm = TRUE), .groups = "drop")

# 2b) Plot heatmap
ggplot(heatmap_df, aes(x = Metric, y = fct_reorder(Country, MeanZ, .fun = median), fill = MeanZ)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low      = "steelblue",
    mid      = "white",
    high     = "indianred",
    midpoint = 0
  ) +
  labs(
    title = "Average Z-Scores of Valuation & Earnings Metrics",
    x     = NULL,
    y     = NULL,
    fill  = "Mean Z"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### -------------------------------------------------
#### Value-gap bar chart (delta = CAPE - P/E) ####


################################################################################
### 5. Valuation Gap Analysis (Current vs 10Y Median) by Region ------------ ###
################################################################################

# 0. Create Mapping Vector (Ticker -> Country Name)
ticker_map <- setNames(countries, msci_tickers)

# 1. Define Region Mapping
# We create a function to assign regions based on the country name
get_region <- function(country_name) {
  case_when(
    # --- Advanced Asia ---
    country_name %in% c("South Korea", "Taiwan", "Japan", "Hong Kong", 
                        "Singapore", "Australia", "New Zealand") ~ "Advanced Asia",
    
    # --- Middle East ---
    country_name %in% c("Saudi Arabia", "Jordan", "Kuwait", "Qatar", "Bahrain", 
                        "UAE", "Oman", "Israel") ~ "Middle East",
    
    # --- Europe (Defaulting the rest to Europe as per your list) ---
    # Includes: UK, France, Germany, Italy, Spain, Switz, Nordics, Eastern Europe, Turkey, etc.
    TRUE ~ "Europe" 
  )
}

# 2. Calculation Function
valuation_gap_df <- imap_dfr(bdh_list, ~ {
  df      <- .x
  ticker  <- .y
  
  country_name <- ifelse(!is.null(ticker_map[[ticker]]), ticker_map[[ticker]], ticker)
  
  if (!is.data.frame(df) || !"PE" %in% names(df)) return(NULL)
  
  start_date_10y <- Sys.Date() - years(10)
  df_10y <- df %>% filter(date >= start_date_10y)
  
  current_pe <- tail(na.omit(df$PE), 1)
  if (length(current_pe) == 0) return(NULL)
  
  med_pe_10y <- median(df_10y$PE, na.rm = TRUE)
  
  tibble(
    Country    = country_name,
    Current_PE = current_pe,
    Med_PE_10Y = med_pe_10y,
    Premium    = (current_pe / med_pe_10y) - 1
  )
})

# 3. Preparation for Plotting
valuation_gap_df <- valuation_gap_df %>%
  mutate(
    # Assign Region
    Region = get_region(Country),
    
    # Reorder Country by Current_PE (this sorts them within the Facets later)
    Country = fct_reorder(Country, Current_PE),
    
    # Determine Status
    Valuation_Status = ifelse(Current_PE < Med_PE_10Y, "Undervalued", "Overvalued")
  )

# 4. Dumbbell Plot with Facets
ggplot(valuation_gap_df) +
  # Draw the connecting line
  geom_segment(
    aes(y = Country, yend = Country, x = Med_PE_10Y, xend = Current_PE, color = Valuation_Status),
    size = 1.5
  ) +
  # Point for 10Y Median
  geom_point(
    aes(x = Med_PE_10Y, y = Country, color = "10Y Median"), 
    size = 3.5
  ) +
  # Point for Current PE
  geom_point(
    aes(x = Current_PE, y = Country, color = "Current TTM"), 
    size = 3.5
  ) +
  # --- REGIONAL FACETTING ---
  # scales="free_y": allows different countries in each block
  # space="free_y": creates block height proportional to number of countries (no empty white space)
  facet_grid(Region ~ ., scales = "free_y", space = "free_y") +
  
  # Colors
  scale_color_manual(
    values = c(
      "10Y Median"       = "grey",      
      "Current TTM"      = "#377eb8",   
      "Undervalued"      = "#009E73",   
      "Overvalued"       = "#e41a1c"    
    ),
    breaks = c("Current TTM", "10Y Median", "Undervalued", "Overvalued"),
    name = NULL 
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(0, 0, 1, 1), 
        shape    = c(16, 16, NA, NA)
      )
    )
  ) +
  labs(
    title = "Valuation Gap: Current P/E vs. 10-Year Historical Median",
    subtitle = "Comparing TTM P/E against the 10-year median",
    x = "P/E Ratio",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.box.margin = margin(t = 10),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9, face = "bold"),
    
    # Add a border around the facets for clarity
    panel.border = element_rect(color = "grey90", fill = NA),
    # Style the strip (Region Headers)
    strip.text = element_text(face = "bold", size = 11, color = "white"),
    strip.background = element_rect(fill = "#5D6D7E", color = NA)
  )
