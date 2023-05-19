#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter arrange mutate group_by summarize recode left_join n
#' @importFrom tidyr drop_na
coreStocks <- function(){
  ## read in data synthesis
  root_dir <- "https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/"

  cores <- read_csv(paste0(root_dir, "CCRCN_cores.csv"), guess_max = 7000)
  depthseries <- read_csv(paste0(root_dir, "CCRCN_depthseries.csv"), guess_max = 60000)
  # bib <- read_csv(paste0(root_dir, "CCRCN_study_citations.csv"), guess_max = 600)

  target_intervals <- data.frame(horizon_min = 0, horizon_max = 100)

  # standardize depthseries to 1m depth
  synthesis_standardized <- depthseries %>%
    # mutate(across(where(cols %in% c("depth_min", "depth_max", "dry_bulk_density", "fraction_organic_matter", "fraction_carbon"))), as.numeric)
    merge(target_intervals) %>%
    # Keeps intervals between min and max horizon
    # If an interval crosses a horizon, it remains
    dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
    dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max) %>%
    # Calculate weights for each interval
    dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
                                           (horizon_max-depth_min),
                                           (depth_max-horizon_min), na.rm=T)) %>%
    dplyr::group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    dplyr::mutate(total_depth = sum(overlapping_depth),
                  weight = overlapping_depth / total_depth) %>%
    # Aggregate by horizon intervals
    dplyr::summarize(dry_bulk_density = sum(dry_bulk_density * weight),
                     fraction_organic_matter = sum(fraction_organic_matter * weight),
                     fraction_carbon = sum(fraction_carbon * weight))

  # calculate carbon stock
  synthesis_stocks <- synthesis_standardized %>%
    drop_na(dry_bulk_density, fraction_carbon) %>%
    # filter(core_id %in% unique(cr_cores$core_id)) %>%
    # select(where(~!all(is.na(.)))) %>%
    # select_if(function(x) {!all(is.na(x))}) %>%
    mutate(carbon_density = dry_bulk_density * fraction_carbon,
           # calculate c stock in each interval (gC m-2)
           # g/cm2 * 10000cm2/m2
           cstock_interval = carbon_density * (horizon_max-horizon_min) * 10000)  %>%
    # cases with fraction carbon but no DBD, drop NA cstock for now
    # drop_na(cstock_interval) %>%
    arrange(core_id, horizon_min) %>%
    group_by(study_id, site_id, core_id) %>%
    summarize(core_stock = sum(cstock_interval)) %>%
    # convert gC m-2 to MgC ha-1
    mutate(core_stock_MgHa = core_stock * (10^4/10^6))

  core_stocks <- left_join(cores, synthesis_stocks) %>%
    drop_na(core_stock) %>%
    mutate(country = ifelse(country == "Laos", "Vietnam", country)) %>%
    mutate(habitat = recode(habitat, "scrub shrub" = "scrub/shrub")) %>%
    group_by(country, habitat) %>%
    summarize(n = n(),
              stock_mean = mean(core_stock_MgHa, na.rm = T),
              stock_se = sd(core_stock_MgHa, na.rm = T)) %>%
    drop_na(country) %>% drop_na(habitat)

  return(core_stocks)
}


