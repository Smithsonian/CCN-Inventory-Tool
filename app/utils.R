## App Utility Functions ####

## this utils folder might need to live in the app folder instead...tbd

## Standard Error ####

se <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

## function to round values in tables 

## function to format table headers?

## plot function

# x = input$chosen_habitat
globalStocks <- function(x, var){
  
  p <- x %>% 
    mutate(territory = recode(territory, "Democratic Republic of the Congo" = "Congo DRC")) %>% 
    filter(habitat == var) %>% 
    drop_na(soil_TierII_mean) %>%
    # filter(n_cores != 4) %>% # this is just to reduce the 
    mutate(territory = paste0(territory, ", n = ", n_cores)) %>% 
    # create base plot
    ggplot(aes(soil_TierII_mean, reorder(territory, soil_TierII_mean),
               xmin = soil_TierII_mean - soil_TierII_se, 
               xmax = soil_TierII_mean + soil_TierII_se)) +
    geom_point(size = 1) +
    geom_errorbar()
  
  # apply appropriate IPCC global value for selected habitat
  switch(var,
         "mangrove" = {
           p <- p + 
             # geom_rect(aes(xmin = 351, xmax = 424, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.005) +
             geom_vline(aes(xintercept = 386), col = "red") +
             geom_vline(aes(xintercept = 351), linetype = "dotted", col = "red") +
             geom_vline(aes(xintercept = 424), linetype = "dotted", col = "red") +
             xlab("1m Mangrove Soil Carbon Stocks (Mg/ha)") + ylab("") +
             # ggtitle("Mangrove Soil Carbon Stocks by Country", 
             #         subtitle = "Red line represents IPCC global value for the selected habitat") +
             theme_bw()
         },
         "marsh" = {
           p <- p + 
             # geom_rect(aes(xmin = 254, xmax = 297, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.005) +
             geom_vline(aes(xintercept = 255), col = "red") +
             geom_vline(aes(xintercept = 254), linetype = "dotted", col = "red") +
             geom_vline(aes(xintercept = 297), linetype = "dotted", col = "red") +
             xlab("1m Marsh Soil Carbon Stocks (Mg/ha)") + ylab("") +
             # ggtitle("Marsh Soil Carbon Stocks by Country", 
             #         subtitle = "Red line represents IPCC global value for the selected habitat") +
             theme_bw(base_size = 15)
         },
         "seagrass" = {
           p <- p + 
             # geom_rect(aes(xmin = 84, xmax = 139, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.005) +
             geom_vline(aes(xintercept = 108), col = "red") +
             geom_vline(aes(xintercept = 84), linetype = "dotted", col = "red") +
             geom_vline(aes(xintercept = 139), linetype = "dotted", col = "red") +
             xlab("1m Seagrass Soil Carbon Stocks (Mg/ha)") + ylab("") +
             # ggtitle("Soil carbon stocks by country. Red line represents IPCC global value for the selected habitat.", 
                     # subtitle = "Red line represents IPCC global value for the selected habitat") +
             theme_bw(base_size = 15)
         }
  )
  return(p)
}
