library(profvis)

# 
# profvis::profvis(f())

# Note the explicit call to runApp() here: this is important
# as otherwise the app won't actually run.
profvis::profvis(shiny::runApp("app"))
