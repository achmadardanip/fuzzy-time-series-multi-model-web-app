my_packages = c("shiny", "ggplot2", "dplyr", "lubridate", "zoo", "DT", "shinythemes", "bslib", "rsconnect")
install_if_missing = function(p) {
    if (p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p)
    }
}
invisible(sapply(my_packages, install_if_missing))