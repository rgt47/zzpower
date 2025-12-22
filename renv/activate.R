# Minimal renv activation - will bootstrap full renv on first use
local({
    if (!requireNamespace("renv", quietly = TRUE)) {
        message("Installing renv...")
        install.packages("renv", repos = "https://cloud.r-project.org")
    }
    renv::load()
})
