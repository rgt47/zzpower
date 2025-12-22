# renv activation
source("renv/activate.R")

# Critical reproducibility options
options(
    stringsAsFactors = FALSE,
    digits = 7,
    OutDec = ".",
    na.action = "na.omit",
    contrasts = c("contr.treatment", "contr.poly")
)

# Auto-restore on startup if packages missing
.First <- function() {
    if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
        status <- tryCatch(renv::status(project = getwd()), error = function(e) NULL)
        if (!is.null(status) && !isTRUE(status$synchronized)) {
            message("Restoring packages from renv.lock...")
            renv::restore(prompt = FALSE)
        }
    }
}

# Auto-snapshot on exit
.Last <- function() {
    if (interactive() && file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
        tryCatch({
            renv::snapshot(prompt = FALSE)
            message("renv.lock updated")
        }, error = function(e) NULL)
    }
}
