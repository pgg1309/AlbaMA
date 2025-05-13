get_key <- function(key_name, rprofile_path = "../.Rprofile") {
    # Read all lines from .Rprofile
    lines <- readLines(rprofile_path)

    # Find the line with the key
    key_line <- grep(
        paste0("options\\(", key_name, "\\s*=\\s*"),
        lines,
        value = TRUE
    )

    # Extract the value using a regular expression
    if (length(key_line) > 0) {
        key_value <- sub(
            '.*options\\(fred.key\\s*=\\s*["\']([^"\']+)["\']\\).*',
            '\\1',
            key_line
        )
        return(key_value)
    } else {
        stop("FRED API key not found in .Rprofile")
    }
}
