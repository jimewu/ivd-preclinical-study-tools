replace_empty_below <- function(df, col) {
    content <- ""

    for (x in 1:nrow(df)) {
        if (nchar(df[[col]][x]) != 0) {
            content <- df[[col]][x]
        } else {
            df[[col]][x] <- content
        }
    }

    return(df)
}
