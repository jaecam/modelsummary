#' sanity check
#'
#' @noRd
sanitize_fmt <- function(fmt, ...) {
    checkmate::assert(
        checkmate::check_numeric(fmt, len = 1, lower = 0),
        checkmate::check_class(fmt, "fmt_factory"),
        checkmate::check_function(fmt),
        checkmate::check_string(fmt, pattern = "%"),
        # checkmate::check_list(fmt, names = "unique"),
        checkmate::check_list(fmt, len = 1),
        checkmate::check_character(fmt),
        checkmate::check_null(fmt))

    if (inherits(fmt, "fmt_factory")) {
        out <- fmt
    } else if (isTRUE(checkmate::check_string(fmt))) {
        out <- fmt_sprintf(fmt)
    } else if (isTRUE(checkmate::check_numeric(fmt))) {
        out <- fmt_decimal(fmt)
    } else if (isTRUE(checkmate::check_function(fmt))) {
        out <- fmt_function(fmt)
    } else if (isTRUE(checkmate::check_null(fmt))) {
        out <- fmt_identity()
    } else if (isTRUE(checkmate::check_list(fmt, len = 1))) {
        elem <- fmt[[1]]
        out <- sanitize_fmt(elem)
    } else if (isTRUE(checkmate::check_character(fmt))) {
        num <- suppressWarnings(as.numeric(fmt))
        if (!is.na(num) && isTRUE(checkmate::check_numeric(num))) {
            out <- fmt_decimal(fmt)
        }
    }
    return(out)
}
