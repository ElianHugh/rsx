assert_self_argument <- function(fn) {
    args <- names(formals(fn))
    length(args) > 0L && "self" %in% args
}

assert_no_argument <- function(fn) {
    length(names(formals(fn))) == 0L
}

assert_no_name_duplication <- function(x, y) {
    any(duplicated(c(x, y)))
}

assert_data_specification <- function(spec, values) {
    all(names(spec) %in% names(values))
}