check_self_argument <- function(fn) {
    args <- names(formals(fn))
    length(args) > 0L && "self" %in% args
}

check_ns_argument <- function(fn) {
    args <- names(formals(fn))
    length(args) > 0L && "ns" %nin% args
}

check_function_return <- function(fn, expected_class) {
    res <- fn()
    inherits(res, expected_class)
}

check_no_argument <- function(fn) {
    length(names(formals(fn))) == 0L
}

check_name_duplication <- function(x, y) {
    any(duplicated(c(x, y)))
}

check_data_specification <- function(spec, values) {
    all(names(spec) %in% names(values))
}