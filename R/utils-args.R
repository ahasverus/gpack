#' Check `numeric` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not NA
#'   - Of type `numeric`
#'   - Of `length = 1`
#' 
#' @noRd

is_numeric <- function(num) {
  
  if (missing(num)) {
    stop("Argument '", deparse(substitute(num)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(num)) {
    stop("Argument '", deparse(substitute(num)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.numeric(num)) {
    stop("Argument '", deparse(substitute(num)), "' must be a numeric", 
         call. = FALSE)
  }
  
  if (length(num) != 1) {
    stop("Argument '", deparse(substitute(num)), "' must be of length 1", 
         call. = FALSE)
  }
  
  if (is.na(num)) {
    stop("Argument '", deparse(substitute(num)), "' cannot be NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check `numeric` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not NA
#'   - Of type `numeric`
#'   - Of `length > 0`
#' 
#' @noRd

is_numerics <- function(num) {
  
  if (missing(num)) {
    stop("Argument '", deparse(substitute(num)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(num)) {
    stop("Argument '", deparse(substitute(num)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.numeric(num)) {
    stop("Argument '", deparse(substitute(num)), "' must be a numeric", 
         call. = FALSE)
  }
  
  if (length(num) == 0) {
    stop("Argument '", deparse(substitute(num)), "' must be of length > 0", 
         call. = FALSE)
  }
  
  if (any(is.na(num))) {
    stop("Argument '", deparse(substitute(num)), "' cannot contain NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}


#' Check `character` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not NA
#'   - Of type `character`
#'   - Of `length = 1`
#' 
#' @noRd

is_character <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  if (length(str) != 1) {
    stop("Argument '", deparse(substitute(str)), "' must be of length 1", 
         call. = FALSE)
  }
  
  if (is.na(str)) {
    stop("Argument '", deparse(substitute(str)), "' cannot be NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check `character` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not empty
#'   - Not NA
#'   - Of type `character`
#'   - Of `length > 0`
#' 
#' @noRd

is_characters <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  if (length(str) == 0) {
    stop("Argument '", deparse(substitute(str)), "' must be of length > 0", 
         call. = FALSE)
  }
  
  if (any(is.na(str))) {
    stop("Argument '", deparse(substitute(str)), "' cannot contain NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check `logical` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not empty
#'   - Not NA
#'   - Of type `logical`
#'   - Of `length = 1`
#' 
#' @noRd

is_logical <- function(bool) {
  
  if (missing(bool)) {
    stop("Argument '", deparse(substitute(bool)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(bool)) {
    stop("Argument '", deparse(substitute(bool)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.logical(bool)) {
    stop("Argument '", deparse(substitute(bool)), "' must be a logical", 
         call. = FALSE)
  }
  
  if (length(bool) != 1) {
    stop("Argument '", deparse(substitute(bool)), "' must be of length 1", 
         call. = FALSE)
  }
  
  if (is.na(bool)) {
    stop("Argument '", deparse(substitute(bool)), "' cannot be NA", 
         call. = FALSE)
  }
  
  if (bool == "") {
    stop("Argument '", deparse(substitute(bool)), "' cannot be empty", 
         call. = FALSE)
  }
  
  invisible(NULL)
}
