##
#' Generic method to make a footnote indicating significant values.
#'
#' @param data Dataset with statistics.
#' @return \code{apa.signif} object; a list consisting of
#' \item{succes}{message in case of an error}
#' \item{signif}{\code{block_list {officer}} object}
#' @importFrom "officer" "block_list" "fpar" "ftext" "fp_text"
#' @export
#'
#' @examples
#'
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c("**", "", "***")
#' )
#'
#' # Use apa.descriptives function
#' apa.signif(data = example)
##
apa.signif = function(data=data.frame()) UseMethod("apa.signif")

##
#' Default method to make a footnote indicating significant values.
#'
#' @param data Dataset with statistics.
#' @return \code{apa.signif} object; a list consisting of
#' \item{succes}{message in case of an error}
#' \item{signif}{\code{block_list {officer}} object}
#' @importFrom "officer" "block_list" "fpar" "ftext" "fp_text"
#' @export
#'
#' @examples
#'
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c("**", "", "***")
#' )
#'
#' # Use apa.descriptives function
#' apa.signif(data = example)
##
apa.signif.default = function(data=data.frame()) {

  est = apaStyleSignificance(data)
  est$call = match.call()
  class(est) = "apa.signif"
  est

}

##
#' Define a print method
#'
#' @param  x A \code{apa.signif} object
#' @export
##
print.apa.signif = function(x, ...) {
  if(x$succes == TRUE) {
    cat("\n")
    cat("Succesfully generated significance footnote.")
    cat("\n\n")
  }
}

# The main function
apaStyleSignificance = function(data) {

  # Initialize function
  options(warn = 0)

  # Check if a valid data frame is supplied
  if ((!is.data.frame(data)) || (is.data.frame(data) && nrow(data) == 0)) {
    error = "Invalid data is supplied."
    warning(error)
    return(list(succes = error))
  }

  # Check the size of the dataset
  if (ncol(data) > 20 | nrow(data) > 100) {
    error = "The supplied data is too big to generate an APA formatted table."
    warning(error)
    return(list(succes = error))
  } else {

    # Convert factors to characters
    i = sapply(data, is.factor)
    data[i] = lapply(data[i], as.character)

    # Convert "+" symbol to unicode dagger symbol
    data[which(data == "+", arr.ind = TRUE)] = "\u2020"

    has.signif1 = apply(data, c(1, 2), function(x) any(x == "\u2020"))
    has.signif2 = apply(data, c(1, 2), function(x) any(x == "*"))
    has.signif3 = apply(data, c(1, 2), function(x) any(x == "**"))
    has.signif4 = apply(data, c(1, 2), function(x) any(x == "***"))
    
    style_normal = officer::fp_text(font.family = "Times", font.size = 12)
    style_italic = officer::fp_text(font.family = "Times", font.size = 12, italic=T)
    style_dagger = officer::fp_text(font.family = "Times", font.size = 12, vertical.align = "superscript")

    if (TRUE %in% has.signif1) {
      sig1 = officer::fpar( 
        officer::ftext("\u2020", prop = style_dagger), 
        officer::ftext("p", prop = style_italic), 
        officer::ftext(" < .10", prop = style_normal) 
      )
    } else {
      sig1 = officer::fpar(officer::ftext("", prop = style_normal))
    }

    if ((TRUE %in% has.signif2) || (TRUE %in% has.signif3 || TRUE %in% has.signif4)) {
      if(!"" %in% sig1[[1]]$value) {
        sig1 = officer::fpar(sig1, officer::ftext("; ", prop = style_normal))
      }
      sig2 = officer::fpar( 
        officer::ftext("*", prop = style_normal), 
        officer::ftext("p", prop = style_italic), 
        officer::ftext(" < .05", prop = style_normal) 
      )

    } else {
      sig2 = officer::fpar(officer::ftext("", prop = style_normal))
    }

    if ((TRUE %in% has.signif3) || (TRUE %in% has.signif4)) {
      if(!"" %in% sig2[[1]]$value) {
        sig2 = officer::fpar(sig2, officer::ftext("; ", prop = style_normal))
      }
      sig3 = officer::fpar( 
        officer::ftext("**", prop = style_normal), 
        officer::ftext("p", prop = style_italic), 
        officer::ftext(" < .01", prop = style_normal) 
      )
    } else {
      sig3 = officer::fpar(officer::ftext("", prop = style_normal))
    }

    if (TRUE %in% has.signif4) {
      if(!"" %in% sig3[[1]]$value) {
        sig3 = officer::fpar(sig3, officer::ftext("; ", prop = style_normal))
      }
      sig4 = officer::fpar( 
        officer::ftext("***", prop = style_normal), 
        officer::ftext("p", prop = style_italic), 
        officer::ftext(" < .001", prop = style_normal) 
      )
    } else {
      sig4 = officer::fpar(officer::ftext("", prop = style_normal))
    }
    apa.signif =  officer::fpar(sig1, sig2, sig3, sig4)
    apa.signif =  ""

  }

  return(list(succes = TRUE, signif = apa.signif))

}
