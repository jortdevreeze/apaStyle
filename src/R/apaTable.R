##
#' Generic method to generate an APA style table for MS Word
#'
#' @param  data Dataset with statistics.
#' @param  level1.header The column names for the first header in the table.
#' @param  level1.colspan (optional) The colspan for the first header column.
#' @param  level2.header (optional) The column names for the second header in the table.
#' @param  number (optional) The table number in the document.
#' @param  title (optional) Name of the table.
#' @param  filename (optional) Specify the filename (including valid '\code{.docx}' extension).
#' @param  note (optional) Add a footnote to the bottom of the table.
#' @param  landscape (optional) Set (\code{TRUE}) if the table should be generated in landscape mode.
#' @param  save (optional) Set (\code{FALSE}) if the table should not be saved in a document.
#' @details
#'
#' This method can generate tables with two headers. If two headers are required, it is necesary to
#' specifify the colspan for the upper level (\code{level1.colspan}). If only one header is required
#' only the header items need to be specified for \code{level1.header}, and \code{level1.colspan} and
#' \code{level2.header} do not need be specified.
#'
#' This method allows users to specify a column in which either the level of significance (header:
#' \code{"*"}), or a subscript (header: \code{"_"}) is given. For example, when there is a column
#' with a F-value and there shouldn't be an additional column with the corresponding p-values, it
#' is possible to specify an additional column with significant values (i.e., +p < .10; *p < .05;
#' **p < .01; ***p < .001) which will be merged as one column in the final table.
#'
#' Often it is necesary to provide a table with the means from different groups or conditions. Using
#' the subscript header (\code{"_"}) it is possible to supply a column with subscripts which indicates
#' which means on a row significantly differ from each other.
#'
#' @return \code{apa.table} object; a list consisting of
#' \item{succes}{message in case of an error}
#' \item{save}{flag which indicates whether the document is saved}
#' \item{table}{\code{flextable {flextable}} object}
#' @importFrom "officer" "read_docx" "body_end_section_landscape" "body_add_fpar" "fpar" "ftext" "fp_text" "fp_border"
#' @importFrom "flextable" "set_flextable_defaults" "flextable" "body_add_flextable" "delete_part" "border_remove" "width" "align" "padding" "italic" "hline" "compose" "as_paragraph" "as_sub" "add_header_row"
#' @importFrom "utils" "head"
#' @export
#'
#' @examples
#'
#' # Use apa.table function with a minimum of parameters
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c(1.23, 1.06, 1.12)
#' )
#'
#' # Create table
#' apa.table(data = example, level1.header = c("Variable", "M", "SD"))
#'
#' # Create a table with two headers
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c(1.23, 1.06, 1.12),
#'   c(8.22, 25.12, 30.27),
#'   c("+", "**", "***")
#' )
#'
#' # Run method and preview table
#' apa.table(
#'   data = example,
#'   level1.header = c("", "Descriptives", "Inferential"),
#'   level1.colspan = c(1, 2, 2),
#'   level2.header = c("Variable", "M", "SD", "t-value", "*")
#' )$table
##
apa.table = function(data=data.frame(), level1.header=NULL, level1.colspan=NULL, level2.header=NULL, number="XX", title="APA Table", filename="APA Table.docx", note=NULL, landscape=FALSE, save=TRUE) UseMethod("apa.table")

##
#' Default method to generate an APA style table for MS Word
#'
#' @param  data Dataset with statistics.
#' @param  level1.header The column names for the first header in the table.
#' @param  level1.colspan (optional) The colspan for the first header column.
#' @param  level2.header (optional) The column names for the second header in the table.
#' @param  number (optional) The table number in the document.
#' @param  title (optional) Name of the table.
#' @param  filename (optional) Specify the filename (including valid '\code{.docx}' extension).
#' @param  note (optional) Add a footnote to the bottom of the table.
#' @param  landscape (optional) Set (\code{TRUE}) if the table should be generated in landscape mode.
#' @param  save (optional) Set (\code{FALSE}) if the table should not be saved in a document.
#' @details
#'
#' This method can generate tables with two headers. If two headers are required, it is necesary to
#' specifify the colspan for the upper level (\code{level1.colspan}). If only one header is required
#' only the header items need to be specified for \code{level1.header}, and \code{level1.colspan} and
#' \code{level2.header} do not need be specified.
#'
#' This method allows users to specify a column in which either the level of significance (header:
#' \code{"*"}), or a subscript (header: \code{"_"}) is given. For example, when there is a column
#' with a F-value and there shouldn't be an additional column with the corresponding p-values, it
#' is possible to specify an additional column with significant values (i.e., +p < .10; *p < .05;
#' **p < .01; ***p < .001) which will be merged as one column in the final table.
#'
#' Often it is necesary to provide a table with the means from different groups or conditions. Using
#' the subscript header (\code{"_"}) it is possible to supply a column with subscripts which indicates
#' which means on a row significantly differ from each other.
#'
#' @return \code{apa.table} object; a list consisting of
#' \item{succes}{message in case of an error}
#' \item{save}{flag which indicates whether the document is saved}
#' \item{table}{\code{flextable {flextable}} object}
#' @importFrom "officer" "read_docx" "body_end_section_landscape" "body_add_fpar" "fpar" "ftext" "fp_text" "fp_border"
#' @importFrom "flextable" "set_flextable_defaults" "flextable" "body_add_flextable" "delete_part" "border_remove" "width" "align" "padding" "italic" "hline" "compose" "as_paragraph" "as_sub" "add_header_row"
#' @importFrom "utils" "head"
#' @export
#'
#' @examples
#'
#' # Use apa.table function with a minimum of parameters
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c(1.23, 1.06, 1.12)
#' )
#'
#' # Create table
#' apa.table(data = example, level1.header = c("Variable", "M", "SD"))
#'
#' # Create a table with two headers
#' # Specify statistics
#' example <- data.frame(
#'   c("Column 1", "Column 2", "Column 3"),
#'   c(3.45, 5.21, 2.64),
#'   c(1.23, 1.06, 1.12),
#'   c(8.22, 25.12, 30.27),
#'   c("+", "**", "***")
#' )
#'
#' # Run method and preview table
#' apa.table(
#'   data = example,
#'   level1.header = c("", "Descriptives", "Inferential"),
#'   level1.colspan = c(1, 2, 2),
#'   level2.header = c("Variable", "M", "SD", "t-value", "*")
#' )$table
##
apa.table.default = function(data=data.frame(), level1.header=NULL, level1.colspan=NULL, level2.header=NULL, number="XX", title="APA Table", filename="APA Table.docx", note=NULL, landscape=FALSE, save=TRUE) {

  est = apaStyleTable(data, level1.header, level1.colspan, level2.header, number, title, filename, note, landscape, save)
  est$call = match.call()
  class(est) = "apa.table"
  est

}

##
#' Define a print method
#'
#' @param  x A \code{apa.table} object
#' @export
##
print.apa.table = function(x, ...) {
  if(x$succes == TRUE) {
    cat("\n")
    if (x$save == TRUE) {
      cat("Word document succesfully generated in: ")
      cat(getwd())
    } else {
      cat("Succesfully generated the APA table")
    }
    cat("\n\n")
  }
}

# The main function
apaStyleTable = function(data, level1.header, level1.colspan, level2.header, number, title, filename, note, landscape, save) {

  # Initialize function
  options(warn = 0)

  # Define variables
  level2 = FALSE
  apa.signif = NULL
  apa.italics = c("B", "d", "df", "F", "M", "n", "N", "p", "r", "R^2", "SD", "SE", "t", "z" )
  apa.tableName = ifelse(is.numeric(number), paste("Table", number, sep = "", collapse = ""), "Table XX")

  # Check if a valid data frame is supplied
  if ((!is.data.frame(data)) || (is.data.frame(data) && nrow(data) == 0)) {
    error = "Invalid data is supplied."
    warning(error)
    return(list(succes = error))
  }

  # Check if valid headers are supplied
  if(!is.character(level1.header)) {
    error = "No valid headers are specified."
    warning(error)
    return(list(succes = error))
  }

  # Check if the save argument is a valid type
  if(!is.logical(save)) {
    error = "The save argument is not of logical type."
    warning(error)
    return(list(succes = error))
  } else {

    if (save == TRUE) {

      # Check if a valid filename is supplied
      if((!is.character(filename)) || (!grepl(".docx", filename))) {
        error = "The supplied filename is not valid. Please specify a valid 'docx' file."
        warning(error)
        return(list(succes = error))
      } else {
        apa.filename = filename
      }

      # Check if the landscape argument is a valid type
      if(!is.logical(landscape)) {
        error = "The landscape argument is not of logical type."
        warning(error)
        return(list(succes = error))
      }

    }
  }

  # Check the size of the dataset
  if (ncol(data) > 22 | nrow(data) > 100) {
    error = cat("The supplied data is too big to generate an APA formatted table (ncol: ", ncol(data), ", nrow: ", nrow(data))
    warning(error)
    return(list(succes = error))
  } else {

    # Convert factors to characters
    i = sapply(data, is.factor)
    data[i] = lapply(data[i], as.character)

    # Prepare table headers

    # Check if level 2 headers are supplied
    if(is.character(level2.header)) {
      if((is.null(level1.colspan)) || (sum(level1.colspan) != length(level2.header))) {
        error = "The level 1 colspan doesn't match the number of level 2 headers."
        warning(error)
        return(list(succes = error))
      } else {

        level2 = TRUE

        # Insert empty columns between (contiguous nonmissing) level1 headers

        # Find indices of nonblank elements, then indices after which to insert blank columns
        nonblanks = grep("\\w|\\S", level1.header, perl = TRUE)
        blankafter = nonblanks[c(diff(nonblanks) == 1, FALSE)]

        # Indices after which we will have a blank column are repeated
        indices = sort(c(seq_along(level1.header), blankafter))

        # All the indices that will not be an inserted blank column
        firstindex = !duplicated(indices)

        tmp.header1 = tmp.colspan = tmp.header2 = NULL

        # Insert the original header values into the these indices
        tmp.header1[firstindex] = level1.header

        # Insert a space for a blank column in all the other places
        tmp.header1[!firstindex] = " "

        # Make new level1 colspan: insert a 1 in between the input elements in a similar way as before

        tmp.colspan[firstindex] = level1.colspan
        tmp.colspan[!firstindex] = 1

        # Insert empty columns between level2 headers to correspond with above
        level2blankafter = cumsum(level1.colspan)[blankafter]
        level2indices = sort(c(seq_along(level2.header), level2blankafter))
        level2firstindex = !duplicated(level2indices)

        tmp.header2[level2firstindex] = level2.header
        tmp.header2[!level2firstindex] = " "

        # Save new generated headers
        level1.colspan = tmp.colspan
        level1.header = tmp.header1
        level2.header = tmp.header2

        if (sum(level1.colspan) != length(level2.header)) {
          error = "The generated level 1 colspan doesn't match the number of level 2 headers."
          warning(error)
          return(list(succes = error))
        }

        apa.header = header = level2.header

      }
    } else {
      apa.header = header = level1.header
    }

    signif = which(header == "*", arr.ind = TRUE) - 1
    script = which(header == "_", arr.ind = TRUE) - 1
    bridge = which(header == " ", arr.ind = TRUE)

    # Check significance columns
    if(length(signif) > 0) {

      # Convert "+" symbol to unicode dagger symbol
      data[which(data == "+", arr.ind = TRUE)] = "\u2020"

      # Create a footnote indicating significant values
      if (save == TRUE) {
        apa.signif = apaStyle::apa.signif(data)$signif
      }
    }

    # Create a user defined footnote
    if(!is.null(note) && save == TRUE) {      
      apa.note = officer::fpar( 
        officer::ftext("Note. ", prop = officer::fp_text(font.family = "Times", font.size = 12, italic = TRUE)), 
        officer::ftext(note, prop = officer::fp_text(font.family = "Times", font.size = 12)) 
      )

    } else {
      apa.note = ""
    }

    # Include empty columns where column spaces are requested
    if (length(bridge) > 0) {
      apa.void = rep("", nrow(data))
      for(i in 1:length(bridge)) {
        if(bridge[i] == length(header)) {
          data = data.frame(data, apa.void)
        } else if (bridge[i] == 1) {
          data = data.frame(apa.void, data)
        } else {
          data = data.frame(data[1:bridge[i]-1], apa.void, data[(bridge[i]):ncol(data)])
        }
      }
    }

    # Check if the length of the dataframe matches with the length of the header
    if (ncol(data) != length(header)) {
      error = "The supplied data doesn't match the specified table header."
      warning(error)
      return(list(succes = error))
    } else {

      # Text default for the APA Table
      flextable::set_flextable_defaults(font.size = 10, font.family = 'Times', table.layout = 'fixed')

      # Create APA table
      apa.table = flextable::flextable(data)
      apa.table = flextable::delete_part(x = apa.table, part = "header")
      apa.table = flextable::border_remove(x = apa.table)
      apa.table = flextable::align(apa.table, j = 2:length(data), align = "center")

      # Set width for the empty bridge columns
      if (length(bridge) > 0) {
        for(idx in 1:length(bridge)) {
          apa.table = flextable::width(apa.table, j = bridge[idx], width = .1)
        }
      }

      colspan = c()
      merged = 0
      skip = FALSE

      for(j in 1:length(data)) {

        if((length(signif) > 0 & signif[1] == j) | (length(script) > 0 & script[1] == j)) {

          # Format all the data
          apa.table = flextable::align(apa.table, j = j, align = "right")
          apa.table = flextable::padding(apa.table, j = j, padding.right = 0, padding.left = 7, padding.top = 7, padding.bottom = 7)
          apa.table = flextable::hline(apa.table, i = nrow(data), j = j, border = officer::fp_border())

          index = j + 1
          remove = index - merged

          colspan = c(colspan, 2)
          apa.header = apa.header[-remove]

          # Format the first column with variable names
          apa.table = flextable::align(apa.table, j = index, align = "left")
          apa.table = flextable::padding(apa.table, j = index, padding.left = 0, padding.right = 7, padding.top = 7, padding.bottom = 7)
          apa.table = flextable::hline(apa.table, i = nrow(data), j = index, border = officer::fp_border())

          merged = merged + 1
          skip = TRUE

          if (length(signif) > 0 & signif[1] == j) {
            signif = signif[-1]
          } else {
            script = script[-1]

            apa.table = flextable::compose(
              apa.table, j = index, value = flextable::as_paragraph(
                flextable::as_sub(apa.table[, index]) 
              ) 
            )

          }

        } else {

          if (TRUE == skip) {
            skip = FALSE
          } else {
            colspan = c(colspan, 1)
            apa.table = flextable::padding(apa.table, j = j, padding = 7)
            apa.table = flextable::hline(apa.table, i = nrow(data), j = j, border = officer::fp_border())
          }

        }
      }

      if (length(data) != sum(colspan)) {
        error = "The sum of colspan is different from the number of columns of the dataset."
        warning(error)
        return(list(succes = error))
      }

      if (level2 == TRUE) {

        if (length(level2.header) != sum(level1.colspan)) {
          error = "The sum of colspan is different from the number of columns of the dataset."
          warning(error)
          return(list(succes = error))
        }

        apa.table = flextable::add_header_row(apa.table, values = apa.header, colwidths = colspan)
        apa.table = flextable::add_header_row(apa.table, values = level1.header, colwidths = level1.colspan)

        borders = sapply(grep("\\w", level1.header, perl = TRUE), function(x) sum(level1.colspan[1:x-1])+1)

        if (length(borders) > 0) {
          for(idx in 1:length(borders)) {
            apa.table = flextable::hline(apa.table, i = 1, j = borders[idx], border = officer::fp_border(), part = "header")
          }
        }

      } else {
        apa.table = flextable::add_header_row(apa.table, values = apa.header, colwidths = colspan)
      }

      apa.table = flextable::hline_top(apa.table, part="all", border = officer::fp_border())
      apa.table = flextable::align(apa.table, j = 1, align = "left", part = "header")
      apa.table = flextable::align(apa.table, j = 2:length(data), align = "center", part = "header")
      apa.table = flextable::padding(apa.table, padding = 7, part = "header")     

      # Put APA reserved abbreviations in italic
      for(j in 1:length(header)) {
        if(is.element(header[j], apa.italics)) {
          row = ifelse(level2 == TRUE, 2, 1)
          apa.table = flextable::italic(apa.table, i = row, j = j, part = "header")
        }
      }

      # Apply autofit differently for complex tables
      if (level2 == TRUE) {
        apa.table =  flextable::autofit(apa.table, part = "body")
      } else {
        apa.table =  flextable::autofit(apa.table)
      }
      
      if (save == TRUE) {

        # Generate MS Word document
        apa.doc = read_docx()

        # Add content to word document
        apa.name = officer::fpar( 
          officer::ftext(apa.tableName, prop = officer::fp_text(font.family = "Times", font.size = 12))
        )

        apa.title = officer::fpar( 
          officer::ftext(title, prop = officer::fp_text(font.family = "Times", font.size = 12, italic = TRUE)) 
        )

        apa.doc = officer::body_add(apa.doc, apa.name)
        apa.doc = officer::body_add(apa.doc, apa.title)
        apa.doc = flextable::body_add_flextable(apa.doc, value = apa.table, align = "left")

        # Add table footers
        if(!is.null(apa.note)) {
          apa.doc = officer::body_add(apa.doc, apa.note)
        }

        if(!is.null(apa.signif)) {
          apa.doc = officer::body_add(apa.doc, apa.signif)
        }

        if(landscape == TRUE) {
          apa.doc = officer::body_end_section_landscape(apa.doc)
        }

        if (file.exists(apa.filename)) {
          if(!file.create(apa.filename, overwrite = TRUE, showWarnings = FALSE)[1]) {
            error = "The specified filename already exists and is used by another application. Make sure you close this application first."
            warning(error)
            return(list(succes = error))
          }
        }

        print(apa.doc, target = apa.filename)

      }

      return(list(succes = TRUE, save = save, table = apa.table))

    }

  }

}
