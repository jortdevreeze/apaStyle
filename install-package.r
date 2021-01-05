args = commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("A requirements file must be supplied.", call.=FALSE)
}

read.table2 <- function(file, ...) {
  tryCatch(read.table(text = readLines(file, warn = FALSE), sep = '=', fill = TRUE), error = function(c) {
    stop(c$message)
  })
}

install.packages2 = function(pkg, tag = NULL) {
    if (length(pkg)) {
      cat('Installing', pkg, 'version:', tag, '.')
      tryCatch(devtools::install_version(pkg, version = tag, dependencies = TRUE), error = function(c) {
        cat('Could not install', pkg, 'version:', tag, '.')
        print(c$message)
      })
    }
}

if (file.info(args[1])$size != 0) {

  print('Installing dependencies:')

  requirements <- read.table2(args[1])
  requirements[] <- lapply(requirements, as.character)

  print(requirements[,1])

  pkg1 <- subset(requirements, (!is.na(requirements[,2])) & (requirements[,2] != ''))
  mapply(install.packages2, pkg1[,1], pkg1[,2])

  pkg2 <- subset(requirements, (is.na(requirements[,2])) | (requirements[,2] == ''))
  mapply(install.packages2, pkg2[,1])
  
}