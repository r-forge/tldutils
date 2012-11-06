#' Computes sales tax
#'
#' \code{compute_sales_tax} computes sales tax and post tax figures
#' 
#' @param pre_tax The pre-tax price 
#' @param tax_rate The ad-valorem sales tax rate
#' @return Returns a list with pre_tax, sales_tax, post_tax, and tax_rate.
#' @export 
compute_sales_tax <- function(pre_tax, tax_rate=0.08375) {
    sales_tax <- pre_tax * tax_rate
    post_tax <- pre_tax + sales_tax
    return(list(pre_tax=pre_tax, sales_tax=sales_tax, post_tax=post_tax, 
                    tax_rate=tax_rate))
}

#' Loads packages, installing them if necessary
#'
#' \code{load_packages_robustly} loads a character vector of packages
#' installing any packages not currently installed
#'
#' @param packages Character vector of package names to be loaded
#' @param repos Base URLs of repositories to use
#' @return Returns invisible(NULL), as side effect loads and possibly installs packages
#' @export 
load_packages_robustly <- function(packages, repos=getOption("repos")) {
    for(package in packages) {
        if(!require(package, character.only=TRUE)) {
            install.packages(package, repos=repos)
            library(package, character.only=TRUE)
        }
    }
    return(invisible(NULL))
}

#' Creates directories (suppressing warnings and creates parents by default)

#' \code{mkdir} Takes a character vector of directories and creates them.  
#'
#' @param directory Character vector of directories to create
#' @param parents If \code{TRUE} (the default) create parent directories if they do not exist
#' @param mode mode to be used on Unix-alikes
#' @return Returns \code{invisible(NULL)}, as a side effect creates directories.
#' @seealso \code{\link[base]{dir.create}}
#' @export
mkdir <- function(directory, parents=TRUE, mode="0777") {
    for(dir_ in directory) {
        base::dir.create(dir_, showWarnings=FALSE, recursive=parents, mode=mode) 
    }
    invisible(NULL)
}

#' Creates a csv file (with different defaults to \code{utils::write.csv})

#' \code{write_csv} creates csv files with different defaults to \code{write.csv}, in particular 
#'  by default we suppress row names and fill missing values with "" instead of "\code{NA}"
#'
#' @param x the object to be written passed to \code{write.csv}
#' @param file passed to write.csv, use \code{file=gzfile(filename)} to write a gzip compressed file
#' @param na String to use for missing values passed to \code{write.csv}
#' @param row.names Passed to \code{write.csv}, determines whether to write row names or what they should be, passed to \code{write.csv}
#' @param ... Other arguments passed to \code{write.csv}
#' @return Returns \code{invisible(NULL)}, as a side effects creates a csv file.
#' @seealso \code{\link[utils]{write.csv}}
write_csv <- function(x, file, na="", row.names=FALSE, ...) {
    utils::write.csv(x, file, na=na, row.names=row.names, ...)
    invisible(NULL)
}

#' Takes out all the numbers in a string and converts to a number
#'
#' \code{to_string} extracts the numbers in a character vector and 
#' returns the extracted numbers as a numeric vector.
#'
#' @param strings Character vector
#' @return Returns a vector of class "numeric".
#' @export
#' @examples
#' strings <- c("FO23_43", "F10.4", "what3,00.", "what3..0..")
#' to_numeric(strings)
to_numeric <- function(strings) {
    strings <- gsub("[^[:digit:]]*([[:digit:]]*\\.{0,1}[[:digit:]]+)[^[:digit:]]*", "\\1", strings)
    base::as.numeric(strings)
}
    
#' Takes a vector and converts it into a square matrix
#'
#' \code{sq_matrix} takes a vector and converts it into a square matrix.
#'
#' @param data Object (probably vector) to be turned into square matrix
#' @param byrow Whether to fill in the matrix by row or by column
#' @return A matrix with the same number of columns and rows
#' @seealso \code{link{matrix}}
#' @examples
#' sq_matrix(1:4)
#' sq_matrix(1:4, FALSE)
#' matrix(1:4, nrow=2)
#' @export 
sq_matrix <- function(data, byrow=TRUE) {
    n <- sqrt(length(data))
    return(matrix(data, n, n, byrow))
}

#' Evaluate strings in the current environment
#'
#' \code{eval_string} evaluates strings of R code in the current environment.
#'
#' @param string A character vector containing legitimate R code
#' @return Returns \code{invisible(NULL)}.  As a side effect evaluates strings of R code in the environment that called \code{eval_string}.
#' @seealso \code{\link{eval}}, \code{\link{parse}}
#' @examples
#' eval("x <- 1") # x not assigned 1
#' eval(parse(text="y <- 2")) # y = 2
#' eval_string("z <- 3") # z = 3
#' @export 
eval_string <- function(string) {
    eval.parent(parse(text=string), 1)
    return(invisible(NULL))
}

#' Prints various stats of a beta distribution
#'
#' \code{compute_beta_stats} calculates mean and variance of beta distribution
#'
#' @param alpha Numeric value for alpha parameter of beta distribution
#' @param beta Numeric value for beta parameter of beta distribution
#' @return Returns a list containing the alpha parameter value,
#' beta parameter value, mean, and variance 
#' @examples
#' compute_beta_stats(1, 2)
#' compute_beta_stats(2, 1)
#' @export 
compute_beta_stats <- function(alpha, beta) {
    return(list(alpha=alpha, beta=beta, mean=alpha/(alpha+beta),
                    variance=alpha*beta / ((alpha+beta)^2 * (alpha+beta+1))))
}

#' Knits and compiles a knitr compatible Sweave file
#'
#' \code{knit_and_compile} Knits and compiles a knitr compatible Sweave file
#'
#' @param file A knitr compatible Sweave file
#' @param pdf Logical, whether we should produce a pdf or dvi file
#' @param clean Logical, whether we delete intermediate latex files
#' @param ... Passed onto tools::texi2dvi
#' @seealso \code{\link[knitr]{knit}, \link[tools]{texi2dvi}}
#' @export
knit_and_compile <- function(file=NULL, pdf=TRUE, clean=TRUE, ...) {
    if (is.null(file)) {
        files = list.files(pattern = ".*.Rnw$")
        file = files[which.max(file.info(files)$mtime)]
    }
    knitr::knit(file)
    file_tex <- sub("Rnw$", "tex", file)
    tools::texi2dvi(file_tex, pdf=pdf, clean=clean, ...)
}

#' Various helpful commands
#'
#' A few random helpful commands:
#' \code{exit()} quits R without saving data,
#' \code{lg()} prints all the objects in current environment.
#' @rdname commands
#' @export
#' @examples
#' lg()
lg <- function() { base::ls(all.names=TRUE, pos=1) }
#' @rdname commands
#' @export
exit <- function() { quit("no") }

# #' print_foobar <- "print('foobar')"
# #' class(print_foobar) <- "command"
# #' print_foobar
# #' square_x <- "x <- x^2"
# #' class(square_x) <- "command"
# #' x <- 2
# #' square_x
# # exit <- "q('no')"
# # class(exit) <- "command"
# # #' @rdname commands
# # #' @export
# # lg <- "print(base::ls(all.names=TRUE, pos=1))"
# # class(lg) <- "command"
# # #' @rdname commands
# # #' @export
# # pwd <- "print(getwd())"
# # class(pwd) <- "command"
# # #' @rdname commands
# # #' @export
# # weave <- "knit_and_compile()"
# # class(weave) <- "command"
# #' @rdname commands
# #' @export
# #' @param x An object of class "command" to execute
# #' @param ... Unused, for consistency with S3 print generic
# #' @return Returns invisible(NULL), side effect executes command
# #' @method print command
# #' @format A character vector of legitimate R code to execute
# # print.command <- function(x, ...) { eval.parent(parse(text=x), 1) }
