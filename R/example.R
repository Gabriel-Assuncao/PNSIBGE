#' Get the path of the PNS example files
#' @description This function provides the path of the PNS example files, loaded with this package.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param path Name of file. If \code{NULL}, the PNS example files names will be listed.
#' @return A vector with names of all the available PNS example files or the path for specific requested PNS example file.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labelling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_labeller} for labelling categorical variables from PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variable to PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.
#' @examples
#' pns_example()
#' pns_example(path="exampledata.txt")
#' pns_example(path="input_example.txt")
#' pns_example(path="dictionaryexample.xls")
#' pns_example(path="deflatorexample.xls")
#' @export

pns_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package="PNSIBGE"))
  }
  else {
    system.file("extdata", path, package="PNSIBGE", mustWork=TRUE)
  }
}
