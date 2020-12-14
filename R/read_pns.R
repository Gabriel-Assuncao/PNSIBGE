#' Read PNS microdata
#' @description This function reads PNS microdata.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param microdata A text file containing microdata from PNS survey, available on official website: (select a microdata file, according to the appropriated year, microdata folder and then, inside, data) - ftp://ftp.ibge.gov.br/PNS/.
#' @param input_txt A text file, related to the microdata, containing the input script for SAS, available on official website: (select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, documentation) - ftp://ftp.ibge.gov.br/PNS/.
#' @param vars Vector of variable names to be kept for analysys. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labelling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{pns_labeller} for labelling categorical variables from PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variable to PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS example files.
#' @examples
#' input_path <- pns_example(path="input_example.txt")
#' data_path <- pns_example(path="exampledata.txt")
#' pns.df <- read_pns(microdata=data_path, input_txt=input_path, vars="J007")
#' @export

read_pns <- function(microdata, input_txt, vars = NULL) {
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(suppressMessages({readr::read_table2(input_txt, col_names=FALSE) %>% subset(substr(X1, 1, 1) == "@") %>%
    dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"), start=as.numeric(gsub("@", "", X1)), X3=as.integer(chartr("$", " ", X3)), end=start+X3-1)}))
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      warning(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c("V0001", "UPA_PNS", "V0006_PNS", "V0015", "V0020", "V0024", "V0028", "V00282", "V00283", "V0029", "V00292", "V00293", "V0030", "V00302", "V00303", "C00301", "M001", "W001", vars))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pns <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  return(data_pns)
}
