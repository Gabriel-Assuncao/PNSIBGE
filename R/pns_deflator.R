#' Add deflator variables to PNS microdata
#' @description This function adds deflator variables to PNS microdata. For deflation of income variables, the documentation provided through the following address must be used: \url{https://ftp.ibge.gov.br/PNS/Documentacao_Geral/PNSIBGE_Deflator.pdf}.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pns A tibble of PNS microdata read with \code{read_pns} function.
#' @param deflator.file The deflator file for selected survey available on official website: (select the deflator zip file) - \url{https://ftp.ibge.gov.br/PNS/Documentacao_Geral/}.
#' @return A tibble with the data provided from PNS survey and the deflator variables added for use.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labeling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_labeller} for labeling categorical variables from PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS toy example files.
#' @examples
#' # Using data read from disk
#' input_path <- pns_example(path="input_example.txt")
#' data_path <- pns_example(path="exampledata.txt")
#' dictionary.path <- pns_example(path="dictionaryexample.xls")
#' deflator.path <- pns_example(path="deflatorexample.xls")
#' pns.df <- read_pns(microdata=data_path, input_txt=input_path, vars=c("J007","J009"))
#' pns.df <- pns_labeller(data_pns=pns.df, dictionary.file=dictionary.path)
#' pns.df <- pns_deflator(data_pns=pns.df, deflator.file=deflator.path)
#' \donttest{
#' # Downloading data
#' pns.df2 <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, vars=c("J007","J009"),
#'                        labels=TRUE, deflator=FALSE, design=FALSE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' deflator.path2 <- pns_example(path="deflatorexample.xls")
#' pns.df2 <- pns_deflator(data_pns=pns.df2, deflator.file=deflator.path2)}
#' @export

pns_deflator <- function(data_pns, deflator.file) {
  if (sum(class(data_pns) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("V0020", "V0001") %in% names(data_pns)))) {
      data_pns <- data_pns[, !names(data_pns) %in% c("Deflator"), drop=FALSE]
      deflator <- suppressMessages(readxl::read_excel(deflator.file))
      colnames(deflator)[c(1:2)] <- c("V0020", "V0001")
      deflator$V0001 <- as.factor(deflator$V0001)
      if (identical(intersect(levels(deflator$V0001), levels(as.factor(data_pns$V0001))), character(0)) & length(levels(deflator$V0001)) == length(levels(as.factor(data_pns$V0001)))) {
        levels(deflator$V0001) <- levels(as.factor(data_pns$V0001))
      }
      data_pns <- merge(x=data_pns, y=deflator, by.x=c("V0020", "V0001"), by.y=c("V0020", "V0001"), all.x=TRUE, all.y=FALSE)
      if (!(FALSE %in% (c("ID_DOMICILIO") %in% names(data_pns)))) {
        data_pns <- data_pns[order(data_pns$V0024, data_pns$ID_DOMICILIO, data_pns$C00301),]
      }
      else {
        data_pns <- data_pns[order(data_pns$V0024, data_pns$UPA_PNS, data_pns$V0006_PNS, data_pns$C00301),]
      }
      data_pns <- tibble::as_tibble(data_pns)
    }
    else {
      message("Merge variables required for adding deflator variables are missing.\n")
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so adding deflator variables is not possible.\n")
  }
  return(data_pns)
}
