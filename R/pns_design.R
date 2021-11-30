#' Create PNS survey object with its sample design
#' @description This function creates PNS survey object with its sample design for analysis using \code{survey} package functions.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pns A tibble of PNS microdata read with \code{read_pns} function.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNS and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labeling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_labeller} for labeling categorical variables from PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variables to PNS microdata.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS toy example files.
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
#' pns.svy <- pns_design(data_pns=pns.df)
#' # Calculating proportion of people diagnosed with chronic diseases
#' if (!is.null(pns.svy)) survey::svymean(x=~J007, design=pns.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' pns.df2 <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, vars=c("J007","J009"),
#'                        labels=TRUE, deflator=TRUE, design=FALSE, savedir=tempdir())
#' pns.svy2 <- pns_design(data_pns=pns.df2)
#' # Calculating proportion of people diagnosed with chronic diseases
#' if (!is.null(pns.svy2)) survey::svymean(x=~J007, design=pns.svy2, na.rm=TRUE)}
#' @export

pns_design <- function(data_pns) {
  if (sum(class(data_pns) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA_PNS", "ID_DOMICILIO", "V0024", "V0028", "V00281", "V00282", "V00283") %in% names(data_pns))) |
        !(FALSE %in% (c("UPA_PNS", "ID_DOMICILIO", "V0024", "V0029", "V00291", "V00292", "V00293") %in% names(data_pns))) |
        !(FALSE %in% (c("UPA_PNS", "ID_DOMICILIO", "V0024", "V0030", "V00301", "V00302", "V00303") %in% names(data_pns)))) {
      options(survey.lonely.psu="adjust")
      options(survey.adjust.domain.lonely=TRUE)
      if (!(FALSE %in% (c("UPA_PNS", "ID_DOMICILIO", "V0024", "V0028", "V00281", "V00282", "V00283") %in% names(data_pns)))) {
        data_prior <- survey::svydesign(ids=~UPA_PNS, strata=~V0024, data=data_pns, weights=~V0028, nest=TRUE)
        popc.types <- data.frame(V00283=as.character(unique(data_pns$V00283)), Freq=as.numeric(unique(data_pns$V00282)))
        popc.types <- popc.types[order(popc.types$V00283),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00283, population=popc.types)
      }
      else if (!(FALSE %in% (c("UPA_PNS", "ID_DOMICILIO", "V0024", "V0029", "V00291", "V00292", "V00293") %in% names(data_pns)))) {
        data_prior <- survey::svydesign(ids=~UPA_PNS, strata=~V0024, data=data_pns, weights=~V0029, nest=TRUE)
        popc.types <- data.frame(V00293=as.character(unique(data_pns$V00293)), Freq=as.numeric(unique(data_pns$V00292)))
        popc.types <- popc.types[order(popc.types$V00293),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00293, population=popc.types)
      }
      else {
        data_prior <- survey::svydesign(ids=~UPA_PNS, strata=~V0024, data=data_pns, weights=~V0030, nest=TRUE)
        popc.types <- data.frame(V00303=as.character(unique(data_pns$V00303)), Freq=as.numeric(unique(data_pns$V00302)))
        popc.types <- popc.types[order(popc.types$V00303),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~V00303, population=popc.types)
      }
    }
    else {
      message("Weight variables required for sample design are missing.")
      data_posterior <- data_pns
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so applying another design is not possible.")
    data_posterior <- data_pns
  }
  return(data_posterior)
}
