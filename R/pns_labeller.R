#' Label categorical variables from PNS microdata
#' @description This function labels categorical variables from PNS microdata.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param data_pns A tibble of PNS microdata read with \code{read_pns} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: (select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, documentation) - ftp://ftp.ibge.gov.br/PNS/.
#' @return A tibble with the data provided from PNS survey and its categorical variables as factors with related labels.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labelling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variable to PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS example files.
#' @examples
#' # Using data read from disk
#' input_path <- pns_example(path="input_example.txt")
#' data_path <- pns_example(path="exampledata.txt")
#' dictionary.path <- pns_example(path="dictionaryexample.xls")
#' pns.df <- read_pns(microdata=data_path, input_txt=input_path, vars="J007")
#' pns.df <- pns_labeller(data_pns=pns.df, dictionary.file=dictionary.path)
#' \donttest{
#' # Downloading data
#' pns.df2 <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, vars="J007",
#'                        labels=FALSE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' dictionary.path2 <- pns_example(path="dictionaryexample.xls")
#' pns.df2 <- pns_labeller(data_pns=pns.df2, dictionary.file=dictionary.path2)}
#' @export

pns_labeller <- function(data_pns, dictionary.file) {
  if (sum(class(data_pns) == "tbl_df") > 0) {
    dictionary <- suppressMessages(readxl::read_excel(dictionary.file))
    X__3 = X__6 = X__7 = NULL
    colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
    dictionary %<>% subset(!is.na(X__6))
    codcurrent <- dictionary$X__3
    for (i in 1:dim(dictionary)[1]) {
      if (is.na(dictionary$X__3[i])) {
        dictionary$X__3[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X__3[i]
      }
    }
    notlabel <- c("UPA_PNS", "UPA", "V0006_PNS", "V0020", "V0022", "V0024",
                  "V0028", "V00281", "V00282", "V00283",
                  "V0029", "V00291", "V00292", "V00293",
                  "V0030", "V00301", "V00302", "V00303",
                  "A010", "A011", "A014",
                  "A01802", "A01804", "A01806", "A01808", "A01810",
                  "A01812", "A01813", "A01814", "A01816", "A01818", "A01819",
                  "A020", "A02301", "A02302", "A02303", "A02304",
                  "VDC001", "VDC002", "C00301", "C00701", "C00702", "C00703",
                  "C008", "E01201", "E01501", "I00701",
                  "Deflator")
    vars <- names(data_pns)
    varsc <- vars[sapply(data_pns, class) == "character"]
    varsf <- setdiff(varsc, notlabel)
    for (i in 1:length(varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X__3)) {
        data_pns[varsf[i]] <- factor(suppressWarnings(as.numeric(unlist(data_pns[varsf[i]]))),
                                       levels=suppressWarnings(as.numeric(unlist(dictionary %>% subset(X__3 == varsf[i]) %>% select(X__6)))),
                                       labels=unlist(dictionary %>% subset(X__3 == varsf[i]) %>% select(X__7)))
      }
    }
  }
  else {
    warning("Sample design was already defined for microdata, so labelling categorical variables is not possible.")
  }
  return(data_pns)
}
