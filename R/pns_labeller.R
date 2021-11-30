#' Label categorical variables from PNS microdata
#' @description This function labels categorical variables from PNS microdata.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pns A tibble of PNS microdata read with \code{read_pns} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: (select the dictionary and input zip file, according to the appropriated year, microdata folder and then, inside, documentation) - \url{https://ftp.ibge.gov.br/PNS/}.
#' @return A tibble with the data provided from PNS survey and its categorical variables as factors with related labels.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{get_pns} for downloading, labeling, deflating and creating survey design object for PNS microdata.\cr \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variables to PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS toy example files.
#' @examples
#' # Using data read from disk
#' input_path <- pns_example(path="input_example.txt")
#' data_path <- pns_example(path="exampledata.txt")
#' dictionary.path <- pns_example(path="dictionaryexample.xls")
#' pns.df <- read_pns(microdata=data_path, input_txt=input_path, vars=c("J007","J009"))
#' pns.df <- pns_labeller(data_pns=pns.df, dictionary.file=dictionary.path)
#' \donttest{
#' # Downloading data
#' pns.df2 <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, vars=c("J007","J009"),
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
    notlabel <- c("UPA_PNS", "UPA", "ID_DOMICILIO", "V0006_PNS", "V0020", "V0022", "V0024",
                  "V0028", "V00281", "V00282", "V00283",
                  "V0029", "V00291", "V00292", "V00293",
                  "V0030", "V00301", "V00302", "V00303",
                  "A010", "A01001", "A011", "A014", "A01401", "A01402",
                  "A018012", "A018014", "A018016", "A018018",
                  "A018020", "A018022", "A018024", "A018026", "A018028",
                  "A01802", "A01804", "A01806", "A01808", "A01810",
                  "A01812", "A01813", "A01814", "A01816", "A01818", "A01819",
                  "A020", "A02102", "A02301", "A02302", "A02303", "A02304",
                  "A02305", "A02306", "A02307", "A02308", "A02401", "A02402",
                  "VDC001", "VDC002", "VDC003", "VDF002", "VDF003", "VDF00102",
                  "C001", "C00301", "C00701", "C00702", "C00703", "C008", "C01801",
                  "E010011", "E010012", "E010013", "E01201", "E01501",
                  "E01602", "E01604", "E017", "E01802", "E01804", "E019",
                  "E024021", "E02501", "E02502", "E02503", "E033",
                  "F001021", "F007021", "F008021", "F010021", "F011021",
                  "F012021", "F013021", "F014021", "I001021", "I00701",
                  "J003", "J006", "J012", "J019", "J038", "J04001", "J04002",
                  "K04302", "M00001", "M00002", "M00003", "M00303", "M00401",
                  "M00402", "M005011", "O00901", "O02101", "P00103", "P00104",
                  "P00403", "P00404", "P006", "P00901", "P01101", "P013", "P015",
                  "P02001", "P01601", "P018", "P02002", "P023", "P02501", "P02602",
                  "P02801", "P029", "P03202", "P035", "P03701", "P03702", "P03904",
                  "P03905", "P03906", "P04001", "P04101", "P04102", "P042", "P04301",
                  "P04302", "P04401", "P04405", "P04406", "P05402", "P05403",
                  "P05405", "P05406", "P05408", "P05409", "P05411", "P05412",
                  "P05414", "P05415", "P05417", "P05418", "P05421", "P05422",
                  "P05601", "P05602", "P05603", "P05604", "P05605", "P057", "P5701",
                  "P05801", "P05802", "P05901", "P05902", "P05903", "P05904",
                  "Q003", "Q031", "Q061", "Q064", "Q070", "Q075", "Q080", "Q085",
                  "Q08901", "Q09301", "Q111", "Q11701", "Q12201", "Q125", "Q133",
                  "R012", "R025", "R027", "S066", "S06701", "S06702", "S06703",
                  "S06901", "S06902", "S099", "S09901", "S11001", "S11801",
                  "U02303", "U02403", "Z00101", "Z00102", "Z002", "Z003", "Z01401",
                  "Z01402", "Y00101", "AA00701", "AA00702", "AA00801", "AA00802",
                  "AA02001", "AA02002", "AA02101", "AA02102", "AA02103", "AA02104",
                  "AA022", "AA02601", "AA02602", "AA02701", "AA02702", "AA03601",
                  "AA03602", "AA03701", "AA03702", "AA03703", "AA03704", "AA038",
                  "W00201", "W00101", "W00202", "W00102", "W00203", "W00103",
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
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so labeling categorical variables is not possible.")
  }
  return(data_pns)
}
