#' Download, label, deflate and create survey design object for PNS microdata
#' @description Core function of package. With this function only, the user can download a PNS microdata from a year and get a sample design object ready to use with \code{survey} package functions.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param year The year of the data to be downloaded. Must be a number equal to 2013 or 2019. Vector not accepted.
#' @param selected Logical value. If \code{TRUE}, the specific questionnaire for selected resident will be used. If \code{FALSE}, the basic questionnaire for household and residents will be used.
#' @param anthropometry Logical value. If \code{TRUE}, the specific questionnaire for the anthropometry module of the selected resident will be used. If \code{FALSE}, the questionnaire defined by the \code{selected} argument of this function will be used. This argument will be used only if \code{year} is equal to 2019.
#' @param vars Vector of variable names to be kept for analysys. Default is to keep all variables.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variable will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} with the data from PNS and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/saude/9160-pesquisa-nacional-de-saude.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNSIBGE]{read_pns} for reading PNS microdata.\cr \link[PNSIBGE]{pns_labeller} for labelling categorical variables from PNS microdata.\cr \link[PNSIBGE]{pns_deflator} for adding deflator variable to PNS microdata.\cr \link[PNSIBGE]{pns_design} for creating PNS survey design object.\cr \link[PNSIBGE]{pns_example} for getting the path of the PNS example files.
#' @examples
#' \donttest{
#' pns.svy <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, vars=c("J007","J00801","J009"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' survey::svymean(x=~J007, design=pns.svy, na.rm=TRUE)
#' pns.svy2 <- get_pns(year=2019, selected=TRUE, anthropometry=FALSE, vars=c("N001","N00101"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' survey::svymean(x=~N001, design=pns.svy2, na.rm=TRUE)
#' pns.svy3 <- get_pns(year=2019, selected=FALSE, anthropometry=TRUE, vars=c("W00101","W00201"),
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' survey::svymean(x=~W00101, design=pns.svy3, na.rm=TRUE)}
#' @export

get_pns <- function(year, selected = FALSE, anthropometry = FALSE, vars = NULL,
                     labels = TRUE, deflator = TRUE, design = TRUE, savedir = tempdir())
{
  if (year != 2013 & year != 2019) {
    stop("Year must be equal to 2013 or 2019.")
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    warning(paste0("The directory provided does not exist, so the directory was set to '", tempdir()), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  ftpdir <- paste0("ftp://ftp.ibge.gov.br/PNS/", year, "/Microdados/")
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n"))
  dataname <- datayear[which(startsWith(datayear, paste0("PNS_", year)))]
  if (length(dataname) == 0) {
    stop("Data unavailable for selected year.")
  }
  docfiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao/"), dirlistonly=TRUE)), "\n"))
  inputzip <- docfiles[which(startsWith(docfiles, "Dicionario_e_input"))]
  utils::download.file(url=paste0(ftpdir, "Documentacao/", inputzip), destfile=paste0(savedir, "/Dicionario_e_input.zip"), mode="wb")
  utils::unzip(zipfile=paste0(savedir, "/Dicionario_e_input.zip"), exdir=savedir)
  utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
  utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
  microdataname <- dir(savedir, pattern=paste0("^PNS_", year, ".*\\.txt$"), ignore.case=FALSE)
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
  inputname <- dir(savedir, pattern=paste0("^input_PNS_", year, ".*\\.txt$"), ignore.case=FALSE)
  inputfile <- paste0(savedir, "/", inputname)
  inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),])[length(inputfile)]
  data_pns <- PNSIBGE::read_pns(microdata=microdatafile, input_txt=inputfile, vars=vars)
  data_pns <- data_pns[data_pns$V0015 == "01",]
  if (anthropometry == TRUE & year == 2019) {
    data_pns <- data_pns[data_pns$W001 == "1",]
    data_pns <- data_pns[,!(names(data_pns) %in% c("V0028", "V00281", "V00282", "V00283", "V0029", "V00291", "V00292", "V00293"))]
    if (selected == TRUE) {
      warning("The definition of TRUE for the selected argument will be ignored, since the anthropometry argument was also defined as TRUE.")
    }
  }
  else if (selected == TRUE | (selected == FALSE & anthropometry == TRUE)) {
    data_pns <- data_pns[data_pns$M001 == "1",]
    data_pns <- data_pns[,!(names(data_pns) %in% c("V0028", "V00281", "V00282", "V00283", "V0030", "V00301", "V00302", "V00303"))]
    if (selected == FALSE) {
      warning("The selected argument was defined as true for the use of the anthropometry module, since the year is different from 2019.")
    }
  }
  else {
    data_pns <- data_pns[,!(names(data_pns) %in% c("V0029", "V00291", "V00292", "V00293", "V0030", "V00301", "V00302", "V00303"))]
  }
  if (labels == TRUE) {
    if (exists("pns_labeller", where="package:PNSIBGE", mode="function")) {
      dicname <- dir(savedir, pattern=paste0("^dicionario_PNS_microdados_", year, ".*\\.xls$"), ignore.case=FALSE)
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
      data_pns <- PNSIBGE::pns_labeller(data_pns=data_pns, dictionary.file=dicfile)
    }
    else {
      warning("Labeller function is unavailable in package PNSIBGE.")
    }
  }
  if (deflator == TRUE) {
    if (exists("pns_deflator", where="package:PNSIBGE", mode="function")) {
      ftpdef <- ("ftp://ftp.ibge.gov.br/PNS/Documentacao_Geral/")
      deffiles <- unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdef, dirlistonly=TRUE)), "\n"))
      defzip <- deffiles[which(startsWith(deffiles, "Deflatores"))]
      utils::download.file(url=paste0(ftpdef, defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
      utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
      defname <- dir(savedir, pattern=paste0("^deflator_PNS.*\\.xls$"), ignore.case=FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
      data_pns <- PNSIBGE::pns_deflator(data_pns=data_pns, deflator.file=deffile)
    }
    else {
      warning("Deflator function is unavailable in package PNSIBGE.")
    }
  }
  if (design == TRUE) {
    if (exists("pns_design", where="package:PNSIBGE", mode="function")) {
      data_pns <- PNSIBGE::pns_design(data_pns=data_pns)
    }
    else {
      warning("Sample design function is unavailable in package PNSIBGE.")
    }
  }
  return(data_pns)
}
