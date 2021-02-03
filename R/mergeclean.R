

#' merge and remove duplicate from pubmed searches
#'
#' merge together pubmed searches, add columns that index search phase, search terms, and search date\cr
#' flag the duplicates, export an excel file\cr
#' remove the duplicates, add a searchid column, export another excel file\cr
#' video tutorial:\href{https://youtu.be/3zj1b8jar28}{https://youtu.be/3zj1b8jar28}
#'
#' @param path the folder where each pubmed .csv export files are stored
#' @param newfilename the name of the two new consolidated excel files without the extension
#'
#' @examples
#' mergeclean(path="/Users/phoebelam/Google Drive/2021/4_meta_els sleep & inflammation/1_search_els & sleep",
#' newfilename = "0_merged els & sleep")
#'
#' @importFrom magrittr "%>%"
#' @export
mergeclean <- function (path, newfilename) {

  #troubleshoot
  # path="/Users/phoebelam/Google Drive/2021/1_meta_els & inflammation/1_search/third search/individual files"
  # f = "/Users/phoebelam/Google Drive/2021/1_meta_els & inflammation/1_search/third search/individual files/1_early adversity & inflammation_2.3.21.csv"
  # newfilename = "0_merged els & inflammation"

  print (sample(imonit, 1))

  Sys.sleep(5)

  setwd (path)
  temp <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(temp, paste(newfilename, "_temp.RDS", sep=""))

  filenames = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = FALSE)

  for (f in filenames) {
    print (f)

    dat <- read.csv(f)
    # dat <- read.csv("9_child maltreatment & inflammation_2.3.21.csv")

    dat %>%
      dplyr::mutate (search = as.character(basename(f))) %>%
      tidyr::separate (search, c("search_phase", "search_terms", "search_date"), sep="_") %>%
      dplyr::mutate (search_date = gsub(".csv", "", search_date)) -> dat

    temp <- readRDS (paste(newfilename, "_temp.RDS", sep=""))
    temp <- gtools::smartbind (temp, dat)
    saveRDS(temp, paste(newfilename, "_temp.RDS", sep=""))
  }

  consol <- readRDS (paste(newfilename, "_temp.RDS", sep=""))[-1,-1]
  consol %>%
    dplyr::mutate (duplicate = dplyr::case_when(duplicated(Title)==T~ "duplicate",
                                  TRUE~ "unique")) %>%
    dplyr::select(search_phase, search_terms, search_date, duplicate, Title:Publication.Year, PMID, Create.Date:DOI) %>%
    arrange (as.numeric(search_phase))-> consol

  consol %>%
    dplyr::distinct(Title, .keep_all = T) %>%
    dplyr::mutate (screenid = row_number()) %>%
    dplyr::select(screenid, search_phase, search_terms, search_date, Title:Publication.Year, PMID, Create.Date:DOI) %>%
    arrange (as.numeric(search_phase)) -> nodup

  file.remove(paste(newfilename, "_temp.RDS", sep=""))

  xlsx::write.xlsx (consol, paste(newfilename, "_",format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
  xlsx::write.xlsx (nodup, paste(newfilename, "_dup removed_",format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)

  # return(paste("donzo! please delete the temp file: ", newfilename, "_temp.RDS", sep=""))
  print (sample(donzo, 1))
}





