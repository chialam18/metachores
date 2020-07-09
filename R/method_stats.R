
#' method discrepancies aggregates
#'
#'
#' @param path the folder where the raw jp method file is and the edited discrepancy file
#' @param start the link.id to begin checking, put in quotes
#' @param end the link.id to end checking, put in quotes
#'
#' @examples
#' method(path="/Users/phoebelam/Google Drive/2020/4_meta_els sleep & inflammation/1_search_els & sleep", start = "a21_1", end = "a30b_1")
#'
#' @importFrom magrittr "%>%"
#' @export
method_stats <- function(path, start, end) {
  
  print (sample(imonit, 1))
  Sys.sleep(5)
  
  raw <- read.csv(paste(path, "/method extraction - method.csv", sep=""), na.strings = "")
  which(raw$link.id == start) -> startrow
  which(raw$link.id == end) -> endrow
  raw[startrow:endrow, ] -> raw
  

  path2 <- paste(path, "/method discrepancies_07.08.20.xlsx", sep = "")
  
  path2 %>% 
    readxl::excel_sheets() %>% 
    purrr::set_names() %>% 
    purrr::map(read_excel, path = path2) -> disc

  lapply(names(disc), function(x) assign(x, disc[[x]], envir = .GlobalEnv))

  tot <- endrow - startrow + 1
  
  discrep_percent = round (c(nrow(`sample notes`)/nrow(filter(raw, is.na(article.id)==F))*100,
                      nrow(iv.assessment)/tot*100,
                      nrow(iv.timeframe)/tot*100,
                      nrow(iv.agemean)/tot*100,
                      nrow(iv.agesd)/tot*100,
                      nrow(iv.agerange)/tot*100,
                      nrow(dv.assessment)/tot*100,
                      nrow(dv.timeframe)/tot*100,
                      nrow(dv.agemean)/tot*100,
                      nrow(dv.agesd)/tot*100,
                      nrow(dv.agerange)/tot*100,
                      nrow(design)/tot*100,
                      nrow(female)/tot*100,
                      nrow(white)/tot*100,
                      nrow(black)/tot*100,
                      nrow(latino)/tot*100,
                      nrow(asian)/tot*100,
                      nrow(other)/tot*100,
                      nrow(psychiatric)/tot*100,
                      nrow(physical)/tot*100,
                      nrow(subgroupna)/tot*100,
                      nrow(country)/tot*100,
                      nrow(adiposity)/tot*100), 2)
  
  discrep = c(nrow(`sample notes`),
              nrow(iv.assessment),
              nrow(iv.timeframe),
              nrow(iv.agemean),
              nrow(iv.agesd),
              nrow(iv.agerange),
              nrow(dv.assessment),
              nrow(dv.timeframe),
              nrow(dv.agemean),
              nrow(dv.agesd),
              nrow(dv.agerange),
              nrow(design),
              nrow(female),
              nrow(white),
              nrow(black),
              nrow(latino),
              nrow(asian),
              nrow(other),
              nrow(psychiatric),
              nrow(physical),
              nrow(subgroupna),
              nrow(country),
              nrow(adiposity))
  
  total = c(nrow(filter(raw, is.na(article.id)==F)),
            rep(tot, length(discrep)-1))
  
  summary <- list("total" = total,
                  "# discrepancies" = discrep,
                  "% discrepant" = discrep_percent)
  
  output <- matrix(unlist(summary), ncol = 3, byrow = FALSE)
  
  
  rownames(output) <- c("sample notes", "iv.assessment", "iv.timeframe", "iv.agemean", "iv.agesd", "iv.agerange",
                        "dv.assessment", "dv.timeframe", "dv.agemean", "dv.agesd", "dv.agerange", 
                        "design", "female", "white", "black", "latino", "asian", "other",
                        "psychiatric", "physical", "subgroupna", "country", "adiposity")
  
  colnames(output) <- c("total", "# discrepancies", "% discrepant")
  
  capture.output(output, file = paste(path, "/method discrepancies_aggregates_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
  
  print (sample(donzo, 1))
  
  
}