#' title screening specific for round 1 on sleep and inflammation
#'
#' tasks performed:\cr
#' 1.	merge together jc and ra responses\cr
#' 2.	identify and only keep rows with discrepant codes \cr
#' 3.	arrange the discrepant codes by jc 0 vs. ra 1/2; jc 1 vs. ra 0/2; jc 2 vs. ra 0/1\cr
#' 4.	grab ra’s name from filename and place as column name\cr
#' 5.	compute summary descriptives:\itemize{
#' \item a.	total number of 0, 1, 2 by coder
#' \item b.	total number of discrepancies
#' \item c.	total number of each combination of discrepancies
#' }
#' 6.	repeat above steps for each identified ra file\cr
#' 7.	for recode runs, the discrepancy summary and excel file will be based on second-round values only (i.e., only checking discrepancy in the recode file)\cr
#'
#' @examples
#' \strong{no recode has happened yet (use original jc coding):}
#' titlescreen1(path="/Users/phoebelam/Google Drive/2020/4_meta_els sleep & inflammation/1_search_els & sleep/reliability",
#' jc.filename = "0_merged els & sleep_11.1.19_jc.xlsx",
#' jc.sheetnumber = 2,
#' pattern="1_merged els & sleep_11.1.19_ra")
#'
#' \strong{recode has happened (use jc second round coding):}
#' titlescreen1(path="/Users/phoebelam/Google Drive/2020/4_meta_els sleep & inflammation/1_search_els & sleep/reliability",
#' jc.filename = "0_merged els & sleep_11.1.19_jc.xlsx",
#' jc.sheetnumber = 2,
#' pattern="1_merged els & sleep_11.1.19_ra",
#' recode=T,
#' recode.ra = "pk")
#'
#'
#' @importFrom magrittr "%>%"
#' @export
titlescreen1 <- function(path,
                         jc.filename,
                         jc.sheetnumber,
                         pattern,
                         recode = F,
                         recode.ra = NULL) {
  # jc <- read.xlsx("0_merged els & sleep_11.1.19_jc.xlsx", sheetName = "pubmed_result (2)")
  # ra <- read.xlsx("reliability_ra screening/0_merged els & sleep_11.1.19_ra_kjk.xls", sheetIndex =1)

  print (sample(imonit, 1))
  Sys.sleep(5)

  setwd(path)

  jc <- xlsx::read.xlsx(jc.filename, sheetIndex = jc.sheetnumber)

  #clean jc sheet
  jc %>%
    dplyr::rename(jc = include...1.unsure...2) %>%
    dplyr::filter (duplicate != "duplicate") %>%
    dplyr::select(jc, Title) %>%
    dplyr::filter (is.na(Title)==F & Title != "Title") %>%
    dplyr::mutate (jc = dplyr::case_when(is.na(jc)==T~ 0,
                           TRUE~jc),
            rownumber = dplyr::row_number()+1) -> jc

  if (recode == T) {
    jcrecode = list.files(path = path, pattern = "_recode", full.names = T, recursive = T)
    recodedf <- xlsx::read.xlsx(jcrecode, sheetIndex = 1)

    #clean jc recoded sheet
    recodedf %>%
      dplyr::rename (jc_1st_round = jc) %>%
      dplyr::mutate (jc = dplyr::case_when(is.na(consensus)==F~ consensus,
                             is.na(jc_2nd_round)==F~ jc_2nd_round,
                             TRUE~ jc_1st_round)) %>%
      dplyr::select (jc, Title, rownumber) -> jcrecode

    jc %>%
      dplyr::filter (!rownumber %in% jcrecode$rownumber) -> jcnorecode

    rbind(jcnorecode, jcrecode) %>%
      dplyr::arrange (rownumber) -> jc

    #write out this jc file as an updated jc codes
    jc1 <- xlsx::read.xlsx(jc.filename, sheetIndex = jc.sheetnumber)
    jc1 %>%
      dplyr::rename (jc_1st_round = include...1.unsure...2) %>%
      dplyr::select(search.phase:Properties) %>%
      dplyr::mutate (rownumber = dplyr::row_number()+1,
              jc_1st_round = dplyr::case_when(is.na(jc_1st_round)==T~ 0,
                                       TRUE~jc_1st_round))-> jc1

    jc %>%
      dplyr::rename(jc_2nd_round = jc) %>%
      dplyr::select(-Title)-> update

    merge(jc1, update, by = "rownumber") %>%
      dplyr::select(search.phase, jc_1st_round, jc_2nd_round, Title:Properties)-> update

    stringr::str_locate_all(pattern ='_', jc.filename)[[1]][2, 1] -> loc
    paste(stringr::str_sub(jc.filename,1,loc-1), "_jc_updated_", sep="") -> new
    xlsx::write.xlsx(update, paste(new, format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)


    #special treatment for the particular ra with whom jc did recode
    #remind jc not to have missing for the recode ra or if she okay with following assumption:
    #empty pk 2nd round is due to discussion
    #and going with jc so not penalizing her for discrepancies in those cells

    #detect and rename colnames base on ra's name in input to generic "ra"
    gsub(recode.ra, "ra", colnames(recodedf)) -> colnames (recodedf)

    recodedf %>%
      dplyr::rename (jc_1st_round = jc,
              ra_1st_round = ra) %>%
      dplyr::mutate (jc = dplyr::case_when(is.na(consensus)==F~ consensus,
                             is.na(jc_2nd_round)==F~ jc_2nd_round,
                             TRUE~ jc_1st_round),
                     ra = dplyr::case_when(is.na(consensus)==F~ consensus,
                             is.na(ra_2nd_round)==F~ra_2nd_round,
                             TRUE~ jc_1st_round)) -> recodedf
    recodedf %>%
      dplyr::filter(jc != ra) %>%
      dplyr::select (rownumber, Title, jc, ra) %>%
      dplyr::arrange(jc, ra)-> trim

    gsub("ra", recode.ra, colnames(trim)) -> colnames(trim)

    xlsx::write.xlsx(trim, paste("1_jc recode_", recode.ra, " recode_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)

    #summary sheet
    # general, number of 0, 1, and 2 for both
    # % and number discrepant discrepant between 0 vs. 1/2, 1 vs 2, and 0 vs. 2
    janitor::tabyl (recodedf$jc) -> sumjc
    janitor::tabyl (recodedf$ra) -> sumra
    nrow(recodedf) -> total
    recodedf %>%
      dplyr::filter (jc!=ra) %>% nrow() %>% paste(., "of discrepancies out of", total, "recoded entries", sep =" ") -> discreptot
    recodedf %>%
      dplyr::filter(jc == 0 & ra == 1) %>% nrow() %>% paste(., "where jc = 0, ra = 1", sep =" ") -> jc0ra1
    recodedf %>%
      dplyr::filter(jc == 0 & ra == 2) %>% nrow() %>% paste(., "where jc = 0, ra = 2", sep =" ") -> jc0ra2
    recodedf %>%
      dplyr::filter(jc == 1 & ra == 0) %>% nrow() %>% paste(., "where jc = 1, ra = 0", sep =" ") -> jc1ra0
    recodedf %>%
      dplyr::filter(jc == 1 & ra == 2) %>% nrow() %>% paste(., "where jc = 1, ra = 2", sep =" ") -> jc1ra2
    recodedf %>%
      dplyr::filter(jc == 2 & ra == 0) %>% nrow() %>% paste(., "where jc = 2, ra = 0", sep =" ") -> jc2ra0
    recodedf %>%
      dplyr::filter(jc == 2 & ra == 1) %>% nrow() %>% paste(., "where jc = 2, ra = 1", sep =" ") -> jc2ra1

    list (sumjc, sumra, discreptot, jc0ra1, jc0ra2, jc1ra0, jc1ra2, jc2ra0, jc2ra1) -> summary
    names(summary) <- c("number/% of 0,1,2 for jc", "number/% of 0,1,2 for ra", "number of discrepancies",
                        "jc0 ra1", "jc0 ra2", "jc1 ra0", "jc1 ra2", "jc2 ra0", "jc2 ra1")

    capture.output(summary, file = paste("1_jc recode_", recode.ra, " recode_summary_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
  }


  filenames = intersect(list.files(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE),
                        list.files(path = path, pattern = ".xlsx", full.names = TRUE, recursive = TRUE))

  if (recode == T) {
    filenames <- filenames[!grepl(recode.ra, filenames, ignore.case=T)]
  }

  for (f in filenames) {
    print (f)

    #clean ra sheet
    ra <- xlsx::read.xlsx(f, sheetIndex =1)
    ra %>%
      dplyr::filter (duplicate != "duplicate") %>%
      dplyr::rename (ra = X1...include.2...unsure) %>%
      dplyr::select (ra, Title) %>%
      dplyr::filter (is.na(Title)==F & Title != "Title") %>%
      dplyr::mutate (ra = dplyr::case_when(is.na(ra)==T~ 0,
                             TRUE~as.numeric(as.character(ra)))) -> ra

    #merge
    merge (jc, ra, by = "Title", all = TRUE) -> merge

    #discrepancy sheet
    merge %>%
      dplyr::filter (jc!=ra) %>%
      dplyr::select (rownumber, Title, jc, ra) %>%
      dplyr::arrange (jc, ra) -> trim

    #grab initials
    basename(f) %>%
      stringr::str_sub(.,-8,-6) %>%
      gsub("_", "", .) -> initial
    colnames(trim) <- c("rownumber", "Title", "jc", initial)

    View (trim)

    if (recode == F){
      xlsx::write.xlsx(trim, paste("1_jc_", initial, "_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    } else {
      xlsx::write.xlsx(trim, paste("1_jc recode_", initial, "_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    }

    #summary sheet
    # general, number of 0, 1, and 2 for both
    # % and number discrepant discrepant between 0 vs. 1/2, 1 vs 2, and 0 vs. 2
    janitor::tabyl (merge$jc) -> sumjc
    janitor::tabyl (merge$ra) -> sumra
    nrow(merge) -> total
    merge %>%
      dplyr::filter (jc!=ra) %>% nrow() %>% paste(., "of discrepancies out of", total, sep =" ") -> discreptot
    merge %>%
      dplyr::filter(jc == 0 & ra == 1) %>% nrow() %>% paste(., "where jc = 0, ra = 1", sep =" ") -> jc0ra1
    merge %>%
      dplyr::filter(jc == 0 & ra == 2) %>% nrow() %>% paste(., "where jc = 0, ra = 2", sep =" ") -> jc0ra2
    merge %>%
      dplyr::filter(jc == 1 & ra == 0) %>% nrow() %>% paste(., "where jc = 1, ra = 0", sep =" ") -> jc1ra0
    merge %>%
      dplyr::filter(jc == 1 & ra == 2) %>% nrow() %>% paste(., "where jc = 1, ra = 2", sep =" ") -> jc1ra2
    merge %>%
      dplyr::filter(jc == 2 & ra == 0) %>% nrow() %>% paste(., "where jc = 2, ra = 0", sep =" ") -> jc2ra0
    merge %>%
      dplyr::filter(jc == 2 & ra == 1) %>% nrow() %>% paste(., "where jc = 2, ra = 1", sep =" ") -> jc2ra1

    list (sumjc, sumra, discreptot, jc0ra1, jc0ra2, jc1ra0, jc1ra2, jc2ra0, jc2ra1) -> summary
    names(summary) <- c("number/% of 0,1,2 for jc", "number/% of 0,1,2 for ra", "number of discrepancies",
                        "jc0 ra1", "jc0 ra2", "jc1 ra0", "jc1 ra2", "jc2 ra0", "jc2 ra1")

    if (recode == F){
      capture.output(summary, file = paste("1_jc_", initial, "_summary_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
    } else {
      capture.output(summary, file = paste("1_jc recode_", initial, "_summary_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
    }
  }

  print (sample(donzo, 1))
}

