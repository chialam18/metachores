#' @export
titlescreen1 <- function(path,
                         jc.filename,
                         jc.sheetnumber,
                         pattern,
                         recode = F,
                         recode.ra = NULL) {
  # jc <- read.xlsx("0_merged els & sleep_11.1.19_jc.xlsx", sheetName = "pubmed_result (2)")
  # ra <- read.xlsx("reliability_ra screening/0_merged els & sleep_11.1.19_ra_kjk.xls", sheetIndex =1)
  
  library (dplyr)
  library (xlsx)
  library (janitor)
  library (stringr)
  library (psych)
  
  setwd(path)
  
  jc <- read.xlsx(jc.filename, sheetIndex = jc.sheetnumber)
  
  #clean jc sheet
  jc %>%
    rename(jc = include...1.unsure...2) %>%
    filter (duplicate != "duplicate") %>%
    select(jc, Title) %>%
    filter (is.na(Title)==F & Title != "Title") %>%
    mutate (jc = case_when(is.na(jc)==T~ 0,
                           TRUE~jc),
            rownumber = row_number()+1) -> jc
  
  if (recode == T) {
    jcrecode = list.files(path = path, pattern = "_recode", full.names = T, recursive = T)
    recodedf <- read.xlsx(jcrecode, sheetIndex = 1)
    
    #clean jc recoded sheet
    recodedf %>%
      rename (jc_1st_round = jc) %>%
      mutate (jc = case_when(is.na(consensus)==F~ consensus, 
                             is.na(jc_2nd_round)==F~ jc_2nd_round,
                             TRUE~ jc_1st_round)) %>%
      select (jc, Title, rownumber) -> jcrecode
    
    jc %>%
      filter (!rownumber %in% jcrecode$rownumber) -> jcnorecode
    
    rbind(jcnorecode, jcrecode) %>% 
      arrange (rownumber) -> jc
    
    #write out this jc file as an updated jc codes
    jc1 <- read.xlsx(jc.filename, sheetIndex = jc.sheetnumber)
    jc1 %>%
      rename (jc_1st_round = include...1.unsure...2) %>%
      select(search.phase:Properties) %>%
      mutate (rownumber = row_number()+1,
              jc_1st_round = case_when(is.na(jc_1st_round)==T~ 0,
                                       TRUE~jc_1st_round))-> jc1
    
    jc %>%
      rename(jc_2nd_round = jc) %>%
      select(-Title)-> update
    
    merge(jc1, update, by = "rownumber") %>%
      select(search.phase, jc_1st_round, jc_2nd_round, Title:Properties)-> update
    
    str_locate_all(pattern ='_', jc.filename)[[1]][2, 1] -> loc
    paste(str_sub(jc.filename,1,loc-1), "_jc_updated_", sep="") -> new
    write.xlsx(update, paste(new, format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    
    
    #special treatment for the particular ra with whom jc did recode
    #remind jc not to have missing for the recode ra or if she okay with following assumption:
    #empty pk 2nd round is due to discussion 
    #and going with jc so not penalizing her for discrepancies in those cells
    
    #detect and rename colnames base on ra's name in input to generic "ra"
    gsub(recode.ra, "ra", colnames(recodedf)) -> colnames (recodedf)
    
    recodedf %>%
      rename (jc_1st_round = jc,
              ra_1st_round = ra) %>%
      mutate (jc = case_when(is.na(consensus)==F~ consensus, 
                             is.na(jc_2nd_round)==F~ jc_2nd_round,
                             TRUE~ jc_1st_round),
              ra = case_when(is.na(consensus)==F~ consensus,
                             is.na(ra_2nd_round)==F~ra_2nd_round,
                             TRUE~ jc_1st_round)) -> recodedf
    recodedf %>%
      filter(jc != ra) %>%
      select (rownumber, Title, jc, ra) %>%
      arrange(jc, ra)-> trim
    
    gsub("ra", recode.ra, colnames(trim)) -> colnames(trim)
    
    write.xlsx(trim, paste("1_jc recode_", recode.ra, " recode_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    
    #summary sheet
    # general, number of 0, 1, and 2 for both
    # % and number discrepant discrepant between 0 vs. 1/2, 1 vs 2, and 0 vs. 2
    tabyl (recodedf$jc) -> sumjc
    tabyl (recodedf$ra) -> sumra
    nrow(recodedf) -> total
    recodedf %>%
      filter (jc!=ra) %>% nrow() %>% paste(., "of discrepancies out of", total, "recoded entries", sep =" ") -> discreptot
    recodedf %>%
      filter(jc == 0 & ra == 1) %>% nrow() %>% paste(., "where jc = 0, ra = 1", sep =" ") -> jc0ra1
    recodedf %>%
      filter(jc == 0 & ra == 2) %>% nrow() %>% paste(., "where jc = 0, ra = 2", sep =" ") -> jc0ra2
    recodedf %>%
      filter(jc == 1 & ra == 0) %>% nrow() %>% paste(., "where jc = 1, ra = 0", sep =" ") -> jc1ra0
    recodedf %>%
      filter(jc == 1 & ra == 2) %>% nrow() %>% paste(., "where jc = 1, ra = 2", sep =" ") -> jc1ra2
    recodedf %>%
      filter(jc == 2 & ra == 0) %>% nrow() %>% paste(., "where jc = 2, ra = 0", sep =" ") -> jc2ra0
    recodedf %>%
      filter(jc == 2 & ra == 1) %>% nrow() %>% paste(., "where jc = 2, ra = 1", sep =" ") -> jc2ra1
    
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
    ra <- read.xlsx(f, sheetIndex =1)
    ra %>% filter (duplicate != "duplicate") %>%
      rename (ra = X1...include.2...unsure) %>%
      select (ra, Title) %>%
      filter (is.na(Title)==F & Title != "Title") %>%
      mutate (ra = case_when(is.na(ra)==T~ 0,
                             TRUE~as.numeric(as.character(ra)))) -> ra
    
    #merge
    merge (jc, ra, by = "Title", all = TRUE) -> merge
    
    #discrepancy sheet
    merge %>%
      filter (jc!=ra) %>%
      select (rownumber, Title, jc, ra) %>%
      arrange (jc, ra) -> trim
    
    #grab initials
    basename(f) %>%
      str_sub(.,-8,-6) %>%
      gsub("_", "", .) -> initial
    colnames(trim) <- c("rownumber", "Title", "jc", initial)
    
    View (trim)
    
    if (recode == F){
      write.xlsx(trim, paste("1_jc_", initial, "_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    } else {
      write.xlsx(trim, paste("1_jc recode_", initial, "_discrep_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""), row.names=F)
    }
    
    #summary sheet
    # general, number of 0, 1, and 2 for both
    # % and number discrepant discrepant between 0 vs. 1/2, 1 vs 2, and 0 vs. 2
    tabyl (merge$jc) -> sumjc
    tabyl (merge$ra) -> sumra
    nrow(merge) -> total
    merge %>%
      filter (jc!=ra) %>% nrow() %>% paste(., "of discrepancies out of", total, sep =" ") -> discreptot
    merge %>%
      filter(jc == 0 & ra == 1) %>% nrow() %>% paste(., "where jc = 0, ra = 1", sep =" ") -> jc0ra1
    merge %>%
      filter(jc == 0 & ra == 2) %>% nrow() %>% paste(., "where jc = 0, ra = 2", sep =" ") -> jc0ra2
    merge %>%
      filter(jc == 1 & ra == 0) %>% nrow() %>% paste(., "where jc = 1, ra = 0", sep =" ") -> jc1ra0
    merge %>%
      filter(jc == 1 & ra == 2) %>% nrow() %>% paste(., "where jc = 1, ra = 2", sep =" ") -> jc1ra2
    merge %>%
      filter(jc == 2 & ra == 0) %>% nrow() %>% paste(., "where jc = 2, ra = 0", sep =" ") -> jc2ra0
    merge %>%
      filter(jc == 2 & ra == 1) %>% nrow() %>% paste(., "where jc = 2, ra = 1", sep =" ") -> jc2ra1
    
    list (sumjc, sumra, discreptot, jc0ra1, jc0ra2, jc1ra0, jc1ra2, jc2ra0, jc2ra1) -> summary
    names(summary) <- c("number/% of 0,1,2 for jc", "number/% of 0,1,2 for ra", "number of discrepancies", 
                        "jc0 ra1", "jc0 ra2", "jc1 ra0", "jc1 ra2", "jc2 ra0", "jc2 ra1")
    
    if (recode == F){
      capture.output(summary, file = paste("1_jc_", initial, "_summary_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
    } else {
      capture.output(summary, file = paste("1_jc recode_", initial, "_summary_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))
    }
  }
  
  print("donzo!")
}

