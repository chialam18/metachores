

#' method discrepancies among coders
#'
#' merge together coder sheets\cr
#' identify and only keep discrepant rows \cr
#'
#' @param path the folder where each pubmed .csv export files are stored
#' @param start the link.id to begin checking, put in quotes
#' @param end the link.id to end checking, put in quotes
#'
#' @examples
#' method(path="/Users/phoebelam/Google Drive/2020/4_meta_els sleep & inflammation/1_search_els & sleep", start = "a21_1", end = "a30b_1")
#'
#' @importFrom magrittr "%>%"
#' @export
method <- function (path, start, end) {


  print (sample(imonit, 1))
  Sys.sleep(5)

  filenames = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = FALSE)
  gsub(path, "", filenames) %>%
    gsub("/method extraction_| - method.csv", "", .) %>%
    gsub("/method extraction", "jp", .) -> dfnames

  print(filenames)

  for (i in 1:length(filenames)) assign(dfnames[i], read.csv(filenames[i], na.strings=""))

  # if wide format
  #rename variable names
  jp %>%
    dplyr::rename_at(dplyr::vars(!link.id), ~paste(., "_jp", sep="")) %>%
    dplyr::filter(is.na(link.id)==F) -> jp
  kk %>%
    dplyr::rename_at(dplyr::vars(!link.id), ~paste(., "_kk", sep="")) %>%
    dplyr::filter(is.na(link.id)==F)-> kk
  ma %>%
    dplyr::rename_at(dplyr::vars(!link.id), ~paste(., "_ma", sep="")) %>%
    dplyr::filter(is.na(link.id)==F)-> ma

  #merge
  dat <- merge(merge(jp, kk, by = "link.id"), ma, by = "link.id")

  which(dat$link.id == start) -> startrow
  which(dat$link.id == end) -> endrow

  dat[startrow:endrow, ] -> dat

  # # if long format
  # filenames = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = FALSE)
  # all = lapply(filenames, read.csv, na.strings="")
  #
  # all <- lapply(all, function(x) filter(x, is.na(link.id)==F))
  #
  # dat <- dplyr::bind_rows(all)
  #
  # all %>%
  #   filter (is.na(link.id)==F)

  # remove excluded links
  dat %>% dplyr::filter (exclude_jp == 0 & exclude_ma == 0 & exclude_kk == 0) -> dat

  #discrepancies
  dat %>%
    dplyr::mutate_at(dplyr::vars(matches("sample.notes|assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
              ~as.character(.)) %>%
    dplyr::mutate_at(dplyr::vars(matches("assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
              ~dplyr::case_when(is.na(.)==T~ "missing", TRUE~.))-> dat

  dat %>%
    dplyr::filter(is.na(sample.notes_jp)==F & is.na(sample.notes_kk)==T |
             is.na(sample.notes_jp)==F & is.na(sample.notes_ma)==T) %>%
    dplyr::select(link.id, sample.notes_jp, sample.notes_kk, sample.notes_ma) %>%
    dplyr::mutate_at(dplyr::vars(sample.notes_jp:sample.notes_ma),
                     ~dplyr::case_when(is.na(.)==T~"missing",
                                       TRUE~.)) -> samplenotes

  dat %>%
    dplyr::filter(iv.assessment_jp != iv.assessment_kk |
             iv.assessment_jp != iv.assessment_ma) %>%
    dplyr::select (link.id, iv.assessment_jp, iv.assessment_kk, iv.assessment_ma) -> iv.ass

  dat %>%
    dplyr::filter(iv.timeframe_jp != iv.timeframe_kk |
             iv.timeframe_jp != iv.timeframe_ma) %>%
    dplyr::select (link.id, iv.timeframe_jp, iv.timeframe_kk, iv.timeframe_ma) -> iv.time

  dat %>%
    dplyr::filter(iv.mean.age_jp != iv.mean.age_kk |
             iv.mean.age_jp != iv.mean.age_ma) %>%
    dplyr::select (link.id, iv.mean.age_jp, iv.mean.age_kk, iv.mean.age_ma) -> iv.agemean

  dat %>%
    dplyr::filter(iv.age.sd_jp != iv.age.sd_kk |
             iv.age.sd_jp != iv.age.sd_ma) %>%
    dplyr::select (link.id, iv.age.sd_jp, iv.age.sd_kk, iv.age.sd_ma) -> iv.agesd

  dat %>%
    dplyr::filter(iv.age.range_jp != iv.age.range_kk |
             iv.age.range_jp != iv.age.range_ma) %>%
    dplyr::select (link.id, iv.age.range_jp, iv.age.range_kk, iv.age.range_ma) -> iv.agerange

  dat %>%
    dplyr::filter(dv.assessment_jp != dv.assessment_kk |
             dv.assessment_jp != dv.assessment_ma) %>%
    dplyr::select (link.id, dv.assessment_jp, dv.assessment_kk, dv.assessment_ma) -> dv.ass

  dat %>%
    dplyr::filter(dv.timeframe_jp != dv.timeframe_kk |
             dv.timeframe_jp != dv.timeframe_ma) %>%
    dplyr::select (link.id, dv.timeframe_jp, dv.timeframe_kk, dv.timeframe_ma) -> dv.time

  dat %>%
    dplyr::filter(dv.mean.age_jp != dv.mean.age_kk |
             dv.mean.age_jp != dv.mean.age_ma) %>%
    dplyr::select (link.id, dv.mean.age_jp, dv.mean.age_kk, dv.mean.age_ma) -> dv.agemean

  dat %>%
    dplyr::filter(dv.age.sd_jp != dv.age.sd_kk |
             dv.age.sd_jp != dv.age.sd_ma) %>%
    dplyr::select (link.id, dv.age.sd_jp, dv.age.sd_kk, dv.age.sd_ma) -> dv.agesd

  dat %>%
    dplyr::filter(dv.age.range_jp != dv.age.range_kk |
             dv.age.range_jp != dv.age.range_ma) %>%
    dplyr::select (link.id, dv.age.range_jp, dv.age.range_kk, dv.age.range_ma) -> dv.agerange

  dat %>%
    dplyr::filter(design_jp != design_kk |
             design_jp != design_ma) %>%
    dplyr::select (link.id, design_jp, design_kk, design_ma) -> design

  dat %>%
    dplyr::filter(female.._jp != female.._kk |
             female.._jp != female.._ma) %>%
    dplyr::select (link.id, female.._jp, female.._kk, female.._ma) -> female

  dat %>%
    dplyr::filter(white.._jp != white.._kk |
             white.._jp != white.._ma) %>%
    dplyr::select (link.id, white.._jp, white.._kk, white.._ma) -> white

  dat %>%
    dplyr::filter(black.._jp != black.._kk |
             black.._jp != black.._ma) %>%
    dplyr::select (link.id, black.._jp, black.._kk, black.._ma) -> black

  dat %>%
    dplyr::filter(latino.._jp != latino.._kk |
             latino.._jp != latino.._ma) %>%
    dplyr::select (link.id, latino.._jp, latino.._kk, latino.._ma) -> latino

  dat %>%
    dplyr::filter(asian.._jp != asian.._kk |
             asian.._jp != asian.._ma) %>%
    dplyr::select (link.id, asian.._jp, asian.._kk, asian.._ma) -> asian

  dat %>%
    dplyr::filter(other.._jp != other.._kk |
             other.._jp != other.._ma) %>%
    dplyr::select (link.id, other.._jp, other.._kk, other.._ma) -> other

  dat %>%
    dplyr::filter(psychiatric.._jp != psychiatric.._kk |
             psychiatric.._jp != psychiatric.._ma) %>%
    dplyr::select (link.id, psychiatric.._jp, psychiatric.._kk, psychiatric.._ma) -> psychiatric

  dat %>%
    dplyr::filter(physical.._jp != physical.._kk |
             physical.._jp != physical.._ma) %>%
    dplyr::select (link.id, physical.._jp, physical.._kk, physical.._ma) -> physical

  dat %>%
    dplyr::filter(subgroup.n.a_jp != subgroup.n.a_kk |
             subgroup.n.a_jp != subgroup.n.a_ma) %>%
    dplyr::select (link.id, subgroup.n.a_jp, subgroup.n.a_kk, subgroup.n.a_ma) -> subgroupna

  dat %>%
    dplyr::filter(country_jp != country_kk |
             country_jp != country_ma) %>%
    dplyr::select (link.id, country_jp, country_kk, country_ma) -> country

  dat %>%
    dplyr::filter(adiposity_jp != adiposity_kk |
             adiposity_jp != adiposity_ma) %>%
    dplyr::select (link.id, adiposity_jp, adiposity_kk, adiposity_ma) -> adiposity

  list ("sample notes" = samplenotes,
        "iv.assessment"= iv.ass,
        "iv.timeframe"= iv.time,
        "iv.agemean" = iv.agemean,
        "iv.agesd" = iv.agesd,
        "iv.agerange" = iv.agerange,
        "dv.assessment"= dv.ass,
        "dv.timeframe"= dv.time,
        "dv.agemean" = dv.agemean,
        "dv.agesd" = dv.agesd,
        "dv.agerange" = dv.agerange,
        "design" = design,
        "female" = female,
        "white" = white,
        "black" = black,
        "latino"= latino,
        "asian"= asian,
        "other" = other,
        "psychiatric" = psychiatric,
        "physical"= physical,
        "subgroupna" = subgroupna,
        "country" = country,
        "adiposity" = adiposity) -> summary

  openxlsx::write.xlsx(summary, file = paste(path, "/method discrepancies_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""))

  # capture.output(summary, file = paste(path, "/method discrepancies_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))

  print (sample(donzo, 1))

}










