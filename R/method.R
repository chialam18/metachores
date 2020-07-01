

#' method discrepancies among coders
#'
#' merge together coder sheets\cr
#' identify and only keep discrepant rows \cr
#' compute summary descriptives of # discrepancies
#'
#' @param path the folder where each pubmed .csv export files are stored
#' @param newfilename the name of the two new consolidated excel files without the extension
#'
#' @examples
#' mergeclean(path="/Users/phoebelam/Google Drive/2020/4_meta_els sleep & inflammation/1_search_els & sleep",
#' newfilename = "0_merged els & sleep")
#'
#' @importFrom magrittr "%>%"
#' @export
method <- function (path) {
  print (sample(imonit, 1))
  Sys.sleep(5)

  filenames = list.files(path = path, pattern = ".csv", full.names = TRUE, recursive = FALSE)
  gsub(path, "", filenames) %>%
    gsub("/method extraction_| - method.csv", "", .) %>%
    gsub("/method extraction", "jp", .) -> dfnames

  for (i in 1:length(filenames)) assign(dfnames[i], read.csv(filenames[i], na.strings=""))

  # if wide format
  #rename variable names
  jp %>%
    rename_at(vars(!link.id), ~paste(., "_jp", sep="")) %>%
    filter(is.na(link.id)==F) -> jp
  kk %>%
    rename_at(vars(!link.id), ~paste(., "_kk", sep="")) %>%
    filter(is.na(link.id)==F)-> kk
  ma %>%
    rename_at(vars(!link.id), ~paste(., "_ma", sep="")) %>%
    filter(is.na(link.id)==F)-> ma

  #merge
  dat <- merge(merge(jp, kk, by = "link.id"), ma, by = "link.id")

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

  #discrepancies
  dat %>%
    mutate_at(vars(matches("assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
              ~as.character(.)) %>%
    mutate_at(vars(matches("assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
              ~case_when(is.na(.)==T~ "missing", TRUE~.))-> dat

  dat %>%
    filter(is.na(sample.notes_jp)==F & is.na(sample.notes_kk)==T |
             is.na(sample.notes_jp)==F & is.na(sample.notes_ma)==T) %>%
    select(link.id, sample.notes_jp, sample.notes_kk, sample.notes_ma) -> samplenotes

  dat %>%
    filter(iv.assessment_jp != iv.assessment_kk |
             iv.assessment_jp != iv.assessment_ma) %>%
    select (link.id, iv.assessment_jp, iv.assessment_kk, iv.assessment_ma) -> iv.ass

  dat %>%
    filter(iv.timeframe_jp != iv.timeframe_kk |
             iv.timeframe_jp != iv.timeframe_ma) %>%
    select (link.id, iv.timeframe_jp, iv.timeframe_kk, iv.timeframe_ma) -> iv.time

  dat %>%
    filter(iv.mean.age_jp != iv.mean.age_kk |
             iv.mean.age_jp != iv.mean.age_ma) %>%
    select (link.id, iv.mean.age_jp, iv.mean.age_kk, iv.mean.age_ma) -> iv.agemean

  dat %>%
    filter(iv.age.sd_jp != iv.age.sd_kk |
             iv.age.sd_jp != iv.age.sd_ma) %>%
    select (link.id, iv.age.sd_jp, iv.age.sd_kk, iv.age.sd_ma) -> iv.agesd

  dat %>%
    filter(iv.age.range_jp != iv.age.range_kk |
             iv.age.range_jp != iv.age.range_ma) %>%
    select (link.id, iv.age.range_jp, iv.age.range_kk, iv.age.range_ma) -> iv.agerange

  dat %>%
    filter(dv.assessment_jp != dv.assessment_kk |
             dv.assessment_jp != dv.assessment_ma) %>%
    select (link.id, dv.assessment_jp, dv.assessment_kk, dv.assessment_ma) -> dv.ass

  dat %>%
    filter(dv.timeframe_jp != dv.timeframe_kk |
             dv.timeframe_jp != dv.timeframe_ma) %>%
    select (link.id, dv.timeframe_jp, dv.timeframe_kk, dv.timeframe_ma) -> dv.time

  dat %>%
    filter(dv.mean.age_jp != dv.mean.age_kk |
             dv.mean.age_jp != dv.mean.age_ma) %>%
    select (link.id, dv.mean.age_jp, dv.mean.age_kk, dv.mean.age_ma) -> dv.agemean

  dat %>%
    filter(dv.age.sd_jp != dv.age.sd_kk |
             dv.age.sd_jp != dv.age.sd_ma) %>%
    select (link.id, dv.age.sd_jp, dv.age.sd_kk, dv.age.sd_ma) -> dv.agesd

  dat %>%
    filter(dv.age.range_jp != dv.age.range_kk |
             dv.age.range_jp != dv.age.range_ma) %>%
    select (link.id, dv.age.range_jp, dv.age.range_kk, dv.age.range_ma) -> dv.agerange

  dat %>%
    filter(design_jp != design_kk |
             design_jp != design_ma) %>%
    select (link.id, design_jp, design_kk, design_ma) -> design

  dat %>%
    filter(female.._jp != female.._kk |
             female.._jp != female.._ma) %>%
    select (link.id, female.._jp, female.._kk, female.._ma) -> female

  dat %>%
    filter(white.._jp != white.._kk |
             white.._jp != white.._ma) %>%
    select (link.id, white.._jp, white.._kk, white.._ma) -> white

  dat %>%
    filter(black.._jp != black.._kk |
             black.._jp != black.._ma) %>%
    select (link.id, black.._jp, black.._kk, black.._ma) -> black

  dat %>%
    filter(latino.._jp != latino.._kk |
             latino.._jp != latino.._ma) %>%
    select (link.id, latino.._jp, latino.._kk, latino.._ma) -> latino

  dat %>%
    filter(asian.._jp != asian.._kk |
             asian.._jp != asian.._ma) %>%
    select (link.id, asian.._jp, asian.._kk, asian.._ma) -> asian

  dat %>%
    filter(other.._jp != other.._kk |
             other.._jp != other.._ma) %>%
    select (link.id, other.._jp, other.._kk, other.._ma) -> other

  dat %>%
    filter(psychiatric.._jp != psychiatric.._kk |
             psychiatric.._jp != psychiatric.._ma) %>%
    select (link.id, psychiatric.._jp, psychiatric.._kk, psychiatric.._ma) -> psychiatric

  dat %>%
    filter(physical.._jp != physical.._kk |
             physical.._jp != physical.._ma) %>%
    select (link.id, physical.._jp, physical.._kk, physical.._ma) -> physical

  dat %>%
    filter(subgroup.n.a_jp != subgroup.n.a_kk |
             subgroup.n.a_jp != subgroup.n.a_ma) %>%
    select (link.id, subgroup.n.a_jp, subgroup.n.a_kk, subgroup.n.a_ma) -> subgroupna

  dat %>%
    filter(country_jp != country_kk |
             country_jp != country_ma) %>%
    select (link.id, country_jp, country_kk, country_ma) -> country

  dat %>%
    filter(adiposity_jp != adiposity_kk |
             adiposity_jp != adiposity_ma) %>%
    select (link.id, adiposity_jp, adiposity_kk, adiposity_ma) -> adiposity

  list (samplenotes, iv.ass, iv.time, iv.agemean, iv.agesd, iv.agerange, dv.ass, dv.time, dv.agemean, dv.agesd, dv.agerange,
        design, female, white, black, latino, asian, other, psychiatric, physical, subgroupna, country, adiposity) -> summary

  capture.output(summary, file = paste(path, "/method discrepancies_", format(Sys.Date(), format="%m.%d.%y"), ".txt", sep=""))

}

