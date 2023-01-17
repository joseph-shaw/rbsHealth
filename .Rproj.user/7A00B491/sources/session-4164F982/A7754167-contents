#' @title Push Bloods to Server
#' @description This function takes a file path and a logical value indicating whether the data should be pushed to a server. It extracts blood test results from HTML files in the specified directory and organizes the data into a data frame.
#' @param file.path character. The path to the directory containing the blood test results.
#' @param push_to_sb logical. Indicates whether the data should be pushed to Smartabase. Default is TRUE.
#' @return A data frame containing the extracted blood test results.
#' @import tidyverse, textreadr
#' @export



#all <- push_bloods_to_sb("C:/Users/shaw_/Documents/R/Royal Ballet School/data/bloods")


push_bloods_to_sb <- function(file.path = "S:/Physio/Blood results/Pending Upload", push_to_sb = TRUE){

  library(tidyverse)
  library(textreadr)

  personal_ids <- neon::pull_smartabase(
    form = "Personal Details",
    start_date = "01/01/2000",
    end_date = "01/01/2050"
  ) %>%
    mutate(
      about = tolower(about)
    ) %>%
    select(about, user_id) %>%
    distinct()

  files <- list.files(file.path, full.names = T, include.dirs = F, recursive = T, pattern = "html")

  markers <- c(
    "HAEMOGLOBIN (g/L)"    , "HCT"                      , "RED CELL COUNT"        , "MCV"                         ,
    "MCH"                  , "MCHC (g/L)"               , "RDW"                   , "PLATELET COUNT"              ,
    "MPV"                  , "WHITE CELL COUNT"         , "Neutrophils"           , "Lymphocytes"                 ,
    "Monocytes"            , "Eosinophils"              , "Basophils"             , "THYROID STIMULATING HORMONE" ,
    "FREE THYROXINE"       , "FREE T3"                  , "FOLLICLE STIM. HORMONE", "LUTEINISING HORMONE"         ,
    "PROGESTERONE"         , "TESTOSTERONE"             , "PROLACTIN"             , "17-Beta OESTRADIOL"          ,
    "ESR"                  , "Folate (serum)"           , "SODIUM"                , "POTASSIUM"                   ,
    "CHLORIDE"             , "BICARBONATE"              , "UREA"                  , "CREATININE"                  ,
    "estimated GFR"        , "BILIRUBIN"                , "ALKALINE PHOSPHATASE"  , "ASPARTATE TRANSFERASE"       ,
    "ALANINE TRANSFERASE"  , "GAMMA GT"                 , "TOTAL PROTEIN"         , "ALBUMIN"                     ,
    "GLOBULIN"             , "CALCIUM"                  , "Corrected Calcium"     , "MAGNESIUM"                   ,
    "PHOSPHATE"            , "FERRITIN"                 , "25 OH Vitamin D"       , "Intact-(Whole molecule)PTH"  ,
    "Active B12"           , "C Reactive protein"       , "LDH"                   , "CK"                          ,
    "URIC ACID"            , "RANDOM BLOOD GLUCOSE (FL)", "TRIGLYCERIDES"         , "CHOLESTEROL"                 ,
    "IRON"                 , "T.I.B.C"                  , "TRANSFERRIN SATURATION", "Rapid Test for Strep A"      ,
    "C. diphtheriae"       , "Appearance"               , "Salmonella (PCR)"      , "Shigella (PCR)"              ,
    "Campylobacter (PCR)"  , "VT E.coli (PCR)"          , "Giardia (OCP PCR)"     , "Cryptosporidium (OCP PCR)"   ,
    "Culture:"             , "Ear Culture"              , "HLA Q2-Q8 for Coeliac Disease", "TRIIODOTHYRONINE(T3):"

  )

  for(a in files){

    data <- textreadr::read_html(file = a, trim = T)

    date <- which(data == "Received:") + 1
    date <- data[date]

    start <- which(data %in% markers)
    if(length(start) == 0){
      print(a)
      next
    }

    details <- data[1:min(start)]
    data <- data[start[1]:length(data)]

    headers = data %in% markers

    data <- data.frame(
      data = data,
      headers = headers
    )

    variables <- data[data$headers,]
    variables <- variables[!is.na(variables$headers),1]

    bloods <- data.frame(
      name = rep(NA, 100),
      date = rep(NA, 100),
      variable = rep(NA, 100),
      value = rep(NA, 100),
      units = rep(NA, 100),
      normal = rep(NA, 100)
    )

    data <- data$data

    for(i in variables){
      j <- which(variables == i)
      index <- which(data == i)
      bloods$name[j] <- tolower(details[2])
      bloods$date[j] <- date
      bloods$variable[j] <- data[index]
      bloods$value[j] <- data[index+1]
      bloods$units[j] <- data[index+2]
      bloods$normal[j] <- data[index+3]
    }

    bloods <- bloods %>%
      mutate(
        normal = ifelse(grepl("-", units, ignore.case = T), units, normal),
        units = ifelse(grepl("-", units, ignore.case = T), "", units),
      ) %>%
      separate(normal, into = c("lower", "upper"), sep = " - ") %>%
      separate(date, into = c("date", "time"), sep = " ") %>%
      mutate(
        tdl_flag = grepl("[*]", value, ignore.case = T),
        #value = gsub(" ", "", value),
        value = gsub("[*]", "", value),
      ) %>%
      #mutate_at(c("value", "lower", "upper"), as.numeric) %>%
      filter(!is.na(name)) %>%
      mutate(value = ifelse(value %in% markers, NA, value)) %>%
      separate(value, sep = "%", into = c("value_1", "value_2")) %>%
      mutate(
        preceding_symbol_1 = ifelse(grepl(">", value_1), ">", NA),
        preceding_symbol_1 = ifelse(grepl("<", value_1), "<", preceding_symbol_1),
        preceding_symbol_2 = ifelse(grepl(">", value_2), ">", NA),
        preceding_symbol_2 = ifelse(grepl("<", value_2), "<", preceding_symbol_2),
        value_1 = gsub(">", "", value_1),
        value_1 = gsub("<", "", value_1),
        value_2 = gsub(">", "", value_2),
        value_2 = gsub("<", "", value_2),
        units = ifelse(units %in% markers, NA, units),
        lower = ifelse(lower %in% markers, NA, lower),
        upper = ifelse(upper %in% markers, NA, upper),
        lower = ifelse(grepl("Ref Range", lower, ignore.case = T), NA, lower),
        upper = ifelse(grepl("Ref Range", lower, ignore.case = T), NA, upper),
        lower = ifelse(grepl("For UK guidelines", lower, ignore.case = T), NA, lower),
        upper = ifelse(grepl("For UK guidelines", lower, ignore.case = T), NA, upper),
        units_1 = ifelse(is.na(value_2), units, "%"),
        units_2 = ifelse(is.na(value_2), NA, units),
        upper = ifelse(grepl("<", lower), lower, upper),
        lower = ifelse(grepl("<", lower), NA, lower),
        lower = gsub(">", "", lower),
        lower = gsub("<", "", lower),
        lower = as.numeric(lower),
        upper = gsub(">", "", upper),
        upper = gsub("<", "", upper),
        upper = as.numeric(upper),
        value_1 = as.numeric(value_1),
        value_2 = as.numeric(value_2),
        flag = case_when(
          value_1 > upper ~ "high",
          value_1 < lower ~ "low",
          value_1 >= lower & value_1 <= upper ~ "normal"
        ),
        flag = case_when(
          is.na(value_2) ~ flag,
          value_2 > upper ~ "high",
          value_2 < lower ~ "low",
          value_2 >= lower & value_2 <= upper ~ "normal"
        ),
        flag = ifelse(tdl_flag & is.na(flag), "flag", flag),
      )

    if(push_to_sb){

      bloods <- bloods %>%
        ungroup() %>%
        mutate(about = tolower(name)) %>%
        left_join(personal_ids, by = "about")
      neon::push_smartabase(
        bloods,
        form = "Blood Testing"
      )
    }

    if(a == files[1]){all_bloods <- bloods}else{
      all_bloods <- rbind(all_bloods, bloods)
    }

  }



  return(all_bloods)


}


