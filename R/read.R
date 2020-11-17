read_adjMat <- function(filename, sheet = 1, rescale = FALSE) {

  require(readxl)

  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")

  # Prepare column names and classes
  colNames <- readxl::read_xlsx(filename, col_types = NULL) %>% names
  index <- colNames %in% c("level", "levelName", "vName")

  nCols <- colNames %>% length
  colClass <- rep("numeric", nCols)
  colClass[index] <- "text"


  # Read in adjMat
  output <-
    readxl::read_xlsx(filename, col_types = colClass) %>%
    select(any_of(c("level", "levelName", "vName")), everything())


  # Adjust values
  output[is.na(output)] <- 0 # Replace any NA values with zero

  if(rescale == TRUE) {
    output <- output %>% mutate_if(is.numeric, ~(if_else(. != 0, ./2, .)))
  }

  if(sum(colNames == "level") == 1) { output <- output %>% mutate(level = as.numeric(level)) }


  # Return output
  return(output)

}

read_vInfo <- function(filename, sheet = 1) {

  require(readxl)

  if(!str_detect(filename, ".xlsx")) stop(".xlsx missing in filename")

  output <- readxl::read_xlsx(filename, col_types = NULL, sheet = sheet)

  colNames <- output %>% names

  index <- colNames %in% c("level", "levelName", "vName")

  output <- output[,index]

  if(sum(colNames == "level") == 1) { output <- output %>% mutate(level = as.numeric(level)) }

  return(output)

}

read_indicatorsDatabase <- function(filename,
                                    location,
                                    preferenceLevels = c("Preferred", "Alternate"),
                                    rescale = FALSE) {

  db <-  filename %>% readxl::read_xlsx()

  colNames <- c("indicator", "indicatorPreference", "from", "to")
  colNames <- c(colNames, location)

  output <-
    db %>%
    select(all_of(colNames)) %>%
    setNames(c(colNames[-5], "location")) %>%
    mutate(indicatorPreference = factor(indicatorPreference, preferenceLevels)) %>%
    filter(!is.na(location)) %>%
    group_by(indicatorPreference, from, to) %>%
    summarise(value = mean(location), .groups = "drop_last") %>%
    ungroup %>%
    spread(indicatorPreference, value) %>%
    mutate(value = coalesce(!!! select(., matches(preferenceLevels)))) %>%
    select(-all_of(preferenceLevels)) %>%
    rename(weightNew = value) %>%
    mutate(layer = "l2VPM_l3GF") %>%
    select(layer, everything())

  if(rescale == TRUE) { output <- output %>% mutate(weightNew = weightNew/2) }

  return(output)

}
