library(googlesheets4)
library(qualtRics)

## Sheet IDs ====

invite_sheet <- "18qZWPefrwjhinjdSS613NXWfASREVvYjzy3dcTna5oM"
consent_tracking_sheet = "1EBGqXqdbSVueoe4r8LRadT7yHQEY5rwhjnLkClfgSoE"
completion_tracking_sheet <- "1HDJsP-WV9ESajZNaixWyZ1gtvnyI2JmQUDgXWIueWI8"

# Authenticate
gs4_auth("day00096@umn.edu")

worksheets <- sheet_names(invite_sheet)
invsheets <- worksheets[str_detect(worksheets, "_invite_")]

invite_sheets <- tibble(sheet_name = invsheets) %>%
  mutate(
    data = map(sheet_name, ~read_sheet(invite_sheet, sheet = .x))
  ) %>%
  unnest(data)

# All surveys ====

my_surveys_f <- "my_surveys.rds"
my_surveys <- all_surveys()
write_rds(my_surveys, my_surveys_f)

ipscr <- my_surveys %>%
  filter(
    str_detect(name, "IPSCR")
  )

id <- list(screener1 = ipscr$id[ipscr$name == "IPSCR Screener W1"],
           screener2 = ipscr$id[ipscr$name == "IPSCR Screener W2"],
           screener3 = ipscr$id[ipscr$name == "IPSCR Screener W3"],
           screener4 = ipscr$id[ipscr$name == "IPSCR Screener W4"],
           screener5 = ipscr$id[ipscr$name == "IPSCR Screener W5"],
           screener6 = ipscr$id[ipscr$name == "IPSCR Screener W6"],
           consent   = ipscr$id[ipscr$name == "IPSCR Consent"],
           vrRSB     = ipscr$id[ipscr$name == "IPSCR vrRSB"],
           cdi       = ipscr$id[ipscr$name == "IPSCR MB-CDI WS"],
           bapq      = ipscr$id[ipscr$name == "IPSCR BAPQ"],
           demo_p1   = ipscr$id[ipscr$name == "IPSCR Demographics"],
           demo_p2   = ipscr$id[ipscr$name == "IPSCR Demographics (P2) [OLD]"])

fix_id <- function(original_id, child_dob, screener) {

  message(original_id)

  # If ID is already six characters, nothing to do - either generated with
  # random pad or was fixed manually
  if (str_length(original_id) == 6) {
      return(original_id)
  }

  options <- screener %>%
    select(screener_id, screener_id2, child_dob) %>%
    filter(
      screener_id == original_id
    )

  n_orig_id <- nrow(options)
  matching_ids <- options$screener_id[options$child_dob == child_dob]


  # [1] The random int pad generated 0-9 which left the original ID as only
  #   five characters, which just gets 0-padded to six characters
  #
  # [2] If there's only one matching ID in the LUT, then just pad with 00s on
  #   the right: [12]xxx00
  if (str_length(original_id) == 5 || n_orig_id <= 1) {

    # Pad the original ID
    new_id <- str_pad(original_id, 6, "right", "0")

  } else {

    if (length(matching_ids) == 1)
      new_id <- matching_ids
    else if (length(matching_ids) == 1)
      stop(paste("Identified two ambiguous candidates for ID", original_id))
    else {
      message(paste("Could not identify resolution for", original_id))
      new_id <- "ERR:NONE"
    }

  }

  return(new_id)

}

# Exclusions ====

# Read list of participants to exclude from tracking sheet (updated manually)
exclusions <- read_sheet(completion_tracking_sheet, range = "R11:R",
                         col_names = "X") %>%
  pull(X) %>%
  str_remove(" .*$") %>%
  str_pad(6, "right", "0")

rm(ipscr, my_surveys, completion_tracking_sheet,
   consent_tracking_sheet, invite_sheet, invsheets, my_surveys_f, worksheets)
