
# what is the point of this function
# 1 - load the matrix from rds?
# 2 - load the csv of the matrices?
  # or both??

# so from string false is just load the matrix
# from string true is load the csv and return it as a df?


load_item <- function(item_name, stim_folder = "stimuli",
                      from_string = FALSE, m_string = "",
                      use_item_folder = TRUE  # set this to false when using other folder
                      )
{

  parent_dir <- paste0(here::here(), "\\", stim_folder)

  if (use_item_folder == FALSE) {
    item_dir <- parent_dir
  } else {
    item_dir <- paste0(parent_dir, "\\", item_name)
  }

  setwd(file.path(item_dir))

  if (from_string == TRUE) {
    # load csv instead
    item_csv <- readr::read_csv(paste0(item_name, ".csv"))
    return(item_csv)
    # use the matrix string from the correct row to do it
  } else {
    item <- readRDS(paste0(item_dir, "\\", item_name))
    return(item)
  }

}



# item_name = "g6_a11_p20_c3_cf5_f3_LRT"

