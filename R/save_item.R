# NOTE: made changes for FIX to folder names, fix changes later
# OTHERS:
  # load_items (way to load all in folder(s) and recalc values)

# STEPS:
  # compute values from matrix
    # or as arguments? Need to decide this
    # maybe can set it as argument
    # if necessary, compute, otherwise, taken from other functions

save_item <- function(matrix, compute_indices = TRUE, gridline_size = 1)
{
  if (nrow(matrix) != ncol(matrix)) {
    return("PERIMETER: unable to save non-square grid")
  }

  # indices could be specified with arguments
  if (compute_indices) {
    c <- compute_complexity(matrix)
    current_item <- data.frame(
      item = 0,  # pick 0 for now
      grid_size = nrow(matrix),
      area = sum(matrix),
      perimeter = calculate_perimeter(matrix),
      uneven_count = check_sides(matrix, check = "uneven_count"),
      uneven_sides = check_sides(matrix, check = "uneven_sides"),
      flat_count = check_sides(matrix, check = "flat_count"),
      flat_sides = check_sides(matrix, check = "flat_sides"),
      complexity = min(c$complexity),  # cbind this with the rest?
      # top <- 1
      # bottom <- 1
      # left <- 1
      # right <- 1
      complexity_fits = min(c$complexity[c$fits == TRUE]),
      # top_fits <- 1
      # bottom_fits <- 1
      # left_fits <- 1
      # right_fits <- 1
      matrix_string = paste0("m_",
                             paste(as.vector(unlist(matrix)), collapse = ""))
    )
  }

  # name to save: i3_a42_p28_s2LT_c5_cf5
  to_save <- paste0("g", current_item$grid_size,
                    "_a", current_item$area,
                    "_p", current_item$perimeter,
                    "_c", current_item$complexity,
                    "_cf", current_item$complexity_fits,
                    "_f", current_item$flat_count,
                    "_",
                    toupper(current_item$flat_sides)
                    )

  # make stimuli folder if not found
  stim_dir <- (paste0(here::here(), "\\stimuliFIX"))
  if(file.exists(stim_dir)) {
    setwd(file.path(stim_dir))
  } else {
    dir.create(file.path(stim_dir))
    setwd(file.path(stim_dir))
  }

  # make subfolder for current area/perimeter if not found
  save_dir <- (paste0(here::here(), "\\stimuliFIX\\", to_save))
  if(file.exists(save_dir)) {
    setwd(file.path(save_dir))
  } else {
    dir.create(file.path(save_dir))
    setwd(file.path(save_dir))
  }

  # if csv doesn't exist, make, otherwise load as df
  csv_name <- paste0(to_save, ".csv")
  if(file.exists(csv_name)) {
    item_list <- read.csv(csv_name)
  } else {
    item_list <- data.frame(matrix(nrow = 0, ncol = ncol(current_item)))
    colnames(item_list) <- colnames(current_item)
  }

  # check if unique indicator is already made
    # if not, find highest item number, current item is that +1
  if (!(current_item$matrix_string %in% item_list$matrix_string)) {
    current_item$item <- nrow(item_list) + 1
    item_list <- rbind(item_list, current_item)
    write.csv(item_list, file = paste0(to_save, ".csv"), row.names = FALSE)
  }

  # save matrix as RDS
  # NOTE: this needs to change name based on rows of csv
    # it's overwriting instead now
  saveRDS(matrix, file = to_save)

  # save matrix as plot
  plot_matrix(matrix, gridline_size, save_image = TRUE,
              name = paste0(to_save, ".png"))

  # save again for ease of viewing (at least for now)
  setwd(file.path(stim_dir))
  plot_matrix(matrix, gridline_size, save_image = TRUE,
              name = paste0(to_save, ".png"))

  return("PERIMETER: item saved successfully")
}





