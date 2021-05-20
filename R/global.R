# Available datasets
dataset_list <- rev(sub(".rds", "", list.files(path = file.path("inst", "extdata"), pattern = "rds")))

# Load all
dataset <- lapply(dataset_list, function(x) {
  readRDS(paste0(file.path(".", "inst", "extdata", ""), x, ".rds"))
})
names(dataset) <- dataset_list
