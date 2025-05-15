library(sf)

INPUT_FOLDER = "/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned/trees"
files = list.files(INPUT_FOLDER, full.names = TRUE)

update_missing_data = function(trees) {
  trees$temp_ID = seq.int(nrow(trees))
  null_trees = trees[is.na(trees$ohvis), ]
  null_trees$ohvis_matching_theshold = 0.1 * null_trees$height + 1

  dists = sf::st_distance(null_trees, trees) |> units::drop_units()
  colnames(dists) = trees$temp_ID
  rownames(dists) = null_trees$temp_ID

  # For each null tree, we need to see if there is any tree which is both
  # * Taller than it
  # * Within the suppression radius

  # Error in null_trees$height : $ operator is invalid for atomic vectors
  # https://github.com/open-forest-observatory/ofo-r/blob/3e3d138ffd99539affb7158979d06fc535bc1066/R/tree-detection-accuracy-assessment.R#L169
  return(trees)
}

for (file in files)
{
  trees = st_read(file)
  update_missing_data(trees)
}
