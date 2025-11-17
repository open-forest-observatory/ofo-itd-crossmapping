library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)

# ------------------------ 1. Settings -----------------------------------------

PARAM_GROUP_ID     <- "7010-1"
ITD_PARAMS_DEF_DIR <- "/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/"

dir.create(ITD_PARAMS_DEF_DIR, showWarnings = FALSE)

# ------------------------ 2. Define parameter ranges --------------------------

PARAM_RANGES <- tribble(
  ~param,          ~min,  ~max,
  "lmf_a",          0.1,   1.9,
  "lmf_b",         -0.1,   0.12,
  "lmf_c",          0,     0,
  "lmf_diam_min",   1,     1,
  "lmf_diam_max",  20,    20
)

# ------------------------ 3. Sample values per parameter ----------------------

# 10 equally spaced values between min and max (or 1 if fixed)
param_samples <- PARAM_RANGES %>%
  mutate(
    values = map2(
      .x = min,
      .y = max,
      ~ if (.x == .y) .x else seq(.x, .y, length.out = 10)
    )
  ) %>%
  select(param, values)

# Named list for expand.grid()
param_list <- setNames(param_samples$values, param_samples$param)

# ------------------------ 4. Cartesian product of all combos -----------------

all_param_combos <- expand.grid(
  param_list,
  KEEP.OUT.ATTRS   = FALSE,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(paramset_id = sprintf("%06d", dplyr::row_number()))

# ------------------------ 5. Save to CSV --------------------------------------

filename <- paste0("itd-paramsets_", PARAM_GROUP_ID, ".csv")
out_path <- file.path(ITD_PARAMS_DEF_DIR, filename)

write_csv(all_param_combos, out_path)

message("Saved ", nrow(all_param_combos), " parameter sets to ", out_path)
