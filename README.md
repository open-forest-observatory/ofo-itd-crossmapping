# ofo_ITD_crossmapping
Evaluation of model transferability of drone based individual tree detections.

## Data management approach for this repo

The data are stored on Jetstream2 at `/ofo-share/ofo-itd-crossmapping_data/`.

In most scripts, there are constants defined at the top of the script that point to specific folders
within this data folder on Jetstream2 where certain data products are to be read from or written to.

## Repo organization

There are two key folders of code in this repo. The `workflow` folder contains scripts that are
intended to be run through in order to accomplish an objective (described below in 'Analysis
workflow'). The `lib` folder contains files defining functions that are used in scripts in the
`workflow` folder. At the time of writing this wtere was nothing in the `lib` folder.

## Dependence on `ofo` R package

We have an [in-development OFO R package](https://github.com/open-forest-observatory/ofo-r) called
`ofo`. Ideally, any functionality needed for ITD crossmapping that would be generally useful OFO
functionality should be added as a function of ofo-r and then called from that package. You can
source all the functions of ofo-r using `devtools::load_all({path/to/ofo-r/repo})`. You can either
source the package at `/ofo-share/utils/ofo-r` (which we should keep up to date with the main
branch), or if you want to make some edits in your own clone of the repo before pushing to the
repo's main branch, you can point this to the path to your clone of the repo on Jetstream2.

If you want to do parallelization using functions of the `ofo-r` package, it is insufficient to source
the functions using `devtools::load_all()`, because they don't load properly for parallelization.
You need to actually install the development version of the package with
`devtools::install_package({path/to/ofo-r/repo})` (possibly preceded by
`devtools::document({path/to/ofo-r/repo})` if documentation is not up to date), then load it with
`library(ofo)`.

## Alternative approach for developing reusable functions

If the overhead of developing functions for ofo-r is too high, it works to just define the functions
at the top of your workflow scripts, or in files in this repo's `lib` folder which are then sourced.
We can then move those functions to the ofo-r package later.

## Analysis workflow

Scripts that run the workflow are intended to be sourced (or run interactively) in their entirety.
They are located in the `workflow/` folder of this repo.

### Select plots to use for the analysis

We need to select field plots where we have both high-quality field reference (stem map) data and
drone imagery. To support this process, we have prepared (externally) a .csv table of relevant plot
attributes for all plots that are covered by drone imagery flown at 120 m with at least 90% overlap.
The script **01_plot-selection-stratification.R** uses this data to select plots across relevant
strata.

### Find drone missions covering the plots

Performed by `02_find-drone-missions-covering-field-plots.R`. Find the drone missions that
completely cover (with some buffer) a provided set of field plots. Create a table of the
associations. Relies on drone mission polygons present in
`/ofo-share/drone-imagery-processed/01/mission-polygons/`.

### Copy in the relevant drone (DEM, DTM) and field data (tree locations, plot boundaries)

Copy as hardlinks into /ofo-share/ofo-idt-crossmapping_data/, with filenames that use the ID of the
field plot. That is, even drone imagery files will be named with the ID of the field plot they
cover. If one mission covers multiple plots, it will be duplicated under different file names (but
as hardlinks, so it will not use additional storage). Performed by
`04_copy-in-drone-data.R` and `05_copy-in-field-data.R`. This assumes the relevant drone missions
have already been processed into DEM and DTM datasets.

### Create CHMs from DEM and DTM

Runs the `ofo-r` function `chm_from_coregistered_dsm_dtm` to ... generate the CHM from coregistered
DSM and DTM. Run in parallel across each drone dataset. Performed by `07_make-chms.R`. Outputs saved
to `/ofo-share/ofo-itd-crossmapping_data/drone/chms-uncropped/`. **Note that two types of CHMs are
produced:** one based on the point cloud (pretty noisy) and one based on the mesh model (smoother).

### Align the field trees to the drone dataset

Due to GPS error, the coordinates of the trees in the field plot may be (rigidly) shifted by 0-20 m
from the drone imagery. However, comparisons of field plot data to drone-derived data assume the two are
correctly co-registered. To co-register them, we first run a preliminary tree detection from the
drone-derived CHM. This is performed by `08_align-field-and-drone/08-01_prelim-tree-detection.R`,
which relies on the function `detect_trees2` in the `ofo-r` package.

Then, we use the tree alignment function `find_best_shift` from the `ofo` package to determing the
optimal rigid shift to apply to the field data so it is aligned to the drone data. We than apply
this shift to the tree locations and the field plot boundary and save the shifted version of these
files in the folder `/ofo-share/ofo-itd-crossmapping_data/field-reference/aligned`. This is performed
by `08_align-field-and-drone/08-02_alignment.R`.

### Crop the CHM to the field plot bounds (plus buffer)

To minimize the compute required for the subsequent steps of tree detection (many different times
for each plot, using different parameters) and comparison of the resulting predicted tree maps to the field
reference data, we can crop the drone data to the bounds of the field data (after shifting it into
alignment by the previous step). We crop it with 20 m buffer so we don't truncate edge trees. This
is performed by `09_crop-chms.R`.

### Detect trees from the drone-derived CHM using a range of ITD parameters

We detect treetops from the CHMs using `lidR::locate_trees` with the `lmf` local maximum filter
function. This step includes resampling the CHM to a user-specified resolution and smoothing the CHM
by a user-specified amount prior to ITD. The step allows the user to define the paramaters a, b, and
c of a quadratic local maximum window size function *win = a + b\*x + c\*x^2*, where *x* is the
height of the focal pixel. (The function also includes min and max caps on the window radius.)
Currently, the values for each of these parameters are supplied as constants defined at the top of
the script, and thus the script generates a single tree map (for each plot) using the single set of
supplied parameters. The resulting detected treetops are saved as .gpkg files (one per each
parameter set per each plot) to `/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees` with
filenames that specify the ITD parameter set ID used and the plot ID; for example
`params-0001_plot-0005.gpkg`. This step is performed by `20_predict-trees-from-chm.R`. **TO DO:**
Convert the current workflow code into a function that can be applied across a large set of ITD
parameters specified in a .csv file or at the top of the script.

### Evaluate tree predictions

Each set of tree detections in `/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees` is
compared against the corresponding field reference data (looking up the plot ID from the predicted
trees filename). This relies on tree map comparison functions in the `ofo-r` package, specifically
those defined in
[`tree-detection-accuracy-assessment.R`](https://github.com/open-forest-observatory/ofo-r/blob/main/R/tree-detection-accuracy-assessment.R),
which includes at its core a tree matching algorithm, currently identical to the one used in our
[published MEE paper](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13860).
Then, the recall, precision, and F score are calculated. **NOTE:** It will be critical that we
carefully review the size ranges of trees that are considered eligible for matching within this
function to ensure that each plot survey protocol involved exhaustively measuring trees within the
matching-eligible size classes. Otherwise, we may (e.g.) end up with an artificially inflated
false-positive rate (low precision) just because some drone-detected trees that existed in reality
were not measured because they were too small. This is performed by `30_evaluate-predicted-trees.R`.
*TODO:* Currently this just runs on a single predicted tree map and does not save the results
anywhere. We need to run this across all predicted tree maps and save the results to file. Ideally,
this would be a single file that contains the accuracy results from each tree map, which should be
pretty feasible to do within this workflow because the compute for each tree map comparison is not
very intensive.

## Remaining steps

Aside fromt the TO DO items associated with each existing step above, the remaining computational
steps include:

- Summarize the resulting accuracy data across each field plot and each ITD parameter set to
  yield inferences into how ITD accuracy changes across stand structure and composition, and how
  this interacts ith the ITD parameters.
