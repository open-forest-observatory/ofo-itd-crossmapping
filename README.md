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
`workflow` folder. At the time of writing this there was nothing in the `lib` folder.

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
`library(ofo)`. For example, if you want to install the version of the package that is in the
`/ofo-share/utils/ofo-r` folder, you would run `devtools::install_package("/ofo-share/utils/ofo-r")`.

## Alternative approach for developing reusable functions

If the overhead of developing functions for ofo-r is too high, it works to just define the functions
at the top of your workflow scripts, or in files in this repo's `lib` folder which are then sourced.
We can then move those functions to the ofo-r package later.

## Analysis workflow

Scripts that run the workflow are intended to be sourced (or run interactively) in their entirety.
They are located in the `workflow/` folder of this repo. The steps of the workflow, in order, are
described below, along with a references to the scripts that implement them and the data files they
operate on.

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

Note that all of the selected field plots aligned properly to the drone data *except* plots 0015,
0046, 0105, and 0110, so these plots should be excluded from evaluation until we improve the
alignment algorithm and successfully align them. 

### Crop the CHM to the field plot bounds (plus buffer)

To minimize the compute required for the subsequent steps of tree detection (many different times
for each plot, using different parameters) and comparison of the resulting predicted tree maps to the field
reference data, we can crop the drone data to the bounds of the field data (after shifting it into
alignment by the previous step). We crop it with 20 m buffer so we don't truncate edge trees. This
is performed by `09_crop-chms.R`.

### Generate a list of ITD parameters to test

We test a range of ITD parameters to see how they affect the accuracy of tree detection. Currently we are only using the `lmf` local maximum filter function of the lidR package. Currently the parameters we are testing are the parameters of the quadratic window size function, including min and max bounds on the function. We are using a random parameter search, whereby we randomly sample the parameter space (within specified ranges for each parameter) and generate hundreds of parameter sets to test. This is done by `21_generate-parameter-sets.R`, which saves the parameter sets as a .csv file in `/ofo-share/ofo-itd-crossmapping_data/itd-paramsets/`. It saves two files: one with a table of all parameter sets and the parameter values, and one with the parameter ranges that were used to create the parameter set. This script, and all the follow, are set up to operate on "groups" of parameter sets, so that if we want, we can run the workflow on one group of parameter sets, then revise (broaden or narrow the search based on the results), and run everything for a new group of parameter sets.

Note that there are several other likely important parameters to test that we are not currently testing. These include the resolution of the CHM (currently set to 0.25 m), the amount of smoothing applied to the CHM (currently a 3-pixel moving window mean filter), and the source of the CHM (point cloud vs. mesh model). We should add these parameters to the search space.

### Detect trees from the drone-derived CHM across a range of ITD parameters

We detect treetops from the CHMs using `lidR::locate_trees` with the `lmf` local maximum filter
function. This step includes resampling the CHM to a user-specified resolution and smoothing the CHM
by a user-specified amount prior to ITD. The step allows the user to define the paramaters a, b, and
c of a quadratic local maximum window size function *win = a + b\*x + c\*x^2*, where *x* is the
height of the focal pixel. (The function also includes min and max caps on the window diameter.)
 This function is run once for each parameter set, using the parameter values specified in the parameter set definition file created in the previous step. The resulting detected treetops are saved as .gpkg files (one per each
parameter set per each plot) to `/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees`, under a folder for the parameter set group being evaluated, with
filenames that specify the ITD parameter set ID used and the plot ID; for example
`paramset-000001_plot-0005.gpkg`. This step is performed by `22_predict-trees.R`.

### Evaluate tree predictions

Each set of tree detections in `/ofo-share/ofo-itd-crossmapping_data/drone/predicted-trees` is
compared against the corresponding field reference data (looking up the plot ID from the predicted
trees filename). This relies on tree map comparison functions in the `ofo-r` package, specifically
those defined in
[`tree-detection-accuracy-assessment.R`](https://github.com/open-forest-observatory/ofo-r/blob/main/R/tree-detection-accuracy-assessment.R),
which includes at its core a tree matching algorithm, currently identical to the one used in our
[published MEE paper](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13860).
Then, the recall, precision, and F score are calculated. This results in accuarcy metrics for each plot and each ITD parameter set. The results are saved to a .csv file in `/ofo-share/ofo-itd-crossmapping_data/drone/predicted-tree-evals/`, with a filename that specifies the ITD parameter set group being evaluated. This step is performed by `30_evaluate-predicted-trees.R`.

This script has the option to calculate accuracy against all trees in the plot, or only against trees that are expected to be visible from overhead. The former (compare against all trees) is the default, but can be changed to compare only against overstory trees by setting the constant `EVAL_OVERSTORY_ONLY` at the top of the script to `TRUE`. See the note at the bottom of this README for considerations around this choice.

 **NOTE:** It will be critical that we
carefully review the size ranges of trees that are considered eligible for matching within this
function to ensure that each plot survey protocol involved exhaustively measuring trees within the
matching-eligible size classes. Otherwise, we may (e.g.) end up with an artificially inflated
false-positive rate (low precision) just because some drone-detected trees that existed in reality
were not measured because they were too small. This is performed by `30_evaluate-predicted-trees.R`.

### Identify the best ITD parameter sets

Summarize the resulting accuracy data across each field plot and each ITD parameter set to
yield inferences into how ITD accuracy changes across stand structure and composition, and how
this interacts ith the ITD parameters. Some initial steps in this direction are performed by
`31_visualize-parameterset-performance.R`, which includes some demonstrations of plotting raw results as a series of scatterplots and fitting a multivariate GAM to visualize the partial dependency plots.

We will need to think through carefully how to summarize the accuracy data across plots and ITD parameter sets. For example, there is probably not one best parameter set across all plots, and instead we will need to explore how the ideal parameter set varies across gradients in forest structure (and possibly species composition). It also may be misleading to focus on the single one optimal parameter set for each scenario, beacuse there may be others that perform almost identically, which are also strong performers for other parameter sets or plots.

## Important considerations

There are (at least) two ways to calculate detection accuracy (F-score): against all trees in the plot, or against only the trees that are expected to be visible from overhead. Currently we are evaluating against all trees, but I kind of think we should restrict to overhead-only trees (i.e., optimize for detecting only the trees that we think it is possible to detect); otherwise, we might end up encouraging an algorithm to be overly sensitive and generate false positive detections, as some of them may match to understory trees just by chance and produce a spuriously high F-score. However, I can see an argument for the alternative as well. If we want to exclude understory trees, set the constant `EVAL_OVERSTORY_ONLY` in script `30_evaluate-predicted-trees.R` to `TRUE`. Note that once the optimal parameter set is identified, we should probably re-run the accuracy assessment with `EVAL_OVERSTORY_ONLY = FALSE` to get a sense of the full (ecologically relevant) detection capability of the algorithm and report this number as well.

We should probably run this assessment for both types of CHM (the CHM from the point cloud, and the CHM from the mesh model) because it is unclear which will yield the best performance and we don't want to leave any accuracy on the table. The CHM from the point cloud has more noise, but possibly also more relevant detail.

We should probably evaluate the effect of the CHM smoothing window (moving window mean filter) as well.
