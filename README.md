# ofo_ITD_crossmapping
Evaluation of model transferability of drone based individual tree detections.

## Data management approach for this repo

The data are stored in a [Box folder](https://ucdavis.box.com/s/8gnqihv8xhteapc9i7uc3jk9afyniarn).
Contact Derek or Nayani for access. Copy this folder to your computer (or sync it using Box Drive).

At the top of each script, set the envrionment variable `DATADIR` to the location of this folder
on your computer. Optionally, you can also add, within the repo folder `datadirs`, a file named
`datadir_{yourname}.txt` in which you specify the path to your local data directory in a single
line. Then at the top of each script, you can read the contents of this file, rather than
hard-coding your data directory in each script. Then if you move your data directory, or want to run
the scripts on another computer, you just have to update the `.txt` file. This is the method Derek
set up in `01_plot-selection-stratification.R` if you want an example.

## Repo organization

There are two key folders of code in this repo. The `workflow` folder contains scripts that are
intended to be run through in order to accomplish an objective (described below in 'Analysis
workflow'). The `lib` folder contains files defining functions that are used in scripts in the
`workflow` folder.

## Analysis workflow

### Select plots to use for the analysis

We need to select field plots where we have both high-quality field reference (stem map) data and
drone imagery. To support this process, we have prepared (externally) a .csv table of relevant plot
attributes for all plots that are covered by drone imagery flow at 120 m with at least 90% overlap.
The script **01_plot-selection-stratification.R** uses this data to select plots across relevant
strata.

