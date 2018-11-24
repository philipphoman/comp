#
# comp_load.R
#
# load ptsd computational project data
#
# created on Jan 18 2017
# Philipp Homan, <philipp dot homan at mssm dot edu>
#-----------------------------------------------------------------------
source("ptsd_func.R")
#-----------------------------------------------------------------------
# main data frame
comp <- read_csv("../data/comp.csv")

# restrict to male participants
#comp %>% filter(gender=="M")

# matlab version at minvera (hard-coded)
matlabversion <- "8.6.0.267246 (R2015b)"

# medication status, restrict to relevant subjects
meds <-
  load_subset("../preproc/clinical/ptsd_clinical_medstatus.csv",
              comp$id) 

# scid
scid <- load_subset("../preproc/clinical/ptsd_clinical_scid.csv",
                    comp$id)

# excluded subjects (=no fmri data available)
exdf <- read_csv("../preproc/clinical/comp_clinical_excluded.csv")

# freesurfer
fs <- load_subset("../preproc/freesurfer/ptsd_freesurfer.csv", comp$id)

# scr
scr <- load_subset("../preproc/scr/ptsd_scr.csv", comp$id)

# old mle data
oldmle <-
  load_subset("../preproc/mle/ptsd_mle_oldmle.csv",
              comp$id)

# old bayes data
oldbayes <-
  load_subset("../preproc/bayes/output/ptsd_bayes_oldbayes.csv",
              comp$id)

# old fmri betas 
oldbetas <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_oldbetas.csv",
              comp$id)

# fmri rw betas 8 mm smoothing
rwbetas8 <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_8mm_rw_betas.csv",
              comp$id) 

# fmri rw betas 8 mm smoothing
rwbetas4 <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_4mm_rw_betas.csv",
              comp$id)

# fmri hybrid alpha v betas 4 mm smoothing
havbetas4 <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_4mm_hav_betas.csv",
              comp$id) 

# fmri hybrid v alpha v betas 4 mm smoothing
vhavbetas4 <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_4mm_vhav_betas.csv",
              comp$id)

# fmri hybrid alpha v rho betas 8 mm smoothing
havrbetas8 <-
  load_subset("../preproc/fmri/betas/ptsd_fmri_8mm_havr_betas.csv",
              comp$id) 

# merge betas
betas <- rwbetas8 %>%
  bind_rows(rwbetas4) %>%
  bind_rows(vhavbetas4) %>%
  bind_rows(havbetas4) %>%
  bind_rows(havrbetas8) 

# merge data with all relevant subsets
comp <- comp %>%
  left_join(meds) %>%
  left_join(fs)  %>%
  left_join(scr) %>%
  left_join(oldbayes)

# for first trial data only
compa <- comp %>% filter(trial==1)

