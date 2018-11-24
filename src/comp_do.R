#
# comp_do.R
#
# graphics of ptsd computational project
#
# created on Wed Sep 14 2018
# Philipp Homan, <philipp dot homan at mssm dot edu>
#-----------------------------------------------------------------------

# load data and add file stamp for makefile
#-----------------------------------------------------------------------
source("comp_load.R")
# Create file marker
system("touch ../output/R/comp_do.Rout")

# produce the original scr figure, final fig1
#-----------------------------------------------------------------------
local({
  fig1ls <- fig1()
  p1 <- fig1ls[[1]]

  # save data of 1b to a table for a grant
  #---------------------------------------------------------------------
  tmpdf <- comp %>% filter(stim %in% c("CSminus", "CSplus")) %>%
    dplyr::select(id, order, stim, trial, scrsqrtrc) %>% 
    rename(scr=scrsqrtrc) %>%
    mutate(newtrial=rep(1:56, nrow(comp %>% filter(trial==1)))) %>%
    dplyr::select(id, order, stim, newtrial, scr) %>%
    rename(trial=newtrial)

  write_csv(tmpdf, "../output/tables/comp_fig1bdata.csv")

  plot(p1)
  ggsave(plot=p1, filename="../output/figures/comp_fig1.pdf",
         width=12.5, height=9.85)
})

# produce previous fig2, the original submission amygdala beta figure
#-----------------------------------------------------------------------
local({
  fig2ls <- fig2()
  p2 <- fig2ls[[1]]

  plot(p2)
  ggsave(plot=p2, filename="../output/figures/comp_fig2.pdf",
         width=14.8, height=10.6) 
})


# create hybrid beta figure, final fig2
#-----------------------------------------------------------------------
local({
  fig3ls <- fig3fast()
  p3 <- fig3ls[[1]]
  p3a <- fig3ls[[2]]
  dicsdf <- fig3ls[[4]]

  # save whole graph
  #---------------------------------------------------------------------
  #ggsave(plot=g2, filename="../output/figures/comp_fig6.pdf",
  #       width=13.4, height=10.2)

  ggsave(plot=p3, filename="../output/figures/comp_fig3.pdf",
         width=17.9, height=5.95)

  # save the dics table to disk
  #---------------------------------------------------------------------
  # write_csv(dicsdf, "../output/tables/comp_dic.csv")

  # save dics figure separately
  #---------------------------------------------------------------------
  ggsave(plot=p3a, filename="../output/figures/comp_figs4.pdf",
         width=10.7, height=3.57)
})

# produce the hybrid roi figure, final fig4
#-----------------------------------------------------------------------
local({
  fig6ls <- fig6()
  ggsave(plot=fig6ls[[1]], filename="../output/figures/comp_fig4.pdf",
         width=13.2, height=10.7)
  #ggsave(plot=fig6ls[[1]], filename="~/Desktop/comp_fig4.png")
})

# create amgydala figure with value regressor, final fig3
#-----------------------------------------------------------------------
local({
  fig4ls <- fig4()
  p4 <- fig4ls[[1]]
  ggsave(plot=p4, filename="../output/figures/comp_fig6.pdf",
         width=14.8, height=10.6) 

  # produce the original amygdala beta figure (rw model) with 4mm
  # and func roi
  #---------------------------------------------------------------------
  fig5ls <- fig5()
  p5 <- fig5ls[[1]]
  ggsave(plot=p5, filename="../output/figures/comp_fig5.pdf",
         width=14.8, height=10.6) 
})

# produce sheet for export with winning model fits
#-----------------------------------------------------------------------
#local({
#  tmphavfits <- read_csv(
#    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv"
#  ) %>%
#    rename(estimate=value) %>%
#    dplyr::select(id, trial, param, estimate, model)
#
#  # filter to get eta
#  tmphavfits %>% filter(trial==1, param=="eta")
#
#  # filter to get kappa 
#  tmphavfits %>% filter(trial==1, param=="kappa")
#
#  write_csv(tmphavfits, "~/Desktop/comp_hybridalphav_modelfits.csv")
#})

# merge all main manuscript figures (fig1 -fig5)
local ({
  # fig1: panel with design overview, scr, revindex
  # fig2: panel with model comparison and eta-caps regression
  # fig3: panel with amygdala and value-caps regressions
  # fig4: panel with regions of interest and value, alpha, delta regrs.
  # fig5: mediation analyis graph
  #fig1 <- fig1()[[1]] 
  #fig2 <- fig3()[[1]]
  #fig3 <- fig4()[[1]]
  #fig4 <- fig6()[[1]]
  #fig5 <- rasterGrob(readPNG("../lib/comp_figs18.png"),
  #                 interpolate=TRUE)

  #pdf("../output/figures/comp_all_main_figs.pdf", paper="a4r")
  #plot(fig1)
  #plot(fig2)
  #plot(fig3)
  #plot(fig4)
  #plot_grid(fig5)
  #dev.off()

  system(paste0("../ext/pdfmerge ../output/figures/comp_mainfigs.pdf",
                " ../output/figures/comp_fig1.pdf",
                " ../output/figures/comp_fig3.pdf",
                " ../output/figures/comp_fig4.pdf",
                " ../output/figures/comp_fig6.pdf",
                " ../lib/comp_figs18.pdf"))

  })
