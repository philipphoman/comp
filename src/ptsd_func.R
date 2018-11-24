#
# ptsd_func.R
#
# user defined functions for PTSD project
#
# created on Jan 18 2017
# Philipp Homan, <philipp dot homan at mssm dot edu>
#-----------------------------------------------------------------------

# we now use pacman for all libraries
if (!require("pacman")) install.packages("pacman")
library("pacman")
pacman::p_load(
          "tidyverse",
          "readr",
          "png",
          "grid",
          "QuantPsyc",
          "mediation",
          "cowplot",
          "psych",
          "R.matlab",
          "rstan",
          "truncnorm",
          "car",
          "lm.beta",
          "tidyr",
          "ggplot2",
          "plotrix",
          "lme4",
          "lmerTest",
          "dplyr"
        )

if(!require("represearch")) {
  devtools::install_github("philipphoman/represearch")
}


get_partcorr_vec <- function(lmfit, xc) {
  #
  # Return partial correlations vectors
  #
  ff <- tempfile()
  png(filename=ff)
  a <- avPlots(lmfit)
  dev.off()
  unlink(ff)
  x <- as.data.frame(a[xc])[, 1]
  y <- as.data.frame(a[xc])[, 2]
  return(data.frame(x=x, y=y))
}

GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

ptsd_colors <- c(
  `red`        = "#CD5C5C",
  `blue`       = "#ADD8E6",
  `green`      = "#00b159",
  `orange`     = "#f37735",
  `yellow`     = "#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

ptsd_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (ptsd_colors)

  ptsd_colors[cols]
}

base_breaks_x <- function(x, col="black"){
  b <- pretty(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  list(geom_segment(data=d, color=col, aes(x=x, y=y, xend=xend, 
                                yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=b))
}

base_breaks_y <- function(x, col="black"){
  b <- c(0, pretty(x))
  #d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  d <- data.frame(x=-Inf, xend=-Inf, y=0, yend=max(b))
  list(geom_segment(data=d, col="white", aes(x=x, y=y, 
                                xend=xend, yend=yend),
                    inherit.aes=FALSE),
       scale_y_continuous(breaks=b))
}

dic <- function(stanfit) {
  #
  # calculate DIC
  # 
  p <- rstan::extract(stanfit, permuted=T)
  return(mean(p$dev) + 0.5 * (sd(p$dev))^2)
}

parse_vals <- function(action, param) {
switch(action,
	pval={
		if (param < 0.001) {
			 return("< 0.001")
		} else {
			 return(paste("=", round(param, 3)))
		}
	})
}

load_subset <- function(filename, id_vector) {
  #
  # load subset of data for later merging
  #
  subdf <- readr::read_csv(filename) %>%
    filter(id %in% id_vector)
}

clean_tmp_vars <- function(list_of_patterns) {
  #
  # clean temporary variables
  # providing a list of patterns
  #
  lapply(list_of_patterns, function(x) rm(list = ls(pattern = x)))
  #lapply(list_of_patterns, unlist)
  
  return(TRUE)
}

# figure 1
#----------------------------------------------------------------------
fig1 <- function(dat=comp, filename=NULL) {
  #
  # create fig1
  #
  fig1bdf <- dat %>% filter(stim %in% c("CSminus", "CSplus")) %>%
    mutate(face = ifelse(stim=="CSplus",
                         "Face A (shock in acquisition)",
                         "Face B (shock in reversal)")) %>%
    group_by(face, ctrial) %>%
    dplyr::summarize(mean=mean(scrsqrtrc),
                     sd=sd(scrsqrtrc),
                     n=sum(!is.na(scrsqrtrc)),
                     se=sd/sqrt(n),
                     ci=1.96*se) 
  
  p1b <- ggplot(fig1bdf, aes(x=ctrial, y=mean, group=face)) +
    geom_point(size=6, aes(shape=face, fill=face)) +
    geom_line() +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0) +
    theme_classic(base_size=30) +
    theme(
      panel.border=element_blank(),
      legend.title=element_blank(),
      legend.key.size = unit(1.5, 'lines'),
      legend.position=c(0.65, 0.9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    #base_breaks_x(figdfm1$ctrial) +
    #base_breaks_y(figdfm1$mean) +
    scale_shape_manual(values=c(21, 21)) +
    #scale_fill_manual(values=c(ptsd_cols("blue"), ptsd_cols("red")))
    scale_fill_manual(values=c("indianred", "lightblue")) +
    ylim(0, 1.4) +
    xlab("Trial") +
    ylab("Normalized SCR") 

  # reversal index
  #---------------------------------------------------------------------
  fig1b2df <- dat  %>% filter(trial==1) %>%
    dplyr::summarize(mean=mean(revlearn),
                     sd=sd(revlearn),
                     n=sum(!is.na(revlearn)),
                     se=sd/sqrt(n),
                     ci=1.96*se) %>%
    mutate(x=2)

  p1b2 <- ggplot(fig1b2df, aes(x=x, y=mean)) +
    geom_point(size=4) +
    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0) +
    theme_classic(base_size=20) +
    theme(
      axis.line.y=element_blank(),
      axis.line.x=element_line(color="white"),
      axis.ticks=element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      plot.title=element_blank(),
      #axis.title=element_text(face="bold"),
      legend.title=element_blank(),
      #strip.text.x=element_blank(),
      #strip.text.x=element_text(face="bold"),
      strip.background = element_blank()
    ) +
    scale_shape_manual(values=c(1, 19)) +
    scale_color_manual(values=c("white", "black"))+
    #base_breaks_x(c(1, 2, 3), col="black") +
    #base_breaks_y(figdfm1$mean, col="white") +
    xlab("Reversal\nIndex") +
    #xlab("RI") +
    ylab("") + 
    ylim(0, 1.4) +  
    xlim(1, 3) +
    geom_hline(yintercept=0, linetype="dashed") +
    annotate("text", x=2.7, y=fig1b2df$mean,
             label=round(fig1b2df$mean, 2), size=5)


  p1a <- rasterGrob(readPNG("../lib/ptsd_design.png"),
                   interpolate=TRUE)

  # produce the multipanel figure
  #---------------------------------------------------------------------
  p1 <- plot_grid(plot_grid(p1a, scale=1),
                 plot_grid(p1b, p1b2, nrow=1, labels=NULL,
                 rel_widths=c(3, 1), align="hv", axis="lgbt"),
                 labels=c("a", "b"), nrow=2, align="hv",
                 label_size=45, scale=0.9)
  plot(p1)
  if (!is.null(filename)) {
    ggsave(plot=p1, filename=filename, width=12.5, height=9.85)
  }

  # return the figure handle, data frames for scr time courses and
  # reversal index
  #---------------------------------------------------------------------
  return(list(p1, fig1bdf, fig1b2df))
}

fig2 <- function(dat=comp, filename=NULL) {
  #
  # original submission figure 2
  #
  oldbetas <- load_subset("../preproc/fmri/betas/ptsd_fmri_oldbetas.csv",
                          dat$id)
  figdf <- dat %>% left_join(oldbetas) %>% filter(trial==1)
  l1 <- lm(caps ~ age + gender + fmrimovparam +
            alpharwscrsqrtrchbayes +
            EstimatedTotalIntraCranialVol +
            Right.Amygdala +
            valuerabeta,
          data=figdf)

  l2 <- lm(caps ~ age + gender +
            alpharwscrsqrtrchbayes +
            valuelabeta,
          data=figdf)

  # use individual amygdala rois
  #figdftmp <- comp %>%
  #  left_join(betas %>% filter(grepl("deltavalue", model),
  #                             contrast=="value",
  #                             roi=="right_amygdala_indiv_roi")) %>%
  #  filter(trial==1)

  #l3 <- lm(caps ~ age + gender +
  #          EstimatedTotalIntraCranialVol +
  #          Right.Amygdala +
  #          beta,
  #        data=figdftmp)

  # partial correlation plot, right amygdala
  #---------------------------------------------------------------------
  tmpdf2b <- get_partcorr_vec(l1, xc="Right.Amygdala")
  rs2b <- cor.test(tmpdf2b$x, tmpdf2b$y)
  p2b <- ggplot(tmpdf2b, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Volume (adj.)") +
    ylab("CAPS (adj.)") +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs2b$estimate, 2),
                         starsfromp(rs2b$p.value), sep=""))

  tmpdf2c <- get_partcorr_vec(l1, xc="valuerabeta")
  rs2c <- cor.test(tmpdf2c$x, tmpdf2c$y)
  p2c <- ggplot(tmpdf2c, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    ylab("CAPS (adj.)") +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs2c$estimate, 2),
                         starsfromp(rs2c$p.value), sep=""))

  # partial correlation plot, left amygdala
  #---------------------------------------------------------------------
  tmpdf2d <- get_partcorr_vec(l2, xc="valuelabeta")
  rs2d <- cor.test(tmpdf2d$x, tmpdf2d$y)
  p2d <- ggplot(tmpdf2d, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    ylab("CAPS (adj.)") +
    ggtitle("L Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs2d$estimate, 2),
                         starsfromp(rs2d$p.value), sep=""))

  p2a <- rasterGrob(readPNG("../lib/ptsd_fmri_roi_amygdala.png"),
                   interpolate=TRUE)

  p2 <- plot_grid(plot_grid(p2a, scale=1.0), NULL, NULL,
                 p2b, p2c, p2d, labels=c("a", "", "",  "b", "c", "d"),
                 label_size=45, nrow=2, align="hv")
  #plot(p)
  if (!is.null(filename)) {
    ggsave(plot=p2, filename=filename, width=14.8, height=10.6) 
  }
  return(list(p2))
}

fig6 <- function(dat=comp, filename=NULL) {
  #
  # produce figure6
  #
  havfits <- read_csv(
    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv"
  )
  modelnamev <- "glm-04-Jun-2018-computational-smoothed4mm-hybridalphav"
  modelnameh <- "glm-22-May-2018-computational-smoothed4mm-hybridalphav"

  # prepare value betas as a third column 
  #---------------------------------------------------------------------
  fig6b2df <- dat %>% filter(trial==1) %>%
    dplyr::select(id, age, gender, caps, group, bdi, stais, asi) %>%
    left_join(betas %>% filter(model==modelnamev,
                               contrast %in% c("value"))) %>%
    mutate(hemi=ifelse(grepl("left", roi), "left",
                ifelse(grepl("right", roi), "right", "both")),
           region=ifelse(roi=="left_amygdala_functional_roi" |
                          roi=="right_amygdala_functional_roi",
                          "Amygdala",
                  ifelse(roi=="left_hippocampus_roi" |
                          roi=="right_hippocampus_roi",
                          "Hippocampus",
                  ifelse(roi=="left_caudate_roi" |
                          roi=="right_caudate_roi",
                          "Striatum",
                  ifelse(roi=="dacc_roi",
                         "dACC", "not needed"))))) %>%
    rename(component=contrast) %>%
    filter(region != "not needed") %>%
    group_by(group, id, region, component) %>%
    dplyr::summarize(betamu=sum(beta)/2) %>%
    left_join(dat %>% filter(trial==1) %>%
              dplyr::select(id, age, gender, caps, group,
                            bdi, stais, asi))

  fig6df <- dat %>% filter(trial==1) %>%
    dplyr::select(id, age, gender, caps, group, bdi, stais, asi) %>%
    left_join(betas %>% filter(model==modelnameh,
                     contrast %in% c("alpha", "delta"))) %>%
    mutate(hemi=ifelse(grepl("left", roi), "left",
                ifelse(grepl("right", roi), "right", "both")),
           region=ifelse(roi=="left_amygdala_functional_roi" |
                          roi=="right_amygdala_functional_roi",
                          "Amygdala",
                  ifelse(roi=="left_hippocampus_roi" |
                          roi=="right_hippocampus_roi",
                          "Hippocampus",
                  ifelse(roi=="left_caudate_roi" |
                          roi=="right_caudate_roi",
                          "Striatum",
                  ifelse(roi=="dacc_roi",
                         "dACC", "not needed"))))) %>%
    rename(component=contrast) %>%
    filter(region != "not needed") %>%
    group_by(group, id, region, component) %>%
    dplyr::summarize(betamu=sum(beta)/2) %>%
    left_join(dat %>% filter(trial==1) %>%
              dplyr::select(id, age, gender, caps, fmrimovparam,
                            EstimatedTotalIntraCranialVol,
                            Left.Amygdala,
                            Right.Amygdala,
                            group, bdi, stais, asi)) %>%
    # add value regressor betas
    bind_rows(fig6b2df) %>%
    left_join(havfits %>% filter(param=="eta", trial==1) %>%
              rename(eta=value) %>% dplyr::select(id, eta)) %>%
    arrange(id, region, component)

  # special request: order of regions, start with amygdala and striatum
  #figdf6$region <- factor(figdf6$region,
  #                       levels=c("Amygdala", "Striatum",
  #                       "dACC", "Hippocampus"))

  # individual regressions for each region and learning component
  #---------------------------------------------------------------------
  lms <- by(fig6df, list(fig6df$region, fig6df$component), lm,
            formula=formula("scale(caps) ~ scale(betamu)"))

  lmsdf <- data.frame(region=rep(unique(fig6df$region, 3)),
                      component=rep(unique(fig6df$component), each=4),
                      beta=sapply(lms, function(x) coef(summary(x))[2]),
                      tval=sapply(lms, function(x) coef(summary(x))[6]),
                      pval=sapply(lms, function(x) coef(summary(x))[8])) %>%
    mutate(sig=starsfromp(pval),
           regcomp=paste(region, component)) %>%
      arrange(regcomp) %>% 
      mutate(regcomp=factor(regcomp, levels=regcomp[c(3, 1, 2,
                                                      12,10,11,
                                                      6, 4, 5,
                                                      9, 7, 8)])) %>%
      arrange(regcomp) %>%
      filter(region != "Amygdala")

  # create another variable for facet wrap
  #---------------------------------------------------------------------
  fig6df$regcomp <- rep(paste(rep(unique(fig6df$region), each=3),
                         rep(unique(fig6df$component), 4)), 54)

  # impose a reasonable order for regcomp
  # make sure striatum is in first row
  # also, use just the region names as facet labes as per
  # special request by one coauthor
  #---------------------------------------------------------------------
  facet_labels <- as_labeller(list(
    `Amygdala value` = "Amygdala",
    `Amygdala alpha` = "Amygdala",
    `Amygdala delta` = "Amygdala",
    `Striatum value` = "Striatum",
    `Striatum alpha` = "Striatum",
    `Striatum delta` = "Striatum",
    `dACC value` = "dACC",
    `dACC alpha` = "dACC",
    `dACC delta` = "dACC",
    `Hippocampus value` = "Hippocampus",
    `Hippocampus alpha` = "Hippocampus",
    `Hippocampus delta` = "Hippocampus"
  ))
  fig6df$regcomp <- factor(fig6df$regcomp,
                           levels=fig6df$regcomp[c( 3, 1, 2,
                                                   12,10,11,
                                                    6, 4, 5,
                                                    9, 7, 8)])

  fig6df$regcomp2 <-
    factor(
      c("Amygdala", " Amygdala ", "  Amygdala  ",
        "Striatum", " Striatum ", "  Striatum  ",
        "dACC", " dACC ", "  dACC  ",
         "Hippocampus", " Hippocampus ", "  Hippocampus  "),
      levels=c("Amygdala", " Amygdala ", "  Amygdala  ",
              "Striatum", " Striatum ", "  Striatum  ",
              "dACC", " dACC ", "  dACC  ",
              "Hippocampus", " Hippocampus ", "  Hippocampus  "))
                         

  p6 <- ggplot(fig6df %>% filter(region != "Amygdala"),
               aes(x=betamu, y=caps)) +
    geom_point(size=4) +
    geom_smooth(method="lm") +
    #facet_grid(component ~ region, scales="free", space="free") +
    facet_wrap(~regcomp2, scale="free", ncol=3) +
    theme_gray(base_size=25) +
    theme(
      #strip.text.x=element_text(size=25,  
      #                          margin=margin(0.4, 0, 0.4, 0, "cm")),
      strip.text.x=element_text(size=25),
      legend.title=element_blank(),
      legend.key.size=unit(1.5, 'lines'),
      #title=element_text(
      axis.title=element_text(size=35)
      ) +
    xlab("Beta estimates (neural activity)") +
    #xlim(c(-6, 5)) +
    ylab("CAPS") +
    ylim(c(0, 100)) +
    ggtitle(paste("          Value          ",
                  "             Associability             ",
                  "Prediction error", sep="")) +
    annotate("text", x=Inf, y=Inf,
             label=paste("r=", round(lmsdf$beta, 2), lmsdf$sig, sep=""),
             size=6, hjust=1, vjust=1)
    #scale_color_manual(values=c("cornflowerblue", "blue4")) +
    #scale_x_discrete("", labels=c(expression(alpha),
    #                              expression(delta),
    #                              expression(alpha),
    #                              expression(delta)))
  #plot(g2)  

  if (!is.null(filename)) {
    ggsave(plot=p6, filename=filename, width=13.4, height=10.2) 
  }
  return(list(p6, fig6df))
}

fig3 <- function(dat=comp, filename=NULL) {
  #
  # produce figure3
  # this is the bayesian model comparison and partial
  # correlation with caps
  #
  havfitsw <- read_csv(
    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv"
  ) %>%
    spread(key=param, value=value)
  
  modelnamev <- "glm-04-Jun-2018-computational-smoothed4mm-hybridalphav"
  modelnameh <- "glm-22-May-2018-computational-smoothed4mm-hybridalphav"

  fig3df <- dat %>% filter(trial==1) %>%
    left_join(havfitsw %>% filter(trial==1))

  # correlation of symptoms and learning rate eta
  #---------------------------------------------------------------------
  lmfit <- lm(caps ~ age + gender + eta, data=fig3df)

  # men only
  #---------------------------------------------------------------------
  # lmfitm <- lm(caps ~ age + eta, data=fig3df %>% filter(gender=="M"))

  # partial correlation of eta and caps
  #---------------------------------------------------------------------
  tmpdf <- get_partcorr_vec(lmfit, xc="eta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p3b <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=6) +
    geom_smooth(method="lm") +
    theme_gray(base_size=25) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab(bquote(eta~(adj.))) +
    ylab("CAPS (adj.)") +
    #ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))
  
  # model comparison
  #---------------------------------------------------------------------
  models <- c(
    "ptsd_bayes_stan_rw.stan_VCC.fit",
    "ptsd_bayes_stan_rw.stan_VPTSD.fit",
    "ptsd_bayes_stan_rwrho.stan_VCC.fit",
    "ptsd_bayes_stan_rwrho.stan_VPTSD.fit",
  #  "ptsd_bayes_stan_rwrho_vfixed.stan_VCC.fit",
  #  "ptsd_bayes_stan_rwrho_vfixed.stan_VPTSD.fit",
    "ptsd_bayes_stan_hybridalpha.stan_VCC.fit",
    "ptsd_bayes_stan_hybridalpha.stan_VPTSD.fit",
    "ptsd_bayes_stan_hybridv.stan_VCC.fit",
    "ptsd_bayes_stan_hybridv.stan_VPTSD.fit",
    "ptsd_bayes_stan_hybridalphav.stan_VCC.fit",
    "ptsd_bayes_stan_hybridalphav.stan_VPTSD.fit",
    "ptsd_bayes_stan_hybridalphavrho.stan_VCC.fit",
    "ptsd_bayes_stan_hybridalphavrho.stan_VPTSD.fit"
  )
  models_fin <- paste("../preproc/bayes/modelfits/", models, sep="")

  cat("Calculating DICs...")
  dics <- unlist(lapply(models_fin, function(x) {
    load(x)
    dic(fit)
    }))
  cat("done\n") 

  dicsums <- rowSums(matrix(dics, ncol=2, byrow=TRUE))

  dicsdf <- data.frame(dic=dicsums,
                       model=c("RW", "RW (rho)",
                             #"RW (rho, vfixed)",
                               "Hybrid (v)",
                               "Hybrid (alpha)", "Hybrid(alpha + v)",
                               "Hybrid (alpha + v, rho)"))

  # save that table to disk
  #---------------------------------------------------------------------
  # write.csv(dicsdf, "../output/tables/comp_dic.csv", row.names=FALSE)

  # model comparison graph
  #---------------------------------------------------------------------
  p3a <- ggplot(dicsdf, aes(x=model, y=dic)) +
    geom_bar(stat="identity") +
    theme_gray(base_size=25) +
    theme(
    #  axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)
    ) +
    coord_flip() +
    ylab("DIC") +
    xlab("") +
    annotate("text", y=1.1 * min(dicsums), x=3.8, label="*", size=16)

  # save dic figure separately (for revision letter)
  #---------------------------------------------------------------------
  # ggsave(plot=p3a, filename="../output/figures/comp_figs4.pdf",
  #       width=10.7, height=3.57)


  p3 <- plot_grid(p3a, p3b, align="h", ncol=2, rel_widths=c(1.7, 1),
                  labels=c("a", "b"), label_size=45)

  if (!is.null(filename)) {
    ggsave(plot=p3, filename=filename, width=17.9, height=5.95)
  }
  return(list(p3, p3a, p3b, dicsdf, fig3df))
}

fig3fast <- function(dat=comp, filename=NULL) {
  #
  # produce figure3
  # this is the bayesian model comparison and partial
  # correlation with caps
  #
  havfitsw <- read_csv(
    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv"
  ) %>%
    spread(key=param, value=value)
  
  modelnamev <- "glm-04-Jun-2018-computational-smoothed4mm-hybridalphav"
  modelnameh <- "glm-22-May-2018-computational-smoothed4mm-hybridalphav"

  fig3df <- dat %>% filter(trial==1) %>%
    left_join(havfitsw %>% filter(trial==1))

  # correlation of symptoms and learning rate eta
  #---------------------------------------------------------------------
  lmfit <- lm(caps ~ age + gender + eta, data=fig3df)

  # men only
  #---------------------------------------------------------------------
  # lmfitm <- lm(caps ~ age + eta, data=fig3df %>% filter(gender=="M"))

  # partial correlation of eta and caps
  #---------------------------------------------------------------------
  tmpdf <- get_partcorr_vec(lmfit, xc="eta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p3b <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=6) +
    geom_smooth(method="lm") +
    theme_gray(base_size=25) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab(bquote(eta~(adj.))) +
    ylab("CAPS (adj.)") +
    #ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))
  
  # model comparison
  #---------------------------------------------------------------------
  dicsdf <- read_csv("../output/tables/comp_dic.csv")

  # model comparison graph
  #---------------------------------------------------------------------
  p3a <- ggplot(dicsdf, aes(x=model, y=dic)) +
    geom_bar(stat="identity") +
    theme_gray(base_size=25) +
    theme(
    #  axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)
    ) +
    coord_flip() +
    ylab("DIC") +
    xlab("") +
    annotate("text", y=1.1 * min(dicsdf$dic), x=3.8, label="*", size=16)

  # save dic figure separately (for revision letter)
  #---------------------------------------------------------------------
  # ggsave(plot=p3a, filename="../output/figures/comp_figs4.pdf",
  #       width=10.7, height=3.57)


  p3 <- plot_grid(p3a, p3b, align="h", ncol=2, rel_widths=c(1.7, 1),
                  labels=c("a", "b"), label_size=45)

  if (!is.null(filename)) {
    ggsave(plot=p3, filename=filename, width=17.9, height=5.95)
  }
  return(list(p3, p3a, p3b, dicsdf, fig3df))
}


fig4 <- function(dat=comp, filename=NULL, component="valuehav") {
  #
  # produce figure 4
  # this is the amygdala roi figure using, by default,
  # the hybrid value regressor
  #
  switch(component,

         # for the value regressor from the rw model
         valuerw = {
           betas <- load_subset(
             "../preproc/fmri/betas/ptsd_fmri_8mm_rw_betas.csv",
             dat$id) %>%
             filter(contrast=="value") %>%
             mutate(contrast=factor(contrast))
           modelname <-
             "glm-28-Jul-2017-computational-smoothed8mm-deltavalue"
         },

         # for the value regressor from the hybrid model
         valuehav = {
           betas <- load_subset(
             "../preproc/fmri/betas/ptsd_fmri_4mm_vhav_betas.csv",
             dat$id) %>%
             filter(contrast=="value") %>%
             mutate(contrast=factor(contrast))
           modelname <-
             "glm-04-Jun-2018-computational-smoothed4mm-hybridalphav"
         },

         # for the alpha regressor from the hybrid model
         alphahav = {
           betas <- load_subset(
             "../preproc/fmri/betas/ptsd_fmri_4mm_hav_betas.csv",
             dat$id) %>%
             filter(contrast=="alpha") %>%
             mutate(contrast=factor(contrast))
           modelname <-
             "glm-22-May-2018-computational-smoothed4mm-hybridalphav"
         },

         # for the delta regressor from the hybrid model
         deltahav = {
           betas <- load_subset(
             "../preproc/fmri/betas/ptsd_fmri_4mm_hav_betas.csv",
             dat$id) %>%
             filter(contrast=="delta") %>%
             mutate(contrast=factor(contrast))
           modelname <-
             "glm-22-May-2018-computational-smoothed4mm-hybridalphav"
         })
         
 
  # distinguish left and right amygdala with hybrid v regressor
  #---------------------------------------------------------------------
  fig4df <- dat %>% filter(trial==1) %>%
    dplyr::select(id, age, gender, caps, group, bdi, stais, asi) %>%
    left_join(betas)%>%
    rename(component=contrast) %>%
    filter(roi %in% c("left_amygdala_functional_roi",
                      "right_amygdala_functional_roi")) %>%
    left_join(dat %>% filter(trial==1) %>%
              dplyr::select(id, age, gender, caps, group, bdi, stais,
                            asi, EstimatedTotalIntraCranialVol,
                            fmrimovparam,
                            Left.Amygdala, Right.Amygdala))

  lm1 <- lm(caps ~ age + gender + EstimatedTotalIntraCranialVol +
                fmrimovparam +
                Left.Amygdala + beta,
            data=fig4df %>% filter(roi=="left_amygdala_functional_roi"))
  summary(lm1)

  lm2 <- lm(caps ~ age + gender + EstimatedTotalIntraCranialVol +
                fmrimovparam +
                Right.Amygdala + beta,
            data=fig4df %>% filter(roi=="right_amygdala_functional_roi"))
  summary(lm2)

  tmpdf <- get_partcorr_vec(lm2, xc="Right.Amygdala")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p4b <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Volume (adj.)") +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))


  tmpdf <- get_partcorr_vec(lm2, xc="beta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p4c <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    #xlim(c(-10, 5)) +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))


  tmpdf <- get_partcorr_vec(lm1, xc="beta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p4d <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    #xlim(c(-10, 5)) +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("L Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))

  p4a <- rasterGrob(readPNG("../lib/ptsd_fmri_roi_amygdala_functional.png"),
                   interpolate=TRUE)

  p4 <- plot_grid(plot_grid(p4a, scale=1.0), NULL, NULL,
                 p4b, p4c, p4d, labels=c("a", "", "",  "b", "c", "d"),
                 label_size=45, nrow=2, align="hv")
  return(list(p4, p4a, p4b, p4c, p4d, fig4df))
}

fig5 <- function(dat=comp, filename=NULL) {
  #
  # produce figure 5, the original amygdala beta figure with 4mm and
  # func roi
  #
  betas4rw <- read_csv("../preproc/fmri/betas/ptsd_fmri_4mm_rw_betas.csv")
  rwfits <- read_csv("../preproc/bayes/output/ptsd_bayes_stan_rw.csv")
  figdf <- dat %>% filter(trial==1) %>%
    left_join(rwfits %>% filter(trial==1, param=="alpha") %>%
              dplyr::select(id, value, param) %>%
              spread(key=param, value=value) %>%
              rename(alpharw=alpha)) %>%
    left_join(betas4rw) 

  l1 <- lm(caps ~ age + gender + fmrimovparam +
            alpharw +
            EstimatedTotalIntraCranialVol +
            Right.Amygdala +
            beta,
           data=figdf %>% filter(roi=="right_amygdala_functional_roi",
                                 contrast=="value"))

  l2 <- lm(caps ~ age + gender + fmrimovparam +
            alpharwscrsqrtrchbayes +
            EstimatedTotalIntraCranialVol +
            Left.Amygdala +
            beta,
           data=figdf %>% filter(roi=="left_amygdala_functional_roi",
                                 contrast=="value"))

  tmpdf <- get_partcorr_vec(l1, xc="Right.Amygdala")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p5b <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Volume (adj.)") +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))


  tmpdf <- get_partcorr_vec(l1, xc="beta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p5c <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("R Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))


  tmpdf <- get_partcorr_vec(l2, xc="beta")
  rs <- cor.test(tmpdf$x, tmpdf$y)
  p5d <- ggplot(tmpdf, aes(x=x, y=y)) +
    geom_point(size=5) +
    geom_smooth(method="lm") +
    theme_gray(base_size=30) +
    theme(plot.title=element_text(hjust=0.5)) +
    xlab("Beta (adj.)") +
    ylab("CAPS (adj.)") +
    ylim(c(-50, 75)) +
    ggtitle("L Amygdala") +
    annotate("text", x=Inf, y=Inf, size=10,
             hjust=1, vjust=1,
             label=paste("r=", round(rs$estimate, 2),
                         starsfromp(rs$p.value), sep=""))

  p5a <- rasterGrob(readPNG("../lib/ptsd_fmri_roi_amygdala_functional.png"),
                   interpolate=TRUE)

  p5 <- plot_grid(plot_grid(p5a, scale=1.0), NULL, NULL,
                 p5b, p5c, p5d, labels=c("a", "", "",  "b", "c", "d"),
                 label_size=45, nrow=2, align="hv")
  return(list(p5))
}

figs15 <- function(dat=compa, betas=havbetas4) {
  #
  #
  # test for additional brain-behavior relationships using prediction 
  # error weight

  # which rois
  rois <- c(
      "left_amygdala_functional_roi",
      "right_amygdala_functional_roi",
      "left_caudate_roi",
      "right_caudate_roi",
      "left_hippocampus_roi",
      "right_hippocampus_roi",
      "dacc_roi")

  # which model
  models <- c(
      "glm-22-May-2018-computational-smoothed4mm-hybridalphav"   
      )

  # which contrasts
  contrasts <- c(
      "delta",
      "alpha"
      )

  groups <- c(
    "VCC",
    "VPTSD"
    )

  havfits <- read_csv(
    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv") %>%
    filter(trial==1, param=="eta") %>%
    dplyr::select(-trial, -model, -param) %>%
    rename(eta=value) %>%
    left_join(dat %>% dplyr::select(id, group, age, gender,
                                      caps, stais, asi)) %>%
    filter(group %in% groups) 


  # create data frame of selection
  b4vccvptsd <- betas %>%
    filter(roi %in% rois,
           model %in% models,
           contrast %in% contrasts) %>%
    left_join(dat %>% dplyr::select(id, group,
                                      factor1caps, factor2caps,
                                      factor3caps, factor4caps,
                                      factor5caps,
                                      Left.Amygdala, Right.Amygdala,
                                      EstimatedTotalIntraCranialVol)) %>%
    filter(group %in% groups) %>%
    mutate(hemi=ifelse(grepl("left", roi), "left",
                ifelse(grepl("right", roi), "right", "both")),
           region=ifelse(roi=="left_amygdala_functional_roi" |
                          roi=="right_amygdala_functional_roi",
                          "Amygdala",
                  ifelse(roi=="left_hippocampus_roi" |
                          roi=="right_hippocampus_roi",
                          "Hippocampus",
                  ifelse(roi=="left_caudate_roi" |
                          roi=="right_caudate_roi",
                          "Striatum",
                  ifelse(roi=="dacc_roi",
                         "dACC", "not needed"))))) %>%
    rename(component=contrast) %>%
    filter(region != "not needed") %>%
    group_by(group, id, model, region, component) %>%
    #dplyr::summarize(betamu=sum(beta)/2) %>%
    left_join(havfits) %>%
    left_join(dat %>% dplyr::select(id, group,
                                      factor1caps, factor2caps,
                                      factor3caps, factor4caps,
                                      factor5caps,
                                      Left.Amygdala, Right.Amygdala,
                                      EstimatedTotalIntraCranialVol))

  # mediation analysis
  # x -> m -> y
  # eta -> neural activity -> caps
  #
  lmfits <- list()
  count <- 0
  for (i in 1:length(contrasts)) {  
    for (j in 1:length(rois)) {
      count <- count + 1
      dat <- b4vccvptsd %>% filter(roi==rois[j],
                                   component==contrasts[i]) %>%
      ungroup() %>%
      rename(y=caps,
             x=eta,
             m=beta) %>%
        dplyr::select(id, y, x, m)
      lmfits[[count]] <- mediation_analysis(dat)
    }
  }
  
  return(list(lmfits, b4vccvptsd))
}

figs16 <- function(dat=compa, betas=havbetas4) {
  #
  # calculate the interaction of region and component
  # as in Li et al 2011, Nat Neurosci
  rois <- c(
       "left_amygdala_functional_roi",
       "right_amygdala_functional_roi",
       "left_caudate_roi",
       "right_caudate_roi",
       "left_hippocampus_roi",
       "right_hippocampus_roi",
       "dacc_roi")

   # which model
   models <- c(
       "glm-22-May-2018-computational-smoothed4mm-hybridalphav"   
       )

   # which contrasts
   contrasts <- c(
       "delta",
       "alpha"
       )

   groups <- c(
     "VCC"
     #"VPTSD"
     )

   havfits <- read_csv(
     "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv") %>%
     filter(trial==1, param=="eta") %>%
     dplyr::select(-trial, -model, -param) %>%
     rename(eta=value) %>%
     left_join(dat %>% dplyr::select(id, group, age, gender,
                                       caps, stais, asi)) %>%
     filter(group %in% groups) 


   # create data frame of selection
   b4vccvptsd <- betas %>%
     filter(roi %in% rois,
            model %in% models,
            contrast %in% contrasts) %>%
     left_join(dat %>% dplyr::select(id, group,
                                       factor1caps, factor2caps,
                                       factor3caps, factor4caps,
                                       factor5caps,
                                       Left.Amygdala, Right.Amygdala,
                                       EstimatedTotalIntraCranialVol)) %>%
     filter(group %in% groups) %>%
     mutate(hemi=ifelse(grepl("left", roi), "left",
                 ifelse(grepl("right", roi), "right", "both")),
            region=ifelse(roi=="left_amygdala_functional_roi" |
                           roi=="right_amygdala_functional_roi",
                           "Amygdala",
                   ifelse(roi=="left_hippocampus_roi" |
                           roi=="right_hippocampus_roi",
                           "Hippocampus",
                   ifelse(roi=="left_caudate_roi" |
                           roi=="right_caudate_roi",
                           "Striatum",
                   ifelse(roi=="dacc_roi",
                          "dACC", "not needed"))))) %>%
     rename(component=contrast) %>%
     filter(region != "not needed") %>%
     group_by(group, id, model, region, component) %>%
     dplyr::summarize(betamu=sum(beta)/2) %>%
     left_join(havfits) %>%
     left_join(dat %>% dplyr::select(id, group,
                                       factor1caps, factor2caps,
                                       factor3caps, factor4caps,
                                       factor5caps,
                                       Left.Amygdala, Right.Amygdala,
                                       EstimatedTotalIntraCranialVol))

  # final data frame for mixed model
  tmpdf <- b4vccvptsd %>%
    ungroup() %>%
    filter(region %in% c("Amygdala", "Striatum"),
           component %in% c("alpha", "delta")) %>%
    dplyr::select(id, betamu, region, component)
                                 

  lmerfit <- lmer(betamu ~ region * component +
                    (1 + region + component|id), data=tmpdf)

  # manual calculation of the effect size
  # let's call it: dissociation index
  di <- tmpdf %>%
    group_by(id, region, component) %>%
    dplyr::summarize(mean=mean(betamu)) %>%
    mutate(regcomp=paste(region, component, sep="")) %>%
    group_by(id) %>%
    dplyr::select(-region, -component) %>%
    spread(key=regcomp, value=mean) %>%
    mutate(dissoc_ind=Amygdalaalpha - Amygdaladelta -
             (Striatumalpha - Striatumdelta)) %>%
    ungroup() %>%
    dplyr::summarize(mean=mean(dissoc_ind),
                     sd=sd(dissoc_ind),
                     n=sum(!is.na(dissoc_ind)),
                     se=sd/sqrt(n),
                     ci=se * 1.96)

  tmpdat <- tmpdf %>%
    group_by(region, component) %>%
    dplyr::summarize(mean=mean(betamu),
                     sd=sd(betamu),
                     n=sum(!is.na(betamu)),
                     se=sd/sqrt(n)) %>%
    mutate(regcomp=paste(region, component, sep="")) %>%
    ungroup() %>%
    dplyr::select(-region, -component) %>%
    gather(variable, value, -regcomp) %>%
    unite(temp, regcomp, variable) %>%
    spread(temp, value) %>%
    dplyr::select(-matches("_n|_sd")) %>%
    magrittr::set_colnames(c("m1", "se1", "m2", "se2",
                             "m3", "se3", "m4", "se4")) %>%
    mutate(n=nrow(tmpdf)/4)

  # using the li2011 function
  di <- li2011(dat=tmpdat)

  return(list(lmerfit, tmpdf, di))
}

li2011 <- function(dat=data.frame(m1=10.5, se1=3.5/2,
                                  m2=2,    se2=4.2/2,
                                  m3=2.4,  se3=4.9/2,
                                  m4=6,    se4=5.1/2,
                                  n=17)) {
  #
  # return the values of the region and component interaction
  # in li et al. 2011, nat neurosci, fig. 2.
  # note: we extract values visually from this figure
  #
  # amygdala alpha
  # m1 <- 10.5
  # se1 <- 3.5/2
  #
  # amygdala delta
  # m2 <- 2
  # se2 <- 4.2/2
  #
  # striatum alpha
  # m3 <- 2.4
  # se3 <- 4.9/2
  #
  # striatum delta
  # m4 <- 6
  # se4 <- 5.1/2
  m1 <- dat$m1
  m2 <- dat$m2
  m3 <- dat$m3
  m4 <- dat$m4
  se1 <- dat$se1
  se2 <- dat$se2
  se3 <- dat$se3
  se4 <- dat$se4
  n <- dat$n

  di <- data.frame(mean=(m1-m2)-(m3-m4),
                   se=sqrt((sqrt(se1^2+se2^2))^2 +
                           (sqrt(se3^2+se4^2))^2)) %>%
    mutate(ci=se * 1.96,
           n=n,
           sd=sqrt(n) * se,
           lower=mean-ci,
           upper=mean+ci,
           t=mean/se,
           d=t/sqrt(n),
           lower_d=psych::cohen.d.ci(d, n1=n)[1],
           upper_d=psych::cohen.d.ci(d, n1=n)[3],
           pval=2*pt(abs(t), n-1, lower=FALSE))
  return(di)
}


mediation_analysis <- function(dat) {
  #
  # run formal mediation analysis
  #
  # dat: data frame with y, x, and m
  #
  # returns: list of lmfits

  formulas <- c(
    "y ~ x",
    "m ~ x",
    "y ~ m",
    "y ~ x + m"
  )
  lmfits <- lapply(formulas, function(x) lm(data=dat, formula=x))
  return(lmfits)
}

parse_di <- function(di) {
  #
  # parse dissociation index data frame
  #
  di <- round(di, 2)
  str <- paste("Cohen's /d/ = ", di$d, ", ",
               "95% CI: [", di$lower_d, "; ", di$upper_d, "], ",
               "/P/ ", represearch::parse_pval(di$pval, 3),
               sep="")
  print(str)
}

parse_sexratios <- function(dat=compa) {
  #
  # parse the sex ratios of li2011 and the current study
  #
  limales <- 9
  lifemales <- 8
  males <- sum(dat$gender=="M")
  females <- sum(dat$gender=="F")
  htest <- fisher.test(matrix(c(males, females,
                                limales, lifemales),
                              nrow=2))
  str <- paste("M:F = ", males, ":", females,
               " in the current study versus ", limales, ":", lifemales,
               " in Li and colleagues; ",
               "/P/ ", represearch::parse_pval(htest$p.val, 3),
               sep="")
  print(str)
}

parse_ages <- function(dat=compa) {
  #
  # parse age differences
  #
  rngli <- range(c(18, 31))
  rng <- range(compa$age)
  str <- paste(rng[1], " - ", rng[2], " in the current study versus ",
               rngli[1], " - ", rngli[2], " in Li and colleagues",
               sep="")
  print(str)
}

figs17 <- function(di1, di2) {
  #
  # plot the dissociation index for both studies
  #
  dat <- rbind(di1, di2) %>%
    mutate(study=factor(c("Li et al. 2011", "Current study"),
                        levels=c("Li et al. 2011", "Current study")))

  p <- ggplot(dat, aes(x=study, y=d)) +
    geom_point(size=3) +
    geom_errorbar(aes(x=study, ymin=lower_d, ymax=upper_d),
                  size=1.2, width=0) +
    theme_gray(base_size=25) +
    ylab("Dissociation index (Cohen's d)") +
    xlab("")

  return(list(p))
}

figs18 <- function(dat) {
  #
  # plot a mediation analysis graph
  #
  #l <- lapply(lmf, function(x) as.data.frame(coef(summary(lm.beta(x)))))
  #dat <- do.call("rbind", l) %>%
    #mutate(label=c(rep("eta -> CAPS", 2),
                   #rep("eta -> Neural activity", 2),
                   #rep("Neural activity -> CAPS", 2),
                   #rep("eta -> CAPS, Neural activity -> CAPS", 3)))

  #
  # graph
  #tmpdat <- data.frame(x=dat$eta, dv=dat$caps, mediatior=dat$beta)
  
}

mediation_analysis_boot <- function(dat, sims=5000) {
  #
  # use bootstrapping to test significance of mediation
  #
  # y: outcome
  # x: independent variable
  # m: mediator
  str <- paste0("Starting bootstrap mediation analysis with ",
                sims, " simulations...")
  cat(str)
  lmfit1 <- lm(y ~ x, data=dat)
  lmfit2 <- lm(m ~ x, data=dat)
  lmfit3 <- lm(y ~ m, data=dat)
  lmfit4 <- lm(y ~ x + m, data=dat)
  tm <- mediation::mediate(lmfit2, lmfit4, treat="x", mediator="m",
                boot=TRUE, sims=sims)
  cat("done\n")
  return(tm)
}

figs19 <- function(dat=compa, betas=havbetas4, stdize=TRUE) {
  #
  # mediation analysis for all rois with bootstrapping
  #
  # which rois
  rois <- c(
      "left_amygdala_functional_roi",
      "right_amygdala_functional_roi",
      "left_caudate_roi",
      "right_caudate_roi",
      "left_hippocampus_roi",
      "right_hippocampus_roi",
      "dacc_roi")

  # which model
  models <- c(
      "glm-22-May-2018-computational-smoothed4mm-hybridalphav"   
      )

  # which contrasts
  contrasts <- c(
      "delta",
      "alpha"
      )

  groups <- c(
    "VCC",
    "VPTSD"
    )

  havfits <- read_csv(
    "../preproc/bayes/output/ptsd_bayes_stan_hybridalphav.csv") %>%
    filter(trial==1, param=="eta") %>%
    dplyr::select(-trial, -model, -param) %>%
    rename(eta=value) %>%
    left_join(dat %>% dplyr::select(id, group, age, gender,
                                      caps, stais, asi)) %>%
    filter(group %in% groups) 


  # create data frame of selection
  b4vccvptsd <- betas %>%
    filter(roi %in% rois,
           model %in% models,
           contrast %in% contrasts) %>%
    left_join(dat %>% dplyr::select(id, group,
                                      factor1caps, factor2caps,
                                      factor3caps, factor4caps,
                                      factor5caps,
                                      Left.Amygdala, Right.Amygdala,
                                      EstimatedTotalIntraCranialVol)) %>%
    filter(group %in% groups) %>%
    mutate(hemi=ifelse(grepl("left", roi), "left",
                ifelse(grepl("right", roi), "right", "both")),
           region=ifelse(roi=="left_amygdala_functional_roi" |
                          roi=="right_amygdala_functional_roi",
                          "Amygdala",
                  ifelse(roi=="left_hippocampus_roi" |
                          roi=="right_hippocampus_roi",
                          "Hippocampus",
                  ifelse(roi=="left_caudate_roi" |
                          roi=="right_caudate_roi",
                          "Striatum",
                  ifelse(roi=="dacc_roi",
                         "dACC", "not needed"))))) %>%
    rename(component=contrast) %>%
    filter(region != "not needed") %>%
    group_by(group, id, model, region, component) %>%
    #dplyr::summarize(betamu=sum(beta)/2) %>%
    left_join(havfits) %>%
    left_join(dat %>% dplyr::select(id, group,
                                      factor1caps, factor2caps,
                                      factor3caps, factor4caps,
                                      factor5caps,
                                      Left.Amygdala, Right.Amygdala,
                                      EstimatedTotalIntraCranialVol))

  # mediation analysis
  # x -> m -> y
  # eta -> neural activity -> caps
  #
  tms <- list()
  count <- 0
  for (i in 1:length(contrasts)) {  
    for (j in 1:length(rois)) {
      count <- count + 1
      dat <- b4vccvptsd %>% filter(roi==rois[j],
                                   component==contrasts[i]) %>%
        ungroup() %>%
        mutate(capsz=scale(caps)[, 1],
               etaz=scale(eta)[, 1],
               betaz=scale(beta)[, 1],
               y=if (stdize==TRUE) capsz else caps,
               x=if (stdize==TRUE) etaz else eta,
               m=if (stdize==TRUE) betaz else beta) %>%
        dplyr::select(id, y, x, m)
      tms[[count]] <- mediation_analysis_boot(dat, sims=500)
    }
  }
  return(list(tms, b4vccvptsd, rois, contrasts))
}


parse_mediate <- function(medtab) {
  #
  # parse mediation analysis table
  #
  s <- summary(medtab)

  # average mediation effect (c' - c)
  b <- s$d0

  # ci of this effect
  ci.lower <- s$d0.ci[1]
  ci.upper <- s$d0.ci[2]

  # p-value
  pval <- s$d0.p
  
  str <- paste0("b = ", round(b, 2), ", ",
                "95% CI: [", round(ci.lower, 2), "; ",
                round(ci.upper, 2), "]; ",
                "/P/ ", represearch::parse_pval(pval, 3))
  print(str)
}
