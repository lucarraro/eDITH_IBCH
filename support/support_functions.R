# evaluate IBCH index for a list of taxa
assign_IBCH <- function(families){

  richness <- length(families)

  if (any(c("Chloroperlidae","Perlidae","Perlodidae","Taeniopterygidae") %in% families)) {GI = 9
  }else if (any(c("Capniidae","Brachycentridae","Odontoceridae","Philopotamidae") %in% families)) {GI = 8
  }else if (any(c("Leuctridae","Glossosomatidae","Beraeidae","Goeridae","Leptophlebiidae") %in% families)) {GI = 7
  }else if (any(c("Nemouridae","Lepidostomatidae","Sericostomatidae","Ephemeridae") %in% families)) {GI = 6
  }else if (any(c("Hydroptilidae","Heptageniidae","Polymitarcidae","Potamanthidae") %in% families)) {GI = 5
  }else if (any(c("Leptoceridae","Polycentropodidae","Psychomyidae","Rhyacophilidae") %in% families)) {GI = 4
  }else if (any(c("Limnephilidae", "Hydropsychidae", "Ephemerellidae", "Aphelocheiridae") %in% families)) {GI = 3
  }else if (any(c("Baetidae", "Caenidae", "Elmidae", "Gammaridae", "Corbiculidae",
                  "Dreissenidae", "Sphaeriidae", "Unionidae") %in% families)) {GI = 2
  }else if (any(c("Chironomidae", "Asellidae", "Erpobdellidae",
                  "Glossiphoniidae", "Hirudidae", "Piscicolidae", "Oligochaeta") %in% families)) {GI = 1
  }else {GI=0}

  # Mollusca  -> Corbiculidae Dreissenidae Sphaeriidae Unionidae
  # Hirudinea -> Erpobdellidae Glossiphoniidae Hirudidae Piscicolidae

  if (richness>=50) {DK=14
  } else if (richness>=45) {DK=13
  } else if (richness>=41) {DK=12
  } else if (richness>=37) {DK=11
  } else if (richness>=33) {DK=10
  } else if (richness>=29) {DK=9
  } else if (richness>=25) {DK=8
  } else if (richness>=21) {DK=7
  } else if (richness>=17) {DK=6
  } else if (richness>=13) {DK=5
  } else if (richness>=10) {DK=4
  } else if (richness>=7) {DK=3
  } else if (richness>=4) {DK=2
  } else if (richness>=1) {DK=1
  } else {DK=0}

  IBCH.value <- min(20,DK+GI-1)
  if (GI==0 & DK==0) IBCH.value <- 0

  if (IBCH.value>=17){ IBCH.color <- "blue"
  } else if (IBCH.value>=13) {IBCH.color <- "green"
  } else if (IBCH.value>=9) {IBCH.color <- "yellow"
  } else if (IBCH.value>=5) {IBCH.color <- "orange"
  } else {IBCH.color <- "red"}

  IBCH <- list(IBCH.value=IBCH.value,  IBCH.color=IBCH.color, GI=GI, DK=DK)

  invisible(IBCH)
}

# evaluate GI for a list of taxa
eval.GI <- function(taxa){

  GI <- numeric(length(taxa))

  for (i in 1:length(taxa)){
    taxon = taxa[i]
    if (taxon %in% c("Chloroperlidae","Perlidae","Perlodidae","Taeniopterygidae")) {GI[i] = 9
    }else if (taxon %in% c("Capniidae","Brachycentridae","Odontoceridae","Philopotamidae")) {GI[i] = 8
    }else if (taxon %in% c("Leuctridae","Glossosomatidae","Beraeidae","Goeridae","Leptophlebiidae")) {GI[i] = 7
    }else if (taxon %in% c("Nemouridae","Lepidostomatidae","Sericostomatidae","Ephemeridae")) {GI[i] = 6
    }else if (taxon %in% c("Hydroptilidae","Heptageniidae","Polymitarcidae","Potamanthidae")) {GI[i] = 5
    }else if (taxon %in% c("Leptoceridae","Polycentropodidae","Psychomyidae","Rhyacophilidae")) {GI[i] = 4
    }else if (taxon %in% c("Limnephilidae", "Hydropsychidae", "Ephemerellidae", "Aphelocheiridae")) {GI[i] = 3
    }else if (taxon %in% c("Baetidae", "Caenidae", "Elmidae", "Gammaridae", "Corbiculidae",
                           "Dreissenidae", "Sphaeriidae", "Unionidae")) {GI[i] = 2
    }else if (taxon %in% c("Chironomidae", "Asellidae", "Erpobdellidae",
                           "Glossiphoniidae", "Hirudidae", "Piscicolidae", "Oligochaeta")) {GI[i] = 1
    }else {GI[i]=0}
  }
  return(GI)
}

# evaluate rates of TP, FP, TN, FN
eval.TP.FP <- function(a,b, discardSite=NULL){
  if (!is.null(discardSite))  a <- a[-discardSite]; b <- b[-discardSite]
  FP <- TP <- TN <- FN <- 0
  for (i in 1:length(a)){
    if (a[i] & b[i]) TP <- TP + 1
    if (a[i] & !b[i]) FP <- FP + 1
    if (!a[i] & !b[i]) TN <- TN + 1
    if (!a[i] & b[i]) FN <- FN + 1
  }
  TPR <- TP/sum(b)
  FPR <- FP/sum(!b)
  TNR <- TN/sum(!b)
  FNR <- FN/sum(b)
  out <- list(TP=TP, FP=FP, TN=TN, FN=FN,
              TPR=TPR, FPR=FPR, TNR=TNR, FNR=FNR)
  return(out)
}
