library("pegas")
library("adegenet")

setwd("/Users/radovicl/path/Haplotype_diversity")

#The only implemented model is Nei (thus no specification was done, model=Nei default)
All <- read.dna("all_samples.min1.fasta", "fasta")
hap.div(All, TRUE)
sqrt(hap.div(All, TRUE))

daC <- read.dna("all_daC.min1.fasta", "fasta")
sqrt(hap.div(daC, TRUE))

AR <- read.dna("AR.min1.fasta", "fasta")
sqrt(hap.div(AR, TRUE))

TB <- read.dna("TB.min1.fasta", "fasta")
sqrt(hap.div(TB, TRUE))

CB <- read.dna("CB_all.min1.fasta", "fasta")
sqrt(hap.div(CB, TRUE))

CB_daC <- read.dna("CB_daC.min1.fasta", "fasta")
sqrt(hap.div(CBdac, TRUE))

SP <- read.dna("SP_All.min1.fasta", "fasta")
sqrt(hap.div(SP, TRUE))

SP_col <- read.dna("SP_col.min1.fasta", "fasta")
sqrt(hap.div(SP_col, TRUE))

SP_Ib <- read.dna("SP_Ib.min1.fasta", "fasta")
sqrt(hap.div(SP_Ib, TRUE))

SP_naf <- read.dna("SP_NAf.min1.fasta", "fasta")
sqrt(hap.div(SP_naf, TRUE))

Riding_daC <- read.dna("Riding_daC.min1.fasta", "fasta")
sqrt(hap.div(Riding_daC, TRUE))

Local_All <- read.dna("local_all.min1.fasta", "fasta")
sqrt(hap.div(Local_All, TRUE))

Local_Asia_all <- read.dna("Local_Asia_all.min1.fasta", "fasta")
sqrt(hap.div(Local_Asia_all, TRUE))

Local_Asia_dac <- read.dna("Local_Asia_daC.min1.fasta", "fasta")
sqrt(hap.div(Local_Asia_dac, TRUE))

Local_dac <- read.dna("local_dac_mega.fas", "fasta")
sqrt(hap.div(Local_dac, TRUE))

LocalUSA_EU <- read.dna("local_usaeu_all.min1.fasta", "fasta")
sqrt(hap.div(LocalUSA_EU, TRUE))

LocalUSA_EU_daC <- read.dna("Local_USA_daC.min1.fasta", "fasta")
sqrt(hap.div(LocalUSA_EU_daC, TRUE))