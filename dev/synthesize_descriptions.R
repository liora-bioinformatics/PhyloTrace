# dev/synthesize_descriptions.R
#
# Replaces the existing summaries in species_metadata.json with
# concise, editorially synthesized descriptions written for this cgMLST
# scheme browser. Each description follows a fixed 3-sentence template:
#   1. identity & morphology (Gram reaction, shape, oxygen, family)
#   2. clinical / biological significance
#   3. relevance (transmission, AMR, biothreat or typing note)
#
# Taxonomy fields (TaxID, lineage) remain NCBI-verified and untouched; they are
# re-aligned to the current CSV `species`/`abb` columns by row position (the
# organisms are unchanged, only the scheme labels), so editing names in the CSV
# does not require re-fetching from NCBI. Schemes whose cgMLST abbreviation ends
# in "_complex" describe a group of species-level taxa (tagged rank = "complex").
#
# Run from the project root:  Rscript dev/synthesize_descriptions.R

library(jsonlite)

json_path <- "app/logic/data/species_metadata.json"
csv_path <- "app/logic/data/cgmlst_schemes.csv"

# Synthesized descriptions, keyed by the CSV `species` field.
descriptions <- c(
  "Acinetobacter_baumannii" = paste(
    "Acinetobacter baumannii is a Gram-negative, aerobic coccobacillus of the",
    "family Moraxellaceae. It is an opportunistic pathogen responsible for",
    "ventilator-associated pneumonia, bloodstream, wound and urinary-tract",
    "infections, almost exclusively in healthcare settings. It is notorious for",
    "multidrug resistance and environmental persistence, and ranks among the",
    "ESKAPE and WHO priority pathogens."
  ),
  "Bacillus_anthracis" = paste(
    "Bacillus anthracis is a Gram-positive, rod-shaped, spore-forming,",
    "facultatively anaerobic bacterium of the family Bacillaceae. It is the",
    "causative agent of anthrax in herbivores and humans, with cutaneous,",
    "inhalational and gastrointestinal forms. Its durable spores and potent",
    "toxins make it a high-consequence biothreat agent."
  ),
  "Bordetella_pertussis" = paste(
    "Bordetella pertussis is a Gram-negative, aerobic coccobacillus of the",
    "family Alcaligenaceae. It causes whooping cough (pertussis), a highly",
    "contagious respiratory disease driven by toxins such as pertussis toxin.",
    "Although vaccine-preventable, it remains resurgent, making surveillance",
    "important."
  ),
  "Brucella_melitensis" = paste(
    "Brucella melitensis is a Gram-negative, facultatively intracellular",
    "coccobacillus of the family Brucellaceae. It is the principal agent of",
    "brucellosis acquired from sheep and goats, causing undulant (Malta) fever",
    "in humans. It is a zoonotic, high-consequence pathogen of public and",
    "veterinary health."
  ),
  "Brucella_spp" = paste(
    "Brucella is a genus of small, Gram-negative, facultatively intracellular",
    "coccobacilli of the family Brucellaceae, including species such as",
    "B. melitensis, B. abortus and B. suis. Its members cause brucellosis, a",
    "zoonosis transmitted from livestock to humans through direct contact or",
    "unpasteurised dairy products. The genus is regarded as a high-consequence",
    "pathogen group for both public and animal health."
  ),
  "Burkholderia_mallei_(RKI)" = paste(
    "Burkholderia mallei is a Gram-negative, non-motile, aerobic bacillus of",
    "the family Burkholderiaceae. It is the causative agent of glanders, a",
    "serious disease chiefly of horses and other equids that is occasionally",
    "transmitted to humans. It is classed as a high-consequence pathogen and",
    "potential biothreat agent; this scheme is the variant curated for the",
    "Robert Koch Institute (RKI)."
  ),
  "Burkholderia_mallei_(FLI)" = paste(
    "Burkholderia mallei is a Gram-negative, non-motile, aerobic bacillus of",
    "the family Burkholderiaceae. It is the causative agent of glanders, a",
    "serious disease chiefly of horses and other equids that is occasionally",
    "transmitted to humans. It is classed as a high-consequence pathogen and",
    "potential biothreat agent; this scheme is the variant curated for the",
    "Friedrich-Loeffler-Institut (FLI)."
  ),
  "Burkholderia_pseudomallei" = paste(
    "Burkholderia pseudomallei is a Gram-negative, motile, aerobic bacillus of",
    "the family Burkholderiaceae. It causes melioidosis, acquired from soil and",
    "surface water in tropical regions, with diverse and often severe clinical",
    "presentations. It is intrinsically resistant to many antibiotics and is",
    "considered a high-consequence pathogen."
  ),
  "Campylobacter_jejuni/coli" = paste(
    "The Campylobacter jejuni/coli complex groups the two closely related",
    "species Campylobacter jejuni and C. coli, Gram-negative, microaerophilic,",
    "spirally curved rods of the family Campylobacteraceae. Together they are",
    "the leading cause of bacterial gastroenteritis (campylobacteriosis)",
    "worldwide, with poultry as the principal reservoir. cgMLST is used to",
    "distinguish the two species and trace foodborne transmission."
  ),
  "Clostridioides_difficile" = paste(
    "Clostridioides difficile is a Gram-positive, rod-shaped, anaerobic,",
    "spore-forming bacterium of the family Peptostreptococcaceae. Its toxins",
    "cause antibiotic-associated diarrhoea and pseudomembranous colitis,",
    "typically after disruption of the gut microbiota. Spread by resilient",
    "spores, it is a leading cause of healthcare-associated infection."
  ),
  "Clostridium_perfringens" = paste(
    "Clostridium perfringens is a Gram-positive, rod-shaped, anaerobic,",
    "spore-forming bacterium of the family Clostridiaceae. Through a broad",
    "repertoire of toxins it causes food poisoning, gas gangrene (clostridial",
    "myonecrosis) and enteritis. Its environmental spores and rapid growth make",
    "it a notable agent of foodborne and wound infections."
  ),
  "Corynebacterium_diphtheriae" = paste(
    "Corynebacterium diphtheriae is a Gram-positive, club-shaped, pleomorphic,",
    "facultatively anaerobic bacillus of the family Corynebacteriaceae.",
    "Toxigenic strains produce diphtheria toxin, causing diphtheria with its",
    "characteristic pharyngeal pseudomembrane. It is vaccine-preventable, but",
    "toxigenic strains remain under close surveillance."
  ),
  "Corynebacterium_pseudotuberculosis" = paste(
    "Corynebacterium pseudotuberculosis is a Gram-positive, club-shaped,",
    "facultatively anaerobic bacillus of the family Corynebacteriaceae. It",
    "causes caseous lymphadenitis in sheep and goats and occasional zoonotic",
    "infections in humans. It is of major veterinary importance in",
    "small-ruminant flocks."
  ),
  "Cronobacter_sakazakii/malonaticus" = paste(
    "The Cronobacter sakazakii/malonaticus complex groups two closely related",
    "species of Gram-negative, rod-shaped bacteria in the family",
    "Enterobacteriaceae. These opportunistic pathogens cause rare but severe",
    "neonatal infections — meningitis, sepsis and necrotising enterocolitis",
    "— frequently linked to powdered infant formula. cgMLST supports species",
    "delineation and source tracing during outbreaks."
  ),
  "Enterococcus_faecalis" = paste(
    "Enterococcus faecalis is a Gram-positive, ovoid coccus, facultatively",
    "anaerobic, of the family Enterococcaceae. A gut commensal, it is also a",
    "leading cause of healthcare-associated urinary-tract, bloodstream and",
    "endocardial infections. It is notable for intrinsic and acquired",
    "resistance, including vancomycin resistance (VRE)."
  ),
  "Enterococcus_faecium" = paste(
    "Enterococcus faecium is a Gram-positive, ovoid coccus, facultatively",
    "anaerobic, of the family Enterococcaceae. A gut commensal and major",
    "nosocomial pathogen, it is prominent among vancomycin-resistant",
    "enterococci (VRE) and is an ESKAPE and WHO priority pathogen. cgMLST",
    "supports tracking of hospital outbreaks and resistant lineages."
  ),
  "Escherichia_coli" = paste(
    "Escherichia coli is a Gram-negative, rod-shaped, facultatively anaerobic",
    "bacterium of the family Enterobacteriaceae. It is a common commensal of",
    "the mammalian gut, but specific pathotypes cause diarrhoeal disease,",
    "urinary-tract infections and bloodstream infections. It is a primary",
    "target for outbreak surveillance and antimicrobial-resistance monitoring."
  ),
  "Francisella_tularensis" = paste(
    "Francisella tularensis is a Gram-negative, facultatively intracellular",
    "coccobacillus of the family Francisellaceae. It causes tularaemia, a",
    "zoonosis acquired from lagomorphs, rodents, arthropod vectors and",
    "contaminated water or aerosols. It is highly infectious at very low doses",
    "and is classed as a high-consequence biothreat agent."
  ),
  "Klebsiella_oxytoca/grimontii/michiganensis/pasteurii" = paste(
    "The Klebsiella oxytoca complex (K. oxytoca sensu lato) comprises several",
    "closely related, Gram-negative, encapsulated rods of the family",
    "Enterobacteriaceae, including K. oxytoca, K. michiganensis and",
    "K. grimontii. These opportunistic pathogens cause nosocomial infections",
    "and antibiotic-associated haemorrhagic colitis. cgMLST is required to",
    "resolve the constituent species, which routine tests cannot separate."
  ),
  "Klebsiella_pneumoniae/variicola/quasipneumoniae" = paste(
    "The Klebsiella pneumoniae complex (K. pneumoniae sensu lato) groups",
    "closely related, Gram-negative, encapsulated rods of the family",
    "Enterobacteriaceae — chiefly K. pneumoniae, K. variicola and",
    "K. quasipneumoniae. A leading cause of healthcare-associated pneumonia,",
    "bloodstream and urinary-tract infections, it is a focus of",
    "multidrug-resistance (ESBL, carbapenemase) and hypervirulence",
    "surveillance. cgMLST distinguishes the complex members and high-risk",
    "clones."
  ),
  "Legionella_pneumophila" = paste(
    "Legionella pneumophila is a Gram-negative, aerobic, facultatively",
    "intracellular bacillus of the family Legionellaceae. It inhabits",
    "freshwater and engineered water systems and causes Legionnaires' disease,",
    "a severe pneumonia, when contaminated aerosols are inhaled. Outbreak",
    "investigation often relies on high-resolution typing to link cases to",
    "water sources."
  ),
  "Listeria_monocytogenes" = paste(
    "Listeria monocytogenes is a Gram-positive, rod-shaped, facultatively",
    "anaerobic, facultatively intracellular bacterium of the family",
    "Listeriaceae. It causes listeriosis, a foodborne disease especially",
    "dangerous in pregnancy, neonates and the immunocompromised. Its ability to",
    "grow at refrigeration temperatures makes it a key food-safety surveillance",
    "target."
  ),
  "Mycobacterium_tuberculosis/bovis/africanum/canettii_complex" = paste(
    "The Mycobacterium tuberculosis complex is a group of closely related,",
    "acid-fast, slow-growing, aerobic bacilli of the family Mycobacteriaceae",
    "— including M. tuberculosis, M. bovis, M. africanum and M. canettii —",
    "that cause tuberculosis in humans and animals. Its members are genetically",
    "near-identical, so high-resolution typing such as cgMLST is used to",
    "resolve lineages and trace transmission."
  ),
  "Mycobacteroides_abscessus" = paste(
    "Mycobacteroides abscessus is an acid-fast, rod-shaped, rapidly growing",
    "nontuberculous mycobacterium of the family Mycobacteriaceae. It causes",
    "difficult-to-treat pulmonary, skin and soft-tissue infections,",
    "particularly in people with cystic fibrosis or compromised immunity. It is",
    "renowned for extensive intrinsic antibiotic resistance."
  ),
  "Mycoplasma_gallisepticum" = paste(
    "Mycoplasma gallisepticum is a small, wall-less bacterium of the family",
    "Mycoplasmoidaceae that lacks a true cell wall. It causes chronic",
    "respiratory disease in chickens and infectious sinusitis in turkeys, with",
    "significant impact on the poultry industry. It spreads both horizontally",
    "and vertically through flocks."
  ),
  "Paenibacillus_larvae" = paste(
    "Paenibacillus larvae is a Gram-positive, rod-shaped, spore-forming",
    "bacterium of the family Paenibacillaceae. It is the causative agent of",
    "American foulbrood, a lethal and highly contagious disease of honeybee",
    "larvae. Its resilient spores underpin its veterinary and agricultural",
    "importance."
  ),
  "Pseudomonas_aeruginosa" = paste(
    "Pseudomonas aeruginosa is a Gram-negative, rod-shaped, aerobic bacterium",
    "of the family Pseudomonadaceae. A versatile opportunistic pathogen, it",
    "causes nosocomial pneumonia, bloodstream, wound and urinary-tract",
    "infections, and chronic lung infection in cystic fibrosis. It is an ESKAPE",
    "and WHO priority pathogen, notable for multidrug resistance and biofilm",
    "formation."
  ),
  "Salmonella_enterica" = paste(
    "Salmonella enterica is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Enterobacteriaceae. Its many serovars",
    "cause foodborne gastroenteritis as well as typhoid and paratyphoid fever.",
    "It is among the most important and most heavily surveilled foodborne",
    "pathogens worldwide."
  ),
  "Serratia_marcescens" = paste(
    "Serratia marcescens is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Yersiniaceae, often producing a red",
    "pigment (prodigiosin). It is an opportunistic pathogen causing",
    "healthcare-associated urinary-tract, respiratory and bloodstream",
    "infections. It is a recurrent cause of outbreaks in neonatal intensive",
    "care units."
  ),
  "Staphylococcus_argenteus" = paste(
    "Staphylococcus argenteus is a Gram-positive coccus occurring in clusters,",
    "facultatively anaerobic, of the family Staphylococcaceae. A close relative",
    "of S. aureus and formerly misidentified as it, it causes similar skin,",
    "soft-tissue and invasive infections. It is distinguished mainly by genomic",
    "methods such as cgMLST."
  ),
  "Staphylococcus_aureus" = paste(
    "Staphylococcus aureus is a Gram-positive coccus occurring in clusters,",
    "facultatively anaerobic, of the family Staphylococcaceae. It is a leading",
    "cause of skin and soft-tissue, bloodstream, bone and device-associated",
    "infections, ranging from commensal carriage to invasive disease.",
    "Methicillin-resistant S. aureus (MRSA) is a major ESKAPE and WHO priority",
    "surveillance target."
  ),
  "Staphylococcus_capitis" = paste(
    "Staphylococcus capitis is a Gram-positive, clustered coccus, facultatively",
    "anaerobic, of the family Staphylococcaceae. A coagulase-negative skin",
    "commensal, it causes device-associated and bloodstream infections,",
    "including neonatal sepsis. The multidrug-resistant NRCS-A clone is of",
    "particular concern in neonatal units."
  ),
  "Streptococcus_pyogenes" = paste(
    "Streptococcus pyogenes is a Gram-positive coccus occurring in chains,",
    "facultatively anaerobic, of the family Streptococcaceae (Lancefield group",
    "A). It causes pharyngitis, scarlet fever, impetigo and severe invasive",
    "disease, as well as post-infectious sequelae such as rheumatic fever. It",
    "is a globally important pathogen under active surveillance."
  ),
  "Yersinia_enterocolitica" = paste(
    "Yersinia enterocolitica is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Yersiniaceae. It causes yersiniosis, a",
    "foodborne gastroenteritis often linked to pork, and can grow at",
    "refrigeration temperatures. It is a notable food-safety pathogen."
  ),
  "Citrobacter_freundii" = paste(
    "Citrobacter freundii is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Enterobacteriaceae. An environmental and",
    "gut organism, it is an opportunistic cause of urinary-tract, bloodstream",
    "and healthcare-associated infections. It is notable for AmpC-mediated and",
    "other antimicrobial resistance."
  ),
  "Citrobacter_freundii/portucalensis/braakii/europaeus" = paste(
    "The Citrobacter complex (Citrobacter sensu lato) comprises several closely",
    "related, Gram-negative, rod-shaped species of the family",
    "Enterobacteriaceae, including C. freundii, C. koseri and their relatives.",
    "These opportunistic pathogens cause urinary-tract, bloodstream and",
    "neonatal infections and carry clinically important resistance",
    "determinants. cgMLST is used to resolve the closely related species."
  ),
  "Enterobacter_hormaechei" = paste(
    "Enterobacter hormaechei is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Enterobacteriaceae and a member of the",
    "Enterobacter cloacae complex. It is an opportunistic nosocomial pathogen",
    "causing bloodstream, respiratory and urinary-tract infections, chiefly in",
    "intensive-care and immunocompromised patients. It is clinically notable",
    "for multidrug resistance, including ESBL/AmpC production and carbapenem",
    "resistance."
  ),
  "Morganella_morganii" = paste(
    "Morganella morganii is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Morganellaceae. A gut and environmental",
    "organism, it causes opportunistic urinary-tract, wound and bloodstream",
    "infections, mainly in healthcare settings. It is notable for intrinsic",
    "resistance to several antibiotics."
  ),
  "Proteus_mirabilis" = paste(
    "Proteus mirabilis is a Gram-negative, rod-shaped, facultatively anaerobic",
    "bacterium of the family Morganellaceae, known for its swarming motility.",
    "It is a common cause of complicated and catheter-associated urinary-tract",
    "infections, where its urease promotes struvite stone formation. It is",
    "frequently implicated in healthcare-associated infection."
  ),
  "Providencia_stuartii" = paste(
    "Providencia stuartii is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Morganellaceae. It is an opportunistic",
    "pathogen causing catheter-associated urinary-tract infections and",
    "bloodstream infections, especially in long-term-care patients. It is",
    "notable for intrinsic multidrug resistance."
  )
)

# --- merge into the metadata file ------------------------------------------

records <- fromJSON(json_path, simplifyVector = FALSE)
schemes <- read.csv(csv_path, stringsAsFactors = FALSE)

if (length(records) != nrow(schemes)) {
  stop("Record count (", length(records), ") != CSV rows (", nrow(schemes),
       "); cannot re-align by position.")
}

genus_of <- function(x) sub("[ _].*$", "", trimws(x))

# Re-align the existing (organism-stable) taxonomy to the current CSV labels by
# row position, then refresh species/abb. A genus sanity-check guards against a
# reordered CSV.
records <- Map(function(r, i) {
  if (!identical(genus_of(r$species), genus_of(schemes$species[i]))) {
    stop("Row ", i, " genus mismatch: JSON '", r$species,
         "' vs CSV '", schemes$species[i], "'. CSV order may have changed.")
  }
  r$species <- schemes$species[i]
  r$abb <- schemes$abb[i]
  r
}, records, seq_along(records))

missing <- setdiff(vapply(records, function(r) r$species, character(1)),
                   names(descriptions))
if (length(missing) > 0) {
  stop("No synthesized description for: ", paste(missing, collapse = ", "))
}

records <- lapply(records, function(r) {
  r$summary <- unname(descriptions[[r$species]])
  r$summary_source <- "synthesized"
  # Flag multi-taxon complex schemes (abb ends in _complex) for the UI badge.
  if (grepl("_complex$", r$abb)) {
    r$rank <- "complex"
  }
  r
})

writeLines(
  toJSON(records, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  json_path
)
message("Synthesized descriptions written for ", length(records), " records.")
