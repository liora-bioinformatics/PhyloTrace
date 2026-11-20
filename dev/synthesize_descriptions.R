# dev/synthesize_descriptions.R
#
# Replaces the existing summaries in species_metadata.json with
# concise, editorially synthesized descriptions written for the species
# catalogue of the database setup. Each description follows a fixed 3-sentence template:
#   1. identity & morphology (Gram reaction, shape, oxygen, family)
#   2. clinical / biological significance
#   3. relevance (transmission, AMR, biothreat or typing note)
#
# Taxonomy fields (TaxID, lineage) remain NCBI-verified and untouched; records
# are matched to their description by the `species` field. Entries whose
# abbreviation ends in "_complex", or species groups, describe several
# species-level taxa (tagged rank = "complex").
#
# Run from the project root:  Rscript dev/synthesize_descriptions.R

box::use(
  jsonlite[fromJSON, toJSON],
)

json_path <- "app/logic/data/species_metadata.json"

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
    "worldwide, with poultry as the principal reservoir. cgMLST is used",
    "to type both species and trace foodborne transmission."
  ),
  "Candidozyma_auris" = paste(
    "Candidozyma auris (formerly Candida auris) is an ascomycetous, budding",
    "yeast of the family Metschnikowiaceae. It is an emerging fungal pathogen",
    "that causes severe invasive candidiasis, bloodstream infections, and",
    "outbreaks in healthcare facilities. It is notorious for widespread multidrug",
    "resistance, environmental persistence, and rapid global dissemination."
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
  "Dermatophilus_congolensis" = paste(
    "Dermatophilus congolensis is a Gram-positive, facultatively anaerobic",
    "actinomycete of the family Dermatophilaceae that grows as branching",
    "filaments which fragment into motile, flagellated zoospores. It is the",
    "causative agent of dermatophilosis (also called streptothricosis, lumpy",
    "wool or strawberry footrot), an exudative, crusting dermatitis of cattle,",
    "sheep, goats and horses, with occasional zoonotic infection of humans.",
    "Infection is favoured by persistent moisture and skin damage and spreads",
    "by contact, arthropod vectors and fomites, making it an important",
    "veterinary pathogen in tropical and subtropical regions."
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
    "and antibiotic-associated haemorrhagic colitis. Genomic methods such",
    "as cgMLST are used to resolve the constituent species, which routine",
    "tests cannot separate."
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
  "Mycobacterium_tuberculosis/bovis/africanum/canettii" = paste(
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
    "soft-tissue and invasive infections. It is distinguished by",
    "MALDI-TOF with updated databases or by genomic methods."
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
    "The Citrobacter freundii complex groups closely related,",
    "Gram-negative, rod-shaped species of the family Enterobacteriaceae,",
    "including C. freundii, C. portucalensis, C. braakii and C.",
    "europaeus.",
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
  ),
  "Achromobacter_xylosoxidans" = paste(
    "Achromobacter xylosoxidans is a Gram-negative, aerobic, motile rod of",
    "the family Alcaligenaceae found in water and soil. It is an",
    "opportunistic pathogen, notably colonising the airways of people with",
    "cystic fibrosis and causing healthcare-associated infections. Its broad",
    "intrinsic and acquired antimicrobial resistance makes infections",
    "difficult to treat."
  ),
  "Aliarcobacter_butzleri" = paste(
    "Aliarcobacter butzleri is a Gram-negative, microaerophilic, curved rod",
    "of the family Arcobacteraceae, formerly placed in the genus Arcobacter.",
    "It is an emerging enteric pathogen associated with diarrhoeal illness in",
    "humans and animals. It is frequently found in poultry, other foods of",
    "animal origin and water, making it relevant to food-safety surveillance."
  ),
  "Avibacterium_paragallinarum" = paste(
    "Avibacterium paragallinarum is a Gram-negative, non-motile coccobacillus",
    "of the family Pasteurellaceae. It causes infectious coryza, an acute",
    "respiratory disease of chickens. Outbreaks reduce egg production and",
    "growth, and typing supports vaccine selection and outbreak tracing in",
    "poultry production."
  ),
  "Bacillus_cereus" = paste(
    "Bacillus cereus is a Gram-positive, rod-shaped, spore-forming,",
    "facultatively anaerobic bacterium of the family Bacillaceae and the",
    "namesake of the B. cereus group. It causes emetic and diarrhoeal food",
    "poisoning through the preformed emetic toxin cereulide and",
    "diarrhoeal enterotoxins, and occasionally invasive",
    "infections. Its heat-resistant spores make it a persistent concern in",
    "food production."
  ),
  "Burkholderia_cepacia" = paste(
    "Burkholderia cepacia is a Gram-negative, aerobic rod of the family",
    "Burkholderiaceae and the namesake of the B. cepacia complex. It is an",
    "opportunistic pathogen of people with cystic fibrosis or chronic",
    "granulomatous disease and a cause of healthcare-associated outbreaks,",
    "often linked to contaminated products. It is intrinsically resistant to",
    "many antibiotics and disinfectants."
  ),
  "Chlamydia_abortus" = paste(
    "Chlamydia abortus is a Gram-negative, obligate intracellular bacterium",
    "of the family Chlamydiaceae. It causes enzootic abortion in sheep and",
    "goats, a major cause of reproductive losses in small ruminants. It is",
    "zoonotic and can cause severe illness and miscarriage in pregnant women",
    "exposed to infected animals."
  ),
  "Chlamydia_trachomatis" = paste(
    "Chlamydia trachomatis is a Gram-negative, obligate intracellular",
    "bacterium of the family Chlamydiaceae. It is the most common bacterial",
    "cause of sexually transmitted infections and, in endemic regions, causes",
    "trachoma, the leading infectious cause of blindness. Genotyping supports",
    "the surveillance of transmission networks and lymphogranuloma venereum",
    "strains."
  ),
  "Clostridium_chauvoei" = paste(
    "Clostridium chauvoei is a Gram-positive, rod-shaped, spore-forming,",
    "anaerobic bacterium of the family Clostridiaceae. It causes blackleg, an",
    "acute and usually fatal myonecrosis of cattle and sheep. Its long-lived",
    "spores persist in soil, and outbreaks are controlled by vaccination."
  ),
  "Clostridium_neonatale" = paste(
    "Clostridium neonatale is a Gram-positive, rod-shaped, spore-forming,",
    "anaerobic bacterium of the family Clostridiaceae. It colonises the gut",
    "of newborns and has been associated with necrotising enterocolitis in",
    "neonatal intensive care units. Typing helps investigate clusters among",
    "hospitalised infants."
  ),
  "Corynebacterium_ulcerans" = paste(
    "Corynebacterium ulcerans is a Gram-positive, rod-shaped, facultatively",
    "anaerobic bacterium of the family Corynebacteriaceae. Toxigenic strains",
    "carry the diphtheria toxin gene and cause diphtheria-like illness in",
    "humans. It is zoonotic, often acquired from companion or farm animals,",
    "and is notifiable in many countries."
  ),
  "Dichelobacter_nodosus" = paste(
    "Dichelobacter nodosus is a Gram-negative, anaerobic rod of the family",
    "Cardiobacteriaceae. It is the primary cause of footrot, a contagious",
    "hoof disease of sheep and goats. Footrot causes major welfare and",
    "economic losses, and strain typing supports control and eradication",
    "programmes."
  ),
  "Enterobacter_bugandensis" = paste(
    "Enterobacter bugandensis is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Enterobacteriaceae and a member of the",
    "E. cloacae complex. It was first described from neonatal sepsis cases",
    "and has since been found in hospital and environmental settings.",
    "Multidrug-resistant strains make it relevant to infection control."
  ),
  "Enterobacter_cloacae" = paste(
    "Enterobacter cloacae is a Gram-negative, rod-shaped, facultatively",
    "anaerobic bacterium of the family Enterobacteriaceae and the namesake of",
    "the E. cloacae complex. It is a common cause of healthcare-associated",
    "infections of the urinary tract, bloodstream and lower respiratory",
    "tract. It readily acquires resistance, including carbapenemases, and",
    "belongs to the ESKAPE pathogens."
  ),
  "Haemophilus_influenzae" = paste(
    "Haemophilus influenzae is a Gram-negative, facultatively anaerobic",
    "coccobacillus of the family Pasteurellaceae. Encapsulated type b strains",
    "cause meningitis and epiglottitis in unvaccinated children, while",
    "non-typeable strains cause otitis media, sinusitis and pneumonia.",
    "Surveillance tracks vaccine impact and the spread of resistant lineages."
  ),
  "Haemophilus_parainfluenzae" = paste(
    "Haemophilus parainfluenzae is a Gram-negative, facultatively anaerobic",
    "coccobacillus of the family Pasteurellaceae, reclassified by NCBI as",
    "Parahaemophilus parainfluenzae. It is a commensal of the human upper",
    "respiratory tract that occasionally causes endocarditis and other",
    "opportunistic infections. It is also a reservoir of antimicrobial",
    "resistance genes shared with H. influenzae."
  ),
  "Legionella_longbeachae" = paste(
    "Legionella longbeachae is a Gram-negative, aerobic rod of the family",
    "Legionellaceae. It causes Legionnaires' disease and Pontiac fever,",
    "mainly through exposure to potting soil and compost rather than water",
    "systems. It is a leading cause of legionellosis in Australia and New",
    "Zealand."
  ),
  "Mycobacterium_avium" = paste(
    "Mycobacterium avium is an acid-fast, slow-growing, rod-shaped bacterium",
    "of the family Mycobacteriaceae and a member of the M. avium complex. It",
    "causes chronic pulmonary disease in people with lung conditions and",
    "disseminated infection in immunocompromised patients; subspecies",
    "paratuberculosis causes Johne's disease in ruminants. It is acquired",
    "from the environment, especially water and soil."
  ),
  "Mycobacterium_chimaera" = paste(
    "Mycobacterium chimaera is an acid-fast, slow-growing, rod-shaped",
    "bacterium of the family Mycobacteriaceae, classified by NCBI as a",
    "subspecies of M. intracellulare. It became known through a global",
    "outbreak of invasive infections after open-heart surgery, traced to",
    "contaminated heater-cooler units. Whole-genome typing was central to",
    "linking these cases."
  ),
  "Mycobacterium_intracellulare" = paste(
    "Mycobacterium intracellulare is an acid-fast, slow-growing, rod-shaped",
    "bacterium of the family Mycobacteriaceae and a member of the M. avium",
    "complex. It is a common cause of non-tuberculous mycobacterial lung",
    "disease, particularly in older adults with underlying lung disease. It",
    "is acquired from environmental sources such as water and soil."
  ),
  "Neisseria_gonorrhoeae" = paste(
    "Neisseria gonorrhoeae is a Gram-negative, aerobic diplococcus of the",
    "family Neisseriaceae. It causes gonorrhoea, one of the most common",
    "sexually transmitted infections, with complications including pelvic",
    "inflammatory disease and infertility. Its rapidly emerging resistance to",
    "successive antibiotic classes makes it a WHO priority pathogen."
  ),
  "Neisseria_lactamica" = paste(
    "Neisseria lactamica is a Gram-negative, aerobic diplococcus of the",
    "family Neisseriaceae. It is a harmless commensal of the human",
    "nasopharynx, most common in young children. Its carriage is thought to",
    "contribute to natural immunity against N. meningitidis, which makes it",
    "of interest for meningococcal research."
  ),
  "Neisseria_meningitidis" = paste(
    "Neisseria meningitidis is a Gram-negative, aerobic diplococcus of the",
    "family Neisseriaceae. It causes invasive meningococcal disease,",
    "including meningitis and septicaemia, which can be rapidly fatal.",
    "Genomic surveillance through PubMLST underpins outbreak detection and",
    "vaccine strategy."
  ),
  "Pasteurella_multocida" = paste(
    "Pasteurella multocida is a Gram-negative, facultatively anaerobic",
    "coccobacillus of the family Pasteurellaceae. It causes fowl cholera,",
    "haemorrhagic septicaemia in cattle and atrophic rhinitis in pigs, and",
    "wound infections in humans after animal bites. Typing supports",
    "veterinary outbreak investigation and vaccine selection."
  ),
  "Staphylococcus_epidermidis" = paste(
    "Staphylococcus epidermidis is a Gram-positive, facultatively anaerobic",
    "coccus of the family Staphylococcaceae. It is a skin commensal and the",
    "leading cause of infections associated with catheters, prosthetic joints",
    "and other implanted devices, aided by biofilm formation.",
    "Hospital-adapted lineages are frequently multidrug-resistant."
  ),
  "Streptococcus_agalactiae" = paste(
    "Streptococcus agalactiae (group B Streptococcus) is a Gram-positive,",
    "facultatively anaerobic coccus of the family Streptococcaceae. It is a",
    "leading cause of neonatal sepsis and meningitis, also causes invasive",
    "disease in older adults, and causes mastitis in cattle. Surveillance",
    "supports maternal screening and vaccine development."
  ),
  "Streptococcus_pneumoniae" = paste(
    "Streptococcus pneumoniae is a Gram-positive, facultatively anaerobic",
    "diplococcus of the family Streptococcaceae. It is a major cause of",
    "pneumonia, meningitis, bacteraemia and otitis media worldwide,",
    "particularly in young children and older adults. Genomic surveillance",
    "tracks serotype replacement after vaccination and the spread of",
    "resistant clones."
  ),
  "Streptococcus_uberis" = paste(
    "Streptococcus uberis is a Gram-positive, facultatively anaerobic coccus",
    "of the family Streptococcaceae. It is one of the most common causes of",
    "bovine mastitis worldwide, with largely environmental transmission.",
    "Typing helps distinguish persistent herd strains from sporadic",
    "environmental infections."
  ),
  "Vibrio_cholerae" = paste(
    "Vibrio cholerae is a Gram-negative, facultatively anaerobic, curved rod",
    "of the family Vibrionaceae. Toxigenic O1 and O139 strains cause cholera,",
    "an acute watery diarrhoea that can kill within hours without",
    "rehydration. Genomic surveillance traces the spread of pandemic lineages",
    "and resistance."
  ),
  "Vibrio_parahaemolyticus" = paste(
    "Vibrio parahaemolyticus is a Gram-negative, facultatively anaerobic,",
    "curved rod of the family Vibrionaceae living in warm coastal and",
    "estuarine waters. It is a leading cause of seafood-associated",
    "gastroenteritis, especially from raw or undercooked shellfish. Typing",
    "tracks pandemic clones and outbreaks linked to aquaculture."
  ),
  "Xanthomonas_citri" = paste(
    "Xanthomonas citri is a Gram-negative, aerobic rod of the family",
    "Lysobacteraceae. Its pathovar citri causes citrus canker, a quarantine",
    "disease that damages fruit and leaves and leads to costly eradication",
    "campaigns. Typing supports the tracing of outbreaks and plant movements."
  ),
  "Yersinia_pestis" = paste(
    "Yersinia pestis is a Gram-negative, facultatively anaerobic",
    "coccobacillus of the family Yersiniaceae. It causes plague in its",
    "bubonic, septicaemic and pneumonic forms and persists in rodent",
    "reservoirs transmitted by fleas. It is a high-consequence biothreat",
    "agent, and genotyping supports outbreak and forensic investigations."
  ),
  "Yersinia_pseudotuberculosis" = paste(
    "Yersinia pseudotuberculosis is a Gram-negative, facultatively anaerobic",
    "coccobacillus of the family Yersiniaceae. It causes enteric yersiniosis",
    "with abdominal pain that can mimic appendicitis, mostly through",
    "contaminated food or water. Y. pestis evolved from it as a recent",
    "clone, and it is monitored in foodborne outbreak surveillance."
  ),
  "Borrelia_spp" = paste(
    "Borrelia is a genus of helical, motile spirochaetes of the family",
    "Borreliaceae transmitted by ticks and lice. It includes the agents of",
    "Lyme borreliosis (the B. burgdorferi sensu lato complex) and of",
    "relapsing fever. Typing distinguishes genospecies and tracks their",
    "geographic distribution."
  ),
  "Elizabethkingia_spp" = paste(
    "Elizabethkingia is a genus of Gram-negative, aerobic, non-motile rods of",
    "the family Weeksellaceae found in soil and water. Species such as E.",
    "anophelis and E. meningoseptica cause neonatal meningitis and",
    "healthcare-associated bloodstream infections. They are intrinsically",
    "resistant to many antibiotics, and typing supports outbreak",
    "investigation."
  ),
  "Leptospira_spp" = paste(
    "Leptospira is a genus of thin, helical, motile spirochaetes of the",
    "family Leptospiraceae. Pathogenic species cause leptospirosis, a",
    "zoonosis acquired through contact with water or soil contaminated by the",
    "urine of infected animals. Severe forms include Weil's disease and",
    "pulmonary haemorrhage, and typing supports epidemiological tracing of",
    "reservoirs."
  ),
  "Shewanella_spp" = paste(
    "Shewanella is a genus of Gram-negative, facultatively anaerobic, motile",
    "rods of the family Shewanellaceae common in marine and freshwater",
    "environments. Species such as S. algae and S. putrefaciens occasionally",
    "cause skin, soft-tissue and ear infections, usually after seawater",
    "exposure. The genus is also a natural reservoir of resistance genes,",
    "including carbapenemase precursors."
  )
)

# --- merge into the metadata file ------------------------------------------

records <- fromJSON(json_path, simplifyVector = FALSE)

missing <- setdiff(
  vapply(records, function(r) r$species, character(1)),
  names(descriptions)
)
if (length(missing) > 0) {
  stop("No synthesized description for: ", paste(missing, collapse = ", "))
}

records <- lapply(records, function(r) {
  r$summary <- unname(descriptions[[r$species]])
  r$summary_source <- "synthesized"
  # Flag multi-taxon complexes (abb ends in _complex, or a species group) for
  # the UI badge.
  if (grepl("_complex$", r$abb) || grepl("/", r$species, fixed = TRUE)) {
    r$rank <- "complex"
  }
  r
})

writeLines(
  toJSON(records, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  json_path
)
message("Synthesized descriptions written for ", length(records), " records.")
