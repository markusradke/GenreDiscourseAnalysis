# Cover Letter (still old)
Dear Editors,

We hereby submit our research article “Power to the People! Assigning Popular Music Genres Based on Listeners' Perspective Using a Genre Tree Approach” for consideration to be published in Transactions of the International Society for Music Information Retrieval.

Our work addresses fundamental methodological challenges in music genre recognition that cut across multiple domains in your journal's scope. Existing genre classification systems rely on commercial taxonomies, expert-curated ontologies, or mixed-source aggregations that lack explicit grounding in genre theory and introduce uncontrolled variance across platforms and research contexts. These inconsistencies degrade model generalizability, complicate cross-study comparison, and raise validity concerns, e.g. about whether classification targets reflect listener perception or institutional authority. To address these challenges, we present a reproducible computational framework that derives genre taxonomies from listener community discourse, bridging computational methods with theoretical foundations from popular music studies and cultural sociology.

The manuscript makes three primary contributions: First, we introduce COMGET, an algorithm that constructs balanced hierarchical trees from folksonomy tag co-occurrences, operationalizing genre-as-discourse through network analysis of community voting patterns. Second, we present FOLDCAT, a folding algorithm that generates optimally balanced categories at user-specified granularities, enabling scalable taxonomies for diverse research applications. Third, drawing on a large corpus of German popular music to which we applied both algorithms, we demonstrate external validity by comparing resulting categories against commonly used taxonomies in MIR and computational musicology research (Discogs, Rosamerica, Deezer). We also show generalizability by training supervised classifiers that reliably predict community-derived categories from multi-dimensional feature space (audio, metadata, lyrics, popularity). 

The work of the first author Markus Radke was funded by a scholarship grant for his PhD studies from Studienstiftung des deutschen Volkes e.V..

We confirm that all authors have agreed to submitting the paper to your journal and the article submission procedures have been followed. The article does not exeed the maximum word count of 8,000 including all notes and references. Please note that we also submit and additional appendix document and an interactive html result inspector in a ZIP file for supporting the review. 

The article has not been published elsewhere and reflects original research conducted by its authors. We have no conflicts of interest to disclose. All code, data processing pipelines, and supplementary materials will be made publicly available to support reproducibility. Please address all correspondence concerning this manuscript to me at markus.radke@tu-berlin.de. 

Thank you for considering this manuscript. We look forward to your editorial assessment.

Sincerely,
Markus Radke, 
Dr. Steffen Lepa


# Keywords
popular music genres, 
hierarchical taxonomy,
folksonomy,
network analysis,
machine learning,
German popular music

# Figure Captions
## Figure 1
Absolute and relative frequency of 20 most common *genre'* tags in the data set with respect to tracks.

## Figure 2 
Weighted Gini coefficient (0–1) across iterations of cutting weak genre links and reconnecting dangling subtrees to *POPULAR MUSIC* during COMGET-G tree balance optimization.

## Figure 3
Weighted Gini coefficient (0--1) across COMGET-G folding solutions for minimum genre sizes *s* from 1,000 to 15,000 in increments of 5, using the POPTRAG corpus.

## Figure 4 
Genre category systems for POPTRAG corpus at four different granularities (5, 12, 25, 32 *genre categories*), projected onto the original COMGET-G tree hierarchy. Colors indicate additional categories introduced at each finer levels of granularity (blue = 5; red = 12; green = 25; grey = 32). Node size represents genre size in POPTRAG (sum of vote-weighted track assignment probabilities).

## Figure 5a and 5b
Contingency tables comparing COMGET-G5 and COMGET-G12 genre assignments with  corresponding genre classification by taxonomies commonly used in MIR and computational musicology; values show *P(COMGET|External)*.  Categories with less than 2.5\% relative frequency in the corpus were discarded.

## Figure 6
Performance comparison for genre classification of different learners (GLMNET, RDA, RF, GBM) on 20\% of the POPTRAG data set for low (5) and medium (12) COMGET-G genre granularity; plots show mean and standard error of macro *F1* across CV folds.

## Figure 7a and 7b
Confusion matrices for the final LightGBM genre classification models for COMGET-G5 and COMGET-G12 on the holdout set. Values show *P(Predicted|Actual)*$; *certainty* indicates the mean assignment probability for each *genre category* across community votes.

## Figure 8a and 8b
Top 20 most important features for algorithmic genre classification of POPTRAG corpus at low (5) and medium (12) granularity of COMGET-G. Values show relative feature importance extracted from final LightGBM models.