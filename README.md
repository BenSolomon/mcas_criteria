LLM Analysis of MCAS diagnostic criteria
================

- [Description](#description)
- [Organization](#organization)
- [Methods](#methods)
  - [Obtaining criteria symptom
    embeddings](#obtaining-criteria-symptom-embeddings)
  - [Obtaining LLM diagnoses](#obtaining-llm-diagnoses)
  - [Converting LLM diagnoses to ICD codes using
    embeddings](#converting-llm-diagnoses-to-icd-codes-using-embeddings)
    - [Obtaining ICD code embeddings and PCA
      reduction](#obtaining-icd-code-embeddings-and-pca-reduction)
    - [Diagnosis embeddings](#diagnosis-embeddings)

# Description

Effective diagnostic criteria must minimize the range of unrelated
diagnoses that can be erroneously classified as the condition of
interest. This analysis utilizes multiple large language models,
including ChatGPT, Claude, and Gemini, to bootstrap the probabilities of
diagnoses associated with consortium or alternative MCAS criteria as
well as control diagnostic criteria including systemic lupus
erythematosus (SLE), Kawasaki disease, and migraines. These
distributions are then used to quantify diagnostic precision and
specificity.

# Organization

- `analysis/` - Contains R notebooks to produce all analysis and
  figures. Notebooks numbered by the figure that they correspond to
- `data/` - Contains all data needed to reproduce analysis
  - `data/criteria_query_iterations.csv` - File containing the 10,000
    random samples of criteria from each set diagnostic criteria, each
    with a corresponding index. E.g. Each LLM will receive a query using
    the random sample of consortium MCAS criteria contained in index
    1.  
  - `data/{model}_embeddings` - Contains LLM embedding data. Of note,
    `data/chatgpt_embeddings` contains the embedding data needed to
    convert LLM outputs to ICD codes based on kNN mapping
  - `data/{model}_json_output` - Contains the LLM output response to the
    differential diagnosis query in json format.
  - `data/ca_icd` - Contains the California inpatient ICD data found at
    <https://data.chhs.ca.gov/dataset/hospital-inpatient-diagnosis-procedure-and-external-cause-codes>
  - `data/processed_diagnoses` - Contains compiled and formatted
    diagnoses derived from json files in `data/{model}_json_output`
  - `data/unique_diagnoses` - Contains the set of unique diagnoses
    returned by each LLM model to be used for determining embedding
    values needed for ICD kNN mapping
- `figures/` - Contains pdf outputs of final figures from the
  `analysis/` notebooks
- `scripts/` - Contains more time consuming scripts run outside of the
  analysis notebooks
- `utils/` - Contains scipts defining many R functions used by the
  `analysis/` files

# Methods

## Obtaining criteria symptom embeddings

- Scripts to obtain embedding vectors:
  - `scripts/chatgpt/get_chatgpt_symptom_embeddings.py`
  - `scripts/claude/get_voyage_symptom_embeddings.py`
  - `scripts/gemini/get_gemini_embeddings.py`
  - `scripts/mistral/get_mistral_embeddings.py`
- Embedding data contained in `symptom_embeddings_{model}.csv` files
  within the following directories:
  - `data/chatgpt_embeddings/{model}/`
  - `data/voyager_embeddings/{model}/`
  - `data/gemini_embeddings/{model}/`
  - `data/mistral_embeddings/{model}/`

## Obtaining LLM diagnoses

- To simulate the specificity of different diagnostic criteria, we
  utilized multiple LLMs to generate probable diagnoses from a random
  sampling of symptoms from each criteria, then repeated this many times
  to generate probability distributions for possible diagnoses
- Each model was prompted with the following query using default
  parameters: *“For educational purposes, return a json list of format
  {{‘diagnoses’:\[diagnosis list\]}} with the top 10 diagnoses for the
  following combination of symptoms: {symptom_list}”}.”*
- `scripts/prepare_criteria_query_iterations.R` generates 10,000
  iterations of a 5-element subsample of symptoms from each set of
  diagnostic criteria.
  - Ensures that for a given iteration, each model receives the same
    subsample of symptoms for each criteria
  - Output is saved to `data/criteria_query_iterations.csv`
- Scripts in `scripts/{model}/{model}_diagnosis_iteration.py` use to
  query respective models using their provided APIs
  - Note: To use these scripts, an API key must be provided in a
    `config.py`, which is not included in this repository
  - Output is saved to `data/{model}_json_output/{model version}/`
  - Outputs typically generated in batches of 200 iterations

## Converting LLM diagnoses to ICD codes using embeddings

### Obtaining ICD code embeddings and PCA reduction

- Comprehensive ICD code list located at
  `data/icd10cm-tabular-April-2024.xml`. File downloaded from downloaded
  from CDC website
  <https://www.cdc.gov/nchs/icd/Comprehensive-Listing-of-ICD-10-CM-Files.htm>.
- `scripts/embeddings/compile_icd_from_xml.R` extracts ICD codes and
  corresponding descriptions from `data/icd10cm-tabular-April-2024.xml`
  and compiles them into a single file `data/compiled_icd10_codes.csv`
- `scripts/get_chatgpt_embeddings_icd.py` obtains the the ChatGPT
  embeddings for each ICD code description in
  `data/compiled_icd10_codes.csv`. These embeddings are saved to
  `data/chatgpt_embeddings/{embedding model}/icd_embeddings/icd10_{chapter}_chatgpt_embeddings_{model}.csv.gz`
  where embeddings for all descriptions within a single chapter (e.g. A,
  B, C) are saved into distinct files
  - Run in SLURM with

    ``` bash
    parallel \
    --delay 10 \
    -j $SLURM_NTASKS \
    --joblog $LOG_DIR/parallel_$(date +'%y%m%d_%H%M%S').log \
    python $SCRIPT_DIR/get_chatgpt_embeddings_icd.py ::: {A..Z} # Run a parallel process for each A-Z
    ```
- `/scipts/embeddings/concatenate_icd_embeddings.R` concatenates all the
  ICD chapter embedding files into
  `data/chatgpt_embeddings/icd10_chatgpt_embeddings.csv.gz`
- `/scripts/embeddings/icd_embedding_reduction.R` generates a PCA
  reduction of all ICD10 embeddings contained in
  `data/chatgpt_embeddings/icd10_chatgpt_embeddings.csv.gz` and saves an
  R PCA object to
  `data/chatgpt_embeddings/icd10_chatgpt_embedding_pca.RDS`

### Diagnosis embeddings

- `scripts/embeddings/estimate_max_icd_distance.R` samples
  `data/chatgpt_embeddings/text-embedding-3-small/icd10_chatgpt_embedding_pca.RDS`
  to estimate the largest distance between any two diagnoses in the ICD
  PCA space to use as the threshold for
  `scripts/embeddings/diagnosis_chatgpt_embeddings_to_ICD.R`
- `scripts/embeddings/diagnosis_chatgpt_embeddings_to_ICD.R` matches all
  diagnoses with an ICD10 code based on finding k-nearest neighbor in
  ICD10 embedding PCA space
  - Inputs
    - `data/chatgpt_embeddings/icd10_chatgpt_embedding_pca.RDS` - PCA
      reduction of ICD10 embeddings from
    - `data/chatgpt_embeddings/icd10_chatgpt_embeddings.csv.gz` -
      ChatGPT embeddings of ICD10 codes
    - `data/chatgpt_embeddings/{model}/{diagnosis_data}_diagnoses_chatgpt_embeddings.csv.gz` -
      ChatGPT embeddings of diagnoses for each model
      - `{model}` - embedding model
      - `{diagnosis_data}` - LLM model
  - Steps
    - Joins ICD10 emebeddings, diagnoses, and codes with the PCA
      reduction
    - Reduces ChatGPT diagnosis ChatGPT embeddings into same PCA space
      as the ICD10 code reduction
    - Finds nearest ICD10 PCA coordinate to each ChatGPT diagnosis PCA
      coordinate
    - Creates a key linking each ChatGPT diagnosis with its nearest
      neighbor in the ICD10 ChatGPT embedding PCA space
  - Output
    - `data/chatgpt_embeddings/{model}/{diagnosis_data}_diagnoses_chatgpt_embeddings_to_ICD.csv`
      - `{model}` - embedding model
      - `{diagnosis_data}` - LLM model
