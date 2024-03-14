import openai
import pandas as pd
import os
import openai_config

# Set OpenAI api key
client = openai.OpenAI(
    api_key=openai_config.api_key
)


# model = "text-embedding-3-large"
model = "text-embedding-3-small"

# def get_embeddings(text_string, model_id = "text-embedding-3-small"):
#     # model_id = "text-embedding-ada-002"
#     embedding = openai.Embedding.create(input=text_string, model=model_id)
#     embedding =  embedding['data'][0]['embedding']
#     embedding = list(zip(range(0,len(embedding)), embedding))
#     return {x:y for x,y in embedding}

# df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_all_chatgpt_diagnoses.csv")
# df = df.drop("criteria", axis=1)
# df = df.drop_duplicates()
# 
# # df = df.iloc[0:20,:]
# 
# df["embeddings"] = df["diagnosis"].map(lambda x: get_embeddings(x))
# 
# df = pd.concat(
#     [df["diagnosis"],
#     df["embeddings"].apply(pd.Series)],
#     axis = 1
# )
# 
# df = df.melt(id_vars="diagnosis", var_name = "index", value_name="embedding")
# df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/diagnosis_chatgpt_embeddings_{model}.csv', index=False)

def get_embeddings(text_string, model_id = "text-embedding-3-small"):
    # model_id = "text-embedding-ada-002"
    embedding = client.embeddings.create(input=text_string, model=model_id)
    embedding =  embedding.data[0].embedding
    embedding = list(zip(range(0,len(embedding)), embedding))
    return {x:y for x,y in embedding}


# df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_symptoms.csv")
# df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_diagnoses.csv")
df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_icd10_codes.csv")
# df = df.head(100)

# df["embeddings"] = df["symptom"].map(lambda x: get_embeddings(x, model_id = model))
df["embeddings"] = df["diagnosis"].map(lambda x: get_embeddings(x, model_id = model))

df = pd.concat(
    # [df["symptom"],
    [df["code"],
    df["diagnosis"],
    df["embeddings"].apply(pd.Series)],
    axis = 1
)

# df = df.melt(id_vars="symptom", var_name = "index", value_name="embedding")
# df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/symptom_chatgpt_embeddings_{model}.csv', index=False)
# df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/diagnosis_chatgpt_embeddings_{model}.csv', index=False)
df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/icd10_chatgpt_embeddings_{model}.csv.gz', compression='gzip', index=False)
