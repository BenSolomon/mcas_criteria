import openai
import pandas as pd
import os
import openai_config

# Set OpenAI api key
client = openai.OpenAI(
    api_key=openai_config.api_key
)


model = "text-embedding-3-large"
# model = "text-embedding-3-small"

def get_embeddings(text_string, model_id = "text-embedding-3-small"):
    embedding = client.embeddings.create(input=text_string, model=model_id)
    embedding =  embedding.data[0].embedding
    embedding = list(zip(range(0,len(embedding)), embedding))
    return {x:y for x,y in embedding}


df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_symptoms.csv")

df["embeddings"] = df["symptom"].map(lambda x: get_embeddings(x, model_id = model))

df = pd.concat(
    [df["symptom"],
    df["embeddings"].apply(pd.Series)],
    axis = 1
)

df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/symptom_chatgpt_embeddings_{model}.csv', index=False)
