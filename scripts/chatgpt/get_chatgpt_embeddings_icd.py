import openai
import pandas as pd
import os
import openai_config
import argparse

# Set OpenAI api key
client = openai.OpenAI(
    api_key=openai_config.api_key
)

parser = argparse.ArgumentParser()
parser.add_argument("letter", help="starting letter of ICD code", type=str)
args = parser.parse_args()

# print(args.letter)

# model = "text-embedding-3-large"
model = "text-embedding-3-small"

def get_embeddings(text_string, model_id = "text-embedding-3-small"):
    # model_id = "text-embedding-ada-002"
    embedding = client.embeddings.create(input=text_string, model=model_id)
    embedding =  embedding.data[0].embedding
    embedding = list(zip(range(0,len(embedding)), embedding))
    return {x:y for x,y in embedding}


df = pd.read_csv("/labs/khatrilab/solomonb/mcas/data/compiled_icd10_codes.csv")
df = df[df['code'].str.startswith(args.letter)]
# df = df.head(5)

# print(df)

df["embeddings"] = df["diagnosis"].map(lambda x: get_embeddings(x, model_id = model))

df = pd.concat(
    [df["code"],
    df["diagnosis"],
    df["embeddings"].apply(pd.Series)],
    axis = 1
)

df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/chatgpt_embeddings/icd_embeddings/icd10_{args.letter}_chatgpt_embeddings_{model}.csv.gz', compression='gzip', index=False)
