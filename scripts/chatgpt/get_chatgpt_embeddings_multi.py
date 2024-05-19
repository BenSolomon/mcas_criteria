import openai
import pandas as pd
import os
import openai_config
from datetime import datetime
import time
import sys

# Set OpenAI api key
client = openai.OpenAI(
    api_key=openai_config.api_key
)


# model = "text-embedding-3-large"
embedding_model = "text-embedding-3-small"
# diagnosis_data = "claude3_haiku_t1.0"
# diagnosis_data = "claude3_opus_t1.0"
# diagnosis_data = "gemini1.0_pro_t1.0"
# diagnosis_data = "gpt3.5"
# diagnosis_data = "gpt4.0"

def get_embeddings(text_string, model_id = "text-embedding-3-small"):
    embedding = client.embeddings.create(input=text_string, model=model_id)
    embedding =  embedding.data[0].embedding
    embedding = list(zip(range(0,len(embedding)), embedding))
    return {x:y for x,y in embedding}

def get_embeddings_pipeline(model, diagnosis_data):
  
  print(f'READING DIAGNOSES - {diagnosis_data} - {datetime.now()}')
  sys.stdout.flush()
  df = pd.read_csv(f'/labs/khatrilab/solomonb/mcas/data/unique_diagnoses/unique_diagnoses_{diagnosis_data}.csv')
  # df = df.head(10)
  # time.sleep(5)
  
  
  print(f'GETTING EMBEDDINGS - {diagnosis_data} - {datetime.now()}')
  sys.stdout.flush()
  df["embeddings"] = df["diagnosis"].map(lambda x: get_embeddings(x, model_id = model))
  
  df = pd.concat(
      [
      df["diagnosis"],
      df["embeddings"].apply(pd.Series)
      ], axis = 1
  )
  
  print(f'WRITING EMBEDDINGS - {diagnosis_data} - {datetime.now()}')
  sys.stdout.flush()
  df.to_csv(f'/labs/khatrilab/solomonb/mcas/data/chatgpt_embeddings/{model}/{diagnosis_data}_diagnoses_chatgpt_embeddings.csv.gz', compression='gzip', index=False)
  

get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gpt3.5")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gpt4.0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "claude3_haiku_t1.0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "claude3_opus_t1.0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gemini1.0_pro_t1.0")
