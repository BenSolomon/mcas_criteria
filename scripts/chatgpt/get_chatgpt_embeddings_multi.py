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

# embedding_model = "text-embedding-3-large"
embedding_model = "text-embedding-3-small"

def get_embeddings(text_string, model_id = "text-embedding-3-small"):
    embedding = client.embeddings.create(input=text_string, model=model_id)
    embedding =  embedding.data[0].embedding
    embedding = list(zip(range(0,len(embedding)), embedding))
    return {x:y for x,y in embedding}

def get_embeddings_pipeline(model, diagnosis_data):
  
  print(f'READING DIAGNOSES - {diagnosis_data} - {datetime.now()}')
  sys.stdout.flush()
  df = pd.read_csv(f'/labs/khatrilab/solomonb/mcas/data/unique_diagnoses/unique_diagnoses_{diagnosis_data}.csv.gz')
  
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
  
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gpt-3.5-turbo-1106")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gpt-4-turbo-preview")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "claude-3-haiku-20240307_t1-0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "claude-3-opus-20240229_t1-0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gemini-1.0-pro-002_t1-0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gemini-1.5-flash-preview-0514_t1-0")
get_embeddings_pipeline(model = embedding_model, diagnosis_data = "gemini-1.5-pro-001_t1-0")
