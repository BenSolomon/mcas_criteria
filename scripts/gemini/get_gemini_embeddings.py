import google.generativeai as genai
import gemini_config
import pandas as pd
import os

model = "models/text-embedding-004"
model_name = os.path.basename(model)
input_path = "/labs/khatrilab/solomonb/mcas/data/compiled_symptoms.csv"
output_path = f'/labs/khatrilab/solomonb/mcas/data/gemini_embeddings/{model_name}/symptom_embeddings_{model_name}.csv'

if not os.path.exists(os.path.dirname(output_path)):
    os.makedirs(os.path.dirname(output_path))

genai.configure(api_key=gemini_config.api_key)

df = pd.read_csv(input_path)
# df = df.head(10)
symptoms = list(df["symptom"])

def get_embeddings(symptoms, model = "models/text-embedding-004"):
    embeddings = genai.embed_content(model=model,content=symptoms,task_type="clustering")
    embeddings = embeddings['embedding']

    my_dict = {
        'symptom':symptoms,
        'embeddings':embeddings
    }
    return pd.DataFrame(my_dict)

batch_size = 128
embeddings = [
    get_embeddings(symptoms[i : i + batch_size], model = "models/text-embedding-004")
    for i in range(0, len(symptoms), batch_size)
]

df = pd.concat(embeddings, ignore_index = True)

df = pd.concat(
    [df["symptom"],
    df["embeddings"].apply(pd.Series)],
    axis = 1
)

# print(df)

df.to_csv(output_path, index=False)