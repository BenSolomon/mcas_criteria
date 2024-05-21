from mistralai.client import MistralClient
import mistral_config
import pandas as pd
import os

model = "mistral-embed"
input_path = "/labs/khatrilab/solomonb/mcas/data/compiled_symptoms.csv"
output_path = f'/labs/khatrilab/solomonb/mcas/data/mistral_embeddings/{model}/symptom_embeddings_{model}.csv'

if not os.path.exists(os.path.dirname(output_path)):
    os.makedirs(os.path.dirname(output_path))

client = MistralClient(api_key=mistral_config.api_key)

df = pd.read_csv(input_path)
# df = df.head(10)
symptoms = list(df["symptom"])

def get_embeddings(symptoms, model = "mistral-embed"):
    embeddings_batch_response = client.embeddings(model=model, input=symptoms)
    embeddings = [x.embedding for x in embeddings_batch_response.data]

    my_dict = {
        'symptom':symptoms,
        'embeddings':embeddings
    }
    return pd.DataFrame(my_dict)

batch_size = 128
embeddings = [
    get_embeddings(symptoms[i : i + batch_size], model = "mistral-embed")
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