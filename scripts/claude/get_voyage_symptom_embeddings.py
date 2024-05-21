import voyageai
import voyage_config
import pandas as pd
import os

model = "voyage-large-2"
input_path = "/labs/khatrilab/solomonb/mcas/data/compiled_symptoms.csv"
output_path = f'/labs/khatrilab/solomonb/mcas/data/voyage_embeddings/{model}/symptom_embeddings_{model}.csv'

if not os.path.exists(os.path.dirname(output_path)):
    os.makedirs(os.path.dirname(output_path))

vo = voyageai.Client(
    # api_key='pa-8MuD7ydWcHVMPojySG9LAL7gC8UhxFtvVg8U-gj0tks'
    api_key = voyage_config.api_key
    )

df = pd.read_csv(input_path)
# df = df.head(10)
symptoms = list(df["symptom"])

def get_embeddings(symptoms, model = "voyage-large-2"):
    embeddings = vo.embed(
        symptoms,
        model=model,
        input_type="document",
    ).embeddings
    my_dict = {
        'symptom':symptoms,
        'embeddings':embeddings
    }
    return pd.DataFrame(my_dict)

batch_size = 128
embeddings = [
    get_embeddings(symptoms[i : i + batch_size], model = "voyage-large-2")
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