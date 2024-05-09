import os
from datetime import datetime
import anthropic
import pandas as pd
import json
import anthropic_config
import math

# Set Anthropic api key
client = anthropic.Anthropic(
    api_key=anthropic_config.api_key
)

# Create ChatGPT query from 
def build_query(symptom_string):
    out_query = f"For educational purposes, return a json list of format {{'diagnoses':[diagnosis list]}} with the top 10 diagnoses for the following combination of symptoms: {symptom_string}"
    return out_query

# Submit ChatGPT query
def diagnosis_query(query, claude_model = "claude-3-haiku-20240307"):
    try:
      response = client.messages.create(
          model=claude_model,
          max_tokens=1000,
          temperature=1,
          system="Good responses only provide the JSON list without additional text outside the JSON list",
          messages=[
              {
                  "role": "user",
                  "content": [
                      {
                          "type": "text",
                          "text": query
                      }
                  ]
              }
          ]
      )
      output = message.content[0].text
    except anthropic.APIError as e:
        print(f"Error: {e}\n{query=}")
        output = {'diagnoses':'error'}
    
    if output is not None:
        output = json.loads(output)    
    return output

# Create json object with query parameters and query output
## Output_query includes full query string in json output for debugging
def anthropic_wrapper(i, criteria, symptoms, claude_version="claude-3-haiku-20240307", output_query=False):
    query = build_query(symptoms)
    json_output = diagnosis_query(query, gpt_version)
    output = {'i':i, 'criteria':criteria, 'symptoms':[s.strip() for s in symptoms.split(",")]}
    if output_query:
        output.update({'query':query})
    if json_output is not None:
        output.update(json_output)
    return output

# Read all json files in the target json output folder into a single dataframe
def read_json_dir(json_dir):
    files =  os.listdir(json_dir)
    json_files = [file for file in files if file.endswith("json")]
    data_list = []
    for json_file in json_files:
        file_path = os.path.join(json_dir, json_file)
        with open(file_path, 'r') as file:
            try:
                json_data = pd.read_json(file)
                data_list.append(json_data)
            except json.JSONDecodeError as e:
                print(f"Error decoding {json_file}: {e}")
    output = pd.concat(data_list, ignore_index=True)
    return output

# Needed for remove_complete_iterations lambda function to specifically deal with nan
def count_diagnoses(x):
    if type(x) == float:
        return 0
    else: 
        return len(x)

# Remove succesfully completed iterations to ChatGPT and remove them from 
# dataframe of all iterations, leaving only unprocessed iterations
def remove_complete_iterations(iteration_path, json_dir, minimum_diagnoses = 5):
    iteration_df=pd.read_csv(iteration_path)
    
    ###### Limit to first 10,000 iterations due to GPT4 rates
    iteration_df = iteration_df.query('i <= 10000')
    
    print(f"Total iteration samples: {iteration_df.shape[0]}")
    # If there are no json files in the output path, return full iteration df
    if len(os.listdir(json_dir))==0:
        output_df = iteration_df
        print(f"Completed iteration samples: 0")
    # If there are json outputs, remove complete samples from full iteration df
    else:
        json_df=read_json_dir(json_dir)
        json_df['n_diagnoses'] =json_df.apply(lambda x: count_diagnoses(x['diagnoses']), axis = 1)
        json_df['complete'] =json_df.apply(lambda x: x['n_diagnoses'] >= minimum_diagnoses, axis = 1)
        print(f"Completed iteration samples: {json_df.shape[0]}")

        output_df = pd.merge(iteration_df, json_df[['criteria','i','complete']], how = 'left') \
        .query('complete != True')

    print(f"Remaining iteration samples: {output_df.shape[0]}")
    return output_df

def chatgpt_pipeline(iteration_path, output_dir, batch_size = 1000, gpt_version = "gpt-3.5-turbo-1106"):
    output_dir = f'/labs/khatrilab/solomonb/mcas/data/chatgpt_json_output/{gpt_version}'
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Read in iteration data, removing previously completed iterations
    df_iteration = remove_complete_iterations(
        iteration_path=iteration_path,
        json_dir=output_dir
    )
    
    # Calculate batch ids
    # batch_size = 1000 # Set total number of ChatGPT iterations to submit per save file
    criteria_number = len(df_iteration['criteria'].unique()) # Determine total number of criteria sets
    samples_per_criteria = batch_size / criteria_number # Determine number of iterations per criteria set to achieve batch size
    samples_per_criteria = int(samples_per_criteria) if samples_per_criteria >= 1 else 1 # Set minumum number iterations per criteria set

    # Add batch id to sampling data
    df_iteration = df_iteration.assign(batch = lambda x: x['i'] // samples_per_criteria)

    batches = df_iteration['batch'].unique()

    # print(df_iteration.shape[0])
    for i in batches:
        print(f"Starting batch {i+1} of {len(batches)} at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

        df = df_iteration.query(f'batch == {i}')

        print(f"\tProcessing {df.shape[0]} samples")

        # Map anthropic_wrapper to criteria iteration data contained in each row
        output = df \
            .apply(lambda row: anthropic_wrapper(
                criteria = row['criteria'], 
                i = str(row['i']), 
                symptoms = row['symptoms'],
                gpt_version = gpt_version),
                axis = 1)
            
        # print(list(output))

        json_path = output_dir+f"/chatgpt_output_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        print(json_path)

        # Write ChatGPT output to json file
        with open(json_path, 'w') as json_file:
            json.dump(list(output), json_file, indent=4)

# Run pipeline
chatgpt_pipeline(
        iteration_path="/labs/khatrilab/solomonb/mcas/data/criteria_query_iterations.csv",
        output_dir="/labs/khatrilab/solomonb/mcas/data/chatgpt_json_output",
        batch_size = 200, 
        gpt_version = "gpt-4-turbo-preview"
)
