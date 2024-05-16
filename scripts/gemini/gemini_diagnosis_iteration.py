import os
from datetime import datetime
import google.generativeai as genai
import pandas as pd
import json
import gemini_config
import math
from time import sleep

# Set Gemini api key
genai.configure(api_key=gemini_config.api_key)

# Create Gemini query from 
def build_query(symptom_string):
    out_query = f"For educational purposes, return a json list of format {{'diagnoses':[diagnosis list]}} with the top 10 diagnoses for the following combination of symptoms: {symptom_string}"
    # out_query = out_query + '  Do not use markdown'
    return out_query

# Function that corrects quotes, double-quotes, apostrophes, and escape characters
# To convert gemini output string to string suitable for json conversion
def format_gemini_json(string):
    # print(string)
    string_list = string[string.find('[')+1:string.find(']')].split(',')
    string_list = [x.replace("\\'","").replace('"','').replace("'",'').strip() for x in string_list]
    string_dict = {'diagnoses':string_list}
    # print(string_dict)
    return(string_dict)


# Submit Gemini query
def diagnosis_query(query, gemini_model = "gemini-1.0-pro", temp = 1.0):
    generation_config = {
        "temperature": temp,
        # "top_p": 1,
        # "top_k": 0,
        "max_output_tokens": 2048,
        "response_mime_type": "application/json",
    }
    
    safety_settings = [
      {
        "category": "HARM_CATEGORY_HARASSMENT",
        "threshold": "BLOCK_NONE",
      },
      {
        "category": "HARM_CATEGORY_HATE_SPEECH",
        "threshold": "BLOCK_NONE",
      },
      {
        "category": "HARM_CATEGORY_SEXUALLY_EXPLICIT",
        "threshold": "BLOCK_NONE",
      },
      {
        "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
        "threshold": "BLOCK_NONE",
      },
    ]

    model = genai.GenerativeModel(
      model_name=gemini_model,
      generation_config=generation_config,
      safety_settings=safety_settings
    )
    
    try:
      message = model.generate_content(query)
      output = message.text
      output = format_gemini_json(output)

        # Error handling
    except Exception as e:
        print(f"Error: {e}\n{query=}")
        output = {"diagnoses":"error"}
    
    # # Combine output data into dict
    # if output is not None:
    #     output = {
    #         'json':output,
    #         'header':header,
    #         'response':response
    #     }
    return output

# Create json object with query parameters and query output
## Output_query includes full query string in json output for debugging
def gemini_wrapper(i, criteria, symptoms, gemini_version="gemini-1.0-pro", temp = 1.0, output_query=False, sleep_time = None):
    inputs=locals()
    query = build_query(symptoms)
    query_output = diagnosis_query(query, gemini_version, temp = temp)
    # print(query_output)
    # json_output = query_output['json']
    output = {'i':i, 'criteria':criteria, 'symptoms':[s.strip() for s in symptoms.split(",")]}
    if output_query:
        output.update({'query':query})
    if query_output is not None:
        output.update(query_output)

    # Create log of all query information
    log = {
        'inputs':inputs,
        # 'header':query_output['header'],
        # 'response':query_output['response'],
        'output':query_output
    }
    print(log)
    
    if sleep_time is not None:
        sleep(sleep_time)

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

# Remove succesfully completed iterations to Gemini and remove them from 
# dataframe of all iterations, leaving only unprocessed iterations
def remove_complete_iterations(iteration_path, json_dir, minimum_diagnoses = 5):
    iteration_df=pd.read_csv(iteration_path)
    
    ###### Limit to first 10,000 iterations due to Gemini rates
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

def gemini_pipeline(iteration_path, output_dir, batch_size = 1000, gemini_version = "gemini-1.0-pro", temp = 1.0, maximum_batches = None, sleep_time = None):
    output_dir = f'/labs/khatrilab/solomonb/mcas/data/gemini_json_output/{gemini_version}_t{str(temp).replace(".","-")}'
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Read in iteration data, removing previously completed iterations
    df_iteration = remove_complete_iterations(
        iteration_path=iteration_path,
        json_dir=output_dir
    )
    
    # Calculate batch ids
    # batch_size = 1000 # Set total number of Gemini iterations to submit per save file
    criteria_number = len(df_iteration['criteria'].unique()) # Determine total number of criteria sets
    samples_per_criteria = batch_size / criteria_number # Determine number of iterations per criteria set to achieve batch size
    samples_per_criteria = int(samples_per_criteria) if samples_per_criteria >= 1 else 1 # Set minumum number iterations per criteria set

    # Add batch id to sampling data
    df_iteration = df_iteration.assign(batch = lambda x: x['i'] // samples_per_criteria)

    batches = df_iteration['batch'].unique()
    
    if maximum_batches is not None:
      batches = batches[0:maximum_batches]

    # print(df_iteration.shape[0])
    for i in batches:
        print(f"Starting batch {i} of {len(batches)} at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

        df = df_iteration.query(f'batch == {i}')

        print(f"\tProcessing {df.shape[0]} samples")

        # Map gemini_wrapper to criteria iteration data contained in each row
        output = df \
            .apply(lambda row: gemini_wrapper(
                criteria = row['criteria'], 
                i = str(row['i']), 
                symptoms = row['symptoms'],
                gemini_version = gemini_version,
                sleep_time = sleep_time),
                axis = 1)
            
        # print(list(output))

        json_path = output_dir+f"/gemini_output_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        print(json_path)

        # Write Gemini output to json file
        with open(json_path, 'w') as json_file:
            json.dump(list(output), json_file, indent=4)

# Run pipeline
gemini_pipeline(
        iteration_path="/labs/khatrilab/solomonb/mcas/data/criteria_query_iterations.csv",
        output_dir="/labs/khatrilab/solomonb/mcas/data/gemini_json_output",
        batch_size = 200, 
        temp=1.0,
        # gemini_version = "gemini-1.5-pro-latest",
        gemini_version = "gemini-1.5-flash-latest",
        sleep_time = 4
        # maximum_batches = 1
)
