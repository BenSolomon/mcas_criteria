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

# Create Claude query from 
def build_query(symptom_string):
    out_query = f"For educational purposes, return a json list of format {{'diagnoses':[diagnosis list]}} with the top 10 diagnoses for the following combination of symptoms: {symptom_string}"
    return out_query

# Function that corrects quotes, double-quotes, apostrophes, and escape characters
# To convert claude output string to string suitable for json conversion
def format_claude_json(string):
    # print(string)
    string_list = string[string.find('[')+1:string.find(']')].split(',')
    string_list = [x.replace("\\'","").replace('"','').replace("'",'').strip() for x in string_list]
    string_dict = {'diagnoses':string_list}
    # print(string_dict)
    return(string_dict)
    # string_list = [f'"{x}"' for x in string_list]
    # string = ', '.join(string_list)
    # string = f'{{"diagnoses":[{string}]}}'
    # print(string)
    # return(string)

# Submit Claude query
def diagnosis_query(query, claude_model = "claude-3-haiku-20240307", temp = 1.0):
    try:
      message = client.messages.with_raw_response.create(
          model=claude_model,
          max_tokens=1000,
          temperature=temp,
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
      # Return response data
      response = message.parse()
      output = format_claude_json(response.content[0].text)

      # Extract rate limit information from API header 
      header = dict(message.headers)
      header = {k:i for k,i in header.items() if 'anthropic' in k and 'reset' not in k}
    
    # Error handling
    except anthropic.APIError as e:
        print(f"Error: {e}\n{query=}")
        output = {"diagnoses":"error"}
        header=""
        response=""
    
    # Combine output data into dict
    if output is not None:
        # print(output)
        # output = output.replace("\\'", "!!").replace("'",'"').replace("!!", "'") # Ensures double-quotes for json
        # print(output)
        # output = json.loads(output)  
        output = {
            'json':output,
            'header':header,
            'response':response
        }
    return output

# Create json object with query parameters and query output
## Output_query includes full query string in json output for debugging
def anthropic_wrapper(i, criteria, symptoms, claude_version="claude-3-haiku-20240307", temp=1.0, output_query=False):
    inputs=locals()
    query = build_query(symptoms)
    query_output = diagnosis_query(query, claude_version, temp=temp)
    json_output = query_output['json']
    output = {'i':i, 'criteria':criteria, 'symptoms':[s.strip() for s in symptoms.split(",")]}
    if output_query:
        output.update({'query':query})
    if json_output is not None:
        output.update(json_output)

    # Create log of all query information
    log = {
        'inputs':inputs,
        'header':query_output['header']
        # 'response':query_output['response'],
        # 'output':query_output['json']
    }
    print(log)

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

# Remove succesfully completed iterations to Claude and remove them from 
# dataframe of all iterations, leaving only unprocessed iterations
def remove_complete_iterations(iteration_path, json_dir, minimum_diagnoses = 5):
    iteration_df=pd.read_csv(iteration_path)
    
    ###### Limit to first 10,000 iterations due to Claude rates
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

def claude_pipeline(iteration_path, output_dir, batch_size = 1000, claude_version = "claude-3-haiku-20240307", temp=1.0, maximum_batches = None):
    output_dir = f'/labs/khatrilab/solomonb/mcas/data/claude_json_output/{claude_version}_t{str(temp).replace(".","-")}'
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Read in iteration data, removing previously completed iterations
    df_iteration = remove_complete_iterations(
        iteration_path=iteration_path,
        json_dir=output_dir
    )
    
    # Calculate batch ids
    # batch_size = 1000 # Set total number of Claude iterations to submit per save file
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

        # Map anthropic_wrapper to criteria iteration data contained in each row
        output = df \
            .apply(lambda row: anthropic_wrapper(
                criteria = row['criteria'], 
                i = str(row['i']), 
                symptoms = row['symptoms'],
                claude_version = claude_version),
                axis = 1)
            
        # print(list(output))

        json_path = output_dir+f"/claude_output_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        print(json_path)

        # Write Claude output to json file
        with open(json_path, 'w') as json_file:
            json.dump(list(output), json_file, indent=4)

# Run pipeline
claude_pipeline(
        iteration_path="/labs/khatrilab/solomonb/mcas/data/criteria_query_iterations.csv",
        output_dir="/labs/khatrilab/solomonb/mcas/data/claude_json_output",
        batch_size = 200, 
        temp=1.0,
        claude_version = "claude-3-opus-20240229"
        # maximum_batches = 60
)
