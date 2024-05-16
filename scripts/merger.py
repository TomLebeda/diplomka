import csv
import os


def parse_file(filepath):
    # Read the content of the file
    with open(filepath, "r") as file:
        lines = file.readlines()

    data = {}

    # Parse each line in the file
    for line in lines:
        # Split each line into key and value based on the colon ':'
        key, value = line.strip().split(":")
        # Store the value in the dictionary with the key as the column name
        data[key.strip()] = value.strip()

    return data


def merge_files_to_csv(directory, output_csv):
    # List all files in the specified directory
    files = [
        f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))
    ]

    # Prepare data to be written to the CSV file
    data = {}

    for filename in files:
        filepath = os.path.join(directory, filename)
        # Parse the file and retrieve the data
        file_data = parse_file(filepath)
        # Store the data with the filename as the key
        data[filename] = file_data

    # Get all unique keys (column names) from all files
    all_keys = sorted(
        set(key for file_data in data.values() for key in file_data.keys())
    )

    # # Transpose the data for writing to the CSV file
    transposed_data = [
        {"Filename": filename, **{key: file_data.get(key, "") for key in all_keys}}
        for filename, file_data in data.items()
    ]

    # Write the data to the CSV file
    with open(output_csv, "w", newline="") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=["Filename"] + all_keys)
        writer.writeheader()
        writer.writerows(transposed_data)


# Specify the directory containing the files to merge
directory_path = "data/out/compare/score/"

# Specify the output CSV file path
output_csv_path = "data/out/compare/merged.csv"

# Call the function to merge files into a CSV
merge_files_to_csv(directory_path, output_csv_path)
