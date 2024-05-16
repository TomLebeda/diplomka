import fileinput
import os


def replace_and_save_files(directory):
    # Iterate over all files in the specified directory
    for filename in os.listdir(directory):
        filepath = os.path.join(directory, filename)

        # Check if the current item is a file
        if os.path.isfile(filepath):
            print(f"Processing file: {filepath}")

            # Read the file's content
            with fileinput.FileInput(filepath, inplace=True) as file:
                for line in file:
                    modified_line = line.replace(",", "")
                    modified_line = modified_line.replace(".", "\n")
                    modified_line = modified_line.lower()
                    # modified_line = modified_line.strip()
                    # Print the modified line (automatically writes to the same file)
                    if len(modified_line) > 0:
                        print(modified_line)

            print(f"File processed and saved: {filepath}")


# Specify the directory containing the files to process
directory_path = "./data/out/compare/inputs/"

# Call the function to replace and save files in the specified directory
replace_and_save_files(directory_path)
