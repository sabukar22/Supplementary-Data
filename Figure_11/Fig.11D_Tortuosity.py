##Calculates tortuosity of 3D cell paths by calculating the d/D every 40 minutes in the cell's migration

import numpy as np
import pandas as pd

def calculate_tortuosity(x, y, z, interval=5):
    num_points = len(x)
    num_intervals = num_points // interval
    tortuosity_values = []

    for i in range(num_intervals):
        start_index = i * interval
        end_index = start_index + interval

        if end_index >= num_points:
            break

        dx = x[end_index] - x[start_index]
        dy = y[end_index] - y[start_index]
        dz = z[end_index] - z[start_index]

        straight_line_distance = np.sqrt(dx**2 + dy**2 + dz**2)

        x_interval = x[start_index:end_index+1]
        y_interval = y[start_index:end_index+1]
        z_interval = z[start_index:end_index+1]

        interval_distance = 0
        for j in range(len(x_interval) - 1):
            dx_interval = x_interval[j+1] - x_interval[j]
            dy_interval = y_interval[j+1] - y_interval[j]
            dz_interval = z_interval[j+1] - z_interval[j]
            interval_distance += np.sqrt(dx_interval**2 + dy_interval**2 + dz_interval**2)

        tortuosity = straight_line_distance / interval_distance
        tortuosity_values.append(tortuosity)

    return tortuosity_values

file_path = r"Fig.7J_Tortuosity.xlsx"
df = pd.read_excel(file_path)

results = []

unique_names = df['Unique_Name'].unique()

for unique_name in unique_names:
    cell_data = df[df['Unique_Name'] == unique_name]
    
    x = cell_data['POSITION_X'].tolist()
    y = cell_data['POSITION_Y'].tolist()
    z = cell_data['POSITION_Z'].tolist()
    
    movie = cell_data['Movie'].iloc[0]  
    fate = cell_data['Fate'].iloc[0]    
    tortuosity_values = calculate_tortuosity(x, y, z)
    
    result_row = [unique_name, movie, fate]  
    
    if tortuosity_values:
        overall_tortuosity = np.mean(tortuosity_values)
        result_row.append(overall_tortuosity)
        result_row.extend(tortuosity_values)  
    else:
        result_row.append(np.nan)  
    
    results.append(result_row)


max_tortuosity_count = max(len(row) - 4 for row in results)  
column_headers = ['Unique_Name', 'Movie', 'Fate', 'Overall_Tortuosity'] + [f'Tortuosity_{i+1}' for i in range(max_tortuosity_count)]
results_df = pd.DataFrame(results, columns=column_headers)

output_file = r"Tortuosity_Results.xlsx"
results_df.to_excel(output_file, index=False)
