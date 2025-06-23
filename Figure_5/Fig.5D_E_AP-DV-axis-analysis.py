import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from itertools import combinations
import matplotlib.cm as cm
import svg

%matplotlib notebook

def project_point_onto_line(point, line_point, direction):
    point = np.array(point)
    line_point = np.array(line_point)
    direction = np.array(direction)

    line_to_point = point - line_point
    direction_normalized = direction / np.linalg.norm(direction)
    projection_scalar = np.dot(line_to_point, direction_normalized)
    projected_point = line_point + projection_scalar * direction_normalized
    return projected_point, projection_scalar


percentages_file_path = ''
file_path = ''
df = pd.read_excel(file_path)
df['Unique_ID'] = df['LABEL'] + "_" + df['TRACK_ID'].astype(str)

#JSQH176 - 880
#pointA = np.array([1493.527651, 883.3285314, 1343.901625])
#pointP = np.array([1278.203435, 712.7597448, 571.9900765])

#JSQH317 - 820
#pointA = np.array([2246.412544, 1306.73612, 1314.584523])
#pointP = np.array([1729.599674, 1678.285134, 1057.241576])

#JSQH305 - 700
#pointA = np.array([918.5253195, 315.6630337, 725.8151972])
#pointP = np.array([568.4128533, 858.4969526, 1010.537542])

#JSQH194 - 750
pointA = np.array([1053.771606, 923.4401335, 662.8242075])
pointP = np.array([998.7978234, 1507.007978, 662.8242075])

#JSQH173 - 700
#pointA = np.array([1598.92038, 980.0406183, 639.5477493])
#pointP = np.array([1581.027801, 1507.634269, 676.7046872])

def distance(point1, point2):
    return np.linalg.norm(point1 - point2)

df['Distance_to_A'] = df.apply(lambda row: distance(np.array([row['POSITION_X'], row['POSITION_Y'], row['POSITION_Z']]), pointA), axis=1)
df['Distance_to_P'] = df.apply(lambda row: distance(np.array([row['POSITION_X'], row['POSITION_Y'], row['POSITION_Z']]), pointP), axis=1)
direction = pointP - pointA
projected_points_with_scalars = [project_point_onto_line(p, pointA, direction) for p in df[['POSITION_X', 'POSITION_Y', 'POSITION_Z']].values]
projected_points = np.array([p[0] for p in projected_points_with_scalars])
line_point_end = pointA + direction
scalars = np.array([p[1] for p in projected_points_with_scalars])
min_scalar = scalars.min()
max_scalar = scalars.max()
new_pointA = pointA + min_scalar * (direction / np.linalg.norm(direction))
new_pointP = pointA + max_scalar * (direction / np.linalg.norm(direction))

line_length = np.linalg.norm(pointP - pointA)

df['Projection_X'] = projected_points[:, 0]
df['Projection_Y'] = projected_points[:, 1]
df['Projection_Z'] = projected_points[:, 2]
max_distances_percentage = {}
grouped = df.groupby('TRACK_ID')

for track_id, group in grouped:
    group_projections = group[['Projection_X', 'Projection_Y', 'Projection_Z']].values
    
    if len(group_projections) < 2:
        continue
        
    max_distance = max(
        np.linalg.norm(np.array(projA) - np.array(projB)) 
        for projA, projB in combinations(group_projections, 2)
    )

    max_distance_percentage = (max_distance / line_length) * 100
    max_distances_percentage[track_id] = max_distance_percentage

for track_id, distance_percentage in max_distances_percentage.items():
    print(f'TRACK_ID {track_id}: clone range {distance_percentage:.2f}%')

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

#plots cells
for _, row in df.iterrows():
    ax.scatter(
        row['POSITION_X'], row['POSITION_Y'], row['POSITION_Z'], 
        color='pink' if row['Distance_to_A'] < row['Distance_to_P'] else 'blue', 
        s=5, label=row['Unique_ID']
    )

ax.scatter(pointA[0], pointA[1], pointA[2], color='pink', s=50, alpha = 1, label='Anterior')
ax.scatter(pointP[0], pointP[1], pointP[2], color='blue', s=50, alpha = 1, label='Posterior')

#ax.scatter(new_pointA[0], new_pointA[1], new_pointA[2], color='pink', s=50, alpha = 1, label='Anterior')
#ax.scatter(new_pointP[0], new_pointP[1], new_pointP[2], color='blue', s=50, alpha = 1, label='Posterior')

line_x = [pointA[0], line_point_end[0]]
line_y = [pointA[1], line_point_end[1]]
line_z = [pointA[2], line_point_end[2]]
ax.plot(line_x, line_y, line_z, color='green', linestyle='--', label='Line')

ax.scatter(projected_points[:, 0], projected_points[:, 1], projected_points[:, 2], 
           color='gray', s=2, alpha=0.5, label='Projected Points')

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')

#ax.set_xlim(800, 1700)
#ax.set_ylim(600, 1200)
#ax.set_zlim(500, 2000)
ax.set_title('AP_axis')

plt.show()

######

df_430 = df[df['TRACK_ID'] == 238]
line_direction = pointP - pointA
line_direction_normalized = line_direction / np.linalg.norm(line_direction)
points_430 = df_430[['POSITION_X', 'POSITION_Y', 'POSITION_Z']].values
projections_430 = []
for point in points_430:
    vector_to_point = point - pointA
    projection_length = np.dot(vector_to_point, line_direction_normalized)
    projection = pointA + projection_length * line_direction_normalized
    projections_430.append(projection)

projections_430 = np.array(projections_430)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

line_point_end = pointA + np.dot((pointP - pointA), line_direction_normalized) * line_direction_normalized
line_x = [pointA[0], line_point_end[0]]
line_y = [pointA[1], line_point_end[1]]
line_z = [pointA[2], line_point_end[2]]
ax.plot(line_x, line_y, line_z, color='green', linestyle='--', label='Line')

ax.scatter(projections_430[:, 0], projections_430[:, 1], projections_430[:, 2], 
           s=20, color='gray', label='Projected points')

ax.scatter(pointA[0], pointA[1], pointA[2], color='pink', s=50, alpha=1, label='Anterior')
ax.scatter(pointP[0], pointP[1], pointP[2], color='blue', s=50, alpha=1, label='Posterior')

for _, row in df_430.iterrows():
    ax.scatter(
        row['POSITION_X'], row['POSITION_Y'], row['POSITION_Z'], 
        color='red', 
        s=20, label=f'{row["TRACK_ID"]}'
    )

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('')

handles, labels = ax.get_legend_handles_labels()
by_label = dict(zip(labels, handles))
#ax.legend(by_label.values(), by_label.keys(), fontsize="7", bbox_to_anchor=(1.05, 1), loc='upper left')

plt.show()
