import plotly.express as px
import pandas as pd
import plotly.graph_objects as go
import plotly.io as pio

file_path = r"Fig.7E_Average_speed.xlsx"
df_all = pd.DataFrame()

excel_data = pd.ExcelFile(file_path)
for sheet in excel_data.sheet_names:
    df = pd.read_excel(file_path, sheet_name=sheet)
    df['LABEL'] = sheet 
    df_all = pd.concat([df_all, df], ignore_index=True)

def create_plot(color_option):
    fig = go.Figure()

    min_val = df_all[color_option].min()
    max_val = df_all[color_option].max()

    for label in df_all['LABEL'].unique():
        plot_data = df_all[df_all['LABEL'] == label]
        color_data = plot_data[color_option]

        fig.add_trace(go.Scatter3d(
            x=plot_data['POSITION_X'] * 0.347,
            y=plot_data['POSITION_Y'] * 0.347,
            z=plot_data['POSITION_Z'] * 0.347,
            mode='lines',
            line=dict(
                width=5, 
                color=color_data, 
                cmin=0.12 if color_option == 'Speed' else min_val,
                cmax=1 if color_option == 'Speed' else max_val,
                colorscale='Viridis' if color_option == 'MEAN_INTENSITY_CH1' else 'Magma',
                showscale=True),
            name=label, 
            showlegend=False
        ))

    fig.update_layout(
        title=f'3D PLOT - {color_option}',
        scene=dict(
            xaxis_title='X',
            yaxis_title='Y',
            zaxis_title='Z',
            camera=dict(
                eye=dict(x=-1, y=-2, z=1.3) 
            )
        )
    )

    return fig

color_option = 'Speed'
fig = create_plot(color_option)


fig.show()

#pio.write_image()
