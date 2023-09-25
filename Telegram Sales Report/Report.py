import pandas as pd
from datetime import timedelta

from Graphs import graph_metric1,graph_metric2,graph_metric3

def make_report(interval=7):
    
    # Importing data
    data = pd.read_csv('Data output/METRICS.csv')
    data['happened_date'] = pd.to_datetime(data['happened_date']).dt.date

    # Calculate the upper and lower bounds for filtering
    upper = data['happened_date'].max()
    lower = upper - timedelta(days=interval-1)

    # Filtering metrics
    filtered_data = data[(data['happened_date'] >= lower) & (data['happened_date'] <= upper)].reset_index()

    # Creating graphs
    graph_metric1(filtered_data)
    graph_metric2(filtered_data)
    graph_metric3(filtered_data)