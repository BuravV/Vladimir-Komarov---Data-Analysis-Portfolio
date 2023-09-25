import os 

import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import matplotlib.ticker as ticker

#functiong for formating graphs
def to_percent(y, _):
    return f'{int(y)}%'

def format_thousands(x,pos):
    return f'{x/1000:.0f}'


#METRIC 1
def graph_metric1(data, save = True): 
    
    sns.set_theme(style="whitegrid", palette="pastel")
    
    plt.figure(figsize=(14, 8))
    ax = sns.lineplot(x='happened_date', y='CCR_app(%)', data=data, label='APP',linewidth=3)
    sns.lineplot(x='happened_date', y='CCR_call(%)', data=data, label='CALL/WA',linewidth=3)
    sns.lineplot(x='happened_date', y='CCR_intro(%)', data=data, label='IL/DL',linewidth=3)    
        
    plt.xlabel('')
    plt.ylabel('')
    plt.title('Customers Cumulative Conversion')
    
    ax.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f'{int(y)}%'))
    
    # Add a legend
    plt.legend()
    plt.legend(loc='upper left', bbox_to_anchor=(1, 1))

    plt.xticks(rotation=45)
    
    if save:
        #the path to Data output
        output_folder = 'Data output'  
        output_file_name = 'CCR cumulative.png'
        output_file_path = os.path.join(output_folder, output_file_name)
        
        #save
        plt.savefig(output_file_path, dpi=300, bbox_inches='tight')
        
    else:
        #show
        plt.show()
    

#METRIC 2 
def graph_metric2(data, save = True): 
    
    sns.set_theme(style="whitegrid", palette="pastel")
    
    plt.figure(figsize=(10, 8))
    
    sns.barplot(x='happened_date', y='DAU_request', data=data, color = 'grey', width=0.6)
    
    plt.xlabel('')
    plt.ylabel('Number of People')
    plt.title('Active users (applications)')
    
    last_date = data['happened_date'].iloc[-1]
    last_date_index = data.index[data['happened_date'] == last_date]
    
    # Highlight the last date in red
    plt.bar(last_date_index, data['DAU_request'].iloc[last_date_index], color='dodgerblue', width=0.6)
    
    plt.xticks(rotation=45)
    
    if save:
        #the path to Data output
        output_folder = 'Data output'  
        output_file_name = 'DUA.png'
        output_file_path = os.path.join(output_folder, output_file_name)
        
        #save 
        plt.savefig(output_file_path, dpi=300, bbox_inches='tight')
    else:
        #show
        plt.show()
         
 
#METRIC 3     
def graph_metric3(data, save = True):
    
    sns.set_theme(style="whitegrid", palette="pastel")
    
    plt.figure(figsize=(10, 8))
    
    sns.barplot(x='happened_date', y='daily_revenue', data=data, color = 'grey', width=0.6)
    
    plt.xlabel('')
    plt.ylabel('RUB(in thousands)')
    plt.title('Daily Revenue( + Var to previous day)')
    
    last_date = data['happened_date'].iloc[-1]
    last_date_index = data.index[data['happened_date'] == last_date][0]
    
    # Highlight the last date in red
    plt.bar(last_date_index, data['daily_revenue'].iloc[last_date_index], color='dodgerblue', width=0.6)
    
    data = data[data['Daily_revenue_VarRel(%)'].notna()]
    
    for i, row in data.iterrows():
        var_rel_percent = row['Daily_revenue_VarRel(%)']
        plt.text(i, row['daily_revenue'], f'{var_rel_percent:.2f}%', ha='center', va='bottom', fontsize=8)
    
    plt.gca().yaxis.set_major_formatter(ticker.FuncFormatter(format_thousands))
    
    plt.xticks(rotation=45)
    
    if save:
        #the path to Data output
        output_folder = 'Data output'  
        output_file_name = 'Daily Revenue.png'
        output_file_path = os.path.join(output_folder, output_file_name)
        
        #save 
        plt.savefig(output_file_path, dpi=300, bbox_inches='tight')
    else:
        #show
        plt.show()