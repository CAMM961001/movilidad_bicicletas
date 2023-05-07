from sklearn.metrics import roc_auc_score,accuracy_score,confusion_matrix
import matplotlib.pyplot as plt
import cufflinks as cf
import unicodedata
import re
import pandas as pd
from dateutil.parser import parse
import plotly.graph_objects as go

cf.go_offline()


def dataset_describe(dataset):
    for col in dataset.columns:
        print("*" * 20)
        print(col)
        print("*" * 20)
        print(dataset[col].value_counts())
        print("\n")

    return 0

# start basic utilitis

def validateCompleteness(data_frame):
    data_frame_aux = pd.DataFrame(data_frame.isnull().sum())
    data_frame_aux.reset_index(inplace=True)
    data_frame_aux = data_frame_aux.rename(columns={"index": "column", 0: "total"})
    data_frame_aux["Completeness"] = (1 - data_frame_aux["total"] / data_frame.shape[0]) * 100
    data_frame_aux = data_frame_aux.sort_values(by="total", ascending=False)
    data_frame_aux.reset_index(inplace=True)
    return data_frame_aux

def replace_list_strings(data_frame, colum_name, dic_replace):
    list_dic = list(dic_replace)
    for srt_val in list_dic:
        data_frame[colum_name] = data_frame[colum_name].str.replace(srt_val, dic_replace[srt_val])
    return data_frame

def is_date(string, fuzzy=False):
    """
    Return whether the string can be interpreted as a date.
    :param string: str, string to check for date
    :param fuzzy: bool, ignore unknown tokens in string if True
    """
    try:
        parse(string, fuzzy=fuzzy)
        return True

    except ValueError:
        return False


def my_histogram(df, col, bins, title="", x_title="", y_title="conteo"):
    """generates plotly histogram

    Parameters
    ----------
    df : pandas.DataFrame
        data frame to extract data from
    col : [string
        column from data frame to plot
    bins : int
        number of bins for histogram
    title : str, optional
        title of the plot, by default ""
    x_title : str, optional
        x axis title, by default ""
    y_title : str, optional
        y axis title, by default "conteo"

    Returns
    -------
    plotly figure
    """
    layout = go.Layout(font_family="Courier New, monospace",
                       font_color="black", title_text=title, title_font_size=20,
                       xaxis={"title": {"text": x_title,
                                        "font": {"family": 'Courier New, monospace', "size": 12, "color": '#002e4d'}}},
                       yaxis={"title": {"text": y_title,
                                        "font": {"family": 'Courier New, monospace', "size": 12, "color": '#002e4d'}}},
                       title_font_family="Arial", title_font_color="#002020",
                       template="plotly_white", plot_bgcolor="rgb(168,168,168)")
    fig = df[[col]].iplot(kind='histogram', x=col, bins=bins, title=title, asFigure=True, layout=layout, sortbars=True,
                          linecolor='#2b2b2b')
    fig.update_traces(marker_color='#045C8C', opacity=0.7)
    return fig
# End basic utilitis
def find_duplicated(dataset):
    print("*" * 20)
    print("Duplicados totales")
    print("*" * 20)
    print(dataset.duplicated().sum())
    print("*" * 20)
    print("\n")
    for col in dataset.columns:
        print("*" * 20)
        print("Duplicados en columna " + col)
        print("*" * 20)
        print(dataset.duplicated(subset=col).sum())
        print("*" * 20)
        print("tipo de dato")
        print(dataset[col].dtypes)

        print("\n")
    return 0


def renameHeaders(data_frame, column, prefix):
    column_aux = [(prefix + x).replace(" ", "_").lower() for x in column]
    data_frame.rename(columns=dict(zip(column, column_aux)), inplace=True)
    return data_frame


def metricas(model,Xv,yv): #Mide efectividad de un Modelo Predictivo
    print( " Roc Validate: %.3f" %roc_auc_score(y_score=model.predict_proba(Xv)[:,1],y_true=yv))
    print( " Acc Validate: %.3f" %accuracy_score(y_pred=model.predict(Xv),y_true=yv))
    print( " Matrix Conf Validate: ", "\n",confusion_matrix(y_pred=model.predict(Xv),y_true=yv))

    return 0


#%%
def plot_histogram(df, feature):
    return df[[feature]].iplot(kind="hist", title = f"{feature} histogram", colors=["#296EAA"])


def null_values_columns(data):
    for col in data.columns:
        print(f"{col}: {data[col].isnull().sum()}")


#%%

def plot_roc(false_positive_rate, true_positive_rate, roc_auc) :
    plt.figure(figsize=(7,7))
    plt.title('Receiver Operating Characteristic')
    plt.plot(false_positive_rate, true_positive_rate, color='red', label= 'AUC = %0.2f' % roc_auc)
    plt.legend(loc='lower right')
    plt.plot ([0,1],[0,1], linestyle='--')
    plt.axis('tight')
    plt.ylabel('true positive rate')
    plt.xlabel('false positive rate')


def clean_text(text):
    res =  unicodedata.normalize('NFD', text).encode('ascii', 'ignore')
    res = re.sub("[^a-zA-Z0-9 ]"," ", res.decode("utf-8"), flags=re.UNICODE)
    res =  u' '.join(res.lower().split())
    return res

#%%
def setHighlightsByTextColumn(dataFrame, columnName):
  dataFrame["n_"+columnName+"_caracteres"]=dataFrame[columnName].str.len()
  dataFrame["n_"+columnName+"_dots"] = dataFrame[columnName].map(lambda x: x.count('.'))
  dataFrame["n_"+columnName+"_lower_ratio_len"] = dataFrame[columnName].map(lambda x:sum(map(str.islower, x))) / dataFrame["n_"+columnName+"_caracteres"]
  dataFrame["n_"+columnName+"_upper_ratio_len"] = dataFrame[columnName].map(lambda x:sum(map(str.isupper, x))) / dataFrame["n_"+columnName+"_caracteres"]
  dataFrame["n_"+columnName+"_words"] = dataFrame[columnName].str.split(" ").str.len()
  dataFrame["n_"+columnName+"_letters"] = dataFrame[columnName].map(lambda x:sum(map(str.isalpha, x)))
  dataFrame["n_"+columnName+"_lower_ratio_letters"] = dataFrame[columnName].map(lambda x:sum(map(str.islower, x))) / dataFrame["n_"+columnName+"_letters"]
  dataFrame["n_"+columnName+"_upper_ratio_letters"] = dataFrame[columnName].map(lambda x:sum(map(str.isupper, x))) / dataFrame["n_"+columnName+"_letters"]
  dataFrame.head(5)
  return dataFrame



#%%

