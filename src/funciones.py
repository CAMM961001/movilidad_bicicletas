#bibliotecas
import re
import unicodedata

import cufflinks as cf
import numpy as np
# bibliotecas para manejo de datos
import pandas as pd
# bibliotecas para graficar
import plotly.graph_objects as go
from scipy import stats
from sklearn.metrics import r2_score, mean_absolute_error, mean_squared_error
from statsmodels.stats.outliers_influence import variance_inflation_factor
from termcolor import colored

pd.options.plotting.backend = "plotly"
cf.go_offline()
pd.set_option("display.max_columns", 200)

def reEtiquetaVars(df,tipo,feats):
	feats_new=[tipo + "_"+x.lower() for x in feats]
	df.rename(columns=dict(zip(feats,feats_new)),inplace=True)

def completitud(df):
    comple=pd.DataFrame(df.isnull().sum())
    comple.reset_index(inplace=True)
    comple=comple.rename(columns={"index":"columna",0:"total"})
    comple["completitud"]=(1-comple["total"]/df.shape[0])*100
    comple=comple.sort_values(by="completitud",ascending=True)
    comple.reset_index(drop=True,inplace=True)
    return comple

def clean_text(text, pattern="[^a-zA-Z0-9 ]"):
    cleaned_text = unicodedata.normalize('NFD', text).encode('ascii', 'ignore')
    cleaned_text = re.sub(pattern, " ", cleaned_text.decode("utf-8"), flags=re.UNICODE)
    cleaned_text = u' '.join(cleaned_text.lower().strip().split())
    return cleaned_text

#graficar
def graph_bar(df,x,y,title="",x_title="",y_title=""):
    layout = go.Layout(font_family="Courier New, monospace",
        font_color="black",title_text=title,title_font_size=20,
        xaxis= {"title": {"text": x_title,"font": {"family": 'Courier New, monospace',"size": 12, "color": '#002e4d'}}},
        yaxis= {"title": {"text": y_title,"font": {"family": 'Courier New, monospace',"size": 12, "color": '#002e4d'}}},
        title_font_family="Arial",title_font_color="#002020",
        template="plotly_white", plot_bgcolor="rgb(168,168,168)")
    fig=df.iplot(kind='bar',x=x,y=y,title=title,asFigure=True,barmode="overlay",sortbars=True,color='#2b2b2b',layout=layout,width=5)
    fig.update_layout(width=800)
    fig.update_traces(marker_color='#045C8C',opacity=0.7)
    return fig


def graph_bar_count(df,x,title="",x_title="",y_title=""):
    layout = go.Layout(font_family="Courier New, monospace",
        font_color="black",title_text=title,title_font_size=20,
        xaxis= {"title": {"text": x_title,"font": {"family": 'Courier New, monospace',"size": 12, "color": '#002e4d'}}},
        yaxis= {"title": {"text": y_title,"font": {"family": 'Courier New, monospace',"size": 12, "color": '#002e4d'}}},
        title_font_family="Arial",title_font_color="#003030",
        template="plotly_white", plot_bgcolor="rgb(168,168,168)")
    aux=pd.DataFrame(df[x].value_counts()).reset_index().rename(columns={"index":"conteo"})
    fig=aux.iplot(kind='bar',x="conteo",y=x,title=title,asFigure=True,barmode="overlay",sortbars=True,color='#2b2b2b',layout=layout,width=5)
    fig.update_layout(width=800)
    fig.update_traces(marker_color='#045C8C',opacity=0.7)
    return fig

def graph_histogram(df,col,bins,title="",x_title="",y_title="conteo"):    
    layout = go.Layout(font_family="Courier New, monospace",
        font_color="black",title_text=title,title_font_size=20,
        xaxis= {"title": {"text": x_title,"font": {"family": 'Courier New, monospace',"size":12,"color": '#002e4d'}}},
        yaxis= {"title": {"text": y_title,"font": {"family": 'Courier New, monospace',"size": 12, "color": '#002e4d'}}},               
        title_font_family="Arial",title_font_color="#002020",
        template="plotly_white", plot_bgcolor="rgb(168,168,168)")
    fig=df[[col]].iplot(kind='histogram',x=col,bins=bins,title=title,asFigure=True,layout=layout,sortbars=True,linecolor='#2b2b2b')
    fig.update_traces(marker_color='#045C8C',opacity=0.7)
    return fig

def graph_pie_count(df,col,title=""):
    layout = go.Layout(template="plotly_white")
    colors=['#4676d0','#95b0e4','#19293c','#6faa9f','#ccceb1','#344647','#02160f','#779a7c','#070919','#2b2b2b','#121212']
    aux=pd.DataFrame(df[col].value_counts()).reset_index().rename(columns={"index":"conteo"})
    fig=aux.iplot(kind='pie',labels='conteo',values=col,title=title,asFigure=True,theme="white")
    fig.update_traces(textfont_size=10,opacity=0.65,
                  marker=dict(colors=colors))
    fig.update_layout(font_family="Courier New, monospace",
    font_color="black",title_text=title,title_font_size=20,title_font_family="Arial",title_font_color="#002020",template="plotly_white")
    return fig

def graph_pie(df,labels,values,title=""):
    layout = go.Layout(template="plotly_white")
    colors=['#4676d0','#95b0e4','#19293c','#6faa9f','#ccceb1','#344647','#02160f','#779a7c','#070919','#2b2b2b','#121212']*2
    fig=df.iplot(kind='pie',labels=labels,values=values,title=title,asFigure=True,theme="white")
    fig.update_traces(textfont_size=10,opacity=0.65,
                  marker=dict(colors=colors))
    fig.update_layout(font_family="Courier New, monospace",
    font_color="black",title_text=title,title_font_size=20,title_font_family="Arial",title_font_color="#002020",template="plotly_white")
    return fig

#BUsqueda de outliers meidiante z-score,IQR y diff entre percentiles
def search_outliers(data,cols):
    df=data.copy()
    results=pd.DataFrame()
    data_iqr=df.copy()
    data_per=df.copy()
    total=[]
    total_per=[]
    total_z=[]
    indices_=[]

    for col in cols:
        #IQR
        Q1=df[col].quantile(0.25)
        Q3=df[col].quantile(0.75)
        IQR=Q3-Q1
        INF=Q1-1.5*(IQR)
        SUP=Q3+1.5*(IQR)
    
        
        n_outliers=df[(df[col] < INF) | (df[col] > SUP)].shape[0]
        total.append(n_outliers)
        indices_iqr=list(df[(df[col] < INF) | (df[col] > SUP)].index)
        #data_iqr=data_iqr[~(data_iqr[col] < INF) | (data_iqr[col] > SUP)].reset_index(drop=True)
        
        #Percentiles
        INF_pe=np.percentile(df[col].dropna(),5)
    
        SUP_pe=np.percentile(df[col].dropna(),95)
        n_outliers_per=df[(df[col] < INF_pe) | (df[col] > SUP_pe)].shape[0]
        total_per.append(n_outliers_per)
        indices_per=list(df[(df[col] < INF_pe) | (df[col] > SUP_pe)].index)
        #data_per=data_per[~(data_per[col] < INF_pe) | (data_per[col] > SUP_pe)].reset_index(drop=True)
        
        #Z-Score
        
        z=np.abs(stats.zscore(df[col],nan_policy='omit'))
        #df[f"zscore_{col}"]=abs((df[col] - df[col].mean())/df[col].std(ddof=0))
        total_z.append(df[[col]][(z>=3)].shape[0])
        indices_z=list(df[[col]][(z>=3)].index)
        
        indices_.append(aux_outliers(indices_iqr,indices_per,indices_z))
        indices_.sort()
        #indices_ = list(indices_ for indices_,_ in itertools.groupby(indices_))
        
    results["features"]=cols
    results["n_outliers_IQR"]=total
    results["n_outliers_Percentil"]=total_per
    results["n_outliers_Z_Score"]=total_z
    results["n_outliers_IQR_%"]=round((results["n_outliers_IQR"]/df.shape[0])*100,2)
    results["n_outliers_Percentil_%"]=round((results["n_outliers_Percentil"]/df.shape[0])*100,2)
    results["n_outliers_Z_Score_%"]=round((results["n_outliers_Z_Score"]/df.shape[0])*100,2)
    results["indices"]=indices_
    results["total_outliers"]=results["indices"].map(lambda x:len(x))
    results["%_outliers"]=results["indices"].map(lambda x:round(((len(x)/df.shape[0])*100),2))
    results=results[['features', 'n_outliers_IQR', 'n_outliers_Percentil',
       'n_outliers_Z_Score', 'n_outliers_IQR_%', 'n_outliers_Percentil_%',
       'n_outliers_Z_Score_%',  'total_outliers', '%_outliers','indices']]
    return results
def aux_outliers(a,b,c):
    a=set(a)
    b=set(b)
    c=set(c)
    
    a_=a.intersection(b)

    b_=b.intersection(c)

    c_=a.intersection(c)

    outliers_index=list(set(list(a_)+list(b_)+list(c_)))
    return outliers_index

def imputar_moda(df,col,X_train,X_test):   
    valor_miss = X_train[col].mode()[0]
    
    x_i=df[col].fillna(valor_miss).value_counts()
    k=x_i.sum()
    p_i=df[col].dropna().value_counts(1)
    m_i=k*p_i
    chi=stats.chisquare(f_obs=x_i,f_exp=m_i)
    p_val=chi.pvalue
    alpha=0.05
    if p_val<alpha:
        print(colored("Rechazamos HO(La porporción de categorias es la misma que la general)",'red'))
        return (X_train[col],X_test[col])
    else:
        print(colored("Aceptamos HO(La porporción de categorias es la misma que la general)",'green'))
        print("Se reemplazan los valores ausentes.")
        return (X_train[col].fillna(valor_miss),X_test[col].fillna(valor_miss))

def imputar_continua(df,col):
    aux = df[col].dropna()
    estadisticos = dict(mean=aux.mean(),median=aux.median(),mode=aux.mode())
    originales=list(df[col].dropna().values)
    for key,value in estadisticos.items():
        imputados=list(df[col].fillna(value).values)
        print(f'{key}\n{stats.ks_2samp(originales,imputados)}')

def calc_vif(X):
    vif = pd.DataFrame()
    vif["variables"] = X.columns
    vif["VIF"] = [variance_inflation_factor(X.values, i) for i in range(X.shape[1])]
    return(vif)

def PCA(X , num_components):
     
    #1
    X_meaned = X - np.mean(X , axis = 0)
     
    #2
    cov_mat = np.cov(X_meaned , rowvar = False)
     
    #3
    eigen_values , eigen_vectors = np.linalg.eigh(cov_mat)
     
    #4
    sorted_index = np.argsort(eigen_values)[::-1]
    sorted_eigenvalue = eigen_values[sorted_index]
    sorted_eigenvectors = eigen_vectors[:,sorted_index]
     
    #5
    eigenvector_subset = sorted_eigenvectors[:,0:num_components]
     
    #6
    X_reduced = np.dot(eigenvector_subset.transpose() , X_meaned.transpose() ).transpose()
     
    return X_reduced


#Modelos lineales

from sklearn.metrics import roc_auc_score,accuracy_score,confusion_matrix

def metricas(model,Xv,yv): #Mide efectividad de un Modelo Predictivo
    print( " Roc Validate: %.3f" %roc_auc_score(y_score=model.predict_proba(Xv)[:,1],y_true=yv))
    print( " Acc Validate: %.3f" %accuracy_score(y_pred=model.predict(Xv),y_true=yv))
    print( " Matrix Conf Validate: ", "\n",confusion_matrix(y_pred=model.predict(Xv),y_true=yv))

def metricas2(model,Xv,yv): #Mide efectividad de un Modelo Predictivo
    print( " Acc Validate: %.3f" %accuracy_score(y_pred=model.predict(Xv),y_true=yv))
    print( " Matrix Conf Validate: ", "\n",confusion_matrix(y_pred=model.predict(Xv),y_true=yv))

def metricas_reg(y_true,y_pred):
    r2=r2_score(y_true,y_pred)
    mae=mean_absolute_error(y_true,y_pred)
    mse=mean_squared_error(y_true,y_pred)
    print(f'El r2 score es {r2}')
    print(f'El error cuadrático medio es {mse}')
    print(f'El error medio absoluto es {mae}')