import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.impute import SimpleImputer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report, confusion_matrix, f1_score
from sklearn.cluster import KMeans
from sklearn.mixture import GaussianMixture
from sklearn.decomposition import PCA

# Load your dataset
# Ensure the file is in the same directory or provide the correct path
file_path = r"C:\Users\2king\Downloads\Extended_Employee_Performance_and_Productivity_Data.xlsm"
if not os.path.exists(file_path):
    raise FileNotFoundError(f"File not found: {file_path}. Please check the path and ensure the file exists.")

xls = pd.ExcelFile(file_path)
projML = xls.parse(xls.sheet_names[0])

# Preprocessing for supervised learning
projML_clean = projML.copy()
projML_clean['Job_Satisfaction_Class'] = projML_clean['Employee_Satisfaction_Score'].round().astype(int)

X = projML_clean.drop(columns=['Employee_ID', 'Hire_Date', 'Employee_Satisfaction_Score', 'Resigned', 'Job_Satisfaction_Class'])
y = projML_clean['Job_Satisfaction_Class']

categorical_cols = X.select_dtypes(include=['object']).columns.tolist()
numeric_cols = X.select_dtypes(include=['int64', 'float64']).columns.tolist()

numeric_transformer = Pipeline([
    ("imputer", SimpleImputer(strategy="median")),
    ("scaler", StandardScaler())
])

categorical_transformer = Pipeline([
    ("imputer", SimpleImputer(strategy="most_frequent")),
    ("encoder", OneHotEncoder(handle_unknown="ignore"))
])

preprocessor = ColumnTransformer([
    ("num", numeric_transformer, numeric_cols),
    ("cat", categorical_transformer, categorical_cols)
])

# Supervised Learning Pipeline
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, stratify=y, random_state=42)

pipeline = Pipeline([
    ("preprocessor", preprocessor),
    ("classifier", DecisionTreeClassifier())
])

pipeline.fit(X_train, y_train)
y_pred = pipeline.predict(X_test)

print("F1 Score:", f1_score(y_test, y_pred, average="weighted"))
print("Classification Report:\n", classification_report(y_test, y_pred))
print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred))

# Visualization: Confusion Matrix
cm = confusion_matrix(y_test, y_pred)
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=[1, 2, 3, 4, 5], yticklabels=[1, 2, 3, 4, 5])
plt.title('Confusion Matrix')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.show()



# Unsupervised Learning Setup
X_unsupervised = projML_clean.drop(columns=['Employee_ID', 'Hire_Date', 'Employee_Satisfaction_Score', 'Job_Satisfaction_Class', 'Resigned'])
X_unsupervised_encoded = pd.get_dummies(X_unsupervised, drop_first=True)

# Sample to reduce memory load
sampled_projML = X_unsupervised_encoded.sample(n=5000, random_state=42)
sampled_scaled = StandardScaler().fit_transform(sampled_projML)

# KMeans Clustering
kmeans = KMeans(n_clusters=4, random_state=42)
kmeans_labels = kmeans.fit_predict(sampled_scaled)

# GMM Clustering
gmm = GaussianMixture(n_components=4, random_state=42)
gmm_labels = gmm.fit_predict(sampled_scaled)

# Append Cluster Labels
cluster_results = sampled_projML.copy()
cluster_results['KMeans_Cluster'] = kmeans_labels
cluster_results['GMM_Cluster'] = gmm_labels

print(cluster_results.head())

# Visualization: Clustering with PCA
pca = PCA(n_components=2)
reduced = pca.fit_transform(sampled_scaled)

scatter = plt.scatter(reduced[:, 0], reduced[:, 1], c=kmeans_labels, cmap='viridis', alpha=0.6)
plt.title('KMeans Clusters (PCA-reduced)')
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.colorbar()
handles, labels = scatter.legend_elements()
plt.legend(handles, [f"Cluster {i}" for i in range(len(handles))], title="KMeans Clusters")
plt.show()

# Visualization: Feature Importance (Top 10)
importances = pipeline.named_steps['classifier'].feature_importances_
feature_names = pipeline.named_steps['preprocessor'].get_feature_names_out()
top_idx = np.argsort(importances)[-10:]

plt.barh(range(10), np.array(importances)[top_idx])
plt.yticks(range(10), np.array(feature_names)[top_idx])
plt.title("Top 10 Feature Importances")
plt.xlabel("Importance")
plt.ylabel("Feature")
plt.show()