"""WineIQ — Plataforma de Segmentación Inteligente de Vinos."""
import io

import pandas as pd
import plotly.express as px
import streamlit as st
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score

from src import config
from src.predictor import (
    ValidationError,
    load_cluster_profiles,
    load_pipeline,
    predict_batch,
    predict_single,
)

st.set_page_config(page_title="WineIQ", page_icon="🍷", layout="wide")

BURGUNDY = "#7B1E3A"
BURGUNDY_DARK = "#4B0E22"


def inject_style():
    st.markdown(
        f"""
        <style>
        .stApp {{ background-color: #FAF6F3; }}
        h1, h2, h3 {{ color: {BURGUNDY_DARK}; }}
        div[data-testid="stMetric"] {{
            background-color: {BURGUNDY}1A;
            border-left: 4px solid {BURGUNDY};
            padding: 10px;
            border-radius: 4px;
        }}
        </style>
        """,
        unsafe_allow_html=True,
    )


@st.cache_resource
def get_pipeline():
    return load_pipeline(config.MODEL_PATH)


@st.cache_resource
def get_profiles():
    return load_cluster_profiles(config.CLUSTER_PROFILE_PATH)


@st.cache_data
def get_labeled_dataset() -> pd.DataFrame:
    df = pd.read_csv(config.DATA_PATH)[config.COLUMN_NAMES].drop_duplicates().reset_index(drop=True)
    return predict_batch(get_pipeline(), get_profiles(), df)


def build_pca_figure(labeled_df: pd.DataFrame):
    scaler = get_pipeline().named_steps["scaler"]
    scaled = scaler.transform(labeled_df[config.COLUMN_NAMES])
    pca = PCA(n_components=2, random_state=config.RANDOM_STATE)
    coords = pca.fit_transform(scaled)

    plot_df = labeled_df.copy()
    plot_df["PC1"] = coords[:, 0]
    plot_df["PC2"] = coords[:, 1]
    return px.scatter(
        plot_df,
        x="PC1",
        y="PC2",
        color="Segmento",
        color_discrete_sequence=["#7B1E3A", "#C08497", "#E8B4BC"],
        title="Segmentación de vinos (PCA 2D)",
        labels={
            "PC1": f"Componente 1 ({pca.explained_variance_ratio_[0]:.1%})",
            "PC2": f"Componente 2 ({pca.explained_variance_ratio_[1]:.1%})",
        },
    )


def render_header(labeled_df: pd.DataFrame):
    st.title("🍷 WineIQ")
    st.caption("Plataforma de Segmentación Inteligente de Vinos")
    col1, col2, col3 = st.columns(3)
    distribution = labeled_df["Segmento"].value_counts()
    col1.metric("Total de vinos", len(labeled_df))
    col2.metric("Segmentos", labeled_df["Segmento"].nunique())
    col3.metric("Segmento más común", distribution.index[0])
    st.bar_chart(distribution)


def render_single_form():
    st.header("Clasificar un vino individual")
    with st.form("single_wine_form"):
        cols = st.columns(3)
        values = {}
        for i, column in enumerate(config.COLUMN_NAMES):
            values[column] = cols[i % 3].number_input(column, value=0.0, format="%.2f", key=f"input_{column}")
        submitted = st.form_submit_button("Clasificar")

    if not submitted:
        return

    try:
        result = predict_single(get_pipeline(), get_profiles(), values)
    except ValidationError as exc:
        st.error(str(exc))
        return

    st.success(f"Segmento: **{result['segment']}**")
    st.write(result["description"])
    col1, col2 = st.columns(2)
    col1.metric("Precio sugerido", result["price_range"])
    col2.metric("Canal sugerido", result["channel"])


def process_uploaded_csv(file_bytes: bytes):
    """Returns (classified_df_or_None, error_messages)."""
    try:
        df = pd.read_csv(io.BytesIO(file_bytes))
    except Exception as exc:
        return None, [f"No se pudo leer el archivo CSV: {exc}"]

    try:
        classified = predict_batch(get_pipeline(), get_profiles(), df)
    except ValidationError as exc:
        return None, str(exc).split("; ")
    return classified, []


def render_batch_upload():
    st.header("Clasificación masiva")
    st.caption("Sube un CSV con las 13 columnas químicas para clasificar varios vinos a la vez.")

    with open(config.SAMPLE_BATCH_PATH, "rb") as f:
        st.download_button("Descargar CSV de ejemplo", f, file_name="sample_batch.csv")

    uploaded = st.file_uploader("CSV de vinos", type="csv", key="batch_uploader")
    if uploaded is None:
        return

    classified, errors = process_uploaded_csv(uploaded.getvalue())
    if classified is None:
        st.error("El archivo tiene errores y no fue procesado:")
        for error in errors:
            st.write(f"- {error}")
        return

    st.success(f"{len(classified)} vinos clasificados correctamente.")
    st.dataframe(classified)
    st.download_button(
        "Descargar resultados clasificados",
        classified.to_csv(index=False).encode("utf-8"),
        file_name="wine_classified.csv",
        mime="text/csv",
    )


def render_model_quality(labeled_df: pd.DataFrame):
    st.header("Sobre el modelo")
    pipeline = get_pipeline()
    scaled = pipeline.named_steps["scaler"].transform(labeled_df[config.COLUMN_NAMES])
    labels = pipeline.named_steps["kmeans"].labels_

    silhouette = silhouette_score(scaled, labels)
    inertia = pipeline.named_steps["kmeans"].inertia_
    total_variance = ((scaled - scaled.mean(axis=0)) ** 2).sum()
    variance_explained = 1 - inertia / total_variance

    col1, col2 = st.columns(2)
    col1.metric("Silhouette Score", f"{silhouette:.3f}")
    col2.metric("Varianza explicada", f"{variance_explained:.1%}")

    st.subheader("Método del codo")
    k_range = range(2, 8)
    inertias = []
    for k in k_range:
        km = KMeans(n_clusters=k, n_init=10, random_state=config.RANDOM_STATE)
        km.fit(scaled)
        inertias.append(km.inertia_)
    elbow_df = pd.DataFrame({"k": list(k_range), "WSS": inertias})
    st.plotly_chart(
        px.line(elbow_df, x="k", y="WSS", markers=True, title="Método del codo"),
        use_container_width=True,
    )


def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)

    tab_dashboard, tab_single, tab_batch, tab_quality = st.tabs(
        ["📊 Dashboard", "🍇 Clasificar un vino", "📁 Clasificación masiva", "🔬 Sobre el modelo"]
    )
    with tab_dashboard:
        st.plotly_chart(build_pca_figure(labeled_df), use_container_width=True)
    with tab_single:
        render_single_form()
    with tab_batch:
        render_batch_upload()
    with tab_quality:
        render_model_quality(labeled_df)


if __name__ == "__main__":
    main()
