"""WineIQ — Plataforma de Segmentación Inteligente de Vinos."""
import io

import pandas as pd
import plotly.express as px
import streamlit as st
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score

from src import config
from src.predictor import ValidationError, WinePredictor

st.set_page_config(page_title="WineIQ", page_icon="🍷", layout="wide")

# Palette drawn from the wine itself: parchment/card neutrals for a tasting-
# card surface, and a burgundy → garnet → rosé progression that mirrors the
# real Color_Intensity ordering of the three segments (see _render_tasting_flight).
PARCHMENT = "#F5EEE1"
CARD = "#FFFBF2"
INK = "#2A1712"
INK_SOFT = "#7A6A5E"
BURGUNDY = "#5C1327"
GARNET = "#9C2B3C"
ROSE = "#D98A93"
GOLD = "#B08D57"

SEGMENT_COLORS = {
    "Premium Reserve": BURGUNDY,
    "Classic Balance": GARNET,
    "Light & Fresh": ROSE,
}
SEGMENT_ORDER = ["Premium Reserve", "Classic Balance", "Light & Fresh"]

PLOTLY_LAYOUT = dict(
    plot_bgcolor=CARD,
    paper_bgcolor="rgba(0,0,0,0)",
    font=dict(family="Inter, sans-serif", color=INK),
    title_font=dict(family="Fraunces, serif", size=18, color=BURGUNDY),
    legend=dict(bgcolor="rgba(0,0,0,0)"),
    margin=dict(t=60, l=10, r=10, b=10),
)


def _style_figure(fig):
    fig.update_layout(**PLOTLY_LAYOUT)
    fig.update_xaxes(gridcolor="rgba(176,141,87,0.25)", zerolinecolor="rgba(176,141,87,0.4)")
    fig.update_yaxes(gridcolor="rgba(176,141,87,0.25)", zerolinecolor="rgba(176,141,87,0.4)")
    return fig


FONT_IMPORT_URL = (
    "https://fonts.googleapis.com/css2?"
    "family=Fraunces:opsz,wght@9..144,600;9..144,700"
    "&family=Inter:wght@400;500;600;700&display=swap"
)


def inject_style():
    css = """
        <style>
        @import url('__FONT_IMPORT_URL__');

        .stApp { background: #F5EEE1; color: #2A1712; }
        .stApp, .stApp p, .stApp li, .stApp label {
            font-family: 'Inter', sans-serif;
        }
        .stApp h1, .stApp h2, .stApp h3 {
            font-family: 'Fraunces', serif;
            color: #5C1327;
            letter-spacing: -0.01em;
        }
        .stApp h1 {
            font-size: 2.75rem;
            font-weight: 700;
            border-bottom: 2px solid #B08D57;
            padding-bottom: 0.5rem;
            display: inline-block;
        }
        [data-testid="stCaptionContainer"] {
            text-transform: uppercase;
            letter-spacing: 0.14em;
            font-size: 0.72rem;
            font-weight: 600;
            color: #7A6A5E;
        }

        /* Signature: tasting-flight color reference card */
        .wineiq-flight {
            display: flex;
            gap: 1rem;
            margin: 1.75rem 0 2.25rem;
            flex-wrap: wrap;
        }
        .flight-swatch {
            flex: 1 1 160px;
            min-width: 160px;
            border-radius: 60% 60% 10px 10px;
            padding: 2.25rem 1rem 1rem;
            text-align: center;
            color: #FFFBF2;
            box-shadow: 0 6px 16px rgba(42, 23, 18, 0.18);
            transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .flight-swatch:hover {
            transform: translateY(-5px);
            box-shadow: 0 10px 22px rgba(42, 23, 18, 0.26);
        }
        .flight-swatch .flight-name {
            display: block;
            font-family: 'Fraunces', serif;
            font-weight: 600;
            font-size: 1.05rem;
            margin-bottom: 0.2rem;
        }
        .flight-swatch .flight-meta {
            display: block;
            font-size: 0.78rem;
            opacity: 0.92;
        }

        [data-testid="stMetric"] {
            background: #FFFBF2;
            border: 1px solid rgba(176, 141, 87, 0.35);
            border-left: 4px solid #5C1327;
            border-radius: 6px;
            padding: 0.9rem 1.1rem;
            box-shadow: 0 2px 8px rgba(42, 23, 18, 0.06);
            transition: transform 0.15s ease, box-shadow 0.15s ease;
        }
        [data-testid="stMetric"]:hover {
            transform: translateY(-3px);
            box-shadow: 0 6px 14px rgba(42, 23, 18, 0.14);
        }
        [data-testid="stMetricLabel"] {
            font-size: 0.78rem;
            text-transform: uppercase;
            letter-spacing: 0.08em;
            color: #7A6A5E;
        }
        [data-testid="stMetricValue"] {
            font-family: 'Fraunces', serif;
            color: #5C1327;
            font-variant-numeric: tabular-nums;
        }

        .stTabs [data-baseweb="tab-list"] {
            gap: 0.5rem;
            border-bottom: 2px solid rgba(176, 141, 87, 0.35);
        }
        .stTabs [data-baseweb="tab"] {
            font-family: 'Inter', sans-serif;
            font-weight: 600;
            color: #7A6A5E;
            padding: 0.6rem 0.9rem;
            transition: color 0.15s ease, border-bottom-color 0.15s ease;
            border-bottom: 3px solid transparent;
        }
        .stTabs [data-baseweb="tab"]:hover {
            color: #9C2B3C;
        }
        .stTabs [aria-selected="true"] {
            color: #5C1327 !important;
            border-bottom: 3px solid #5C1327 !important;
        }

        .stButton button, .stDownloadButton button, [data-testid="stFormSubmitButton"] button {
            background: #5C1327;
            color: #FFFBF2;
            border: none;
            border-radius: 4px;
            font-weight: 600;
            letter-spacing: 0.03em;
            padding: 0.5rem 1.4rem;
            transition: background 0.15s ease, transform 0.15s ease;
        }
        .stButton button:hover, .stDownloadButton button:hover,
        [data-testid="stFormSubmitButton"] button:hover {
            background: #9C2B3C;
            color: #FFFBF2;
            transform: translateY(-2px);
        }

        [data-testid="stForm"] {
            background: #FFFBF2;
            border: 1px solid rgba(176, 141, 87, 0.35);
            border-radius: 8px;
            padding: 1.25rem 1.5rem;
        }

        @keyframes wineiq-fade-in {
            from { opacity: 0; transform: translateY(-8px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .wineiq-flight { animation: wineiq-fade-in 0.5s ease both; }
        .flight-swatch:nth-child(1) { animation-delay: 0.05s; }
        .flight-swatch:nth-child(2) { animation-delay: 0.15s; }
        .flight-swatch:nth-child(3) { animation-delay: 0.25s; }

        @media (max-width: 640px) {
            .stApp h1 { font-size: 2rem; }
            .wineiq-flight { flex-direction: column; }
            .flight-swatch { border-radius: 10px; padding: 1rem; }
        }
        @media (prefers-reduced-motion: reduce) {
            * { animation: none !important; transition: none !important; }
        }
        </style>
        """
    # st.html() renders this verbatim — st.markdown(unsafe_allow_html=True)
    # runs the string through the Markdown parser first, which turns
    # indented/blank-line-separated CSS into a visible code block instead of
    # applying it.
    st.html(css.replace("__FONT_IMPORT_URL__", FONT_IMPORT_URL))


def _render_tasting_flight(labeled_df: pd.DataFrame):
    """Signature visual: a sommelier-style color reference card. Swatch depth
    (burgundy → garnet → rosé) and the printed intensity value both come from
    the real mean Color_Intensity per segment — not decoration."""
    counts = labeled_df["Segmento"].value_counts()
    color_intensity = labeled_df.groupby("Segmento")["Color_Intensity"].mean()

    swatches = ""
    for segment in SEGMENT_ORDER:
        if segment not in counts.index:
            continue
        count = int(counts[segment])
        intensity = color_intensity[segment]
        meta = f"{count} vinos · intensidad de color prom. {intensity:.1f}"
        swatches += f"""
        <div class="flight-swatch" style="background:{SEGMENT_COLORS[segment]};">
            <span class="flight-name">{segment}</span>
            <span class="flight-meta">{meta}</span>
        </div>
        """
    st.html(f'<div class="wineiq-flight">{swatches}</div>')


@st.cache_resource
def get_predictor() -> WinePredictor:
    return WinePredictor()


@st.cache_data
def get_labeled_dataset() -> pd.DataFrame:
    df = pd.read_csv(config.DATA_PATH)[config.COLUMN_NAMES].drop_duplicates().reset_index(drop=True)
    return get_predictor().predict_batch(df)


def build_pca_figure(labeled_df: pd.DataFrame):
    scaler = get_predictor().pipeline.named_steps["scaler"]
    scaled = scaler.transform(labeled_df[config.COLUMN_NAMES])
    pca = PCA(n_components=2, random_state=config.RANDOM_STATE)
    coords = pca.fit_transform(scaled)

    plot_df = labeled_df.copy()
    plot_df["PC1"] = coords[:, 0]
    plot_df["PC2"] = coords[:, 1]
    fig = px.scatter(
        plot_df,
        x="PC1",
        y="PC2",
        color="Segmento",
        category_orders={"Segmento": SEGMENT_ORDER},
        color_discrete_map=SEGMENT_COLORS,
        title="Segmentación de vinos (PCA 2D)",
        labels={
            "PC1": f"Componente 1 ({pca.explained_variance_ratio_[0]:.1%})",
            "PC2": f"Componente 2 ({pca.explained_variance_ratio_[1]:.1%})",
        },
    )
    return _style_figure(fig)


def render_header(labeled_df: pd.DataFrame):
    st.title("🍷 WineIQ")
    st.caption("Plataforma de Segmentación Inteligente de Vinos")
    _render_tasting_flight(labeled_df)

    col1, col2, col3 = st.columns(3)
    distribution = labeled_df["Segmento"].value_counts()
    col1.metric("Total de vinos", len(labeled_df))
    col2.metric("Segmentos", labeled_df["Segmento"].nunique())
    col3.metric("Segmento más común", distribution.index[0])


def render_single_form():
    st.header(":material/edit_note: Clasificar un vino individual")
    with st.form("single_wine_form"):
        cols = st.columns(3)
        values = {}
        for i, column in enumerate(config.COLUMN_NAMES):
            values[column] = cols[i % 3].number_input(
                column, value=0.0, format="%.2f", key=f"input_{column}"
            )
        submitted = st.form_submit_button(":material/wine_bar: Clasificar", type="primary")

    if not submitted:
        return

    try:
        result = get_predictor().predict_single(values)
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
        classified = get_predictor().predict_batch(df)
    except ValidationError as exc:
        return None, str(exc).split("; ")
    return classified, []


def render_batch_upload():
    st.header(":material/upload_file: Clasificación masiva")
    st.caption("Sube un CSV con las 13 columnas químicas para clasificar varios vinos a la vez.")

    with open(config.SAMPLE_BATCH_PATH, "rb") as f:
        st.download_button(
            ":material/download: Descargar CSV de ejemplo", f, file_name="sample_batch.csv"
        )

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
        ":material/download_done: Descargar resultados clasificados",
        classified.to_csv(index=False).encode("utf-8"),
        file_name="wine_classified.csv",
        mime="text/csv",
        type="primary",
    )


def render_model_quality(labeled_df: pd.DataFrame):
    st.header(":material/query_stats: Sobre el modelo")
    pipeline = get_predictor().pipeline
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
    elbow_fig = px.line(elbow_df, x="k", y="WSS", markers=True, title="Método del codo")
    elbow_fig.update_traces(line_color=BURGUNDY, marker_color=GARNET)
    st.plotly_chart(_style_figure(elbow_fig), width="stretch")


def main():
    inject_style()
    labeled_df = get_labeled_dataset()
    render_header(labeled_df)

    tab_dashboard, tab_single, tab_batch, tab_quality = st.tabs(
        [
            ":material/wine_bar: Dashboard",
            ":material/edit_note: Clasificar un vino",
            ":material/upload_file: Clasificación masiva",
            ":material/query_stats: Sobre el modelo",
        ]
    )
    with tab_dashboard:
        st.plotly_chart(build_pca_figure(labeled_df), width="stretch")
    with tab_single:
        render_single_form()
    with tab_batch:
        render_batch_upload()
    with tab_quality:
        render_model_quality(labeled_df)


if __name__ == "__main__":
    main()
