import json
import os

import pandas as pd
import pytest

from src import config
from src.train_model import (
    build_pipeline,
    load_raw_data,
    rank_clusters_to_profiles,
    train,
)


@pytest.fixture
def tmp_paths(tmp_path):
    return str(tmp_path / "wine_kmeans_pipeline.joblib"), str(tmp_path / "cluster_profile.json")


def test_load_raw_data_returns_thirteen_columns_no_duplicates():
    df = load_raw_data(config.DATA_PATH)
    assert list(df.columns) == config.COLUMN_NAMES
    assert df.duplicated().sum() == 0


def test_build_pipeline_has_scaler_and_kmeans_with_spec_hyperparameters():
    pipeline = build_pipeline()
    kmeans = pipeline.named_steps["kmeans"]
    assert kmeans.n_clusters == 3
    assert kmeans.n_init == 25
    assert kmeans.random_state == 123


def test_rank_clusters_to_profiles_orders_by_flavanoids_descending():
    labeled_df = pd.DataFrame(
        {
            "Flavanoids": [3.0, 3.0, 0.8, 0.8, 2.0, 2.0],
            "cluster": [0, 0, 1, 1, 2, 2],
        }
    )
    mapping = rank_clusters_to_profiles(labeled_df)
    assert mapping[0] == "premium_reserve"
    assert mapping[2] == "classic_balance"
    assert mapping[1] == "light_fresh"


def test_train_writes_model_and_profile_with_three_segments(tmp_paths):
    model_path, profile_path = tmp_paths
    expected_rows = len(load_raw_data(config.DATA_PATH))

    metrics = train(data_path=config.DATA_PATH, model_path=model_path, profile_path=profile_path)

    assert os.path.exists(model_path)
    assert os.path.exists(profile_path)
    assert 0.0 <= metrics["silhouette"] <= 1.0
    assert 0.0 <= metrics["variance_explained"] <= 1.0
    assert sum(metrics["cluster_sizes"].values()) == expected_rows

    with open(profile_path, encoding="utf-8") as f:
        profiles = json.load(f)
    names = {p["name"] for p in profiles.values()}
    assert names == {"Premium Reserve", "Classic Balance", "Light & Fresh"}
